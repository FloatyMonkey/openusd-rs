use crate::{
	sdf::{self, FIELD_KEYS},
	tf, usda, usdc, vt,
};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex, OnceLock};

static LAYER_CACHE_LOCK: OnceLock<Mutex<HashMap<PathBuf, Arc<Layer>>>> = OnceLock::new();

fn get_layer_cache() -> &'static Mutex<HashMap<PathBuf, Arc<Layer>>> {
	LAYER_CACHE_LOCK.get_or_init(|| Mutex::new(HashMap::new()))
}

/// A scene description container that can combine with other such containers
/// to form simple component assets, and successively larger aggregates.
/// The contents of a Layer adhere to the [sdf::AbstractData] data model.
pub struct Layer {
	file_path: PathBuf,
	pub(crate) data: Box<dyn sdf::AbstractData>,
}

impl std::fmt::Debug for Layer {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("Layer")
			.field("file_path", &self.file_path)
			.finish()
	}
}

/// Metadata
impl Layer {
	/// Return this layer's 'defaultPrim' metadata.
	pub fn default_prim(&self) -> tf::Token {
		self.data
			.get(&sdf::Path::absolute_root_path(), &FIELD_KEYS.default_prim)
			.and_then(|v| v.get::<tf::Token>())
			.unwrap_or_default()
	}

	/// Return this layer's 'defaultPrim' metadata interpreted as an absolute prim path.
	pub fn default_prim_as_path(&self) -> sdf::Path {
		let default_prim_token = self.default_prim();
		if !default_prim_token.is_empty() {
			// TODO: More robust way to make path absolute.
			if default_prim_token.as_str().starts_with('/') {
				return sdf::Path::from(default_prim_token.as_str());
			} else {
				assert!(!default_prim_token.as_str().contains('/'));
				return sdf::Path::absolute_root_path().append_child(&default_prim_token);
			}
		}

		sdf::Path::empty_path()
	}
}

/// Sublayers
impl Layer {
	pub fn sub_layer_paths(&self) -> vt::Array<String> {
		self.data
			.get(&sdf::Path::absolute_root_path(), &FIELD_KEYS.sub_layers)
			.and_then(|v| v.get::<vt::Array<String>>())
			.unwrap_or_default()
	}

	pub fn sub_layer_offsets(&self) -> vt::Array<sdf::Retiming> {
		self.data
			.get(
				&sdf::Path::absolute_root_path(),
				&FIELD_KEYS.sub_layer_offsets,
			)
			.and_then(|v| v.get::<vt::Array<sdf::Retiming>>())
			.unwrap_or_default()
	}
}

/// Relocates
impl Layer {
	pub fn relocates(&self) -> vt::Array<sdf::Relocate> {
		self.data
			.get(
				&sdf::Path::absolute_root_path(),
				&FIELD_KEYS.layer_relocates,
			)
			.and_then(|v| v.get::<vt::Array<sdf::Relocate>>())
			.unwrap_or_default()
	}
}

impl Layer {
	pub fn open(path: impl AsRef<Path>) -> Arc<Self> {
		let path_ref = path.as_ref();
		let path_buf = if path_ref.is_absolute() {
			path_ref.to_path_buf()
		} else {
			std::env::current_dir().unwrap_or_default().join(path_ref)
		};

		let cache = get_layer_cache();
		{
			let cache_guard = cache.lock().unwrap();
			if let Some(layer) = cache_guard.get(&path_buf) {
				return layer.clone();
			}
		}

		let layer = Arc::new(Self::new(path_buf.clone()));

		let mut cache_guard = cache.lock().unwrap();
		if let Some(layer) = cache_guard.get(&path_buf) {
			return layer.clone();
		}
		cache_guard.insert(path_buf.clone(), layer.clone());

		layer
	}

	fn new(path: PathBuf) -> Self {
		let data: Box<dyn sdf::AbstractData> = if let Some(extension) = path.extension() {
			// TODO: Detect file format properly
			if extension == "usd" || extension == "usdc" {
				usdc::Data::open(&path).unwrap()
			} else if extension == "usda" {
				let content = std::fs::read_to_string(&path)
					.unwrap_or_else(|_| panic!("Failed to read file {:?}", path));
				Box::new(usda::parser::parse(&content).expect("Failed to parse USDA"))
			} else {
				panic!("Unsupported extension {:?}", extension)
			}
		} else {
			panic!("No extension for file {:?}", path)
		};

		Self {
			file_path: path,
			data,
		}
	}

	pub(crate) fn resolve_path(&self, file_path: &str) -> PathBuf {
		let base = self.file_path.parent().unwrap_or(Path::new("."));
		base.join(file_path)
	}

	pub(crate) fn has_spec(&self, path: &sdf::Path) -> bool {
		self.data.spec_form(path).is_some()
	}

	pub(crate) fn identifier(&self) -> &str {
		self.file_path.to_str().unwrap_or_default()
	}
}
