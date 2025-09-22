use crate::{
	sdf::{self, AbstractData, FileFormat},
	usda, usdc,
};

use super::Prim;

/// The outermost container for scene description, which owns and presents composed prims as a scenegraph,
/// following the composition recipe recursively described in its associated "root layer".
pub struct Stage {
	data: Box<dyn AbstractData>,

	asset_path: std::path::PathBuf,
}

impl Stage {
	pub fn open(asset_path: impl AsRef<std::path::Path>) -> Self {
		let path = asset_path.as_ref();
		let data: Box<dyn AbstractData> = if let Some(extension) = path.extension() {
			if extension == "usdc" {
				Box::new(usdc::Data::open(path).unwrap())
			} else if extension == "usda" {
				Box::new(usda::UsdaFile::open(path).unwrap())
			} else {
				panic!("Unsupported file extension: {:?}", extension)
			}
		} else {
			panic!("File has no extension")
		};

		Stage {
			data,
			asset_path: path.to_path_buf(),
		}
	}

	pub fn save(&mut self) {
		self.data.save(&self.asset_path).unwrap();
	}

	pub fn pseudo_root(&self) -> Prim<'_> {
		Prim::new(self, sdf::Path::absolute_root_path())
	}

	pub fn prim_at_path(&self, path: impl Into<sdf::Path>) -> Prim<'_> {
		Prim::new(self, path.into())
	}

	pub(crate) fn data(&self) -> &dyn AbstractData {
		&*self.data
	}
}
