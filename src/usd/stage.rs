use crate::{
	sdf::{self, AbstractData},
	usda,
	usdc::UsdcFile,
};

use super::Prim;

/// The outermost container for scene description, which owns and presents composed prims as a scenegraph,
/// following the composition recipe recursively described in its associated "root layer".
pub struct Stage {
	data: Box<dyn AbstractData>,
}

impl Stage {
	pub fn open(asset_path: impl AsRef<std::path::Path>) -> Self {
		let path = asset_path.as_ref();
		let data: Box<dyn AbstractData> = if let Some(extension) = path.extension() {
			if extension == "usdc" {
				Box::new(UsdcFile::open(path).unwrap())
			} else if extension == "usda" {
				match std::fs::read_to_string(path) {
					Ok(content) => match usda::parser::parse(&content) {
						Ok(text_data) => Box::new(text_data),
						Err(err) => panic!("Failed to parse USDA file: {:?}", err),
					},
					Err(err) => panic!("Failed to read USDA file: {:?}", err),
				}
			} else {
				panic!("Unsupported file extension: {:?}", extension)
			}
		} else {
			panic!("File has no extension")
		};

		Stage { data }
	}

	pub fn pseudo_root(&self) -> Prim {
		Prim::new(self, sdf::Path::absolute_root_path())
	}

	pub fn prim_at_path(&self, path: impl Into<sdf::Path>) -> Prim {
		Prim::new(self, path.into())
	}

	pub(crate) fn data(&self) -> &dyn AbstractData {
		&*self.data
	}
}
