//! Scene Description Foundations

mod abstract_data;
mod list_op;
mod path;
mod path_node;
mod path_parser;
mod retiming;
mod schema;

pub use abstract_data::*;
pub use list_op::*;
pub use path::*;
pub use retiming::*;
pub use schema::{CHILDREN_KEYS, FIELD_KEYS};

use crate::vt;

/// An enum that specifies the type of an object.
/// Objects have fields and are adressable by path.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SpecType {
	// Note: Do not change the order, it is used in the binary format!
	Unknown = 0,
	Attribute,
	Connection,
	Expression,
	Mapper,
	MapperArg,
	Prim,
	PseudoRoot,
	Relationship,
	RelationshipTarget,
	Variant,
	VariantSet,
}

/// An enum that identifies the possible specifiers for a PrimSpec.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Specifier {
	Def,
	Over,
	Class,
}

/// An enum that defines permission levels.
#[derive(Debug, Clone, Copy)]
pub enum Permission {
	Public,
	Private,
}

/// An enum that identifies variability types for attributes.
#[derive(Debug, Clone, Copy)]
pub enum Variability {
	Varying,
	Uniform,
}

/// Represents a reference and all its meta data.
#[derive(Debug, Default, Clone)]
pub struct Reference {
	/// The asset path to the external layer.
	pub asset_path: String,
	/// The path to the referenced prim in the external layer.
	pub prim_path: Path,
	/// The layer offset to transform time.
	pub layer_offset: Retiming,
	/// The custom data associated with the reference.
	pub custom_data: vt::Dictionary,
}

/// Represents a payload and all its meta data.
#[derive(Debug, Default, Clone)]
pub struct Payload {
	/// The asset path to the external layer.
	pub asset_path: String,
	/// The root prim path to the referenced prim in the external layer.
	pub prim_path: Path,
	/// The layer offset to transform time.
	pub layer_offset: Retiming,
}

/// A single relocate specifying a source and target path for a relocation.
pub struct Relocate {
	pub source: Path,
	pub target: Path,
}

/// Contains an asset path and optional evaluated and resolved paths.
#[derive(Debug, Clone, PartialEq)]
pub struct AssetPath {
	pub authored_path: String,
	pub evaluated_path: String,
	pub resolved_path: String,
}

impl AssetPath {
	/// Return the asset path. If the the evaluated path is not empty, it will
	/// be returned, otherwise the raw, authored path is returned. The value this
	/// function returns is the exact input that is passed to asset resolution.
	pub fn asset_path(&self) -> &String {
		if !self.evaluated_path.is_empty() {
			&self.evaluated_path
		} else {
			&self.authored_path
		}
	}
}
