//! Scene Description Foundations

mod abstract_data;
mod layer;
mod list_op;
mod path;
mod path_node;
mod path_parser;
mod retiming;
mod schema;

pub use abstract_data::*;
pub use layer::*;
pub use list_op::*;
pub use path::*;
pub use retiming::*;
pub use schema::{CHILDREN_KEYS, FIELD_KEYS};

use crate::vt;

/// An enum that specifies the type of an object.
/// Objects have fields and are adressable by path.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SpecForm {
	// Note: Do not change the order, it is used in the binary format!
	Unknown = 0,
	Attribute = 1,
	// Connection = 2,
	// Expression = 3,
	// Mapper = 4,
	// MapperArg = 5,
	Prim = 6,
	Layer = 7,
	Relationship = 8,
	// RelationshipTarget = 9,
	Variant = 10,
	VariantSet = 11,
}

/// An enum that identifies the possible specifiers for a PrimSpec.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Specifier {
	Def = 0,
	Over = 1,
	Class = 2,
}

/// An enum that identifies variability types for attributes.
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Variability {
	Varying = 0,
	Uniform = 1,
}

/// Represents a reference and all its meta data.
#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct Reference {
	/// The asset path to the external layer.
	pub asset_path: String,
	/// The path to the referenced prim in the external layer.
	pub prim_path: Path,
	/// The layer offset to transform time.
	pub layer_offset: Retiming,
}

/// Represents a payload and all its meta data.
#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct Payload {
	/// The asset path to the external layer.
	pub asset_path: String,
	/// The root prim path to the referenced prim in the external layer.
	pub prim_path: Path,
	/// The layer offset to transform time.
	pub layer_offset: Retiming,
}

/// A single relocate specifying a source and target path for a relocation.
#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
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

/// A special value type that can be used to explicitly author an
/// opinion for an attribute's default value or time sample value
/// that represents having no value. Note that this is different
/// from not having a value authored.
#[derive(Debug, Clone)]
pub struct ValueBlock;

/// A map from sample times to sample values.
pub type TimeSampleMap = Vec<(f64, vt::Value)>;

/// A map of reference variant set names to variants in those sets.
pub type VariantSelectionMap = std::collections::HashMap<String, String>;
