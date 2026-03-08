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
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SpecForm {
	Unknown,
	Layer,
	Prim,
	Attribute,
	Relationship,
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

/// An enum that identifies variability types for attributes.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Variability {
	Varying,
	Uniform,
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
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct ValueBlock;

/// Value type that represents a time code. It's equivalent to a double type
/// value but is used to indicate that this value should be resolved by any
/// time based value resolution.
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct TimeCode(pub f64);

/// A map from sample times to sample values.
pub type TimeSampleMap = Vec<(f64, vt::Value)>;

/// A map of reference variant set names to variants in those sets.
pub type VariantSelectionMap = std::collections::HashMap<String, String>;
