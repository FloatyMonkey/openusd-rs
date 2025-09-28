use super::Property;
use crate::{sdf, usd};

/// A [`usd::Relationship`] creates dependencies between scenegraph objects by allowing a prim to target other prims, attributes, or relationships.
#[repr(transparent)]
pub struct Relationship<'a>(Property<'a>);

impl<'a> Relationship<'a> {
	pub(crate) fn new(stage: &'a usd::Stage, path: sdf::Path) -> Self {
		Relationship(Property::new(stage, path))
	}
}

/// Editing Relationships at current EditTarget
impl<'a> Relationship<'a> {
	/// Compose this relationship's targets and return the result.
	pub fn targets(&self) -> Vec<sdf::Path> {
		self.metadata::<sdf::PathListOp>(&sdf::FIELD_KEYS.target_paths)
			.unwrap_or_default()
			.explicit_items
	}
}

impl<'a> Clone for Relationship<'a> {
	fn clone(&self) -> Self {
		Relationship(self.0.clone())
	}
}
impl<'a> std::ops::Deref for Relationship<'a> {
	type Target = Property<'a>;
	fn deref(&self) -> &Self::Target {
		unsafe { std::mem::transmute(self) }
	}
}
