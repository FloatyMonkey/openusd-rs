use super::Property;
use crate::{sdf, usd, vt};

/// A [`usd::Relationship`] creates dependencies between scenegraph objects by allowing a prim to target other prims, attributes, or relationships.
#[repr(transparent)]
pub struct Relationship<'a>(Property<'a>);

impl<'a> Relationship<'a> {
	pub(crate) fn new(stage: &'a usd::Stage, path: sdf::Path) -> Self {
		Relationship(Property::new(stage, path))
	}
}

impl<'a> Relationship<'a> {
    pub fn targets(&self) -> Option<vt::Array<sdf::Path>> {
        self.metadata::<vt::Array<sdf::Path>>(&sdf::FIELD_KEYS.target_paths)
    }
}

impl<'a> std::ops::Deref for Relationship<'a> {
	type Target = Property<'a>;
	fn deref(&self) -> &Self::Target {
		unsafe { std::mem::transmute(self) }
	}
}
