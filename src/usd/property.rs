use super::Object;
use crate::{sdf, usd};

/// Base class for [`usd::Attribute`] and [`usd::Relationship`] scenegraph objects.
#[repr(transparent)]
pub struct Property<'a>(Object<'a>);

impl<'a> Property<'a> {
	pub(crate) fn new(stage: &'a usd::Stage, path: sdf::Path) -> Self {
		Property(Object::new(stage, path))
	}
}

impl<'a> std::ops::Deref for Property<'a> {
	type Target = Object<'a>;
	fn deref(&self) -> &Self::Target {
		unsafe { std::mem::transmute(self) }
	}
}
