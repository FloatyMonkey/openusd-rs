use super::Object;
use crate::{sdf, usd, vt};

/// Base class for [`usd::Attribute`] and [`usd::Relationship`] scenegraph objects.
#[repr(transparent)]
pub struct Property<'a>(Object<'a>);

impl<'a> Property<'a> {
	pub(crate) fn new(stage: &'a usd::Stage, path: sdf::Path) -> Self {
		Property(Object::new(stage, path))
	}

	/// Return the authored value of this property (if any).
	pub fn get_value(&self) -> Option<vt::Value> {
		// same as Attribute::get_value, but lifted to Property
		self.stage()
			.data()
			.get(&self.path(), &sdf::FIELD_KEYS.default)
	}
}

impl<'a> std::ops::Deref for Property<'a> {
	type Target = Object<'a>;
	fn deref(&self) -> &Self::Target {
		unsafe { std::mem::transmute(self) }
	}
}
