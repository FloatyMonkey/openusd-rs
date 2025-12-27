use super::Property;
use crate::{
	sdf, tf, usd,
	vt::{self, ValueType},
};

/// Scenegraph object for authoring and retrieving numeric, string, and array valued data, sampled over time, or animated by a spline.
#[repr(transparent)]
pub struct Attribute<'a>(Property<'a>);

impl<'a> Attribute<'a> {
	pub(crate) fn new(stage: &'a usd::Stage, path: sdf::Path) -> Self {
		Attribute(Property::new(stage, path))
	}

	#[track_caller]
	pub fn get<T: ValueType>(&self) -> T {
		self.stage()
			.resolve_value(self.path(), &sdf::FIELD_KEYS.default)
			.unwrap()
			.get::<T>()
			.unwrap()
	}

	pub fn try_get<T: ValueType>(&self) -> Option<T> {
		self.stage()
			.resolve_value(self.path(), &sdf::FIELD_KEYS.default)
			.and_then(|v| v.get::<T>())
	}

	pub fn get_value(&self) -> Option<vt::Value> {
		self.stage()
			.resolve_value(self.path(), &sdf::FIELD_KEYS.default)
	}

	pub fn type_name(&self) -> tf::Token {
		self.metadata(&sdf::FIELD_KEYS.type_name)
			.unwrap_or_default()
	}
}

impl<'a> std::ops::Deref for Attribute<'a> {
	type Target = Property<'a>;
	fn deref(&self) -> &Self::Target {
		unsafe { std::mem::transmute(self) }
	}
}
