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
			.data()
			.get(&self.path(), &sdf::FIELD_KEYS.default)
			.unwrap()
			.get::<T>()
			.unwrap()
	}

	pub fn get_value(&self) -> Option<vt::Value> {
		self.stage()
			.data()
			.get(&self.path(), &sdf::FIELD_KEYS.default)
	}

	pub fn type_name(&self) -> tf::Token {
		self.metadata(&sdf::FIELD_KEYS.type_name)
			.unwrap_or_default()
	}
	/// Return the attribute/property name (e.g. "points", "normals", "doubleSided").
	pub fn name(&self) -> String {
		let s = self.path().to_string();
		if let Some(idx) = s.rfind('.') {
			s[idx + 1..].to_string()
		} else {
			// if it's a property but somehow no dot, just return the last token after '/'
			s.rsplit('/').next().unwrap_or(&s).to_string()
		}
	}
}

impl<'a> Clone for Attribute<'a> {
	fn clone(&self) -> Self {
		Attribute(self.0.clone())
	}
}
impl<'a> std::ops::Deref for Attribute<'a> {
	type Target = Property<'a>;
	fn deref(&self) -> &Self::Target {
		unsafe { std::mem::transmute(self) }
	}
}
