use super::Prim;
use crate::{
	sdf, tf, usd,
	vt::{self, ValueType},
};

/// Base class for Usd scenegraph objects, providing common API.
///
/// The commonality between the three types of scenegraph objects in Usd
/// ([`usd::Prim`], [`usd::Attribute`], [`usd::Relationship`]) is that they
/// can all have metadata. Other objects in the API simply are kinds of metadata.
///
/// **Inheritance hierarchy:**
/// - [`usd::Object`]
///   - [`usd::Prim`]
///   - [`usd::Property`]
///     - [`usd::Attribute`]
///     - [`usd::Relationship`]
pub struct Object<'a> {
	stage: &'a usd::Stage,
	path: sdf::Path,
}

impl<'a> Object<'a> {
	pub(crate) fn new(stage: &'a usd::Stage, path: sdf::Path) -> Self {
		Object { stage, path }
	}

	/// Return the stage that owns the object, and to whose state and lifetime this object's validity is tied.
	pub fn stage(&self) -> &usd::Stage {
		self.stage
	}

	/// Return the full name of this object, i.e. the last component of its [`sdf::Path`] in namespace.
	pub fn name(&self) -> tf::Token {
		self.path.name_token()
	}

	/// Return the complete scene path to this object on its [`usd::Stage`].
	pub fn path(&self) -> &sdf::Path {
		&self.path
	}

	/// Return this object if it is a prim, otherwise return this object's nearest owning prim.
	pub fn prim(&self) -> Prim<'_> {
		Prim::new(self.stage(), self.path().prim_path())
	}

	/// Return the requested metadatum named `key`.
	pub fn metadata<T: ValueType>(&self, key: &tf::Token) -> Option<T> {
		self.stage()
			.data()
			.get(self.path(), key)
			.and_then(|v| v.get::<T>())
	}

	/// Return this object's documentation (metadata).
	///
	/// This returns the empty string if no documentation has been set.
	pub fn documentation(&self) -> String {
		self.metadata(&sdf::FIELD_KEYS.documentation)
			.unwrap_or_default()
	}

	/// Return this object's composed customData dictionary.
	pub fn custom_data(&self) -> vt::Dictionary {
		self.metadata(&sdf::FIELD_KEYS.custom_data)
			.unwrap_or_default()
	}

	#[doc(hidden)]
	pub fn spec_type(&self) -> Option<sdf::SpecType> {
		self.stage().data().spec_type(self.path())
	}
}
