use crate::{declare_public_tokens, tf, usd, vt};

declare_public_tokens!(Tokens, TOKENS, [
	interpolation: "interpolation",
	constant: "constant",
	uniform: "uniform",
	vertex: "vertex",
	varying: "varying",
	face_varying: "faceVarying",
	element_size: "elementSize",

	primvars_prefix: "primvars:",
	indices_suffix: ":indices"
]);

/// Schema wrapper for [`usd::Attribute`] for authoring and introspecting attributes that are primvars.
pub struct Primvar<'a> {
	pub attr: usd::Attribute<'a>,
}

impl<'a> Primvar<'a> {
	pub fn new(attr: usd::Attribute<'a>) -> Self {
		Self { attr }
	}

	/// Return the Primvar's interpolation, which is `constant` if unauthored.
	pub fn interpolation(&self) -> tf::Token {
		self.attr
			.metadata(&TOKENS.interpolation)
			.unwrap_or(TOKENS.constant.clone())
	}

	/// Return the Primvar's element size, which is 1 if unauthored.
	pub fn element_size(&self) -> i32 {
		self.attr.metadata(&TOKENS.element_size).unwrap_or(1)
	}

	/// Test whether a given `name` represents a valid name of a primvar,
	/// which implies that creating a [`Primvar`] with the given name will succeed.
	pub fn is_valid_primvar_name(name: &str) -> bool {
		name.starts_with(TOKENS.primvars_prefix.as_str())
			&& !name.ends_with(TOKENS.indices_suffix.as_str())
	}

	/// Validate that the provided `interpolation` is a valid setting for interpolation.
	pub fn is_valid_interpolation(interpolation: &tf::Token) -> bool {
		interpolation == &TOKENS.constant
			|| interpolation == &TOKENS.uniform
			|| interpolation == &TOKENS.vertex
			|| interpolation == &TOKENS.varying
			|| interpolation == &TOKENS.face_varying
	}
}

/// Indexed primvars API
impl<'a> Primvar<'a> {
	/// Returns the value of the indices array associated with the indexed primvar.
	pub fn indices(&self) -> Option<vt::Array<i32>> {
		let indices_attr_name = tf::Token::new(format!(
			"{}{}",
			self.attr.name().as_str(),
			TOKENS.indices_suffix.as_str()
		));

		if !self.attr.prim().has_attribute(&indices_attr_name) {
			return None;
		}

		self.attr
			.prim()
			.attribute(&indices_attr_name)
			.try_get::<vt::Array<i32>>()
	}
}

impl<'a> From<Primvar<'a>> for usd::Attribute<'a> {
	fn from(primvar: Primvar<'a>) -> Self {
		primvar.attr
	}
}
