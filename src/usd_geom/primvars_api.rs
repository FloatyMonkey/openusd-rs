use super::primvar::{Primvar, TOKENS};
use crate::{tf, usd};

/// [`PrimvarsApi`] encodes geometric "primitive variables" as [`Primvar`],
/// which interpolate across a primitive's topology,
/// can override shader inputs, and inherit down namespace.
pub struct PrimvarsApi<'a> {
	pub prim: &'a usd::Prim<'a>,
}

impl<'a> PrimvarsApi<'a> {
	/// Construct a [`PrimvarsApi`] for the given [`usd::Prim`].
	pub fn new(prim: &'a usd::Prim<'a>) -> Self {
		Self { prim }
	}

	/// Return the [`Primvar`] object named by `name`.
	///
	/// Name lookup will account for Primvar namespacing.
	pub fn primvar(&self, name: &tf::Token) -> Primvar<'_> {
		let attr_name = make_namespaced(name);
		Primvar::new(self.prim.attribute(&attr_name))
	}

	/// Is there a defined [`Primvar`] `name` on this prim?
	///
	/// Name lookup will account for Primvar namespacing.
	pub fn has_primvar(&self, name: &tf::Token) -> bool {
		let attr_name = make_namespaced(name);
		self.prim.has_attribute(&attr_name)
	}
}

fn make_namespaced(name: &tf::Token) -> tf::Token {
	if name.as_str().starts_with(TOKENS.primvars_prefix.as_str()) {
		name.clone()
	} else {
		tf::Token::new(format!("{}{}", TOKENS.primvars_prefix, name))
	}
}
