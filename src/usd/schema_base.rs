use crate::usd;

/// The base class for all schema types in Usd.
///
/// Schema objects hold a [`usd::Prim`] internally and provide a
/// layer of specific named API atop the underlying scene graph.
#[repr(transparent)]
pub struct SchemaBase<'a>(usd::Prim<'a>);

impl<'a> SchemaBase<'a> {
	pub(crate) fn new(prim: usd::Prim<'a>) -> Self {
		SchemaBase(prim)
	}

	pub fn prim(&self) -> &usd::Prim<'a> {
		&self.0
	}
}
