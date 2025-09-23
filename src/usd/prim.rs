use super::{Attribute, Object, Property, Relationship};
use crate::{sdf, tf, usd, vt};

/// [`usd::Prim`] is the sole persistent scenegraph object on a [`usd::Stage`],
/// and is the embodiment of a "Prim" as described in the *Universal Scene Description Composition Compendium*.
#[repr(transparent)]
pub struct Prim<'a>(Object<'a>);

impl<'a> Prim<'a> {
	pub(crate) fn new(stage: &'a usd::Stage, path: sdf::Path) -> Self {
		Prim(Object::new(stage, path))
	}

	pub fn specifier(&self) -> Option<sdf::Specifier> {
		self.stage()
			.data()
			.get(self.path(), &sdf::FIELD_KEYS.specifier)
			.map(|v| v.get::<sdf::Specifier>())
			.flatten()
	}

	pub fn children<'b>(&'b self) -> ChildrenIter<'b> {
		ChildrenIter::new(self.stage(), self.path())
	}

	pub fn type_name(&self) -> tf::Token {
		self.metadata(&sdf::FIELD_KEYS.type_name)
			.unwrap_or_default()
	}

	/// Return a [`usd::Property`] with the given `name`.
	pub fn property<'b>(&'b self, name: &tf::Token) -> Property<'b> {
		Property::new(self.stage(), self.path().append_property(name))
	}

	/// Return a [`usd::Attribute`] with the given `name`.
	pub fn attribute<'b>(&'b self, name: &tf::Token) -> Attribute<'b> {
		Attribute::new(self.stage(), self.path().append_property(name))
	}

	pub fn has_attribute(&self, name: &tf::Token) -> bool {
		self.stage()
			.data()
			.get(&self.path().append_property(name), &sdf::FIELD_KEYS.default)
			.is_some()
	}
	pub fn has_property(&self, name: &str) -> bool {
		self.properties().iter().any(|p| p.name() == name)
	}

	/// Return all authored properties (USD attributes) on this prim.
	pub fn properties(&self) -> Vec<usd::Attribute<'_>> {
		let mut out = Vec::new();
		let data = self.stage().data();

		let props_tok = tf::Token::new("properties");
		if let Some(val) = data.get(self.path(), &props_tok) {
			// In USD, "properties" is an array of token names
			if let Some(arr) = val.get::<vt::Array<tf::Token>>() {
				for tok in arr.iter() {
					out.push(self.attribute(tok));
				}
			}
		}

		out
	}
}

/// Relationships
impl<'a> Prim<'a> {
	/// Return a [`usd::Relationship`] with the given `name`.
	pub fn relationship<'b>(&'b self, name: &tf::Token) -> Relationship<'b> {
		Relationship::new(self.stage(), self.path().append_property(name))
	}
}

impl<'a> std::ops::Deref for Prim<'a> {
	type Target = Object<'a>;
	fn deref(&self) -> &Self::Target {
		unsafe { std::mem::transmute(self) }
	}
}

pub struct ChildrenIter<'a> {
	stage: &'a usd::Stage,
	base_path: sdf::Path,
	prim_children: vt::Array<tf::Token>,
	index: usize,
}

impl<'a> ChildrenIter<'a> {
	pub fn new(stage: &'a usd::Stage, path: &sdf::Path) -> Self {
		ChildrenIter {
			stage,
			base_path: path.clone(),
			prim_children: stage
				.data()
				.get(&path, &sdf::CHILDREN_KEYS.prim_children)
				.map(|v| v.get::<vt::Array<tf::Token>>())
				.flatten()
				.unwrap_or_default(),
			index: 0,
		}
	}
}

impl<'a> Iterator for ChildrenIter<'a> {
	type Item = Prim<'a>;

	fn next(&mut self) -> Option<Self::Item> {
		if self.index < self.prim_children.len() {
			let path = self.prim_children[self.index].clone();
			self.index += 1;
			Some(Prim::new(self.stage, self.base_path.append_child(&path)))
		} else {
			None
		}
	}
}
