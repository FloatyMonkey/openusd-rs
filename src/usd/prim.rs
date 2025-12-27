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
		self.metadata(&sdf::FIELD_KEYS.specifier)
	}

	pub fn children<'b>(&'b self) -> ChildrenIter<'b> {
		ChildrenIter::new(self.stage(), self.path())
	}

	pub fn properties<'b>(&'b self) -> PropertyIter<'b> {
		PropertyIter::new(self.stage(), self.path())
	}

	pub fn type_name(&self) -> tf::Token {
		self.metadata(&sdf::FIELD_KEYS.type_name)
			.unwrap_or_default()
	}

	/// Return a [`usd::Property`] with the given `name`.
	pub fn property<'b>(&'b self, name: &tf::Token) -> Property<'b> {
		Property::new(self.stage(), self.path().append_property(name))
	}
}

/// Variants
impl<'a> Prim<'a> {
	/// Return the variant selections that apply to this prim.
	// TODO: Move this to a VariantSets wrapper.
	pub fn variant_selections(&self) -> std::collections::HashMap<String, String> {
		if let Some(index) = self.stage().prim_index(self.path()) {
			index.variant_selections()
		} else {
			std::collections::HashMap::new()
		}
	}
}

/// Attributes
impl<'a> Prim<'a> {
	/// Return a [`usd::Attribute`] with the given `name`.
	pub fn attribute<'b>(&'b self, name: &tf::Token) -> Attribute<'b> {
		Attribute::new(self.stage(), self.path().append_property(name))
	}

	pub fn has_attribute(&self, name: &tf::Token) -> bool {
		self.stage()
			.resolve_value(&self.path().append_property(name), &sdf::FIELD_KEYS.default)
			.is_some()
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
			prim_children: stage.compose_children(path),
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

pub struct PropertyIter<'a> {
	stage: &'a usd::Stage,
	base_path: sdf::Path,
	property_children: vt::Array<tf::Token>,
	index: usize,
}

impl<'a> PropertyIter<'a> {
	pub fn new(stage: &'a usd::Stage, path: &sdf::Path) -> Self {
		PropertyIter {
			stage,
			base_path: path.clone(),
			property_children: stage.compose_properties(path),
			index: 0,
		}
	}
}

impl<'a> Iterator for PropertyIter<'a> {
	type Item = Property<'a>;

	fn next(&mut self) -> Option<Self::Item> {
		if self.index < self.property_children.len() {
			let name = self.property_children[self.index].clone();
			self.index += 1;
			Some(Property::new(
				self.stage,
				self.base_path.append_property(&name),
			))
		} else {
			None
		}
	}
}
