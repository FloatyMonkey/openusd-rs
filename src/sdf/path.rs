use super::path_node::*;
use crate::tf;

/// A path value used to locate objects in layers or scenegraphs.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Path {
	pub(super) prim: PoolHandle,
	pub(super) prop: PoolHandle,
}

impl Path {
	/// The empty path value.
	pub const fn empty_path() -> Self {
		Self {
			prim: INVALID_NODE_HANDLE,
			prop: INVALID_NODE_HANDLE,
		}
	}

	/// The absolute path representing the top of the namespace hierarchy.
	pub const fn absolute_root_path() -> Self {
		Self {
			prim: ABSOLUTE_ROOT_NODE_HANDLE,
			prop: INVALID_NODE_HANDLE,
		}
	}

	/// The relative path representing "self".
	pub const fn reflexive_relative_path() -> Self {
		Self {
			prim: RELATIVE_ROOT_NODE_HANDLE,
			prop: INVALID_NODE_HANDLE,
		}
	}

	/// Returns true if this path is the [`Path::empty_path`].
	pub fn is_empty(&self) -> bool {
		self.prim == INVALID_NODE_HANDLE && self.prop == INVALID_NODE_HANDLE
	}

	/// Returns true if this path is the [`Path::absolute_root_path`].
	pub fn is_absolute_root(&self) -> bool {
		*self == Path::absolute_root_path()
	}

	/// Creates a path by appending an element for `child_name` to this path.
	///
	/// This path must be a prim path, the AbsoluteRootPath or the ReflexiveRelativePath.
	pub fn append_child(&self, child_name: &tf::Token) -> Self {
		if self.prop != INVALID_NODE_HANDLE {
			return Self::empty_path();
		}

		Self {
			prim: find_or_create_path_node(
				&PATH_PRIM_PART_POOL,
				Some(self.prim),
				&PathNodeData::Prim {
					name: child_name.clone(),
				},
			),
			prop: INVALID_NODE_HANDLE,
		}
	}

	/// Creates a path by appending an element for `prop_name` to this path.
	///
	/// This path must be a prim path or the ReflexiveRelativePath.
	pub fn append_property(&self, prop_name: &tf::Token) -> Self {
		if self.prop != INVALID_NODE_HANDLE {
			return Self::empty_path();
		}

		Self {
			prim: self.prim,
			prop: find_or_create_path_node(
				&PATH_PROP_PART_POOL,
				None,
				&PathNodeData::PrimProperty {
					name: prop_name.clone(),
				},
			),
		}
	}

	/// Creates a path by appending an element for `variant_set` and `variant` to this path.
	///
	/// This path must be a prim path.
	pub fn append_variant_selection(&self, variant_set: &str, variant: &str) -> Self {
		Self {
			prim: find_or_create_path_node(
				&PATH_PRIM_PART_POOL,
				Some(self.prim),
				&PathNodeData::PrimVariantSelection {
					variant_set: tf::Token::new(variant_set),
					variant_name: tf::Token::new(variant),
				},
			),
			prop: INVALID_NODE_HANDLE,
		}
	}

	/// Creates a path by appending an element for `target_path` to this path.
	///
	/// This path must be a prim property or relational attribute path.
	pub fn append_target(&self, target_path: &Path) -> Self {
		Self {
			prim: self.prim,
			prop: find_or_create_path_node(
				&PATH_PROP_PART_POOL,
				Some(self.prop),
				&PathNodeData::Target {
					target_path: target_path.clone(),
				},
			),
		}
	}

	/// Creates a path by appending an element for `attr_name` to this path.
	///
	/// This path must be a target path.
	pub fn append_relational_attribute(&self, attr_name: &tf::Token) -> Self {
		Self {
			prim: self.prim,
			prop: find_or_create_path_node(
				&PATH_PROP_PART_POOL,
				Some(self.prop),
				&PathNodeData::RelationalAttribute {
					name: attr_name.clone(),
				},
			),
		}
	}

	/// Creates a path by appending an element for `target_path` to this path.
	///
	/// This path must be a prim property or relational attribute path.
	pub fn append_mapper(&self, target_path: &Path) -> Self {
		Self {
			prim: self.prim,
			prop: find_or_create_path_node(
				&PATH_PROP_PART_POOL,
				Some(self.prop),
				&PathNodeData::Mapper {
					target_path: target_path.clone(),
				},
			),
		}
	}

	/// Creates a path by appending an element for `arg_name` to this path.
	///
	/// This path must be a mapper path.
	pub fn append_mapper_arg(&self, arg_name: &tf::Token) -> Self {
		Self {
			prim: self.prim,
			prop: find_or_create_path_node(
				&PATH_PROP_PART_POOL,
				Some(self.prop),
				&PathNodeData::MapperArg {
					name: arg_name.clone(),
				},
			),
		}
	}

	/// Creates a path by appending an expression element to this path.
	///
	/// This path must be a prim property or relational attribute path.
	pub fn append_expression(&self) -> Self {
		Self {
			prim: self.prim,
			prop: find_or_create_path_node(
				&PATH_PROP_PART_POOL,
				Some(self.prop),
				&PathNodeData::Expression,
			),
		}
	}

	/// Return the path that identifies this path's namespace parent.
	pub fn parent_path(&self) -> Self {
		if self.is_empty() {
			return Self::empty_path();
		}

		// If this is a property-like path, trim that first.
		if self.prop != INVALID_NODE_HANDLE {
			let prop_pool = PATH_PROP_PART_POOL.read().unwrap();
			let prop_node = prop_pool.get(self.prop).unwrap();

			return Self {
				prim: self.prim,
				prop: prop_node.parent,
			};
		}

		// This is a prim-like path. If this is an absolute path (most common case)
		// then it's just the parent path node. On the other hand if this path is a
		// relative path, and is '.' or ends with '..', the logical parent path is
		// made by appending a '..' component.
		let prim_pool = PATH_PRIM_PART_POOL.read().unwrap();
		let prim_node = prim_pool.get(self.prim).unwrap();

		if prim_node.is_absolute_path()
			|| (self.prim != RELATIVE_ROOT_NODE_HANDLE
				&& match &prim_node.data {
					PathNodeData::Prim { name } => name.as_str() != "..",
					_ => true,
				}) {
			return Self {
				prim: prim_node.parent,
				prop: INVALID_NODE_HANDLE,
			};
		}

		// Is relative root '.' or ends with '..'.
		self.append_child(&tf::Token::new(".."))
	}

	pub fn name(&self) -> String {
		if self.prop != INVALID_NODE_HANDLE {
			let prop_pool = PATH_PROP_PART_POOL.read().unwrap();
			let prop_node = prop_pool.get(self.prop).unwrap();
			return prop_node.name().to_string();
		}

		if self.prim != INVALID_NODE_HANDLE {
			let prim_pool = PATH_PRIM_PART_POOL.read().unwrap();
			let prim_node = prim_pool.get(self.prim).unwrap();
			return prim_node.name().to_string();
		}

		"".to_string()
	}
}

impl Default for Path {
	fn default() -> Self {
		Self::empty_path()
	}
}

impl From<&str> for Path {
	fn from(s: &str) -> Self {
		super::path_parser::parse_path(s).unwrap_or_else(|_| Self::empty_path())
	}
}

impl std::fmt::Display for Path {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{}", PathNode::path_string(self.prim, self.prop))
	}
}

#[cfg(test)]
#[rustfmt::skip]
mod tests {
	use super::*;

	fn p(s: &str) -> Path {
		Path::from(s)
	}

	fn t(s: &str) -> tf::Token {
		tf::Token::new(s)
	}

	#[test]
	fn append_child() {
		assert_eq!(p("/foo").append_child(&t("bar")), p("/foo/bar"));
		assert_eq!(p("foo").append_child(&t("bar")), p("foo/bar"));
		assert_eq!(p("/foo.prop").append_child(&t("bar")), Path::empty_path());
	}

	#[test]
	fn append_property() {
		assert_eq!(p("/foo").append_property(&t("prop")), p("/foo.prop"));
		assert_eq!(p("/foo").append_property(&t("prop:foo:bar")), p("/foo.prop:foo:bar"));
		assert_eq!(p("/foo.prop").append_property(&t("prop2")), Path::empty_path());
		assert_eq!(p("/foo.prop").append_property(&t("prop2:foo:bar")), Path::empty_path());
	}

	#[test]
	fn parent_path() {
		assert_eq!(p("/foo").parent_path(), Path::absolute_root_path());
		assert_eq!(p("/foo/bar").parent_path(), p("/foo"));
		assert_eq!(p("foo/bar").parent_path(), p("foo"));
		assert_eq!(p("/foo.prop").parent_path(), p("/foo"));
		assert_eq!(p("foo.prop").parent_path(), p("foo"));
		assert_eq!(p("/foo.prop:bar").parent_path(), p("/foo"));
	}

	#[test]
	fn print() {
		assert_eq!(p("/foo").to_string(), "/foo");
		assert_eq!(p("/foo/bar").to_string(), "/foo/bar");
		assert_eq!(p("foo/bar").to_string(), "foo/bar");
		assert_eq!(p("/foo.prop").to_string(), "/foo.prop");
	}
}
