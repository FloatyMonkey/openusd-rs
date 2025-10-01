use super::path_node::*;
use crate::tf;

/// A path value used to locate objects in layers or scenegraphs.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
}

/// Querying paths.
impl Path {
	/// Returns true if this path is the [`Self::empty_path`].
	pub fn is_empty(&self) -> bool {
		*self == Self::empty_path()
	}

	/// Returns true if this path is the [`Self::absolute_root_path`].
	pub fn is_absolute_root(&self) -> bool {
		*self == Self::absolute_root_path()
	}

	/// Returns whether the path identifies a prim.
	pub fn is_prim_path(&self) -> bool {
		self.prop == INVALID_NODE_HANDLE
			&& self.prim != INVALID_NODE_HANDLE
			&& ({
				let prim_pool = PATH_PRIM_PART_POOL.read().unwrap();
				let prim_node = prim_pool.get(self.prim).unwrap();
				matches!(prim_node.data, PathNodeData::Prim { .. })
			} || *self == Self::reflexive_relative_path())
	}

	/// Returns whether the path identifies a property.
	///
	/// A relational attribute is considered to be a property,
	/// so this method will return true for relational attributes as well as properties of prims.
	pub fn is_property_path(&self) -> bool {
		self.prop != INVALID_NODE_HANDLE && {
			let prop_pool = PATH_PROP_PART_POOL.read().unwrap();
			let prop_node = prop_pool.get(self.prop).unwrap();
			matches!(
				prop_node.data,
				PathNodeData::PrimProperty { .. } | PathNodeData::RelationalAttribute { .. }
			)
		}
	}

	/// Returns whether the path identifies a prim's property.
	///
	/// A relational attribute is not a prim property.
	pub fn is_prim_property_path(&self) -> bool {
		self.prop != INVALID_NODE_HANDLE && {
			let prop_pool = PATH_PROP_PART_POOL.read().unwrap();
			let prop_node = prop_pool.get(self.prop).unwrap();
			matches!(prop_node.data, PathNodeData::PrimProperty { .. })
		}
	}

	/// Returns whether the path or any of its parent paths identifies a variant selection for a prim.
	pub fn contains_prim_variant_selection(&self) -> bool {
		let prim_pool = PATH_PRIM_PART_POOL.read().unwrap();
		prim_pool
			.get(self.prim)
			.is_some_and(|node| node.contains_prim_variant_selection())
	}

	/// Returns whether this path is or has a prefix that's a target path or a mapper path.
	pub fn contains_target_path(&self) -> bool {
		let prop_pool = PATH_PROP_PART_POOL.read().unwrap();
		prop_pool
			.get(self.prop)
			.is_some_and(|node| node.contains_target_path())
	}

	/// Returns whether the path identifies a relationship or connection target.
	pub fn is_target_path(&self) -> bool {
		self.prop != INVALID_NODE_HANDLE && {
			let prop_pool = PATH_PROP_PART_POOL.read().unwrap();
			let prop_node = prop_pool.get(self.prop).unwrap();
			matches!(prop_node.data, PathNodeData::Target { .. })
		}
	}

	/// Return whether this path and `prefix` are not the empty path and this path has `prefix` as a prefix.
	pub fn has_prefix(&self, prefix: &Self) -> bool {
		if self.is_empty() || prefix.is_empty() {
			return false;
		}

		if prefix.prop != INVALID_NODE_HANDLE {
			// The prefix is a property-like path, in order for it to be a prefix of
			// this path, we must also have a property part, and our prim part must
			// be the same as the prefix's prim part.
			if self.prim != prefix.prim || self.prop == INVALID_NODE_HANDLE {
				return false;
			}

			// Now walk up property parts until we hit prefix.prop or we recurse above its depth.
			let prop_pool = PATH_PROP_PART_POOL.read().unwrap();
			let mut prop_node = prop_pool.get(self.prop);
			let prefix_prop_node = prop_pool.get(prefix.prop);

			while prop_node.is_some() && prop_node != prefix_prop_node {
				prop_node = prop_pool.get(prop_node.unwrap().parent);
			}

			prop_node == prefix_prop_node
		} else {
			// The prefix is a prim-like path. Walk up nodes until we achieve the
			// same depth as the prefix, then just check for equality.

			let prim_pool = PATH_PRIM_PART_POOL.read().unwrap();
			let mut prim_node = prim_pool.get(self.prim).unwrap();

			if prim_node.is_absolute_path() && prefix.is_absolute_root() {
				return true;
			}

			let prefix_prim_node = prim_pool.get(prefix.prim).unwrap();

			let prefix_depth = prefix_prim_node.element_count();
			let mut cur_depth = prim_node.element_count();

			if cur_depth < prefix_depth {
				return false;
			}

			while cur_depth > prefix_depth {
				prim_node = prim_pool.get(prim_node.parent).unwrap();
				cur_depth -= 1;
			}

			prim_node == prefix_prim_node
		}
	}

	/// Return a range for iterating over the ancestors of this path.
	///
	/// The range provides iteration over the prefixes of a path, ordered from longest to shortest.
	/// Starting with the path itself and ending with a single element path, not including the empty/root path.
	pub fn ancestors_range(&self) -> PathAncestorsRange {
		PathAncestorsRange { path: self.clone() }
	}

	/// Returns the name of the prim, property or relational attribute identified by the path.
	pub fn name_token(&self) -> tf::Token {
		if self.prop != INVALID_NODE_HANDLE {
			let prop_pool = PATH_PROP_PART_POOL.read().unwrap();
			let prop_node = prop_pool.get(self.prop).unwrap();
			return prop_node.name().clone();
		}

		if self.prim != INVALID_NODE_HANDLE {
			let prim_pool = PATH_PRIM_PART_POOL.read().unwrap();
			let prim_node = prim_pool.get(self.prim).unwrap();
			return prim_node.name().clone();
		}

		tf::Token::empty()
	}

	/// Returns a string representation of the "terminal" element of this path.
	///
	/// Empty, absolute root and reflexive relative paths are *not* considered elements
	/// (one of the defining properties of elements is that they have a parent),
	/// so this function will return an empty token for these paths.
	///
	/// Unlike [`Self::name_token`], which provides you "some" information about the terminal element,
	/// this provides a complete representation of the element, for all element types.
	pub fn element_token(&self) -> tf::Token {
		if self.prop != INVALID_NODE_HANDLE {
			let prop_pool = PATH_PROP_PART_POOL.read().unwrap();
			let prop_node = prop_pool.get(self.prop).unwrap();
			return prop_node.element().clone();
		}

		if self.prim != INVALID_NODE_HANDLE {
			let prim_pool = PATH_PRIM_PART_POOL.read().unwrap();
			let prim_node = prim_pool.get(self.prim).unwrap();
			return prim_node.element().clone();
		}

		tf::Token::empty()
	}
}

/// Creating new paths by modifying existing paths.
impl Path {
	/// Return the path that identifies this path's namespace parent.
	///
	/// Note that the parent path of a relative parent path (`..`) is a relative grandparent path (`../..`).
	/// Use caution writing loops that walk to parent paths since relative paths have infinitely many ancestors.
	/// To more safely traverse ancestor paths, consider using [`Self::ancestors_range`].
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

		let parent = prim_node.parent;
		drop(prim_pool);

		Self {
			prim: find_or_create_path_node(
				&PATH_PRIM_PART_POOL,
				Some(parent),
				&PathNodeData::Prim {
					name: tf::Token::new(".."),
				},
			),
			prop: INVALID_NODE_HANDLE,
		}
	}

	/// Creates a path by stripping all relational attributes, targets,
	/// properties, and variant selections from the leafmost prim path,
	/// leaving the nearest path for which [`Self::is_prim_path`] returns true.
	///
	/// If the path is already a prim path, the same path is returned.
	pub fn prim_path(&self) -> Self {
		let prim_pool = PATH_PRIM_PART_POOL.read().unwrap();
		let mut prim_node = prim_pool.get(self.prim);
		let mut prim_handle = self.prim;

		while let Some(node) = prim_node {
			if matches!(node.data, PathNodeData::Prim { .. }) {
				break;
			}
			prim_node = prim_pool.get(node.parent);
			prim_handle = node.parent;
		}

		Self {
			prim: prim_handle,
			prop: INVALID_NODE_HANDLE,
		}
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

pub struct PathAncestorsRange {
	path: Path,
}

impl std::iter::Iterator for PathAncestorsRange {
	type Item = Path;

	fn next(&mut self) -> Option<Self::Item> {
		if self.path.is_empty() {
			return None;
		}

		let current_path = self.path.clone();

		let prim_pool = PATH_PRIM_PART_POOL.read().unwrap();
		let prop_pool = PATH_PROP_PART_POOL.read().unwrap();

		let mut prim_handle = INVALID_NODE_HANDLE;
		let mut prop_handle = INVALID_NODE_HANDLE;

		if self.path.prop != INVALID_NODE_HANDLE {
			prop_handle = prop_pool.get(self.path.prop).unwrap().parent;
			prim_handle = self.path.prim;
		} else if self.path.prim != INVALID_NODE_HANDLE {
			let prim_node = prim_pool.get(self.path.prim).unwrap();
			if prim_node.element_count() > 1 {
				prim_handle = prim_node.parent;
			}
		}

		self.path = Path {
			prim: prim_handle,
			prop: prop_handle,
		};

		Some(current_path)
	}
}

#[cfg(test)]
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
		assert_eq!(
			p("/foo").append_property(&t("prop:foo:bar")),
			p("/foo.prop:foo:bar")
		);
		assert_eq!(
			p("/foo.prop").append_property(&t("prop2")),
			Path::empty_path()
		);
		assert_eq!(
			p("/foo.prop").append_property(&t("prop2:foo:bar")),
			Path::empty_path()
		);
	}

	#[test]
	fn has_prefix() {
		let path = p("/foo/bar/baz.prop");

		assert!(!Path::empty_path().has_prefix(&path));
		assert!(!path.has_prefix(&Path::empty_path()));

		assert!(path.has_prefix(&p("/foo/bar/baz.prop")));
		assert!(path.has_prefix(&p("/foo/bar/baz")));
		assert!(path.has_prefix(&p("/foo/bar")));
		assert!(path.has_prefix(&p("/foo")));

		assert!(!path.has_prefix(&p("/bar/baz")));
		assert!(!path.has_prefix(&p("/bar")));
	}

	#[test]
	fn parent_path() {
		assert_eq!(Path::empty_path().parent_path(), Path::empty_path());
		assert_eq!(p("/foo").parent_path(), Path::absolute_root_path());
		assert_eq!(p("/foo/bar").parent_path(), p("/foo"));
		assert_eq!(p("foo/bar").parent_path(), p("foo"));
		assert_eq!(p("/foo.prop").parent_path(), p("/foo"));
		assert_eq!(p("foo.prop").parent_path(), p("foo"));
		assert_eq!(p("/foo.prop:bar").parent_path(), p("/foo"));
	}

	#[test]
	fn prim_path() {
		assert_eq!(Path::empty_path().prim_path(), Path::empty_path());
		assert_eq!(p("/foo").prim_path(), p("/foo"));
		assert_eq!(p("/foo/bar").prim_path(), p("/foo/bar"));
		assert_eq!(p("/foo.prop").prim_path(), p("/foo"));
		assert_eq!(p("/foo.prop:bar").prim_path(), p("/foo"));
		assert_eq!(p("/foo{set=sel}").prim_path(), p("/foo"));
		assert_eq!(p("/foo{set=sel}.prop").prim_path(), p("/foo"));
		assert_eq!(p("/foo{set=sel}.prop:bar").prim_path(), p("/foo"));
	}

	#[test]
	fn ancestors_range() {
		let path = p("/foo/bar/baz");
		let mut ancestors = path.ancestors_range();
		assert_eq!(ancestors.next(), Some(p("/foo/bar/baz")));
		assert_eq!(ancestors.next(), Some(p("/foo/bar")));
		assert_eq!(ancestors.next(), Some(p("/foo")));
		assert_eq!(ancestors.next(), None);

		let path = p("/foo/bar/baz.prop");
		let mut ancestors = path.ancestors_range();
		assert_eq!(ancestors.next(), Some(p("/foo/bar/baz.prop")));
		assert_eq!(ancestors.next(), Some(p("/foo/bar/baz")));
		assert_eq!(ancestors.next(), Some(p("/foo/bar")));
		assert_eq!(ancestors.next(), Some(p("/foo")));
		assert_eq!(ancestors.next(), None);
	}

	#[test]
	fn element_token() {
		assert!(Path::empty_path().element_token().is_empty());
		assert!(Path::absolute_root_path().element_token().is_empty());
		assert!(Path::reflexive_relative_path().element_token().is_empty());

		assert_eq!(p("/foo").element_token(), t("foo"));
		assert_eq!(p("/foo.prop").element_token(), t(".prop"));
		assert_eq!(p("/foo{set=sel}").element_token(), t("{set=sel}"));
	}

	#[test]
	fn print() {
		assert_eq!(p("/foo").to_string(), "/foo");
		assert_eq!(p("/foo/bar").to_string(), "/foo/bar");
		assert_eq!(p("foo/bar").to_string(), "foo/bar");
		assert_eq!(p("/foo.prop").to_string(), "/foo.prop");
	}
}
