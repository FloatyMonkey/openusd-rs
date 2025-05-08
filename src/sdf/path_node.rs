use super::Path;
use crate::{declare_public_tokens, tf};
use std::sync::{LazyLock, RwLock};

declare_public_tokens!(PathTokens, PATH_TOKENS, [
	empty: "",
	absolute_indicator: "/",
	relative_root: ".",
	expression_indicator: "expression"
]);

#[derive(Clone, PartialEq)]
pub enum PathNodeData {
	Root,
	Prim {
		name: tf::Token,
	},
	PrimVariantSelection {
		variant_set: tf::Token,
		variant_name: tf::Token,
	},
	PrimProperty {
		name: tf::Token,
	},
	Target {
		target_path: Path,
	},
	RelationalAttribute {
		name: tf::Token,
	},
	Mapper {
		target_path: Path,
	},
	MapperArg {
		name: tf::Token,
	},
	Expression,
}

pub struct PathNode {
	pub parent: PoolHandle,
	ref_count: std::sync::atomic::AtomicU32,
	element_count: u16,
	flags: u8,
	pub data: PathNodeData,
}

impl PathNode {
	pub const IS_ABSOLUTE_FLAG: u8 = 1 << 0;

	pub fn is_absolute_path(&self) -> bool {
		self.flags & Self::IS_ABSOLUTE_FLAG != 0
	}

	pub fn is_absolute_root(&self) -> bool {
		self.flags & Self::IS_ABSOLUTE_FLAG != 0 && self.element_count == 0
	}

	pub fn element_count(&self) -> u16 {
		self.element_count
	}

	pub fn name(&self) -> &tf::Token {
		match &self.data {
			PathNodeData::Root => {
				if self.is_absolute_path() {
					&PATH_TOKENS.absolute_indicator
				} else {
					&PATH_TOKENS.relative_root
				}
			}
			PathNodeData::Prim { name } => name,
			PathNodeData::PrimProperty { name } => name,
			PathNodeData::PrimVariantSelection {
				variant_set,
				variant_name,
			} => {
				if variant_name.is_empty() {
					variant_set
				} else {
					variant_name
				}
			}
			PathNodeData::RelationalAttribute { name } => name,
			PathNodeData::MapperArg { name } => name,
			PathNodeData::Expression => &PATH_TOKENS.expression_indicator,
			_ => &PATH_TOKENS.empty,
		}
	}

	fn write_node_string(node: &PathNode, out: &mut String) {
		match &node.data {
			PathNodeData::Root => {}
			PathNodeData::Prim { name } => out.insert_str(0, name.as_str()),
			PathNodeData::PrimProperty { name } => {
				out.insert_str(0, name.as_str());
				out.insert_str(0, ".");
			}
			PathNodeData::PrimVariantSelection {
				variant_set,
				variant_name,
			} => {
				out.insert_str(0, "}");
				out.insert_str(0, variant_name.as_str());
				out.insert_str(0, "=");
				out.insert_str(0, variant_set.as_str());
				out.insert_str(0, "{");
			}
			PathNodeData::Target { target_path } => {
				out.insert_str(0, "]");
				Self::write_path_string(target_path.prim, target_path.prop, out);
				out.insert_str(0, "[");
			}
			PathNodeData::RelationalAttribute { name } => {
				out.insert_str(0, name.as_str());
				out.insert_str(0, ".");
			}
			PathNodeData::Mapper { target_path } => {
				out.insert_str(0, "]");
				Self::write_path_string(target_path.prim, target_path.prop, out);
				out.insert_str(0, "mapper[");
			}
			PathNodeData::MapperArg { name } => {
				out.insert_str(0, name.as_str());
				out.insert_str(0, ".");
			}
			PathNodeData::Expression => out.insert_str(0, ".expression"),
		};
	}

	fn write_path_string(prim_part: PoolHandle, prop_part: PoolHandle, out: &mut String) {
		let prop_pool = PATH_PROP_PART_POOL.read().unwrap();
		let prim_pool = PATH_PRIM_PART_POOL.read().unwrap();

		if prim_part == INVALID_NODE_HANDLE && prop_part == INVALID_NODE_HANDLE {
			return;
		}

		if prim_part == RELATIVE_ROOT_NODE_HANDLE && prop_part == INVALID_NODE_HANDLE {
			out.insert_str(0, ".");
			return;
		}

		let root = if prim_pool.get(prim_part).unwrap().is_absolute_path() {
			ABSOLUTE_ROOT_NODE_HANDLE
		} else {
			RELATIVE_ROOT_NODE_HANDLE
		};

		let mut cur_node = prop_part;
		while cur_node != INVALID_NODE_HANDLE {
			let prop_node = prop_pool.get(cur_node).unwrap();
			Self::write_node_string(prop_node, out);
			cur_node = prop_node.parent;
		}

		// This covers cases like '../.property'
		if prop_part != INVALID_NODE_HANDLE
			&& match &prim_pool.get(prim_part).unwrap().data {
				PathNodeData::Prim { name } => name.as_str() == "..",
				_ => false,
			} {
			out.insert_str(0, "/");
		}

		cur_node = prim_part;
		while cur_node != INVALID_NODE_HANDLE && cur_node != root {
			let prim_node = prim_pool.get(cur_node).unwrap();
			Self::write_node_string(prim_node, out);
			let parent = prim_pool.get(prim_node.parent);
			if matches!(&prim_node.data, PathNodeData::Prim { .. })
				&& parent.is_some()
				&& matches!(&parent.unwrap().data, PathNodeData::Prim { .. })
			{
				out.insert_str(0, "/");
			}
			cur_node = prim_node.parent;
		}

		// Add the leading '/' for absolute paths
		if prim_pool.get(prim_part).unwrap().is_absolute_path() {
			out.insert_str(0, "/");
		}
	}

	pub fn path_string(prim_part: PoolHandle, prop_part: PoolHandle) -> String {
		let mut out = String::new();
		Self::write_path_string(prim_part, prop_part, &mut out);
		out
	}
}

// TODO: Update ref_count when cloning or dropping Handle
pub type PoolHandle = u32;

pub const ABSOLUTE_ROOT_NODE_HANDLE: PoolHandle = 0;
pub const RELATIVE_ROOT_NODE_HANDLE: PoolHandle = 1;
pub const INVALID_NODE_HANDLE: PoolHandle = u32::MAX;

pub struct Pool<T> {
	elements: Vec<T>,
	free_list: Vec<usize>,
}

impl<T> Pool<T> {
	fn new() -> Self {
		Pool {
			elements: Vec::new(),
			free_list: Vec::new(),
		}
	}

	fn insert(&mut self, element: T) -> PoolHandle {
		if let Some(index) = self.free_list.pop() {
			self.elements[index] = element;
			index as PoolHandle
		} else {
			self.elements.push(element);
			(self.elements.len() - 1) as PoolHandle
		}
	}

	fn remove(&mut self, handle: PoolHandle) {
		self.free_list.push(handle as usize);
	}

	pub fn get(&self, handle: PoolHandle) -> Option<&T> {
		self.elements.get(handle as usize)
	}

	fn get_mut(&mut self, handle: PoolHandle) -> Option<&mut T> {
		self.elements.get_mut(handle as usize)
	}
}

pub static PATH_PRIM_PART_POOL: LazyLock<RwLock<Pool<PathNode>>> = LazyLock::new(|| {
	let mut pool = Pool::new();

	pool.insert(PathNode {
		parent: INVALID_NODE_HANDLE,
		ref_count: std::sync::atomic::AtomicU32::new(1),
		element_count: 0,
		flags: PathNode::IS_ABSOLUTE_FLAG,
		data: PathNodeData::Root,
	});

	pool.insert(PathNode {
		parent: INVALID_NODE_HANDLE,
		ref_count: std::sync::atomic::AtomicU32::new(1),
		element_count: 0,
		flags: 0,
		data: PathNodeData::Root,
	});

	RwLock::new(pool)
});

pub static PATH_PROP_PART_POOL: LazyLock<RwLock<Pool<PathNode>>> =
	LazyLock::new(|| RwLock::new(Pool::new()));

pub fn find_or_create_path_node(
	pool: &'static LazyLock<RwLock<Pool<PathNode>>>,
	parent: Option<PoolHandle>,
	data: &PathNodeData,
) -> PoolHandle {
	let mut w_pool = pool.write().unwrap();

	for (i, node) in w_pool.elements.iter().enumerate() {
		if node.data == *data && node.parent == parent.unwrap_or(INVALID_NODE_HANDLE) {
			node.ref_count
				.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
			return i as PoolHandle;
		}
	}

	let parent_node = parent.map(|p| w_pool.get(p)).flatten();

	let node = PathNode {
		parent: parent.unwrap_or(INVALID_NODE_HANDLE),
		ref_count: std::sync::atomic::AtomicU32::new(1),
		element_count: parent_node.map_or(0, |p| p.element_count + 1),
		flags: parent_node.map_or(0, |p| p.flags),
		data: data.clone(),
	};

	let new_handle = w_pool.insert(node);

	new_handle
}
