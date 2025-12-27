use super::ArcType;
use super::layer_stack::LayerStack;
use crate::sdf;
use std::sync::Arc;

// Index used to represent an invalid node.
pub const INVALID_NODE_INDEX: u16 = u16::MAX;

#[derive(Debug, Clone)]
pub struct Node {
	pub layer_stack: Arc<LayerStack>,

	pub parent_index: u16,
	pub origin_index: u16,

	// The indices of the first/last child, previous/next sibling.
	// The previous sibling index of a first child and the next
	// sibling index of a last child are `INVALID_NODE_INDEX`
	// (i.e. they form a list, not a ring).
	pub first_child_index: u16,
	pub last_child_index: u16,
	pub prev_sibling_index: u16,
	pub next_sibling_index: u16,

	pub arc_type: ArcType,
}

impl Node {
	pub fn new(
		layer_stack: Arc<LayerStack>,
		parent_index: u16,
		origin_index: u16,
		arc_type: ArcType,
		_map_to_parent: sdf::Retiming,
	) -> Self {
		Self {
			layer_stack,
			parent_index,
			origin_index,
			first_child_index: INVALID_NODE_INDEX,
			last_child_index: INVALID_NODE_INDEX,
			prev_sibling_index: INVALID_NODE_INDEX,
			next_sibling_index: INVALID_NODE_INDEX,
			arc_type,
		}
	}
}

#[derive(Debug, Clone)]
pub struct UnsharedData {
	pub site_path: sdf::Path,
}

#[derive(Debug, Clone, Default)]
pub struct PrimIndexGraph {
	pub nodes: Vec<Node>,
	pub unshared: Vec<UnsharedData>,
}

impl PrimIndexGraph {
	pub fn new() -> Self {
		Self {
			nodes: Vec::new(),
			unshared: Vec::new(),
		}
	}

	pub fn add_node(&mut self, node: Node, site_path: sdf::Path) -> u16 {
		let index = self.nodes.len();
		self.nodes.push(node);
		self.unshared.push(UnsharedData { site_path });
		index as u16
	}

	pub fn get_node(&self, index: u16) -> Option<&Node> {
		if index == INVALID_NODE_INDEX {
			None
		} else {
			self.nodes.get(index as usize)
		}
	}

	pub fn get_node_mut(&mut self, index: u16) -> Option<&mut Node> {
		if index == INVALID_NODE_INDEX {
			None
		} else {
			self.nodes.get_mut(index as usize)
		}
	}

	pub fn get_unshared(&self, index: u16) -> Option<&UnsharedData> {
		if index == INVALID_NODE_INDEX {
			None
		} else {
			self.unshared.get(index as usize)
		}
	}

	pub fn add_child(&mut self, parent_idx: u16, node: Node, site_path: sdf::Path) -> u16 {
		let arc_type = node.arc_type;
		let child_idx = self.add_node(node, site_path);

		// Insert child in LIVERPS strength order (weaker arcs go later)
		// Find the correct position to insert based on arc strength
		let mut insert_after = INVALID_NODE_INDEX;
		let mut current = self.nodes[parent_idx as usize].first_child_index;

		while current != INVALID_NODE_INDEX {
			let current_arc = self.nodes[current as usize].arc_type;
			if arc_strength(arc_type) < arc_strength(current_arc) {
				// New child is stronger, insert before current
				break;
			}
			insert_after = current;
			current = self.nodes[current as usize].next_sibling_index;
		}

		// Perform insertion
		if insert_after == INVALID_NODE_INDEX {
			// Insert at the beginning
			let old_first = self.nodes[parent_idx as usize].first_child_index;
			self.nodes[parent_idx as usize].first_child_index = child_idx;
			if old_first != INVALID_NODE_INDEX {
				self.nodes[old_first as usize].prev_sibling_index = child_idx;
				self.nodes[child_idx as usize].next_sibling_index = old_first;
			} else {
				self.nodes[parent_idx as usize].last_child_index = child_idx;
			}
		} else {
			// Insert after insert_after
			let next = self.nodes[insert_after as usize].next_sibling_index;
			self.nodes[insert_after as usize].next_sibling_index = child_idx;
			self.nodes[child_idx as usize].prev_sibling_index = insert_after;
			self.nodes[child_idx as usize].next_sibling_index = next;
			if next != INVALID_NODE_INDEX {
				self.nodes[next as usize].prev_sibling_index = child_idx;
			} else {
				self.nodes[parent_idx as usize].last_child_index = child_idx;
			}
		}

		child_idx
	}
}

/// Returns the strength value for a given arc type (lower = stronger)
/// LIVERPS order: Local(0), Inherit(1), Variant(2), Relocate(3), Reference(4), Payload(5), Specialize(6)
fn arc_strength(arc: ArcType) -> u8 {
	match arc {
		ArcType::Local => 0,
		ArcType::Inherit => 1,
		ArcType::Variant => 2,
		ArcType::Relocate => 3,
		ArcType::Reference => 4,
		ArcType::Payload => 5,
		ArcType::Specialize => 6,
	}
}
