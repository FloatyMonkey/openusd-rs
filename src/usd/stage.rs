use super::Prim;
use crate::{
	pcp,
	sdf::{self, FIELD_KEYS},
	tf, vt,
};
use std::collections::HashMap;
use std::path::Path;
use std::sync::{Arc, Mutex};

/// The outermost container for scene description, which owns and presents composed prims as a scenegraph,
/// following the composition recipe recursively described in its associated "root layer".
pub struct Stage {
	root_layer: Arc<sdf::Layer>,
	prim_index_cache: Mutex<HashMap<sdf::Path, Arc<pcp::PrimIndex>>>,
}

impl Stage {
	pub fn open(path: impl AsRef<Path>) -> Self {
		let root_layer = sdf::Layer::open(path);
		Self {
			root_layer,
			prim_index_cache: Mutex::new(HashMap::new()),
		}
	}

	pub fn pseudo_root(&self) -> Prim<'_> {
		Prim::new(self, sdf::Path::absolute_root_path())
	}

	pub fn prim_at_path(&self, path: impl Into<sdf::Path>) -> Prim<'_> {
		Prim::new(self, path.into())
	}

	// TODO: Move to Prim ?
	pub(crate) fn prim_index(&self, path: &sdf::Path) -> Option<Arc<pcp::PrimIndex>> {
		let prim_path = if path.is_prim_property_path() {
			path.parent_path()
		} else {
			path.clone()
		};

		// Check parent for activity (pruning)
		if !prim_path.is_absolute_root() {
			let parent_path = prim_path.parent_path();
			if !parent_path.is_absolute_root() {
				// Ensure parent is valid
				self.prim_index(&parent_path)?;
				// Ensure parent is active
				if let Some(active_val) = self.resolve_value(&parent_path, &FIELD_KEYS.active)
					&& let Some(active) = active_val.get::<bool>()
					&& !active
				{
					return None;
				}
			}
		}

		let mut cache = self.prim_index_cache.lock().unwrap();
		if let Some(index) = cache.get(&prim_path) {
			Some(index.clone())
		} else {
			let index = Arc::new(pcp::PrimIndex::new(
				self.root_layer.clone(),
				prim_path.clone(),
			));
			if index.is_valid() && index.has_specs() {
				cache.insert(prim_path.clone(), index.clone());
				Some(index)
			} else {
				None
			}
		}
	}

	pub(crate) fn resolve_value(&self, path: &sdf::Path, name: &tf::Token) -> Option<vt::Value> {
		let index = self.prim_index(path)?;

		let mut values = Vec::new();

		self.visit_nodes(&index, |node, site_path| {
			let actual_path = if path.is_prim_property_path() {
				let prop_name = path.name_token();
				site_path.append_property(&prop_name)
			} else {
				site_path.clone()
			};

			for layer in &node.layer_stack.layers {
				if let Some(val) = layer.data.get(&actual_path, name) {
					values.push(val.clone());
				}
			}
		});

		if values.is_empty() {
			return None;
		}

		let strongest = values[0].clone();

		// Dictionary merging
		if let Some(mut strong_dict) = strongest.get::<vt::Dictionary>() {
			for weak_val in values.iter().skip(1) {
				if let Some(weak_dict) = weak_val.get::<vt::Dictionary>() {
					for (k, v) in weak_dict {
						strong_dict.entry(k).or_insert(v);
					}
				}
			}
			return Some(vt::Value::new(strong_dict));
		}

		// ListOp merging
		macro_rules! try_merge_list_op {
			($type:ty) => {
				if let Some(strong_op) = strongest.get::<$type>() {
					let mut merged = strong_op;
					for weak_val in values.iter().skip(1) {
						if let Some(weak_op) = weak_val.get::<$type>() {
							merged = merged.combined_with(&weak_op);
						}
					}
					return Some(vt::Value::new(merged));
				}
			};
		}

		try_merge_list_op!(crate::sdf::IntListOp);
		try_merge_list_op!(crate::sdf::UIntListOp);
		try_merge_list_op!(crate::sdf::Int64ListOp);
		try_merge_list_op!(crate::sdf::UInt64ListOp);
		try_merge_list_op!(crate::sdf::StringListOp);
		try_merge_list_op!(crate::sdf::TokenListOp);
		try_merge_list_op!(crate::sdf::PathListOp);
		try_merge_list_op!(crate::sdf::ReferenceListOp);
		try_merge_list_op!(crate::sdf::PayloadListOp);

		// TODO: Timesamples merging?

		Some(strongest)
	}

	fn visit_nodes<F>(&self, index: &pcp::PrimIndex, mut callback: F)
	where
		F: FnMut(&pcp::Node, &sdf::Path),
	{
		if !index.is_valid() {
			return;
		}

		self.visit_node_recursive(index, 0, &mut callback);
	}

	fn visit_node_recursive<F>(&self, index: &pcp::PrimIndex, node_idx: u16, callback: &mut F)
	where
		F: FnMut(&pcp::Node, &sdf::Path),
	{
		if node_idx == pcp::INVALID_NODE_INDEX {
			return;
		}

		let node = index.graph.get_node(node_idx).unwrap();
		let unshared = index.graph.get_unshared(node_idx).unwrap();

		callback(node, &unshared.site_path);

		let mut child_idx = node.first_child_index;
		while child_idx != pcp::INVALID_NODE_INDEX {
			self.visit_node_recursive(index, child_idx, callback);
			let child = index.graph.get_node(child_idx).unwrap();
			child_idx = child.next_sibling_index;
		}
	}

	pub(crate) fn compose_children(&self, path: &sdf::Path) -> vt::Array<tf::Token> {
		let Some(index) = self.prim_index(path) else {
			return vt::Array::new();
		};

		let mut ordered_children: Vec<tf::Token> = Vec::new();
		let mut orders: Vec<Vec<tf::Token>> = Vec::new();

		self.visit_nodes(&index, |node, site_path| {
			for layer in &node.layer_stack.layers {
				// Merge children
				if let Some(children_val) =
					layer.data.get(site_path, &sdf::CHILDREN_KEYS.prim_children)
					&& let Some(children) = children_val.get::<vt::Array<tf::Token>>()
				{
					for child in children.iter() {
						if !ordered_children.contains(child) {
							ordered_children.push(child.clone());
						}
					}
				}

				// Collect order
				if let Some(order_val) = layer.data.get(site_path, &sdf::FIELD_KEYS.prim_order) {
					let order = if let Some(op) = order_val.get::<sdf::TokenListOp>() {
						Some(op.ordered_elements())
					} else {
						order_val
							.get::<vt::Array<tf::Token>>()
							.map(|v| v.iter().cloned().collect())
					};

					if let Some(order) = order {
						orders.push(order);
					}
				}
			}
		});

		for order in orders.iter().rev() {
			ordered_children = reorder(&ordered_children, order);
		}

		// Apply relocates from the root layer stack
		if let Some(root_node) = index.graph.get_node(0) {
			for (source, target) in &root_node.layer_stack.relocates {
				let parent = source.parent_path();
				if parent == *path {
					let child_name = source.name_token();
					if let Some(idx) = ordered_children
						.iter()
						.position(|c| c.as_str() == child_name.as_str())
					{
						let target_parent = target.parent_path();
						if target_parent == *path {
							// Rename
							let new_name = target.name_token();
							ordered_children[idx] = new_name;
						} else {
							// Remove (relocated outside)
							ordered_children.remove(idx);
						}
					}
				}
			}
		}

		vt::Array::from(ordered_children)
	}

	pub(crate) fn compose_properties(&self, path: &sdf::Path) -> vt::Array<tf::Token> {
		let Some(index) = self.prim_index(path) else {
			return vt::Array::new();
		};

		let mut all_properties = std::collections::HashSet::new();
		let mut strongest_order: Option<Vec<tf::Token>> = None;

		self.visit_nodes(&index, |node, site_path| {
			for layer in &node.layer_stack.layers {
				if let Some(props_val) = layer
					.data
					.get(site_path, &sdf::CHILDREN_KEYS.property_children)
					&& let Some(props) = props_val.get::<vt::Array<tf::Token>>()
				{
					for prop in props.iter() {
						all_properties.insert(prop.clone());
					}
				}

				if strongest_order.is_none()
					&& let Some(order_val) =
						layer.data.get(site_path, &sdf::FIELD_KEYS.property_order)
					&& let Some(arr) = order_val.get::<vt::Array<tf::Token>>()
				{
					strongest_order = Some(arr.iter().cloned().collect());
				}
			}
		});

		// TODO: Ensure this uses path element order from the specification
		let mut result: Vec<tf::Token> = all_properties.into_iter().collect();
		result.sort_by(|a, b| a.as_str().cmp(b.as_str()));

		if let Some(order) = strongest_order {
			result = reorder(&result, &order);
		}

		vt::Array::from(result)
	}
}

fn reorder(items: &[tf::Token], order: &[tf::Token]) -> Vec<tf::Token> {
	let mut result = Vec::new();
	let mut items_set: std::collections::HashSet<&tf::Token> = items.iter().collect();

	for token in order {
		if items_set.contains(token) {
			result.push(token.clone());
			items_set.remove(token);
		}
	}

	for item in items {
		if items_set.contains(item) {
			result.push(item.clone());
		}
	}

	result
}
