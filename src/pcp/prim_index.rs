use super::ArcType;
use super::graph::{INVALID_NODE_INDEX, Node, PrimIndexGraph};
use super::layer_stack::LayerStack;
use crate::sdf::{self, FIELD_KEYS};
use std::sync::Arc;

/// An index of all the sites of scene description that contribute
/// opinions to a specific prim, under composition semantics.
#[derive(Debug)]
pub struct PrimIndex {
	pub graph: PrimIndexGraph,
}

impl PrimIndex {
	pub fn new(root_layer: Arc<sdf::Layer>, root_path: sdf::Path) -> Self {
		let mut index = Self {
			graph: PrimIndexGraph::new(),
		};

		index.compute(root_layer, root_path);
		index
	}

	pub fn is_valid(&self) -> bool {
		!self.graph.nodes.is_empty()
	}

	pub fn has_specs(&self) -> bool {
		for (node_idx, node) in self.graph.nodes.iter().enumerate() {
			let unshared = self.graph.get_unshared(node_idx as u16).unwrap();
			for layer in &node.layer_stack.layers {
				if layer.has_spec(&unshared.site_path) {
					return true;
				}
			}
		}
		false
	}

	fn compute(&mut self, root_layer: Arc<sdf::Layer>, path: sdf::Path) {
		let root_layer_stack = Arc::new(LayerStack::new(root_layer));

		let mut site_path = path.clone();
		for (source, target) in &root_layer_stack.relocates {
			if path == *source {
				return;
			}

			if path == *target {
				site_path = source.clone();
				break;
			}

			// TODO: Handle children of relocated paths
		}

		let root_node = Node::new(
			root_layer_stack,
			INVALID_NODE_INDEX,
			INVALID_NODE_INDEX,
			ArcType::Local,
			sdf::Retiming::default(),
		);

		let root_node_idx = self.graph.add_node(root_node, site_path);

		self.compose_subtree(root_node_idx);
	}

	fn compose_subtree(&mut self, node_idx: u16) {
		let (layer_stack, site_path) = {
			let node = self.graph.get_node(node_idx).unwrap();
			let unshared = self.graph.get_unshared(node_idx).unwrap();
			(node.layer_stack.clone(), unshared.site_path.clone())
		};

		let root_layer_stack = self.graph.get_node(0).unwrap().layer_stack.clone();

		for check_path in site_path.ancestors_range() {
			self.eval_inherits(
				layer_stack.clone(),
				root_layer_stack.clone(),
				&check_path,
				&site_path,
				node_idx,
			);

			self.eval_references(layer_stack.clone(), &check_path, &site_path, node_idx);

			self.eval_payloads(layer_stack.clone(), &check_path, &site_path, node_idx);
		}

		self.eval_specializes(
			layer_stack.clone(),
			root_layer_stack.clone(),
			&site_path,
			node_idx,
		);

		self.eval_variants(layer_stack, &site_path, node_idx);
	}

	fn eval_variants(
		&mut self,
		layer_stack: Arc<LayerStack>,
		site_path: &sdf::Path,
		node_idx: u16,
	) {
		let mut selections = self.compose_variant_selections(node_idx);
		let context_selections = self.resolve_variant_selections(node_idx);
		for (k, v) in context_selections {
			selections.insert(k, v);
		}

		for check_path in site_path.ancestors_range() {
			let variant_sets = self.gather_variant_sets(&layer_stack, &check_path);
			if !variant_sets.is_empty() {
				for variant_set in variant_sets {
					let mut already_selected = false;
					for p in site_path.ancestors_range() {
						if p == check_path {
							break;
						}
						let (vset, _) = p.variant_selection();
						if vset == variant_set {
							already_selected = true;
							break;
						}
					}
					if already_selected {
						continue;
					}

					if let Some(variant_name) = selections.get(&variant_set) {
						let variant_base =
							check_path.append_variant_selection(&variant_set, variant_name);
						let variant_path =
							append_relative_path(&variant_base, site_path, &check_path);

						let child_node = Node::new(
							layer_stack.clone(),
							node_idx,
							node_idx,
							ArcType::Variant,
							sdf::Retiming::default(),
						);
						let child_idx = self.graph.add_child(node_idx, child_node, variant_path);
						self.compose_subtree(child_idx);
					}
				}
			}
		}
	}

	fn eval_references(
		&mut self,
		layer_stack: Arc<LayerStack>,
		check_path: &sdf::Path,
		site_path: &sdf::Path,
		node_idx: u16,
	) {
		let references = compose_site_references(&layer_stack, check_path);
		for reference in references {
			let asset_path = reference.asset_path;
			let prim_path = reference.prim_path;
			let offset = reference.layer_offset;

			let (target_layer_stack, default_path) = if asset_path.is_empty() {
				(layer_stack.clone(), sdf::Path::absolute_root_path())
			} else {
				let root_layer = &layer_stack.layers[0];
				let resolved_path = root_layer.resolve_path(&asset_path);
				let ref_layer = sdf::Layer::open(resolved_path);
				let ref_layer_stack = Arc::new(LayerStack::new(ref_layer));
				let def_path = ref_layer_stack.layers[0].default_prim_as_path();
				(ref_layer_stack, def_path)
			};

			let base_target = if prim_path.is_empty() {
				default_path
			} else {
				prim_path
			};

			let target_path = append_relative_path(&base_target, site_path, check_path);

			// Apply relocates in target layer stack
			let mut actual_target_path = target_path.clone();
			for (source, target) in &target_layer_stack.relocates {
				if target_path == *target {
					actual_target_path = source.clone();
					break;
				}
			}

			let child_node = Node::new(
				target_layer_stack,
				node_idx,
				node_idx,
				ArcType::Reference,
				offset,
			);
			let child_idx = self
				.graph
				.add_child(node_idx, child_node, actual_target_path);
			self.compose_subtree(child_idx);
		}
	}

	fn eval_payloads(
		&mut self,
		layer_stack: Arc<LayerStack>,
		check_path: &sdf::Path,
		site_path: &sdf::Path,
		node_idx: u16,
	) {
		let payloads = compose_site_payloads(&layer_stack, check_path);
		for payload in payloads {
			let asset_path = payload.asset_path;
			let prim_path = payload.prim_path;
			let offset = payload.layer_offset;

			let (target_layer_stack, default_path) = if asset_path.is_empty() {
				(layer_stack.clone(), sdf::Path::absolute_root_path())
			} else {
				let root_layer = &layer_stack.layers[0];
				let resolved_path = root_layer.resolve_path(&asset_path);
				let payload_layer = sdf::Layer::open(resolved_path);
				let payload_layer_stack = Arc::new(LayerStack::new(payload_layer));
				let def_path = payload_layer_stack.layers[0].default_prim_as_path();
				(payload_layer_stack, def_path)
			};

			let base_target = if prim_path.is_empty() {
				default_path
			} else {
				prim_path
			};

			let target_path = append_relative_path(&base_target, site_path, check_path);

			// Apply relocates in target layer stack
			let mut actual_target_path = target_path.clone();
			for (source, target) in &target_layer_stack.relocates {
				if target_path == *target {
					actual_target_path = source.clone();
					break;
				}
			}

			let child_node = Node::new(
				target_layer_stack,
				node_idx,
				node_idx,
				ArcType::Payload,
				offset,
			);
			let child_idx = self
				.graph
				.add_child(node_idx, child_node, actual_target_path);
			self.compose_subtree(child_idx);
		}
	}

	fn eval_inherits(
		&mut self,
		layer_stack: Arc<LayerStack>,
		root_layer_stack: Arc<LayerStack>,
		check_path: &sdf::Path,
		site_path: &sdf::Path,
		node_idx: u16,
	) {
		let is_root = Arc::ptr_eq(&layer_stack, &root_layer_stack);

		let inherits = compose_site_inherits(&layer_stack, check_path);
		for inherit_path in inherits {
			let target_path = append_relative_path(&inherit_path, site_path, check_path);

			// Implied global inherit (in root layer stack)
			let global_path = self.map_to_root(node_idx, &target_path);
			let child_node = Node::new(
				root_layer_stack.clone(),
				node_idx,
				node_idx,
				ArcType::Inherit,
				sdf::Retiming::default(),
			);
			let child_idx = self.graph.add_child(node_idx, child_node, global_path);
			self.compose_subtree(child_idx);

			// Local inherit (if different from root)
			if !is_root {
				let local_node = Node::new(
					layer_stack.clone(),
					node_idx,
					node_idx,
					ArcType::Inherit,
					sdf::Retiming::default(),
				);
				let local_idx = self.graph.add_child(node_idx, local_node, target_path);
				self.compose_subtree(local_idx);
			}
		}
	}

	fn eval_specializes(
		&mut self,
		layer_stack: Arc<LayerStack>,
		root_layer_stack: Arc<LayerStack>,
		site_path: &sdf::Path,
		node_idx: u16,
	) {
		let is_root = Arc::ptr_eq(&layer_stack, &root_layer_stack);

		let specializes = compose_site_specializes(&layer_stack, site_path);
		for specialize_path in specializes {
			// Implied global specialize (in root layer stack)
			let global_path = self.map_to_root(node_idx, &specialize_path);
			let child_node = Node::new(
				root_layer_stack.clone(),
				node_idx,
				node_idx,
				ArcType::Specialize,
				sdf::Retiming::default(),
			);
			let child_idx = self.graph.add_child(node_idx, child_node, global_path);
			self.compose_subtree(child_idx);

			// Local specialize (if different from root)
			if !is_root {
				let local_node = Node::new(
					layer_stack.clone(),
					node_idx,
					node_idx,
					ArcType::Specialize,
					sdf::Retiming::default(),
				);
				let local_idx = self.graph.add_child(node_idx, local_node, specialize_path);
				self.compose_subtree(local_idx);
			}
		}
	}

	fn gather_variant_sets(&self, layer_stack: &LayerStack, path: &sdf::Path) -> Vec<String> {
		let mut combined_op = sdf::StringListOp::default();
		for layer in &layer_stack.layers {
			if let Some(val) = layer.data.get(path, &FIELD_KEYS.variant_set_names)
				&& let Some(list_op) = val.get::<sdf::StringListOp>()
			{
				combined_op = combined_op.combined_with(&list_op);
			}
		}

		combined_op.ordered_elements()
	}

	pub fn variant_selections(&self) -> std::collections::HashMap<String, String> {
		let mut selections = self.compose_variant_selections(0);
		let context = self.resolve_variant_selections(0);
		for (k, v) in context {
			selections.insert(k, v);
		}
		selections
	}

	fn resolve_variant_selections(
		&self,
		node_idx: u16,
	) -> std::collections::HashMap<String, String> {
		let mut selections = std::collections::HashMap::new();
		let mut current_idx = node_idx;
		let mut path_to_root = Vec::new();

		loop {
			path_to_root.push(current_idx);
			if current_idx == 0 || current_idx == INVALID_NODE_INDEX {
				break;
			}
			let node = self.graph.get_node(current_idx).unwrap();
			current_idx = node.parent_index;
		}

		for &idx in path_to_root.iter().rev() {
			let node = self.graph.get_node(idx).unwrap();
			let unshared = self.graph.get_unshared(idx).unwrap();

			let node_selections = self
				.gather_variant_selections_in_layer_stack(&node.layer_stack, &unshared.site_path);
			for (k, v) in node_selections {
				selections.entry(k).or_insert(v);
			}
		}
		selections
	}

	fn gather_variant_selections_in_layer_stack(
		&self,
		layer_stack: &LayerStack,
		path: &sdf::Path,
	) -> std::collections::HashMap<String, String> {
		let mut selections = std::collections::HashMap::new();

		for check_path in path.ancestors_range() {
			for layer in &layer_stack.layers {
				if let Some(val) = layer.data.get(&check_path, &FIELD_KEYS.variant_selection)
					&& let Some(map) = val.get::<sdf::VariantSelectionMap>()
				{
					for (k, v) in map {
						if !selections.contains_key(&k) {
							selections.insert(k.clone(), v.clone());
						}
					}
				}
			}
		}
		selections
	}

	fn get_children_indices(&self, node_idx: u16) -> Vec<u16> {
		let mut indices = Vec::new();
		if let Some(node) = self.graph.get_node(node_idx) {
			let mut current = node.first_child_index;
			while current != INVALID_NODE_INDEX {
				indices.push(current);
				current = self.graph.get_node(current).unwrap().next_sibling_index;
			}
		}
		indices
	}

	fn compose_variant_selections(
		&self,
		node_idx: u16,
	) -> std::collections::HashMap<String, String> {
		let mut selections = std::collections::HashMap::new();

		let mut children = self.get_children_indices(node_idx);
		// Reverse for Weak-to-Strong iteration
		children.reverse();

		for child_idx in children {
			let child_selections = self.compose_variant_selections(child_idx);
			for (k, v) in child_selections {
				selections.insert(k, v);
			}
		}

		let node = self.graph.get_node(node_idx).unwrap();
		let unshared = self.graph.get_unshared(node_idx).unwrap();
		let local_selections =
			self.gather_variant_selections_in_layer_stack(&node.layer_stack, &unshared.site_path);

		for (k, v) in local_selections {
			selections.insert(k, v);
		}

		selections
	}

	fn map_to_root(&self, node_idx: u16, target_path: &sdf::Path) -> sdf::Path {
		let node_data = self.graph.get_unshared(node_idx).unwrap();
		let root_node_data = self.graph.get_unshared(0).unwrap();

		if node_idx == 0 {
			return target_path.clone();
		}

		let current_path_str = node_data.site_path.to_string();
		let target_path_str = target_path.to_string();
		let root_path_str = root_node_data.site_path.to_string();

		let current_parts: Vec<&str> = current_path_str
			.trim_start_matches('/')
			.split('/')
			.collect();
		let target_parts: Vec<&str> = target_path_str.trim_start_matches('/').split('/').collect();
		let root_parts: Vec<&str> = root_path_str.trim_start_matches('/').split('/').collect();

		let mut common_len = 0;
		for (a, b) in current_parts.iter().zip(target_parts.iter()) {
			if *a == *b {
				common_len += 1;
			} else {
				break;
			}
		}

		let up_steps = current_parts.len() - common_len;
		let down_steps = &target_parts[common_len..];

		if up_steps > root_parts.len() {
			return target_path.clone();
		}

		let mut new_root_parts = root_parts[0..root_parts.len() - up_steps].to_vec();
		new_root_parts.extend_from_slice(down_steps);

		let new_path_str = if new_root_parts.is_empty() {
			"/".to_string()
		} else {
			format!("/{}", new_root_parts.join("/"))
		};
		sdf::Path::from(new_path_str.as_str())
	}
}

fn append_relative_path(base: &sdf::Path, path: &sdf::Path, ancestor: &sdf::Path) -> sdf::Path {
	if path == ancestor {
		return base.clone();
	}
	let mut elements = Vec::new();
	let mut cur = path.clone();
	while cur != *ancestor && !cur.is_empty() {
		elements.push(cur.element_token());
		cur = cur.parent_path();
	}

	let mut result = base.clone();
	for element in elements.iter().rev() {
		result = result.append_element(element);
	}
	result
}

fn compose_site_references(layer_stack: &LayerStack, path: &sdf::Path) -> Vec<sdf::Reference> {
	let mut result = sdf::ReferenceListOp::default();
	for layer in &layer_stack.layers {
		if let Some(val) = layer.data.get(path, &FIELD_KEYS.references)
			&& let Some(list_op) = val.get::<sdf::ReferenceListOp>()
		{
			let mapped_list_op = list_op.map(|r: &sdf::Reference| -> sdf::Reference {
				let mut new_r = r.clone();
				if !new_r.asset_path.is_empty() {
					let resolved = layer.resolve_path(&new_r.asset_path);
					new_r.asset_path = resolved.to_string_lossy().to_string();
				}
				new_r
			});

			result = result.combined_with(&mapped_list_op);
		}
	}
	result.ordered_elements()
}

fn compose_site_payloads(layer_stack: &LayerStack, path: &sdf::Path) -> Vec<sdf::Payload> {
	let mut result = sdf::PayloadListOp::default();
	for layer in &layer_stack.layers {
		if let Some(val) = layer.data.get(path, &FIELD_KEYS.payload)
			&& let Some(list_op) = val.get::<sdf::PayloadListOp>()
		{
			let mapped_list_op = list_op.map(|p: &sdf::Payload| -> sdf::Payload {
				let mut new_p = p.clone();
				if !new_p.asset_path.is_empty() {
					let resolved = layer.resolve_path(&new_p.asset_path);
					new_p.asset_path = resolved.to_string_lossy().to_string();
				}
				new_p
			});

			result = result.combined_with(&mapped_list_op);
		}
	}
	result.ordered_elements()
}

fn compose_site_inherits(layer_stack: &LayerStack, path: &sdf::Path) -> Vec<sdf::Path> {
	let mut result = sdf::PathListOp::default();
	for layer in &layer_stack.layers {
		if let Some(val) = layer.data.get(path, &FIELD_KEYS.inherit_paths)
			&& let Some(list_op) = val.get::<sdf::PathListOp>()
		{
			result = result.combined_with(&list_op);
		}
	}
	result.ordered_elements()
}

fn compose_site_specializes(layer_stack: &LayerStack, path: &sdf::Path) -> Vec<sdf::Path> {
	let mut result = sdf::PathListOp::default();
	for layer in &layer_stack.layers {
		if let Some(val) = layer.data.get(path, &FIELD_KEYS.specializes)
			&& let Some(list_op) = val.get::<sdf::PathListOp>()
		{
			result = result.combined_with(&list_op);
		}
	}
	result.ordered_elements()
}
