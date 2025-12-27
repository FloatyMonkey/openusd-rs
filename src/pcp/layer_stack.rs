use crate::sdf;
use std::sync::Arc;

/// Represents a stack of layers that contribute opinions to composition.
#[derive(Debug)]
pub struct LayerStack {
	/// Layers in the stack, ordered from strongest (index 0) to weakest (last index).
	pub layers: Vec<Arc<sdf::Layer>>,
	pub layer_offsets: Vec<sdf::Retiming>,
	pub relocates: Vec<(sdf::Path, sdf::Path)>,
}

impl LayerStack {
	pub fn new(root_layer: Arc<sdf::Layer>) -> Self {
		let mut stack = Self {
			layers: Vec::new(),
			layer_offsets: Vec::new(),
			relocates: Vec::new(),
		};

		stack.build(root_layer, sdf::Retiming::default());
		stack.compute_relocates();
		stack
	}

	fn compute_relocates(&mut self) {
		// TODO: Proper composition of relocates
		if let Some(layer) = self.layers.first() {
			let relocates = layer.relocates();
			for r in relocates.iter() {
				self.relocates.push((r.source.clone(), r.target.clone()));
			}
		}
	}

	fn build(&mut self, layer: Arc<sdf::Layer>, offset: sdf::Retiming) {
		self.layers.push(layer.clone());
		self.layer_offsets.push(offset);

		let sub_layers = layer.sub_layer_paths();
		let sub_layer_offsets = layer.sub_layer_offsets();

		for (i, sub_layer_path) in sub_layers.iter().enumerate() {
			let sub_offset = if i < sub_layer_offsets.len() {
				sub_layer_offsets[i]
			} else {
				sdf::Retiming::default()
			};

			let combined_offset = offset * sub_offset;

			let resolved_path = layer.resolve_path(sub_layer_path);
			let sub_layer = sdf::Layer::open(resolved_path);

			self.build(sub_layer, combined_offset);
		}
	}
}
