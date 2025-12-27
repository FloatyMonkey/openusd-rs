use super::ArcType;
use super::graph::{INVALID_NODE_INDEX, PrimIndexGraph};
use super::prim_index::PrimIndex;
use std::io::Write;

pub fn dump_dot_graph(index: &PrimIndex, writer: &mut dyn Write) -> std::io::Result<()> {
	writeln!(writer, "digraph PcpPrimIndex {{")?;
	if !index.graph.nodes.is_empty() {
		write_graph(writer, &index.graph, 0)?;
	}
	writeln!(writer, "}}")?;
	Ok(())
}

fn write_graph(
	writer: &mut dyn Write,
	graph: &PrimIndexGraph,
	node_idx: u16,
) -> std::io::Result<()> {
	let node = graph.get_node(node_idx).unwrap();
	let unshared = graph.get_unshared(node_idx).unwrap();

	let layer = &node.layer_stack.layers[0];
	let layer_name = std::path::Path::new(layer.identifier())
		.file_name()
		.unwrap_or_default()
		.to_string_lossy();

	let node_label = format!("@{}@<{}> ({})", layer_name, unshared.site_path, node_idx);

	writeln!(
		writer,
		"\t{} [label=\"{}\", shape=\"box\"];",
		node_idx, node_label
	)?;

	let mut child_idx = node.first_child_index;
	while child_idx != INVALID_NODE_INDEX {
		let child = graph.get_node(child_idx).unwrap();

		let (color, label) = match child.arc_type {
			ArcType::Local => ("black", "local"),
			ArcType::Inherit => ("green", "inherit"),
			ArcType::Variant => ("orange", "variant"),
			ArcType::Reference => ("red", "reference"),
			ArcType::Relocate => ("purple", "relocate"),
			ArcType::Payload => ("indigo", "payload"),
			ArcType::Specialize => ("sienna", "specialize"),
		};

		writeln!(
			writer,
			"\t{} -> {} [label=\"{}\", color={}];",
			node_idx, child_idx, label, color
		)?;

		write_graph(writer, graph, child_idx)?;

		child_idx = child.next_sibling_index;
	}

	Ok(())
}
