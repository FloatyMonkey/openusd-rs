//! Prim Cache Population

pub mod diagnostic;
mod graph;
mod layer_stack;
mod prim_index;

pub use graph::*;
pub use layer_stack::*;
pub use prim_index::*;

/// Describes the type of arc connecting two nodes in the prim index.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ArcType {
	// The arcs are listed in strength order (LIVERPS).
	// Local is the root node of the prim index and has no parent node.
	Local,
	Inherit,
	Variant,
	Relocate,
	Reference,
	Payload,
	Specialize,
}
