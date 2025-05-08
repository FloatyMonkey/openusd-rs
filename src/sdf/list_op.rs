use crate::{sdf, tf};

/// Enum for specifying one of the list editing operation types.
pub enum ListOpType {
	Explicit,
	Added,
	Deleted,
	Ordered,
	Prepended,
	Appended,
}

/// Value type representing a list-edit operation.
#[derive(Debug, Default, Clone)]
pub struct ListOp<T> {
	pub is_explicit: bool,
	pub explicit_items: Vec<T>,
	pub added_items: Vec<T>,
	pub prepended_items: Vec<T>,
	pub appended_items: Vec<T>,
	pub deleted_items: Vec<T>,
	pub ordered_items: Vec<T>,
}

pub type IntListOp = ListOp<i32>;
pub type UIntListOp = ListOp<u32>;
pub type Int64ListOp = ListOp<i64>;
pub type UInt64ListOp = ListOp<u64>;

pub type TokenListOp = ListOp<tf::Token>;
pub type StringListOp = ListOp<String>;
pub type PathListOp = ListOp<sdf::Path>;
pub type ReferenceListOp = ListOp<sdf::Reference>;
pub type PayloadListOp = ListOp<sdf::Payload>;
