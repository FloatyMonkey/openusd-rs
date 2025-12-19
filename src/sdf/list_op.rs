use std::collections::HashSet;
use std::fmt::Debug;
use std::hash::Hash;
use std::iter::FromIterator;

use crate::{sdf, tf};

/// Value type representing a list-edit operation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ListOp<T> {
	explicit_items: Option<UniqueList<T>>,
	prepended_items: UniqueList<T>,
	appended_items: UniqueList<T>,
	deleted_items: UniqueList<T>,
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

impl<T: Hash + Eq + Clone> Default for ListOp<T> {
	fn default() -> Self {
		Self {
			explicit_items: None,
			prepended_items: UniqueList::new(),
			appended_items: UniqueList::new(),
			deleted_items: UniqueList::new(),
		}
	}
}

impl<T: Hash + Eq + Clone> ListOp<T> {
	pub fn explicit_items(&self) -> Option<&Vec<T>> {
		self.explicit_items.as_ref().map(|ul| &ul.elements)
	}

	pub fn prepended_items(&self) -> &Vec<T> {
		&self.prepended_items.elements
	}

	pub fn appended_items(&self) -> &Vec<T> {
		&self.appended_items.elements
	}

	pub fn deleted_items(&self) -> &Vec<T> {
		&self.deleted_items.elements
	}

	pub fn from_explicit(explicit_items: Vec<T>) -> Self {
		Self {
			explicit_items: Some(explicit_items.into_iter().collect()),
			..Default::default()
		}
	}

	pub fn from_composable(
		prepended_items: Vec<T>,
		appended_items: Vec<T>,
		deleted_items: Vec<T>,
	) -> Self {
		Self {
			explicit_items: None,
			prepended_items: prepended_items.into_iter().collect(),
			appended_items: appended_items.into_iter().collect(),
			deleted_items: deleted_items.into_iter().collect(),
		}
	}

	pub fn is_explicit(&self) -> bool {
		self.explicit_items.is_some()
	}

	pub fn is_inert(&self) -> bool {
		self.explicit_items.is_some()
			|| !self.deleted_items.is_empty()
			|| !self.prepended_items.is_empty()
			|| !self.appended_items.is_empty()
	}

	fn compose_over_explicit(&self, weaker_list_op: &ListOp<T>) -> ListOp<T> {
		assert!(!self.is_explicit());
		assert!(weaker_list_op.is_explicit());

		let explicit_items: UniqueList<T> = self
			.prepended_items
			.iter()
			.filter(|e| !self.appended_items.contains(e))
			.chain(
				weaker_list_op
					.explicit_items
					.as_ref()
					.unwrap()
					.iter()
					.filter(|e| {
						!self.prepended_items.contains(e)
							&& !self.appended_items.contains(e)
							&& !self.deleted_items.contains(e)
					}),
			)
			.chain(self.appended_items.iter())
			.cloned()
			.collect();

		Self::from_explicit(explicit_items.elements)
	}

	fn compose_prepended_items(&self, weaker_list_op: &ListOp<T>) -> UniqueList<T> {
		let items_from_weaker = weaker_list_op.prepended_items.iter().filter(|e| {
			!self.prepended_items.contains(e)
				&& !self.appended_items.contains(e)
				&& !self.deleted_items.contains(e)
		});

		let items_from_stronger = self
			.prepended_items
			.iter()
			.filter(|e| !self.appended_items.contains(e));

		items_from_stronger
			.chain(items_from_weaker)
			.cloned()
			.collect()
	}

	fn compose_appended_items(&self, weaker_list_op: &ListOp<T>) -> UniqueList<T> {
		let items_from_weaker = weaker_list_op.appended_items.iter().filter(|e| {
			!self.prepended_items.contains(e)
				&& !self.appended_items.contains(e)
				&& !self.deleted_items.contains(e)
		});

		items_from_weaker
			.chain(self.appended_items.iter())
			.cloned()
			.collect()
	}

	fn compose_deleted_items(&self, weaker_list_op: &ListOp<T>) -> UniqueList<T> {
		let items_from_weaker = weaker_list_op
			.deleted_items
			.iter()
			.filter(|e| !self.prepended_items.contains(e) && !self.appended_items.contains(e));

		let items_from_stronger = self.deleted_items.iter().filter(|e| {
			!self.prepended_items.contains(e)
				&& !self.appended_items.contains(e)
				&& !weaker_list_op.deleted_items.contains(e)
		});

		items_from_weaker
			.chain(items_from_stronger)
			.cloned()
			.collect()
	}

	fn compose_over_appliable(&self, weaker_list_op: &ListOp<T>) -> ListOp<T> {
		assert!(!self.is_explicit());
		assert!(!weaker_list_op.is_explicit());

		ListOp {
			explicit_items: None,
			prepended_items: self.compose_prepended_items(weaker_list_op),
			appended_items: self.compose_appended_items(weaker_list_op),
			deleted_items: self.compose_deleted_items(weaker_list_op),
		}
	}

	pub fn combined_with(&self, weaker_list_op: &ListOp<T>) -> ListOp<T> {
		if !self.is_inert() && !weaker_list_op.is_inert() {
			return ListOp::default();
		}

		if !self.is_inert() {
			if weaker_list_op.is_explicit() {
				return ListOp {
					explicit_items: weaker_list_op.explicit_items.clone(),
					..Default::default()
				};
			} else {
				return ListOp {
					explicit_items: None,
					deleted_items: weaker_list_op.deleted_items.clone(),
					prepended_items: weaker_list_op.prepended_items.clone(),
					appended_items: weaker_list_op.appended_items.clone(),
				};
			}
		}

		if self.is_explicit() {
			return ListOp {
				explicit_items: self.explicit_items.clone(),
				..Default::default()
			};
		}

		if !weaker_list_op.is_inert() {
			return ListOp {
				explicit_items: None,
				deleted_items: self.deleted_items.clone(),
				prepended_items: self.prepended_items.clone(),
				appended_items: self.appended_items.clone(),
			};
		}

		if weaker_list_op.is_explicit() {
			return self.compose_over_explicit(weaker_list_op);
		}

		self.compose_over_appliable(weaker_list_op)
	}

	pub fn reduced(&self) -> ListOp<T> {
		if self.is_explicit() {
			ListOp {
				explicit_items: self.explicit_items.clone(),
				..Default::default()
			}
		} else {
			let deleted_items: UniqueList<T> = self
				.deleted_items
				.iter()
				.filter(|i| !self.appended_items.contains(i) && !self.prepended_items.contains(i))
				.cloned()
				.collect();

			let prepended_items: UniqueList<T> = self
				.prepended_items
				.iter()
				.filter(|i| !self.appended_items.contains(i))
				.cloned()
				.collect();

			ListOp {
				explicit_items: None,
				deleted_items,
				prepended_items,
				appended_items: self.appended_items.clone(),
			}
		}
	}

	pub fn ordered_elements(&self) -> Vec<T> {
		if let Some(explicit) = &self.explicit_items {
			explicit.elements.clone()
		} else {
			self.prepended_items
				.iter()
				.filter(|e| !self.appended_items.contains(e))
				.chain(self.appended_items.iter())
				.cloned()
				.collect()
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct UniqueList<T> {
	elements: Vec<T>,
}

impl<T: Hash + Eq + Clone> UniqueList<T> {
	pub fn new() -> Self {
		Self {
			elements: Vec::new(),
		}
	}

	pub fn contains(&self, value: &T) -> bool {
		self.elements.contains(value)
	}

	pub fn len(&self) -> usize {
		self.elements.len()
	}

	pub fn is_empty(&self) -> bool {
		self.elements.is_empty()
	}

	pub fn iter(&self) -> std::slice::Iter<'_, T> {
		self.elements.iter()
	}
}

impl<T: Hash + Eq + Clone> FromIterator<T> for UniqueList<T> {
	fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
		let elements: Vec<T> = iter.into_iter().collect();
		let mut set = HashSet::new();
		for item in &elements {
			if !set.insert(item.clone()) {
				panic!("UniqueList initialized with repeated elements");
			}
		}
		Self { elements }
	}
}

impl<T: Hash + Eq + Clone> IntoIterator for UniqueList<T> {
	type Item = T;
	type IntoIter = std::vec::IntoIter<T>;

	fn into_iter(self) -> Self::IntoIter {
		self.elements.into_iter()
	}
}

impl<'a, T: Hash + Eq + Clone> IntoIterator for &'a UniqueList<T> {
	type Item = &'a T;
	type IntoIter = std::slice::Iter<'a, T>;

	fn into_iter(self) -> Self::IntoIter {
		self.elements.iter()
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use serde::Deserialize;
	use std::fs::File;
	use std::io::BufReader;

	#[derive(Deserialize)]
	struct JsonListOp<T> {
		explicit_items: Option<Vec<T>>,
		prepended_items: Option<Vec<T>>,
		appended_items: Option<Vec<T>>,
		deleted_items: Option<Vec<T>>,
	}

	#[derive(Deserialize)]
	struct ReduceChainCase {
		description: String,
		chain: Vec<JsonListOp<i64>>,
		combined_reduced: JsonListOp<i64>,
	}

	fn make_reference_list_op(data: &JsonListOp<i64>) -> ListOp<i64> {
		if let Some(explicit) = &data.explicit_items {
			return ListOp::from_explicit(explicit.iter().cloned().collect());
		}

		ListOp::from_composable(
			data.prepended_items.clone().unwrap_or_default(),
			data.appended_items.clone().unwrap_or_default(),
			data.deleted_items.clone().unwrap_or_default(),
		)
	}

	fn run_json_test(path: &str) {
		let f = File::open(path).unwrap();
		let reader = BufReader::new(f);
		let cases: Vec<ReduceChainCase> = serde_json::from_reader(reader).unwrap();
		assert!(!cases.is_empty());

		for case in cases {
			// Combine chain left-to-right: first element is strongest
			let combined_reduced = case
				.chain
				.into_iter()
				.map(|c| make_reference_list_op(&c))
				.reduce(|s, w| s.combined_with(&w))
				.unwrap_or_default()
				.reduced();

			let expected = make_reference_list_op(&case.combined_reduced);

			assert_eq!(combined_reduced, expected, "case: {}", case.description);
		}
	}

	#[test]
	fn append_over_composable() {
		run_json_test("resources/combine_chain/append_over_composable.json");
	}

	#[test]
	fn append_over_explicit() {
		run_json_test("resources/combine_chain/append_over_explicit.json");
	}

	#[test]
	fn composable_only() {
		run_json_test("resources/combine_chain/composable_only.json");
	}

	#[test]
	fn delete_over_explicit() {
		run_json_test("resources/combine_chain/delete_over_explicit.json");
	}

	#[test]
	fn explicit_only() {
		run_json_test("resources/combine_chain/explicit_only.json");
	}

	#[test]
	fn prepend_over_composable() {
		run_json_test("resources/combine_chain/prepend_over_composable.json");
	}

	#[test]
	fn prepend_over_explicit() {
		run_json_test("resources/combine_chain/prepend_over_explicit.json");
	}
}
