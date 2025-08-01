use crate::{sdf, tf};

use std::collections::HashMap;

/// Object holding information describing skeleton topology.
///
/// This provides the hierarchical information needed to reason about joint
/// relationships in a manner suitable to computations.
pub struct Topology {
	parent_indices: Vec<i32>,
}

impl Topology {
	/// Construct a topology from `paths`, an array of joint paths.
	pub fn from_paths(paths: &[sdf::Path]) -> Self {
		let path_map: HashMap<sdf::Path, usize> = paths
			.iter()
			.enumerate()
			.map(|(i, p)| (p.clone(), i))
			.collect();

		let parent_indices = paths
			.iter()
			.map(|path| Self::parent_index_helper(&path_map, path))
			.collect();

		Self { parent_indices }
	}

	/// Construct a topology from `tokens`, an array holding ordered joint
	/// paths as tokens. If an array of paths is already available, prefer
	/// using [`Self::from_paths`] for better performance.
	pub fn from_tokens(tokens: &[tf::Token]) -> Self {
		let paths = tokens.iter().map(|t| t.as_str().into()).collect::<Vec<_>>();

		Self::from_paths(&paths)
	}

	/// Construct a topology from an array of parent indices.
	/// For each joint, this provides the parent index of that joint, or -1 if none.
	pub fn from_indices(parent_indices: &[i32]) -> Self {
		Self {
			parent_indices: parent_indices.to_vec(),
		}
	}

	/// Returns the number of joints in the topology.
	pub fn num_joints(&self) -> usize {
		self.parent_indices.len()
	}

	/// Returns all parent indices of all joints in the topology.
	pub fn parent_indices(&self) -> &[i32] {
		&self.parent_indices
	}

	/// Returns the parent joint index of the `index`th joint.
	/// Returns -1 for joints with no parent (root joints).
	pub fn parent(&self, index: i32) -> i32 {
		assert!(index < self.num_joints() as i32);
		self.parent_indices[index as usize]
	}

	/// Returns true if the `index`th joint is a root joint.
	pub fn is_root(&self, index: i32) -> bool {
		index < 0
	}

	/// Validates the topology, ensuring that no joint has itself as a parent
	/// and that joints are ordered such that parent joints always come before
	/// their children.
	///
	/// This ordering restriction primarily simplifies hierarchy evaluation.
	/// It also ensures that the topology is non-cyclic.
	pub fn validate(&self) -> Result<(), TopologyError> {
		for (joint, parent) in self
			.parent_indices
			.iter()
			.enumerate()
			.map(|(j, p)| (j as i32, *p))
		{
			if parent >= 0 {
				if parent >= joint {
					if parent == joint {
						return Err(TopologyError::SelfParent { joint });
					}
					return Err(TopologyError::MisorderedParent { joint, parent });
				}
			}
		}
		Ok(())
	}

	fn parent_index_helper(path_map: &HashMap<sdf::Path, usize>, path: &sdf::Path) -> i32 {
		if path.is_prim_path() {
			// Recurse over all ancestor paths, not just the direct parent.
			// For instance, if the map includes only paths 'a' and 'a/b/c',
			// 'a' will be treated as the parent of 'a/b/c'.
			// Skip the first ancestor, which is the path itself.
			let ancestors = path.ancestors_range().skip(1);

			for ancestor in ancestors {
				if let Some(&index) = path_map.get(&ancestor) {
					return index as i32;
				}
			}
		}

		-1
	}
}

/// Error returned by [`Topology::validate`] for invalid topologies.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TopologyError {
	/// Joint has itself as its parent.
	SelfParent { joint: i32 },
	/// Joint has mis-ordered parent. Joints are expected to be ordered
	/// with parent joints always coming before children.
	MisorderedParent { joint: i32, parent: i32 },
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn from_tokens() {
		let topology = Topology::from_tokens(&[
			tf::Token::new("/a"),
			tf::Token::new("/a/b"),
			tf::Token::new("/a/b/c"),
			tf::Token::new("/a/d"),
			tf::Token::new("/e"),
			tf::Token::new("/e/f/g"),
		]);

		assert_eq!(topology.num_joints(), 6);
		assert_eq!(topology.parent_indices(), vec![-1, 0, 1, 0, -1, 4]);
	}

	#[test]
	fn validate() {
		let valid_topology = Topology::from_indices(&[-1, 0, 1, 0, -1, 4]);
		assert!(valid_topology.validate().is_ok());

		let self_parent_topology = Topology::from_indices(&[-1, 0, 1, 3, -1, 4]);
		assert!(matches!(
			self_parent_topology.validate(),
			Err(TopologyError::SelfParent { joint: 3 })
		));

		let misordered_parent_topology = Topology::from_indices(&[-1, 0, 1, 0, 5, -1]);
		assert!(matches!(
			misordered_parent_topology.validate(),
			Err(TopologyError::MisorderedParent {
				joint: 4,
				parent: 5,
			})
		));
	}
}
