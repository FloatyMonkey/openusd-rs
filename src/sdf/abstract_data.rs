use crate::{sdf, tf, vt};

/// Interface for scene description data storage.
pub trait AbstractData {
	/// Return the type of the spec at `path`.
	fn spec_type(&self, path: &sdf::Path) -> Option<sdf::SpecType>;

	/// Return the value for the given `path` and `field`.
	fn get(&self, path: &sdf::Path, field: &tf::Token) -> Option<vt::Value>;

	/// Return the names of all the fields that are set at `path`.
	fn list(&self, path: &sdf::Path) -> Vec<&tf::Token>;

	/// Visit every spec in this AbstractData object in arbitrary order.
	fn visit_specs(&self) -> Vec<&sdf::Path>;
}

pub fn debug_dump(data: &dyn sdf::AbstractData) {
	println!("AbstractData dump");
	println!("Spec count: {}\n", data.visit_specs().len());

	for path in data.visit_specs() {
		let spec_type = data.spec_type(path).unwrap();
		println!("[{:?}] {}", spec_type, path);

		for field in data.list(path) {
			if let Some(value) = data.get(path, field) {
				println!("    {} = {:?}", field, value);
			}
		}
	}
}
