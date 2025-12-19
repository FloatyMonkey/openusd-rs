use crate::{sdf, tf, vt};

/// Interface for scene description data storage.
pub trait AbstractData {
	/// Return the type of the spec at `path`.
	fn spec_form(&self, path: &sdf::Path) -> Option<sdf::SpecForm>;

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
		let spec_form = data.spec_form(path).unwrap();
		println!("[{:?}] {}", spec_form, path);

		for field in data.list(path) {
			if let Some(value) = data.get(path, field) {
				println!("    {} = {:?}", field, value);
			}
		}
	}
}
