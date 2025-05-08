pub mod parser;

use crate::{sdf, tf, vt};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Spec {
	pub ty: sdf::SpecType,
	pub fields: HashMap<tf::Token, vt::Value>,
}

impl Spec {
	pub fn new(ty: sdf::SpecType) -> Self {
		Spec {
			ty,
			fields: HashMap::new(),
		}
	}

	pub fn add(&mut self, key: &tf::Token, value: impl Into<vt::Value>) {
		self.fields.insert(key.clone(), value.into());
	}
}

#[derive(Debug, Clone)]
pub struct UsdaFile {
	pub data: HashMap<sdf::Path, Spec>,
}

impl sdf::AbstractData for UsdaFile {
	fn spec_type(&self, path: &sdf::Path) -> Option<sdf::SpecType> {
		self.data.get(path).map(|spec| spec.ty)
	}

	fn get(&self, path: &sdf::Path, field: &tf::Token) -> Option<vt::Value> {
		let spec = self.data.get(path)?;
		spec.fields.get(field).cloned()
	}

	fn list(&self, path: &sdf::Path) -> Vec<&tf::Token> {
		if let Some(spec) = self.data.get(path) {
			spec.fields.keys().collect()
		} else {
			Vec::new()
		}
	}

	fn visit_specs(&self) -> Vec<&sdf::Path> {
		self.data.keys().collect()
	}
}
