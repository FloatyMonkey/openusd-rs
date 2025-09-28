use crate::{tf, usd, vt};

pub const DEFAULT_OUTPUT_NAME: &str = "out";
const MATERIALX_CONFIG_API: &str = "MaterialXConfigAPI";
const API_SCHEMAS_FIELD: &str = "apiSchemas";
const CONFIG_VERSION_ATTR: &str = "config:mtlx:version";

#[derive(Clone)]
pub struct MaterialXConfigAPI<'a> {
	prim: usd::Prim<'a>,
}

impl<'a> MaterialXConfigAPI<'a> {
	pub fn from_prim(prim: usd::Prim<'a>) -> Option<Self> {
		if Self::is_applied_to(&prim) {
			Some(Self { prim })
		} else {
			None
		}
	}

	pub fn is_applied_to(prim: &usd::Prim<'_>) -> bool {
		prim.metadata::<vt::Array<tf::Token>>(&tf::Token::new(API_SCHEMAS_FIELD))
			.map(|schemas| {
				schemas
					.iter()
					.any(|tok| tok.as_str() == MATERIALX_CONFIG_API)
			})
			.unwrap_or(false)
	}

	pub fn version(&self) -> Option<String> {
		let token = tf::Token::new(CONFIG_VERSION_ATTR);
		if self.prim.has_attribute(&token) {
			Some(self.prim.attribute(&token).get::<String>())
		} else {
			None
		}
	}
}
