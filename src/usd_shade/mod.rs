use crate::{sdf, tf, usd, vt};

const INPUT_PREFIX: &str = "inputs:";
const OUTPUT_PREFIX: &str = "outputs:";
const CONNECT_SUFFIX: &str = ".connect";
const INFO_ID: &str = "info:id";
const IMPLEMENTATION_SOURCE: &str = "info:implementationSource";

pub struct Shader<'a> {
	prim: usd::Prim<'a>,
}

impl<'a> Shader<'a> {
	pub fn get(stage: &'a usd::Stage, path: impl Into<sdf::Path>) -> Option<Self> {
		get_typed_prim(stage, path, "Shader").map(|prim| Self { prim })
	}

	pub fn prim(&self) -> &usd::Prim<'a> {
		&self.prim
	}

	pub fn implementation_id(&self) -> Option<tf::Token> {
		self.get_token_attr(INFO_ID)
	}

	pub fn implementation_source(&self) -> Option<tf::Token> {
		self.get_token_attr(IMPLEMENTATION_SOURCE)
	}

	fn get_token_attr(&self, name: &str) -> Option<tf::Token> {
		let token = tf::Token::new(name);
		if self.prim.has_attribute(&token) {
			Some(self.prim.attribute(&token).get::<tf::Token>())
		} else {
			None
		}
	}

	pub fn inputs(&self) -> Vec<ShaderInput<'a>> {
		collect_ports(&self.prim, INPUT_PREFIX)
			.into_iter()
			.map(|(token, display)| ShaderInput {
				prim: self.prim.clone(),
				token,
				display_name: display,
			})
			.collect()
	}

	pub fn outputs(&self) -> Vec<ShaderOutput<'a>> {
		collect_ports(&self.prim, OUTPUT_PREFIX)
			.into_iter()
			.map(|(token, display)| ShaderOutput {
				prim: self.prim.clone(),
				token,
				display_name: display,
			})
			.collect()
	}
}

pub struct ShaderInput<'a> {
	prim: usd::Prim<'a>,
	token: tf::Token,
	display_name: String,
}

impl<'a> ShaderInput<'a> {
	pub fn name(&self) -> &str {
		&self.display_name
	}

	pub fn attribute(&self) -> usd::Attribute<'_> {
		self.prim.attribute(&self.token)
	}

	pub fn value(&self) -> Option<vt::Value> {
		self.attribute().get_value()
	}

	pub fn connection_targets(&self) -> Vec<sdf::Path> {
		relationship_targets(&self.prim, &self.token)
	}
}

pub struct ShaderOutput<'a> {
	prim: usd::Prim<'a>,
	token: tf::Token,
	display_name: String,
}

impl<'a> ShaderOutput<'a> {
	pub fn name(&self) -> &str {
		&self.display_name
	}

	pub fn attribute(&self) -> usd::Attribute<'_> {
		self.prim.attribute(&self.token)
	}

	pub fn connection_targets(&self) -> Vec<sdf::Path> {
		relationship_targets(&self.prim, &self.token)
	}
}

pub struct NodeGraph<'a> {
	prim: usd::Prim<'a>,
}

impl<'a> NodeGraph<'a> {
	pub fn get(stage: &'a usd::Stage, path: impl Into<sdf::Path>) -> Option<Self> {
		get_typed_prim(stage, path, "NodeGraph").map(|prim| Self { prim })
	}

	pub fn prim(&self) -> &usd::Prim<'a> {
		&self.prim
	}

	pub fn inputs(&self) -> Vec<ShaderInput<'a>> {
		collect_ports(&self.prim, INPUT_PREFIX)
			.into_iter()
			.map(|(token, display)| ShaderInput {
				prim: self.prim.clone(),
				token,
				display_name: display,
			})
			.collect()
	}

	pub fn outputs(&self) -> Vec<ShaderOutput<'a>> {
		collect_ports(&self.prim, OUTPUT_PREFIX)
			.into_iter()
			.map(|(token, display)| ShaderOutput {
				prim: self.prim.clone(),
				token,
				display_name: display,
			})
			.collect()
	}
}

pub struct Material<'a> {
	prim: usd::Prim<'a>,
}

impl<'a> Material<'a> {
	pub fn get(stage: &'a usd::Stage, path: impl Into<sdf::Path>) -> Option<Self> {
		get_typed_prim(stage, path, "Material").map(|prim| Self { prim })
	}

	pub fn prim(&self) -> &usd::Prim<'a> {
		&self.prim
	}

	pub fn outputs(&self) -> Vec<MaterialOutput<'a>> {
		collect_ports(&self.prim, OUTPUT_PREFIX)
			.into_iter()
			.map(|(token, display)| MaterialOutput {
				prim: self.prim.clone(),
				token,
				display_name: display,
			})
			.collect()
	}

	pub fn surface_output(&self) -> Option<MaterialOutput<'a>> {
		self.outputs()
			.into_iter()
			.find(|output| output.name() == "surface")
	}
}

pub struct MaterialOutput<'a> {
	prim: usd::Prim<'a>,
	token: tf::Token,
	display_name: String,
}

impl<'a> MaterialOutput<'a> {
	pub fn name(&self) -> &str {
		&self.display_name
	}

	pub fn attribute(&self) -> usd::Attribute<'_> {
		self.prim.attribute(&self.token)
	}

	pub fn connection_targets(&self) -> Vec<sdf::Path> {
		relationship_targets(&self.prim, &self.token)
	}
}

fn get_typed_prim<'a>(
	stage: &'a usd::Stage,
	path: impl Into<sdf::Path>,
	type_name: &str,
) -> Option<usd::Prim<'a>> {
	let path = path.into();
	let prim = stage.prim_at_path(path);
	if prim.is_valid() && prim.type_name() == tf::Token::new(type_name) {
		Some(prim)
	} else {
		None
	}
}

fn collect_ports(prim: &usd::Prim<'_>, prefix: &str) -> Vec<(tf::Token, String)> {
	prim.properties()
		.into_iter()
		.filter_map(|attr| {
			let name = attr.name();
			if let Some(base) = name.strip_prefix(prefix) {
				Some((tf::Token::new(name.as_str()), base.to_string()))
			} else {
				None
			}
		})
		.collect()
}

fn relationship_targets(prim: &usd::Prim<'_>, property_token: &tf::Token) -> Vec<sdf::Path> {
	let attr = prim.attribute(property_token);
	if let Some(paths) = attr.metadata::<sdf::PathListOp>(&sdf::FIELD_KEYS.connection_paths) {
		let mut targets = paths.explicit_items.clone();
		targets.extend(paths.added_items.iter().cloned());
		targets.extend(paths.appended_items.iter().cloned());
		if !targets.is_empty() {
			return targets;
		}
	}

	let rel_name = format!("{}{}", property_token.as_str(), CONNECT_SUFFIX);
	let rel = prim.relationship(&tf::Token::new(rel_name));
	rel.targets()
}


