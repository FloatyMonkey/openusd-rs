//! This script generates Rust code for USD schemas.
//! It is driven by a USD layer (schema.usda) that defines the schema classes.

use std::collections::HashSet;
use std::fs;
use std::io;
use std::path::Path;

use openusd_rs::{sdf, tf, usd, vt};

#[derive(Debug, Clone)]
struct ClassDef {
	name: String,
	documentation: String,
	inherit: String,
	properties: Vec<PropertyDef>,
}

#[derive(Debug, Clone)]
struct PropertyDef {
	name: String,
	api_name: String,
	documentation: String,
	is_relationship: bool,
}

fn parse_schema_file(path: &Path) -> io::Result<Vec<ClassDef>> {
	let stage = usd::Stage::open(path);
	let mut class_defs = Vec::new();

	let pseudo_root = stage.pseudo_root();

	process_prim(&pseudo_root, &mut class_defs);

	Ok(class_defs)
}

fn process_prim(prim: &usd::Prim, class_defs: &mut Vec<ClassDef>) {
	if let Some(specifier) = prim.specifier() {
		if specifier == sdf::Specifier::Class {
			let mut inherits = Vec::new();

			if let Some(inherit_paths) =
				prim.metadata::<sdf::PathListOp>(&sdf::FIELD_KEYS.inherit_paths)
			{
				for inherit_path in &inherit_paths.explicit_items {
					inherits.push(inherit_path.name_token().to_string());
				}
			}

			if inherits.len() > 1 {
				panic!("Multiple inheritance is not supported: {:?}", inherits);
			}

			if inherits.is_empty() {
				inherits.push("usd::SchemaBase".to_string());
			}

			let mut properties = Vec::new();

			if let Some(prop_names) =
				prim.metadata::<vt::Array<tf::Token>>(&sdf::CHILDREN_KEYS.property_children)
			{
				for prop_name in &prop_names {
					let prop = prim.property(&prop_name);

					// TODO: apiName should be of type tf::Token but parser detects it as String for now
					let api_name = prop
						.custom_data()
						.get("apiName")
						.and_then(|v| v.get::<String>())
						.unwrap_or_else(|| prop_name.to_string());

					let is_relationship = prop.spec_type() == Some(sdf::SpecType::Relationship);

					properties.push(PropertyDef {
						name: prop_name.to_string(),
						api_name,
						documentation: prop.documentation(),
						is_relationship,
					});
				}
			}

			class_defs.push(ClassDef {
				name: prim.path().name_token().to_string(),
				documentation: prim.documentation(),
				inherit: inherits[0].clone(),
				properties,
			});
		}
	}

	for child in prim.children() {
		process_prim(&child, class_defs);
	}
}

/// Converts camelCase to snake_case, stripping out non-alphanumeric characters.
fn snakecase_from_camelcase(name: &str) -> String {
	let mut result = String::new();
	let mut was_uppercase = false;

	for (i, c) in name.chars().enumerate() {
		if !c.is_alphanumeric() {
			if i > 0 && !was_uppercase {
				result.push('_');
			}
			was_uppercase = true;
			continue;
		}

		if c.is_uppercase() {
			if i > 0 && !was_uppercase {
				result.push('_');
			}
			result.push(c.to_lowercase().next().unwrap());
			was_uppercase = true;
		} else {
			result.push(c);
			was_uppercase = false;
		}
	}

	result.to_lowercase()
}

fn write_documentation(out: &mut String, doc: &str, indent: &str) {
	if !doc.is_empty() {
		// TODO: Iterator::intersperse (https://github.com/rust-lang/rust/issues/79524) would be nice here once stable.
		let mut lines = doc.trim().lines().map(|line| line.trim_start());
		if let Some(first) = lines.next() {
			out.push_str(&format!("{}/// {}", indent, first));
			for line in lines {
				out.push_str(&format!("\n{}/// {}", indent, line));
			}
		}
		out.push_str("\n");
	}
}

fn generate_tokens(classes: &[ClassDef]) -> String {
	let mut token_entries = HashSet::new();

	for class in classes {
		for prop in &class.properties {
			token_entries.insert(format!(
				"\t{}: \"{}\"",
				snakecase_from_camelcase(&prop.name),
				prop.name
			));
		}
	}

	let mut sorted_entries: Vec<_> = token_entries.into_iter().collect();
	sorted_entries.sort();

	format!(
		"declare_public_tokens!(Tokens, TOKENS, [\n{}\n]);",
		sorted_entries.join(",\n")
	)
}

fn generate_class(class: &ClassDef) -> String {
	let mut code = String::new();

	write_documentation(&mut code, &class.documentation, "");
	code.push_str("#[repr(transparent)]\n");
	code.push_str(&format!(
		"pub struct {}<'a>(usd::SchemaBase<'a>);\n\n",
		class.name
	));

	code.push_str(&format!("impl {}<'_> {{\n", class.name));
	code.push_str(&format!(
		"\tpub fn define(stage: &usd::Stage, path: impl Into<sdf::Path>) -> {}<'_> {{\n",
		class.name
	));
	code.push_str(&format!(
		"\t\t{}(usd::SchemaBase::new(stage.prim_at_path(path)))\n",
		class.name
	));
	code.push_str("\t}\n\n");

	for prop in &class.properties {
		write_documentation(&mut code, &prop.documentation, "\t");
		if prop.is_relationship {
			code.push_str(&format!(
				"\tpub fn {}_rel(&self) -> usd::Relationship<'_> {{\n",
				snakecase_from_camelcase(&prop.api_name)
			));
			code.push_str(&format!(
				"\t\tself.prim().relationship(&TOKENS.{})\n",
				snakecase_from_camelcase(&prop.name)
			));
			code.push_str("\t}\n\n");
		} else {
			code.push_str(&format!(
				"\tpub fn {}_attr(&self) -> usd::Attribute<'_> {{\n",
				snakecase_from_camelcase(&prop.api_name)
			));
			code.push_str(&format!(
				"\t\tself.prim().attribute(&TOKENS.{})\n",
				snakecase_from_camelcase(&prop.name)
			));
			code.push_str("\t}\n\n");
		}
	}

	code.push_str("}\n\n");

	code.push_str(&format!(
		"impl<'a> std::ops::Deref for {}<'a> {{\n",
		class.name
	));
	code.push_str(&format!("\ttype Target = {}<'a>;\n", class.inherit));
	code.push_str("\tfn deref(&self) -> &Self::Target {\n");
	code.push_str("\t\tunsafe { std::mem::transmute(self) }\n");
	code.push_str("\t}\n");
	code.push_str("}\n\n");

	code
}

pub fn generate_code_from_schema(schema_path: &Path, output_path: &Path) -> io::Result<()> {
	let classes = parse_schema_file(schema_path)?;

	let mut output = String::new();
	output.push_str("// This file is auto-generated by genschema.rs.\n");
	output.push_str("// Code added after the 'BEGIN CUSTOM CODE' marker will be preserved.\n\n");
	output.push_str("use crate::{declare_public_tokens, sdf, tf, usd};\n\n");
	output.push_str(&generate_tokens(&classes));
	output.push_str("\n\n");

	for class in &classes {
		output.push_str(&generate_class(class));
	}

	let custom_code_boundary = "// --- (BEGIN CUSTOM CODE) ---";
	let mut final_output = output;

	if let Ok(existing_content) = fs::read_to_string(output_path) {
		if let Some((_, custom_code)) = existing_content.split_once(custom_code_boundary) {
			final_output.push_str(custom_code_boundary);
			final_output.push_str(custom_code);
		} else {
			final_output.push_str(custom_code_boundary);
		}
	} else {
		final_output.push_str(custom_code_boundary);
	}

	fs::write(output_path, final_output)?;

	Ok(())
}

fn main() -> io::Result<()> {
	let schemas = vec![
		["src/usd_geom/schema.usda", "src/usd_geom/generated.rs"],
		["src/usd_lux/schema.usda", "src/usd_lux/generated.rs"],
		["src/usd_skel/schema.usda", "src/usd_skel/generated.rs"],
	];

	for [in_path_str, out_path_str] in schemas {
		let schema_path = Path::new(in_path_str);
		let output_path = Path::new(out_path_str);

		generate_code_from_schema(schema_path, output_path)?;
		println!(
			"Successfully generated Rust code from schema: {}",
			schema_path.display()
		);
	}

	Ok(())
}
