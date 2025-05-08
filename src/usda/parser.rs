use super::{Spec, UsdaFile};
use crate::peg::{
	Error, Input, blank, delimited, eoi, if_must, list_collect, must, one, one_or_more, opt, pad,
	pad_opt, preceded, sor, tag, terminated, until, zero_or_more,
};
use crate::sdf::{CHILDREN_KEYS, FIELD_KEYS};
use crate::{sdf, tf, vt};
use std::collections::HashMap;

type PResult<T> = std::result::Result<T, Error>;

pub struct Context {
	data: HashMap<sdf::Path, Spec>,
	current_path: sdf::Path,

	parent_children_stack: Vec<Vec<tf::Token>>,
	parent_properties_stack: Vec<Vec<tf::Token>>,
}

impl Context {
	pub fn new() -> Self {
		Context {
			data: HashMap::new(),
			current_path: sdf::Path::absolute_root_path(),
			parent_children_stack: vec![Vec::new()],
			parent_properties_stack: vec![Vec::new()],
		}
	}

	pub fn add_spec(&mut self, path: sdf::Path, spec: Spec) {
		self.data.insert(path, spec);
	}

	pub fn into_text_data(&self) -> UsdaFile {
		UsdaFile {
			data: self.data.clone(),
		}
	}
}

type In<'a> = Input<'a, Context>;

fn ws<'a, C>(i: &mut Input<'a, C>) -> PResult<()> {
	blank(i)
}

fn lexeme<'a, T, P, C>(i: &mut Input<'a, C>, parser: P) -> PResult<T>
where
	P: FnMut(&mut Input<'a, C>) -> PResult<T>,
{
	terminated(i, parser, ws)
}

fn symbol<'a, C>(i: &mut Input<'a, C>, s: &'static str) -> PResult<&'a str> {
	lexeme(i, |i| tag(i, s))
}

fn identifier<'a, C>(i: &mut Input<'a, C>) -> PResult<&'a str> {
	let start_pos = i.pos;
	// USD identifiers can contain alphanumeric, '_', and ':' (for namespaces)
	match i.current_char() {
		Some(c) if c.is_alphabetic() || c == '_' => {
			i.advance();
		}
		_ => return Err(Error::from_msg("Expected identifier")),
	}
	zero_or_more(i, |i| match i.current_char() {
		Some(c) if c.is_alphanumeric() || c == '_' || c == ':' => {
			i.advance();
			Ok(())
		}
		_ => Err(Error::from_msg("No match")),
	})?;
	Ok(i.slice(start_pos..i.pos))
}

fn utf8<'a, C>(i: &mut Input<'a, C>) -> PResult<()> {
	match i.current_char() {
		Some(_) => {
			i.advance();
			Ok(())
		}
		_ => Err(Error::from_msg("No match")),
	}
}

fn utf8_no_eolf<'a, C>(i: &mut Input<'a, C>) -> PResult<()> {
	match i.current_char() {
		Some(c) if c != '\n' && c != '\r' => {
			i.advance();
			Ok(())
		}
		_ => Err(Error::from_msg("No match")),
	}
}

fn eol<'a, C>(i: &mut Input<'a, C>) -> PResult<()> {
	match i.current_char() {
		Some(c) if c == '\n' || c == '\r' => {
			i.advance();
			Ok(())
		}
		_ => Err(Error::from_msg("No match")),
	}
}

// TODO: Wrong?
fn eoli<'a, C>(i: &mut Input<'a, C>) -> PResult<()> {
	match i.current_char() {
		Some(c) if c == '\n' || c == '\r' => {
			i.advance();
			Ok(())
		}
		_ => Err(Error::from_msg("No match")),
	}
}

fn two<'a, C>(i: &mut Input<'a, C>, c: char) -> PResult<()> {
	one(i, c)?;
	one(i, c)?;
	Ok(())
}

fn three<'a, C>(i: &mut Input<'a, C>, c: char) -> PResult<()> {
	one(i, c)?;
	one(i, c)?;
	one(i, c)?;
	Ok(())
}

fn space<'a, C>(i: &mut Input<'a, C>) -> PResult<()> {
	sor!(i, |i| one(i, ' '), |i| one(i, '\t'))
}

fn equals<'a, C>(i: &mut Input<'a, C>) -> PResult<()> {
	one(i, '=')
}

fn assignment<'a>(i: &mut In<'a>) -> PResult<()> {
	pad(i, equals, inline_padding)
}

// Comments

fn python_style_comment<'a>(i: &mut In<'a>) -> PResult<()> {
	one(i, '#')?;
	until(i, eoli, utf8)?;
	Ok(())
}

fn cpp_style_single_line_comment<'a>(i: &mut In<'a>) -> PResult<()> {
	two(i, '/')?;
	until(i, eoli, utf8)?;
	Ok(())
}

fn cpp_style_multi_line_comment<'a>(i: &mut In<'a>) -> PResult<()> {
	tag(i, "/*")?;
	until(i, |i| tag(i, "*/").map(|_| ()), |i| utf8(i))?;
	Ok(())
}

fn comment<'a>(i: &mut In<'a>) -> PResult<()> {
	sor!(
		i,
		|i| python_style_comment(i),
		|i| cpp_style_single_line_comment(i),
		|i| cpp_style_multi_line_comment(i)
	)
}

// Whitespace

fn inline_padding<'a>(i: &mut In<'a>) -> PResult<()> {
	sor!(i, space, cpp_style_multi_line_comment)
}

fn single_line_padding<'a>(i: &mut In<'a>) -> PResult<()> {
	sor!(i, space, comment)
}

fn multi_line_padding<'a>(i: &mut In<'a>) -> PResult<()> {
	sor!(i, space, eol, comment)
}

fn token_separator<'a>(i: &mut In<'a>) -> PResult<()> {
	zero_or_more(i, cpp_style_multi_line_comment)?;
	space(i)?;
	zero_or_more(i, inline_padding)
}

// Array Type

fn array_type<'a>(i: &mut In<'a>) -> PResult<()> {
	if_must(
		i,
		|i| one(i, '['),
		|i| {
			zero_or_more(i, inline_padding)?;
			one(i, ']')
		},
	)
}

fn escape_quote<'a>(i: &mut In<'a>, quote: char) -> PResult<()> {
	one(i, '\\')?;
	one(i, quote)
}

fn single_line_string<'a>(i: &mut In<'a>, quote: char) -> PResult<&'a str> {
	if_must(
		i,
		|i| one(i, quote),
		|i| {
			until(
				i,
				|i| one(i, quote),
				|i| {
					sor!(i, |i| two(i, '\\'), |i| escape_quote(i, quote), |i| {
						utf8_no_eolf(i)
					})
				},
			)
		},
	)
}

fn multi_line_string<'a>(i: &mut In<'a>, quote: char) -> PResult<&'a str> {
	if_must(
		i,
		|i| three(i, quote),
		|i| {
			until(
				i,
				|i| three(i, quote),
				|i| sor!(i, |i| two(i, '\\'), |i| escape_quote(i, quote), utf8),
			)
		},
	)
}

fn string_literal<'a>(i: &mut In<'a>) -> PResult<&'a str> {
	sor!(
		i,
		|i| multi_line_string(i, '\''),
		|i| single_line_string(i, '\''),
		|i| multi_line_string(i, '"'),
		|i| single_line_string(i, '"'),
	)
}

fn number<'a, C>(i: &mut Input<'a, C>) -> PResult<&'a str> {
	// TODO: Handle floats, scientific notation, signs
	let start_pos = i.pos;
	opt(i, |i| one(i, '-'))?;
	one_or_more(i, |i| match i.current_char() {
		Some(c) if c.is_digit(10) => {
			i.advance();
			Ok(())
		}
		_ => Err(Error::from_msg("No match")),
	})?;
	opt(i, |i| {
		one(i, '.')?;
		one_or_more(i, |i| match i.current_char() {
			Some(c) if c.is_digit(10) => {
				i.advance();
				Ok(())
			}
			_ => Err(Error::from_msg("No match")),
		})
	})?;
	Ok(i.slice(start_pos..i.pos))
}

fn path_ref<'a>(i: &mut In<'a>) -> PResult<sdf::Path> {
	let start_pos = i.pos;
	one(i, '<')?;
	while let Some(c) = i.current_char() {
		if c == '>' {
			break;
		}
		i.advance();
	}
	let content_end = i.pos;
	one(i, '>')?;
	let parsed_ref = i.slice(start_pos + 1..content_end);
	Ok(sdf::Path::from(parsed_ref))
}

fn asset_ref<'a>(i: &mut In<'a>) -> PResult<sdf::AssetPath> {
	let path = if_must(
		i,
		|i| one(i, '@'),
		|i| until(i, |i| one(i, '@'), |i| utf8(i)),
	)?;

	Ok(sdf::AssetPath {
		asset_path: path.to_string(),
		resolved_path: String::new(),
	})
}

// Skips tokens until the matching closer is found. Does NOT parse contents.
fn skip_until<'a, C>(i: &mut Input<'a, C>, opener: char, closer: char) -> PResult<()> {
	let mut level = 1;
	while let Some(c) = i.current_char() {
		i.advance();
		if c == opener {
			level += 1;
		} else if c == closer {
			level -= 1;
			if level == 0 {
				return Ok(());
			}
		}
	}
	Err(Error::from_msg("Unmatched opener/closer"))
}

fn parse_array<'a, C>(i: &mut Input<'a, C>) -> PResult<()> {
	preceded(
		i,
		|i| symbol(i, "[").map(|_| ()),
		|i| skip_until(i, '[', ']'),
	)?;
	ws(i)
}

fn parse_tuple<'a, C>(i: &mut Input<'a, C>) -> PResult<()> {
	preceded(
		i,
		|i| symbol(i, "(").map(|_| ()),
		|i| skip_until(i, '(', ')'),
	)?;
	ws(i)
}

fn dictionary_item<'a>(i: &mut In<'a>) -> PResult<(String, vt::Value)> {
	let _ty = lexeme(i, identifier)?;
	let key = lexeme(i, identifier)?;
	symbol(i, "=")?;
	let value = parse_value(i)?;
	Ok((key.to_string(), value))
}

fn dictionary_value<'a>(i: &mut In<'a>) -> PResult<vt::Dictionary> {
	let items = delimited(
		i,
		|i| symbol(i, "{").map(|_| ()),
		|i| list_collect(i, dictionary_item, ws),
		|i| symbol(i, "}").map(|_| ()),
	)?;
	Ok(items.into_iter().collect())
}

fn parse_value<'a>(i: &mut In<'a>) -> PResult<vt::Value> {
	sor!(
		i,
		|i| lexeme(i, string_literal).map(|s| vt::Value::new(s.to_string())),
		|i| lexeme(i, number).and_then(|n_str| {
			n_str
				.parse::<f64>()
				.map(vt::Value::new)
				.map_err(|_| Error::from_msg("Invalid number"))
		}),
		|i| lexeme(i, identifier).and_then(|id| match id {
			"true" => Ok(vt::Value::new(true)),
			"false" => Ok(vt::Value::new(false)),
			_ => Ok(vt::Value::new(tf::Token::new(id))), // Treat as Token for now
		}),
		|i| lexeme(i, path_ref).map(|path| vt::Value::new(path)),
		|i| lexeme(i, asset_ref).map(|asset| vt::Value::new(asset)),
		|i| parse_tuple(i).map(|_| vt::Value::empty()),
		|i| parse_array(i).map(|_| vt::Value::empty()),
		|i| dictionary_value(i).map(|dict| vt::Value::new(dict)),
	)
	.map_err(|_| Error::from_msg("Invalid value type"))
}

fn parse_metadata_item<'a>(i: &mut In<'a>) -> PResult<(String, vt::Value)> {
	let key = lexeme(i, identifier)?;
	symbol(i, "=")?;
	let value = parse_value(i)?;
	Ok((key.to_string(), value))
}

fn parse_metadata<'a>(i: &mut In<'a>) -> PResult<HashMap<String, vt::Value>> {
	let items = delimited(
		i,
		|i| symbol(i, "(").map(|_| ()),
		|i| list_collect(i, parse_metadata_item, ws),
		|i| symbol(i, ")").map(|_| ()),
	)?;
	Ok(items.into_iter().collect())
}

fn attribute_variability<'a>(i: &mut In<'a>) -> PResult<sdf::Variability> {
	sor!(
		i,
		|i| tag(i, "config").map(|_| sdf::Variability::Uniform),
		|i| tag(i, "uniform").map(|_| sdf::Variability::Uniform),
	)
}

fn property_spec<'a>(i: &mut In<'a>) -> PResult<()> {
	let is_custom = opt(i, |i| symbol(i, "custom").map(|_| true))?.unwrap_or(false);

	let variability = opt(i, |i| terminated(i, attribute_variability, token_separator))?;

	let type_name_str = identifier(i)?;

	let is_array = opt(i, |i| {
		preceded(i, |i| zero_or_more(i, inline_padding), array_type)
	})?
	.is_some();

	ws(i)?; // TODO: use token_separator()

	let type_name = if is_array {
		tf::Token::new(format!("{}[]", type_name_str))
	} else {
		tf::Token::new(type_name_str)
	};

	let name = lexeme(i, identifier)?;
	let name_token = tf::Token::new(name);

	let value = opt(i, |i| {
		assignment(i)?;
		parse_value(i)
	})?;

	let metadata = opt(i, parse_metadata)?;

	let mut spec = Spec::new(sdf::SpecType::Attribute);
	spec.add(&FIELD_KEYS.custom, is_custom);
	if let Some(var) = variability {
		spec.add(&FIELD_KEYS.variability, var);
	}
	spec.add(&FIELD_KEYS.type_name, type_name);
	if let Some(value) = value {
		spec.add(&FIELD_KEYS.default, value);
	}

	if let Some(meta) = metadata {
		if let Some(documentation) = meta.get("doc") {
			spec.add(&FIELD_KEYS.documentation, documentation.clone());
		}

		if let Some(custom_data) = meta.get("customData") {
			spec.add(&FIELD_KEYS.custom_data, custom_data.clone());
		}
	}

	let parent_path = &i.ctx.current_path;
	let path = parent_path.append_property(&name_token);

	if let Some(properties) = i.ctx.parent_properties_stack.last_mut() {
		properties.push(name_token.clone());
	}

	i.ctx.add_spec(path, spec);

	println!("Parsed property: {} {}", type_name_str, name);

	Ok(())
}

fn specifier<'a>(i: &mut In<'a>) -> PResult<sdf::Specifier> {
	sor!(
		i,
		|i| tag(i, "def").map(|_| sdf::Specifier::Def),
		|i| tag(i, "over").map(|_| sdf::Specifier::Over),
		|i| tag(i, "class").map(|_| sdf::Specifier::Class)
	)
	.map_err(|_| Error::from_msg("Expected specifier"))
}

fn prim_spec<'a>(i: &mut In<'a>) -> PResult<()> {
	let specifier = specifier(i)?;
	token_separator(i)?;

	must(i, |i| {
		let type_name =
			opt(i, |i| terminated(i, identifier, token_separator))?.map(|s| tf::Token::new(s));

		let name = string_literal(i)
			.map(|s| tf::Token::new(s))
			.map_err(|_| Error::from_msg("Expected prim name"))?;

		let metadata = pad_opt(i, parse_metadata, multi_line_padding)?;

		let parrent_path = i.ctx.current_path.clone();
		let path = parrent_path.append_child(&name);
		i.ctx.current_path = path.clone();

		i.ctx.parent_children_stack.push(Vec::new());
		i.ctx.parent_properties_stack.push(Vec::new());

		delimited(
			i,
			|i| symbol(i, "{").map(|_| ()),
			|i| {
				zero_or_more(i, |i| {
					ws(i)?;
					sor!(i, prim_spec, property_spec)
				})
			},
			|i| symbol(i, "}").map(|_| ()),
		)?;

		let children = i.ctx.parent_children_stack.pop().unwrap_or_default();
		let properties = i.ctx.parent_properties_stack.pop().unwrap_or_default();

		let mut spec = Spec::new(sdf::SpecType::Prim);
		spec.add(&FIELD_KEYS.specifier, specifier);
		if let Some(type_name) = &type_name {
			spec.add(&FIELD_KEYS.type_name, type_name.clone());
		}
		if !children.is_empty() {
			spec.add(&CHILDREN_KEYS.prim_children, children);
		}
		if !properties.is_empty() {
			spec.add(&CHILDREN_KEYS.property_children, properties);
		}

		if let Some(meta) = metadata {
			if let Some(inherits) = meta.get("inherits") {
				if let Some(path) = inherits.get::<sdf::Path>() {
					let mut inherit_paths = sdf::PathListOp::default();
					inherit_paths.explicit_items.push(path.clone());
					inherit_paths.is_explicit = true;
					spec.add(&FIELD_KEYS.inherit_paths, inherit_paths.clone());
					println!("Parsed inherits: {:?}", inherit_paths);
				}
			}

			if let Some(documentation) = meta.get("doc") {
				spec.add(&FIELD_KEYS.documentation, documentation.clone());
			}
		}

		if let Some(parent_children) = i.ctx.parent_children_stack.last_mut() {
			parent_children.push(name.clone());
		}

		i.ctx.add_spec(path, spec);

		i.ctx.current_path = parrent_path;

		println!("Parsed prim: {:?} {} {:?}", specifier, name, type_name);

		Ok(())
	})
}

fn layer_header<'a>(i: &mut In<'a>) -> PResult<&'a str> {
	must(i, |i| {
		one(i, '#')?;
		until(i, eoli, utf8)
	})
}

pub fn layer_spec<'a>(i: &mut In<'a>) -> PResult<()> {
	layer_header(i)?;

	pad_opt(i, parse_metadata, multi_line_padding)?;

	zero_or_more(i, |i| {
		ws(i)?;
		prim_spec(i)
	})?;

	ws(i)?;

	Ok(())
}

pub fn parse<'a>(input_str: &'a str) -> PResult<UsdaFile> {
	let mut ctx = Context::new();
	let mut input = Input::new(input_str, &mut ctx);

	let mut pseudo_root = Spec::new(sdf::SpecType::PseudoRoot);
	let root_path = sdf::Path::absolute_root_path();

	let result = layer_spec(&mut input);
	if let Err(error) = &result {
		let error_str = format!("{:?}", error);
		println!("{}", input.format_error(&error_str));
		return Err(error.clone());
	}

	let root_children = input.ctx.parent_children_stack.pop().unwrap_or_default();

	pseudo_root.add(&CHILDREN_KEYS.prim_children, root_children);

	input.ctx.add_spec(root_path, pseudo_root);

	let text_data = input.ctx.into_text_data();

	eoi(&mut input)?;

	Ok(text_data)
}

#[cfg(test)]
mod tests {
	use super::*;

	fn run_parse<'a, T: std::fmt::Debug, P>(
		ctx: &'a mut Context,
		parser: P,
		input_str: &'a str,
	) -> PResult<T>
	where
		P: FnOnce(&mut In<'a>) -> PResult<T>,
	{
		let mut input = Input::new(input_str, ctx);
		let result = parser(&mut input);
		println!("Input: \"{}\"", input_str);
		println!("Result: {:?}", result);

		if let Err(error) = &result {
			let error_str = format!("{:?}", error);
			println!("Error: {:?}", input.format_error(&error_str));
		}

		result
	}

	#[test]
	fn test_identifier() {
		let mut c = Context::new();
		assert!(run_parse(&mut c, identifier, "hello").is_ok());
		assert!(run_parse(&mut c, identifier, "hello_world").is_ok());
		assert!(run_parse(&mut c, identifier, "_private").is_ok());
		assert!(run_parse(&mut c, identifier, "namespace:token").is_ok());
		assert!(run_parse(&mut c, identifier, "valid123").is_ok());
		assert!(run_parse(&mut c, identifier, "1invalid").is_err());
		assert!(run_parse(&mut c, identifier, " invalid").is_err());
	}

	#[test]
	fn test_string_literal() {
		let mut c = Context::new();
		assert_eq!(
			run_parse(&mut c, string_literal, "\"hello\"").unwrap(),
			"hello"
		);
		assert_eq!(
			run_parse(&mut c, string_literal, "\"with space\"").unwrap(),
			"with space"
		);
		assert!(run_parse(&mut c, string_literal, "hello").is_err());
		assert!(run_parse(&mut c, string_literal, "\"unterminated").is_err());
	}

	#[test]
	fn test_number() {
		let mut c = Context::new();
		assert_eq!(run_parse(&mut c, number, "123").unwrap(), "123");
		assert_eq!(run_parse(&mut c, number, "123.45").unwrap(), "123.45");
		assert_eq!(run_parse(&mut c, number, "-42").unwrap(), "-42");
		assert_eq!(run_parse(&mut c, number, "-0.5").unwrap(), "-0.5");
		// TODO: These crash the parser
		//assert!(run_parse(&mut c, number, "abc").is_err());
		//assert!(run_parse(&mut c, number, "1.2.3").is_err());
	}

	#[test]
	fn test_path_literal() {
		let mut c = Context::new();
		assert_eq!(
			run_parse(&mut c, path_ref, "</Root/Prim>").unwrap(),
			sdf::Path::from("/Root/Prim")
		);
		assert_eq!(
			run_parse(&mut c, path_ref, "<.>").unwrap(),
			sdf::Path::from(".")
		);
		assert!(run_parse(&mut c, path_ref, "/Root/Prim>").is_err());
		assert!(run_parse(&mut c, path_ref, "<Root/Prim").is_err());
	}

	#[test]
	fn test_asset_path_literal() {
		let mut c = Context::new();
		assert_eq!(
			run_parse(&mut c, asset_ref, "@/asset/path.png@").unwrap(),
			sdf::AssetPath {
				asset_path: "/asset/path.png".to_string(),
				resolved_path: String::new(),
			}
		);
		assert_eq!(
			run_parse(&mut c, asset_ref, "@./relative.usd@").unwrap(),
			sdf::AssetPath {
				asset_path: "./relative.usd".to_string(),
				resolved_path: String::new(),
			}
		);
		assert!(run_parse(&mut c, asset_ref, "/asset/path.png@").is_err());
		assert!(run_parse(&mut c, asset_ref, "@/asset/path.png").is_err());
	}

	#[test]
	fn test_skip_array() {
		let mut c = Context::new();
		let result = run_parse(&mut c, parse_value, "[1, 2]");
		assert!(result.unwrap().is_empty());

		let mut ctx = ();
		let mut input = Input::new("[1, 2] trailing", &mut ctx);
		let skip_res = parse_array(&mut input);
		assert!(skip_res.is_ok());
		assert_eq!(input.current_char(), Some('t'));
	}

	#[test]
	fn test_skip_tuple() {
		let mut c = Context::new();
		let result = run_parse(&mut c, parse_value, "(1, 2.5, \"world\")");
		assert!(result.unwrap().is_empty());

		let mut ctx = ();
		let mut input = Input::new("(1, 2) trailing", &mut ctx);
		let skip_res = parse_tuple(&mut input);
		assert!(skip_res.is_ok());
		assert_eq!(input.current_char(), Some('t'));
	}

	#[test]
	fn test_parse_metadata() {
		let mut c = Context::new();
		let input = r#"(
            defaultPrim = "hello"
            metersPerUnit = 1.0
            customData = {
                string test = "value"
            }
        )"#;
		let result = run_parse(&mut c, parse_metadata, input);
		assert!(result.is_ok());
		let map = result.unwrap();
		assert_eq!(map.len(), 3);
		assert!(map.contains_key("defaultPrim"));
		assert!(map.contains_key("metersPerUnit"));
		assert!(map.contains_key("customData"));
		assert_eq!(
			map["customData"].get_unchecked::<vt::Dictionary>()["test"].get_unchecked::<String>(),
			"value"
		);
	}

	#[test]
	fn test_parse_simple_prim_structure() {
		let input = r#"#usda 1.0
        def Xform "hello" ( test = 1 ) { }
        over Sphere "world" { }
        "#;
		let result = parse(input);
		assert!(result.is_ok());
	}

	#[test]
	fn test_parse_full_structure() {
		let input = std::fs::read_to_string("resources/cube.usda").unwrap();
		let result = parse(&input);

		if let Ok(data) = &result {
			sdf::debug_dump(data);
		}

		assert!(result.is_ok());
	}
}
