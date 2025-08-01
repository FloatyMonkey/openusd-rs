use crate::peg::{Error, Input, blank, eoi, list, one, one_or_more, opt, pad, sor, zero_or_more};
use crate::{sdf, tf};

struct Context {
	path: sdf::Path,
}

type In<'a> = Input<'a, Context>;
type PResult<T> = std::result::Result<T, Error>;

fn alpha_num<'a, C>(i: &mut Input<'a, C>) -> PResult<()> {
	match i.current_char() {
		Some(c) if c.is_alphanumeric() || c == '_' => {
			i.advance();
			Ok(())
		}
		_ => Err(Error::from_msg("Expected alphanumeric character")),
	}
}

fn identifier<'a, C>(i: &mut Input<'a, C>) -> PResult<&'a str> {
	let start_pos = i.pos;
	one_or_more(i, alpha_num)?;
	if start_pos == i.pos {
		Err(Error::from_msg("Expected identifier"))
	} else {
		Ok(i.slice(start_pos..i.pos))
	}
}

fn absolute_root<'a>(i: &mut In<'a>) -> PResult<()> {
	one(i, '/')?;
	i.ctx.path = sdf::Path::absolute_root_path();
	Ok(())
}

fn reflexive_relative<'a>(i: &mut In<'a>) -> PResult<()> {
	one(i, '.')?;
	i.ctx.path = sdf::Path::reflexive_relative_path();
	Ok(())
}

fn dot_dot<'a>(i: &mut In<'a>) -> PResult<()> {
	one(i, '.')?;
	one(i, '.')?;
	println!("DotDot");
	if i.ctx.path.is_empty() {
		i.ctx.path = sdf::Path::reflexive_relative_path();
	}
	i.ctx.path = i.ctx.path.parent_path();
	Ok(())
}

fn dot_dots<'a>(i: &mut In<'a>) -> PResult<()> {
	list(i, dot_dot, |i| one(i, '/'))?;
	Ok(())
}

fn variant_selection<'a>(i: &mut In<'a>) -> PResult<()> {
	pad(i, |i| one(i, '{'), blank)?;
	let variant_set = identifier(i)?;
	pad(i, |i| one(i, '='), blank)?;
	let variant = opt(i, identifier)?;
	pad(i, |i| one(i, '}'), blank)?;

	println!(
		"VariantSelection: {} = {}",
		variant_set,
		variant.unwrap_or("<None>")
	);

	i.ctx.path = i
		.ctx
		.path
		.append_variant_selection(variant_set, variant.unwrap_or_default());

	Ok(())
}

fn variant_selections<'a>(i: &mut In<'a>) -> PResult<()> {
	one_or_more(i, variant_selection)
}

fn prim_name<'a>(i: &mut In<'a>) -> PResult<()> {
	let name = identifier(i)?;

	println!("PrimName: {}", name);

	if i.ctx.path.is_empty() {
		i.ctx.path = sdf::Path::reflexive_relative_path();
	}

	i.ctx.path = i.ctx.path.append_child(&tf::Token::new(name));

	Ok(())
}

fn identifier_or_variant<'a>(i: &mut In<'a>) -> PResult<()> {
	let slash_identifier_parser = |i: &mut In<'a>| {
		one(i, '/')?;
		prim_name(i)
	};

	sor!(i, slash_identifier_parser, variant_selections)
}

fn prim_elts<'a>(i: &mut In<'a>) -> PResult<()> {
	prim_name(i)?;
	zero_or_more(i, identifier_or_variant)?;
	opt(i, variant_selections)?;
	Ok(())
}

fn property_name<'a>(i: &mut In<'a>) -> PResult<&'a str> {
	list(i, identifier, |i| one(i, ':'))
}

fn prop_elts<'a>(i: &mut In<'a>) -> PResult<&'a str> {
	one(i, '.')?;
	let prop_name = property_name(i)?;

	// TODO: optional TargetPathSeq, MapperPathSeq, Expression

	println!("PropElts: {}", prop_name);

	if i.ctx.path.is_empty() {
		i.ctx.path = sdf::Path::reflexive_relative_path();
	}
	i.ctx.path = i.ctx.path.append_property(&tf::Token::new(prop_name));

	Ok(prop_name)
}

fn path_elts<'a>(i: &mut In<'a>) -> PResult<()> {
	let prim_then_prop = |i: &mut In<'a>| -> PResult<()> {
		prim_elts(i)?;
		opt(i, prop_elts)?;
		Ok(())
	};

	let only_prop = |i: &mut In<'a>| {
		prop_elts(i)?;
		Ok(())
	};

	sor!(i, prim_then_prop, only_prop)
}

fn prim_first_path_elts<'a>(i: &mut In<'a>) -> PResult<()> {
	prim_elts(i)?;
	opt(i, prop_elts)?;
	Ok(())
}

fn path<'a>(i: &mut In<'a>) -> PResult<()> {
	let alt1 = |i: &mut In<'a>| -> PResult<()> {
		absolute_root(i)?;
		opt(i, prim_first_path_elts)?;
		Ok(())
	};

	let alt2 = |i: &mut In<'a>| -> PResult<()> {
		dot_dots(i)?;
		opt(i, |i| {
			one(i, '/')?;
			path_elts(i)
		})?;
		Ok(())
	};

	sor!(i, alt1, alt2, path_elts, reflexive_relative)
		.map_err(|_| Error::from_msg("No valid path alternative"))
}

pub fn parse_path<'a>(input: &'a str) -> PResult<sdf::Path> {
	if input.is_empty() {
		return Ok(sdf::Path::empty_path());
	}

	let mut ctx = Context {
		path: sdf::Path::empty_path(),
	};

	let mut input_state = Input::new(input, &mut ctx);
	path(&mut input_state)?;
	eoi(&mut input_state)?;

	Ok(ctx.path)
}

#[cfg(test)]
mod tests {
	use super::*;

	fn run_parse(path_str: &str) -> PResult<sdf::Path> {
		println!("\n--- Testing path: \"{}\" ---", path_str);
		let result = parse_path(path_str);
		println!("Parse result: {:?}", result);
		result
	}

	#[test]
	fn test_valid_paths() {
		assert!(run_parse("").is_ok());
		assert!(run_parse(".").is_ok());
		assert!(run_parse("/").is_ok());
		assert!(run_parse("/root/prim").is_ok());
		assert!(run_parse("my_identifier").is_ok());
		assert!(run_parse("prim.property").is_ok());
		assert!(run_parse("prim.prop:subprop:detail").is_ok());
		assert!(run_parse("..").is_ok());
		assert!(run_parse("../../relative/path").is_ok());
		assert!(run_parse("prim{var=val}").is_ok());
		assert!(run_parse("prim{ var = val }").is_ok());
		assert!(run_parse("prim{set=}").is_ok());
		assert!(run_parse("prim{ set = }").is_ok());
		assert!(run_parse("prim/ident{set=val}.prop").is_ok());
		assert!(run_parse("/p1/p2{v=a}{v=b}/p3.prop:name").is_ok());
		assert!(run_parse("../p1{v=a}/p2.prop").is_ok());
		assert!(run_parse("../../../foo_123/bar_456{variant=name}{foo=bar}.prop_name:sub").is_ok());
	}
}
