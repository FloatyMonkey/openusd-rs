use super::{compression, integer_coding::*};
use crate::io_ext::{ReadBytesExt, WriteBytesExt};
use crate::{gf, sdf, tf, vt};

use std::collections::HashMap;
use std::{
	cell::RefCell,
	collections::HashSet,
	io::{Cursor, Read, Seek, SeekFrom, Write},
};

use half::f16;

type Result<T> = std::io::Result<T>;

type Index = u32;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
struct Version {
	pub major: u8,
	pub minor: u8,
	pub patch: u8,
}

impl Version {
	fn new(major: u8, minor: u8, patch: u8) -> Self {
		Self {
			major,
			minor,
			patch,
		}
	}
}

impl std::fmt::Display for Version {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
	}
}

#[repr(C)]
struct Bootstrap {
	ident: [u8; 8],
	version: [u8; 8],
	toc_offset: u64,
	_reserved: [u64; 8],
}

#[repr(C)]
struct Section {
	name: [u8; 16],
	pub start: u64,
	pub size: u64,
}

impl Section {
	const TOKENS: &str = "TOKENS";
	const STRINGS: &str = "STRINGS";
	const FIELDS: &str = "FIELDS";
	const FIELDSETS: &str = "FIELDSETS";
	const PATHS: &str = "PATHS";
	const SPECS: &str = "SPECS";

	const KNOWN_SECTIONS: &[&str] = &[
		Self::TOKENS,
		Self::STRINGS,
		Self::FIELDS,
		Self::FIELDSETS,
		Self::PATHS,
		Self::SPECS,
	];

	fn name(&self) -> Option<&str> {
		let len = self.name.iter().position(|&x| x == 0)?;
		str::from_utf8(&self.name[0..len]).ok()
	}

	fn is_known(&self) -> bool {
		self.name()
			.is_some_and(|name| Self::KNOWN_SECTIONS.contains(&name))
	}
}

#[derive(Default)]
struct TableOfContents {
	sections: Vec<Section>,
}

impl TableOfContents {
	fn section(&self, name: &str) -> Option<&Section> {
		self.sections.iter().find(|s| s.name() == Some(name))
	}

	fn add_section(&mut self, name: &str, start: u64, size: u64) {
		let name = {
			let mut arr = [0u8; 16];
			arr[..name.len()].copy_from_slice(name.as_bytes());
			arr
		};
		self.sections.push(Section { name, start, size });
	}

	fn minimum_section_start(&self) -> u64 {
		self.sections
			.iter()
			.min_by(|l, r| l.start.cmp(&r.start))
			.map_or(size_of::<Bootstrap>() as u64, |s| s.start)
	}
}

#[repr(C)]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Field {
	token_index: TokenIndex,
	value_rep: ValueRep,
}

#[repr(C)]
struct Spec {
	path_index: PathIndex,
	field_set_index: FieldSetIndex,
	spec_type: sdf::SpecType,
}

#[repr(u32)]
#[derive(Debug, Eq, PartialEq)]
enum Type {
	Invalid = 0,

	// Array types
	Bool = 1,
	UChar = 2,
	Int = 3,
	UInt = 4,
	Int64 = 5,
	UInt64 = 6,
	Half = 7,
	Float = 8,
	Double = 9,
	String = 10,
	Token = 11,
	AssetPath = 12,
	Quatd = 16,
	Quatf = 17,
	Quath = 18,
	Vec2d = 19,
	Vec2f = 20,
	Vec2h = 21,
	Vec2i = 22,
	Vec3d = 23,
	Vec3f = 24,
	Vec3h = 25,
	Vec3i = 26,
	Vec4d = 27,
	Vec4f = 28,
	Vec4h = 29,
	Vec4i = 30,
	Matrix2d = 13,
	Matrix3d = 14,
	Matrix4d = 15,
	TimeCode = 56,
	PathExpression = 57,

	// Non-array types
	Dictionary = 31,
	TokenListOp = 32,
	StringListOp = 33,
	PathListOp = 34,
	ReferenceListOp = 35,
	IntListOp = 36,
	Int64ListOp = 37,
	UIntListOp = 38,
	UInt64ListOp = 39,
	PathVector = 40,
	TokenVector = 41,
	Specifier = 42,
	Permission = 43,
	Variability = 44,
	VariantSelectionMap = 45,
	TimeSamples = 46,
	Payload = 47,
	DoubleVector = 48,
	LayerOffsetVector = 49,
	StringVector = 50,
	ValueBlock = 51,
	Value = 52,
	UnregisteredValue = 53,
	UnregisteredValueListOp = 54,
	PayloadListOp = 55,
	Relocates = 58,
	Spline = 59,
}

#[repr(C)]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct ValueRep(u64);

impl ValueRep {
	const IS_ARRAY_BIT: u64 = 1 << 63;
	const IS_INLINED_BIT: u64 = 1 << 62;
	const IS_COMPRESSED_BIT: u64 = 1 << 61;
	const PAYLOAD_MASK: u64 = (1 << 48) - 1;

	fn is_array(&self) -> bool {
		self.0 & Self::IS_ARRAY_BIT != 0
	}

	fn set_is_array(&mut self) {
		self.0 |= Self::IS_ARRAY_BIT;
	}

	fn is_inlined(&self) -> bool {
		self.0 & Self::IS_INLINED_BIT != 0
	}

	fn set_is_inlined(&mut self) {
		self.0 |= Self::IS_INLINED_BIT;
	}

	fn is_compressed(&self) -> bool {
		self.0 & Self::IS_COMPRESSED_BIT != 0
	}

	fn set_is_compressed(&mut self) {
		self.0 |= Self::IS_COMPRESSED_BIT;
	}

	fn ty(&self) -> Type {
		unsafe { std::mem::transmute::<u32, Type>(((self.0 >> 48) & 0xFF) as u32) }
	}

	fn set_ty(&mut self, t: Type) {
		self.0 &= !(0xFF << 48);
		self.0 |= (t as u64) << 48;
	}

	fn payload(&self) -> u64 {
		self.0 & Self::PAYLOAD_MASK
	}

	fn set_payload(&mut self, payload: u64) {
		self.0 &= !Self::PAYLOAD_MASK;
		self.0 |= payload & Self::PAYLOAD_MASK;
	}
}

pub struct ListOpHeader(u8);

impl ListOpHeader {
	const IS_EXPLICIT_BIT: u8 = 1 << 0;
	const HAS_EXPLICIT_ITEMS_BIT: u8 = 1 << 1;
	const HAS_ADDED_ITEMS_BIT: u8 = 1 << 2;
	const HAS_DELETED_ITEMS_BIT: u8 = 1 << 3;
	const HAS_ORDERED_ITEMS_BIT: u8 = 1 << 4;
	const HAS_PREPENDED_ITEMS_BIT: u8 = 1 << 5;
	const HAS_APPENDED_ITEMS_BIT: u8 = 1 << 6;

	pub fn is_explicit(&self) -> bool {
		self.0 & Self::IS_EXPLICIT_BIT != 0
	}

	pub fn has_explicit_items(&self) -> bool {
		self.0 & Self::HAS_EXPLICIT_ITEMS_BIT != 0
	}

	pub fn has_added_items(&self) -> bool {
		self.0 & Self::HAS_ADDED_ITEMS_BIT != 0
	}

	pub fn has_prepended_items(&self) -> bool {
		self.0 & Self::HAS_PREPENDED_ITEMS_BIT != 0
	}

	pub fn has_appended_items(&self) -> bool {
		self.0 & Self::HAS_APPENDED_ITEMS_BIT != 0
	}

	pub fn has_deleted_items(&self) -> bool {
		self.0 & Self::HAS_DELETED_ITEMS_BIT != 0
	}

	pub fn has_ordered_items(&self) -> bool {
		self.0 & Self::HAS_ORDERED_ITEMS_BIT != 0
	}
}

impl<T> From<&sdf::ListOp<T>> for ListOpHeader {
	fn from(op: &sdf::ListOp<T>) -> Self {
		let mut header = ListOpHeader(0);

		if op.is_explicit {
			header.0 |= ListOpHeader::IS_EXPLICIT_BIT;
		}

		if !op.explicit_items.is_empty() {
			header.0 |= ListOpHeader::HAS_EXPLICIT_ITEMS_BIT;
		}

		if !op.added_items.is_empty() {
			header.0 |= ListOpHeader::HAS_ADDED_ITEMS_BIT;
		}

		if !op.prepended_items.is_empty() {
			header.0 |= ListOpHeader::HAS_PREPENDED_ITEMS_BIT;
		}

		if !op.appended_items.is_empty() {
			header.0 |= ListOpHeader::HAS_APPENDED_ITEMS_BIT;
		}

		if !op.deleted_items.is_empty() {
			header.0 |= ListOpHeader::HAS_DELETED_ITEMS_BIT;
		}

		if !op.ordered_items.is_empty() {
			header.0 |= ListOpHeader::HAS_ORDERED_ITEMS_BIT;
		}

		header
	}
}

fn read_toc(cursor: &mut Cursor<&[u8]>) -> Result<TableOfContents> {
	let mut sections = vec![];

	let toc_offset = cursor.read_as::<u64>()?;
	cursor.set_position(toc_offset);

	let section_count = cursor.read_as::<u64>()?;

	for _ in 0..section_count {
		let mut name = [0; 16];
		cursor.read_exact(&mut name).unwrap();

		let start = cursor.read_as::<u64>()?;
		let size = cursor.read_as::<u64>()?;

		sections.push(Section { name, start, size });
	}

	Ok(TableOfContents { sections })
}

fn write_tokens(c: &mut Cursor<Vec<u8>>, tokens: &[tf::Token]) -> Result<()> {
	let mut uncompressed = Vec::new();

	for token in tokens {
		uncompressed.extend_from_slice(token.as_str().as_bytes());
		uncompressed.push(b'\0');
	}

	let compressed = compression::compress_to_buffer(&uncompressed);

	c.write_as::<u64>(tokens.len() as u64)?;
	c.write_as::<u64>(uncompressed.len() as u64)?;
	c.write_as::<u64>(compressed.len() as u64)?;
	c.write_all(&compressed)?;

	Ok(())
}

fn read_tokens(cursor: &mut Cursor<&[u8]>, section: &Section) -> Result<Vec<tf::Token>> {
	cursor.set_position(section.start);

	let token_count = cursor.read_as::<u64>()?;
	let uncompressed_size = cursor.read_as::<u64>()?;
	let compressed_size = cursor.read_as::<u64>()?;

	let mut compressed_buffer = vec![0; compressed_size as usize];
	cursor.read_exact(&mut compressed_buffer)?;

	let buffer =
		compression::decompress_from_buffer(&compressed_buffer, uncompressed_size as usize);

	if buffer.last() != Some(&b'\0') {
		panic!("Tokens section not null-terminated in crate file");
	}

	let tokens = buffer[..buffer.len() - 1]
		.split(|c| *c == b'\0')
		.map(|b| std::str::from_utf8(b).unwrap())
		.map(|s| tf::Token::new(s))
		.collect::<Vec<_>>();

	// TODO: Add a range check to all sections
	if tokens.len() != token_count as usize {
		panic!(
			"Crate file claims {} tokens, found {}",
			token_count,
			tokens.len()
		);
	}

	Ok(tokens)
}

fn write_strings(cursor: &mut Cursor<Vec<u8>>, strings: &[TokenIndex]) -> Result<()> {
	cursor.write_as::<u64>(strings.len() as u64)?;

	for string in strings {
		cursor.write_as::<Index>(string.0)?;
	}

	Ok(())
}

fn read_strings(cursor: &mut Cursor<&[u8]>, section: &Section) -> Result<Vec<TokenIndex>> {
	cursor.set_position(section.start);

	let string_count = cursor.read_as::<u64>()? as usize;
	let mut indices = Vec::with_capacity(string_count as usize);

	// TODO: Replace this with a single read_exact call
	for _ in 0..string_count {
		indices.push(TokenIndex(cursor.read_as::<Index>()?));
	}

	Ok(indices)
}

fn write_fields(cursor: &mut Cursor<Vec<u8>>, fields: &[Field]) -> Result<()> {
	cursor.write_as::<u64>(fields.len() as u64)?;

	// Token indices.
	let token_indices = fields.iter().map(|f| f.token_index.0).collect::<Vec<_>>();
	write_compressed_ints::<u32>(cursor, &token_indices)?;

	// ValueReps.
	let value_reps = fields
		.iter()
		.flat_map(|f| f.value_rep.0.to_ne_bytes())
		.collect::<Vec<_>>();
	let compressed_value_reps = compression::compress_to_buffer(&value_reps);
	cursor.write_as::<u64>(compressed_value_reps.len() as u64)?;
	cursor.write_all(&compressed_value_reps)?;

	Ok(())
}

fn read_fields(cursor: &mut Cursor<&[u8]>, section: &Section) -> Result<Vec<Field>> {
	cursor.set_position(section.start);

	let field_count = cursor.read_as::<u64>()? as usize;

	let token_indices = read_compressed_ints::<u32>(cursor, field_count)?;

	let compressed_size = cursor.read_as::<u64>()?;
	let mut compressed_buffer = vec![0; compressed_size as usize];
	cursor.read_exact(&mut compressed_buffer)?;

	let buffer = compression::decompress_from_buffer(
		&compressed_buffer,
		field_count * size_of::<ValueRep>(),
	);
	let mut value_cursor = Cursor::new(buffer.as_slice());

	let fields = token_indices
		.iter()
		.map(|&index| Field {
			token_index: TokenIndex(index),
			value_rep: ValueRep(value_cursor.read_as::<u64>().unwrap()),
		})
		.collect();

	Ok(fields)
}

fn write_field_sets(cursor: &mut Cursor<Vec<u8>>, field_sets: &[FieldIndex]) -> Result<()> {
	cursor.write_as::<u64>(field_sets.len() as u64)?;
	let temp = field_sets.iter().map(|fs| fs.0).collect::<Vec<_>>();
	write_compressed_ints::<u32>(cursor, &temp)
}

fn read_field_sets(cursor: &mut Cursor<&[u8]>, section: &Section) -> Result<Vec<FieldIndex>> {
	cursor.set_position(section.start);

	let field_set_count = cursor.read_as::<u64>()? as usize;
	let temp = read_compressed_ints::<Index>(cursor, field_set_count)?;
	// TODO: Prevent this extra allocation, read compressed directly as FieldIndex
	Ok(temp.into_iter().map(|i| FieldIndex(i)).collect())
}

fn write_paths(c: &mut Cursor<Vec<u8>>, paths: &[sdf::Path], file: &UsdcFile) -> Result<()> {
	let mut ppaths: Vec<(sdf::Path, u32)> = Vec::with_capacity(paths.len());
	for path in paths {
		if !path.is_empty() {
			let index = file
				.pack_ctx
				.path_index_from_path
				.get(path)
				.map_or(0, |f| f.0);

			ppaths.push((path.clone(), index));
		}
	}

	ppaths.sort_by_key(|(path, _)| path.clone()); // TODO: get rid of this clone

	let mut path_indices = vec![0; ppaths.len()];
	let mut element_token_indices = vec![0; ppaths.len()];
	let mut jumps = vec![0; ppaths.len()];

	let mut iter = ppaths.iter().map(|(p, i)| (p.clone(), *i));

	compress_paths_recursive(
		&mut 0,
		&mut iter,
		&mut path_indices,
		&mut element_token_indices,
		&mut jumps,
		&|token| {
			file.pack_ctx
				.token_index_from_token
				.get(&token)
				.map_or(0, |f| f.0)
		},
	);

	// Write the total number of paths.
	c.write_as::<u64>(paths.len() as u64)?;
	// Write the number of encoded paths.
	// This can differ from the total since we do not write out empty paths.
	c.write_as::<u64>(ppaths.len() as u64)?;

	write_compressed_ints::<u32>(c, &path_indices)?;
	write_compressed_ints::<i32>(c, &element_token_indices)?;
	write_compressed_ints::<i32>(c, &jumps)?;

	Ok(())
}

fn read_paths(
	cursor: &mut Cursor<&[u8]>,
	section: &Section,
	tokens: &[tf::Token],
) -> Result<Vec<sdf::Path>> {
	cursor.set_position(section.start);

	let total_path_count = cursor.read_as::<u64>()? as usize;
	let encoded_path_count = cursor.read_as::<u64>()? as usize;

	let path_indices = read_compressed_ints::<u32>(cursor, encoded_path_count)?;
	let element_token_indices = read_compressed_ints::<i32>(cursor, encoded_path_count)?;
	let jumps = read_compressed_ints::<i32>(cursor, encoded_path_count)?;

	let mut paths = vec![sdf::Path::empty_path(); total_path_count];

	decompress_paths_recursive(
		&path_indices,
		&element_token_indices,
		&jumps,
		0,
		sdf::Path::empty_path(),
		tokens,
		&mut paths,
	);

	Ok(paths)
}

fn write_specs(c: &mut Cursor<Vec<u8>>, specs: &[Spec]) -> Result<()> {
	let mut temp = Vec::with_capacity(specs.len());

	c.write_as::<u64>(specs.len() as u64)?;

	// Path indices.
	temp.extend(specs.iter().map(|s| s.path_index.0));
	write_compressed_ints::<u32>(c, &temp)?;

	// Field set indices.
	temp.clear();
	temp.extend(specs.iter().map(|s| s.field_set_index.0));
	write_compressed_ints::<u32>(c, &temp)?;

	// Spec types.
	temp.clear();
	temp.extend(specs.iter().map(|s| s.spec_type as u32));
	write_compressed_ints::<u32>(c, &temp)?;

	Ok(())
}

fn read_specs(cursor: &mut Cursor<&[u8]>, section: &Section) -> Result<Vec<Spec>> {
	cursor.set_position(section.start);

	let spec_count = cursor.read_as::<u64>()? as usize;

	let path_indices = read_compressed_ints::<u32>(cursor, spec_count)?;
	let field_set_indices = read_compressed_ints::<u32>(cursor, spec_count)?;
	let spec_types = read_compressed_ints::<u32>(cursor, spec_count)?;

	let mut spec = Vec::with_capacity(spec_count);

	for i in 0..spec_count {
		spec.push(Spec {
			path_index: PathIndex(path_indices[i]),
			field_set_index: FieldSetIndex(field_set_indices[i]),
			spec_type: unsafe { std::mem::transmute::<u32, sdf::SpecType>(spec_types[i]) },
		});
	}

	Ok(spec)
}

/// Don't compress arrays smaller than this.
const MIN_COMPRESSED_ARRAY_SIZE: usize = 16;

fn read_int_array<T: Clone + Integer>(
	cursor: &mut Cursor<&[u8]>,
	rep: ValueRep,
) -> Result<vt::Array<T>> {
	let count = cursor.read_as::<u64>()? as usize;

	if !rep.is_compressed() {
		// TODO: Should use read_uncompressed_array here.
		// This can use a memory mapping technique to avoid copying data.
		return Ok(read_contiguous(cursor, count)?.into());
	}

	if count < MIN_COMPRESSED_ARRAY_SIZE {
		return Ok(read_contiguous(cursor, count)?.into());
	}

	Ok(read_compressed_ints(cursor, count)?.into())
}

fn read_float_array<T: Clone + Float>(
	cursor: &mut Cursor<&[u8]>,
	rep: ValueRep,
) -> Result<vt::Array<T>> {
	let count = cursor.read_as::<u64>()? as usize;

	if !rep.is_compressed() {
		// TODO: Should use read_uncompressed_array here.
		// This can use a memory mapping technique to avoid copying data.
		return Ok(read_contiguous(cursor, count)?.into());
	}

	if count < MIN_COMPRESSED_ARRAY_SIZE {
		return Ok(read_contiguous(cursor, count)?.into());
	}

	let code = cursor.read_as::<u8>()? as char;

	Ok(match code {
		'i' => {
			// Compressed integers
			let ints = read_compressed_ints::<i32>(cursor, count)?;
			ints.iter().map(|&i| T::from(i)).collect()
		}
		't' => {
			// Lookup table & indices
			let lut_count = cursor.read_as::<u32>()? as usize;
			let lut = read_contiguous::<T>(cursor, lut_count)?;

			let indices = read_compressed_ints::<u32>(cursor, count)?;
			indices.iter().map(|&i| lut[i as usize].clone()).collect()
		}
		_ => {
			panic!("Corrupt data stream detected reading compressed array"); // TODO: Echo crate asset path
		}
	})
}

fn compress_paths_recursive<I>(
	cur_index: &mut usize,
	paths_with_indices: &mut I,
	path_indices: &mut Vec<u32>,
	element_token_indices: &mut Vec<i32>,
	jumps: &mut Vec<i32>,
	index_from_token: &dyn Fn(&tf::Token) -> Index,
) -> Option<(sdf::Path, Index)>
where
	I: Iterator<Item = (sdf::Path, Index)> + Clone,
{
	let mut next_item = paths_with_indices.next();

	while let Some((ref current_path, current_index)) = next_item {
		let current_clone = current_path.clone();
		let mut lookahead = paths_with_indices.clone();
		let mut next_subtree_item = None;

		while let Some((path, idx)) = lookahead.next() {
			if !path.has_prefix(&current_clone) {
				next_subtree_item = Some((path, idx));
				break;
			}
		}

		let mut iter_clone = paths_with_indices.clone();
		let next_sequential = iter_clone.next();

		let has_child = if let Some((next_path, _)) = &next_sequential {
			if let Some((subtree_path, _)) = &next_subtree_item {
				next_path != subtree_path && next_path.parent_path() == *current_path
			} else {
				next_path.parent_path() == *current_path
			}
		} else {
			false
		};

		let has_sibling = next_subtree_item.is_some_and(|(subtree_path, _)| {
			subtree_path.parent_path() == current_path.parent_path()
		});

		let is_prim_property_path = current_path.is_prim_property_path();

		let element_token = if is_prim_property_path {
			current_path.name_token()
		} else {
			current_path.element_token()
		};

		let this_index = *cur_index;
		*cur_index += 1;

		path_indices[this_index] = current_index;
		element_token_indices[this_index] = if is_prim_property_path {
			-(index_from_token(&element_token) as i32)
		} else {
			index_from_token(&element_token) as i32
		};

		// If there is a child, recurse.
		if has_child {
			next_item = compress_paths_recursive(
				cur_index,
				paths_with_indices,
				path_indices,
				element_token_indices,
				jumps,
				index_from_token,
			);
		}

		// If we have a sibling, then fill in the offset that it will be
		// written at (it will be written next).
		if has_sibling && has_child {
			jumps[this_index] = (*cur_index - this_index) as i32;
		} else if has_sibling {
			jumps[this_index] = 0;
		} else if has_child {
			jumps[this_index] = -1;
		} else {
			jumps[this_index] = -2;
		}

		if !has_sibling {
			return next_item;
		}
	}

	None
}

fn decompress_paths_recursive(
	path_indices: &[u32],
	element_token_indices: &[i32],
	jumps: &[i32],
	mut cur_index: usize, // TODO: Should these be references?
	mut parent_path: sdf::Path,
	tokens: &[tf::Token],
	paths: &mut [sdf::Path],
) {
	loop {
		let this_index = cur_index;
		cur_index += 1;

		if parent_path.is_empty() {
			parent_path = sdf::Path::absolute_root_path();
			paths[path_indices[this_index] as usize] = parent_path.clone();
		} else {
			let token_index = element_token_indices[this_index];
			let is_prim_property_path = token_index < 0;
			let token_index = token_index.abs();
			let elem_token = &tokens[token_index as usize];
			paths[path_indices[this_index] as usize] = if is_prim_property_path {
				parent_path.append_property(elem_token)
			} else {
				// TODO: This should be append_element, the path is not necessarily
				// just a child path but could be of another type eg. target, mapper, ...
				parent_path.append_child(elem_token)
			};
		}

		// If we have either a child or a sibling but not both, then just
		// continue to the neighbor. If we have both then spawn a task for the
		// sibling and do the child ourself. We think that our path trees tend
		// to be broader more often than deep.

		let has_child = jumps[this_index] > 0 || jumps[this_index] == -1;
		let has_sibling = jumps[this_index] >= 0;

		if has_child {
			if has_sibling {
				// Branch off a function for the sibling subtree.
				let sibling_index = this_index + jumps[this_index] as usize;
				decompress_paths_recursive(
					path_indices,
					element_token_indices,
					jumps,
					sibling_index,
					parent_path.clone(),
					tokens,
					paths,
				);
			}

			// Have a child (may have also had a sibling). Reset parent path.
			parent_path = paths[path_indices[this_index] as usize].clone();
		}

		// If we had only a sibling, we just continue since the parent path is
		// unchanged and the next thing in the reader stream is the sibling's
		// header.
		if !has_child && !has_sibling {
			break;
		}
	}
}

fn read_inline_vec2<T: Copy + From<i8>>(rep: ValueRep) -> gf::Vec2<T> {
	let v = (rep.payload() as u32).to_ne_bytes().map(|b| b as i8);
	gf::Vec2::<T>::new(v[0].into(), v[1].into())
}

fn read_inline_vec3<T: Copy + From<i8>>(rep: ValueRep) -> gf::Vec3<T> {
	let v = (rep.payload() as u32).to_ne_bytes().map(|b| b as i8);
	gf::Vec3::<T>::new(v[0].into(), v[1].into(), v[2].into())
}

fn read_inline_vec4<T: Copy + From<i8>>(rep: ValueRep) -> gf::Vec4<T> {
	let v = (rep.payload() as u32).to_ne_bytes().map(|b| b as i8);
	gf::Vec4::<T>::new(v[0].into(), v[1].into(), v[2].into(), v[3].into())
}

fn read_inline_mat2(rep: ValueRep) -> gf::Matrix2d {
	let v = (rep.payload() as u32).to_ne_bytes().map(|b| b as i8);
	let v = gf::Vec2d::new(v[0].into(), v[1].into());
	gf::Matrix2d::from_diagonal(v)
}

fn read_inline_mat3(rep: ValueRep) -> gf::Matrix3d {
	let v = (rep.payload() as u32).to_ne_bytes().map(|b| b as i8);
	let v = gf::Vec3d::new(v[0].into(), v[1].into(), v[2].into());
	gf::Matrix3d::from_diagonal(v)
}

fn read_inline_mat4(rep: ValueRep) -> gf::Matrix4d {
	let v = (rep.payload() as u32).to_ne_bytes().map(|b| b as i8);
	let v = gf::Vec4d::new(v[0].into(), v[1].into(), v[2].into(), v[3].into());
	gf::Matrix4d::from_diagonal(v)
}

fn read_pod<T: Sized + Default>(cursor: &mut Cursor<&[u8]>) -> Result<T> {
	let pod = T::default();
	let byte_slice =
		unsafe { std::slice::from_raw_parts_mut(&pod as *const T as *mut u8, size_of::<T>()) };
	cursor.read_exact(byte_slice)?;
	Ok(pod)
}

fn read_pod_vec<T: Sized + Clone + Default>(cursor: &mut Cursor<&[u8]>) -> Result<vt::Array<T>> {
	let n = cursor.read_as::<u64>()? as usize;

	let mut vec: Vec<T> = vec![Default::default(); n];
	let byte_slice =
		unsafe { std::slice::from_raw_parts_mut(vec.as_mut_ptr() as *mut u8, n * size_of::<T>()) };
	cursor.read_exact(byte_slice)?;
	Ok(vec.into())
}

fn read_inline(file: &UsdcFile, val: ValueRep) -> Option<vt::Value> {
	if val.is_array() || val.is_compressed() || !val.is_inlined() {
		return None;
	}

	Some(match val.ty() {
		Type::Token => {
			let token_index = TokenIndex(val.payload() as Index);
			file.get_token(token_index).clone().into()
		}

		Type::Half => f16::from_bits(val.payload() as u16).into(),
		Type::Float => f32::from_bits(val.payload() as u32).into(),
		Type::Double => f64::from_bits(val.payload() as u64).into(),

		Type::Vec2i => read_inline_vec2::<i32>(val).into(),
		Type::Vec2h => read_inline_vec2::<f16>(val).into(),
		Type::Vec2f => read_inline_vec2::<f32>(val).into(),
		Type::Vec2d => read_inline_vec2::<f64>(val).into(),

		Type::Vec3i => read_inline_vec3::<i32>(val).into(),
		Type::Vec3h => read_inline_vec3::<f16>(val).into(),
		Type::Vec3f => read_inline_vec3::<f32>(val).into(),
		Type::Vec3d => read_inline_vec3::<f64>(val).into(),

		Type::Vec4i => read_inline_vec4::<i32>(val).into(),
		Type::Vec4h => read_inline_vec4::<f16>(val).into(),
		Type::Vec4f => read_inline_vec4::<f32>(val).into(),
		Type::Vec4d => read_inline_vec4::<f64>(val).into(),

		Type::Matrix2d => read_inline_mat2(val).into(),
		Type::Matrix3d => read_inline_mat3(val).into(),
		Type::Matrix4d => read_inline_mat4(val).into(),

		_ => return None,
	})
}

fn unpack_value_rep(file: &UsdcFile, value: ValueRep) -> Result<Option<vt::Value>> {
	let buffer = &file.buffer;

	if let Some(v) = read_inline(file, value) {
		return Ok(Some(v));
	}

	let mut cursor = Cursor::new(buffer.as_slice());
	cursor.set_position(value.payload());

	Ok(Some(match value.ty() {
		Type::Half if value.is_array() => read_float_array::<f16>(&mut cursor, value)?.into(),
		Type::Float if value.is_array() => read_float_array::<f32>(&mut cursor, value)?.into(),
		Type::Double if value.is_array() => read_float_array::<f64>(&mut cursor, value)?.into(),

		// TODO: This is always be an inlined value
		Type::AssetPath => {
			let token = file.get_token(TokenIndex(value.payload() as Index));
			vt::Value::new(sdf::AssetPath {
				authored_path: token.as_str().into(),
				evaluated_path: String::new(),
				resolved_path: "".into(),
			})
		}

		Type::Int if value.is_array() => read_int_array::<i32>(&mut cursor, value)?.into(),
		Type::UInt if value.is_array() => read_int_array::<u32>(&mut cursor, value)?.into(),

		Type::Vec3f if value.is_array() => read_pod_vec::<gf::Vec3f>(&mut cursor)?.into(),

		Type::Quath => read_pod::<gf::Quath>(&mut cursor)?.into(),
		Type::Quatf => read_pod::<gf::Quatf>(&mut cursor)?.into(),
		Type::Quatd => read_pod::<gf::Quatd>(&mut cursor)?.into(),

		Type::Vec2h => read_pod::<gf::Vec2h>(&mut cursor)?.into(),
		Type::Vec2f => read_pod::<gf::Vec2f>(&mut cursor)?.into(),
		Type::Vec2d => read_pod::<gf::Vec2d>(&mut cursor)?.into(),
		Type::Vec2i => read_pod::<gf::Vec2i>(&mut cursor)?.into(),

		Type::Vec3h => read_pod::<gf::Vec3h>(&mut cursor)?.into(),
		Type::Vec3f => read_pod::<gf::Vec3f>(&mut cursor)?.into(),
		Type::Vec3d => read_pod::<gf::Vec3d>(&mut cursor)?.into(),
		Type::Vec3i => read_pod::<gf::Vec3i>(&mut cursor)?.into(),

		Type::Vec4h => read_pod::<gf::Vec4h>(&mut cursor)?.into(),
		Type::Vec4f => read_pod::<gf::Vec4f>(&mut cursor)?.into(),
		Type::Vec4d => read_pod::<gf::Vec4d>(&mut cursor)?.into(),
		Type::Vec4i => read_pod::<gf::Vec4i>(&mut cursor)?.into(),

		Type::Matrix2d => read_pod::<gf::Matrix2d>(&mut cursor)?.into(),
		Type::Matrix3d => read_pod::<gf::Matrix3d>(&mut cursor)?.into(),
		Type::Matrix4d => read_pod::<gf::Matrix4d>(&mut cursor)?.into(),

		Type::PathVector => {
			let indices = Vec::<Index>::read(file, &mut cursor)?;
			let vector = indices
				.iter()
				.map(|i| file.get_path(PathIndex(*i)).clone())
				.collect::<vt::Array<_>>();
			vt::Value::new(vector)
		}

		Type::TokenVector => {
			let indices = Vec::<Index>::read(file, &mut cursor)?;
			let vector = indices
				.iter()
				.map(|i| file.get_token(TokenIndex(*i)).clone())
				.collect::<vt::Array<_>>();
			vt::Value::new(vector)
		}

		Type::Token if value.is_array() => {
			let indices = Vec::<Index>::read(file, &mut cursor)?;
			let vector = indices
				.iter()
				.map(|i| file.get_token(TokenIndex(*i)).clone())
				.collect::<vt::Array<_>>();
			vt::Value::new(vector)
		}

		Type::IntListOp => sdf::IntListOp::read(file, &mut cursor)?.into(),
		Type::UIntListOp => sdf::UIntListOp::read(file, &mut cursor)?.into(),
		Type::Int64ListOp => sdf::Int64ListOp::read(file, &mut cursor)?.into(),
		Type::UInt64ListOp => sdf::UInt64ListOp::read(file, &mut cursor)?.into(),

		Type::TokenListOp => sdf::TokenListOp::read(file, &mut cursor)?.into(),
		Type::StringListOp => sdf::StringListOp::read(file, &mut cursor)?.into(),
		Type::PathListOp => sdf::PathListOp::read(file, &mut cursor)?.into(),
		Type::ReferenceListOp => sdf::ReferenceListOp::read(file, &mut cursor)?.into(),
		Type::PayloadListOp => sdf::PayloadListOp::read(file, &mut cursor)?.into(),

		Type::Dictionary => vt::Dictionary::read(file, &mut cursor)?.into(),

		_ => return Ok(None),
	}))
}

impl sdf::AbstractData for UsdcFile {
	fn get(&self, path: &sdf::Path, field: &tf::Token) -> Option<vt::Value> {
		let path_index = PathIndex(self.paths.iter().position(|p| *p == *path)? as u32);

		let spec = self.specs.iter().find(|s| s.path_index == path_index)?;

		let field = fields(self, spec).find(|f| self.get_token(f.token_index) == field)?;

		unpack_value_rep(self, field.value_rep).unwrap()
	}

	fn spec_type(&self, path: &sdf::Path) -> Option<sdf::SpecType> {
		let path_index = PathIndex(self.paths.iter().position(|p| *p == *path)? as u32);

		self.specs
			.iter()
			.find(|s| s.path_index == path_index)
			.map(|spec| spec.spec_type)
	}

	fn list(&self, path: &sdf::Path) -> Vec<&tf::Token> {
		let path_index = match self.paths.iter().position(|p| *p == *path) {
			Some(index) => PathIndex(index as u32),
			None => return Vec::new(),
		};

		let spec = match self.specs.iter().find(|s| s.path_index == path_index) {
			Some(spec) => spec,
			None => return Vec::new(),
		};

		fields(self, spec)
			.map(|field| self.get_token(field.token_index))
			.collect()
	}

	fn visit_specs(&self) -> Vec<&sdf::Path> {
		self.specs
			.iter()
			.map(|spec| self.get_path(spec.path_index))
			.collect()
	}
}

pub struct UsdcFile {
	version: Version,
	buffer: Vec<u8>,

	specs: Vec<Spec>,
	fields: Vec<Field>,
	field_sets: Vec<FieldIndex>,

	paths: Vec<sdf::Path>,
	tokens: Vec<tf::Token>,
	strings: Vec<TokenIndex>,

	toc: TableOfContents,

	pack_ctx: PackCtx,
}

#[derive(Default)]
struct PackCtx {
	// Deduplication tables.
	token_index_from_token: HashMap<tf::Token, TokenIndex>,
	string_index_from_string: HashMap<String, StringIndex>,
	path_index_from_path: HashMap<sdf::Path, PathIndex>,
	field_index_from_field: HashMap<Field, FieldIndex>,

	// Mapping from a group of fields to their starting index in field_sets.
	field_set_index_from_fields: HashMap<Vec<FieldIndex>, FieldSetIndex>,

	// Unknown sections we're moving to the new structural area.
	unknown_sections: Vec<(String, Vec<u8>)>,
}

impl PackCtx {
	fn new(usdc: &UsdcFile) -> Self {
		let token_index_from_token = usdc
			.tokens
			.iter()
			.enumerate()
			.map(|(i, token)| (token.clone(), TokenIndex(i as Index)))
			.collect::<HashMap<_, _>>();

		let string_index_from_string = usdc
			.strings
			.iter()
			.enumerate()
			.map(|(i, _)| {
				let index = StringIndex(i as Index);
				(usdc.get_string(index).to_string(), index)
			})
			.collect::<HashMap<_, _>>();

		let path_index_from_path = usdc
			.paths
			.iter()
			.enumerate()
			.map(|(i, path)| (path.clone(), PathIndex(i as Index)))
			.collect::<HashMap<_, _>>();

		let field_index_from_field = usdc
			.fields
			.iter()
			.enumerate()
			.map(|(i, field)| (field.clone(), FieldIndex(i as Index)))
			.collect::<HashMap<_, _>>();

		// TODO: Unsure if this is correct
		let base = usdc.field_sets.as_ptr();
		let field_set_index_from_fields = usdc
			.field_sets
			.split(|&f| f.0 == 0)
			.map(|s| {
				// TODO: Remove unsafe code
				let start = unsafe { s.as_ptr().offset_from(base) as usize }; // total index into the original slice
				(s.to_vec(), FieldSetIndex(start as Index))
			})
			.collect::<HashMap<_, _>>();

		// Read in any unknown sections so we can rewrite them later.
		let unknown_sections = usdc
			.toc
			.sections
			.iter()
			.filter(|s| !s.is_known())
			.map(|s| {
				let (start, end) = (s.start as usize, s.start as usize + s.size as usize);
				(s.name().unwrap().into(), usdc.buffer[start..end].into())
			})
			.collect::<Vec<_>>();

		// Set file pos to start of the structural sections in the current TOC.
		// cursor.seek(usdc.toc.minimum_section_start())

		Self {
			token_index_from_token,
			string_index_from_string,
			path_index_from_path,
			field_index_from_field,

			field_set_index_from_fields,

			unknown_sections,
		}
	}
}

trait CrateIo {
	fn read(file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self>
	where
		Self: Sized;

	fn write(&self, file: &mut UsdcFile, cursor: &mut Cursor<&mut [u8]>) -> Result<()> {
		unimplemented!()
	}
}

fn read_contiguous<T: Clone>(cursor: &mut Cursor<&[u8]>, count: usize) -> Result<Vec<T>> {
	let mut vec = vec![unsafe { std::mem::zeroed::<T>() }; count];
	let slice = unsafe {
		std::slice::from_raw_parts_mut(vec.as_mut_ptr() as *mut u8, vec.len() * size_of::<T>())
	};
	cursor.read_exact(slice)?;
	Ok(vec)
}

impl<T: Clone> CrateIo for Vec<T> {
	fn read(file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
		let count = cursor.read_as::<u64>()? as usize;
		read_contiguous(cursor, count)
	}
}

fn read_typed_vec<T: CrateIo>(file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Vec<T>> {
	let count = cursor.read_as::<u64>()? as usize;
	(0..count).map(|_| T::read(file, cursor)).collect()
}

fn write_typed_vec<T: CrateIo>(
	file: &mut UsdcFile,
	cursor: &mut Cursor<&mut [u8]>,
	values: &[T],
) -> Result<()> {
	cursor.write_as(values.len() as u64)?;

	for value in values {
		value.write(file, cursor)?;
	}

	Ok(())
}

impl<T: Copy + crate::io_ext::Readable + crate::io_ext::Writeable> CrateIo for T {
	fn read(file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
		Ok(cursor.read_as::<T>()?)
	}

	fn write(&self, file: &mut UsdcFile, cursor: &mut Cursor<&mut [u8]>) -> Result<()> {
		cursor.write_as(*self)
	}
}

impl<T: CrateIo + Default> CrateIo for sdf::ListOp<T> {
	fn read(file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
		let h = ListOpHeader(cursor.read_as::<u8>()?);

		let mut list_op = sdf::ListOp::default();

		if h.is_explicit() {
			list_op.is_explicit = true;
		}
		if h.has_explicit_items() {
			list_op.explicit_items = read_typed_vec(file, cursor)?;
		}
		if h.has_added_items() {
			list_op.added_items = read_typed_vec(file, cursor)?;
		}
		if h.has_prepended_items() {
			list_op.prepended_items = read_typed_vec(file, cursor)?;
		}
		if h.has_appended_items() {
			list_op.appended_items = read_typed_vec(file, cursor)?;
		}
		if h.has_deleted_items() {
			list_op.deleted_items = read_typed_vec(file, cursor)?;
		}
		if h.has_ordered_items() {
			list_op.ordered_items = read_typed_vec(file, cursor)?;
		}

		Ok(list_op)
	}

	fn write(&self, file: &mut UsdcFile, cursor: &mut Cursor<&mut [u8]>) -> Result<()> {
		let h = ListOpHeader::from(self);

		cursor.write_as::<u8>(h.0)?;

		if h.has_explicit_items() {
			write_typed_vec(file, cursor, &self.explicit_items)?;
		}
		if h.has_added_items() {
			write_typed_vec(file, cursor, &self.added_items)?;
		}
		if h.has_prepended_items() {
			write_typed_vec(file, cursor, &self.prepended_items)?;
		}
		if h.has_appended_items() {
			write_typed_vec(file, cursor, &self.appended_items)?;
		}
		if h.has_deleted_items() {
			write_typed_vec(file, cursor, &self.deleted_items)?;
		}
		if h.has_ordered_items() {
			write_typed_vec(file, cursor, &self.ordered_items)?;
		}

		Ok(())
	}
}

impl UsdcFile {
	pub fn open(asset_path: &std::path::Path) -> Result<Self> {
		let buffer = std::fs::read(asset_path)?;
		let mut cursor = Cursor::new(buffer.as_slice());

		let mut magic = [0; 8];
		cursor.read_exact(&mut magic)?;

		assert_eq!(&magic, b"PXR-USDC");

		let mut version = [0; 8];
		cursor.read_exact(&mut version)?;

		let version = Version::new(version[0], version[1], version[2]);

		// Version 0.8.0 will be the minimum supported version somewhere in the future:
		// https://github.com/PixarAnimationStudios/OpenUSD/commit/09b8c2c
		assert!(version >= Version::new(0, 8, 0) && version <= Version::new(0, 12, 0));

		let toc = read_toc(&mut cursor)?;

		let tokens = read_tokens(&mut cursor, toc.section(Section::TOKENS).unwrap())?;
		let strings = read_strings(&mut cursor, toc.section(Section::STRINGS).unwrap())?;
		let fields = read_fields(&mut cursor, toc.section(Section::FIELDS).unwrap())?;
		let field_sets = read_field_sets(&mut cursor, toc.section(Section::FIELDSETS).unwrap())?;
		let paths = read_paths(&mut cursor, toc.section(Section::PATHS).unwrap(), &tokens)?;
		let specs = read_specs(&mut cursor, toc.section(Section::SPECS).unwrap())?;

		Ok(UsdcFile {
			version,
			buffer,
			tokens,
			strings,
			fields,
			field_sets,
			paths,
			specs,
			toc,

			pack_ctx: PackCtx::default(),
		})
	}

	pub fn save(&mut self, asset_path: &std::path::Path) -> Result<()> {
		let mut sorted_paths = self.paths.clone();

		// Sort by path for better namespace-grouped data layout.
		// Prim paths before property paths, then property paths grouped by property name.
		sorted_paths.sort_by(|a, b| {
			let a_is_prop = a.is_prim_property_path();
			let b_is_prop = b.is_prim_property_path();

			if a_is_prop != b_is_prop {
				return if a_is_prop {
					std::cmp::Ordering::Greater
				} else {
					std::cmp::Ordering::Less
				};
			}

			if a_is_prop && b_is_prop {
				let an = a.name_token();
				let bn = b.name_token();

				if an != bn {
					return an.cmp(&bn);
				}
			}

			a.cmp(b)
		});

		// TODO: Implement the rest of the save function
		Ok(())
	}

	fn write_sections(&mut self) -> Result<()> {
		// TODO: Implement the write function

		let mut cursor = Cursor::new(Vec::<u8>::new());

		let mut toc = TableOfContents::default();

		// TODO: Write out the sections we don't know about that the packing context captured.

		let start = cursor.position() as u64;
		write_tokens(&mut cursor, &self.tokens)?;
		toc.add_section(Section::TOKENS, start, cursor.position() as u64 - start);

		let start = cursor.position() as u64;
		write_strings(&mut cursor, &self.strings)?;
		toc.add_section(Section::STRINGS, start, cursor.position() as u64 - start);

		let start = cursor.position() as u64;
		write_fields(&mut cursor, &self.fields)?;
		toc.add_section(Section::FIELDS, start, cursor.position() as u64 - start);

		let start = cursor.position() as u64;
		write_field_sets(&mut cursor, &self.field_sets)?;
		toc.add_section(Section::FIELDSETS, start, cursor.position() as u64 - start);

		let start = cursor.position() as u64;
		write_paths(&mut cursor, &self.paths, &self)?;
		toc.add_section(Section::PATHS, start, cursor.position() as u64 - start);

		let start = cursor.position() as u64;
		write_specs(&mut cursor, &self.specs)?;
		toc.add_section(Section::SPECS, start, cursor.position() as u64 - start);

		let toc_offset = cursor.position() as u64;
		// TODO: Write toc

		cursor.seek(SeekFrom::Start(0))?;
		// TODO: Write bootstrap

		self.toc = toc;
		//self.boot = boot;

		// TODO: Clear value handler dedup tables

		Ok(())
	}
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct TokenIndex(Index);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct StringIndex(Index);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct PathIndex(Index);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldIndex(Index);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldSetIndex(Index);

pub type FieldValuePair = (tf::Token, vt::Value);

impl UsdcFile {
	fn get_string(&self, index: StringIndex) -> &str {
		self.get_token(self.strings[index.0 as usize]).as_str()
	}

	fn get_token(&self, index: TokenIndex) -> &tf::Token {
		&self.tokens[index.0 as usize]
	}

	fn get_path(&self, index: PathIndex) -> &sdf::Path {
		&self.paths[index.0 as usize]
	}

	fn get_field(&self, index: FieldIndex) -> &Field {
		&self.fields[index.0 as usize]
	}

	fn add_spec(
		&mut self,
		path: &sdf::Path,
		spec_type: sdf::SpecType,
		fields: Vec<FieldValuePair>,
	) {
		// TODO: Support TimeSamples
		// TODO: Support ts::Spline

		let ordinary_fields: Vec<FieldIndex> = fields.iter().map(|fv| self.add_field(fv)).collect();

		let path_index = self.add_path(path);
		let field_set_index = self.add_field_set(&ordinary_fields);

		self.specs.push(Spec {
			path_index,
			spec_type,
			field_set_index,
		});
	}

	fn add_field(&mut self, fv: &FieldValuePair) -> FieldIndex {
		let field = Field {
			token_index: self.add_token(&fv.0),
			value_rep: self.pack_value(&fv.1),
		};

		*self
			.pack_ctx
			.field_index_from_field
			.entry(field.clone())
			.or_insert_with(|| {
				let index = FieldIndex(self.fields.len() as u32);
				self.fields.push(field);
				index
			})
	}

	fn add_field_set(&mut self, field_indices: &Vec<FieldIndex>) -> FieldSetIndex {
		*self
			.pack_ctx
			.field_set_index_from_fields
			.entry(field_indices.clone())
			.or_insert_with(|| {
				let index = FieldSetIndex(self.field_sets.len() as u32);
				self.field_sets.extend_from_slice(&field_indices);
				self.field_sets.push(FieldIndex(0)); // Separator
				index
			})
	}

	fn add_token(&mut self, token: &tf::Token) -> TokenIndex {
		*self
			.pack_ctx
			.token_index_from_token
			.entry(token.clone())
			.or_insert_with(|| {
				let index = TokenIndex(self.tokens.len() as u32);
				self.tokens.push(token.clone());
				index
			})
	}

	fn add_string(&mut self, string: &str) -> StringIndex {
		if let Some(index) = self.pack_ctx.string_index_from_string.get(string) {
			return *index;
		}

		let token = self.add_token(&tf::Token::new(string));

		let index = StringIndex(self.strings.len() as u32);

		self.pack_ctx
			.string_index_from_string
			.insert(string.to_string(), index);

		self.strings.push(token);

		index
	}

	fn add_path(&mut self, path: &sdf::Path) -> PathIndex {
		if let Some(index) = self.pack_ctx.path_index_from_path.get(path) {
			return *index;
		}

		if path.is_target_path() {
			// TODO: self.add_path(path.target_path());
			todo!("Target paths are not supported yet");
		}

		if !path.is_absolute_root() {
			self.add_path(&path.parent_path());
		}

		// We treat prim property paths separately since there are so many,
		// the name with the dot would double the number of tokens we store.
		self.add_token(&if path.is_prim_property_path() {
			path.name_token()
		} else {
			path.element_token()
		});

		let index = PathIndex(self.tokens.len() as u32);

		self.pack_ctx
			.path_index_from_path
			.insert(path.clone(), index);

		self.paths.push(path.clone());

		index
	}

	fn pack_value(&mut self, value: &vt::Value) -> ValueRep {
		todo!()
	}

	fn read<T: CrateIo>(&self, cursor: &mut Cursor<&[u8]>) -> Result<T> {
		T::read(self, cursor)
	}

	fn write<T: CrateIo>(&mut self, value: &T, cursor: &mut Cursor<&mut [u8]>) -> Result<()> {
		value.write(self, cursor)
	}
}

impl CrateIo for String {
	fn read(file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
		let index = StringIndex(cursor.read_as()?);
		Ok(file.get_string(index).to_string())
	}

	fn write(&self, file: &mut UsdcFile, cursor: &mut Cursor<&mut [u8]>) -> Result<()> {
		let index = file.add_string(self);
		cursor.write_as(index.0)
	}
}

impl CrateIo for tf::Token {
	fn read(file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
		let index = TokenIndex(cursor.read_as()?);
		Ok(file.get_token(index).clone())
	}

	fn write(&self, file: &mut UsdcFile, cursor: &mut Cursor<&mut [u8]>) -> Result<()> {
		let index = file.add_token(self);
		cursor.write_as(index.0)
	}
}

impl CrateIo for sdf::Path {
	fn read(file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
		let index = PathIndex(cursor.read_as()?);
		Ok(file.get_path(index).clone())
	}

	fn write(&self, file: &mut UsdcFile, cursor: &mut Cursor<&mut [u8]>) -> Result<()> {
		let index = file.add_path(self);
		cursor.write_as(index.0)
	}
}

thread_local! {
	static LOCAL_UNPACK_RECURSION_GUARD: RefCell<HashSet<u64>> = RefCell::new(HashSet::new());
}

impl CrateIo for vt::Value {
	fn read(file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
		let offset = cursor.read_as::<i64>()?;
		cursor.seek_relative(offset - 8)?;

		let value_rep = ValueRep(cursor.read_as::<u64>()?);

		/*let mut recursion_guard = LOCAL_UNPACK_RECURSION_GUARD.with(|guard| guard.borrow_mut());

		if recursion_guard.insert(value_rep.0) {
			// TODO: Read value
		} else {
			panic!("Recursion detected while unpacking value");
		}

		recursion_guard.remove(&value_rep.0);*/

		Ok(vt::Value::new(0.0))
	}
}

impl CrateIo for vt::Dictionary {
	fn read(file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
		let size = cursor.read_as::<u64>()? as usize;

		Ok((0..size)
			.map(|_| {
				(
					file.read::<String>(cursor).unwrap(),
					file.read::<vt::Value>(cursor).unwrap(),
				)
			})
			.collect())
	}
}

impl CrateIo for sdf::LayerOffset {
	fn read(file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
		Ok(Self {
			offset: cursor.read_as::<f64>()?,
			scale: cursor.read_as::<f64>()?,
		})
	}

	fn write(&self, file: &mut UsdcFile, cursor: &mut Cursor<&mut [u8]>) -> Result<()> {
		cursor.write_as::<f64>(self.offset)?;
		cursor.write_as::<f64>(self.scale)?;
		Ok(())
	}
}

impl CrateIo for sdf::Reference {
	fn read(file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
		Ok(Self {
			asset_path: file.read::<String>(cursor)?,
			prim_path: file.read::<sdf::Path>(cursor)?,
			layer_offset: file.read::<sdf::LayerOffset>(cursor)?,
			custom_data: file.read::<vt::Dictionary>(cursor)?,
		})
	}

	fn write(&self, file: &mut UsdcFile, cursor: &mut Cursor<&mut [u8]>) -> Result<()> {
		file.write::<String>(&self.asset_path, cursor)?;
		file.write::<sdf::Path>(&self.prim_path, cursor)?;
		file.write::<sdf::LayerOffset>(&self.layer_offset, cursor)?;
		file.write::<vt::Dictionary>(&self.custom_data, cursor)?;
		Ok(())
	}
}

impl CrateIo for sdf::Payload {
	fn read(file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
		Ok(Self {
			asset_path: file.read::<String>(cursor)?,
			prim_path: file.read::<sdf::Path>(cursor)?,
			layer_offset: file.read::<sdf::LayerOffset>(cursor)?,
		})
	}

	fn write(&self, file: &mut UsdcFile, cursor: &mut Cursor<&mut [u8]>) -> Result<()> {
		file.write::<String>(&self.asset_path, cursor)?;
		file.write::<sdf::Path>(&self.prim_path, cursor)?;
		file.write::<sdf::LayerOffset>(&self.layer_offset, cursor)?;
		Ok(())
	}
}

impl CrateIo for sdf::Relocate {
	fn read(file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
		Ok(Self {
			source: file.read::<sdf::Path>(cursor)?,
			target: file.read::<sdf::Path>(cursor)?,
		})
	}

	fn write(&self, file: &mut UsdcFile, cursor: &mut Cursor<&mut [u8]>) -> Result<()> {
		file.write::<sdf::Path>(&self.source, cursor)?;
		file.write::<sdf::Path>(&self.target, cursor)?;
		Ok(())
	}
}

fn fields<'a>(usdc: &'a UsdcFile, spec: &Spec) -> impl Iterator<Item = &'a Field> + 'a {
	usdc.field_sets
		.iter()
		.skip(spec.field_set_index.0 as usize)
		.take_while(|&&x| x != FieldIndex(Index::MAX))
		.map(|&x| usdc.get_field(x))
}

trait Float {
	fn from(value: i32) -> Self;
}

impl Float for f16 {
	fn from(value: i32) -> Self {
		f16::from_f32(value as f32)
	}
}

impl Float for f32 {
	fn from(value: i32) -> Self {
		value as f32
	}
}

impl Float for f64 {
	fn from(value: i32) -> Self {
		value as f64
	}
}
