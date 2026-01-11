#![allow(dead_code)] // TODO: Remove once writing is implemented.

use super::{compression, integer_coding::*};
use crate::io_ext::{ReadBytesExt, WriteBytesExt};
use crate::{gf, sdf, tf, vt};

use std::collections::HashMap;
use std::{
	cell::RefCell,
	collections::HashSet,
	io::{Cursor, Read, Seek},
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

	fn name(&self) -> Option<&str> {
		let len = self.name.iter().position(|&x| x == 0)?;
		str::from_utf8(&self.name[0..len]).ok()
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
	spec_form: sdf::SpecForm,
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
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
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
	const MAKE_EXPLICIT_BIT: u8 = 1 << 0;
	const EXPLICIT_ITEMS_BIT: u8 = 1 << 1;
	const ADD_ITEMS_BIT: u8 = 1 << 2;
	const DELETE_ITEMS_BIT: u8 = 1 << 3;
	const REORDER_ITEMS_BIT: u8 = 1 << 4;
	const PREPEND_ITEMS_BIT: u8 = 1 << 5;
	const APPEND_ITEMS_BIT: u8 = 1 << 6;

	pub fn is_make_explicit(&self) -> bool {
		self.0 & Self::MAKE_EXPLICIT_BIT != 0
	}

	pub fn has_explicit_items(&self) -> bool {
		self.0 & Self::EXPLICIT_ITEMS_BIT != 0
	}

	pub fn has_add_items(&self) -> bool {
		self.0 & Self::ADD_ITEMS_BIT != 0
	}

	pub fn has_delete_items(&self) -> bool {
		self.0 & Self::DELETE_ITEMS_BIT != 0
	}

	pub fn has_reorder_items(&self) -> bool {
		self.0 & Self::REORDER_ITEMS_BIT != 0
	}

	pub fn has_prepend_items(&self) -> bool {
		self.0 & Self::PREPEND_ITEMS_BIT != 0
	}

	pub fn has_append_items(&self) -> bool {
		self.0 & Self::APPEND_ITEMS_BIT != 0
	}
}

impl<T: std::hash::Hash + Eq + Clone + std::fmt::Debug> From<&sdf::ListOp<T>> for ListOpHeader {
	fn from(op: &sdf::ListOp<T>) -> Self {
		let mut header = ListOpHeader(0);

		if op.is_explicit() {
			header.0 |= ListOpHeader::MAKE_EXPLICIT_BIT;

			if !op.explicit_items().unwrap().is_empty() {
				header.0 |= ListOpHeader::EXPLICIT_ITEMS_BIT;
			}
		}

		if !op.prepended_items().is_empty() {
			header.0 |= ListOpHeader::PREPEND_ITEMS_BIT;
		}

		if !op.appended_items().is_empty() {
			header.0 |= ListOpHeader::APPEND_ITEMS_BIT;
		}

		if !op.deleted_items().is_empty() {
			header.0 |= ListOpHeader::DELETE_ITEMS_BIT;
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
		.map(tf::Token::new)
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

fn read_strings(cursor: &mut Cursor<&[u8]>, section: &Section) -> Result<Vec<TokenIndex>> {
	cursor.set_position(section.start);

	let string_count = cursor.read_as::<u64>()? as usize;
	let mut indices = Vec::with_capacity(string_count);

	// TODO: Replace this with a single read_exact call
	for _ in 0..string_count {
		indices.push(TokenIndex(cursor.read_as::<Index>()?));
	}

	Ok(indices)
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

fn read_field_sets(cursor: &mut Cursor<&[u8]>, section: &Section) -> Result<Vec<FieldIndex>> {
	cursor.set_position(section.start);

	let field_set_count = cursor.read_as::<u64>()? as usize;
	let temp = read_compressed_ints::<Index>(cursor, field_set_count)?;
	// TODO: Prevent this extra allocation, read compressed directly as FieldIndex
	Ok(temp.into_iter().map(FieldIndex).collect())
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
			spec_form: unsafe { std::mem::transmute::<u32, sdf::SpecForm>(spec_types[i]) },
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
				parent_path.append_element(elem_token)
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

		Type::String => {
			let token_index = StringIndex(val.payload() as Index);
			file.get_string(token_index).to_string().into()
		}

		Type::Specifier => {
			unsafe { std::mem::transmute::<u32, sdf::Specifier>(val.payload() as u32) }.into()
		}

		Type::Variability => {
			unsafe { std::mem::transmute::<u32, sdf::Variability>(val.payload() as u32) }.into()
		}

		Type::ValueBlock => sdf::ValueBlock.into(),

		Type::Bool => (val.payload() != 0).into(),

		Type::UChar => (val.payload() as u8).into(),
		Type::Int => (val.payload() as i32).into(),
		Type::UInt => (val.payload() as u32).into(),
		Type::Int64 => (val.payload() as i64).into(),
		Type::UInt64 => val.payload().into(),

		Type::Half => f16::from_bits(val.payload() as u16).into(),
		Type::Float => f32::from_bits(val.payload() as u32).into(),
		Type::Double => f64::from_bits(val.payload()).into(),

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
		Type::DoubleVector => read_pod_vec::<f64>(&mut cursor)?.into(),

		Type::String => file.read::<String>(&mut cursor)?.into(),

		// TODO: This is always an inlined value
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

		Type::Quath if value.is_array() => read_pod_vec::<gf::Quath>(&mut cursor)?.into(),

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

		Type::TimeSamples => {
			assert!(!value.is_inlined() && !value.is_compressed());

			let offset = cursor.read_as::<i64>()?;
			cursor.seek(std::io::SeekFrom::Current(offset - 8))?;

			let times_rep = read_pod::<ValueRep>(&mut cursor)?;
			assert!(
				times_rep.ty() == Type::DoubleVector
					|| (times_rep.ty() == Type::Double && times_rep.is_array())
			);

			let saved_position = cursor.stream_position()?;

			let times_value = unpack_value_rep(file, times_rep)?.unwrap();

			let times = times_value.get::<vt::Array<f64>>().unwrap();

			cursor.set_position(saved_position);

			let offset = cursor.read_as::<i64>()?;
			cursor.seek(std::io::SeekFrom::Current(offset - 8))?;

			let count = cursor.read_as::<u64>()? as usize;
			assert_eq!(count, times.len());

			let mut value_reps = Vec::with_capacity(count);
			for _ in 0..count {
				let val = cursor.read_as::<u64>()?;
				value_reps.push(ValueRep(val));
			}

			let values = value_reps
				.into_iter()
				.map(|value| unpack_value_rep(file, value).unwrap().unwrap())
				.collect::<Vec<_>>();

			times
				.iter()
				.copied()
				.zip(values)
				.collect::<sdf::TimeSampleMap>()
				.into()
		}

		Type::PathVector => file
			.read::<Vec<Index>>(&mut cursor)?
			.iter()
			.map(|i| file.get_path(PathIndex(*i)).clone())
			.collect::<vt::Array<_>>()
			.into(),

		Type::TokenVector => file
			.read::<Vec<Index>>(&mut cursor)?
			.iter()
			.map(|i| file.get_token(TokenIndex(*i)).clone())
			.collect::<vt::Array<_>>()
			.into(),

		Type::StringVector => file
			.read::<Vec<Index>>(&mut cursor)?
			.iter()
			.map(|i| file.get_string(StringIndex(*i)).to_string())
			.collect::<vt::Array<_>>()
			.into(),

		Type::Token if value.is_array() => file
			.read::<Vec<Index>>(&mut cursor)?
			.iter()
			.map(|i| file.get_token(TokenIndex(*i)).clone())
			.collect::<vt::Array<_>>()
			.into(),

		Type::IntListOp => file.read::<sdf::IntListOp>(&mut cursor)?.into(),
		Type::UIntListOp => file.read::<sdf::UIntListOp>(&mut cursor)?.into(),
		Type::Int64ListOp => file.read::<sdf::Int64ListOp>(&mut cursor)?.into(),
		Type::UInt64ListOp => file.read::<sdf::UInt64ListOp>(&mut cursor)?.into(),

		Type::TokenListOp => file.read::<sdf::TokenListOp>(&mut cursor)?.into(),
		Type::StringListOp => file.read::<sdf::StringListOp>(&mut cursor)?.into(),
		Type::PathListOp => file.read::<sdf::PathListOp>(&mut cursor)?.into(),
		Type::ReferenceListOp => file.read::<sdf::ReferenceListOp>(&mut cursor)?.into(),
		Type::PayloadListOp => file.read::<sdf::PayloadListOp>(&mut cursor)?.into(),

		Type::Payload => {
			// TODO: Unsure if this is correct. However the payloads field is a ListOp according to the core spec.
			let payload = file.read::<sdf::Payload>(&mut cursor)?;
			sdf::PayloadListOp::from_explicit(vec![payload]).into()
		}

		Type::LayerOffsetVector => {
			vt::Array::from(read_typed_vec::<sdf::Retiming>(file, &mut cursor)?).into()
		}
		Type::Relocates => {
			vt::Array::from(read_typed_vec::<sdf::Relocate>(file, &mut cursor)?).into()
		}

		Type::Dictionary => file.read::<vt::Dictionary>(&mut cursor)?.into(),
		Type::VariantSelectionMap => file.read::<sdf::VariantSelectionMap>(&mut cursor)?.into(),

		_ => {
			println!("Unimplemented value type {:?}", value.ty());
			return Ok(None);
		}
	}))
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
}

trait CrateIo {
	fn read(file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self>
	where
		Self: Sized;

	fn write(&self, _file: &mut UsdcFile, _cursor: &mut Cursor<&mut [u8]>) -> Result<()> {
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
	fn read(_file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
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
	fn read(_file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
		cursor.read_as::<T>()
	}

	fn write(&self, _file: &mut UsdcFile, cursor: &mut Cursor<&mut [u8]>) -> Result<()> {
		cursor.write_as(*self)
	}
}

impl<T: CrateIo + Default + std::hash::Hash + Eq + Clone + std::fmt::Debug> CrateIo
	for sdf::ListOp<T>
{
	fn read(file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
		let h = ListOpHeader(cursor.read_as::<u8>()?);

		let mut explicit_items = None;
		let mut prepended_items = Vec::new();
		let mut appended_items = Vec::new();
		let mut deleted_items = Vec::new();

		if h.has_explicit_items() {
			explicit_items = Some(read_typed_vec(file, cursor)?);
		} else if h.is_make_explicit() {
			explicit_items = Some(Vec::new());
		}

		if h.has_add_items() {
			// TODO: Should print a warning here, since this is non-normative
			let items = read_typed_vec::<T>(file, cursor)?;
			appended_items.extend(items);
		}

		if h.has_prepend_items() {
			prepended_items = read_typed_vec(file, cursor)?;
		}

		if h.has_append_items() {
			appended_items = read_typed_vec(file, cursor)?;
		}

		if h.has_delete_items() {
			deleted_items = read_typed_vec(file, cursor)?;
		}

		if h.has_reorder_items() {
			read_typed_vec::<T>(file, cursor)?;
			println!("ListOp 'reorder' operation is deprecated (discarding)");
		}

		Ok(if let Some(explicit_items) = explicit_items {
			sdf::ListOp::from_explicit(explicit_items)
		} else {
			sdf::ListOp::from_composable(prepended_items, appended_items, deleted_items)
		})
	}

	fn write(&self, file: &mut UsdcFile, cursor: &mut Cursor<&mut [u8]>) -> Result<()> {
		let h = ListOpHeader::from(self);

		cursor.write_as::<u8>(h.0)?;

		if h.has_explicit_items() {
			write_typed_vec(file, cursor, self.explicit_items().unwrap_or(&Vec::new()))?;
		}

		if h.has_prepend_items() {
			write_typed_vec(file, cursor, self.prepended_items())?;
		}

		if h.has_append_items() {
			write_typed_vec(file, cursor, self.appended_items())?;
		}

		if h.has_delete_items() {
			write_typed_vec(file, cursor, self.deleted_items())?;
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
		})
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

	fn read<T: CrateIo>(&self, cursor: &mut Cursor<&[u8]>) -> Result<T> {
		T::read(self, cursor)
	}
}

impl CrateIo for String {
	fn read(file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
		let index = StringIndex(cursor.read_as()?);
		Ok(file.get_string(index).to_string())
	}
}

impl CrateIo for tf::Token {
	fn read(file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
		let index = TokenIndex(cursor.read_as()?);
		Ok(file.get_token(index).clone())
	}
}

impl CrateIo for sdf::Path {
	fn read(file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
		let index = PathIndex(cursor.read_as()?);
		Ok(file.get_path(index).clone())
	}
}

thread_local! {
	static LOCAL_UNPACK_RECURSION_GUARD: RefCell<HashSet<u64>> = RefCell::new(HashSet::new());
}

impl CrateIo for vt::Value {
	fn read(_file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
		let offset = cursor.read_as::<i64>()?;
		cursor.seek_relative(offset - 8)?;

		/*let value_rep = ValueRep(cursor.read_as::<u64>()?);

		let mut recursion_guard = LOCAL_UNPACK_RECURSION_GUARD.with(|guard| guard.borrow_mut());

		if recursion_guard.insert(value_rep.0) {
			// TODO: Read value
		} else {
			panic!("Recursion detected while unpacking value");
		}

		recursion_guard.remove(&value_rep.0);*/

		Ok(vt::Value::new(0.0))
	}
}

impl<K: CrateIo + Eq + std::hash::Hash, V: CrateIo> CrateIo for HashMap<K, V> {
	fn read(file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
		let size = cursor.read_as::<u64>()? as usize;
		let mut map = HashMap::with_capacity(size);
		for _ in 0..size {
			let key = file.read::<K>(cursor)?;
			let value = file.read::<V>(cursor)?;
			map.insert(key, value);
		}
		Ok(map)
	}
}

impl CrateIo for sdf::Retiming {
	fn read(_file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
		Ok(Self {
			offset: cursor.read_as::<f64>()?,
			scale: cursor.read_as::<f64>()?,
		})
	}
}

impl CrateIo for sdf::Reference {
	fn read(file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
		let reference = Self {
			asset_path: file.read::<String>(cursor)?,
			prim_path: file.read::<sdf::Path>(cursor)?,
			layer_offset: file.read::<sdf::Retiming>(cursor)?,
		};

		file.read::<vt::Dictionary>(cursor)?;

		Ok(reference)
	}
}

impl CrateIo for sdf::Payload {
	fn read(file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
		Ok(Self {
			asset_path: file.read::<String>(cursor)?,
			prim_path: file.read::<sdf::Path>(cursor)?,
			layer_offset: file.read::<sdf::Retiming>(cursor)?,
		})
	}
}

impl CrateIo for sdf::Relocate {
	fn read(file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
		Ok(Self {
			source: file.read::<sdf::Path>(cursor)?,
			target: file.read::<sdf::Path>(cursor)?,
		})
	}
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

fn fields<'a>(usdc: &'a UsdcFile, spec: &Spec) -> impl Iterator<Item = &'a Field> + 'a {
	usdc.field_sets
		.iter()
		.skip(spec.field_set_index.0 as usize)
		.take_while(|&&x| x != FieldIndex(Index::MAX))
		.map(|&x| usdc.get_field(x))
}

struct SpecData {
	fields: Vec<FieldValuePair>,
	spec_form: sdf::SpecForm,
}

pub struct Data {
	data: HashMap<sdf::Path, SpecData>,
	crate_data: Box<UsdcFile>,
}

impl Data {
	pub fn open(asset_path: &std::path::Path) -> Result<Box<dyn sdf::AbstractData>> {
		let data = HashMap::new();
		let crate_data = Box::new(UsdcFile::open(asset_path)?);
		Ok(Box::new(Self { data, crate_data }))
	}
}

impl sdf::AbstractData for Data {
	fn spec_form(&self, path: &sdf::Path) -> Option<sdf::SpecForm> {
		// TODO: self.data.get(path).map(|spec| spec.spec_form)

		let path_index = PathIndex(self.crate_data.paths.iter().position(|p| *p == *path)? as u32);

		self.crate_data
			.specs
			.iter()
			.find(|s| s.path_index == path_index)
			.map(|spec| spec.spec_form)
	}

	fn get(&self, path: &sdf::Path, field: &tf::Token) -> Option<vt::Value> {
		let path_index = PathIndex(self.crate_data.paths.iter().position(|p| *p == *path)? as u32);

		let spec = self
			.crate_data
			.specs
			.iter()
			.find(|s| s.path_index == path_index)?;

		let field = fields(&self.crate_data, spec)
			.find(|f| self.crate_data.get_token(f.token_index) == field)?;

		unpack_value_rep(&self.crate_data, field.value_rep).unwrap()
	}

	fn list(&self, path: &sdf::Path) -> Vec<&tf::Token> {
		let path_index = match self.crate_data.paths.iter().position(|p| *p == *path) {
			Some(index) => PathIndex(index as u32),
			None => return Vec::new(),
		};

		let spec = match self
			.crate_data
			.specs
			.iter()
			.find(|s| s.path_index == path_index)
		{
			Some(spec) => spec,
			None => return Vec::new(),
		};

		fields(&self.crate_data, spec)
			.map(|field| self.crate_data.get_token(field.token_index))
			.collect()
	}

	fn visit_specs(&self) -> Vec<&sdf::Path> {
		// TODO: self.data.keys().collect()

		self.crate_data
			.specs
			.iter()
			.map(|spec| self.crate_data.get_path(spec.path_index))
			.collect()
	}
}
