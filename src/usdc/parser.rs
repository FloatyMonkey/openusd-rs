use super::integer_coding::*;
use crate::io_ext::{ReadBytesExt, WriteBytesExt};
use crate::{gf, sdf, tf, vt};

use std::{
	cell::RefCell,
	collections::HashSet,
	io::{Cursor, Read, Seek},
	path::Path,
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
	const TOKENS: &'static str = "TOKENS";
	const STRINGS: &'static str = "STRINGS";
	const FIELDS: &'static str = "FIELDS";
	const FIELDSETS: &'static str = "FIELDSETS";
	const PATHS: &'static str = "PATHS";
	const SPECS: &'static str = "SPECS";

	fn name(&self) -> Option<&str> {
		let len = self.name.iter().position(|&x| x == 0)?;
		std::str::from_utf8(&self.name[0..len]).ok()
	}
}

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
#[derive(Clone, Copy)]
struct Field {
	token_index: Index,
	value_rep: ValueRep,
}

#[repr(C)]
struct Spec {
	path_index: Index,
	field_set_index: Index,
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
#[derive(Clone, Copy)]
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

fn decompress_from_buffer(compressed_buffer: &[u8], workspace_size: usize) -> Vec<u8> {
	let mut workspace_buffer = vec![0u8; workspace_size];
	let mut offset = 1;
	let count = compressed_buffer[0] as i32;
	let mut total_decompressed = 0;
	let chunk_size = compressed_buffer.len() - 1;

	for _ in 0..count.max(1) {
		let mut current_chunk_size = chunk_size;

		if count != 0 {
			let size_bytes: [u8; 4] = compressed_buffer[offset..offset + 4]
				.try_into()
				.expect("Slice with incorrect length");
			current_chunk_size = i32::from_le_bytes(size_bytes) as usize;
			offset += 4;
		}

		let decompressed_data = lz4_flex::decompress(
			&compressed_buffer[offset..offset + current_chunk_size],
			workspace_buffer.len() - total_decompressed,
		)
		.unwrap();

		let decompressed_size = decompressed_data.len();
		if decompressed_size > workspace_buffer.len() - total_decompressed {
			panic!("Unexpected decompressed chunk size");
		}

		workspace_buffer[total_decompressed..total_decompressed + decompressed_size]
			.copy_from_slice(&decompressed_data);

		offset += current_chunk_size;
		total_decompressed += decompressed_size;
	}

	workspace_buffer.truncate(total_decompressed);
	workspace_buffer
}

const LZ4_MAX_INPUT_SIZE: usize = 0x7E000000;

fn get_max_input_size() -> usize {
	127 * LZ4_MAX_INPUT_SIZE
}

fn lz4_compress_bound(size: usize) -> usize {
	if size > LZ4_MAX_INPUT_SIZE {
		return 0;
	}
	size + (size / 255) + 16
}

fn get_compressed_buffer_size(input_size: usize) -> usize {
	if input_size > get_max_input_size() {
		return 0;
	}

	if input_size <= LZ4_MAX_INPUT_SIZE {
		return lz4_compress_bound(input_size) + 1;
	}

	let n_whole_chunks = input_size / LZ4_MAX_INPUT_SIZE;
	let part_chunk_sz = input_size % LZ4_MAX_INPUT_SIZE;
	let mut sz = 1 + n_whole_chunks * (lz4_compress_bound(LZ4_MAX_INPUT_SIZE) + 4);
	if part_chunk_sz > 0 {
		sz += lz4_compress_bound(part_chunk_sz) + 4;
	}
	sz
}

fn read_compressed_ints<T: IntMapper>(cursor: &mut Cursor<&[u8]>, count: usize) -> Result<Vec<T>> {
	let compressed_size = cursor.read_as::<u64>()?;
	let workspace_size = get_compressed_buffer_size(get_encoded_buffer_size::<T>(count));

	let mut compressed_buffer = vec![0; compressed_size as usize];
	cursor.read_exact(&mut compressed_buffer)?;

	let uncompressed_buffer = decompress_from_buffer(&compressed_buffer, workspace_size);

	decode_integers::<T>(&uncompressed_buffer, count)
}

fn read_tokens(cursor: &mut Cursor<&[u8]>, section: &Section) -> Result<Vec<tf::Token>> {
	cursor.set_position(section.start);

	let token_count = cursor.read_as::<u64>()?;
	let uncompressed_size = cursor.read_as::<u64>()?;
	let compressed_size = cursor.read_as::<u64>()?;

	assert_eq!(compressed_size + 24, section.size);

	let mut compressed_buffer = vec![0; compressed_size as usize];
	cursor.read_exact(&mut compressed_buffer)?;

	let mut buffer = decompress_from_buffer(&compressed_buffer, uncompressed_size as usize);

	if buffer.last() != Some(&b'\0') {
		panic!("Tokens section not null-terminated in crate file");
	}

	buffer.pop(); // Prevent empty string at the end.

	let tokens = buffer
		.split(|c| *c == b'\0')
		.map(|b| std::str::from_utf8(b).unwrap())
		.map(|s| tf::Token::new(s))
		.collect::<Vec<_>>();

	if tokens.len() != token_count as usize {
		panic!(
			"Crate file claims {} tokens, found {}",
			token_count,
			tokens.len()
		);
	}

	Ok(tokens)
}

fn read_strings(cursor: &mut Cursor<&[u8]>, section: &Section) -> Result<Vec<Index>> {
	cursor.set_position(section.start);

	let indices_count = cursor.read_as::<u64>()?;

	let mut indices = vec![];

	for _ in 0..indices_count {
		indices.push(cursor.read_as::<Index>()?);
	}

	Ok(indices)
}

fn read_fields(cursor: &mut Cursor<&[u8]>, section: &Section) -> Result<Vec<Field>> {
	cursor.set_position(section.start);

	let field_count = cursor.read_as::<u64>()? as usize;

	let indices = read_compressed_ints::<u32>(cursor, field_count)?;

	let flag_size = cursor.read_as::<u64>()?;
	let mut compressed_buffer = vec![0; flag_size as usize];
	cursor.read_exact(&mut compressed_buffer)?;

	let uncompressed_buffer = decompress_from_buffer(&compressed_buffer, field_count * 8);
	let mut uncompressed_cursor = Cursor::new(uncompressed_buffer.as_slice());

	let fields = indices
		.iter()
		.map(|&name| Field {
			token_index: name as Index,
			value_rep: ValueRep(uncompressed_cursor.read_as::<u64>().unwrap()),
		})
		.collect();

	Ok(fields)
}

fn read_field_sets(cursor: &mut Cursor<&[u8]>, section: &Section) -> Result<Vec<Index>> {
	cursor.set_position(section.start);

	let field_set_count = cursor.read_as::<u64>()? as usize;

	read_compressed_ints::<Index>(cursor, field_set_count)
}

fn read_paths(
	cursor: &mut Cursor<&[u8]>,
	section: &Section,
	tokens: &[tf::Token],
) -> Result<Vec<sdf::Path>> {
	cursor.set_position(section.start);

	let path_count = cursor.read_as::<u64>()? as usize;
	let _ = cursor.read_as::<u64>()?; // Must be equal to path_count

	let path_indices = read_compressed_ints::<u32>(cursor, path_count)?;
	let element_token_indices = read_compressed_ints::<i32>(cursor, path_count)?;
	let jumps = read_compressed_ints::<i32>(cursor, path_count)?;

	let mut paths = vec![sdf::Path::empty_path(); path_count];

	build_decompressed_paths_recursive(
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
			path_index: path_indices[i],
			field_set_index: field_set_indices[i],
			spec_type: unsafe { std::mem::transmute::<u32, sdf::SpecType>(spec_types[i]) },
		});
	}

	Ok(spec)
}

/// Don't compress arrays smaller than this.
const MIN_COMPRESSED_ARRAY_SIZE: usize = 16;

fn read_int_array<T: Clone + IntMapper>(
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

fn build_decompressed_paths_recursive(
	path_indices: &[u32],
	element_token_indices: &[i32],
	jumps: &[i32],
	mut cur_index: usize,
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
				build_decompressed_paths_recursive(
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

fn read_inline(val: ValueRep) -> Option<vt::Value> {
	Some(match val.ty() {
		Type::Vec2i if val.is_inlined() => read_inline_vec2::<i32>(val).into(),
		Type::Vec2h if val.is_inlined() => read_inline_vec2::<f16>(val).into(),
		Type::Vec2f if val.is_inlined() => read_inline_vec2::<f32>(val).into(),
		Type::Vec2d if val.is_inlined() => read_inline_vec2::<f64>(val).into(),

		Type::Vec3i if val.is_inlined() => read_inline_vec3::<i32>(val).into(),
		Type::Vec3h if val.is_inlined() => read_inline_vec3::<f16>(val).into(),
		Type::Vec3f if val.is_inlined() => read_inline_vec3::<f32>(val).into(),
		Type::Vec3d if val.is_inlined() => read_inline_vec3::<f64>(val).into(),

		Type::Vec4i if val.is_inlined() => read_inline_vec4::<i32>(val).into(),
		Type::Vec4h if val.is_inlined() => read_inline_vec4::<f16>(val).into(),
		Type::Vec4f if val.is_inlined() => read_inline_vec4::<f32>(val).into(),
		Type::Vec4d if val.is_inlined() => read_inline_vec4::<f64>(val).into(),

		Type::Matrix2d if val.is_inlined() => read_inline_mat2(val).into(),
		Type::Matrix3d if val.is_inlined() => read_inline_mat3(val).into(),
		Type::Matrix4d if val.is_inlined() => read_inline_mat4(val).into(),

		_ => return None,
	})
}

fn unpack_value_rep(file: &UsdcFile, value: ValueRep) -> Result<Option<vt::Value>> {
	let buffer = &file.buffer;

	if value.ty() == Type::Token && !value.is_array() && value.is_inlined() {
		let token_index = value.payload();
		let token = file.tokens[token_index as usize].clone();

		return Ok(Some(vt::Value::new(token)));
	}

	if let Some(v) = read_inline(value) {
		return Ok(Some(v));
	}

	let mut cursor = Cursor::new(buffer.as_slice());
	cursor.set_position(value.payload());

	Ok(Some(match value.ty() {
		Type::Half if value.is_array() => read_float_array::<f16>(&mut cursor, value)?.into(),
		Type::Float if value.is_array() => read_float_array::<f32>(&mut cursor, value)?.into(),
		Type::Double if value.is_array() => read_float_array::<f64>(&mut cursor, value)?.into(),

		Type::Float if value.is_inlined() => {
			let val = value.payload() as u32;
			f32::from_bits(val).into()
		}

		Type::AssetPath => {
			let token = file.tokens[value.payload() as usize].clone();
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
				.map(|&i| file.paths[i as usize].clone())
				.collect::<vt::Array<sdf::Path>>();
			vt::Value::new(vector)
		}

		Type::TokenVector => {
			let indices = Vec::<Index>::read(file, &mut cursor)?;
			let vector = indices
				.iter()
				.map(|i| file.tokens[*i as usize].clone())
				.collect::<vt::Array<_>>();
			vt::Value::new(vector)
		}

		Type::Token if value.is_array() => {
			let indices = Vec::<Index>::read(file, &mut cursor)?;
			let vector = indices
				.iter()
				.map(|i| file.tokens[*i as usize].clone())
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
		let path_index = self.paths.iter().position(|p| *p == *path)?;

		let spec = self
			.specs
			.iter()
			.find(|s| s.path_index == path_index as Index)?;

		let field = fields(self, spec).find(|f| self.tokens[f.token_index as usize] == *field)?;

		unpack_value_rep(self, field.value_rep).unwrap()
	}

	fn spec_type(&self, path: &sdf::Path) -> Option<sdf::SpecType> {
		let path_index = self.paths.iter().position(|p| *p == *path)?;

		self.specs
			.iter()
			.find(|s| s.path_index == path_index as Index)
			.map(|spec| spec.spec_type)
	}

	fn list(&self, path: &sdf::Path) -> Vec<&tf::Token> {
		let path_index = match self.paths.iter().position(|p| *p == *path) {
			Some(index) => index,
			None => return Vec::new(),
		};

		let spec = match self
			.specs
			.iter()
			.find(|s| s.path_index == path_index as Index)
		{
			Some(spec) => spec,
			None => return Vec::new(),
		};

		fields(self, spec)
			.map(|field| &self.tokens[field.token_index as usize])
			.collect()
	}

	fn visit_specs(&self) -> Vec<&sdf::Path> {
		self.specs
			.iter()
			.map(|spec| &self.paths[spec.path_index as usize])
			.collect()
	}
}

pub struct UsdcFile {
	version: Version,
	buffer: Vec<u8>,

	specs: Vec<Spec>,
	fields: Vec<Field>,
	field_sets: Vec<Index>,

	paths: Vec<sdf::Path>,
	tokens: Vec<tf::Token>,
	strings: Vec<Index>,
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
	pub fn open(asset_path: &Path) -> Result<Self> {
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
		})
	}
}

impl UsdcFile {
	fn get_string(&self, index: Index) -> &str {
		self.tokens[self.strings[index as usize] as usize].as_str()
	}

	fn get_token(&self, index: Index) -> &tf::Token {
		&self.tokens[index as usize]
	}

	fn get_path(&self, index: Index) -> &sdf::Path {
		&self.paths[index as usize]
	}

	fn read<T: CrateIo>(&self, cursor: &mut Cursor<&[u8]>) -> Result<T> {
		T::read(self, cursor)
	}
}

impl CrateIo for String {
	fn read(file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
		Ok(file.get_string(cursor.read_as::<Index>()?).to_string())
	}
}

impl CrateIo for tf::Token {
	fn read(file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
		Ok(file.get_token(cursor.read_as::<Index>()?).clone())
	}
}

impl CrateIo for sdf::Path {
	fn read(file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
		Ok(file.get_path(cursor.read_as::<Index>()?).clone())
	}
}

impl CrateIo for sdf::LayerOffset {
	fn read(file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
		Ok(Self {
			offset: cursor.read_as::<f64>()?,
			scale: cursor.read_as::<f64>()?,
		})
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

impl CrateIo for sdf::Reference {
	fn read(file: &UsdcFile, cursor: &mut Cursor<&[u8]>) -> Result<Self> {
		Ok(Self {
			asset_path: file.read::<String>(cursor)?,
			prim_path: file.read::<sdf::Path>(cursor)?,
			layer_offset: file.read::<sdf::LayerOffset>(cursor)?,
			custom_data: file.read::<vt::Dictionary>(cursor)?,
		})
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
}

fn fields<'a>(usdc: &'a UsdcFile, spec: &Spec) -> impl Iterator<Item = &'a Field> + 'a {
	usdc.field_sets
		.iter()
		.skip(spec.field_set_index as usize)
		.take_while(|&&x| x != Index::MAX)
		.map(|&x| &usdc.fields[x as usize])
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
