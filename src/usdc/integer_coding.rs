#![allow(dead_code)] // TODO: Remove once writing is implemented.

use std::collections::HashMap;
use std::io::{Cursor, Read, Write};

use super::compression;
use crate::io_ext::{ReadBytesExt, Readable, WriteBytesExt, Writeable};

fn encoded_buffer_size<T>(count: usize) -> usize {
	if count == 0 {
		0
	} else {
		// common_value + num_codes_bytes + max_int_bytes
		size_of::<T>() + (count * 2).div_ceil(8) + (count * size_of::<T>())
	}
}

fn decode_integers<T: Integer>(buffer: &[u8], count: usize) -> std::io::Result<Vec<T>> {
	let mut cursor = Cursor::new(buffer);
	let common_value = cursor.read_as::<T::Delta>()?;
	let num_codes_bytes = (count * 2).div_ceil(8);
	let offset = size_of::<T::Delta>();

	let mut codes = Cursor::new(&buffer[offset..offset + num_codes_bytes]);
	let mut vints = Cursor::new(&buffer[offset + num_codes_bytes..]);

	let mut result = Vec::new();
	let mut prev = T::Delta::default();
	let mut ints_left = count;

	while ints_left > 0 {
		let to_process = ints_left.min(4);
		let code_byte = codes.read_as::<u8>()?;

		for i in 0..to_process {
			let delta = match (code_byte >> (2 * i)) & 3 {
				0 => common_value,
				1 => vints.read_as::<T::Small>()?.into(),
				2 => vints.read_as::<T::Medium>()?.into(),
				_ => vints.read_as::<T::Delta>()?,
			};
			prev = prev + delta;
			result.push(T::from_delta(prev));
		}

		ints_left -= to_process;
	}

	assert_eq!(result.len(), count);
	Ok(result)
}

fn encode_integers<T: Integer>(values: &[T]) -> Vec<u8> {
	if values.is_empty() {
		return Vec::new();
	}

	let mut delta_counts: HashMap<T::Delta, usize> = HashMap::new();
	let mut prev = T::Delta::default();
	for value in values {
		let cur = value.to_delta();
		*delta_counts.entry(cur - prev).or_insert(0) += 1;
		prev = cur;
	}

	let common = delta_counts
		.iter()
		.max_by(|a, b| a.1.cmp(b.1).then(a.0.cmp(b.0)))
		.map(|(&d, _)| d)
		.unwrap_or_default();

	let mut output: Vec<u8> = Vec::new();
	output.write_as(common).unwrap();

	let codes_start = output.len();
	output.resize(codes_start + (values.len() * 2).div_ceil(8), 0);

	let mut vints: Vec<u8> = Vec::new();
	let mut codes_pos = 0;
	let mut bit_pos = 0;

	prev = T::Delta::default();
	for value in values {
		let cur = value.to_delta();
		let delta = cur - prev;
		prev = cur;

		let code = T::get_code(delta, common);
		output[codes_start + codes_pos] |= code << bit_pos;

		match code {
			1 => vints.write_as(T::to_small(delta)).unwrap(),
			2 => vints.write_as(T::to_medium(delta)).unwrap(),
			3 => vints.write_as(delta).unwrap(),
			_ => {}
		}

		bit_pos += 2;
		if bit_pos >= 8 {
			bit_pos = 0;
			codes_pos += 1;
		}
	}

	output.extend_from_slice(&vints);
	output
}

pub fn read_compressed_ints<T: Integer>(
	cursor: &mut Cursor<&[u8]>,
	count: usize,
) -> std::io::Result<Vec<T>> {
	let compressed_size = cursor.read_as::<u64>()?;
	let workspace_size =
		compression::compressed_buffer_size(encoded_buffer_size::<T::Delta>(count));

	let mut compressed_buffer = vec![0; compressed_size as usize];
	cursor.read_exact(&mut compressed_buffer)?;

	let uncompressed_buffer =
		compression::decompress_from_buffer(&compressed_buffer, workspace_size);

	decode_integers::<T>(&uncompressed_buffer, count)
}

pub fn write_compressed_ints<T: Integer>(
	cursor: &mut Cursor<Vec<u8>>,
	values: &[T],
) -> std::io::Result<()> {
	let encoded_data = encode_integers(values);
	let compressed_data = compression::compress_to_buffer(&encoded_data);

	cursor.write_as::<u64>(compressed_data.len() as u64)?;
	cursor.write_all(&compressed_data)?;
	Ok(())
}

pub trait Integer: Sized + Copy {
	type Delta: Copy
		+ Ord
		+ Default
		+ Eq
		+ std::hash::Hash
		+ std::ops::Add<Output = Self::Delta>
		+ std::ops::Sub<Output = Self::Delta>
		+ Readable
		+ Writeable;
	type Small: Readable + Writeable + Copy + Into<Self::Delta>;
	type Medium: Readable + Writeable + Copy + Into<Self::Delta>;

	fn to_delta(self) -> Self::Delta;
	fn from_delta(d: Self::Delta) -> Self;
	fn get_code(delta: Self::Delta, common: Self::Delta) -> u8;
	fn to_small(d: Self::Delta) -> Self::Small;
	fn to_medium(d: Self::Delta) -> Self::Medium;
}

macro_rules! impl_integer {
	($int:ty, $delta:ty, $small:ty, $medium:ty) => {
		impl Integer for $int {
			type Delta = $delta;
			type Small = $small;
			type Medium = $medium;

			fn to_delta(self) -> $delta {
				self as $delta
			}
			fn from_delta(d: $delta) -> Self {
				d as $int
			}

			fn get_code(delta: $delta, common: $delta) -> u8 {
				if delta == common {
					0
				} else if (<$small>::MIN as $delta..=<$small>::MAX as $delta).contains(&delta) {
					1
				} else if (<$medium>::MIN as $delta..=<$medium>::MAX as $delta).contains(&delta) {
					2
				} else {
					3
				}
			}

			fn to_small(d: $delta) -> $small {
				d as $small
			}
			fn to_medium(d: $delta) -> $medium {
				d as $medium
			}
		}
	};
}

impl_integer!(i32, i32, i8, i16);
impl_integer!(u32, i32, i8, i16);
impl_integer!(i64, i64, i16, i32);
impl_integer!(u64, i64, i16, i32);

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn encode_decode_u32() {
		// See https://github.com/PixarAnimationStudios/OpenUSD/blob/29876eddefc2c9c62fc752da0b482456408cfd48/pxr/usd/sdf/integerCoding.cpp#L23
		// input  = [123, 124, 125, 100125, 100125, 100126, 100126]
		// output = [int32(1) 01 00 00 11 01 00 01 XX int8(123) int32(100000) int8(0) int8(0)]

		let input = vec![123_u32, 124, 125, 100125, 100125, 100126, 100126];

		let mut output = Vec::new();
		output.extend_from_slice(&1_i32.to_le_bytes());
		output.extend_from_slice(&0b00_01_00_01_11_00_00_01_u16.to_le_bytes()); // Reverse order
		output.extend_from_slice(&123_i8.to_le_bytes());
		output.extend_from_slice(&100000_i32.to_le_bytes());
		output.extend_from_slice(&0_i8.to_le_bytes());
		output.extend_from_slice(&0_i8.to_le_bytes());

		assert_eq!(output, encode_integers::<u32>(&input));
		assert_eq!(input, decode_integers::<u32>(&output, 7).unwrap());
	}

	#[test]
	fn encode_decode_u64() {
		let input = vec![123_u64, 124, 125, 100125, 100125, 100126, 100126];

		let encoded = encode_integers::<u64>(&input);
		let decoded = decode_integers::<u64>(&encoded, 7).unwrap();

		assert_eq!(input, decoded);
	}
}
