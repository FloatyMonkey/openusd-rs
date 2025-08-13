use std::collections::HashMap;
use std::io::{Cursor, Read, Write};

use super::compression;
use crate::io_ext::{ReadBytesExt, WriteBytesExt};

fn encoded_buffer_size<T>(count: usize) -> usize {
	if count == 0 {
		0
	} else {
		// common_value + num_codes_bytes + max_int_bytes
		size_of::<T>() + ((count * 2 + 7) / 8) + (count * size_of::<T>())
	}
}

fn decode_integers<T: Integer>(buffer: &[u8], count: usize) -> std::io::Result<Vec<T>> {
	let common_value = i32::from_le_bytes(buffer[0..4].try_into().unwrap());
	let num_codes_bytes = (count * 2 + 7) / 8;

	let codes_in = &buffer[4..4 + num_codes_bytes];
	let vints_in = &buffer[4 + num_codes_bytes..];

	let mut codes_cursor = Cursor::new(codes_in);
	let mut vints_cursor = Cursor::new(vints_in);

	let mut result = Vec::new();

	let mut prev_value = 0;
	let mut ints_left = count;
	while ints_left > 0 {
		let to_process = ints_left.min(4);

		let code_byte = codes_cursor.read_as::<u8>()?;

		for i in 0..to_process {
			prev_value += match (code_byte >> (2 * i)) & 3 {
				1 => vints_cursor.read_as::<i8>()? as i32,
				2 => vints_cursor.read_as::<i16>()? as i32,
				3 => vints_cursor.read_as::<i32>()?,
				_ => common_value,
			};

			result.push(T::map_int(prev_value));
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

	// Find the most common delta value
	let mut delta_counts: HashMap<i32, usize> = HashMap::new();
	let mut prev_value = 0i32;
	for value in values {
		let current = value.to_i32();
		let delta = current - prev_value;
		*delta_counts.entry(delta).or_insert(0) += 1;
		prev_value = current;
	}

	let common_value = delta_counts
		.iter()
		.max_by(|a, b| a.1.cmp(b.1).then(a.0.cmp(b.0)))
		.map(|(delta, _)| *delta)
		.unwrap_or(0);

	let mut output = Vec::new();
	let mut cursor = Cursor::new(&mut output);
	cursor.write_as(common_value).unwrap();

	let num_codes_bytes = (values.len() * 2 + 7) / 8;
	let codes_start = output.len();
	output.resize(codes_start + num_codes_bytes, 0);

	let mut vints_data = Vec::new();
	let mut vints_cursor = Cursor::new(&mut vints_data);
	let mut codes_pos = 0;
	let mut bit_pos = 0;

	prev_value = 0;
	for value in values {
		let current = value.to_i32();
		let delta = current - prev_value;
		prev_value = current;

		let code = get_code(delta, common_value);

		let byte_idx = codes_start + codes_pos;
		output[byte_idx] |= (code << bit_pos) as u8;

		match code {
			1 => vints_cursor.write_as::<i8>(delta as i8).unwrap(),
			2 => vints_cursor.write_as::<i16>(delta as i16).unwrap(),
			3 => vints_cursor.write_as::<i32>(delta).unwrap(),
			_ => {} // common_value
		}

		bit_pos += 2;
		if bit_pos >= 8 {
			bit_pos = 0;
			codes_pos += 1;
		}
	}

	output.extend_from_slice(&vints_data);
	output
}

fn get_code(delta: i32, common_value: i32) -> u8 {
	if delta == common_value {
		0 // Common
	} else if delta >= i8::MIN as i32 && delta <= i8::MAX as i32 {
		1 // Small (i8)
	} else if delta >= i16::MIN as i32 && delta <= i16::MAX as i32 {
		2 // Medium (i16)
	} else {
		3 // Large (i32)
	}
}

pub fn read_compressed_ints<T: Integer>(
	cursor: &mut Cursor<&[u8]>,
	count: usize,
) -> std::io::Result<Vec<T>> {
	let compressed_size = cursor.read_as::<u64>()?;
	let workspace_size = compression::compressed_buffer_size(encoded_buffer_size::<T>(count));

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

// TODO: Support i64, u64 and perform intermediate computations as correct type
pub trait Integer {
	fn map_int(value: i32) -> Self;
	fn to_i32(&self) -> i32;
}

impl Integer for i32 {
	fn map_int(value: i32) -> i32 {
		value
	}

	fn to_i32(&self) -> i32 {
		*self
	}
}

impl Integer for u32 {
	fn map_int(value: i32) -> u32 {
		value as u32
	}

	fn to_i32(&self) -> i32 {
		*self as i32
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn encode_decode() {
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
}
