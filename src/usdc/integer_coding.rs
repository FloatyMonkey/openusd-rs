use std::io::Cursor;

use crate::io_ext::ReadBytesExt;

pub fn get_encoded_buffer_size<T>(count: usize) -> usize {
	if count == 0 {
		0
	} else {
		// common_value + num_codes_bytes + max_int_bytes
		size_of::<T>() + (count * 2).div_ceil(8) + (count * size_of::<T>())
	}
}

// TODO: Support i64, u64 and perform intermediate computations as correct type
pub trait IntMapper {
	fn map_int(value: i32) -> Self;
}

impl IntMapper for i32 {
	fn map_int(value: i32) -> i32 {
		value
	}
}

impl IntMapper for u32 {
	fn map_int(value: i32) -> u32 {
		value as u32
	}
}

pub fn decode_integers<T: IntMapper>(buffer: &[u8], count: usize) -> std::io::Result<Vec<T>> {
	let common_value = i32::from_le_bytes(buffer[0..4].try_into().unwrap());
	let num_codes_bytes = (count * 2).div_ceil(8);

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

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn decode() {
		// See https://github.com/PixarAnimationStudios/OpenUSD/blob/29876eddefc2c9c62fc752da0b482456408cfd48/pxr/usd/sdf/integerCoding.cpp#L23
		// input  = [123, 124, 125, 100125, 100125, 100126, 100126]
		// output = [int32(1) 01 00 00 11 01 00 01 XX int8(123) int32(100000) int8(0) int8(0)]

		let input = &[123_u32, 124, 125, 100125, 100125, 100126, 100126];

		let mut output = Vec::new();
		output.extend_from_slice(&1_i32.to_le_bytes());
		output.extend_from_slice(&0b00_01_00_01_11_00_00_01_u16.to_le_bytes()); // Reverse order
		output.extend_from_slice(&123_i8.to_le_bytes());
		output.extend_from_slice(&100000_i32.to_le_bytes());
		output.extend_from_slice(&0_i8.to_le_bytes());
		output.extend_from_slice(&0_i8.to_le_bytes());

		assert_eq!(
			input,
			decode_integers::<u32>(&output, 7).unwrap().as_slice()
		);
	}
}
