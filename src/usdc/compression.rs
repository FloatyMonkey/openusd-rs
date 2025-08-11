const LZ4_MAX_INPUT_SIZE: usize = 0x7E000000;

fn lz4_compress_bound(size: usize) -> usize {
	if size > LZ4_MAX_INPUT_SIZE {
		return 0;
	}
	size + (size / 255) + 16
}

fn max_input_size() -> usize {
	127 * LZ4_MAX_INPUT_SIZE
}

pub fn compressed_buffer_size(input_size: usize) -> usize {
	if input_size > max_input_size() {
		return 0;
	}

	if input_size <= LZ4_MAX_INPUT_SIZE {
		return lz4_compress_bound(input_size) + 1;
	}

	let n_whole_chunks = input_size / LZ4_MAX_INPUT_SIZE;
	let part_chunk_sz = input_size % LZ4_MAX_INPUT_SIZE;
	let mut sz = 1 + n_whole_chunks * (lz4_compress_bound(LZ4_MAX_INPUT_SIZE) + size_of::<i32>());
	if part_chunk_sz > 0 {
		sz += lz4_compress_bound(part_chunk_sz) + size_of::<i32>();
	}
	sz
}

pub fn compress_to_buffer(input: &[u8]) -> Vec<u8> {
	let mut output = vec![0u8; compressed_buffer_size(input.len())];
	let mut compressed = &mut output[..];

	let n_chunks = if input.len() <= LZ4_MAX_INPUT_SIZE {
		0
	} else {
		input.len().div_ceil(LZ4_MAX_INPUT_SIZE)
	};

	compressed[0] = n_chunks as u8;
	compressed = &mut compressed[1..];

	let compressed_size = if n_chunks == 0 {
		lz4_flex::compress_into(input, compressed).unwrap()
	} else {
		let mut offset = 0;
		let mut compressed_size = 0;

		for _ in 0..n_chunks {
			let chunk_size = if offset + LZ4_MAX_INPUT_SIZE < input.len() {
				LZ4_MAX_INPUT_SIZE
			} else {
				input.len() - offset
			};

			compressed_size +=
				lz4_flex::compress_into(&input[offset..offset + chunk_size], compressed).unwrap();
			compressed[..size_of::<i32>()].copy_from_slice(&(chunk_size as i32).to_le_bytes());
			compressed = &mut compressed[size_of::<i32>()..];
			offset += chunk_size;
		}

		compressed_size
	};

	output.truncate(compressed_size + 1); // +1 for the chunk count
	output
}

pub fn decompress_from_buffer(compressed: &[u8], max_output_size: usize) -> Vec<u8> {
	let mut output = vec![0u8; max_output_size];

	let n_chunks = compressed[0] as usize;
	let mut compressed = &compressed[1..];

	let decompressed_size = if n_chunks == 0 {
		lz4_flex::decompress_into(compressed, &mut output).unwrap()
	} else {
		let mut total_decompressed = 0;

		for _ in 0..n_chunks {
			let chunk_size_bytes = compressed[..size_of::<i32>()].try_into().unwrap();
			let chunk_size = i32::from_le_bytes(chunk_size_bytes) as usize;

			compressed = &compressed[size_of::<i32>()..];

			let decompressed_chunk_size =
				lz4_flex::decompress_into(compressed, &mut output[total_decompressed..]).unwrap();

			total_decompressed += decompressed_chunk_size;
			compressed = &compressed[chunk_size..];
		}

		total_decompressed
	};

	output.truncate(decompressed_size);
	output
}
