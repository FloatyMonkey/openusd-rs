use std::io::Read;

pub trait Readable {
	fn read_from<R: Read + ?Sized>(reader: &mut R) -> Self;
}

impl Readable for u8 {
	#[inline]
	fn read_from<R: Read + ?Sized>(reader: &mut R) -> Self {
		let mut byte = [0; 1];
		reader.read_exact(&mut byte).unwrap();
		byte[0]
	}
}

impl Readable for i8 {
	#[inline]
	fn read_from<R: Read + ?Sized>(reader: &mut R) -> Self {
		let mut byte = [0; 1];
		reader.read_exact(&mut byte).unwrap();
		byte[0] as i8
	}
}

macro_rules! impl_readable {
	($t:ty) => {
		impl Readable for $t {
			#[inline]
			fn read_from<R: Read + ?Sized>(reader: &mut R) -> Self {
				let mut byte = [0; size_of::<Self>()];
				reader.read_exact(&mut byte).unwrap();
				Self::from_le_bytes(byte)
			}
		}
	};
}

impl_readable!(i16);
impl_readable!(i32);
impl_readable!(i64);
impl_readable!(u16);
impl_readable!(u32);
impl_readable!(u64);
impl_readable!(f32);
impl_readable!(f64);

pub trait ReadBytesExt: std::io::Read {
	fn read_as<T: Readable>(&mut self) -> T {
		T::read_from(self)
	}
}

impl<R: std::io::Read + ?Sized> ReadBytesExt for R {}

pub trait Writeable {
	fn write_to<W: std::io::Write + ?Sized>(&self, writer: &mut W);
}

impl Writeable for u8 {
	#[inline]
	fn write_to<W: std::io::Write + ?Sized>(&self, writer: &mut W) {
		writer.write_all(&[*self]).unwrap();
	}
}

impl Writeable for i8 {
	#[inline]
	fn write_to<W: std::io::Write + ?Sized>(&self, writer: &mut W) {
		writer.write_all(&[*self as u8]).unwrap();
	}
}

macro_rules! impl_writeable {
	($t:ty) => {
		impl Writeable for $t {
			#[inline]
			fn write_to<W: std::io::Write + ?Sized>(&self, writer: &mut W) {
				writer.write_all(&self.to_le_bytes()).unwrap();
			}
		}
	};
}

impl_writeable!(i16);
impl_writeable!(i32);
impl_writeable!(i64);
impl_writeable!(u16);
impl_writeable!(u32);
impl_writeable!(u64);
impl_writeable!(f32);
impl_writeable!(f64);

pub trait WriteBytesExt: std::io::Write {
	fn write_as<T: Writeable>(&mut self, value: T) {
		value.write_to(self)
	}
}

impl<W: std::io::Write + ?Sized> WriteBytesExt for W {}
