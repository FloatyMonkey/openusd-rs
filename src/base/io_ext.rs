use std::io::{Read, Result, Write};

pub trait Readable {
	fn read_from<R: Read + ?Sized>(reader: &mut R) -> Result<Self>
	where
		Self: Sized;
}

impl Readable for u8 {
	#[inline]
	fn read_from<R: Read + ?Sized>(reader: &mut R) -> Result<Self> {
		let mut byte = [0; 1];
		reader.read_exact(&mut byte)?;
		Ok(byte[0])
	}
}

impl Readable for i8 {
	#[inline]
	fn read_from<R: Read + ?Sized>(reader: &mut R) -> Result<Self> {
		let mut byte = [0; 1];
		reader.read_exact(&mut byte)?;
		Ok(byte[0] as i8)
	}
}

macro_rules! impl_readable {
	($t:ty) => {
		impl Readable for $t {
			#[inline]
			fn read_from<R: Read + ?Sized>(reader: &mut R) -> Result<Self> {
				let mut byte = [0; size_of::<Self>()];
				reader.read_exact(&mut byte)?;
				Ok(Self::from_le_bytes(byte))
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

pub trait ReadBytesExt: Read {
	fn read_as<T: Readable>(&mut self) -> Result<T> {
		T::read_from(self)
	}
}

impl<R: Read + ?Sized> ReadBytesExt for R {}

pub trait Writeable {
	fn write_to<W: Write + ?Sized>(&self, writer: &mut W) -> Result<()>;
}

impl Writeable for u8 {
	#[inline]
	fn write_to<W: Write + ?Sized>(&self, writer: &mut W) -> Result<()> {
		writer.write_all(&[*self])
	}
}

impl Writeable for i8 {
	#[inline]
	fn write_to<W: Write + ?Sized>(&self, writer: &mut W) -> Result<()> {
		writer.write_all(&[*self as u8])
	}
}

macro_rules! impl_writeable {
	($t:ty) => {
		impl Writeable for $t {
			#[inline]
			fn write_to<W: Write + ?Sized>(&self, writer: &mut W) -> Result<()> {
				writer.write_all(&self.to_le_bytes())
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

pub trait WriteBytesExt: Write {
	fn write_as<T: Writeable>(&mut self, value: T) -> Result<()> {
		value.write_to(self)
	}
}

impl<W: Write + ?Sized> WriteBytesExt for W {}
