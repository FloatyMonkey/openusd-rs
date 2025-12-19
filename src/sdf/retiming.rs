/// Represents a time offset and scale between layers.
#[derive(Debug, Default, Clone)]
pub struct Retiming {
	pub offset: f64,
	pub scale: f64,
}

impl Retiming {
	pub fn inv(&self) -> Self {
		Self {
			offset: -self.offset / self.scale,
			scale: 1.0 / self.scale,
		}
	}
}

impl std::ops::Mul for Retiming {
	type Output = Self;

	fn mul(self, rhs: Self) -> Self {
		Self {
			offset: rhs.offset * self.scale + self.offset,
			scale: rhs.scale * self.scale,
		}
	}
}

impl std::ops::Mul<f64> for Retiming {
	type Output = f64;

	fn mul(self, rhs: f64) -> f64 {
		rhs * self.scale + self.offset
	}
}
