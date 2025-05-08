use half::f16;

#[repr(C)]
#[derive(Clone, Copy, Debug, Default)]
pub struct Vec2<T> {
	pub x: T,
	pub y: T,
}

impl<T> Vec2<T> {
	pub fn new(x: T, y: T) -> Self {
		Self { x, y }
	}
}

#[repr(C)]
#[derive(Clone, Copy, Debug, Default)]
pub struct Vec3<T> {
	pub x: T,
	pub y: T,
	pub z: T,
}

impl<T> Vec3<T> {
	pub fn new(x: T, y: T, z: T) -> Self {
		Self { x, y, z }
	}
}

#[repr(C)]
#[derive(Clone, Copy, Debug, Default)]
pub struct Vec4<T> {
	pub x: T,
	pub y: T,
	pub z: T,
	pub w: T,
}

impl<T> Vec4<T> {
	pub fn new(x: T, y: T, z: T, w: T) -> Self {
		Self { x, y, z, w }
	}
}

pub type Vec2h = Vec2<f16>;
pub type Vec2f = Vec2<f32>;
pub type Vec2d = Vec2<f64>;
pub type Vec2i = Vec2<i32>;

pub type Vec3h = Vec3<f16>;
pub type Vec3f = Vec3<f32>;
pub type Vec3d = Vec3<f64>;
pub type Vec3i = Vec3<i32>;

pub type Vec4h = Vec4<f16>;
pub type Vec4f = Vec4<f32>;
pub type Vec4d = Vec4<f64>;
pub type Vec4i = Vec4<i32>;

impl From<Vec3h> for Vec3d {
	fn from(v: Vec3h) -> Self {
		Self {
			x: v.x.into(),
			y: v.y.into(),
			z: v.z.into(),
		}
	}
}

impl From<Vec3f> for Vec3d {
	fn from(v: Vec3f) -> Self {
		Self {
			x: v.x.into(),
			y: v.y.into(),
			z: v.z.into(),
		}
	}
}

#[repr(C)]
#[derive(Clone, Copy, Debug, Default)]
pub struct Quat<T> {
	pub i: T,
	pub j: T,
	pub k: T,
	pub w: T,
}

pub type Quath = Quat<f16>;
pub type Quatf = Quat<f32>;
pub type Quatd = Quat<f64>;

impl From<Quath> for Quatd {
	fn from(q: Quath) -> Self {
		Self {
			i: q.i.into(),
			j: q.j.into(),
			k: q.k.into(),
			w: q.w.into(),
		}
	}
}

impl From<Quatf> for Quatd {
	fn from(q: Quatf) -> Self {
		Self {
			i: q.i.into(),
			j: q.j.into(),
			k: q.k.into(),
			w: q.w.into(),
		}
	}
}

#[repr(C)]
#[derive(Clone, Copy, Debug, Default)]
pub struct Matrix2d {
	pub data: [[f64; 2]; 2],
}

impl Matrix2d {
	pub fn from_diagonal(diagonal: Vec2d) -> Self {
		Self {
			data: [[diagonal.x, 0.0], [0.0, diagonal.y]],
		}
	}
}

#[repr(C)]
#[derive(Clone, Copy, Debug, Default)]
pub struct Matrix3d {
	pub data: [[f64; 3]; 3],
}

impl Matrix3d {
	pub fn from_diagonal(diagonal: Vec3d) -> Self {
		Self {
			data: [
				[diagonal.x, 0.0, 0.0],
				[0.0, diagonal.y, 0.0],
				[0.0, 0.0, diagonal.z],
			],
		}
	}
}

#[repr(C)]
#[derive(Clone, Copy, Debug, Default)]
pub struct Matrix4d {
	pub data: [[f64; 4]; 4],
}

impl Matrix4d {
	pub fn from_diagonal(diagonal: Vec4d) -> Self {
		Self {
			data: [
				[diagonal.x, 0.0, 0.0, 0.0],
				[0.0, diagonal.y, 0.0, 0.0],
				[0.0, 0.0, diagonal.z, 0.0],
				[0.0, 0.0, 0.0, diagonal.w],
			],
		}
	}
}

// Not an official USD type, but useful for external interop.
#[repr(C)]
#[derive(Clone, Copy, Debug, Default)]
pub struct Transform3d {
	pub translation: Vec3d,
	pub rotation: Quatd,
	pub scale: Vec3d,
}
