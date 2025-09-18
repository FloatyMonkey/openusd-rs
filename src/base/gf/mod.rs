//! Graphics Foundations

use half::f16;

use std::ops::{AddAssign, Index, IndexMut, Mul, MulAssign, SubAssign};

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
	// ---- Constructors ----
	pub fn post_mult(&self, rhs: &Matrix4d) -> Matrix4d {
		let mut out = [[0.0f64; 4]; 4];
		for i in 0..4 {
			for j in 0..4 {
				out[i][j] = (0..4).map(|k| self[i][k] * rhs[k][j]).sum();
			}
		}
		Matrix4d::from_array(out)
	}

	pub fn pre_mult(&self, lhs: &Matrix4d) -> Matrix4d {
		lhs.post_mult(self)
	}

	pub fn new() -> Self {
		Self::default()
	}
	pub fn as_array(&self) -> &[[f64; 4]; 4] {
		&self.data
	}
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

	#[allow(clippy::too_many_arguments)]
	pub fn from_elements(
		m00: f64,
		m01: f64,
		m02: f64,
		m03: f64,
		m10: f64,
		m11: f64,
		m12: f64,
		m13: f64,
		m20: f64,
		m21: f64,
		m22: f64,
		m23: f64,
		m30: f64,
		m31: f64,
		m32: f64,
		m33: f64,
	) -> Self {
		Self {
			data: [
				[m00, m01, m02, m03],
				[m10, m11, m12, m13],
				[m20, m21, m22, m23],
				[m30, m31, m32, m33],
			],
		}
	}

	pub fn from_array(m: [[f64; 4]; 4]) -> Self {
		Self { data: m }
	}

	pub fn from_scalar(s: f64) -> Self {
		let mut out = Self::zero();
		out.data[0][0] = s;
		out.data[1][1] = s;
		out.data[2][2] = s;
		out.data[3][3] = s;
		out
	}

	pub fn from_rows(r0: Vec4d, r1: Vec4d, r2: Vec4d, r3: Vec4d) -> Self {
		Self {
			data: [
				[r0.x, r0.y, r0.z, r0.w],
				[r1.x, r1.y, r1.z, r1.w],
				[r2.x, r2.y, r2.z, r2.w],
				[r3.x, r3.y, r3.z, r3.w],
			],
		}
	}

	pub fn from_cols(c0: Vec4d, c1: Vec4d, c2: Vec4d, c3: Vec4d) -> Self {
		Self {
			data: [
				[c0.x, c1.x, c2.x, c3.x],
				[c0.y, c1.y, c2.y, c3.y],
				[c0.z, c1.z, c2.z, c3.z],
				[c0.w, c1.w, c2.w, c3.w],
			],
		}
	}

	pub fn from_rot_trans(rot3: [[f64; 3]; 3], t: Vec3d) -> Self {
		Self {
			data: [
				[rot3[0][0], rot3[0][1], rot3[0][2], t.x],
				[rot3[1][0], rot3[1][1], rot3[1][2], t.y],
				[rot3[2][0], rot3[2][1], rot3[2][2], t.z],
				[0.0, 0.0, 0.0, 1.0],
			],
		}
	}

	// ---- Setters / Getters ----
	pub fn set_row(&mut self, i: usize, v: Vec4d) {
		self.data[i] = [v.x, v.y, v.z, v.w];
	}

	pub fn set_column(&mut self, j: usize, v: Vec4d) {
		for i in 0..4 {
			self.data[i][j] = [v.x, v.y, v.z, v.w][i];
		}
	}

	pub fn row(&self, i: usize) -> Vec4d {
		let r = self.data[i];
		Vec4d::new(r[0], r[1], r[2], r[3])
	}

	pub fn column(&self, j: usize) -> Vec4d {
		Vec4d::new(
			self.data[0][j],
			self.data[1][j],
			self.data[2][j],
			self.data[3][j],
		)
	}

	#[allow(clippy::too_many_arguments)]
	pub fn set(
		&mut self,
		m00: f64,
		m01: f64,
		m02: f64,
		m03: f64,
		m10: f64,
		m11: f64,
		m12: f64,
		m13: f64,
		m20: f64,
		m21: f64,
		m22: f64,
		m23: f64,
		m30: f64,
		m31: f64,
		m32: f64,
		m33: f64,
	) -> &mut Self {
		*self = Self::from_elements(
			m00, m01, m02, m03, m10, m11, m12, m13, m20, m21, m22, m23, m30, m31, m32, m33,
		);
		self
	}

	pub fn set_array(&mut self, m: [[f64; 4]; 4]) -> &mut Self {
		self.data = m;
		self
	}

	pub fn identity() -> Self {
		Self {
			data: [
				[1.0, 0.0, 0.0, 0.0],
				[0.0, 1.0, 0.0, 0.0],
				[0.0, 0.0, 1.0, 0.0],
				[0.0, 0.0, 0.0, 1.0],
			],
		}
	}

	pub fn set_identity(&mut self) -> &mut Self {
		*self = Self::identity();
		self
	}

	pub fn zero() -> Self {
		Self {
			data: [[0.0; 4]; 4],
		}
	}

	pub fn set_zero(&mut self) -> &mut Self {
		*self = Self::zero();
		self
	}

	pub fn set_diagonal_scalar(&mut self, s: f64) -> &mut Self {
		*self = Self::from_scalar(s);
		self
	}

	pub fn set_diagonal_vec(&mut self, v: Vec4d) -> &mut Self {
		*self = Self::from_diagonal(v);
		self
	}

	pub fn get_into<'a>(&self, out: &'a mut [[f64; 4]; 4]) -> &'a [[f64; 4]; 4] {
		*out = self.data;
		out
	}

	pub fn as_ptr(&self) -> *const f64 {
		self.data.as_ptr() as *const f64
	}

	pub fn as_mut_ptr(&mut self) -> *mut f64 {
		self.data.as_mut_ptr() as *mut f64
	}

	// ---- Equality ----
	pub fn eq_matrix(&self, other: &Self) -> bool {
		self.data == other.data
	}

	// ---- Linear algebra ----
	pub fn transpose(&self) -> Self {
		let mut t = Self::zero();
		for i in 0..4 {
			for j in 0..4 {
				t.data[i][j] = self.data[j][i];
			}
		}
		t
	}

	pub fn determinant(&self) -> f64 {
		let m = &self.data;
		let det3 =
			|a: f64, b: f64, c: f64, d: f64, e: f64, f_: f64, g: f64, h: f64, i: f64| -> f64 {
				a * (e * i - f_ * h) - b * (d * i - f_ * g) + c * (d * h - e * g)
			};

		m[0][0]
			* det3(
				m[1][1], m[1][2], m[1][3], m[2][1], m[2][2], m[2][3], m[3][1], m[3][2], m[3][3],
			) - m[0][1]
			* det3(
				m[1][0], m[1][2], m[1][3], m[2][0], m[2][2], m[2][3], m[3][0], m[3][2], m[3][3],
			) + m[0][2]
			* det3(
				m[1][0], m[1][1], m[1][3], m[2][0], m[2][1], m[2][3], m[3][0], m[3][1], m[3][3],
			) - m[0][3]
			* det3(
				m[1][0], m[1][1], m[1][2], m[2][0], m[2][1], m[2][2], m[3][0], m[3][1], m[3][2],
			)
	}

	pub fn inverse(&self, det_out: Option<&mut f64>, eps: f64) -> Option<Self> {
		let det = self.determinant();
		if let Some(d) = det_out {
			*d = det;
		}
		if det.abs() <= eps {
			return None;
		}

		let m = &self.data;
		let c = |r: usize, c: usize| -> f64 {
			let mut v = [0.0; 9];
			let mut idx = 0;
			for i in 0..4 {
				if i == r {
					continue;
				}
				for j in 0..4 {
					if j == c {
						continue;
					}
					v[idx] = m[i][j];
					idx += 1;
				}
			}
			let det3 =
				|a: f64, b: f64, c: f64, d: f64, e: f64, f_: f64, g: f64, h: f64, i: f64| -> f64 {
					a * (e * i - f_ * h) - b * (d * i - f_ * g) + c * (d * h - e * g)
				};
			let s = if (r + c) % 2 == 0 { 1.0 } else { -1.0 };
			s * det3(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8])
		};
		let mut adj_t = [[0.0; 4]; 4];
		for i in 0..4 {
			for j in 0..4 {
				adj_t[i][j] = c(j, i);
			}
		}
		let inv = adj_t.map(|row| row.map(|x| x / det));
		Some(Self { data: inv })
	}

	pub fn set_row3(&mut self, i: usize, v: Vec3d) {
		self.data[i][0] = v.x;
		self.data[i][1] = v.y;
		self.data[i][2] = v.z;
	}

	pub fn row3(&self, i: usize) -> Vec3d {
		Vec3d::new(self.data[i][0], self.data[i][1], self.data[i][2])
	}

	pub fn determinant3(&self) -> f64 {
		let a = self.data;
		let (a00, a01, a02) = (a[0][0], a[0][1], a[0][2]);
		let (a10, a11, a12) = (a[1][0], a[1][1], a[1][2]);
		let (a20, a21, a22) = (a[2][0], a[2][1], a[2][2]);
		a00 * (a11 * a22 - a12 * a21) - a01 * (a10 * a22 - a12 * a20)
			+ a02 * (a10 * a21 - a11 * a20)
	}

	pub fn has_orthogonal_rows3(&self, tol: f64) -> bool {
		let r0 = self.row3(0);
		let r1 = self.row3(1);
		let r2 = self.row3(2);
		let dot = |a: Vec3d, b: Vec3d| a.x * b.x + a.y * b.y + a.z * b.z;
		let nrm2 = |a: Vec3d| dot(a, a);
		(dot(r0, r1).abs() <= tol)
			&& (dot(r0, r2).abs() <= tol)
			&& (dot(r1, r2).abs() <= tol)
			&& ((nrm2(r0) - 1.0).abs() <= tol)
			&& ((nrm2(r1) - 1.0).abs() <= tol)
			&& ((nrm2(r2) - 1.0).abs() <= tol)
	}

	pub fn orthonormalize(&mut self) {
		let mut c0 = self.column(0);
		let mut c1 = self.column(1);
		let mut c2 = self.column(2);
		let mut v0 = Vec3d::new(c0.x, c0.y, c0.z);
		let mut v1 = Vec3d::new(c1.x, c1.y, c1.z);
		let mut v2 = Vec3d::new(c2.x, c2.y, c2.z);

		normalize(&mut v0);
		v1 = sub(v1, scale(v0, dot(v1, v0)));
		normalize(&mut v1);
		v2 = sub(v2, scale(v0, dot(v2, v0)));
		v2 = sub(v2, scale(v1, dot(v2, v1)));
		normalize(&mut v2);

		self.set_column(0, Vec4d::new(v0.x, v0.y, v0.z, c0.w));
		self.set_column(1, Vec4d::new(v1.x, v1.y, v1.z, c1.w));
		self.set_column(2, Vec4d::new(v2.x, v2.y, v2.z, c2.w));
	}

	pub fn orthonormalized(&self) -> Self {
		let mut m = *self;
		m.orthonormalize();
		m
	}

	pub fn handedness(&self) -> f64 {
		let d = self.determinant3();
		if d >= 0.0 {
			1.0
		} else {
			-1.0
		}
	}

	pub fn is_right_handed(&self) -> bool {
		self.determinant3() > 0.0
	}
	pub fn is_left_handed(&self) -> bool {
		self.determinant3() < 0.0
	}

	pub fn post_mul_assign(&mut self, m: &Self) {
		*self *= *m;
	}

	pub fn scale_assign(&mut self, s: f64) {
		for i in 0..4 {
			for j in 0..4 {
				self.data[i][j] *= s;
			}
		}
	}

	pub fn add_assign(&mut self, m: &Self) {
		for i in 0..4 {
			for j in 0..4 {
				self.data[i][j] += m.data[i][j];
			}
		}
	}

	pub fn sub_assign(&mut self, m: &Self) {
		for i in 0..4 {
			for j in 0..4 {
				self.data[i][j] -= m.data[i][j];
			}
		}
	}

	pub fn set_scale(&mut self, s: f64) -> &mut Self {
		*self = Self::from_scalar(1.0);
		self.data[0][0] = s;
		self.data[1][1] = s;
		self.data[2][2] = s;
		self
	}

	pub fn remove_scale_shear(&self) -> Self {
		let mut r = *self;
		r.orthonormalize();
		r
	}
}

// ---- Indexing like m[i][j] ----
impl Index<usize> for Matrix4d {
	type Output = [f64; 4];
	fn index(&self, i: usize) -> &Self::Output {
		&self.data[i]
	}
}
impl IndexMut<usize> for Matrix4d {
	fn index_mut(&mut self, i: usize) -> &mut Self::Output {
		&mut self.data[i]
	}
}

// ---- Operators ----
impl PartialEq for Matrix4d {
	fn eq(&self, other: &Self) -> bool {
		self.data == other.data
	}
}

impl MulAssign<Matrix4d> for Matrix4d {
	fn mul_assign(&mut self, rhs: Matrix4d) {
		let mut out = [[0.0; 4]; 4];
		for i in 0..4 {
			for j in 0..4 {
				out[i][j] = self.data[i][0] * rhs.data[0][j]
					+ self.data[i][1] * rhs.data[1][j]
					+ self.data[i][2] * rhs.data[2][j]
					+ self.data[i][3] * rhs.data[3][j];
			}
		}
		self.data = out;
	}
}

impl AddAssign<Matrix4d> for Matrix4d {
	fn add_assign(&mut self, rhs: Matrix4d) {
		self.add_assign(&rhs);
	}
}
impl SubAssign<Matrix4d> for Matrix4d {
	fn sub_assign(&mut self, rhs: Matrix4d) {
		self.sub_assign(&rhs);
	}
}
impl MulAssign<f64> for Matrix4d {
	fn mul_assign(&mut self, rhs: f64) {
		self.scale_assign(rhs);
	}
}
impl Mul<f64> for Matrix4d {
	type Output = Matrix4d;
	fn mul(mut self, rhs: f64) -> Matrix4d {
		self *= rhs;
		self
	}
}

// ---- Small helpers ----
#[inline]
fn dot(a: Vec3d, b: Vec3d) -> f64 {
	a.x * b.x + a.y * b.y + a.z * b.z
}
#[inline]
fn scale(a: Vec3d, s: f64) -> Vec3d {
	Vec3d::new(a.x * s, a.y * s, a.z * s)
}
#[inline]
fn sub(a: Vec3d, b: Vec3d) -> Vec3d {
	Vec3d::new(a.x - b.x, a.y - b.y, a.z - b.z)
}
#[inline]
fn normalize(v: &mut Vec3d) {
	let n = (dot(*v, *v)).sqrt();
	if n > 0.0 {
		v.x /= n;
		v.y /= n;
		v.z /= n;
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
