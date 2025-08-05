use crate::{gf, sdf, tf, vt};
use half::f16;
use std::collections::HashMap;

pub type Array<T> = Vec<T>;
pub type Dictionary = HashMap<String, vt::Value>;

#[derive(Debug, Clone)]
pub struct Value {
	store: ValueStore,
}

impl Value {
	pub fn new<T: ValueType>(value: T) -> Self {
		Value {
			store: value.store(),
		}
	}

	pub fn empty() -> Self {
		Value {
			store: ValueStore::Empty,
		}
	}

	pub fn is_empty(&self) -> bool {
		matches!(self.store, ValueStore::Empty)
	}

	pub fn get<T: ValueType>(&self) -> Option<T> {
		T::load(&self.store)
	}

	#[track_caller]
	pub fn get_unchecked<T: ValueType>(&self) -> T {
		T::load(&self.store).unwrap()
	}
}

#[derive(Debug, Clone)]
pub enum ValueStore {
	Empty,

	Bool(bool),
	BoolArray(Array<bool>),

	UChar(u8),
	UCharArray(Array<u8>),

	IntArray(Array<i32>),
	UIntArray(Array<u32>),

	Int(i32),
	Half(f16),
	HalfArray(Array<f16>),
	Float(f32),
	FloatArray(Array<f32>),
	Double(f64),
	DoubleArray(Array<f64>),

	Vec2i(gf::Vec2i),
	Vec2h(gf::Vec2h),
	Vec2f(gf::Vec2f),
	Vec2fArray(Array<gf::Vec2f>),
	Vec2d(gf::Vec2d),

	Vec3i(gf::Vec3i),
	Vec3h(gf::Vec3h),
	Vec3f(gf::Vec3f),
	Vec3fArray(Array<gf::Vec3f>),
	Vec3d(gf::Vec3d),

	Vec4i(gf::Vec4i),
	Vec4h(gf::Vec4h),
	Vec4f(gf::Vec4f),
	Vec4fArray(Array<gf::Vec4f>),
	Vec4d(gf::Vec4d),

	Quath(gf::Quath),
	Quatf(gf::Quatf),
	Quatd(gf::Quatd),

	Matrix2d(gf::Matrix2d),
	Matrix3d(gf::Matrix3d),
	Matrix4d(gf::Matrix4d),

	Token(tf::Token),
	TokenArray(Array<tf::Token>),

	String(String),
	StringArray(Array<String>),

	IntListOp(sdf::IntListOp),
	UIntListOp(sdf::UIntListOp),
	Int64ListOp(sdf::Int64ListOp),
	UInt64ListOp(sdf::UInt64ListOp),

	TokenListOp(sdf::TokenListOp),
	StringListOp(sdf::StringListOp),
	PathListOp(sdf::PathListOp),
	ReferenceListOp(sdf::ReferenceListOp),
	PayloadListOp(sdf::PayloadListOp),

	Path(sdf::Path),
	PathArray(Array<sdf::Path>),

	AssetPath(sdf::AssetPath),

	Specifier(sdf::Specifier),
	Variability(sdf::Variability),

	Dictionary(Dictionary),
}

pub trait ValueType {
	fn load(store: &ValueStore) -> Option<Self>
	where
		Self: Sized;
	fn store(self) -> ValueStore;
}

macro_rules! impl_value_type_clone {
	($type:ty, $store:ident) => {
		impl ValueType for $type {
			fn load(store: &ValueStore) -> Option<Self> {
				match store {
					ValueStore::$store(v) => Some(v.clone()),
					_ => None,
				}
			}

			fn store(self) -> ValueStore {
				ValueStore::$store(self)
			}
		}

		impl From<$type> for Value {
			fn from(value: $type) -> Self {
				Value {
					store: ValueStore::$store(value),
				}
			}
		}
	};
}

macro_rules! impl_value_type_deref {
	($type:ty, $store:ident) => {
		impl ValueType for $type {
			fn load(store: &ValueStore) -> Option<Self> {
				match store {
					ValueStore::$store(v) => Some(*v),
					_ => None,
				}
			}

			fn store(self) -> ValueStore {
				ValueStore::$store(self)
			}
		}

		impl From<$type> for Value {
			fn from(value: $type) -> Self {
				Value {
					store: ValueStore::$store(value),
				}
			}
		}
	};
}

impl_value_type_deref!(bool, Bool);
impl_value_type_clone!(Array<bool>, BoolArray);

impl_value_type_deref!(u8, UChar);
impl_value_type_clone!(Array<u8>, UCharArray);

impl_value_type_clone!(Array<i32>, IntArray);
impl_value_type_clone!(Array<u32>, UIntArray);

impl_value_type_clone!(tf::Token, Token);
impl_value_type_clone!(Array<tf::Token>, TokenArray);

impl_value_type_clone!(String, String);
impl_value_type_clone!(Array<String>, StringArray);

impl_value_type_clone!(sdf::IntListOp, IntListOp);
impl_value_type_clone!(sdf::UIntListOp, UIntListOp);
impl_value_type_clone!(sdf::Int64ListOp, Int64ListOp);
impl_value_type_clone!(sdf::UInt64ListOp, UInt64ListOp);

impl_value_type_clone!(sdf::Path, Path);
impl_value_type_clone!(sdf::AssetPath, AssetPath);

impl_value_type_deref!(i32, Int);
impl_value_type_deref!(f16, Half);
impl_value_type_clone!(Array<f16>, HalfArray);
impl_value_type_deref!(f32, Float);
impl_value_type_clone!(Array<f32>, FloatArray);
impl_value_type_deref!(f64, Double);
impl_value_type_clone!(Array<f64>, DoubleArray);

impl_value_type_deref!(gf::Vec2i, Vec2i);
impl_value_type_deref!(gf::Vec2h, Vec2h);
impl_value_type_deref!(gf::Vec2f, Vec2f);
impl_value_type_clone!(Array<gf::Vec2f>, Vec2fArray);
impl_value_type_deref!(gf::Vec2d, Vec2d);

impl_value_type_deref!(gf::Vec3i, Vec3i);
impl_value_type_deref!(gf::Vec3h, Vec3h);
impl_value_type_deref!(gf::Vec3f, Vec3f);
impl_value_type_clone!(Array<gf::Vec3f>, Vec3fArray);
impl_value_type_deref!(gf::Vec3d, Vec3d);

impl_value_type_deref!(gf::Vec4i, Vec4i);
impl_value_type_deref!(gf::Vec4h, Vec4h);
impl_value_type_deref!(gf::Vec4f, Vec4f);
impl_value_type_clone!(Array<gf::Vec4f>, Vec4fArray);
impl_value_type_deref!(gf::Vec4d, Vec4d);

impl_value_type_deref!(gf::Quath, Quath);
impl_value_type_deref!(gf::Quatf, Quatf);
impl_value_type_deref!(gf::Quatd, Quatd);

impl_value_type_deref!(gf::Matrix2d, Matrix2d);
impl_value_type_deref!(gf::Matrix3d, Matrix3d);
impl_value_type_deref!(gf::Matrix4d, Matrix4d);

impl_value_type_deref!(sdf::Specifier, Specifier);
impl_value_type_deref!(sdf::Variability, Variability);

impl_value_type_clone!(Dictionary, Dictionary);

impl_value_type_clone!(sdf::PathListOp, PathListOp);
