use super::Array;
use crate::{gf, sdf, tf, vt};
use half::f16;
use std::collections::HashMap;

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

pub trait ValueType {
	fn load(store: &ValueStore) -> Option<Self>
	where
		Self: Sized;
	fn store(self) -> ValueStore;
}

macro_rules! impl_value_type {
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

macro_rules! def_value_types {
	($( ($t:ty, $v:ident $(, $va:ident)? ) ),* $(,)? ) => {
		#[derive(Debug, Clone)]
		pub enum ValueStore {
			Empty,
			$(
				$v($t),
				$( $va(Array<$t>), )?
			)*
		}

		$(
			impl_value_type!($t, $v);
			$( impl_value_type!(Array<$t>, $va); )?
		)*
	};
}

def_value_types!(
	(bool, Bool, BoolArray),
	(u8, UChar, UCharArray),
	(i32, Int, IntArray),
	(u32, UInt, UIntArray),
	(i64, Int64, Int64Array),
	(u64, UInt64, UInt64Array),
	(f16, Half, HalfArray),
	(f32, Float, FloatArray),
	(f64, Double, DoubleArray),
	//
	(tf::Token, Token, TokenArray),
	(String, String, StringArray),
	(sdf::Path, Path, PathArray),
	(sdf::AssetPath, AssetPath, AssetPathArray),
	//
	(gf::Vec2i, Vec2i, Vec2iArray),
	(gf::Vec2h, Vec2h, Vec2hArray),
	(gf::Vec2f, Vec2f, Vec2fArray),
	(gf::Vec2d, Vec2d, Vec2dArray),
	//
	(gf::Vec3i, Vec3i, Vec3iArray),
	(gf::Vec3h, Vec3h, Vec3hArray),
	(gf::Vec3f, Vec3f, Vec3fArray),
	(gf::Vec3d, Vec3d, Vec3dArray),
	//
	(gf::Vec4i, Vec4i, Vec4iArray),
	(gf::Vec4h, Vec4h, Vec4hArray),
	(gf::Vec4f, Vec4f, Vec4fArray),
	(gf::Vec4d, Vec4d, Vec4dArray),
	//
	(gf::Quath, Quath, QuathArray),
	(gf::Quatf, Quatf, QuatfArray),
	(gf::Quatd, Quatd, QuatdArray),
	//
	(gf::Matrix2d, Matrix2d, Matrix2dArray),
	(gf::Matrix3d, Matrix3d, Matrix3dArray),
	(gf::Matrix4d, Matrix4d, Matrix4dArray),
	//
	(sdf::IntListOp, IntListOp),
	(sdf::UIntListOp, UIntListOp),
	(sdf::Int64ListOp, Int64ListOp),
	(sdf::UInt64ListOp, UInt64ListOp),
	//
	(sdf::TokenListOp, TokenListOp),
	(sdf::StringListOp, StringListOp),
	(sdf::PathListOp, PathListOp),
	(sdf::ReferenceListOp, ReferenceListOp),
	(sdf::PayloadListOp, PayloadListOp),
	//
	(sdf::Specifier, Specifier),
	(sdf::Variability, Variability),
	(sdf::Retiming, Retiming, RetimingArray),
	(sdf::Relocate, Relocate, RelocateArray),
	//
	(sdf::ValueBlock, ValueBlock),
	//
	(Dictionary, Dictionary),
	(sdf::TimeSampleMap, TimeSampleMap),
	(sdf::VariantSelectionMap, VariantSelectionMap)
);
