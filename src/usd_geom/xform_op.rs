use crate::{gf, tf, usd, usd_geom::TOKENS, vt};
use glam::{DMat4, DQuat, DVec3, dvec3};
use half::f16;

pub enum XformOpType {
	// Scalar
	TranslateX,
	TranslateY,
	TranslateZ,
	// Vec3
	Translate,

	// Scalar
	ScaleX,
	ScaleY,
	ScaleZ,
	// Vec3
	Scale,

	// Scalar
	RotateX,
	RotateY,
	RotateZ,

	// Vec3
	RotateXYZ,
	RotateXZY,
	RotateYXZ,
	RotateYZX,
	RotateZXY,
	RotateZYX,

	// Quat
	Orient,

	// Matrix4
	Transform,
}

impl TryFrom<&str> for XformOpType {
	type Error = ();
	fn try_from(s: &str) -> Result<Self, Self::Error> {
		Ok(match s {
			"translateX" => XformOpType::TranslateX,
			"translateY" => XformOpType::TranslateY,
			"translateZ" => XformOpType::TranslateZ,
			"translate" => XformOpType::Translate,
			"scaleX" => XformOpType::ScaleX,
			"scaleY" => XformOpType::ScaleY,
			"scaleZ" => XformOpType::ScaleZ,
			"scale" => XformOpType::Scale,
			"rotateX" => XformOpType::RotateX,
			"rotateY" => XformOpType::RotateY,
			"rotateZ" => XformOpType::RotateZ,
			"rotateXYZ" => XformOpType::RotateXYZ,
			"rotateXZY" => XformOpType::RotateXZY,
			"rotateYXZ" => XformOpType::RotateYXZ,
			"rotateYZX" => XformOpType::RotateYZX,
			"rotateZXY" => XformOpType::RotateZXY,
			"rotateZYX" => XformOpType::RotateZYX,
			"orient" => XformOpType::Orient,
			"transform" => XformOpType::Transform,
			_ => return Err(()),
		})
	}
}

/// Schema wrapper for UsdAttribute for authoring and computing
/// transformation operations, as consumed by UsdGeomXformable schema.
pub struct XformOp {}

impl XformOp {
	fn get_op_transform(op_type: XformOpType, value: vt::Value, is_inverse: bool) -> Option<DMat4> {
		use XformOpType::*;

		let get_scalar = || -> Option<f64> {
			value
				.get::<f64>()
				.or_else(|| value.get::<f32>().map(|v| v.into()))
				.or_else(|| value.get::<f16>().map(|v| v.into()))
		};

		let get_vec3 = || -> Option<DVec3> {
			value
				.get::<gf::Vec3d>()
				.or_else(|| value.get::<gf::Vec3f>().map(|v| v.into()))
				.or_else(|| value.get::<gf::Vec3h>().map(|v| v.into()))
				.map(|v| DVec3::new(v.x, v.y, v.z))
		};

		let get_quat = || -> Option<DQuat> {
			value
				.get::<gf::Quatd>()
				.or_else(|| value.get::<gf::Quatf>().map(|v| v.into()))
				.or_else(|| value.get::<gf::Quath>().map(|v| v.into()))
				.map(|v| DQuat::from_xyzw(v.i, v.j, v.k, v.w))
		};

		Some(match op_type {
			TranslateX if is_inverse => DMat4::from_translation(dvec3(-get_scalar()?, 0.0, 0.0)),
			TranslateY if is_inverse => DMat4::from_translation(dvec3(0.0, -get_scalar()?, 0.0)),
			TranslateZ if is_inverse => DMat4::from_translation(dvec3(0.0, 0.0, -get_scalar()?)),
			Translate if is_inverse => DMat4::from_translation(-get_vec3()?),

			TranslateX => DMat4::from_translation(dvec3(get_scalar()?, 0.0, 0.0)),
			TranslateY => DMat4::from_translation(dvec3(0.0, get_scalar()?, 0.0)),
			TranslateZ => DMat4::from_translation(dvec3(0.0, 0.0, get_scalar()?)),
			Translate => DMat4::from_translation(get_vec3()?),

			ScaleX if is_inverse => DMat4::from_scale(dvec3(1.0 / get_scalar()?, 1.0, 1.0)),
			ScaleY if is_inverse => DMat4::from_scale(dvec3(1.0, 1.0 / get_scalar()?, 1.0)),
			ScaleZ if is_inverse => DMat4::from_scale(dvec3(1.0, 1.0, 1.0 / get_scalar()?)),
			Scale if is_inverse => DMat4::from_scale(1.0 / get_vec3()?),

			ScaleX => DMat4::from_scale(dvec3(get_scalar()?, 1.0, 1.0)),
			ScaleY => DMat4::from_scale(dvec3(1.0, get_scalar()?, 1.0)),
			ScaleZ => DMat4::from_scale(dvec3(1.0, 1.0, get_scalar()?)),
			Scale => DMat4::from_scale(get_vec3()?),

			RotateX if is_inverse => DMat4::from_rotation_x(-get_scalar()?),
			RotateY if is_inverse => DMat4::from_rotation_y(-get_scalar()?),
			RotateZ if is_inverse => DMat4::from_rotation_z(-get_scalar()?),

			RotateX => DMat4::from_rotation_x(get_scalar()?),
			RotateY => DMat4::from_rotation_y(get_scalar()?),
			RotateZ => DMat4::from_rotation_z(get_scalar()?),

			RotateXYZ | RotateXZY | RotateYXZ | RotateYZX | RotateZXY | RotateZYX => {
				let vec = if is_inverse {
					-get_vec3()?
				} else {
					get_vec3()?
				};

				let rot_x = DQuat::from_axis_angle(DVec3::X, vec.x);
				let rot_y = DQuat::from_axis_angle(DVec3::Y, vec.y);
				let rot_z = DQuat::from_axis_angle(DVec3::Z, vec.z);

				let rot = match op_type {
					RotateXYZ if is_inverse => rot_z * rot_y * rot_x,
					RotateXZY if is_inverse => rot_y * rot_z * rot_x,
					RotateYXZ if is_inverse => rot_z * rot_x * rot_y,
					RotateYZX if is_inverse => rot_x * rot_z * rot_y,
					RotateZXY if is_inverse => rot_y * rot_x * rot_z,
					RotateZYX if is_inverse => rot_x * rot_y * rot_z,

					RotateXYZ => rot_x * rot_y * rot_z,
					RotateXZY => rot_x * rot_z * rot_y,
					RotateYXZ => rot_y * rot_x * rot_z,
					RotateYZX => rot_y * rot_z * rot_x,
					RotateZXY => rot_z * rot_x * rot_y,
					RotateZYX => rot_z * rot_y * rot_x,
					_ => unreachable!(),
				};

				DMat4::from_quat(rot)
			}
			Orient => DMat4::from_quat(if is_inverse {
				get_quat()?.inverse()
			} else {
				get_quat()?
			}),
			Transform => {
				let mat = value.get::<gf::Matrix4d>()?;
				// USD uses row-major, glam uses column-major, so transpose here
				let mat = DMat4::from_cols_array_2d(&mat.data).transpose();
				if is_inverse { mat.inverse() } else { mat }
			}
		})
	}

	pub fn get_local_matrix(prim: &usd::Prim) -> Option<gf::Matrix4d> {
		let mut transform = DMat4::IDENTITY;

		if !prim.has_attribute(&TOKENS.xform_op_order) {
			return None;
		}

		let op_order = prim
			.attribute(&TOKENS.xform_op_order)
			.get::<vt::Array<tf::Token>>();

		for op in op_order.iter().rev() {
			let (op, is_inverse) = op
				.as_str()
				.strip_prefix("!invert!")
				.map_or((op.as_str(), false), |s| (s, true));

			if let Some(op_type) = XformOpType::try_from(op.trim_start_matches("xformOp:")).ok()
				&& let Some(op_value) = prim.attribute(&tf::Token::new(op)).get_value()
				&& let Some(op_transform) = Self::get_op_transform(op_type, op_value, is_inverse)
			{
				transform *= op_transform;
			}
		}

		Some(gf::Matrix4d {
			data: transform.transpose().to_cols_array_2d(),
		})
	}

	pub fn get_local_transform(prim: &usd::Prim) -> Option<gf::Transform3d> {
		Self::get_local_matrix(prim).map(|m| m.into())
	}
}
