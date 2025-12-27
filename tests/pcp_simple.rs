/*use openusd_rs::{sdf, usd::Stage};

#[test]
fn inheritance() {
	let stage = Stage::open("tests/data/inheritance/entry.usd");

	let valid_paths = vec![
		"/",
		"/Root.first",
		"/Root.second",
		"/Inherit",
		"/Inherit.first",
	];

	for path in valid_paths {
		assert!(
			stage.prim_at_path(sdf::Path::from(path)).is_valid(),
			"Could not find {path}"
		);
	}

	let first_prim = stage.prim_at_path(sdf::Path::from("/Root.first"));
	let first_val = first_prim
		.metadata::<i32>(&sdf::FIELD_KEYS.default)
		.unwrap();

	let second_prim = stage.prim_at_path(sdf::Path::from("/Root.second"));
	let second_val = second_prim
		.metadata::<i32>(&sdf::FIELD_KEYS.default)
		.unwrap();

	assert_eq!(first_val, 99);
	assert_eq!(second_val, 24);
}

#[test]
fn layer_offsets() {
	let stage = Stage::open("tests/data/layer_offsets/entry.usd");
	let prim = stage.prim_at_path(sdf::Path::from("/Root.root"));
	let samples = prim
		.metadata::<sdf::TimeSampleMap>(&sdf::FIELD_KEYS.time_samples)
		.unwrap();

	assert_eq!(samples.len(), 3);

	let find_sample = |time: f64| -> Option<i32> {
		samples
			.iter()
			.find(|(t, _)| (t - time).abs() < 1e-6)
			.and_then(|(_, v)| v.get::<i32>())
	};

	assert_eq!(find_sample(-10.0), Some(100));
	assert_eq!(find_sample(0.0), Some(200));
	assert_eq!(find_sample(15.0), Some(300));

	// TODO: Check if layer offsets are applied correctly
}

#[test]
fn listop_ordering() {
	let stage = Stage::open("tests/data/listop_ordering/entry.usd");

	let valid_paths = vec![
		"/append",
		"/explicit_order",
		"/explicit_remove",
		"/append.identifier",
		"/explicit_order.identifier",
		"/explicit_remove.identifier",
	];

	for path in valid_paths {
		assert!(
			stage.prim_at_path(sdf::Path::from(path)).is_valid(),
			"Could not find {path}"
		);
	}

	let append_prim = stage.prim_at_path(sdf::Path::from("/append.identifier"));
	let append_val = append_prim
		.metadata::<String>(&sdf::FIELD_KEYS.default)
		.unwrap();

	let explicit_order_prim = stage.prim_at_path(sdf::Path::from("/explicit_order.identifier"));
	let explicit_order_val = explicit_order_prim
		.metadata::<String>(&sdf::FIELD_KEYS.default)
		.unwrap();

	let explicit_remove_prim = stage.prim_at_path(sdf::Path::from("/explicit_remove.identifier"));
	let explicit_remove_val = explicit_remove_prim
		.metadata::<String>(&sdf::FIELD_KEYS.default)
		.unwrap();

	assert_eq!(append_val, "B_from_Entry");
	assert_eq!(explicit_order_val, "A_from_Entry");
	assert_eq!(explicit_remove_val, "B_from_Entry");
}

#[test]
fn relocates() {
	let stage = Stage::open("tests/data/relocates/entry.usd");

	let valid_paths = vec![
		"/",
		"/Foo",
		"/Foo/Foo",
		"/Root",
		"/Root/ChildB",
		"/Root/Spam",
	];

	for path in valid_paths {
		assert!(
			stage.prim_at_path(sdf::Path::from(path)).is_valid(),
			"Could not find {path}"
		);
	}

	let invalid_paths = vec!["/Root/Foo", "/Root/ChildA"];

	for path in invalid_paths {
		assert!(
			!stage.prim_at_path(sdf::Path::from(path)).is_valid(),
			"Found unexpected path {path}"
		);
	}
}

#[test]
fn simple_sublayer() {
	let stage = Stage::open("tests/data/simple_sublayer/entry.usd");

	let valid_paths = vec![
		"/",
		"/RootBaseClass",
		"/RootBaseClass/FirstChild",
		"/RootBaseClass/FirstChild.fromFirstChild:RootBaseClass",
		"/RootBaseClass/InheritedChild",
		"/RootBaseClass.fromRootBaseClass",
		"/RootBaseClass.name",
		"/Root",
		"/Root/FirstChild",
		"/Root/FirstChild.fromFirstChild",
		"/Root/FirstChild.fromFirstChild:RootBaseClass",
		"/Root/FromSecondLayer",
		"/Root/FromSecondLayer.secondLayerAttr",
		"/Root/InheritedChild",
		"/Root/FromReference",
		"/Root.name",
		"/Root.fromRootBaseClass",
		"/WithVariants",
		"/WithVariants/Short",
		"/WithVariants.blue",
		"/WithVariants.red",
		"/Sleeping",
		"/FromSecond",
	];

	for path in valid_paths {
		assert!(
			stage.prim_at_path(sdf::Path::from(path)).is_valid(),
			"Could not find {path}"
		);
	}

	let invalid_paths = vec!["/Sleeping/SleepingChild"];

	for path in invalid_paths {
		assert!(
			!stage.prim_at_path(sdf::Path::from(path)).is_valid(),
			"Found unexpected path {path}"
		);
	}
}

#[test]
fn specializes() {
	let stage = Stage::open("tests/data/specializes/entry.usd");

	let valid_paths = vec![
		"/",
		"/Root",
		"/Root/Container",
		"/Root/Container/Base",
		"/Root/Container/Base.standard",
		"/Root/Container/Base.arc",
		"/Root/Container/Specialized",
		"/Root/Container/Specialized.arc",
		"/Root/Container/Specialized.standard",
		"/Root/Container/Inherits",
		"/Root/Container/Inherits.arc",
		"/Root/Container/Inherits.standard",
	];

	for path in valid_paths {
		assert!(
			stage.prim_at_path(sdf::Path::from(path)).is_valid(),
			"Could not find {path}"
		);
	}

	let base_prim = stage.prim_at_path(sdf::Path::from("/Root/Container/Base.arc"));
	let base_val = base_prim
		.metadata::<String>(&sdf::FIELD_KEYS.default)
		.unwrap();

	let specialized_prim = stage.prim_at_path(sdf::Path::from("/Root/Container/Specialized.arc"));
	let specialized_val = specialized_prim
		.metadata::<String>(&sdf::FIELD_KEYS.default)
		.unwrap();

	let inherits_prim = stage.prim_at_path(sdf::Path::from("/Root/Container/Inherits.arc"));
	let inherits_val = inherits_prim
		.metadata::<String>(&sdf::FIELD_KEYS.default)
		.unwrap();

	assert_eq!(base_val, "FromRoot");
	assert_eq!(specialized_val, "FromSpecialized");
	assert_eq!(inherits_val, "FromInherits");
}*/
