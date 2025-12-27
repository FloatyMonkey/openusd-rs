/*use openusd_rs::{sdf, usd::Stage};

fn run_pcp_test(test_dir: &str) {
	let test_path = std::path::Path::new("tests/data").join(test_dir);
	let json_path = test_path.join("pcp.json");

	let file = std::fs::File::open(json_path).unwrap();
	let json: serde_json::Value = serde_json::from_reader(file).unwrap();

	if json.get("Errors").is_some() {
		println!("Skipping test {} due to expected errors", test_dir);
		return;
	}

	let stage_path = test_path.join(json.get("Entry").unwrap().as_str().unwrap());
	let stage = Stage::open(stage_path);

	let composing = json.get("Composing").unwrap().as_object().unwrap();

	for (prim_path_str, data) in composing {
		let prim_path = sdf::Path::from(prim_path_str.as_str());
		let prim = stage.prim_at_path(prim_path.clone());
		assert!(prim.is_valid(), "Prim at {prim_path_str} should be valid");
		println!("Prim: {prim_path_str}");

		// Child names
		if let Some(expected_children) = data.get("Child names").and_then(|v| v.as_array()) {
			let mut actual_children: Vec<String> =
				prim.children().map(|c| c.name().to_string()).collect();
			actual_children.sort();

			let mut expected_children_vec: Vec<String> = expected_children
				.iter()
				.map(|v| v.as_str().unwrap().to_string())
				.collect();
			expected_children_vec.sort();

			assert_eq!(
				actual_children, expected_children_vec,
				"Children mismatch for {prim_path_str}",
			);

			println!("  Children: {actual_children:?}");
		}

		// Property names
		if let Some(expected_props) = data.get("Property names").and_then(|v| v.as_array()) {
			let mut actual_props: Vec<String> =
				prim.properties().map(|p| p.name().to_string()).collect();
			actual_props.sort();

			let mut expected_props_vec: Vec<String> = expected_props
				.iter()
				.map(|v| v.as_str().unwrap().to_string())
				.collect();
			expected_props_vec.sort();

			assert_eq!(
				actual_props, expected_props_vec,
				"Properties mismatch for {prim_path_str}",
			);

			println!("  Properties: {actual_props:?}");
		}

		// Variant selections
		if let Some(expected_variants) = data.get("Variant Selections").and_then(|v| v.as_object())
		{
			let actual_variants = prim.variant_selections();

			for (variant_set, expected_variant_value) in expected_variants {
				let expected_variant = expected_variant_value.as_str().unwrap();
				match actual_variants.get(variant_set) {
					Some(actual_variant) => {
						assert_eq!(
							actual_variant, expected_variant,
							"Variant selection mismatch for {}:{} in {}",
							variant_set, expected_variant, prim_path_str
						);
					}
					None => {
						panic!(
							"Missing variant selection {}:{} in {}",
							variant_set, expected_variant, prim_path_str
						);
					}
				}
			}

			println!("  Variants: {actual_variants:?}");
		}
	}
}

#[test]
fn basic_ancestral_reference() {
	run_pcp_test("BasicAncestralReference_root");
}

#[test]
fn basic_duplicate_sublayer() {
	run_pcp_test("BasicDuplicateSublayer_root");
}

#[test]
fn basic_inherits() {
	run_pcp_test("BasicInherits_root");
}

#[test]
fn basic_instancing_and_nested_instances() {
	run_pcp_test("BasicInstancingAndNestedInstances_root");
}

#[test]
fn basic_instancing_and_variants() {
	run_pcp_test("BasicInstancingAndVariants_root");
}

#[test]
fn basic_instancing() {
	run_pcp_test("BasicInstancing_root");
}

#[test]
fn basic_list_editing_with_inherits() {
	run_pcp_test("BasicListEditingWithInherits_root");
}

#[test]
fn basic_list_editing() {
	run_pcp_test("BasicListEditing_root");
}

#[test]
fn basic_local_and_global_class_combination() {
	run_pcp_test("BasicLocalAndGlobalClassCombination_root");
}

#[test]
fn basic_nested_payload() {
	run_pcp_test("BasicNestedPayload_root");
}

#[test]
fn basic_nested_variants_with_same_name() {
	run_pcp_test("BasicNestedVariantsWithSameName_root");
}

#[test]
fn basic_nested_variants() {
	run_pcp_test("BasicNestedVariants_root");
}

#[test]
fn basic_owner() {
	run_pcp_test("BasicOwner_root");
}

#[test]
fn basic_payload_diamond() {
	run_pcp_test("BasicPayloadDiamond_root");
}

#[test]
fn basic_payload() {
	run_pcp_test("BasicPayload_root");
}

#[test]
fn basic_reference_and_class_diamond() {
	run_pcp_test("BasicReferenceAndClassDiamond_root");
}

#[test]
fn basic_reference_and_class() {
	run_pcp_test("BasicReferenceAndClass_root");
}

#[test]
fn basic_reference_diamond() {
	run_pcp_test("BasicReferenceDiamond_root");
}

#[test]
fn basic_relocate_to_anim_interface_as_new_root_prim() {
	run_pcp_test("BasicRelocateToAnimInterfaceAsNewRootPrim_root");
}

#[test]
fn basic_relocate_to_anim_interface() {
	run_pcp_test("BasicRelocateToAnimInterface_root");
}

#[test]
fn basic_specializes_and_inherits() {
	run_pcp_test("BasicSpecializesAndInherits_root");
}

#[test]
fn basic_specializes_and_references() {
	run_pcp_test("BasicSpecializesAndReferences_root");
}

#[test]
fn basic_specializes_and_variants() {
	run_pcp_test("BasicSpecializesAndVariants_root");
}

#[test]
fn basic_specializes() {
	run_pcp_test("BasicSpecializes_root");
}

#[test]
fn basic_time_offset() {
	run_pcp_test("BasicTimeOffset_root");
}

#[test]
fn basic_variant_with_connections() {
	run_pcp_test("BasicVariantWithConnections_root");
}

#[test]
fn basic_variant_with_reference() {
	run_pcp_test("BasicVariantWithReference_root");
}

#[test]
fn bug69932() {
	run_pcp_test("bug69932_root");
}

#[test]
fn bug74847() {
	run_pcp_test("bug74847_root");
}

#[test]
fn bug92827() {
	run_pcp_test("bug92827_root");
}

#[test]
fn case1() {
	run_pcp_test("case1_root");
}

#[test]
fn elided_ancestral_relocates() {
	run_pcp_test("ElidedAncestralRelocates_root");
}

#[test]
fn error_arc_cycle() {
	run_pcp_test("ErrorArcCycle_root");
}

#[test]
fn error_connection_permission_denied() {
	run_pcp_test("ErrorConnectionPermissionDenied_root");
}

#[test]
fn error_inconsistent_properties() {
	run_pcp_test("ErrorInconsistentProperties_root");
}

#[test]
fn error_invalid_authored_relocates() {
	run_pcp_test("ErrorInvalidAuthoredRelocates_root");
}

#[test]
fn error_invalid_conflicting_relocates() {
	run_pcp_test("ErrorInvalidConflictingRelocates_root");
}

#[test]
fn error_invalid_instance_target_path() {
	run_pcp_test("ErrorInvalidInstanceTargetPath_root");
}

#[test]
fn error_invalid_payload() {
	run_pcp_test("ErrorInvalidPayload_root");
}

#[test]
fn error_invalid_pre_relocate_target_path() {
	run_pcp_test("ErrorInvalidPreRelocateTargetPath_root");
}

#[test]
fn error_invalid_reference_to_relocation_source() {
	run_pcp_test("ErrorInvalidReferenceToRelocationSource_root");
}

#[test]
fn error_invalid_target_path() {
	run_pcp_test("ErrorInvalidTargetPath_root");
}

#[test]
fn error_opinion_at_relocation_source() {
	run_pcp_test("ErrorOpinionAtRelocationSource_root");
}

#[test]
fn error_owner() {
	run_pcp_test("ErrorOwner_root");
}

#[test]
fn error_permission_denied() {
	run_pcp_test("ErrorPermissionDenied_root");
}

#[test]
fn error_relocate_with_variant_selection() {
	run_pcp_test("ErrorRelocateWithVariantSelection_root");
}

#[test]
fn error_sublayer_cycle() {
	run_pcp_test("ErrorSublayerCycle_root");
}

#[test]
fn expressions_in_payloads() {
	run_pcp_test("ExpressionsInPayloads_root");
}

#[test]
fn expressions_in_references() {
	run_pcp_test("ExpressionsInReferences_root");
}

#[test]
fn implied_and_ancestral_inherits_complex_evaluation() {
	run_pcp_test("ImpliedAndAncestralInherits_ComplexEvaluation_root");
}

#[test]
fn implied_and_ancestral_inherits() {
	run_pcp_test("ImpliedAndAncestralInherits_root");
}

#[test]
fn payloads_and_ancestral_arcs2() {
	run_pcp_test("PayloadsAndAncestralArcs2_root");
}

#[test]
fn payloads_and_ancestral_arcs3() {
	run_pcp_test("PayloadsAndAncestralArcs3_root");
}

#[test]
fn payloads_and_ancestral_arcs() {
	run_pcp_test("PayloadsAndAncestralArcs_root");
}

#[test]
fn reference_list_ops_with_offsets() {
	run_pcp_test("ReferenceListOpsWithOffsets_root");
}

#[test]
fn relative_path_payloads() {
	run_pcp_test("RelativePathPayloads_root");
}

#[test]
fn relative_path_references() {
	run_pcp_test("RelativePathReferences_root");
}

#[test]
fn relocate_prims_with_same_name() {
	run_pcp_test("RelocatePrimsWithSameName_root");
}

#[test]
fn relocate_to_none() {
	run_pcp_test("RelocateToNone_root");
}

#[test]
fn specializes_and_ancestral_arcs2() {
	run_pcp_test("SpecializesAndAncestralArcs2_root");
}

#[test]
fn specializes_and_ancestral_arcs3() {
	run_pcp_test("SpecializesAndAncestralArcs3_root");
}

#[test]
fn specializes_and_ancestral_arcs4() {
	run_pcp_test("SpecializesAndAncestralArcs4_root");
}

#[test]
fn specializes_and_ancestral_arcs5() {
	run_pcp_test("SpecializesAndAncestralArcs5_root");
}

#[test]
fn specializes_and_ancestral_arcs() {
	run_pcp_test("SpecializesAndAncestralArcs_root");
}

#[test]
fn specializes_and_variants2() {
	run_pcp_test("SpecializesAndVariants2_root");
}

#[test]
fn specializes_and_variants3() {
	run_pcp_test("SpecializesAndVariants3_root");
}

#[test]
fn specializes_and_variants4() {
	run_pcp_test("SpecializesAndVariants4_root");
}

#[test]
fn specializes_and_variants() {
	run_pcp_test("SpecializesAndVariants_root");
}

#[test]
fn subroot_inherits_and_variants() {
	run_pcp_test("SubrootInheritsAndVariants_root");
}

#[test]
fn subroot_reference_and_classes() {
	run_pcp_test("SubrootReferenceAndClasses_root");
}

#[test]
fn subroot_reference_and_relocates() {
	run_pcp_test("SubrootReferenceAndRelocates_root");
}

#[test]
fn subroot_reference_and_variants2() {
	run_pcp_test("SubrootReferenceAndVariants2_root");
}

#[test]
fn subroot_reference_and_variants() {
	run_pcp_test("SubrootReferenceAndVariants_root");
}

#[test]
fn subroot_reference_non_cycle() {
	run_pcp_test("SubrootReferenceNonCycle_root");
}

#[test]
fn time_codes_per_second() {
	run_pcp_test("TimeCodesPerSecond_root");
}

#[test]
fn tricky_class_hierarchy() {
	run_pcp_test("TrickyClassHierarchy_root");
}

#[test]
fn tricky_connection_to_relocated_attribute() {
	run_pcp_test("TrickyConnectionToRelocatedAttribute_root");
}

#[test]
fn tricky_inherits_and_relocates2() {
	run_pcp_test("TrickyInheritsAndRelocates2_root");
}

#[test]
fn tricky_inherits_and_relocates3() {
	run_pcp_test("TrickyInheritsAndRelocates3_root");
}

#[test]
fn tricky_inherits_and_relocates4() {
	run_pcp_test("TrickyInheritsAndRelocates4_root");
}

#[test]
fn tricky_inherits_and_relocates5() {
	run_pcp_test("TrickyInheritsAndRelocates5_root");
}

#[test]
fn tricky_inherits_and_relocates_to_new_root_prim() {
	run_pcp_test("TrickyInheritsAndRelocatesToNewRootPrim_root");
}

#[test]
fn tricky_inherits_and_relocates() {
	run_pcp_test("TrickyInheritsAndRelocates_root");
}

#[test]
fn tricky_inherits_in_variants2() {
	run_pcp_test("TrickyInheritsInVariants2_root");
}

#[test]
fn tricky_inherits_in_variants() {
	run_pcp_test("TrickyInheritsInVariants_root");
}

#[test]
fn tricky_list_edited_target_paths() {
	run_pcp_test("TrickyListEditedTargetPaths_root");
}

#[test]
fn tricky_local_class_hierarchy_with_relocates() {
	run_pcp_test("TrickyLocalClassHierarchyWithRelocates_root");
}

#[test]
fn tricky_multiple_relocations2() {
	run_pcp_test("TrickyMultipleRelocations2_root");
}

#[test]
fn tricky_multiple_relocations3() {
	run_pcp_test("TrickyMultipleRelocations3_root");
}

#[test]
fn tricky_multiple_relocations4() {
	run_pcp_test("TrickyMultipleRelocations4_root");
}

#[test]
fn tricky_multiple_relocations5() {
	run_pcp_test("TrickyMultipleRelocations5_root");
}

#[test]
fn tricky_multiple_relocations_and_classes2() {
	run_pcp_test("TrickyMultipleRelocationsAndClasses2_root");
}

#[test]
fn tricky_multiple_relocations_and_classes() {
	run_pcp_test("TrickyMultipleRelocationsAndClasses_root");
}

#[test]
fn tricky_multiple_relocations() {
	run_pcp_test("TrickyMultipleRelocations_root");
}

#[test]
fn tricky_nested_classes2() {
	run_pcp_test("TrickyNestedClasses2_root");
}

#[test]
fn tricky_nested_classes3() {
	run_pcp_test("TrickyNestedClasses3_root");
}

#[test]
fn tricky_nested_classes4() {
	run_pcp_test("TrickyNestedClasses4_root");
}

#[test]
fn tricky_nested_classes() {
	run_pcp_test("TrickyNestedClasses_root");
}

#[test]
fn tricky_nested_specializes2() {
	run_pcp_test("TrickyNestedSpecializes2_root");
}

#[test]
fn tricky_nested_specializes() {
	run_pcp_test("TrickyNestedSpecializes_root");
}

#[test]
fn tricky_nested_variants() {
	run_pcp_test("TrickyNestedVariants_root");
}

#[test]
fn tricky_non_local_variant_selection() {
	run_pcp_test("TrickyNonLocalVariantSelection_root");
}

#[test]
fn tricky_relocated_target_in_variant() {
	run_pcp_test("TrickyRelocatedTargetInVariant_root");
}

#[test]
fn tricky_relocation_of_prim_from_payload() {
	run_pcp_test("TrickyRelocationOfPrimFromPayload_root");
}

#[test]
fn tricky_relocation_of_prim_from_variant() {
	run_pcp_test("TrickyRelocationOfPrimFromVariant_root");
}

#[test]
fn tricky_relocation_squatter() {
	run_pcp_test("TrickyRelocationSquatter_root");
}

#[test]
fn tricky_specializes_and_inherits2() {
	run_pcp_test("TrickySpecializesAndInherits2_root");
}

#[test]
fn tricky_specializes_and_inherits3() {
	run_pcp_test("TrickySpecializesAndInherits3_root");
}

#[test]
fn tricky_specializes_and_inherits() {
	run_pcp_test("TrickySpecializesAndInherits_root");
}

#[test]
fn tricky_specializes_and_relocates() {
	run_pcp_test("TrickySpecializesAndRelocates_root");
}

#[test]
fn tricky_spooky_inherits_in_symmetric_arm_rig() {
	run_pcp_test("TrickySpookyInheritsInSymmetricArmRig_root");
}

#[test]
fn tricky_spooky_inherits_in_symmetric_brow_rig() {
	run_pcp_test("TrickySpookyInheritsInSymmetricBrowRig_root");
}

#[test]
fn tricky_spooky_inherits() {
	run_pcp_test("TrickySpookyInherits_root");
}

#[test]
fn tricky_spooky_variant_selection_in_class() {
	run_pcp_test("TrickySpookyVariantSelectionInClass_root");
}

#[test]
fn tricky_spooky_variant_selection() {
	run_pcp_test("TrickySpookyVariantSelection_root");
}

#[test]
fn tricky_variant_ancestral_selection() {
	run_pcp_test("TrickyVariantAncestralSelection_root");
}

#[test]
fn tricky_variant_independent_selection() {
	run_pcp_test("TrickyVariantIndependentSelection_root");
}

#[test]
fn tricky_variant_in_payload() {
	run_pcp_test("TrickyVariantInPayload_root");
}

#[test]
fn tricky_variant_override_of_local_class() {
	run_pcp_test("TrickyVariantOverrideOfLocalClass_root");
}

#[test]
fn tricky_variant_override_of_relocated_prim() {
	run_pcp_test("TrickyVariantOverrideOfRelocatedPrim_root");
}

#[test]
fn tricky_variant_selection_in_variant2() {
	run_pcp_test("TrickyVariantSelectionInVariant2_root");
}

#[test]
fn tricky_variant_selection_in_variant() {
	run_pcp_test("TrickyVariantSelectionInVariant_root");
}

#[test]
fn tricky_variant_weaker_selection2() {
	run_pcp_test("TrickyVariantWeakerSelection2_root");
}

#[test]
fn tricky_variant_weaker_selection3() {
	run_pcp_test("TrickyVariantWeakerSelection3_root");
}

#[test]
fn tricky_variant_weaker_selection4() {
	run_pcp_test("TrickyVariantWeakerSelection4_root");
}

#[test]
fn tricky_variant_weaker_selection() {
	run_pcp_test("TrickyVariantWeakerSelection_root");
}

#[test]
fn typical_reference_to_chargroup_with_rename() {
	run_pcp_test("TypicalReferenceToChargroupWithRename_root");
}

#[test]
fn typical_reference_to_chargroup() {
	run_pcp_test("TypicalReferenceToChargroup_root");
}

#[test]
fn typical_reference_to_rigged_model() {
	run_pcp_test("TypicalReferenceToRiggedModel_root");
}

#[test]
fn variant_specializes_and_reference_surprising_behavior() {
	run_pcp_test("VariantSpecializesAndReferenceSurprisingBehavior_root");
}

#[test]
fn variant_specializes_and_reference() {
	run_pcp_test("VariantSpecializesAndReference_root");
}*/
