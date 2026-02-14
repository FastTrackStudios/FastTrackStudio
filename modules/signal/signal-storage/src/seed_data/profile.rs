//! Profile seed data — keys-scoped profiles for performance setlists.

use signal_proto::metadata::Metadata;
use signal_proto::overrides::{NodePath, Override};
use signal_proto::profile::{Patch, Profile};
use signal_proto::seed_id;

/// All default profile collections.
pub fn profiles() -> Vec<Profile> {
    vec![keys_feature_profile()]
}

/// Keys Feature Profile — demonstrates profile patches selecting different
/// Keys MegaRig presets (rig scenes).
fn keys_feature_profile() -> Profile {
    let foundation = Patch::new(
        seed_id("keys-feature-foundation"),
        "Foundation",
        seed_id("keys-megarig"),
        seed_id("keys-megarig-default"),
    )
    .with_override(Override::set(
        NodePath::engine("keys-engine")
            .with_layer("keys-layer-core")
            .with_block("keys-core-comp")
            .with_parameter("threshold"),
        0.42,
    ))
    .with_metadata(Metadata::new().with_tag("keys").with_tag("foundation"));

    let wide = Patch::new(
        seed_id("keys-feature-wide"),
        "Wide",
        seed_id("keys-megarig"),
        seed_id("keys-megarig-wide"),
    )
    .with_override(Override {
        path: NodePath::engine("synth-engine")
            .with_layer("synth-layer-motion")
            .with_module("time-parallel"),
        op: signal_proto::overrides::NodeOverrideOp::ReplaceRef(
            seed_id("time-parallel-ambient").to_string(),
        ),
    })
    .with_metadata(Metadata::new().with_tag("keys").with_tag("wide"));

    let focus = Patch::new(
        seed_id("keys-feature-focus"),
        "Focus",
        seed_id("keys-megarig"),
        seed_id("keys-megarig-focus"),
    )
    .with_override(Override {
        path: NodePath::engine("keys-engine")
            .with_layer("keys-layer-space")
            .with_block("keys-space-verb"),
        op: signal_proto::overrides::NodeOverrideOp::ReplaceRef(
            seed_id("reverb-space-plate").to_string(),
        ),
    })
    .with_metadata(Metadata::new().with_tag("keys").with_tag("focus"));

    let air = Patch::new(
        seed_id("keys-feature-air"),
        "Air",
        seed_id("keys-megarig"),
        seed_id("keys-megarig-air"),
    )
    .with_override(Override::set(
        NodePath::engine("pad-engine")
            .with_layer("pad-layer-shimmer")
            .with_block("pad-shimmer-delay")
            .with_parameter("mix"),
        0.61,
    ))
    .with_metadata(Metadata::new().with_tag("keys").with_tag("air"));

    let mut profile = Profile::new(seed_id("keys-feature-profile"), "Keys Feature", foundation);
    profile.add_patch(wide);
    profile.add_patch(focus);
    profile.add_patch(air);
    profile.with_metadata(
        Metadata::new()
            .with_tag("keys")
            .with_tag("setlist")
            .with_description("Keys profile with four patches mapped to distinct Keys MegaRig scenes"),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn profile_count() {
        assert_eq!(profiles().len(), 1);
    }

    #[test]
    fn keys_feature_profile_has_four_patches() {
        let profile = &profiles()[0];
        assert_eq!(profile.name, "Keys Feature");
        assert_eq!(profile.patches.len(), 4);
        assert_eq!(
            profile.default_patch_id.as_str(),
            seed_id("keys-feature-foundation").to_string()
        );
    }

    #[test]
    fn all_patches_target_keys_megarig_scenes() {
        let profile = &profiles()[0];
        for patch in &profile.patches {
            assert_eq!(patch.rig_id.as_str(), seed_id("keys-megarig").to_string());
        }

        let scene_ids: Vec<String> = profile
            .patches
            .iter()
            .map(|p| p.rig_variant_id.as_str().to_string())
            .collect();

        assert!(scene_ids.contains(&seed_id("keys-megarig-default").to_string()));
        assert!(scene_ids.contains(&seed_id("keys-megarig-wide").to_string()));
        assert!(scene_ids.contains(&seed_id("keys-megarig-focus").to_string()));
        assert!(scene_ids.contains(&seed_id("keys-megarig-air").to_string()));
    }
}
