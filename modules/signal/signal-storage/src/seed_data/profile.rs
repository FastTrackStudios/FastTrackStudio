//! Profile seed data — profiles for performance setlists.

use signal_proto::metadata::Metadata;
use signal_proto::overrides::{NodeOverrideOp, NodePath, Override};
use signal_proto::profile::{Patch, Profile};
use signal_proto::seed_id;

/// All default profile collections.
pub fn profiles() -> Vec<Profile> {
    vec![
        keys_feature_profile(),
        guitar_worship_profile(),
        guitar_blues_profile(),
        guitar_rock_profile(),
    ]
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
            .with_description(
                "Keys profile with four patches mapped to distinct Keys MegaRig scenes",
            ),
    )
}

// ── Guitar helper ─────────────────────────────────────────────────────
// All Guitar patches target the Guitar MegaRig and override paths in
// the Archetype JM layer (`guitar-layer-archetype-jm`).

fn guitar_path() -> NodePath {
    NodePath::engine("guitar-engine").with_layer("guitar-layer-archetype-jm")
}

/// Worship profile — 8 patches, default: Clean.
fn guitar_worship_profile() -> Profile {
    let clean = Patch::new(
        seed_id("guitar-worship-clean"),
        "Clean",
        seed_id("guitar-megarig"),
        seed_id("guitar-megarig-default"),
    )
    .with_override(Override {
        path: guitar_path().with_module("pre-fx"),
        op: NodeOverrideOp::ReplaceRef(seed_id("pre-fx-gravity-tank").to_string()),
    })
    .with_override(Override::set(
        guitar_path().with_block("amp").with_parameter("gain"),
        0.18,
    ))
    .with_metadata(
        Metadata::new()
            .with_tag("guitar")
            .with_tag("clean")
            .with_description("AC30 Ambient Clean w/ Pre-FX → Gravity Tank / Spring Reverb Light"),
    );

    let crunch = Patch::new(
        seed_id("guitar-worship-crunch"),
        "Crunch",
        seed_id("guitar-megarig"),
        seed_id("guitar-megarig-default"),
    )
    .with_override(Override::set(
        guitar_path().with_block("amp").with_parameter("gain"),
        0.42,
    ))
    .with_metadata(Metadata::new().with_tag("guitar").with_tag("crunch"));

    let drive = Patch::new(
        seed_id("guitar-worship-drive"),
        "Drive",
        seed_id("guitar-megarig"),
        seed_id("guitar-megarig-default"),
    )
    .with_override(Override::set(
        guitar_path().with_block("amp").with_parameter("gain"),
        0.62,
    ))
    .with_override(Override {
        path: guitar_path().with_module("jm-amp-module"),
        op: NodeOverrideOp::ReplaceRef(seed_id("jm-amp-module-crunch").to_string()),
    })
    .with_metadata(Metadata::new().with_tag("guitar").with_tag("drive"));

    let lead = Patch::new(
        seed_id("guitar-worship-lead"),
        "Lead",
        seed_id("guitar-megarig"),
        seed_id("guitar-megarig-lead"),
    )
    .with_override(Override::set(
        guitar_path().with_block("dream-delay").with_parameter("mix"),
        0.38,
    ))
    .with_metadata(Metadata::new().with_tag("guitar").with_tag("lead"));

    let ambient = Patch::new(
        seed_id("guitar-worship-ambient"),
        "Ambient",
        seed_id("guitar-megarig"),
        seed_id("guitar-megarig-default"),
    )
    .with_override(Override::set(
        guitar_path().with_block("dream-delay").with_parameter("mix"),
        0.65,
    ))
    .with_override(Override::set(
        guitar_path().with_block("dream-delay").with_parameter("feedback"),
        0.55,
    ))
    .with_metadata(Metadata::new().with_tag("guitar").with_tag("ambient"));

    let tremolo = Patch::new(
        seed_id("guitar-worship-tremolo"),
        "Tremolo",
        seed_id("guitar-megarig"),
        seed_id("guitar-megarig-default"),
    )
    .with_override(Override::set(
        guitar_path().with_block("mod").with_parameter("rate"),
        0.50,
    ))
    .with_override(Override::set(
        guitar_path().with_block("mod").with_parameter("depth"),
        0.70,
    ))
    .with_metadata(Metadata::new().with_tag("guitar").with_tag("tremolo"));

    let delay = Patch::new(
        seed_id("guitar-worship-delay"),
        "Delay",
        seed_id("guitar-megarig"),
        seed_id("guitar-megarig-default"),
    )
    .with_override(Override::set(
        guitar_path().with_block("dream-delay").with_parameter("mix"),
        0.52,
    ))
    .with_override(Override::set(
        guitar_path().with_block("dream-delay").with_parameter("time"),
        0.40,
    ))
    .with_metadata(Metadata::new().with_tag("guitar").with_tag("delay"));

    let solo = Patch::new(
        seed_id("guitar-worship-solo"),
        "Solo",
        seed_id("guitar-megarig"),
        seed_id("guitar-megarig-lead"),
    )
    .with_override(Override::set(
        guitar_path().with_block("amp").with_parameter("gain"),
        0.72,
    ))
    .with_override(Override::set(
        guitar_path().with_block("dream-delay").with_parameter("mix"),
        0.30,
    ))
    .with_metadata(Metadata::new().with_tag("guitar").with_tag("solo"));

    let mut profile = Profile::new(seed_id("guitar-worship-profile"), "Worship", clean);
    profile.add_patch(crunch);
    profile.add_patch(drive);
    profile.add_patch(lead);
    profile.add_patch(ambient);
    profile.add_patch(tremolo);
    profile.add_patch(delay);
    profile.add_patch(solo);
    profile.with_metadata(
        Metadata::new()
            .with_tag("guitar")
            .with_tag("worship")
            .with_tag("setlist")
            .with_description("Worship guitar profile — ambient cleans, lush delays, dynamic drive"),
    )
}

/// Blues profile — 8 patches, default: Crunch.
fn guitar_blues_profile() -> Profile {
    let clean = Patch::new(
        seed_id("guitar-blues-clean"),
        "Clean",
        seed_id("guitar-megarig"),
        seed_id("guitar-megarig-default"),
    )
    .with_override(Override::set(
        guitar_path().with_block("amp").with_parameter("gain"),
        0.15,
    ))
    .with_metadata(Metadata::new().with_tag("guitar").with_tag("clean"));

    let crunch = Patch::new(
        seed_id("guitar-blues-crunch"),
        "Crunch",
        seed_id("guitar-megarig"),
        seed_id("guitar-megarig-default"),
    )
    .with_override(Override::set(
        guitar_path().with_block("amp").with_parameter("gain"),
        0.48,
    ))
    .with_override(Override::set(
        guitar_path().with_block("amp").with_parameter("tone"),
        0.55,
    ))
    .with_metadata(Metadata::new().with_tag("guitar").with_tag("crunch"));

    let drive = Patch::new(
        seed_id("guitar-blues-drive"),
        "Drive",
        seed_id("guitar-megarig"),
        seed_id("guitar-megarig-default"),
    )
    .with_override(Override::set(
        guitar_path().with_block("amp").with_parameter("gain"),
        0.60,
    ))
    .with_metadata(Metadata::new().with_tag("guitar").with_tag("drive"));

    let lead = Patch::new(
        seed_id("guitar-blues-lead"),
        "Lead",
        seed_id("guitar-megarig"),
        seed_id("guitar-megarig-lead"),
    )
    .with_override(Override::set(
        guitar_path().with_block("amp").with_parameter("gain"),
        0.65,
    ))
    .with_override(Override::set(
        guitar_path().with_block("dream-delay").with_parameter("mix"),
        0.22,
    ))
    .with_metadata(Metadata::new().with_tag("guitar").with_tag("lead"));

    let funk = Patch::new(
        seed_id("guitar-blues-funk"),
        "Funk",
        seed_id("guitar-megarig"),
        seed_id("guitar-megarig-default"),
    )
    .with_override(Override::set(
        guitar_path().with_block("amp").with_parameter("gain"),
        0.30,
    ))
    .with_override(Override::set(
        guitar_path().with_block("comp").with_parameter("threshold"),
        0.35,
    ))
    .with_metadata(Metadata::new().with_tag("guitar").with_tag("funk"));

    let qtron = Patch::new(
        seed_id("guitar-blues-qtron"),
        "Q-Tron",
        seed_id("guitar-megarig"),
        seed_id("guitar-megarig-default"),
    )
    .with_override(Override {
        path: guitar_path().with_module("pre-fx"),
        op: NodeOverrideOp::ReplaceRef(seed_id("pre-fx-qtron").to_string()),
    })
    .with_override(Override::set(
        guitar_path().with_block("amp").with_parameter("gain"),
        0.35,
    ))
    .with_metadata(Metadata::new().with_tag("guitar").with_tag("qtron"));

    let roomy = Patch::new(
        seed_id("guitar-blues-roomy"),
        "Roomy",
        seed_id("guitar-megarig"),
        seed_id("guitar-megarig-default"),
    )
    .with_override(Override::set(
        guitar_path().with_block("reverb").with_parameter("mix"),
        0.55,
    ))
    .with_override(Override::set(
        guitar_path().with_block("reverb").with_parameter("decay"),
        0.60,
    ))
    .with_metadata(Metadata::new().with_tag("guitar").with_tag("roomy"));

    let solo = Patch::new(
        seed_id("guitar-blues-solo"),
        "Solo",
        seed_id("guitar-megarig"),
        seed_id("guitar-megarig-lead"),
    )
    .with_override(Override::set(
        guitar_path().with_block("amp").with_parameter("gain"),
        0.70,
    ))
    .with_override(Override::set(
        guitar_path().with_block("dream-delay").with_parameter("mix"),
        0.28,
    ))
    .with_metadata(Metadata::new().with_tag("guitar").with_tag("solo"));

    // Default patch is Crunch (second patch) — pass clean as constructor, then override default.
    let mut profile = Profile::new(seed_id("guitar-blues-profile"), "Blues", clean);
    profile.add_patch(crunch);
    profile.add_patch(drive);
    profile.add_patch(lead);
    profile.add_patch(funk);
    profile.add_patch(qtron);
    profile.add_patch(roomy);
    profile.add_patch(solo);
    // Override default to Crunch
    profile.default_patch_id = seed_id("guitar-blues-crunch").into();
    profile.with_metadata(
        Metadata::new()
            .with_tag("guitar")
            .with_tag("blues")
            .with_tag("setlist")
            .with_description("Blues guitar profile — warm crunch, funky cleans, soulful leads"),
    )
}

/// Rock profile — 8 patches, default: Drive.
fn guitar_rock_profile() -> Profile {
    let clean = Patch::new(
        seed_id("guitar-rock-clean"),
        "Clean",
        seed_id("guitar-megarig"),
        seed_id("guitar-megarig-default"),
    )
    .with_override(Override::set(
        guitar_path().with_block("amp").with_parameter("gain"),
        0.20,
    ))
    .with_metadata(Metadata::new().with_tag("guitar").with_tag("clean"));

    let crunch = Patch::new(
        seed_id("guitar-rock-crunch"),
        "Crunch",
        seed_id("guitar-megarig"),
        seed_id("guitar-megarig-default"),
    )
    .with_override(Override::set(
        guitar_path().with_block("amp").with_parameter("gain"),
        0.52,
    ))
    .with_override(Override {
        path: guitar_path().with_module("jm-amp-module"),
        op: NodeOverrideOp::ReplaceRef(seed_id("jm-amp-module-crunch").to_string()),
    })
    .with_metadata(Metadata::new().with_tag("guitar").with_tag("crunch"));

    let drive = Patch::new(
        seed_id("guitar-rock-drive"),
        "Drive",
        seed_id("guitar-megarig"),
        seed_id("guitar-megarig-default"),
    )
    .with_override(Override::set(
        guitar_path().with_block("amp").with_parameter("gain"),
        0.68,
    ))
    .with_override(Override {
        path: guitar_path().with_module("jm-amp-module"),
        op: NodeOverrideOp::ReplaceRef(seed_id("jm-amp-module-crunch").to_string()),
    })
    .with_metadata(Metadata::new().with_tag("guitar").with_tag("drive"));

    let lead = Patch::new(
        seed_id("guitar-rock-lead"),
        "Lead",
        seed_id("guitar-megarig"),
        seed_id("guitar-megarig-lead"),
    )
    .with_override(Override::set(
        guitar_path().with_block("amp").with_parameter("gain"),
        0.72,
    ))
    .with_metadata(Metadata::new().with_tag("guitar").with_tag("lead"));

    let ambient = Patch::new(
        seed_id("guitar-rock-ambient"),
        "Ambient",
        seed_id("guitar-megarig"),
        seed_id("guitar-megarig-default"),
    )
    .with_override(Override::set(
        guitar_path().with_block("dream-delay").with_parameter("mix"),
        0.55,
    ))
    .with_override(Override::set(
        guitar_path().with_block("reverb").with_parameter("mix"),
        0.50,
    ))
    .with_metadata(Metadata::new().with_tag("guitar").with_tag("ambient"));

    let phaser = Patch::new(
        seed_id("guitar-rock-phaser"),
        "Phaser",
        seed_id("guitar-megarig"),
        seed_id("guitar-megarig-default"),
    )
    .with_override(Override::set(
        guitar_path().with_block("mod").with_parameter("rate"),
        0.35,
    ))
    .with_override(Override::set(
        guitar_path().with_block("mod").with_parameter("depth"),
        0.60,
    ))
    .with_metadata(Metadata::new().with_tag("guitar").with_tag("phaser"));

    let dly_lead = Patch::new(
        seed_id("guitar-rock-dly-lead"),
        "DLY Lead",
        seed_id("guitar-megarig"),
        seed_id("guitar-megarig-lead"),
    )
    .with_override(Override::set(
        guitar_path().with_block("dream-delay").with_parameter("mix"),
        0.45,
    ))
    .with_override(Override::set(
        guitar_path().with_block("dream-delay").with_parameter("feedback"),
        0.40,
    ))
    .with_metadata(Metadata::new().with_tag("guitar").with_tag("delay"));

    let solo = Patch::new(
        seed_id("guitar-rock-solo"),
        "Solo",
        seed_id("guitar-megarig"),
        seed_id("guitar-megarig-lead"),
    )
    .with_override(Override::set(
        guitar_path().with_block("amp").with_parameter("gain"),
        0.78,
    ))
    .with_override(Override::set(
        guitar_path().with_block("dream-delay").with_parameter("mix"),
        0.25,
    ))
    .with_metadata(Metadata::new().with_tag("guitar").with_tag("solo"));

    // Default patch is Drive (third patch) — pass clean as constructor, then override.
    let mut profile = Profile::new(seed_id("guitar-rock-profile"), "Rock", clean);
    profile.add_patch(crunch);
    profile.add_patch(drive);
    profile.add_patch(lead);
    profile.add_patch(ambient);
    profile.add_patch(phaser);
    profile.add_patch(dly_lead);
    profile.add_patch(solo);
    // Override default to Drive
    profile.default_patch_id = seed_id("guitar-rock-drive").into();
    profile.with_metadata(
        Metadata::new()
            .with_tag("guitar")
            .with_tag("rock")
            .with_tag("setlist")
            .with_description("Rock guitar profile — punchy drive, soaring leads, spatial effects"),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn profile_count() {
        assert_eq!(profiles().len(), 4);
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
    fn all_keys_patches_target_keys_megarig_scenes() {
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

    #[test]
    fn guitar_worship_has_eight_patches() {
        let profile = profiles().into_iter().find(|p| p.name == "Worship").unwrap();
        assert_eq!(profile.patches.len(), 8);
        assert_eq!(
            profile.default_patch_id.as_str(),
            seed_id("guitar-worship-clean").to_string(),
            "Worship default should be Clean"
        );
        let names: Vec<&str> = profile.patches.iter().map(|p| p.name.as_str()).collect();
        assert_eq!(
            names,
            ["Clean", "Crunch", "Drive", "Lead", "Ambient", "Tremolo", "Delay", "Solo"]
        );
    }

    #[test]
    fn guitar_blues_has_eight_patches_default_crunch() {
        let profile = profiles().into_iter().find(|p| p.name == "Blues").unwrap();
        assert_eq!(profile.patches.len(), 8);
        assert_eq!(
            profile.default_patch_id.as_str(),
            seed_id("guitar-blues-crunch").to_string(),
            "Blues default should be Crunch"
        );
        let names: Vec<&str> = profile.patches.iter().map(|p| p.name.as_str()).collect();
        assert_eq!(
            names,
            ["Clean", "Crunch", "Drive", "Lead", "Funk", "Q-Tron", "Roomy", "Solo"]
        );
    }

    #[test]
    fn guitar_rock_has_eight_patches_default_drive() {
        let profile = profiles().into_iter().find(|p| p.name == "Rock").unwrap();
        assert_eq!(profile.patches.len(), 8);
        assert_eq!(
            profile.default_patch_id.as_str(),
            seed_id("guitar-rock-drive").to_string(),
            "Rock default should be Drive"
        );
        let names: Vec<&str> = profile.patches.iter().map(|p| p.name.as_str()).collect();
        assert_eq!(
            names,
            ["Clean", "Crunch", "Drive", "Lead", "Ambient", "Phaser", "DLY Lead", "Solo"]
        );
    }

    #[test]
    fn all_guitar_patches_target_guitar_megarig() {
        for profile in profiles().iter().skip(1) {
            for patch in &profile.patches {
                assert_eq!(
                    patch.rig_id.as_str(),
                    seed_id("guitar-megarig").to_string(),
                    "patch '{}' in '{}' should target guitar-megarig",
                    patch.name,
                    profile.name,
                );
            }
        }
    }
}
