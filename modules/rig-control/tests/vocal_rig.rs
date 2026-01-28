//! Integration test: Build a complete FTS-Vocal rig with module presets,
//! top-level presets, profiles, songs, and a multi-rig director with roles.
//!
//! This exercises the full public API surface for a vocal rig setup and
//! validates the director's role assignment resolution across songs/scenes.

use rig_control::category::{BaseTone, Genre};
use rig_control::defaults::vocals;
use rig_control::director::{RigDirector, Role, RoleAssignment};
use rig_control::module::ModuleType;
use rig_control::module_preset::ModulePreset;
use rig_control::rig::InstrumentType;
use rig_control::*;

// ─────────────────────────────────────────────────────────────────────────────
// Vocal Rig Construction
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn vocal_rig_builds_successfully() {
    let defaults = vocals::build_vocal_rig();
    assert_eq!(defaults.rig.name, "FTS Vocal");
    assert_eq!(defaults.rig.instrument_type, InstrumentType::Vocals);
}

#[test]
fn vocal_rig_has_correct_module_preset_count() {
    let defaults = vocals::build_vocal_rig();
    // rescue, pitch_correction, vocal_tone, vocal_chorus, vocal_flanger, vocal_sends
    assert_eq!(defaults.rig.module_presets.len(), 6);
}

#[test]
fn vocal_module_presets_have_correct_snapshot_counts() {
    let defaults = vocals::build_vocal_rig();
    let rig = &defaults.rig;

    let find_mp = |name: &str| -> &ModulePreset {
        rig.module_presets
            .iter()
            .find(|mp| mp.name == name)
            .unwrap_or_else(|| panic!("Module preset '{name}' not found"))
    };

    // Vocal Rescue: 4 snapshots (Standard, Heavy Gate, Light Touch, No Gate)
    assert_eq!(find_mp("Vocal Rescue").snapshots.len(), 4);

    // Pitch Correction: 4 snapshots (Subtle, Standard, Hard Tune, Off)
    assert_eq!(find_mp("Pitch Correction").snapshots.len(), 4);

    // Vocal Tone: 5 snapshots (Natural, Warm, Bright, Powerful, Breathy)
    assert_eq!(find_mp("Vocal Tone").snapshots.len(), 5);

    // Vocal Chorus: 0 snapshots
    assert_eq!(find_mp("Vocal Chorus").snapshots.len(), 0);

    // Vocal Flanger: 0 snapshots
    assert_eq!(find_mp("Vocal Flanger").snapshots.len(), 0);

    // Vocal Sends: 7 snapshots (Dry, Room, Hall, Plate, Slapback, Long Delay, Spacey)
    assert_eq!(find_mp("Vocal Sends").snapshots.len(), 7);
}

#[test]
fn vocal_module_presets_have_at_least_one_block() {
    let defaults = vocals::build_vocal_rig();
    for mp in &defaults.rig.module_presets {
        assert!(
            !mp.blocks.is_empty(),
            "Module preset '{}' should have at least one block",
            mp.name
        );
    }
}

#[test]
fn vocal_module_presets_have_correct_default_snapshots() {
    let defaults = vocals::build_vocal_rig();
    let rig = &defaults.rig;

    let find_mp = |name: &str| -> &ModulePreset {
        rig.module_presets.iter().find(|mp| mp.name == name).unwrap()
    };

    // Rescue default: "Standard"
    let rescue = find_mp("Vocal Rescue");
    assert_eq!(rescue.default_snapshot().unwrap().name, "Standard");

    // Correction default: "Subtle"
    let correction = find_mp("Pitch Correction");
    assert_eq!(correction.default_snapshot().unwrap().name, "Subtle");

    // Tone default: "Natural"
    let tone = find_mp("Vocal Tone");
    assert_eq!(tone.default_snapshot().unwrap().name, "Natural");

    // Sends default: "Dry"
    let sends = find_mp("Vocal Sends");
    assert_eq!(sends.default_snapshot().unwrap().name, "Dry");

    // Chorus and Flanger: no snapshots, no default
    assert!(find_mp("Vocal Chorus").default_snapshot().is_none());
    assert!(find_mp("Vocal Flanger").default_snapshot().is_none());
}

// ─────────────────────────────────────────────────────────────────────────────
// Vocal Rescue Block Chain
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn vocal_rescue_has_correct_block_chain() {
    let defaults = vocals::build_vocal_rig();
    let rescue = defaults
        .rig
        .module_presets
        .iter()
        .find(|mp| mp.name == "Vocal Rescue")
        .unwrap();

    assert_eq!(rescue.blocks.len(), 4);
    assert_eq!(rescue.blocks[0].block.name, "De-Esser");
    assert_eq!(rescue.blocks[1].block.name, "Renegate");
    assert_eq!(rescue.blocks[2].block.name, "Rescue EQ");
    assert_eq!(rescue.blocks[3].block.name, "Control Compressor");
}

#[test]
fn vocal_tonal_has_correct_block_chain() {
    let defaults = vocals::build_vocal_rig();
    let tonal = defaults
        .rig
        .module_presets
        .iter()
        .find(|mp| mp.name == "Vocal Tone")
        .unwrap();

    assert_eq!(tonal.blocks.len(), 3);
    assert_eq!(tonal.blocks[0].block.name, "Style Compressor");
    assert_eq!(tonal.blocks[1].block.name, "Tonal EQ");
    assert_eq!(tonal.blocks[2].block.name, "Saturator");
}

// ─────────────────────────────────────────────────────────────────────────────
// Vocal Presets
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn vocal_presets_have_correct_count_and_categories() {
    let defaults = vocals::build_vocal_rig();
    assert_eq!(defaults.presets.len(), 5);

    let find = |name: &str| defaults.presets.iter().find(|p| p.name == name).unwrap();

    // Natural Clean: Generic Natural
    assert_eq!(find("Natural Clean").category.base_tone(), BaseTone::Natural);
    assert!(find("Natural Clean").category.genre().is_none());

    // Warm Worship: Genre Worship Warm
    assert_eq!(find("Warm Worship").category.base_tone(), BaseTone::Warm);
    assert_eq!(find("Warm Worship").category.genre(), Some(&Genre::Worship));

    // Bright Pop: Genre Pop Bright
    assert_eq!(find("Bright Pop").category.base_tone(), BaseTone::Bright);
    assert_eq!(find("Bright Pop").category.genre(), Some(&Genre::Pop));

    // Powerful Rock: Genre Rock Powerful
    assert_eq!(
        find("Powerful Rock").category.base_tone(),
        BaseTone::Powerful
    );
    assert_eq!(
        find("Powerful Rock").category.genre(),
        Some(&Genre::Rock)
    );

    // Breathy Ballad: Genre Pop Breathy
    assert_eq!(
        find("Breathy Ballad").category.base_tone(),
        BaseTone::Breathy
    );
    assert_eq!(find("Breathy Ballad").category.genre(), Some(&Genre::Pop));
}

#[test]
fn vocal_presets_have_correct_module_assignments() {
    let defaults = vocals::build_vocal_rig();

    let find = |name: &str| defaults.presets.iter().find(|p| p.name == name).unwrap();

    // Natural Clean: Rescue, Correction, Tonal, Sends (4)
    let nc = find("Natural Clean");
    assert!(nc.get_module_assignment(ModuleType::Rescue).is_some());
    assert!(nc.get_module_assignment(ModuleType::Correction).is_some());
    assert!(nc.get_module_assignment(ModuleType::Tonal).is_some());
    assert!(nc.get_module_assignment(ModuleType::Sends).is_some());
    assert_eq!(nc.module_assignments.len(), 4);

    // Warm Worship: Rescue, Correction, Tonal, VocalModulation, Sends (5)
    let ww = find("Warm Worship");
    assert!(
        ww.get_module_assignment(ModuleType::VocalModulation)
            .is_some()
    );
    assert_eq!(ww.module_assignments.len(), 5);

    // Bright Pop: 4 (no VocalModulation)
    let bp = find("Bright Pop");
    assert!(
        bp.get_module_assignment(ModuleType::VocalModulation)
            .is_none()
    );
    assert_eq!(bp.module_assignments.len(), 4);
}

#[test]
fn vocal_module_assignment_snapshots_reference_valid_snapshots() {
    let defaults = vocals::build_vocal_rig();

    for preset in &defaults.presets {
        for assignment in &preset.module_assignments {
            if let Some(snap_id) = assignment.module_snapshot_id {
                let mp = defaults
                    .rig
                    .get_module_preset(assignment.module_preset_id)
                    .unwrap_or_else(|| {
                        panic!(
                            "Module preset ID {:?} referenced by preset '{}' not found in rig",
                            assignment.module_preset_id, preset.name
                        )
                    });
                assert!(
                    mp.snapshot(snap_id).is_some(),
                    "Snapshot {:?} referenced by preset '{}' for {} not found in module preset '{}'",
                    snap_id,
                    preset.name,
                    assignment.module_type,
                    mp.name
                );
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Vocal Profiles
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn vocal_profiles_have_correct_template_counts() {
    let defaults = vocals::build_vocal_rig();

    assert_eq!(defaults.profiles.len(), 3);
    assert_eq!(defaults.profiles[0].name, "Worship");
    assert_eq!(defaults.profiles[0].scene_templates.len(), 6);

    assert_eq!(defaults.profiles[1].name, "Pop/R&B");
    assert_eq!(defaults.profiles[1].scene_templates.len(), 6);

    assert_eq!(defaults.profiles[2].name, "Rock");
    assert_eq!(defaults.profiles[2].scene_templates.len(), 6);
}

#[test]
fn vocal_profile_default_template_is_first() {
    let defaults = vocals::build_vocal_rig();
    for profile in &defaults.profiles {
        let default = profile.default_scene_template().unwrap();
        assert_eq!(
            default.id, profile.scene_templates[0].id,
            "Default should be first template in profile '{}'",
            profile.name
        );
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Vocal Songs
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn vocal_songs_have_correct_scene_counts() {
    let defaults = vocals::build_vocal_rig();

    assert_eq!(defaults.songs.len(), 3);
    assert_eq!(defaults.songs[0].name, "Oceans");
    assert_eq!(defaults.songs[0].scene_count(), 4);

    assert_eq!(defaults.songs[1].name, "Thriller");
    assert_eq!(defaults.songs[1].scene_count(), 4);

    assert_eq!(defaults.songs[2].name, "Bohemian Rhapsody");
    assert_eq!(defaults.songs[2].scene_count(), 5);
}

#[test]
fn oceans_has_song_level_correction_override() {
    let defaults = vocals::build_vocal_rig();
    let oceans = &defaults.songs[0];
    assert_eq!(oceans.name, "Oceans");

    assert_eq!(oceans.module_overrides.len(), 1);
    let ov = &oceans.module_overrides[0];
    assert_eq!(ov.module_type, ModuleType::Correction);
    assert!(ov.is_swap());
}

#[test]
fn bohemian_rhapsody_opera_has_scene_override() {
    let defaults = vocals::build_vocal_rig();
    let bohemian = &defaults.songs[2];
    assert_eq!(bohemian.name, "Bohemian Rhapsody");

    let opera = &bohemian.scenes[1];
    assert_eq!(opera.name, "Opera");
    assert_eq!(opera.module_overrides.len(), 1);
    assert_eq!(
        opera.module_overrides[0].module_type,
        ModuleType::VocalModulation
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Director: Multi-Rig Role Coordination
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn director_coordinates_four_vocal_rigs_via_roles() {
    // Build 4 independent vocal rigs
    let rig1 = vocals::build_vocal_rig();
    let rig2 = vocals::build_vocal_rig();
    let rig3 = vocals::build_vocal_rig();
    let rig4 = vocals::build_vocal_rig();

    // Create a director with a shared sends engine
    let sends_engine = rig_control::module::Module::new("Shared Sends", ModuleType::Sends);
    let mut director = RigDirector::new("Vocal Director", sends_engine);

    director.add_rig(rig1.rig);
    director.add_rig(rig2.rig);
    director.add_rig(rig3.rig);
    director.add_rig(rig4.rig);

    // Define roles
    let lead = Role::lead("Lead Vocal");
    let bg1 = Role::background("BG Vocal 1", Some(1));
    let bg2 = Role::background("BG Vocal 2", Some(2));

    let lead_id = lead.id;
    let bg1_id = bg1.id;
    let bg2_id = bg2.id;

    director.add_role(lead);
    director.add_role(bg1);
    director.add_role(bg2);

    // Get rig IDs
    let rig1_id = director.rigs[0].id;
    let rig2_id = director.rigs[1].id;
    let rig3_id = director.rigs[2].id;

    // Global defaults: Rig 1 = Lead, Rig 2 = BG1, Rig 3 = BG2
    director.add_assignment(RoleAssignment::global(lead_id, rig1_id));
    director.add_assignment(RoleAssignment::global(bg1_id, rig2_id));
    director.add_assignment(RoleAssignment::global(bg2_id, rig3_id));

    // Song A: same as global (just inherits)
    let song_a_id = SongId::new();

    // Song B: Rig 3 becomes Lead, Rig 1 becomes BG1
    let song_b_id = SongId::new();
    director.add_assignment(RoleAssignment::for_song(lead_id, rig3_id, song_b_id));
    director.add_assignment(RoleAssignment::for_song(bg1_id, rig1_id, song_b_id));

    // Verify Song A uses global assignments
    assert_eq!(
        director.resolve_role(lead_id, song_a_id, None),
        Some(rig1_id)
    );
    assert_eq!(
        director.resolve_role(bg1_id, song_a_id, None),
        Some(rig2_id)
    );
    assert_eq!(
        director.resolve_role(bg2_id, song_a_id, None),
        Some(rig3_id)
    );

    // Verify Song B overrides Lead and BG1
    assert_eq!(
        director.resolve_role(lead_id, song_b_id, None),
        Some(rig3_id)
    );
    assert_eq!(
        director.resolve_role(bg1_id, song_b_id, None),
        Some(rig1_id)
    );
    // BG2 falls back to global
    assert_eq!(
        director.resolve_role(bg2_id, song_b_id, None),
        Some(rig3_id)
    );

    // Verify director state
    assert_eq!(director.rigs.len(), 4);
    assert_eq!(director.roles.len(), 3);

    // Verify all rigs are Vocals type
    let vocal_rigs = director.rigs_by_type(&InstrumentType::Vocals);
    assert_eq!(vocal_rigs.len(), 4);
}

#[test]
fn director_scene_override_beats_song_for_roles() {
    let rig1 = vocals::build_vocal_rig();
    let rig2 = vocals::build_vocal_rig();

    let sends_engine = rig_control::module::Module::new("Sends", ModuleType::Sends);
    let mut director = RigDirector::new("Test Director", sends_engine);

    director.add_rig(rig1.rig);
    director.add_rig(rig2.rig);

    let lead = Role::lead("Lead");
    let lead_id = lead.id;
    director.add_role(lead);

    let rig1_id = director.rigs[0].id;
    let rig2_id = director.rigs[1].id;

    let song_id = SongId::new();
    let scene_id = SceneId::new();

    // Song-level: Rig 1 is Lead
    director.add_assignment(RoleAssignment::for_song(lead_id, rig1_id, song_id));

    // Scene-level: Rig 2 is Lead (for a specific scene only)
    director.add_assignment(RoleAssignment::for_scene(
        lead_id, rig2_id, song_id, scene_id,
    ));

    // Without scene context: song assignment wins
    assert_eq!(
        director.resolve_role(lead_id, song_id, None),
        Some(rig1_id)
    );

    // With scene context: scene assignment wins
    assert_eq!(
        director.resolve_role(lead_id, song_id, Some(scene_id)),
        Some(rig2_id)
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// BaseTone vocal variants
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn vocal_base_tones_exist() {
    let tones = BaseTone::vocal_tones();
    assert_eq!(tones.len(), 5);
    assert_eq!(tones[0], BaseTone::Natural);
    assert_eq!(tones[1], BaseTone::Warm);
    assert_eq!(tones[2], BaseTone::Bright);
    assert_eq!(tones[3], BaseTone::Breathy);
    assert_eq!(tones[4], BaseTone::Powerful);

    // Display names
    assert_eq!(BaseTone::Natural.as_str(), "Natural");
    assert_eq!(BaseTone::Warm.as_str(), "Warm");
    assert_eq!(BaseTone::Bright.as_str(), "Bright");
    assert_eq!(BaseTone::Breathy.as_str(), "Breathy");
    assert_eq!(BaseTone::Powerful.as_str(), "Powerful");

    // Parse round-trip
    for tone in tones {
        assert_eq!(BaseTone::parse(tone.as_str()), Some(*tone));
    }
}
