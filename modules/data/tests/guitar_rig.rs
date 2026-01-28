//! Integration test: Build a complete FTS-Guitar rig with module presets,
//! top-level presets, profiles, songs, and module overrides.
//!
//! This exercises the full public API surface of the `data` crate to ensure
//! all types compose correctly for a realistic guitar rig setup.

use data::block::{Block, GlobalBlock, PluginId};
use data::category::{BaseTone, Genre, PresetCategory};
use data::module::{ModuleBlock, ModuleType};
use data::module_preset::{
    GlobalModuleOverride, ModuleAssignment, ModuleOverride, ModulePreset, ModuleSnapshot,
};
use data::performance::{PerformanceSetlist, PerformanceSong, Scene};
use data::preset::builder::PresetBuilder;
use data::preset::{Credit, Inspiration, Preset, Snapshot};
use data::profile::{Profile, SceneTemplate};
use data::rig::{InstrumentType, Rig};
use data::tags::Tags;
use data::*;

// ─────────────────────────────────────────────────────────────────────────────
// Fixture
// ─────────────────────────────────────────────────────────────────────────────

struct GuitarRigFixture {
    rig: Rig,
    presets: Vec<Preset>,
    profiles: Vec<Profile>,
    songs: Vec<PerformanceSong>,
    setlist: PerformanceSetlist,
}

/// Helper: create a VST3 block inside a module preset.
fn make_block(name: &str, uid: &str, order: u8) -> ModuleBlock {
    let block = Block::new(name, PluginId::vst3(uid, name));
    ModuleBlock::new(block, Order::new(order))
}

/// Helper: create a module preset, optionally with snapshots.
/// Returns the module preset with blocks and snapshots added, and default
/// snapshot set to the first snapshot if any exist.
fn make_module_preset(
    name: &str,
    module_type: ModuleType,
    block_name: &str,
    block_uid: &str,
    snapshot_names: &[&str],
) -> ModulePreset {
    let mut mp = ModulePreset::new(name, module_type)
        .with_description(format!("{name} module preset"));
    mp.add_block(make_block(block_name, block_uid, 0));

    let mut first_snap_id = None;
    for snap_name in snapshot_names {
        let snap = ModuleSnapshot::new(*snap_name);
        let snap_id = mp.add_snapshot(snap);
        if first_snap_id.is_none() {
            first_snap_id = Some(snap_id);
        }
    }
    if let Some(id) = first_snap_id {
        mp.set_default_snapshot(id);
    }
    mp
}

fn build_guitar_rig() -> GuitarRigFixture {
    // ── 1. Rig ───────────────────────────────────────────────────────────
    let mut rig = Rig::new("FTS-Guitar", InstrumentType::Guitar);

    // ── 1a. Module Presets ───────────────────────────────────────────────
    // Source
    let mp_source = make_module_preset(
        "Guitar Input",
        ModuleType::Source,
        "Noise Gate",
        "com.fts.noisegate",
        &["Subtle Gate", "No Gate", "Heavy Gate"],
    );
    let mp_source_id = mp_source.id;

    // Dynamics
    let mp_dynamics = make_module_preset(
        "Compressor",
        ModuleType::Dynamics,
        "Studio Comp",
        "com.fts.compressor",
        &[],
    );
    let mp_dynamics_id = mp_dynamics.id;

    // Special (4 presets)
    let mp_envelope = make_module_preset(
        "Envelope Filter",
        ModuleType::Special,
        "Envelope Filter",
        "com.fts.envelope",
        &[],
    );
    let mp_wah = make_module_preset(
        "Wah Pedal",
        ModuleType::Special,
        "Cry Baby Wah",
        "com.fts.wah",
        &[],
    );
    let mp_pitch = make_module_preset(
        "Pitch Octave FX",
        ModuleType::Special,
        "Octave Pitch",
        "com.fts.pitch",
        &[],
    );
    let mp_doubler = make_module_preset(
        "Doubler",
        ModuleType::Special,
        "Doubler FX",
        "com.fts.doubler",
        &[],
    );

    // Drive (2 presets)
    let mp_blues_stack = make_module_preset(
        "Blues Stack",
        ModuleType::Drive,
        "Halfman Drive",
        "com.fts.halfman",
        &["Halfman", "Halfman + Teal", "Halfman + BluesBreaker"],
    );
    let mp_blues_stack_id = mp_blues_stack.id;

    let mp_protein = make_module_preset(
        "Protein + JHS Kilt",
        ModuleType::Drive,
        "Protein Drive",
        "com.fts.protein",
        &[
            "Blue Light",
            "Green Light",
            "Blue Heavy",
            "Green Heavy",
            "Blue + Green",
        ],
    );
    let mp_protein_id = mp_protein.id;

    // PreFx
    let mp_gravity_tank = make_module_preset(
        "Gravity Tank",
        ModuleType::PreFx,
        "Gravity Spring",
        "com.fts.gravity",
        &[
            "Harmonic Tremolo Light",
            "Harmonic Tremolo Strong",
            "Spring Reverb Light",
            "Boing",
        ],
    );
    let mp_gravity_tank_id = mp_gravity_tank.id;

    // Amp (4 presets)
    let mp_dream_ruby = make_module_preset(
        "Dream and Ruby",
        ModuleType::Amp,
        "Dream Amp",
        "com.fts.dream",
        &["Clean", "Breakup", "Drive"],
    );
    let mp_dream_ruby_id = mp_dream_ruby.id;

    let mp_deluxe_ac30 = make_module_preset(
        "Deluxe and AC30",
        ModuleType::Amp,
        "Deluxe Reverb",
        "com.fts.deluxe",
        &["Clean", "Breakup", "Drive"],
    );
    let mp_deluxe_ac30_id = mp_deluxe_ac30.id;

    let mp_dumble = make_module_preset(
        "Dumble and Two-Rock",
        ModuleType::Amp,
        "Dumble ODS",
        "com.fts.dumble",
        &["Ultra-Clean", "Breakup", "Can't Find the Light", "Roomy"],
    );
    let mp_dumble_id = mp_dumble.id;

    let mp_marshall = make_module_preset(
        "Marshall Stack",
        ModuleType::Amp,
        "JCM800",
        "com.fts.marshall",
        &["Clean", "Drive"],
    );
    let mp_marshall_id = mp_marshall.id;

    // Modulation (3 presets)
    let mp_chorus = make_module_preset(
        "Chorus",
        ModuleType::Modulation,
        "Stereo Chorus",
        "com.fts.chorus",
        &[],
    );
    let mp_chorus_id = mp_chorus.id;

    let mp_flanger = make_module_preset(
        "Flanger",
        ModuleType::Modulation,
        "Jet Flanger",
        "com.fts.flanger",
        &[],
    );

    let mp_phaser = make_module_preset(
        "Phaser",
        ModuleType::Modulation,
        "Phase 90",
        "com.fts.phaser",
        &[],
    );
    let mp_phaser_id = mp_phaser.id;

    // Time (3 presets)
    let mp_delay = make_module_preset(
        "Delay",
        ModuleType::Time,
        "Digital Delay",
        "com.fts.delay",
        &[],
    );
    let mp_delay_id = mp_delay.id;

    let mp_reverb = make_module_preset(
        "Reverb",
        ModuleType::Time,
        "Hall Reverb",
        "com.fts.reverb",
        &[],
    );
    let mp_reverb_id = mp_reverb.id;

    let mp_freeze = make_module_preset(
        "Freeze",
        ModuleType::Time,
        "Freeze Pad",
        "com.fts.freeze",
        &[],
    );

    // Motion (5 presets)
    let mp_trem8 = make_module_preset(
        "Tremolo 8th",
        ModuleType::Motion,
        "Tremolo",
        "com.fts.trem8",
        &[],
    );
    let mp_trem8_id = mp_trem8.id;

    let mp_trem16 = make_module_preset(
        "Tremolo 16th",
        ModuleType::Motion,
        "Tremolo Fast",
        "com.fts.trem16",
        &[],
    );

    let mp_vibrato = make_module_preset(
        "Vibrato",
        ModuleType::Motion,
        "Vibrato FX",
        "com.fts.vibrato",
        &[],
    );

    let mp_rotary = make_module_preset(
        "Rotary",
        ModuleType::Motion,
        "Leslie Rotary",
        "com.fts.rotary",
        &[],
    );

    let mp_toomuch = make_module_preset(
        "Too Much to Drink",
        ModuleType::Motion,
        "Wobble FX",
        "com.fts.wobble",
        &[],
    );

    // Add all module presets to the rig
    rig.add_module_preset(mp_source);
    rig.add_module_preset(mp_dynamics);
    rig.add_module_preset(mp_envelope);
    rig.add_module_preset(mp_wah);
    rig.add_module_preset(mp_pitch);
    rig.add_module_preset(mp_doubler);
    rig.add_module_preset(mp_blues_stack);
    rig.add_module_preset(mp_protein);
    rig.add_module_preset(mp_gravity_tank);
    rig.add_module_preset(mp_dream_ruby);
    rig.add_module_preset(mp_deluxe_ac30);
    rig.add_module_preset(mp_dumble);
    rig.add_module_preset(mp_marshall);
    rig.add_module_preset(mp_chorus);
    rig.add_module_preset(mp_flanger);
    rig.add_module_preset(mp_phaser);
    rig.add_module_preset(mp_delay);
    rig.add_module_preset(mp_reverb);
    rig.add_module_preset(mp_freeze);
    rig.add_module_preset(mp_trem8);
    rig.add_module_preset(mp_trem16);
    rig.add_module_preset(mp_vibrato);
    rig.add_module_preset(mp_rotary);
    rig.add_module_preset(mp_toomuch);

    // ── Snapshot ID lookups (from rig's stored presets) ──────────────────
    let dream_clean_snap = rig
        .get_module_preset(mp_dream_ruby_id)
        .unwrap()
        .snapshot_by_name("Clean")
        .unwrap()
        .id;
    let deluxe_breakup_snap = rig
        .get_module_preset(mp_deluxe_ac30_id)
        .unwrap()
        .snapshot_by_name("Breakup")
        .unwrap()
        .id;
    let dumble_breakup_snap = rig
        .get_module_preset(mp_dumble_id)
        .unwrap()
        .snapshot_by_name("Breakup")
        .unwrap()
        .id;
    let marshall_drive_snap = rig
        .get_module_preset(mp_marshall_id)
        .unwrap()
        .snapshot_by_name("Drive")
        .unwrap()
        .id;
    let blues_halfman_teal_snap = rig
        .get_module_preset(mp_blues_stack_id)
        .unwrap()
        .snapshot_by_name("Halfman + Teal")
        .unwrap()
        .id;
    let protein_blue_green_snap = rig
        .get_module_preset(mp_protein_id)
        .unwrap()
        .snapshot_by_name("Blue + Green")
        .unwrap()
        .id;
    let gravity_spring_snap = rig
        .get_module_preset(mp_gravity_tank_id)
        .unwrap()
        .snapshot_by_name("Spring Reverb Light")
        .unwrap()
        .id;

    // ── 1b. Global Block ─────────────────────────────────────────────────
    let global_eq = GlobalBlock {
        block: Block::new("Global EQ", PluginId::vst3("com.fts.globaleq", "Global EQ")),
        order: Order::new(0),
        tags: Tags::new(),
    };
    rig.add_global_block(global_eq);

    // ── 2. Top-Level Presets ─────────────────────────────────────────────
    // Preset 1: AC30 Ambient Clean
    let preset_ac30 = PresetBuilder::new()
        .name("AC30 Ambient Clean")
        .category(PresetCategory::Generic {
            base_tone: BaseTone::Clean,
        })
        .rating(Rating::new(5))
        .description("Lush ambient clean with chorus and reverb")
        .credit(Credit::new("Cody Wright", "Sound Designer"))
        .inspiration(
            Inspiration::new("The Edge")
                .with_source("U2")
                .with_notes("Shimmer clean tones"),
        )
        .module_assignment(
            ModuleAssignment::new(ModuleType::Amp, mp_dream_ruby_id, Order::new(0))
                .with_snapshot(dream_clean_snap),
        )
        .module_assignment(ModuleAssignment::new(
            ModuleType::Time,
            mp_reverb_id,
            Order::new(1),
        ))
        .module_assignment(ModuleAssignment::new(
            ModuleType::Modulation,
            mp_chorus_id,
            Order::new(2),
        ))
        .snapshot(Snapshot::new("Default"))
        .build();
    let preset_ac30_id = preset_ac30.id;

    // Preset 2: Tremolo Swells
    let preset_trem = PresetBuilder::new()
        .name("Tremolo Swells")
        .category(PresetCategory::Generic {
            base_tone: BaseTone::Clean,
        })
        .rating(Rating::new(4))
        .description("Clean with rhythmic tremolo movement")
        .credit(Credit::new("Cody Wright", "Sound Designer"))
        .inspiration(
            Inspiration::new("Daniel Lanois")
                .with_source("Ambient Works")
                .with_notes("Tremolo movement on pads"),
        )
        .module_assignment(
            ModuleAssignment::new(ModuleType::Amp, mp_dream_ruby_id, Order::new(0))
                .with_snapshot(dream_clean_snap),
        )
        .module_assignment(ModuleAssignment::new(
            ModuleType::Motion,
            mp_trem8_id,
            Order::new(1),
        ))
        .build();
    let preset_trem_id = preset_trem.id;

    // Preset 3: 80's Drive
    let preset_80s = PresetBuilder::new()
        .name("80's Drive")
        .category(PresetCategory::Genre {
            base_tone: BaseTone::Drive,
            genre: Genre::Blues,
        })
        .rating(Rating::new(4))
        .description("Classic blues drive with 80s character")
        .credit(Credit::new("Cody Wright", "Sound Designer"))
        .credit(Credit::new("FTS Community", "Beta Tester"))
        .inspiration(
            Inspiration::new("Stevie Ray Vaughan")
                .with_source("Texas Flood")
                .with_notes("Tube Screamer into Fender"),
        )
        .module_assignment(
            ModuleAssignment::new(ModuleType::Drive, mp_blues_stack_id, Order::new(0))
                .with_snapshot(blues_halfman_teal_snap),
        )
        .module_assignment(
            ModuleAssignment::new(ModuleType::Amp, mp_deluxe_ac30_id, Order::new(1))
                .with_snapshot(deluxe_breakup_snap),
        )
        .build();
    let preset_80s_id = preset_80s.id;

    // Preset 4: Stank
    let preset_stank = PresetBuilder::new()
        .name("Stank")
        .category(PresetCategory::Genre {
            base_tone: BaseTone::Drive,
            genre: Genre::Rock,
        })
        .rating(Rating::new(5))
        .description("High-gain rock drive for aggressive riffing")
        .credit(Credit::new("Cody Wright", "Sound Designer"))
        .inspiration(
            Inspiration::new("Tom Morello")
                .with_source("Rage Against the Machine")
                .with_notes("Aggressive heavy tones"),
        )
        .module_assignment(
            ModuleAssignment::new(ModuleType::Drive, mp_protein_id, Order::new(0))
                .with_snapshot(protein_blue_green_snap),
        )
        .module_assignment(
            ModuleAssignment::new(ModuleType::Amp, mp_marshall_id, Order::new(1))
                .with_snapshot(marshall_drive_snap),
        )
        .build();
    let preset_stank_id = preset_stank.id;

    // Preset 5: Edge of Breakup
    let preset_edge = PresetBuilder::new()
        .name("Edge of Breakup")
        .category(PresetCategory::Genre {
            base_tone: BaseTone::Crunch,
            genre: Genre::Blues,
        })
        .rating(Rating::new(5))
        .description("Dynamic crunch that cleans up with volume knob")
        .credit(Credit::new("Cody Wright", "Sound Designer"))
        .inspiration(
            Inspiration::new("John Mayer")
                .with_source("Continuum")
                .with_notes("Edge-of-breakup Dumble tones"),
        )
        .inspiration(Inspiration::new("Robben Ford").with_notes("Two-Rock clarity"))
        .module_assignment(
            ModuleAssignment::new(ModuleType::Amp, mp_dumble_id, Order::new(0))
                .with_snapshot(dumble_breakup_snap),
        )
        .build();
    let preset_edge_id = preset_edge.id;

    let presets = vec![preset_ac30, preset_trem, preset_80s, preset_stank, preset_edge];

    // ── 3. Profiles ──────────────────────────────────────────────────────
    // Profile: Worship (8 scene templates)
    let mut profile_worship = Profile::new("Worship", rig.id);
    {
        let mut clean_template = SceneTemplate::direct("Clean", preset_ac30_id);
        clean_template.add_module_override(ModuleOverride::swap_preset(
            ModuleType::PreFx,
            mp_gravity_tank_id,
            Some(gravity_spring_snap),
        ));
        profile_worship.add_scene_template(clean_template);
    }
    profile_worship.add_scene_template(SceneTemplate::direct("Crunch", preset_edge_id));
    profile_worship.add_scene_template(SceneTemplate::direct("Drive", preset_80s_id));
    profile_worship.add_scene_template(SceneTemplate::direct("Lead", preset_stank_id));
    profile_worship.add_scene_template(SceneTemplate::direct("Ambient", preset_ac30_id));
    profile_worship.add_scene_template(SceneTemplate::direct("Tremolo", preset_trem_id));
    profile_worship.add_scene_template(SceneTemplate::direct("Delay", preset_ac30_id));
    profile_worship.add_scene_template(SceneTemplate::direct("Solo", preset_stank_id));

    // Profile: Blues (7 scene templates)
    let mut profile_blues = Profile::new("Blues", rig.id);
    profile_blues.add_scene_template(SceneTemplate::direct("Clean", preset_ac30_id));
    profile_blues.add_scene_template(SceneTemplate::direct("Crunch", preset_edge_id));
    profile_blues.add_scene_template(SceneTemplate::direct("Drive", preset_80s_id));
    profile_blues.add_scene_template(SceneTemplate::direct("Lead", preset_80s_id));
    profile_blues.add_scene_template(SceneTemplate::direct("Funk", preset_ac30_id));
    profile_blues.add_scene_template(SceneTemplate::direct("Q-Tron", preset_ac30_id));
    profile_blues.add_scene_template(SceneTemplate::direct("Solo", preset_stank_id));

    // Profile: Rock (8 scene templates)
    let mut profile_rock = Profile::new("Rock", rig.id);
    profile_rock.add_scene_template(SceneTemplate::direct("Clean", preset_ac30_id));
    profile_rock.add_scene_template(SceneTemplate::direct("Crunch", preset_edge_id));
    profile_rock.add_scene_template(SceneTemplate::direct("Drive", preset_stank_id));
    profile_rock.add_scene_template(SceneTemplate::direct("Lead", preset_stank_id));
    profile_rock.add_scene_template(SceneTemplate::direct("Ambient", preset_ac30_id));
    profile_rock.add_scene_template(SceneTemplate::direct("Phaser", preset_ac30_id));
    profile_rock.add_scene_template(SceneTemplate::direct("DLY Lead", preset_80s_id));
    profile_rock.add_scene_template(SceneTemplate::direct("Solo", preset_stank_id));

    let profiles = vec![profile_worship, profile_blues, profile_rock];

    // ── 4. Songs ─────────────────────────────────────────────────────────
    // Song 1: Cryin' (Mateus Asato)
    let mut song_cryin = PerformanceSong::new("Cryin' (Mateus Asato)");
    song_cryin.add_scene(Scene::new("Ambient", preset_ac30_id));
    song_cryin.add_scene(Scene::new("Rhythm", preset_edge_id));
    song_cryin.add_scene(Scene::new("Lead", preset_80s_id));
    song_cryin.add_scene(Scene::new("Solo", preset_stank_id));
    // Song-level module override: PostFx swapped to Delay
    song_cryin.add_module_override(ModuleOverride::swap_preset(
        ModuleType::Time,
        mp_delay_id,
        None,
    ));

    // Song 2: Thriller (Dirty Loops)
    let mut song_thriller = PerformanceSong::new("Thriller (Dirty Loops)");
    song_thriller.add_scene(Scene::new("Crunch", preset_edge_id));
    song_thriller.add_scene(Scene::new("Rock Lead", preset_stank_id));
    {
        let mut bridge_scene = Scene::new("Bridge", preset_ac30_id);
        bridge_scene.add_module_override(ModuleOverride::swap_preset(
            ModuleType::Modulation,
            mp_phaser_id,
            None,
        ));
        song_thriller.add_scene(bridge_scene);
    }
    song_thriller.add_scene(Scene::new("Solo", preset_80s_id));
    song_thriller.add_scene(Scene::new("Ambient", preset_ac30_id));

    // Song 3: Girl Goodbye (Toto)
    let mut song_girl = PerformanceSong::new("Girl Goodbye (Toto)");
    song_girl.add_scene(Scene::new("Drive", preset_80s_id));
    song_girl.add_scene(Scene::new("Verse", preset_edge_id));
    song_girl.add_scene(Scene::new("Chorus", preset_stank_id));
    song_girl.add_scene(Scene::new("Solo", preset_stank_id));

    let songs = vec![song_cryin, song_thriller, song_girl];

    // ── 5. Setlist ───────────────────────────────────────────────────────
    let mut setlist = PerformanceSetlist::new("Friday Night Gig");
    // Clone songs for the setlist since we also keep them in the fixture
    for song in &songs {
        setlist.add_song(song.clone());
    }

    // ── 6. Global Module Override ────────────────────────────────────────
    let global_amp_override =
        GlobalModuleOverride::locked(ModuleType::Amp, mp_dream_ruby_id)
            .with_snapshot(dream_clean_snap);
    rig.add_global_override(global_amp_override);

    GuitarRigFixture {
        rig,
        presets,
        profiles,
        songs,
        setlist,
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn module_presets_have_correct_snapshot_counts() {
    let fixture = build_guitar_rig();
    let rig = &fixture.rig;

    // Find module presets by name and verify snapshot counts
    let find_mp = |name: &str| -> &ModulePreset {
        rig.module_presets
            .iter()
            .find(|mp| mp.name == name)
            .unwrap_or_else(|| panic!("Module preset '{name}' not found"))
    };

    // Source: Guitar Input -> 3 snapshots
    assert_eq!(find_mp("Guitar Input").snapshots.len(), 3);

    // Dynamics: Compressor -> 0 snapshots
    assert_eq!(find_mp("Compressor").snapshots.len(), 0);

    // Drive: Blues Stack -> 3 snapshots
    assert_eq!(find_mp("Blues Stack").snapshots.len(), 3);

    // Drive: Protein + JHS Kilt -> 5 snapshots
    assert_eq!(find_mp("Protein + JHS Kilt").snapshots.len(), 5);

    // PreFx: Gravity Tank -> 4 snapshots
    assert_eq!(find_mp("Gravity Tank").snapshots.len(), 4);

    // Amp: Dream and Ruby -> 3 snapshots
    assert_eq!(find_mp("Dream and Ruby").snapshots.len(), 3);

    // Amp: Deluxe and AC30 -> 3 snapshots
    assert_eq!(find_mp("Deluxe and AC30").snapshots.len(), 3);

    // Amp: Dumble and Two-Rock -> 4 snapshots
    assert_eq!(find_mp("Dumble and Two-Rock").snapshots.len(), 4);

    // Amp: Marshall Stack -> 2 snapshots
    assert_eq!(find_mp("Marshall Stack").snapshots.len(), 2);

    // Modulation presets: 0 snapshots each
    assert_eq!(find_mp("Chorus").snapshots.len(), 0);
    assert_eq!(find_mp("Flanger").snapshots.len(), 0);
    assert_eq!(find_mp("Phaser").snapshots.len(), 0);

    // Time presets: 0 snapshots each
    assert_eq!(find_mp("Delay").snapshots.len(), 0);
    assert_eq!(find_mp("Reverb").snapshots.len(), 0);
    assert_eq!(find_mp("Freeze").snapshots.len(), 0);

    // Motion presets: 0 snapshots each
    assert_eq!(find_mp("Tremolo 8th").snapshots.len(), 0);
    assert_eq!(find_mp("Tremolo 16th").snapshots.len(), 0);
    assert_eq!(find_mp("Vibrato").snapshots.len(), 0);
    assert_eq!(find_mp("Rotary").snapshots.len(), 0);
    assert_eq!(find_mp("Too Much to Drink").snapshots.len(), 0);

    // Special presets: 0 snapshots each
    assert_eq!(find_mp("Envelope Filter").snapshots.len(), 0);
    assert_eq!(find_mp("Wah Pedal").snapshots.len(), 0);
    assert_eq!(find_mp("Pitch Octave FX").snapshots.len(), 0);
    assert_eq!(find_mp("Doubler").snapshots.len(), 0);

    // Total module presets in rig
    assert_eq!(rig.module_presets.len(), 24);
}

#[test]
fn top_level_presets_have_module_assignments() {
    let fixture = build_guitar_rig();

    let find_preset = |name: &str| -> &Preset {
        fixture
            .presets
            .iter()
            .find(|p| p.name == name)
            .unwrap_or_else(|| panic!("Preset '{name}' not found"))
    };

    // AC30 Ambient Clean: Amp, Time, Modulation
    let ac30 = find_preset("AC30 Ambient Clean");
    assert!(ac30.get_module_assignment(ModuleType::Amp).is_some());
    assert!(ac30.get_module_assignment(ModuleType::Time).is_some());
    assert!(ac30.get_module_assignment(ModuleType::Modulation).is_some());
    assert_eq!(ac30.module_assignments.len(), 3);

    // Tremolo Swells: Amp, Motion
    let trem = find_preset("Tremolo Swells");
    assert!(trem.get_module_assignment(ModuleType::Amp).is_some());
    assert!(trem.get_module_assignment(ModuleType::Motion).is_some());
    assert_eq!(trem.module_assignments.len(), 2);

    // 80's Drive: Drive, Amp
    let eighties = find_preset("80's Drive");
    assert!(eighties.get_module_assignment(ModuleType::Drive).is_some());
    assert!(eighties.get_module_assignment(ModuleType::Amp).is_some());
    assert_eq!(eighties.module_assignments.len(), 2);

    // Stank: Drive, Amp
    let stank = find_preset("Stank");
    assert!(stank.get_module_assignment(ModuleType::Drive).is_some());
    assert!(stank.get_module_assignment(ModuleType::Amp).is_some());
    assert_eq!(stank.module_assignments.len(), 2);

    // Edge of Breakup: Amp
    let edge = find_preset("Edge of Breakup");
    assert!(edge.get_module_assignment(ModuleType::Amp).is_some());
    assert_eq!(edge.module_assignments.len(), 1);
}

#[test]
fn worship_clean_has_prefx_override() {
    let fixture = build_guitar_rig();
    let worship = &fixture.profiles[0];
    assert_eq!(worship.name, "Worship");

    let clean_template = worship
        .get_scene_template_by_name("Clean")
        .expect("Worship profile should have a 'Clean' scene template");

    assert_eq!(clean_template.module_overrides.len(), 1);
    let override_ = &clean_template.module_overrides[0];
    assert_eq!(override_.module_type, ModuleType::PreFx);
    assert!(override_.is_swap());
}

#[test]
fn song_level_override_applies() {
    let fixture = build_guitar_rig();
    let cryin = &fixture.songs[0];
    assert_eq!(cryin.name, "Cryin' (Mateus Asato)");

    // Song-level Time override
    assert_eq!(cryin.module_overrides.len(), 1);
    let override_ = &cryin.module_overrides[0];
    assert_eq!(override_.module_type, ModuleType::Time);
    assert!(override_.is_swap());
}

#[test]
fn global_override_locks_amp() {
    let fixture = build_guitar_rig();
    assert!(fixture.rig.is_module_locked(&ModuleType::Amp));
    // Other module types should not be locked
    assert!(!fixture.rig.is_module_locked(&ModuleType::Drive));
    assert!(!fixture.rig.is_module_locked(&ModuleType::Time));
    assert!(!fixture.rig.is_module_locked(&ModuleType::Modulation));
}

#[test]
fn setlist_has_all_songs() {
    let fixture = build_guitar_rig();
    assert_eq!(fixture.setlist.song_count(), 3);
    assert_eq!(fixture.setlist.songs[0].name, "Cryin' (Mateus Asato)");
    assert_eq!(fixture.setlist.songs[1].name, "Thriller (Dirty Loops)");
    assert_eq!(fixture.setlist.songs[2].name, "Girl Goodbye (Toto)");
}

#[test]
fn profiles_have_correct_template_counts() {
    let fixture = build_guitar_rig();

    let worship = &fixture.profiles[0];
    let blues = &fixture.profiles[1];
    let rock = &fixture.profiles[2];

    assert_eq!(worship.name, "Worship");
    assert_eq!(worship.scene_templates.len(), 8);

    assert_eq!(blues.name, "Blues");
    assert_eq!(blues.scene_templates.len(), 7);

    assert_eq!(rock.name, "Rock");
    assert_eq!(rock.scene_templates.len(), 8);
}

#[test]
fn module_preset_lookup_by_id() {
    let fixture = build_guitar_rig();
    let rig = &fixture.rig;

    // Pick a few module presets and verify lookup by ID
    for mp in &rig.module_presets {
        let found = rig
            .get_module_preset(mp.id)
            .expect("Should find module preset by its ID");
        assert_eq!(found.name, mp.name);
        assert_eq!(found.module_type, mp.module_type);
    }

    // Lookup with a non-existent ID should return None
    assert!(rig.get_module_preset(ModulePresetId::new()).is_none());
}

#[test]
fn scene_override_on_individual_scene() {
    let fixture = build_guitar_rig();

    // Thriller's "Bridge" scene (index 2) has a Modulation override
    let thriller = &fixture.songs[1];
    assert_eq!(thriller.name, "Thriller (Dirty Loops)");

    let bridge_scene = &thriller.scenes[2];
    assert_eq!(bridge_scene.name, "Bridge");
    assert_eq!(bridge_scene.module_overrides.len(), 1);

    let override_ = &bridge_scene.module_overrides[0];
    assert_eq!(override_.module_type, ModuleType::Modulation);
    assert!(override_.is_swap());
}

#[test]
fn module_reordering() {
    let fixture = build_guitar_rig();

    // The "80's Drive" preset has Drive at Order(0) and Amp at Order(1)
    let eighties = fixture
        .presets
        .iter()
        .find(|p| p.name == "80's Drive")
        .unwrap();

    let drive_assignment = eighties
        .get_module_assignment(ModuleType::Drive)
        .unwrap();
    let amp_assignment = eighties
        .get_module_assignment(ModuleType::Amp)
        .unwrap();

    assert_eq!(drive_assignment.order, Order::new(0));
    assert_eq!(amp_assignment.order, Order::new(1));

    // Drive should come before Amp in signal chain
    assert!(drive_assignment.order < amp_assignment.order);

    // The "AC30 Ambient Clean" has Amp(0), Time(1), Modulation(2)
    let ac30 = fixture
        .presets
        .iter()
        .find(|p| p.name == "AC30 Ambient Clean")
        .unwrap();

    let amp = ac30.get_module_assignment(ModuleType::Amp).unwrap();
    let time = ac30.get_module_assignment(ModuleType::Time).unwrap();
    let modulation = ac30
        .get_module_assignment(ModuleType::Modulation)
        .unwrap();

    assert_eq!(amp.order, Order::new(0));
    assert_eq!(time.order, Order::new(1));
    assert_eq!(modulation.order, Order::new(2));
    assert!(amp.order < time.order);
    assert!(time.order < modulation.order);
}

#[test]
fn credits_and_inspirations() {
    let fixture = build_guitar_rig();

    let find_preset = |name: &str| -> &Preset {
        fixture
            .presets
            .iter()
            .find(|p| p.name == name)
            .unwrap()
    };

    // AC30 Ambient Clean: 1 credit, 1 inspiration
    let ac30 = find_preset("AC30 Ambient Clean");
    assert_eq!(ac30.credits.len(), 1);
    assert_eq!(ac30.credits[0].name, "Cody Wright");
    assert_eq!(ac30.credits[0].role, "Sound Designer");
    assert_eq!(ac30.inspirations.len(), 1);
    assert_eq!(ac30.inspirations[0].name, "The Edge");
    assert_eq!(ac30.inspirations[0].source.as_deref(), Some("U2"));
    assert!(ac30.inspirations[0].notes.is_some());

    // 80's Drive: 2 credits, 1 inspiration
    let eighties = find_preset("80's Drive");
    assert_eq!(eighties.credits.len(), 2);
    assert_eq!(eighties.credits[1].name, "FTS Community");
    assert_eq!(eighties.credits[1].role, "Beta Tester");
    assert_eq!(eighties.inspirations.len(), 1);
    assert_eq!(eighties.inspirations[0].name, "Stevie Ray Vaughan");
    assert_eq!(
        eighties.inspirations[0].source.as_deref(),
        Some("Texas Flood")
    );

    // Edge of Breakup: 1 credit, 2 inspirations
    let edge = find_preset("Edge of Breakup");
    assert_eq!(edge.credits.len(), 1);
    assert_eq!(edge.inspirations.len(), 2);
    assert_eq!(edge.inspirations[0].name, "John Mayer");
    assert_eq!(
        edge.inspirations[0].source.as_deref(),
        Some("Continuum")
    );
    assert_eq!(edge.inspirations[1].name, "Robben Ford");
    assert!(edge.inspirations[1].source.is_none());
    assert!(edge.inspirations[1].notes.is_some());

    // Stank: 1 credit, 1 inspiration
    let stank = find_preset("Stank");
    assert_eq!(stank.credits.len(), 1);
    assert_eq!(stank.inspirations.len(), 1);
    assert_eq!(stank.inspirations[0].name, "Tom Morello");

    // Tremolo Swells: 1 credit, 1 inspiration
    let trem = find_preset("Tremolo Swells");
    assert_eq!(trem.credits.len(), 1);
    assert_eq!(trem.inspirations.len(), 1);
    assert_eq!(trem.inspirations[0].name, "Daniel Lanois");
}

#[test]
fn module_presets_have_default_snapshots() {
    let fixture = build_guitar_rig();
    let rig = &fixture.rig;

    let find_mp = |name: &str| -> &ModulePreset {
        rig.module_presets
            .iter()
            .find(|mp| mp.name == name)
            .unwrap()
    };

    // Module presets with snapshots should have a default snapshot set
    let blues_stack = find_mp("Blues Stack");
    assert!(blues_stack.default_snapshot_id.is_some());
    let default = blues_stack.default_snapshot().unwrap();
    assert_eq!(default.name, "Halfman"); // first snapshot is default

    let dream = find_mp("Dream and Ruby");
    assert!(dream.default_snapshot_id.is_some());
    let default = dream.default_snapshot().unwrap();
    assert_eq!(default.name, "Clean");

    let protein = find_mp("Protein + JHS Kilt");
    assert!(protein.default_snapshot_id.is_some());
    let default = protein.default_snapshot().unwrap();
    assert_eq!(default.name, "Blue Light");

    let gravity = find_mp("Gravity Tank");
    assert!(gravity.default_snapshot_id.is_some());
    let default = gravity.default_snapshot().unwrap();
    assert_eq!(default.name, "Harmonic Tremolo Light");

    // Module presets without snapshots should have no default
    let chorus = find_mp("Chorus");
    assert!(chorus.default_snapshot_id.is_none());
    assert!(chorus.default_snapshot().is_none());
}

#[test]
fn module_presets_have_at_least_one_block() {
    let fixture = build_guitar_rig();
    for mp in &fixture.rig.module_presets {
        assert!(
            !mp.blocks.is_empty(),
            "Module preset '{}' should have at least one block",
            mp.name
        );
    }
}

#[test]
fn presets_have_correct_categories() {
    let fixture = build_guitar_rig();

    let find_preset = |name: &str| -> &Preset {
        fixture
            .presets
            .iter()
            .find(|p| p.name == name)
            .unwrap()
    };

    // AC30 Ambient Clean: Generic Clean
    assert_eq!(
        find_preset("AC30 Ambient Clean").category.base_tone(),
        BaseTone::Clean
    );
    assert!(find_preset("AC30 Ambient Clean").category.genre().is_none());

    // 80's Drive: Genre Blues Drive
    let eighties = find_preset("80's Drive");
    assert_eq!(eighties.category.base_tone(), BaseTone::Drive);
    assert_eq!(eighties.category.genre(), Some(&Genre::Blues));

    // Stank: Genre Rock Drive
    let stank = find_preset("Stank");
    assert_eq!(stank.category.base_tone(), BaseTone::Drive);
    assert_eq!(stank.category.genre(), Some(&Genre::Rock));

    // Edge of Breakup: Genre Blues Crunch
    let edge = find_preset("Edge of Breakup");
    assert_eq!(edge.category.base_tone(), BaseTone::Crunch);
    assert_eq!(edge.category.genre(), Some(&Genre::Blues));
}

#[test]
fn songs_have_correct_scene_counts() {
    let fixture = build_guitar_rig();

    assert_eq!(fixture.songs[0].name, "Cryin' (Mateus Asato)");
    assert_eq!(fixture.songs[0].scene_count(), 4);

    assert_eq!(fixture.songs[1].name, "Thriller (Dirty Loops)");
    assert_eq!(fixture.songs[1].scene_count(), 5);

    assert_eq!(fixture.songs[2].name, "Girl Goodbye (Toto)");
    assert_eq!(fixture.songs[2].scene_count(), 4);
}

#[test]
fn rig_has_global_block() {
    let fixture = build_guitar_rig();
    assert_eq!(fixture.rig.global_blocks.len(), 1);
    assert_eq!(fixture.rig.global_blocks[0].block.name, "Global EQ");
    assert_eq!(fixture.rig.global_blocks[0].order, Order::new(0));
}

#[test]
fn rig_instrument_type_is_guitar() {
    let fixture = build_guitar_rig();
    assert_eq!(fixture.rig.instrument_type, InstrumentType::Guitar);
    assert_eq!(fixture.rig.name, "FTS-Guitar");
}

#[test]
fn global_override_has_snapshot() {
    let fixture = build_guitar_rig();
    let global_override = &fixture.rig.global_module_overrides[0];
    assert_eq!(global_override.module_type, ModuleType::Amp);
    assert!(global_override.locked);
    assert!(global_override.module_snapshot_id.is_some());

    // Verify the snapshot ID points to the "Clean" snapshot of "Dream and Ruby"
    let dream = fixture
        .rig
        .module_presets
        .iter()
        .find(|mp| mp.name == "Dream and Ruby")
        .unwrap();
    let clean_snap = dream.snapshot_by_name("Clean").unwrap();
    assert_eq!(global_override.module_snapshot_id, Some(clean_snap.id));
    assert_eq!(global_override.module_preset_id, dream.id);
}

#[test]
fn profile_default_template_is_first() {
    let fixture = build_guitar_rig();
    for profile in &fixture.profiles {
        let default = profile.default_scene_template().unwrap();
        assert_eq!(
            default.id, profile.scene_templates[0].id,
            "Default should be the first template in profile '{}'",
            profile.name
        );
    }
}

#[test]
fn module_assignment_snapshots_reference_valid_module_preset_snapshots() {
    let fixture = build_guitar_rig();

    // For each preset's module assignments that have a snapshot, verify that
    // the referenced module preset actually contains that snapshot.
    for preset in &fixture.presets {
        for assignment in &preset.module_assignments {
            if let Some(snap_id) = assignment.module_snapshot_id {
                let mp = fixture
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
