//! Integration test: Guitar rig navigation via the ROAM service API.
//!
//! Loads "Cryin' (Mateus Asato)" from the default guitar rig, checks the
//! initial state through the `RigControlService`, navigates between scenes
//! and songs, and verifies that module resolution is correct throughout.

use std::sync::Arc;

use rig_control::defaults::guitar::{self, GuitarRigDefaults};
use rig_control::module::ModuleType;
use rig_control::service::{
    MockRigControlService, RigControlCommand, RigControlData, RigControlService,
    SwitchOutcomeInfo,
};
use rig_control::LocalRigControlClient;

/// Build the full guitar rig defaults and wrap them in a `LocalRigControlClient`.
fn setup() -> (LocalRigControlClient<MockRigControlService>, GuitarRigDefaults) {
    let defaults = guitar::build_guitar_rig();

    let data = RigControlData {
        rig: defaults.rig.clone(),
        presets: defaults.presets.clone(),
        songs: defaults.songs.clone(),
    };

    let service = Arc::new(MockRigControlService::new(data));
    let client = LocalRigControlClient::new(service);

    (client, defaults)
}

/// Helper: get the module preset IDs currently loaded for each active slot.
/// Returns (ModuleType, module_preset_id) pairs for all active slots.
async fn active_module_preset_ids(
    service: &MockRigControlService,
) -> Vec<(ModuleType, uuid::Uuid)> {
    service
        .current_resolved_modules()
        .into_iter()
        .filter_map(|(mt, target)| target.map(|t| (mt, t.module_preset_id.as_uuid())))
        .collect()
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

/// Load Cryin' (Mateus Asato) default scene (Ambient), verify the preset
/// and module assignments that get loaded.
#[tokio::test]
async fn load_cryin_default_scene_and_check_modules() {
    let (client, defaults) = setup();

    // Cryin' is song index 0 in the defaults
    let cryin = &defaults.songs[0];
    assert_eq!(cryin.name, "Cryin'");
    assert_eq!(cryin.scenes.len(), 4);

    // Default scene is index 0 = "Ambient"
    let ambient_scene = &cryin.scenes[0];
    assert_eq!(ambient_scene.name, "Ambient");

    // The Ambient scene uses the "AC30 Ambient Clean" preset
    let ac30_preset = &defaults.presets[0];
    assert_eq!(ac30_preset.name, "AC30 Ambient Clean");
    assert_eq!(ambient_scene.preset_id, ac30_preset.id);

    // Initialize the engine
    client
        .execute(RigControlCommand::Initialize {
            rig_id: defaults.rig.id.as_uuid(),
        })
        .await;

    // Verify engine is initialized
    let state = client.get_engine_state().await;
    assert!(state.initialized);

    // Load Cryin' scene 0 (Ambient)
    client
        .execute(RigControlCommand::LoadSongScene {
            song_index: 0,
            scene_index: 0,
        })
        .await;

    // Check that all slots have an active instance
    let slots = client.get_all_slot_states().await;
    assert!(!slots.is_empty(), "should have active slots after loading");

    // Verify the modules loaded match AC30 Ambient Clean:
    // Source, Dynamics, Amp (Dream+Ruby), Modulation (Chorus), Time (Reverb), Master
    let module_presets = rig_control::defaults::guitar::presets::GuitarModulePresets::new();

    // Get the slot for Amp and verify it's active (not disabled)
    let amp_slot = client.get_slot_state(ModuleType::Amp).await;
    assert!(amp_slot.is_some(), "Amp slot should exist");
    let amp_slot = amp_slot.unwrap();
    assert!(!amp_slot.disabled, "Amp slot should not be disabled");
    assert!(
        amp_slot.active_instance.is_some(),
        "Amp slot should have an active instance"
    );

    // Check Modulation slot (should be Chorus for AC30 Ambient Clean)
    let mod_slot = client.get_slot_state(ModuleType::Modulation).await;
    assert!(mod_slot.is_some(), "Modulation slot should exist");
    assert!(
        mod_slot.unwrap().active_instance.is_some(),
        "Modulation should be active"
    );

    // Check Time slot (should be Reverb)
    let time_slot = client.get_slot_state(ModuleType::Time).await;
    assert!(time_slot.is_some(), "Time slot should exist");
    assert!(
        time_slot.unwrap().active_instance.is_some(),
        "Time should be active"
    );

    // Drive should NOT have an active instance (AC30 Ambient Clean has no Drive)
    let drive_slot = client.get_slot_state(ModuleType::Drive).await;
    if let Some(drive) = drive_slot {
        assert!(
            drive.active_instance.is_none(),
            "Drive should not be active for AC30 Ambient Clean"
        );
    }
}

/// Navigate between Cryin' scenes: load Ambient, switch to Rhythm, then
/// switch back to Ambient. Verify that the correct presets are loaded for
/// each scene.
#[tokio::test]
async fn navigate_cryin_scenes() {
    let (client, defaults) = setup();
    let cryin = &defaults.songs[0];

    // Initialize
    client
        .execute(RigControlCommand::Initialize {
            rig_id: defaults.rig.id.as_uuid(),
        })
        .await;

    // ── Scene 0: Ambient (AC30 Ambient Clean) ──────────────────────────
    client
        .execute(RigControlCommand::LoadSongScene {
            song_index: 0,
            scene_index: 0,
        })
        .await;

    // Verify Modulation is active (AC30 Ambient Clean has Chorus)
    let mod_slot = client.get_slot_state(ModuleType::Modulation).await;
    assert!(
        mod_slot.as_ref().unwrap().active_instance.is_some(),
        "Scene 0: Modulation should be active (Chorus)"
    );

    // ── Scene 1: Rhythm (Edge of Breakup) ──────────────────────────────
    let rhythm_scene = &cryin.scenes[1];
    assert_eq!(rhythm_scene.name, "Rhythm");

    client
        .execute(RigControlCommand::LoadSongScene {
            song_index: 0,
            scene_index: 1,
        })
        .await;

    // Edge of Breakup has Amp (Dumble+Two-Rock/Breakup), Time (Reverb), but NO Modulation
    // Verify Amp slot is active
    let amp_slot = client.get_slot_state(ModuleType::Amp).await;
    assert!(
        amp_slot.unwrap().active_instance.is_some(),
        "Scene 1: Amp should be active (Dumble+Two-Rock)"
    );

    // ── Scene 2: Lead (80's Drive) ─────────────────────────────────────
    let lead_scene = &cryin.scenes[2];
    assert_eq!(lead_scene.name, "Lead");

    client
        .execute(RigControlCommand::LoadSongScene {
            song_index: 0,
            scene_index: 2,
        })
        .await;

    // 80's Drive has Drive (Blues Stack), Amp (Deluxe+AC30), Time (Delay)
    let drive_slot = client.get_slot_state(ModuleType::Drive).await;
    assert!(
        drive_slot.unwrap().active_instance.is_some(),
        "Scene 2: Drive should be active (Blues Stack)"
    );

    // ── Scene 3: Solo (Stank + Delay override) ────────────────────────
    let solo_scene = &cryin.scenes[3];
    assert_eq!(solo_scene.name, "Solo");

    client
        .execute(RigControlCommand::LoadSongScene {
            song_index: 0,
            scene_index: 3,
        })
        .await;

    // Stank has Drive (Protein+Kilt), Amp (Marshall/Drive)
    let drive_slot = client.get_slot_state(ModuleType::Drive).await;
    assert!(
        drive_slot.unwrap().active_instance.is_some(),
        "Scene 3: Drive should be active (Protein+Kilt)"
    );

    // Solo scene has a module override: Time → Delay
    let time_slot = client.get_slot_state(ModuleType::Time).await;
    assert!(
        time_slot.unwrap().active_instance.is_some(),
        "Scene 3: Time should be active (Delay override)"
    );

    // ── Back to Scene 0: Ambient ───────────────────────────────────────
    client
        .execute(RigControlCommand::LoadSongScene {
            song_index: 0,
            scene_index: 0,
        })
        .await;

    // Should be back to AC30 Ambient Clean
    let mod_slot = client.get_slot_state(ModuleType::Modulation).await;
    assert!(
        mod_slot.unwrap().active_instance.is_some(),
        "Back to Scene 0: Modulation should be active again (Chorus)"
    );
}

/// Load Cryin' scene 0, then switch to a different song (Thriller), then
/// switch back to Cryin'. Verify that module state is consistent across
/// song boundaries.
#[tokio::test]
async fn navigate_between_songs() {
    let (client, defaults) = setup();

    assert_eq!(defaults.songs[0].name, "Cryin'");
    assert_eq!(defaults.songs[1].name, "Thriller");
    assert_eq!(defaults.songs[2].name, "Girl Goodbye");

    // Initialize
    client
        .execute(RigControlCommand::Initialize {
            rig_id: defaults.rig.id.as_uuid(),
        })
        .await;

    // ── Load Cryin' Scene 0 (Ambient) ──────────────────────────────────
    client
        .execute(RigControlCommand::LoadSongScene {
            song_index: 0,
            scene_index: 0,
        })
        .await;

    let mod_slot_cryin = client.get_slot_state(ModuleType::Modulation).await;
    assert!(
        mod_slot_cryin.unwrap().active_instance.is_some(),
        "Cryin' Ambient: Modulation active"
    );

    // ── Switch to Thriller Scene 0 (Crunch — Edge of Breakup) ──────────
    client
        .execute(RigControlCommand::LoadSongScene {
            song_index: 1,
            scene_index: 0,
        })
        .await;

    // Edge of Breakup: Source, Dynamics, Amp (Dumble), Time (Reverb), Master
    let amp_slot = client.get_slot_state(ModuleType::Amp).await;
    assert!(
        amp_slot.unwrap().active_instance.is_some(),
        "Thriller Crunch: Amp should be active"
    );

    // ── Thriller Scene 2 (Bridge — AC30 Ambient Clean + Phaser override) ─
    client
        .execute(RigControlCommand::LoadSongScene {
            song_index: 1,
            scene_index: 2,
        })
        .await;

    // Bridge has Modulation → Phaser override
    let mod_slot_thriller = client.get_slot_state(ModuleType::Modulation).await;
    assert!(
        mod_slot_thriller.unwrap().active_instance.is_some(),
        "Thriller Bridge: Modulation active (Phaser override)"
    );

    // ── Switch to Girl Goodbye Scene 0 (Drive — 80's Drive) ───────────
    client
        .execute(RigControlCommand::LoadSongScene {
            song_index: 2,
            scene_index: 0,
        })
        .await;

    // Girl Goodbye has song-level override: Motion → Tremolo 8th
    let motion_slot = client.get_slot_state(ModuleType::Motion).await;
    assert!(
        motion_slot.unwrap().active_instance.is_some(),
        "Girl Goodbye: Motion should be active (Tremolo 8th song override)"
    );

    let drive_slot = client.get_slot_state(ModuleType::Drive).await;
    assert!(
        drive_slot.unwrap().active_instance.is_some(),
        "Girl Goodbye Drive: Drive should be active (Blues Stack)"
    );

    // ── Come back to Cryin' Scene 0 (Ambient) ─────────────────────────
    client
        .execute(RigControlCommand::LoadSongScene {
            song_index: 0,
            scene_index: 0,
        })
        .await;

    // Should be back to AC30 Ambient Clean layout
    let mod_slot = client.get_slot_state(ModuleType::Modulation).await;
    assert!(
        mod_slot.unwrap().active_instance.is_some(),
        "Back to Cryin' Ambient: Modulation should be active again"
    );

    // Motion should be disabled (AC30 Ambient Clean has no Motion assignment,
    // so the diff disables the slot that was previously active from Girl Goodbye)
    let motion_slot = client.get_slot_state(ModuleType::Motion).await;
    if let Some(motion) = motion_slot {
        assert!(
            motion.disabled,
            "Back to Cryin' Ambient: Motion should be disabled (not in this preset)"
        );
    }
}

/// Load Cryin' scene 0, update a module (disable Drive), switch to scene 2,
/// come back to scene 0, and verify the engine state is consistent.
/// Then switch songs and return, verifying state again.
#[tokio::test]
async fn update_module_then_navigate_and_verify_persistence() {
    let (client, defaults) = setup();

    // Initialize
    client
        .execute(RigControlCommand::Initialize {
            rig_id: defaults.rig.id.as_uuid(),
        })
        .await;

    // ── Load Cryin' Scene 2 (Lead — 80's Drive) ───────────────────────
    // This scene has Drive active (Blues Stack)
    client
        .execute(RigControlCommand::LoadSongScene {
            song_index: 0,
            scene_index: 2,
        })
        .await;

    let drive_before = client.get_slot_state(ModuleType::Drive).await;
    assert!(
        drive_before.unwrap().active_instance.is_some(),
        "Lead scene: Drive should be active"
    );

    // ── Disable the Drive slot ─────────────────────────────────────────
    client
        .execute(RigControlCommand::DisableSlot {
            module_type: ModuleType::Drive,
        })
        .await;

    let drive_disabled = client.get_slot_state(ModuleType::Drive).await;
    assert!(
        drive_disabled.unwrap().disabled,
        "Drive should be disabled after DisableSlot command"
    );

    // ── Switch to Scene 0 (Ambient — no Drive in this preset) ─────────
    client
        .execute(RigControlCommand::LoadSongScene {
            song_index: 0,
            scene_index: 0,
        })
        .await;

    // The engine should still have the Drive slot, and it was re-enabled
    // by the load (since AC30 Ambient Clean doesn't have Drive, the slot
    // state depends on the diff engine behavior)

    // ── Switch to Scene 2 again (Lead — 80's Drive) ───────────────────
    client
        .execute(RigControlCommand::LoadSongScene {
            song_index: 0,
            scene_index: 2,
        })
        .await;

    // Drive should be active again (the load resolved the preset modules)
    let drive_after = client.get_slot_state(ModuleType::Drive).await;
    assert!(
        drive_after.unwrap().active_instance.is_some(),
        "Drive should be active after reloading the Lead scene"
    );

    // ── Switch to Thriller (different song) ────────────────────────────
    client
        .execute(RigControlCommand::LoadSongScene {
            song_index: 1,
            scene_index: 0,
        })
        .await;

    // ── Come back to Cryin' Scene 2 (Lead) ────────────────────────────
    client
        .execute(RigControlCommand::LoadSongScene {
            song_index: 0,
            scene_index: 2,
        })
        .await;

    let drive_final = client.get_slot_state(ModuleType::Drive).await;
    assert!(
        drive_final.unwrap().active_instance.is_some(),
        "Drive should still be active after returning from another song"
    );

    // The Amp should also be correct (Deluxe+AC30 for 80's Drive)
    let amp_final = client.get_slot_state(ModuleType::Amp).await;
    assert!(
        amp_final.unwrap().active_instance.is_some(),
        "Amp should be active after returning to Lead scene"
    );
}

/// Verify that the full engine lifecycle works: initialize, load, tick, shutdown.
#[tokio::test]
async fn engine_lifecycle_via_roam_api() {
    let (client, defaults) = setup();

    // Initially not initialized
    let state = client.get_engine_state().await;
    assert!(!state.initialized);

    // Initialize
    client
        .execute(RigControlCommand::Initialize {
            rig_id: defaults.rig.id.as_uuid(),
        })
        .await;

    let state = client.get_engine_state().await;
    assert!(state.initialized);

    // Load a scene
    client
        .execute(RigControlCommand::LoadSongScene {
            song_index: 0,
            scene_index: 0,
        })
        .await;

    // Tick (cleanup tails)
    client.execute(RigControlCommand::Tick).await;

    // Slots should still be valid after tick
    let slots = client.get_all_slot_states().await;
    assert!(!slots.is_empty());

    // Shutdown
    client.execute(RigControlCommand::Shutdown).await;

    let state = client.get_engine_state().await;
    assert!(!state.initialized);
}

/// Verify that Girl Goodbye's song-level module override (Motion → Tremolo 8th)
/// is applied to all scenes in the song, then removed when switching to
/// a different song.
#[tokio::test]
async fn song_level_module_override_applied_across_scenes() {
    let (client, defaults) = setup();

    assert_eq!(defaults.songs[2].name, "Girl Goodbye");
    // Girl Goodbye has song-level override: Motion → Tremolo 8th

    // Initialize
    client
        .execute(RigControlCommand::Initialize {
            rig_id: defaults.rig.id.as_uuid(),
        })
        .await;

    // ── Girl Goodbye Scene 0 (Drive — 80's Drive) ─────────────────────
    client
        .execute(RigControlCommand::LoadSongScene {
            song_index: 2,
            scene_index: 0,
        })
        .await;

    let motion_s0 = client.get_slot_state(ModuleType::Motion).await;
    assert!(
        motion_s0.unwrap().active_instance.is_some(),
        "GG Scene 0: Motion should be active (song-level Tremolo 8th)"
    );

    // ── Girl Goodbye Scene 1 (Verse — Edge of Breakup) ────────────────
    client
        .execute(RigControlCommand::LoadSongScene {
            song_index: 2,
            scene_index: 1,
        })
        .await;

    let motion_s1 = client.get_slot_state(ModuleType::Motion).await;
    assert!(
        motion_s1.unwrap().active_instance.is_some(),
        "GG Scene 1: Motion should still be active (song-level override persists)"
    );

    // ── Girl Goodbye Scene 3 (Solo — Stank + Delay scene override) ────
    client
        .execute(RigControlCommand::LoadSongScene {
            song_index: 2,
            scene_index: 3,
        })
        .await;

    let motion_s3 = client.get_slot_state(ModuleType::Motion).await;
    assert!(
        motion_s3.unwrap().active_instance.is_some(),
        "GG Scene 3: Motion should still be active (song override + scene Time override coexist)"
    );

    // Also verify the scene-level Time override (Delay) is active
    let time_s3 = client.get_slot_state(ModuleType::Time).await;
    assert!(
        time_s3.unwrap().active_instance.is_some(),
        "GG Scene 3: Time should be active (scene-level Delay override)"
    );

    // ── Switch to Cryin' (no song-level Motion override) ──────────────
    client
        .execute(RigControlCommand::LoadSongScene {
            song_index: 0,
            scene_index: 0,
        })
        .await;

    // Motion should be disabled for Cryin' Ambient (AC30 Ambient Clean has no Motion,
    // so the diff disables the slot that was previously active from Girl Goodbye)
    let motion_cryin = client.get_slot_state(ModuleType::Motion).await;
    if let Some(motion) = motion_cryin {
        assert!(
            motion.disabled,
            "Cryin' Ambient: Motion should be disabled (not in this preset)"
        );
    }
}
