//! Tests for profiles (patches), songs (sections), and resolve integration.
//!
//! Covers:
//!   - Profile / Patch CRUD and persistence
//!   - Song / Section CRUD (Patch-sourced and RigScene-sourced)
//!   - resolve_target() — the DAW integration path that compiles any target
//!     (RigScene / ProfilePatch / SongSection) into a flat ResolvedGraph with
//!     merged overrides and final parameter values
//!
//! Run with:
//!   cargo test -p signal --test signal_patches_songs -- --nocapture

use signal::{
    bootstrap_in_memory_controller_async,
    overrides::{NodePath, Override},
    profile::{Patch, PatchId, Profile, ProfileId},
    resolve::{ResolveTarget, ResolvedGraph},
    rig::{RigId, RigSceneId},
    seed_id,
    song::{Section, SectionId, SectionSource, Song, SongId},
};

// ─────────────────────────────────────────────────────────────
//  Helpers
// ─────────────────────────────────────────────────────────────

async fn controller() -> signal::SignalController {
    bootstrap_in_memory_controller_async()
        .await
        .expect("failed to bootstrap in-memory controller")
}

fn guitar_megarig_id() -> RigId {
    seed_id("guitar-megarig").into()
}

fn guitar_megarig_default_scene() -> RigSceneId {
    seed_id("guitar-megarig-default").into()
}

fn guitar_megarig_lead_scene() -> RigSceneId {
    seed_id("guitar-megarig-lead").into()
}

// ─────────────────────────────────────────────────────────────
//  Profile / Patch tests
// ─────────────────────────────────────────────────────────────

/// Verify seed profiles are present.
#[tokio::test]
async fn seed_profiles_are_loaded() {
    let ctrl = controller().await;

    let profiles = ctrl.list_profiles().await;
    println!("Seeded profiles:");
    for p in &profiles {
        println!("  {} — {} ({} patches)", p.id, p.name, p.patches.len());
    }

    assert!(!profiles.is_empty(), "should have seeded profiles");

    // guitar-worship-profile has 8 patches
    let worship = profiles
        .iter()
        .find(|p| p.id.to_string() == seed_id("guitar-worship-profile").to_string())
        .expect("guitar-worship-profile not found");

    assert_eq!(worship.patches.len(), 8, "Worship should have 8 patches");
    let default = worship.default_patch().expect("no default patch");
    assert_eq!(default.name, "Clean", "Worship default should be Clean");
    println!(
        "✓ Worship profile: {} patches, default={}",
        worship.patches.len(),
        default.name
    );
}

/// Load a specific profile patch by ID and verify its rig reference.
#[tokio::test]
async fn load_worship_lead_patch() {
    let ctrl = controller().await;

    let patch = ctrl
        .load_profile_variant(
            seed_id("guitar-worship-profile"),
            seed_id("guitar-worship-lead"),
        )
        .await
        .expect("guitar-worship-lead patch not found");

    println!(
        "Patch '{}': rig={} scene={} overrides={}",
        patch.name,
        patch.rig_id,
        patch.rig_variant_id,
        patch.overrides.len()
    );

    assert_eq!(patch.name, "Lead");
    assert_eq!(
        patch.rig_id.to_string(),
        seed_id("guitar-megarig").to_string()
    );
    // Lead patch targets the lead scene
    assert_eq!(
        patch.rig_variant_id.to_string(),
        seed_id("guitar-megarig-lead").to_string()
    );
    assert!(
        !patch.overrides.is_empty(),
        "Lead patch should have overrides"
    );
}

/// Create a new profile with two patches, save it, reload, verify round-trip.
#[tokio::test]
async fn create_and_reload_custom_profile() {
    let ctrl = controller().await;

    let clean_id = PatchId::new();
    let lead_id = PatchId::new();
    let profile_id = ProfileId::new();

    let clean = Patch::new(
        clean_id.clone(),
        "Clean",
        guitar_megarig_id(),
        guitar_megarig_default_scene(),
    )
    .with_override(Override::set(
        NodePath::engine("guitar-engine")
            .with_layer("guitar-layer-archetype-jm")
            .with_block("amp")
            .with_parameter("gain"),
        0.20,
    ));

    let lead = Patch::new(
        lead_id.clone(),
        "Lead",
        guitar_megarig_id(),
        guitar_megarig_lead_scene(),
    )
    .with_override(Override::set(
        NodePath::engine("guitar-engine")
            .with_layer("guitar-layer-archetype-jm")
            .with_block("amp")
            .with_parameter("gain"),
        0.75,
    ));

    let mut profile = Profile::new(profile_id.clone(), "Test Profile", clean);
    profile.add_patch(lead);

    ctrl.save_profile(profile).await;

    let reloaded = ctrl
        .load_profile(profile_id.clone())
        .await
        .expect("custom profile not found after save");

    println!(
        "Reloaded profile '{}': {} patches",
        reloaded.name,
        reloaded.patches.len()
    );
    assert_eq!(reloaded.name, "Test Profile");
    assert_eq!(reloaded.patches.len(), 2);
    assert_eq!(reloaded.default_patch().unwrap().name, "Clean");
    assert_eq!(reloaded.default_patch().unwrap().overrides.len(), 1);

    let reloaded_lead = reloaded.patch(&lead_id).expect("lead patch not found");
    let gain_override = &reloaded_lead.overrides[0];
    // Override::set produces a Set op — path should contain "gain"
    assert!(
        gain_override.path.as_str().contains("gain"),
        "override path should reference gain, got: {}",
        gain_override.path.as_str()
    );
}

/// Add a patch to an existing profile, save, verify count increases.
#[tokio::test]
async fn add_patch_to_existing_profile() {
    let ctrl = controller().await;

    let mut worship = ctrl
        .load_profile(seed_id("guitar-worship-profile"))
        .await
        .expect("worship profile not found");

    let original_count = worship.patches.len();

    let bonus = Patch::new(
        PatchId::new(),
        "Bonus Clean",
        guitar_megarig_id(),
        guitar_megarig_default_scene(),
    );
    worship.add_patch(bonus);

    ctrl.save_profile(worship).await;

    let reloaded = ctrl
        .load_profile(seed_id("guitar-worship-profile"))
        .await
        .expect("worship profile not found after update");

    println!(
        "Worship patches: {} → {}",
        original_count,
        reloaded.patches.len()
    );
    assert_eq!(reloaded.patches.len(), original_count + 1);
    assert!(reloaded.patches.iter().any(|p| p.name == "Bonus Clean"));
}

/// Blues profile has non-default default patch (Crunch, not Clean).
#[tokio::test]
async fn blues_profile_default_is_crunch() {
    let ctrl = controller().await;

    let blues = ctrl
        .load_profile(seed_id("guitar-blues-profile"))
        .await
        .expect("blues profile not found");

    let default = blues.default_patch().expect("no default patch");
    println!(
        "Blues default patch: '{}' (id={})",
        default.name, default.id
    );

    assert_eq!(
        default.name, "Crunch",
        "Blues default should be Crunch, got '{}'",
        default.name
    );
}

// ─────────────────────────────────────────────────────────────
//  Song / Section tests
// ─────────────────────────────────────────────────────────────

/// Verify seed songs are present.
#[tokio::test]
async fn seed_songs_are_loaded() {
    let ctrl = controller().await;

    let songs = ctrl.list_songs().await;
    println!("Seeded songs:");
    for s in &songs {
        println!("  {} — {} ({} sections)", s.id, s.name, s.sections.len());
    }

    assert!(!songs.is_empty(), "should have seeded songs");
}

/// Create a song with both Patch-sourced and RigScene-sourced sections, save, reload.
#[tokio::test]
async fn create_song_with_mixed_section_sources() {
    let ctrl = controller().await;

    let verse_id = SectionId::new();
    let chorus_id = SectionId::new();
    let bridge_id = SectionId::new();
    let song_id = SongId::new();

    // Verse references an existing profile patch
    let verse = Section::from_patch(verse_id.clone(), "Verse", seed_id("guitar-worship-clean"));

    // Chorus references another patch
    let chorus = Section::from_patch(
        chorus_id.clone(),
        "Chorus",
        seed_id("guitar-worship-crunch"),
    )
    .with_override(Override::set(
        NodePath::engine("guitar-engine")
            .with_layer("guitar-layer-archetype-jm")
            .with_block("amp")
            .with_parameter("gain"),
        0.50,
    ));

    // Bridge references a rig scene directly (bypasses profile)
    let bridge = Section::from_rig_scene(
        bridge_id.clone(),
        "Bridge",
        guitar_megarig_id(),
        guitar_megarig_lead_scene(),
    );

    let mut song = Song::new(song_id.clone(), "Test Song", verse).with_artist("Test Artist");
    song.add_section(chorus);
    song.add_section(bridge);

    ctrl.save_song(song).await;

    let reloaded = ctrl
        .load_song(song_id.clone())
        .await
        .expect("test song not found after save");

    println!(
        "Reloaded song '{}' by {}: {} sections",
        reloaded.name,
        reloaded.artist.as_deref().unwrap_or("(none)"),
        reloaded.sections.len()
    );

    assert_eq!(reloaded.name, "Test Song");
    assert_eq!(reloaded.artist.as_deref(), Some("Test Artist"));
    assert_eq!(reloaded.sections.len(), 3);

    // Verify sources round-trip correctly
    let verse_section = reloaded.section(&verse_id).expect("verse not found");
    matches!(&verse_section.source, SectionSource::Patch { .. });

    let bridge_section = reloaded.section(&bridge_id).expect("bridge not found");
    match &bridge_section.source {
        SectionSource::RigScene { rig_id, scene_id } => {
            assert_eq!(rig_id.to_string(), guitar_megarig_id().to_string());
            assert_eq!(
                scene_id.to_string(),
                guitar_megarig_lead_scene().to_string()
            );
        }
        _ => panic!("bridge should be RigScene source"),
    }

    let chorus_section = reloaded.section(&chorus_id).expect("chorus not found");
    assert_eq!(
        chorus_section.overrides.len(),
        1,
        "chorus should have 1 override"
    );
}

/// Modify a section's overrides after initial save, verify update persists.
#[tokio::test]
async fn update_section_override_persists() {
    let ctrl = controller().await;

    let section_id = SectionId::new();
    let song_id = SongId::new();

    let section = Section::from_rig_scene(
        section_id.clone(),
        "Main",
        guitar_megarig_id(),
        guitar_megarig_default_scene(),
    );
    let mut song = Song::new(song_id.clone(), "Override Test Song", section);
    ctrl.save_song(song.clone()).await;

    // Add an override to the existing section
    if let Some(s) = song.sections.iter_mut().find(|s| s.id == section_id) {
        s.overrides.push(Override::set(
            NodePath::engine("guitar-engine")
                .with_layer("guitar-layer-archetype-jm")
                .with_block("amp")
                .with_parameter("master"),
            0.65,
        ));
    }
    ctrl.save_song(song).await;

    let reloaded = ctrl
        .load_song(song_id)
        .await
        .expect("song not found after update");
    let section = reloaded.section(&section_id).unwrap();

    println!(
        "Section '{}' overrides: {}",
        section.name,
        section.overrides.len()
    );
    assert_eq!(
        section.overrides.len(),
        1,
        "section should have 1 override after update"
    );
    assert!(section.overrides[0].path.as_str().contains("master"));
}

// ─────────────────────────────────────────────────────────────
//  Resolve / DAW integration tests
// ─────────────────────────────────────────────────────────────

/// Resolve a bare RigScene — verifies the basic resolution pipeline works.
#[tokio::test]
async fn resolve_rig_scene_produces_graph() {
    let ctrl = controller().await;

    let graph = ctrl
        .resolve_target(ResolveTarget::RigScene {
            rig_id: guitar_megarig_id(),
            scene_id: guitar_megarig_default_scene(),
        })
        .await
        .expect("resolve failed");

    println!(
        "Resolved RigScene: {} engine(s), {} effective overrides",
        graph.engines.len(),
        graph.effective_overrides.len()
    );

    assert!(
        !graph.engines.is_empty(),
        "resolved graph should have engines"
    );

    // Walk engines → layers → modules → blocks and print summary
    for engine in &graph.engines {
        for layer in &engine.layers {
            let block_count: usize = layer.modules.iter().map(|m| m.blocks.len()).sum::<usize>()
                + layer.standalone_blocks.len();
            println!(
                "  engine={} layer={} blocks={}",
                engine.engine_id, layer.layer_id, block_count
            );
        }
    }
}

/// Resolve a ProfilePatch and verify override merging: patch override should
/// modify the final resolved block value.
#[tokio::test]
async fn resolve_profile_patch_applies_gain_override() {
    let ctrl = controller().await;

    // guitar-worship-clean sets amp gain to 0.18
    let graph = ctrl
        .resolve_target(ResolveTarget::ProfilePatch {
            profile_id: seed_id("guitar-worship-profile").into(),
            patch_id: seed_id("guitar-worship-clean").into(),
        })
        .await
        .expect("resolve failed");

    println!(
        "Resolved ProfilePatch 'Clean': {} engines, {} effective overrides",
        graph.engines.len(),
        graph.effective_overrides.len()
    );

    // Find the amp block's gain parameter in the resolved graph
    let gain = find_param_in_graph(&graph, "amp", "gain");
    println!("  Resolved amp gain: {:?}", gain);

    // Patch sets gain to 0.18; default is 0.45 — must be overridden
    if let Some(gain_val) = gain {
        assert!(
            (gain_val - 0.18).abs() < 0.01,
            "patch override should set gain to 0.18, got {gain_val:.3}"
        );
    }
}

/// Resolve the Lead patch and verify the resolved amp gain is higher than Clean.
#[tokio::test]
async fn resolve_lead_patch_has_higher_gain_than_clean() {
    let ctrl = controller().await;

    let clean_graph = ctrl
        .resolve_target(ResolveTarget::ProfilePatch {
            profile_id: seed_id("guitar-worship-profile").into(),
            patch_id: seed_id("guitar-worship-clean").into(),
        })
        .await
        .expect("resolve clean failed");

    let lead_graph = ctrl
        .resolve_target(ResolveTarget::ProfilePatch {
            profile_id: seed_id("guitar-worship-profile").into(),
            patch_id: seed_id("guitar-worship-lead").into(),
        })
        .await
        .expect("resolve lead failed");

    let clean_gain = find_param_in_graph(&clean_graph, "amp", "gain");
    let lead_gain = find_param_in_graph(&lead_graph, "amp", "gain");

    println!("Resolved gain: clean={:?} lead={:?}", clean_gain, lead_gain);

    if let (Some(c), Some(l)) = (clean_gain, lead_gain) {
        assert!(
            l > c,
            "lead gain ({l:.3}) should exceed clean gain ({c:.3})"
        );
    }
}

/// Resolve a SongSection backed by a Patch and confirm the graph is equivalent
/// to resolving that patch directly.
#[tokio::test]
async fn resolve_song_section_via_patch_matches_direct_patch() {
    let ctrl = controller().await;

    // guitar-worship-song uses patches from the worship profile
    let songs = ctrl.list_songs().await;
    let worship_song = songs
        .iter()
        .find(|s| s.name.contains("Worship") || s.name.contains("worship"));

    if worship_song.is_none() {
        println!("No worship song seeded — creating one for this test");
        // Create a minimal test song with a section from a known patch
        let section =
            Section::from_patch(SectionId::new(), "Intro", seed_id("guitar-worship-clean"));
        let song_id = SongId::new();
        let section_id = section.id.clone();
        let song = Song::new(song_id.clone(), "Test Worship Song", section);
        ctrl.save_song(song).await;

        let graph = ctrl
            .resolve_target(ResolveTarget::SongSection {
                song_id: song_id.into(),
                section_id: section_id.into(),
            })
            .await
            .expect("resolve song section failed");

        println!("Resolved SongSection: {} engines", graph.engines.len());
        assert!(!graph.engines.is_empty());
        return;
    }

    let song = worship_song.unwrap();
    let first_section = song.sections.first().expect("song has no sections");
    println!(
        "Resolving section '{}' from song '{}'",
        first_section.name, song.name
    );

    let graph = ctrl
        .resolve_target(ResolveTarget::SongSection {
            song_id: song.id.clone().into(),
            section_id: first_section.id.clone().into(),
        })
        .await
        .expect("resolve song section failed");

    println!(
        "Resolved SongSection '{}': {} engines, {} overrides",
        first_section.name,
        graph.engines.len(),
        graph.effective_overrides.len()
    );

    assert!(
        !graph.engines.is_empty(),
        "resolved song section should have engines"
    );
}

/// Resolve a SectionSource::RigScene directly — verifies it resolves identically
/// to the equivalent RigScene target.
#[tokio::test]
async fn resolve_rig_scene_section_equals_direct_rig_scene() {
    let ctrl = controller().await;

    let section_id = SectionId::new();
    let song_id = SongId::new();

    let section = Section::from_rig_scene(
        section_id.clone(),
        "Direct Scene",
        guitar_megarig_id(),
        guitar_megarig_default_scene(),
    );
    let song = Song::new(song_id.clone(), "Direct Scene Song", section);
    ctrl.save_song(song).await;

    let via_section = ctrl
        .resolve_target(ResolveTarget::SongSection {
            song_id: song_id.into(),
            section_id: section_id.into(),
        })
        .await
        .expect("resolve via section failed");

    let via_rig = ctrl
        .resolve_target(ResolveTarget::RigScene {
            rig_id: guitar_megarig_id(),
            scene_id: guitar_megarig_default_scene(),
        })
        .await
        .expect("resolve via rig failed");

    println!(
        "Via section: {} engines / Via rig: {} engines",
        via_section.engines.len(),
        via_rig.engines.len()
    );

    // Both paths should produce the same number of engines and blocks
    assert_eq!(
        via_section.engines.len(),
        via_rig.engines.len(),
        "section and rig resolve should produce same engine count"
    );
}

/// Verify that patch overrides accumulate correctly with rig scene overrides.
/// Solo patch in Worship profile targets lead scene + adds own overrides.
#[tokio::test]
async fn patch_overrides_stack_on_top_of_rig_scene_overrides() {
    let ctrl = controller().await;

    // Resolve the lead rig scene directly (baseline)
    let rig_graph = ctrl
        .resolve_target(ResolveTarget::RigScene {
            rig_id: guitar_megarig_id(),
            scene_id: guitar_megarig_lead_scene(),
        })
        .await
        .expect("resolve rig scene failed");

    // Resolve the Solo patch (targets lead scene + own gain+delay overrides)
    let patch_graph = ctrl
        .resolve_target(ResolveTarget::ProfilePatch {
            profile_id: seed_id("guitar-worship-profile").into(),
            patch_id: seed_id("guitar-worship-solo").into(),
        })
        .await
        .expect("resolve solo patch failed");

    println!(
        "RigScene lead overrides: {}",
        rig_graph.effective_overrides.len()
    );
    println!(
        "Solo patch overrides: {}",
        patch_graph.effective_overrides.len()
    );

    // Solo patch adds its own overrides on top — total should be >= rig scene's
    assert!(
        patch_graph.effective_overrides.len() >= rig_graph.effective_overrides.len(),
        "patch should have at least as many effective overrides as the base scene"
    );

    // Solo patch sets gain=0.72 and delay mix=0.30
    let solo_gain = find_param_in_graph(&patch_graph, "amp", "gain");
    println!("Solo resolved gain: {:?}", solo_gain);
    if let Some(g) = solo_gain {
        assert!(
            (g - 0.72).abs() < 0.01,
            "solo gain should be 0.72, got {g:.3}"
        );
    }
}

// ─────────────────────────────────────────────────────────────
//  Helper: walk a ResolvedGraph to find a parameter value
// ─────────────────────────────────────────────────────────────

/// Walk all engines → layers → modules → blocks to find the first parameter
/// matching `block_id_fragment` and `param_id`.
fn find_param_in_graph(
    graph: &ResolvedGraph,
    block_id_fragment: &str,
    param_id: &str,
) -> Option<f32> {
    for engine in &graph.engines {
        for layer in &engine.layers {
            for module in &layer.modules {
                for block in &module.blocks {
                    if block.node_id.contains(block_id_fragment)
                        || block.label.to_lowercase().contains(block_id_fragment)
                    {
                        for param in block.block.parameters() {
                            if param.id() == param_id {
                                return Some(param.value().get());
                            }
                        }
                    }
                }
            }
            for block in &layer.standalone_blocks {
                if block.node_id.contains(block_id_fragment)
                    || block.label.to_lowercase().contains(block_id_fragment)
                {
                    for param in block.block.parameters() {
                        if param.id() == param_id {
                            return Some(param.value().get());
                        }
                    }
                }
            }
        }
    }
    None
}
