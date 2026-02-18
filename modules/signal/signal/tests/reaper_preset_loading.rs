//! REAPER integration tests — applies signal domain presets via the live JM
//! plugin, then verifies round-trips, morphing, and snapshot behaviour.
//!
//! Run with:
//!
//!   cargo xtask reaper-test

mod daw_helpers;

use daw_helpers::{
    add_jm_track, apply_block, apply_graph, build_morph_engine, capture_block_from_fx, get_fx0,
    read_gain, remove_track,
};
use reaper_test::reaper_test;
use signal::{
    bootstrap_in_memory_controller_async, resolve::ResolveTarget, seed_id, SignalController,
};
use signal_proto::easing::EasingCurve;

// FX ID used as the key in DawParameterSnapshot entries for the JM plugin.
const JM_FX_ID: &str = "jm-amp";

async fn signal_controller() -> SignalController {
    bootstrap_in_memory_controller_async()
        .await
        .expect("failed to bootstrap signal controller")
}

// ─────────────────────────────────────────────────────────────
//  Scenario 1: Load JM plugin on a new track
// ─────────────────────────────────────────────────────────────

#[reaper_test]
async fn load_jm_plugin(ctx: &ReaperTestContext) -> Result<()> {
    println!("\n=== scenario: load_jm_plugin_on_new_track ===");
    let track = add_jm_track(ctx.project(), "JM Plugin Test").await?;

    let fx_list = track.fx_chain().all().await?;
    println!("FX on new JM track ({} total):", fx_list.len());
    for fx in &fx_list {
        println!(
            "  [{}] {} — {} params",
            fx.index, fx.name, fx.parameter_count
        );
    }
    assert!(!fx_list.is_empty(), "track should have JM plugin loaded");
    assert!(
        fx_list[0].name.contains("John Mayer") || fx_list[0].plugin_name.contains("John Mayer"),
        "first FX should be the JM plugin, got: {}",
        fx_list[0].name
    );

    remove_track(ctx.project(), track).await;
    println!("PASS");
    Ok(())
}

// ─────────────────────────────────────────────────────────────
//  Scenario 2: Apply default amp block to live FX
// ─────────────────────────────────────────────────────────────

#[reaper_test]
async fn amp_default_block(ctx: &ReaperTestContext) -> Result<()> {
    println!("\n=== scenario: apply_jm_amp_default_block_to_live_fx ===");
    let ctrl = signal_controller().await;
    let track = add_jm_track(ctx.project(), "JM Amp Default Block").await?;

    let block = ctrl
        .load_collection_default(signal::BlockType::Amp, seed_id("jm-amp"))
        .await
        .expect("jm-amp default not found");

    let applied = apply_block(&track, &block, JM_FX_ID).await?;
    println!("Applied {} parameter(s) to live JM plugin", applied);

    remove_track(ctx.project(), track).await;
    println!("PASS");
    Ok(())
}

// ─────────────────────────────────────────────────────────────
//  Scenario 3: Apply each JM block preset to a new track
// ─────────────────────────────────────────────────────────────

#[reaper_test]
async fn each_jm_block_preset(ctx: &ReaperTestContext) -> Result<()> {
    println!("\n=== scenario: apply_each_jm_block_preset_to_new_track ===");
    let ctrl = signal_controller().await;

    let jm_presets = [
        (signal::BlockType::Boost, "jm-justa-boost", "Justa Boost"),
        (
            signal::BlockType::Filter,
            "jm-antelope-filter",
            "Antelope Filter",
        ),
        (signal::BlockType::Drive, "jm-halfman-od", "Halfman OD"),
        (signal::BlockType::Drive, "jm-tealbreaker", "Tealbreaker"),
        (
            signal::BlockType::Delay,
            "jm-millipede-delay",
            "Millipede Delay",
        ),
        (
            signal::BlockType::Tremolo,
            "jm-harmonic-tremolo",
            "Harmonic Tremolo",
        ),
        (
            signal::BlockType::Reverb,
            "jm-spring-reverb",
            "Spring Reverb",
        ),
        (signal::BlockType::Amp, "jm-amp", "JM Amp"),
        (signal::BlockType::Cabinet, "jm-cab", "JM Cabinet"),
        (signal::BlockType::Eq, "jm-eq", "JM EQ"),
        (signal::BlockType::Delay, "jm-dream-delay", "Dream Delay"),
        (signal::BlockType::Reverb, "jm-studio-verb", "Studio Verb"),
    ];

    let mut tracks = Vec::new();
    for (block_type, preset_id, label) in &jm_presets {
        let block = match ctrl
            .load_collection_default(*block_type, seed_id(preset_id))
            .await
        {
            Some(b) => b,
            None => {
                println!("  ⚠ {} — preset not found, skipping", label);
                continue;
            }
        };
        let track = add_jm_track(ctx.project(), &format!("JM {label}")).await?;
        let applied = apply_block(&track, &block, preset_id).await?;
        println!("  ✓ {} — {} param(s) applied", label, applied);
        tracks.push(track);
    }

    for track in tracks {
        remove_track(ctx.project(), track).await;
    }
    println!("PASS — all {} JM block presets applied", jm_presets.len());
    Ok(())
}

// ─────────────────────────────────────────────────────────────
//  Scenario 4: Apply worship profile patches to tracks
// ─────────────────────────────────────────────────────────────

#[reaper_test]
async fn worship_patches(ctx: &ReaperTestContext) -> Result<()> {
    println!("\n=== scenario: apply_worship_profile_patches_to_tracks ===");
    let ctrl = signal_controller().await;

    let worship = ctrl
        .load_profile(seed_id("guitar-worship-profile"))
        .await
        .expect("worship profile not found");

    println!("Applying {} Worship patches:", worship.patches.len());

    let mut tracks = Vec::new();
    for patch in &worship.patches {
        let graph = ctrl
            .resolve_target(ResolveTarget::ProfilePatch {
                profile_id: seed_id("guitar-worship-profile").into(),
                patch_id: patch.id.clone().into(),
            })
            .await
            .map_err(|e| eyre::eyre!("resolve failed: {:?}", e))?;

        let track = add_jm_track(ctx.project(), &format!("Worship/{}", patch.name)).await?;
        let applied = apply_graph(&track, &graph, JM_FX_ID).await?;
        let gain = read_gain(&track).await?;
        println!(
            "  ✓ {} — {} params applied, gain={:?}",
            patch.name, applied, gain
        );
        tracks.push(track);
    }

    for track in tracks {
        remove_track(ctx.project(), track).await;
    }
    println!("PASS");
    Ok(())
}

// ─────────────────────────────────────────────────────────────
//  Scenario 5: Apply worship song sections to tracks
// ─────────────────────────────────────────────────────────────

#[reaper_test]
async fn worship_song_sections(ctx: &ReaperTestContext) -> Result<()> {
    println!("\n=== scenario: apply_worship_song_sections_to_tracks ===");
    let ctrl = signal_controller().await;

    let songs = ctrl.list_songs().await;
    let worship_song = songs
        .iter()
        .find(|s| s.name.contains("Worship"))
        .expect("no Worship song seeded");

    println!(
        "Sections from '{}' ({} sections):",
        worship_song.name,
        worship_song.sections.len()
    );

    let mut tracks = Vec::new();
    for section in &worship_song.sections {
        let graph = ctrl
            .resolve_target(ResolveTarget::SongSection {
                song_id: worship_song.id.clone().into(),
                section_id: section.id.clone().into(),
            })
            .await
            .map_err(|e| eyre::eyre!("resolve {:?}: {:?}", section.name, e))?;

        let track = add_jm_track(ctx.project(), &format!("Section/{}", section.name)).await?;
        let applied = apply_graph(&track, &graph, JM_FX_ID).await?;
        println!(
            "  ✓ '{}' — {} params applied, {} overrides",
            section.name,
            applied,
            graph.effective_overrides.len()
        );
        tracks.push(track);
    }

    for track in tracks {
        remove_track(ctx.project(), track).await;
    }
    println!("PASS");
    Ok(())
}

// ─────────────────────────────────────────────────────────────
//  Scenario 6: Snapshot round-trip (capture → mutate → restore → verify)
// ─────────────────────────────────────────────────────────────

#[reaper_test]
async fn snapshot_round_trip(ctx: &ReaperTestContext) -> Result<()> {
    println!("\n=== scenario: snapshot_live_params_save_recall ===");
    let ctrl = signal_controller().await;

    let track = add_jm_track(ctx.project(), "Snapshot Round-Trip").await?;
    let fx = get_fx0(&track).await?;
    let live_params = fx.parameters().await?;
    println!("Live JM params ({}):", live_params.len());
    for p in live_params.iter().take(10) {
        println!("  [{}] {} = {:.4}", p.index, p.name, p.value);
    }

    let amp_block = ctrl
        .load_collection_default(signal::BlockType::Amp, seed_id("jm-amp"))
        .await
        .expect("jm-amp not found");

    // Capture live values into the domain block
    let captured = capture_block_from_fx(&track, amp_block).await?;

    // Overwrite the default snapshot with captured values
    ctrl.update_snapshot_params(
        signal::BlockType::Amp,
        seed_id("jm-amp"),
        seed_id("jm-amp-default"),
        captured.clone(),
    )
    .await;

    // Apply Lead to dirty state
    let lead_block = ctrl
        .load_variant(
            signal::BlockType::Amp,
            seed_id("jm-amp"),
            seed_id("jm-amp-lead"),
        )
        .await
        .expect("lead block not found");
    apply_block(&track, &lead_block, JM_FX_ID).await?;
    println!("Applied Lead snapshot.");

    // Restore default
    let restored = ctrl
        .load_collection_default(signal::BlockType::Amp, seed_id("jm-amp"))
        .await
        .expect("jm-amp not found after save");
    apply_block(&track, &restored, JM_FX_ID).await?;
    println!("Restored live snapshot.");

    // Verify
    let final_params = fx.parameters().await?;
    let mut mismatches = 0;
    for sp in captured.parameters() {
        if let Some(fp) = final_params
            .iter()
            .find(|p| p.name.to_lowercase().contains(&sp.id().to_lowercase()))
        {
            let diff = (fp.value - sp.value().get() as f64).abs();
            if diff > 0.02 {
                println!(
                    "  ✗ {} expected={:.4} got={:.4}",
                    sp.id(),
                    sp.value().get(),
                    fp.value
                );
                mismatches += 1;
            } else {
                println!("  ✓ {} = {:.4}", sp.id(), fp.value);
            }
        }
    }

    remove_track(ctx.project(), track).await;
    assert_eq!(
        mismatches, 0,
        "{mismatches} param(s) didn't match after snapshot restore"
    );
    println!("PASS");
    Ok(())
}

// ─────────────────────────────────────────────────────────────
//  Scenario 7: Morph between clean and lead patches
// ─────────────────────────────────────────────────────────────

#[reaper_test]
async fn morph_between_patches(ctx: &ReaperTestContext) -> Result<()> {
    println!("\n=== scenario: morph_between_patches ===");
    let ctrl = signal_controller().await;

    let track = add_jm_track(ctx.project(), "Morph Clean→Lead").await?;
    let fx = get_fx0(&track).await?;

    let clean = ctrl
        .load_variant(
            signal::BlockType::Amp,
            seed_id("jm-amp"),
            seed_id("jm-amp-clean"),
        )
        .await
        .expect("jm-amp-clean not found");
    let lead = ctrl
        .load_variant(
            signal::BlockType::Amp,
            seed_id("jm-amp"),
            seed_id("jm-amp-lead"),
        )
        .await
        .expect("jm-amp-lead not found");

    let (engine, snap_a, snap_b) = build_morph_engine(&track, &clean, &lead, JM_FX_ID).await?;

    let gain_idx_a = snap_a
        .params
        .iter()
        .find(|p| p.param_name.to_lowercase().contains("gain"))
        .map(|p| p.param_index)
        .expect("gain in snap_a");
    let gain_a = snap_a
        .params
        .iter()
        .find(|p| p.param_index == gain_idx_a)
        .map(|p| p.value)
        .unwrap();
    let gain_b = snap_b
        .params
        .iter()
        .find(|p| p.param_index == gain_idx_a)
        .map(|p| p.value)
        .unwrap();

    println!(
        "  Snapshot A (Clean): {} params, gain={:.4}",
        snap_a.params.len(),
        gain_a
    );
    println!(
        "  Snapshot B (Lead):  {} params, gain={:.4}",
        snap_b.params.len(),
        gain_b
    );
    println!("  Diff params: {}", engine.diff_count());
    assert!(engine.diff_count() > 0, "Clean and Lead should differ");

    let mut prev_gain = gain_a;
    for &t in &[0.0f64, 0.25, 0.5, 0.75, 1.0] {
        let changes = engine.morph(t, EasingCurve::Linear);
        for ch in &changes {
            fx.param(ch.param_index).set(ch.current_value).await?;
        }
        let live = fx.param(gain_idx_a).get().await?;
        let expected = gain_a + t * (gain_b - gain_a);
        println!("  t={:.2}: gain={:.4} (expected≈{:.4})", t, live, expected);
        assert!(
            (live - expected).abs() < 0.02,
            "gain at t={t} should be ≈{expected:.4}, got {live:.4}"
        );
        if t > 0.0 && gain_b > gain_a {
            assert!(live >= prev_gain - 0.001, "gain should be non-decreasing");
        }
        prev_gain = live;
    }

    remove_track(ctx.project(), track).await;
    println!("PASS");
    Ok(())
}

// ─────────────────────────────────────────────────────────────
//  Scenario 8: Morph easing curves
// ─────────────────────────────────────────────────────────────

#[reaper_test]
async fn morph_easing_curves(ctx: &ReaperTestContext) -> Result<()> {
    println!("\n=== scenario: morph_easing_curves ===");
    let ctrl = signal_controller().await;

    let track = add_jm_track(ctx.project(), "Morph Easing").await?;
    let fx = get_fx0(&track).await?;

    let clean = ctrl
        .load_variant(
            signal::BlockType::Amp,
            seed_id("jm-amp"),
            seed_id("jm-amp-clean"),
        )
        .await
        .expect("jm-amp-clean");
    let lead = ctrl
        .load_variant(
            signal::BlockType::Amp,
            seed_id("jm-amp"),
            seed_id("jm-amp-lead"),
        )
        .await
        .expect("jm-amp-lead");

    let (engine, snap_a, _snap_b) = build_morph_engine(&track, &clean, &lead, JM_FX_ID).await?;
    let gain_idx = snap_a
        .params
        .iter()
        .find(|p| p.param_name.to_lowercase().contains("gain"))
        .map(|p| p.param_index)
        .expect("gain param");

    for curve in [
        EasingCurve::Linear,
        EasingCurve::EaseIn,
        EasingCurve::EaseOut,
        EasingCurve::EaseInOut,
    ] {
        let changes = engine.morph(0.25, curve);
        for ch in &changes {
            fx.param(ch.param_index).set(ch.current_value).await?;
        }
        let live = fx.param(gain_idx).get().await?;
        println!("  {:?} at t=0.25: gain={:.4}", curve, live);
    }

    // Boundary conditions: t=0 → A, t=1 → B for all curves
    for curve in [
        EasingCurve::Linear,
        EasingCurve::EaseIn,
        EasingCurve::EaseOut,
        EasingCurve::EaseInOut,
    ] {
        for ch in &engine.morph(0.0, curve) {
            assert!(
                (ch.current_value - ch.from_value).abs() < 1e-9,
                "{curve:?} t=0 != A"
            );
        }
        for ch in &engine.morph(1.0, curve) {
            assert!(
                (ch.current_value - ch.to_value).abs() < 1e-9,
                "{curve:?} t=1 != B"
            );
        }
    }

    remove_track(ctx.project(), track).await;
    println!("PASS");
    Ok(())
}

// ─────────────────────────────────────────────────────────────
//  Scenario 9: Variant cycling (clean → lead → crunch)
// ─────────────────────────────────────────────────────────────

#[reaper_test]
async fn variant_cycling(ctx: &ReaperTestContext) -> Result<()> {
    println!("\n=== scenario: variant_cycling ===");
    let ctrl = signal_controller().await;

    let track = add_jm_track(ctx.project(), "Amp Variants").await?;

    // Named JM amp variants (excluding default, which is mutated by snapshot round-trip tests)
    let variants = [
        ("jm-amp-clean", "Clean", 0.25f32),
        ("jm-amp-lead", "Lead", 0.75),
        ("jm-amp-crunch", "Crunch", 0.62),
    ];

    for (variant_id, label, expected_gain) in &variants {
        let block = match ctrl
            .load_variant(
                signal::BlockType::Amp,
                seed_id("jm-amp"),
                seed_id(variant_id),
            )
            .await
        {
            Some(b) => b,
            None => {
                println!("  ⚠ {} not found, skipping", label);
                continue;
            }
        };

        apply_block(&track, &block, JM_FX_ID).await?;

        let actual = read_gain(&track).await?.unwrap_or(0.0) as f32;
        let ok = (actual - expected_gain).abs() < 0.02;
        println!(
            "  {} gain: expected={:.4} actual={:.4} {}",
            label,
            expected_gain,
            actual,
            if ok { "✓" } else { "✗" }
        );
        assert!(ok, "{label} gain should be ≈{expected_gain}, got {actual}");
    }

    remove_track(ctx.project(), track).await;
    println!("PASS");
    Ok(())
}

// ─────────────────────────────────────────────────────────────
//  Scenario 10: Save new block snapshot
// ─────────────────────────────────────────────────────────────

#[reaper_test]
async fn save_new_snapshot(ctx: &ReaperTestContext) -> Result<()> {
    println!("\n=== scenario: save_new_block_snapshot ===");
    let ctrl = signal_controller().await;

    let track = add_jm_track(ctx.project(), "Save Snapshot").await?;

    // Apply Lead to put the plugin in a known state
    let lead = ctrl
        .load_variant(
            signal::BlockType::Amp,
            seed_id("jm-amp"),
            seed_id("jm-amp-lead"),
        )
        .await
        .expect("jm-amp-lead");
    apply_block(&track, &lead, JM_FX_ID).await?;

    // Capture live (post-Lead) → domain block
    let template = ctrl
        .load_collection_default(signal::BlockType::Amp, seed_id("jm-amp"))
        .await
        .expect("jm-amp default");
    let captured = capture_block_from_fx(&track, template).await?;

    // Save as the default snapshot
    ctrl.update_snapshot_params(
        signal::BlockType::Amp,
        seed_id("jm-amp"),
        seed_id("jm-amp-default"),
        captured.clone(),
    )
    .await;

    // Reload and verify every parameter round-tripped
    let reloaded = ctrl
        .load_collection_default(signal::BlockType::Amp, seed_id("jm-amp"))
        .await
        .expect("jm-amp default after save");

    let mut mismatches = 0;
    for (orig, reloaded_sp) in captured
        .parameters()
        .iter()
        .zip(reloaded.parameters().iter())
    {
        let diff = (orig.value().get() - reloaded_sp.value().get()).abs();
        if diff > 0.001 {
            println!(
                "  ✗ {} saved={:.4} reloaded={:.4}",
                orig.id(),
                orig.value().get(),
                reloaded_sp.value().get()
            );
            mismatches += 1;
        }
    }
    assert_eq!(
        mismatches, 0,
        "{mismatches} param(s) didn't survive snapshot save/reload"
    );
    println!(
        "  ✓ All {} amp params round-tripped through save",
        captured.parameters().len()
    );

    remove_track(ctx.project(), track).await;
    println!("PASS");
    Ok(())
}

// ─────────────────────────────────────────────────────────────
//  Scenario 11: Override stacking across worship song sections
// ─────────────────────────────────────────────────────────────

#[reaper_test]
async fn override_stacking(ctx: &ReaperTestContext) -> Result<()> {
    println!("\n=== scenario: override_stacking ===");
    let ctrl = signal_controller().await;

    let songs = ctrl.list_songs().await;
    let worship = songs
        .iter()
        .find(|s| s.name.contains("Worship"))
        .expect("Worship song");

    struct SectionResult {
        name: String,
        gain: f64,
        override_count: usize,
    }
    let mut results = Vec::new();

    for section in &worship.sections {
        let graph = ctrl
            .resolve_target(ResolveTarget::SongSection {
                song_id: worship.id.clone().into(),
                section_id: section.id.clone().into(),
            })
            .await
            .map_err(|e| eyre::eyre!("{:?}", e))?;

        let track = add_jm_track(ctx.project(), &format!("Override/{}", section.name)).await?;
        apply_graph(&track, &graph, JM_FX_ID).await?;
        let gain = read_gain(&track).await?.unwrap_or(0.0);
        println!(
            "  {}: {} overrides, gain={:.4}",
            section.name,
            graph.effective_overrides.len(),
            gain
        );
        results.push(SectionResult {
            name: section.name.clone(),
            override_count: graph.effective_overrides.len(),
            gain,
        });
        remove_track(ctx.project(), track).await;
    }

    let solo = results
        .iter()
        .find(|r| r.name == "Solo")
        .expect("Solo section");
    let outro = results
        .iter()
        .find(|r| r.name == "Outro")
        .expect("Outro section");
    assert!(
        solo.override_count > outro.override_count,
        "Solo ({}) should have more overrides than Outro ({})",
        solo.override_count,
        outro.override_count
    );
    for r in &results {
        assert!(r.gain > 0.0, "Section '{}' resolved to zero gain", r.name);
    }

    println!("PASS");
    Ok(())
}
