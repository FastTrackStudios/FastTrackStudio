//! REAPER integration tests for the sync SHM guest.
//!
//! These tests connect to a live REAPER instance via daw-bridge and exercise
//! the `DawLinkCallbacks` code paths — the same RPC calls that `fts-sync`
//! makes at runtime. This validates that the daw API surface used by the
//! guest actually works end-to-end over SHM.
//!
//! Run with:
//!   cargo xtask reaper-test -- reaper_sync_shm

use std::time::Duration;

use reaper_test::reaper_test;

// ── Transport round-trip ────────────────────────────────────────────────────

/// Verify that transport state capture works — the core of `capture_state()`.
#[reaper_test(isolated)]
async fn sync_capture_transport_state(ctx: &reaper_test::ReaperTestContext) -> eyre::Result<()> {
    let transport = ctx.project().transport();

    let state = transport.get_state().await?;
    println!(
        "Transport state: tempo={:.1}, timesig={}/{}, playrate={:.2}",
        state.tempo.bpm,
        state.time_signature.numerator,
        state.time_signature.denominator,
        state.playrate,
    );

    assert!(state.tempo.bpm > 0.0, "tempo should be positive");
    assert!(state.time_signature.numerator > 0, "time sig numerator > 0");
    assert!(
        state.time_signature.denominator > 0,
        "time sig denominator > 0"
    );
    assert!(state.playrate > 0.0, "playrate should be positive");

    Ok(())
}

// ── Tempo set/get ───────────────────────────────────────────────────────────

/// Verify tempo round-trip — `set_tempo()` / `get_tempo()` as used by
/// puppet mode's tempo sync.
#[reaper_test(isolated)]
async fn sync_tempo_round_trip(ctx: &reaper_test::ReaperTestContext) -> eyre::Result<()> {
    let transport = ctx.project().transport();

    let original = transport.get_tempo().await?;
    println!("Original tempo: {original:.1} BPM");

    transport.set_tempo(142.0).await?;
    tokio::time::sleep(Duration::from_millis(50)).await;

    let readback = transport.get_tempo().await?;
    println!("After set_tempo(142.0): {readback:.1} BPM");
    assert!(
        (readback - 142.0).abs() < 0.1,
        "tempo should be ~142.0, got {readback:.1}"
    );

    // Restore
    transport.set_tempo(original).await?;
    Ok(())
}

// ── Play/stop ───────────────────────────────────────────────────────────────

/// Verify play/stop — used by puppet mode for transport start/stop sync.
#[reaper_test(isolated)]
async fn sync_play_stop(ctx: &reaper_test::ReaperTestContext) -> eyre::Result<()> {
    let transport = ctx.project().transport();

    // Start playback
    transport.play().await?;
    tokio::time::sleep(Duration::from_millis(100)).await;
    assert!(transport.is_playing().await?, "should be playing");

    // Stop
    transport.stop().await?;
    tokio::time::sleep(Duration::from_millis(100)).await;
    assert!(!transport.is_playing().await?, "should be stopped");

    Ok(())
}

// ── Cursor position ─────────────────────────────────────────────────────────

/// Verify set_position/get_position — used by puppet mode for cursor alignment.
#[reaper_test(isolated)]
async fn sync_cursor_position(ctx: &reaper_test::ReaperTestContext) -> eyre::Result<()> {
    let transport = ctx.project().transport();

    transport.set_position(5.0).await?;
    tokio::time::sleep(Duration::from_millis(50)).await;

    let pos = transport.get_position().await?;
    println!("After set_position(5.0): {pos:.3}s");
    assert!(
        (pos - 5.0).abs() < 0.1,
        "position should be ~5.0s, got {pos:.3}"
    );

    // Reset
    transport.set_position(0.0).await?;
    Ok(())
}

// ── ExtState ────────────────────────────────────────────────────────────────

/// Verify ExtState read/write — used by the tick loop for mode control.
#[reaper_test(isolated)]
async fn sync_ext_state_mode_control(ctx: &reaper_test::ReaperTestContext) -> eyre::Result<()> {
    let ext = ctx.daw.ext_state();

    // Write a mode value
    ext.set("FTS_SYNC_TEST", "link_mode", "puppet", false)
        .await?;
    tokio::time::sleep(Duration::from_millis(50)).await;

    let value = ext.get("FTS_SYNC_TEST", "link_mode").await?;
    println!("ExtState FTS_SYNC_TEST/link_mode = {:?}", value);
    assert_eq!(value.as_deref(), Some("puppet"));

    // Clean up
    ext.delete("FTS_SYNC_TEST", "link_mode", false).await?;
    let deleted = ext.get("FTS_SYNC_TEST", "link_mode").await?;
    assert!(deleted.is_none(), "should be deleted");

    Ok(())
}

// ── Audio latency ───────────────────────────────────────────────────────────

/// Verify audio latency query — used by `capture_state()` for output_latency.
#[reaper_test]
async fn sync_audio_latency(ctx: &reaper_test::ReaperTestContext) -> eyre::Result<()> {
    let latency = ctx.daw.audio_engine().output_latency_seconds().await?;
    println!("Output latency: {latency:.6}s");

    // Latency should be non-negative (could be 0 with null audio driver)
    assert!(latency >= 0.0, "latency should be >= 0, got {latency}");

    Ok(())
}

// ── Playrate ────────────────────────────────────────────────────────────────

/// Verify playrate get/set — used by puppet mode's phase drift correction.
#[reaper_test(isolated)]
async fn sync_playrate_round_trip(ctx: &reaper_test::ReaperTestContext) -> eyre::Result<()> {
    let transport = ctx.project().transport();

    let original = transport.get_playrate().await?;
    println!("Original playrate: {original:.4}");
    assert!(
        (original - 1.0).abs() < 0.01,
        "default playrate should be ~1.0"
    );

    transport.set_playrate(1.05).await?;
    tokio::time::sleep(Duration::from_millis(50)).await;

    let readback = transport.get_playrate().await?;
    println!("After set_playrate(1.05): {readback:.4}");
    assert!(
        (readback - 1.05).abs() < 0.01,
        "playrate should be ~1.05, got {readback:.4}"
    );

    // Restore
    transport.set_playrate(1.0).await?;
    Ok(())
}

// ── Run command ─────────────────────────────────────────────────────────────

/// Verify run_command — used by nudge_playrate_up/down and reset_playrate.
#[reaper_test(isolated)]
async fn sync_run_command(ctx: &reaper_test::ReaperTestContext) -> eyre::Result<()> {
    let project = ctx.project().clone();

    // Action 40521: "Transport: Set playrate to 1.0"
    let result = project.run_command("40521").await?;
    println!("run_command(40521) = {result}");
    assert!(result, "known action should return true");

    Ok(())
}

// ── Markers ─────────────────────────────────────────────────────────────────

/// Verify marker add/remove — used by quantized launch.
#[reaper_test(isolated)]
async fn sync_marker_lifecycle(ctx: &reaper_test::ReaperTestContext) -> eyre::Result<()> {
    let project = ctx.project().clone();
    let markers = project.markers();

    let id = markers.add(10.0, "sync-test-marker").await?;
    println!("Added marker id={id} at 10.0s");

    let all = markers.all().await?;
    let found = all.iter().any(|m| m.name == "sync-test-marker");
    assert!(found, "marker should exist after add");

    markers.remove(id).await?;
    println!("Removed marker id={id}");

    Ok(())
}

// ── Regions ─────────────────────────────────────────────────────────────────

/// Verify region add/remove — used by quantized launch pre-roll and target.
#[reaper_test(isolated)]
async fn sync_region_lifecycle(ctx: &reaper_test::ReaperTestContext) -> eyre::Result<()> {
    let project = ctx.project().clone();
    let regions = project.regions();

    let id = regions.add(5.0, 10.0, "sync-test-region").await?;
    println!("Added region id={id} at 5.0-10.0s");

    let all = regions.all().await?;
    let found = all.iter().any(|r| r.name == "sync-test-region");
    assert!(found, "region should exist after add");

    regions.remove(id).await?;
    println!("Removed region id={id}");

    Ok(())
}

// ── Tempo map ───────────────────────────────────────────────────────────────

/// Verify tempo_map.add_point — used by set_tempo_at_position.
#[reaper_test(isolated)]
async fn sync_tempo_map_add_point(ctx: &reaper_test::ReaperTestContext) -> eyre::Result<()> {
    let project = ctx.project().clone();
    let tempo_map = project.tempo_map();

    let initial_count = tempo_map.count().await?;
    println!("Initial tempo points: {initial_count}");

    tempo_map.add_point(10.0, 155.0).await?;
    println!("Added tempo point: 155.0 BPM at 10.0s");

    let new_count = tempo_map.count().await?;
    assert!(
        new_count > initial_count,
        "should have more tempo points after add"
    );

    let tempo_at = tempo_map.tempo_at(10.0).await?;
    println!("Tempo at 10.0s: {tempo_at:.1} BPM");
    assert!(
        (tempo_at - 155.0).abs() < 1.0,
        "tempo at 10.0s should be ~155.0, got {tempo_at:.1}"
    );

    Ok(())
}

// ── Time signature ──────────────────────────────────────────────────────────

/// Verify time_signature retrieval — used by capture_state for quantum calc.
#[reaper_test(isolated)]
async fn sync_time_signature(ctx: &reaper_test::ReaperTestContext) -> eyre::Result<()> {
    let transport = ctx.project().transport();

    let ts = transport.get_time_signature().await?;
    println!("Time signature: {}/{}", ts.numerator, ts.denominator);

    assert!(ts.numerator > 0 && ts.numerator <= 32);
    assert!(ts.denominator > 0 && ts.denominator <= 32);

    Ok(())
}
