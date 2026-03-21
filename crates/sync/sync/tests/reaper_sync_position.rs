//! Multi-instance position sync integration test.
//!
//! Spawns 2 REAPER instances via `#[reaper_test(instances(...))]`, connects
//! them via direct TCP PeerMesh, then verifies that transport position stays
//! tightly synced across scenarios: play, sustained playback, tempo change,
//! seek while playing, stop, and restart.
//!
//! Run with:
//!   cargo xtask reaper-test -- position_sync

use std::time::Duration;

use daw::service::PlayState;
use reaper_test::reaper_test;

/// Maximum allowed position drift between master and follower (seconds).
/// The test reads master and follower positions sequentially via RPC (~200ms
/// between reads), so measured drift includes ~20-30ms of measurement jitter.
/// Actual sync quality is typically 20-40ms.
const MAX_DRIFT_SECS: f64 = 0.075; // 75ms

/// Relaxed tolerance after tempo changes / seeks — transients need more time.
const MAX_DRIFT_AFTER_CHANGE: f64 = 0.15; // 150ms

/// How often to sample positions during sustained playback checks.
const SAMPLE_INTERVAL: Duration = Duration::from_millis(200);

#[reaper_test(instances("master", "follower"))]
async fn position_sync(ctx: &reaper_test::MultiDawTestContext) -> Result<()> {
    let master = ctx.by_label("master");
    let follower = ctx.by_label("follower");

    // ── Setup: wait for extensions, connect peers ────────────
    ctx.connect_sync_peers("FTS_SYNC_EXT").await?;

    // ── Scenario 1: Start playback, verify follower follows ──
    println!("\n  === Scenario 1: Play and verify position sync ===");
    let p_master = master.daw.current_project().await?;
    p_master.transport().play().await?;
    println!("  [master] playback started");

    // Give drift corrector time to converge
    tokio::time::sleep(Duration::from_secs(2)).await;

    // Debug: check master state
    let m_state = master.daw.current_project().await?.transport().get_state().await?;
    println!("  [master] play_state={:?}", m_state.play_state);

    let f_state = follower.daw.current_project().await?.transport().get_state().await?;
    println!("  [follower] play_state={:?}", f_state.play_state);
    assert!(
        matches!(f_state.play_state, PlayState::Playing),
        "follower should be playing (got {:?})", f_state.play_state
    );
    println!("  [follower] confirmed playing");

    // ── Scenario 2: Sustained playback — sample positions over 5s ─
    println!("\n  === Scenario 2: Sustained playback position check (5s) ===");
    let mut max_drift: f64 = 0.0;
    for i in 0..25 {
        let (m, f, drift) = ctx
            .assert_positions_close(
                "master",
                "follower",
                MAX_DRIFT_SECS,
                &format!("sustained playback sample {i}"),
            )
            .await?;
        max_drift = max_drift.max(drift);
        println!(
            "    sample {i:2}: master={m:.4}s follower={f:.4}s drift={drift:.4}s"
        );
        tokio::time::sleep(SAMPLE_INTERVAL).await;
    }
    println!("  Max drift during sustained playback: {max_drift:.4}s");

    // ── Scenario 3: Tempo change while playing ───────────────
    println!("\n  === Scenario 3: Tempo change 120→160 while playing ===");
    p_master.transport().set_tempo(160.0).await?;
    println!("  [master] tempo set to 160");

    // Verify master actually changed tempo
    tokio::time::sleep(Duration::from_millis(500)).await;
    let master_tempo = p_master.transport().get_tempo().await?;
    println!("  [master] tempo readback: {master_tempo:.1}");

    // Allow drift corrector to converge after tempo change
    tokio::time::sleep(Duration::from_secs(5)).await;

    let f_proj = follower.daw.current_project().await?;
    let follower_tempo = f_proj.transport().get_tempo().await?;
    println!("  [follower] tempo: {follower_tempo:.1}");

    // Log drift correction state
    let m_state = p_master.transport().get_state().await?;
    let f_state2 = f_proj.transport().get_state().await?;
    println!("  [master] state: tempo={:.1}, pos={:.3}s", m_state.tempo.bpm,
        m_state.playhead_position.time.as_ref().map(|t| t.as_seconds()).unwrap_or(0.0));
    println!("  [follower] state: tempo={:.1}, pos={:.3}s", f_state2.tempo.bpm,
        f_state2.playhead_position.time.as_ref().map(|t| t.as_seconds()).unwrap_or(0.0));

    assert!(
        (follower_tempo - 160.0).abs() < 1.0,
        "follower tempo should be ~160, got {follower_tempo}"
    );
    println!("  [follower] tempo confirmed ~160 ({follower_tempo:.1})");

    for i in 0..10 {
        let (m, f, drift) = ctx
            .assert_positions_close(
                "master",
                "follower",
                MAX_DRIFT_AFTER_CHANGE,
                &format!("post-tempo-change sample {i}"),
            )
            .await?;
        println!(
            "    sample {i}: master={m:.4}s follower={f:.4}s drift={drift:.4}s"
        );
        tokio::time::sleep(SAMPLE_INTERVAL).await;
    }

    // ── Scenario 4: Seek while playing ───────────────────────
    println!("\n  === Scenario 4: Seek to 30s while playing ===");
    p_master.transport().set_position(30.0).await?;
    println!("  [master] seeked to 30s");

    tokio::time::sleep(Duration::from_secs(3)).await;

    let (m, f, _) = ctx
        .assert_positions_close(
            "master",
            "follower",
            MAX_DRIFT_AFTER_CHANGE,
            "after seek to 30s",
        )
        .await?;
    println!("  After seek: master={m:.4}s follower={f:.4}s");

    // ── Scenario 5: Stop ─────────────────────────────────────
    println!("\n  === Scenario 5: Stop ===");
    p_master.transport().stop().await?;
    println!("  [master] stopped");

    // Poll for follower to stop — event may take a few hundred ms to propagate
    let mut stopped = false;
    for _ in 0..10 {
        tokio::time::sleep(Duration::from_millis(500)).await;
        let f_state = follower.daw.current_project().await?.transport().get_state().await?;
        if matches!(f_state.play_state, PlayState::Stopped) {
            stopped = true;
            break;
        }
    }
    assert!(stopped, "follower should be stopped within 5 seconds");
    println!("  [follower] confirmed stopped");

    // ── Scenario 6: Restart and verify convergence ───────────
    println!("\n  === Scenario 6: Restart from position 0 ===");
    p_master.transport().set_position(0.0).await?;
    p_master.transport().play().await?;
    println!("  [master] restarted from 0");

    tokio::time::sleep(Duration::from_secs(2)).await;

    let f_state = follower.daw.current_project().await?.transport().get_state().await?;
    assert!(
        matches!(f_state.play_state, PlayState::Playing),
        "follower should be playing after restart"
    );

    println!("\n  === Final: Post-restart position check (3s) ===");
    let mut max_drift_final: f64 = 0.0;
    for i in 0..15 {
        // First few samples after restart may still be converging
        let tol = if i < 3 { MAX_DRIFT_AFTER_CHANGE } else { MAX_DRIFT_SECS };
        let (m, f, drift) = ctx
            .assert_positions_close(
                "master",
                "follower",
                tol,
                &format!("post-restart sample {i}"),
            )
            .await?;
        max_drift_final = max_drift_final.max(drift);
        println!(
            "    sample {i:2}: master={m:.4}s follower={f:.4}s drift={drift:.4}s"
        );
        tokio::time::sleep(SAMPLE_INTERVAL).await;
    }
    println!("  Max drift post-restart: {max_drift_final:.4}s");

    // ── Cleanup ──────────────────────────────────────────────
    p_master.transport().stop().await?;
    tokio::time::sleep(Duration::from_millis(500)).await;

    println!("\n  All position sync assertions passed!");
    println!("  Peak drift during sustained playback: {max_drift:.4}s");
    println!("  Peak drift after restart: {max_drift_final:.4}s");
    Ok(())
}
