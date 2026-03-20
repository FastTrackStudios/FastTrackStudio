//! Multi-instance position sync integration test.
//!
//! Spawns 2 REAPER instances via `run_multi_reaper_test`, connects them via
//! direct TCP PeerMesh, then verifies that transport position stays tightly
//! synced across scenarios: play, sustained playback, tempo change, seek
//! while playing, stop, and restart.
//!
//! Run with:
//!   cargo xtask reaper-test -- position_sync

use std::time::Duration;

use eyre::Result;
use reaper_test::{DawInstanceConfig, run_multi_reaper_test};

/// Maximum allowed position drift between master and follower (seconds).
const MAX_DRIFT_SECS: f64 = 0.05; // 50ms

/// How often to sample positions during sustained playback checks.
const SAMPLE_INTERVAL: Duration = Duration::from_millis(200);

/// Read transport position from an instance, returning seconds.
async fn get_position(daw: &daw::Daw) -> Result<f64> {
    let project = daw.current_project().await?;
    let state = project.transport().get_state().await?;
    let pos = state
        .playhead_position
        .time
        .as_ref()
        .map(|t| t.as_seconds())
        .unwrap_or(0.0);
    Ok(pos)
}

/// Read play state from an instance.
async fn is_playing(daw: &daw::Daw) -> Result<bool> {
    let project = daw.current_project().await?;
    let state = project.transport().get_state().await?;
    Ok(matches!(
        state.play_state,
        daw::service::PlayState::Playing
    ))
}

/// Sample positions from both instances and assert they're within tolerance.
async fn assert_positions_close(
    master: &daw::Daw,
    follower: &daw::Daw,
    context: &str,
) -> Result<(f64, f64)> {
    let master_pos = get_position(master).await?;
    let follower_pos = get_position(follower).await?;
    let drift = (master_pos - follower_pos).abs();
    assert!(
        drift < MAX_DRIFT_SECS,
        "{context}: position drift {drift:.4}s exceeds {MAX_DRIFT_SECS}s \
         (master={master_pos:.4}s, follower={follower_pos:.4}s)"
    );
    Ok((master_pos, follower_pos))
}

/// Poll an ExtState key until it matches `expected`, with retries.
async fn poll_ext_state(
    daw: &daw::Daw,
    section: &str,
    key: &str,
    expected: &str,
    retries: u32,
    interval_ms: u64,
) -> Result<()> {
    let ext = daw.ext_state();
    for i in 0..retries {
        if let Ok(Some(val)) = ext.get(section, key).await {
            if val == expected {
                return Ok(());
            }
        }
        if i < retries - 1 {
            tokio::time::sleep(Duration::from_millis(interval_ms)).await;
        }
    }
    Err(eyre::eyre!(
        "ExtState {section}/{key} did not reach '{expected}' after {retries} retries"
    ))
}

/// Poll an ExtState key until it has any non-empty value.
async fn poll_ext_state_value(
    daw: &daw::Daw,
    section: &str,
    key: &str,
    retries: u32,
    interval_ms: u64,
) -> Result<String> {
    let ext = daw.ext_state();
    for i in 0..retries {
        if let Ok(Some(val)) = ext.get(section, key).await {
            if !val.is_empty() {
                return Ok(val);
            }
        }
        if i < retries - 1 {
            tokio::time::sleep(Duration::from_millis(interval_ms)).await;
        }
    }
    Err(eyre::eyre!(
        "ExtState {section}/{key} never got a value after {retries} retries"
    ))
}

/// Test position sync between two REAPER instances connected via PeerMesh.
#[test]
#[ignore = "reaper-test: run with `cargo xtask reaper-test`"]
fn position_sync() -> Result<()> {
    // Multi-instance REAPERs need -cfgfile pointing to the FTS config dir
    // so they find UserPlugins/extensions and use the correct reaper.ini.
    let home = std::env::var("HOME").unwrap_or_else(|_| "/tmp".to_string());
    let ini_str = std::env::var("FTS_REAPER_CONFIG")
        .map(|p| format!("{p}/reaper.ini"))
        .unwrap_or_else(|_| format!("{home}/.config/FastTrackStudio/Reaper/reaper.ini"));

    let mut master_cfg = DawInstanceConfig::new("master");
    master_cfg.args.extend(["-cfgfile".to_string(), ini_str.clone()]);
    let mut follower_cfg = DawInstanceConfig::new("follower");
    follower_cfg.args.extend(["-cfgfile".to_string(), ini_str]);

    run_multi_reaper_test(
        "position_sync",
        vec![master_cfg, follower_cfg],
        |ctx| {
            Box::pin(async move {
                let master = ctx.by_label("master");
                let follower = ctx.by_label("follower");

                // ── Setup: wait for extensions, connect peers ────────────
                for (label, inst) in [("master", master), ("follower", follower)] {
                    poll_ext_state(&inst.daw, "FTS_SYNC_EXT", "status", "ready", 30, 1000)
                        .await
                        .unwrap_or_else(|_| {
                            panic!("{label}: sync extension did not become ready")
                        });
                    println!("  [{label}] sync extension ready");
                }

                let mut peers = Vec::new();
                for (label, inst) in [("master", master), ("follower", follower)] {
                    let port = poll_ext_state_value(
                        &inst.daw,
                        "FTS_SYNC_EXT",
                        "mesh_port",
                        10,
                        500,
                    )
                    .await
                    .unwrap_or_else(|_| panic!("{label}: no mesh_port"));
                    let peer_id = poll_ext_state_value(
                        &inst.daw,
                        "FTS_SYNC_EXT",
                        "peer_id",
                        10,
                        500,
                    )
                    .await
                    .unwrap_or_else(|_| panic!("{label}: no peer_id"));
                    println!("  [{label}] peer_id={peer_id}, mesh_port={port}");
                    peers.push((label, peer_id, port));
                }

                let connect_str: String = peers
                    .iter()
                    .map(|(_, peer_id, port)| format!("{peer_id}@127.0.0.1:{port}"))
                    .collect::<Vec<_>>()
                    .join(",");

                for (_label, inst) in [("master", master), ("follower", follower)] {
                    inst.daw
                        .ext_state()
                        .set("FTS_SYNC_EXT", "connect_peers", &connect_str, false)
                        .await?;
                }
                println!("  Triggered direct peer connections: {connect_str}");

                // Wait for both to see 1 peer
                for (label, inst) in [("master", master), ("follower", follower)] {
                    poll_ext_state(&inst.daw, "FTS_SYNC_EXT", "peer_count", "1", 30, 1000)
                        .await
                        .unwrap_or_else(|_| panic!("{label}: did not reach 1 peer"));
                    println!("  [{label}] connected to 1 peer");
                }

                // ── Scenario 1: Start playback, verify follower follows ──
                println!("\n  === Scenario 1: Play and verify position sync ===");
                let p_master = master.daw.current_project().await?;
                p_master.transport().play().await?;
                println!("  [master] playback started");

                // Give drift corrector time to converge
                tokio::time::sleep(Duration::from_secs(2)).await;

                assert!(
                    is_playing(&follower.daw).await?,
                    "follower should be playing"
                );
                println!("  [follower] confirmed playing");

                // ── Scenario 2: Sustained playback — sample positions over 5s ─
                println!("\n  === Scenario 2: Sustained playback position check (5s) ===");
                let mut max_drift: f64 = 0.0;
                for i in 0..25 {
                    let (m, f) = assert_positions_close(
                        &master.daw,
                        &follower.daw,
                        &format!("sustained playback sample {i}"),
                    )
                    .await?;
                    let drift = (m - f).abs();
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

                tokio::time::sleep(Duration::from_secs(3)).await;

                let follower_project = follower.daw.current_project().await?;
                let follower_tempo = follower_project.transport().get_tempo().await?;
                assert!(
                    (follower_tempo - 160.0).abs() < 1.0,
                    "follower tempo should be ~160, got {follower_tempo}"
                );
                println!("  [follower] tempo confirmed ~160 ({follower_tempo:.1})");

                for i in 0..10 {
                    let (m, f) = assert_positions_close(
                        &master.daw,
                        &follower.daw,
                        &format!("post-tempo-change sample {i}"),
                    )
                    .await?;
                    let drift = (m - f).abs();
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

                let (m, f) = assert_positions_close(
                    &master.daw,
                    &follower.daw,
                    "after seek to 30s",
                )
                .await?;
                println!("  After seek: master={m:.4}s follower={f:.4}s");

                // ── Scenario 5: Stop ─────────────────────────────────────
                println!("\n  === Scenario 5: Stop ===");
                p_master.transport().stop().await?;
                println!("  [master] stopped");

                tokio::time::sleep(Duration::from_secs(2)).await;

                assert!(
                    !is_playing(&follower.daw).await?,
                    "follower should be stopped"
                );
                println!("  [follower] confirmed stopped");

                // ── Scenario 6: Restart and verify convergence ───────────
                println!("\n  === Scenario 6: Restart from position 0 ===");
                p_master.transport().set_position(0.0).await?;
                p_master.transport().play().await?;
                println!("  [master] restarted from 0");

                tokio::time::sleep(Duration::from_secs(2)).await;

                assert!(
                    is_playing(&follower.daw).await?,
                    "follower should be playing after restart"
                );

                println!("\n  === Final: Post-restart position check (3s) ===");
                let mut max_drift_final: f64 = 0.0;
                for i in 0..15 {
                    let (m, f) = assert_positions_close(
                        &master.daw,
                        &follower.daw,
                        &format!("post-restart sample {i}"),
                    )
                    .await?;
                    let drift = (m - f).abs();
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
            })
        },
    )
}
