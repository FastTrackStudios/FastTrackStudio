//! In-process `Daw` client backed by `Standalone`.
//!
//! Proves the backend-swap path: a `daw_control::Daw` instance built
//! from a `Standalone` server is functionally identical (for the
//! domains Standalone implements) to one backed by REAPER. This is
//! the foundation for running `daw-synchronization`-style tests
//! against Standalone.

#![cfg(feature = "bootstrap")]

use daw_proto::{PlayState, ProjectInfo};
use daw_standalone::bootstrap::build_in_process_daw;
use daw_standalone::sync::Standalone;

fn seeded() -> Standalone {
    let s = Standalone::new();
    s.seed_project(ProjectInfo {
        guid: "test-proj".into(),
        name: "test".into(),
        path: String::new(),
    });
    s
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn current_project_through_in_process_daw() -> eyre::Result<()> {
    let bundle = build_in_process_daw(seeded()).await?;
    let project = bundle.daw.current_project().await?;
    // GUID round-trips through the RPC client.
    let info = project.info().await?;
    assert_eq!(info.guid, "test-proj");
    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn transport_play_through_in_process_daw() -> eyre::Result<()> {
    let bundle = build_in_process_daw(seeded()).await?;
    let project = bundle.daw.current_project().await?;
    let transport = project.transport();

    assert!(!transport.is_playing().await?);
    transport.play().await?;
    // Drive a few soft-clock ticks so the engine advances measurably.
    tokio::time::sleep(std::time::Duration::from_millis(120)).await;
    assert!(transport.is_playing().await?);
    let pos = transport.get_position().await?;
    assert!(pos > 0.05, "expected playhead to advance, got {pos}s");
    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn transport_reaper_parity_through_in_process_daw() -> eyre::Result<()> {
    let bundle = build_in_process_daw(seeded()).await?;
    let project = bundle.daw.current_project().await?;
    let transport = project.transport();

    assert_eq!(transport.get_play_state().await?, PlayState::Stopped);
    assert!(!transport.is_playing().await?);
    assert!(!transport.is_recording().await?);

    transport.play().await?;
    tokio::time::sleep(std::time::Duration::from_millis(120)).await;
    assert_eq!(transport.get_play_state().await?, PlayState::Playing);
    assert!(transport.is_playing().await?);
    assert!(
        transport.get_position().await? > 0.05,
        "standalone playhead should advance while playing"
    );

    transport.pause().await?;
    assert_eq!(transport.get_play_state().await?, PlayState::Paused);
    assert!(!transport.is_playing().await?);

    transport.play_pause().await?;
    assert_eq!(transport.get_play_state().await?, PlayState::Playing);
    transport.play_pause().await?;
    assert_eq!(transport.get_play_state().await?, PlayState::Paused);

    transport.play_stop().await?;
    assert_eq!(transport.get_play_state().await?, PlayState::Playing);
    transport.play_stop().await?;
    assert_eq!(transport.get_play_state().await?, PlayState::Stopped);
    assert!(!transport.is_playing().await?);

    transport.set_position(5.0).await?;
    assert!((transport.get_position().await? - 5.0).abs() < 0.05);
    transport.goto_start().await?;
    assert!(transport.get_position().await? < 0.05);

    let state = transport.get_state().await?;
    assert_eq!(state.play_state, PlayState::Stopped);
    assert!(state.tempo.bpm > 0.0);

    let original_tempo = transport.get_tempo().await?;
    transport.set_tempo(140.0).await?;
    assert!((transport.get_tempo().await? - 140.0).abs() < 1e-9);
    transport.set_tempo(original_tempo).await?;

    transport.set_loop(true).await?;
    assert!(transport.is_looping().await?);
    transport.toggle_loop().await?;
    assert!(!transport.is_looping().await?);

    transport.set_time_selection(4.0, 2.0).await?;
    let selection = transport
        .get_time_selection()
        .await?
        .expect("time selection should be set");
    assert_eq!(selection.start_seconds, 2.0);
    assert_eq!(selection.end_seconds, 4.0);
    transport.clear_time_selection().await?;
    assert!(transport.get_time_selection().await?.is_none());

    transport.set_playrate(0.5).await?;
    assert!((transport.get_playrate().await? - 0.5).abs() < 1e-9);
    transport.set_playrate(1.0).await?;

    let ts = transport.get_time_signature().await?;
    assert!(ts.numerator > 0);
    assert!(ts.denominator > 0);

    transport.record().await?;
    assert_eq!(transport.get_play_state().await?, PlayState::Recording);
    assert!(transport.is_playing().await?);
    assert!(transport.is_recording().await?);
    transport.stop_recording().await?;
    assert_eq!(transport.get_play_state().await?, PlayState::Stopped);
    assert!(!transport.is_recording().await?);

    transport.toggle_recording().await?;
    assert_eq!(transport.get_play_state().await?, PlayState::Recording);
    transport.toggle_recording().await?;
    assert_eq!(transport.get_play_state().await?, PlayState::Stopped);

    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn tempo_round_trip_through_in_process_daw() -> eyre::Result<()> {
    let bundle = build_in_process_daw(seeded()).await?;
    let project = bundle.daw.current_project().await?;
    project.transport().set_tempo(140.0).await?;
    let bpm = project.transport().get_tempo().await?;
    assert!((bpm - 140.0).abs() < 1e-9);
    Ok(())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn marker_add_through_in_process_daw() -> eyre::Result<()> {
    let bundle = build_in_process_daw(seeded()).await?;
    let project = bundle.daw.current_project().await?;
    let _ = project.markers().add(1.5, "test-marker").await?;
    let markers = project.markers().all().await?;
    assert!(
        markers.iter().any(|m| m.name == "test-marker"),
        "marker should appear in list, got {:?}",
        markers.iter().map(|m| &m.name).collect::<Vec<_>>()
    );
    Ok(())
}
