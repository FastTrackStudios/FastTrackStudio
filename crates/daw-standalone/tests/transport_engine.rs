//! End-to-end test: soft-clock advances playhead through the proto
//! `Transport` service.

use std::time::Duration;

use daw_proto::transport::service::Transport;
use daw_proto::{PlayState, ProjectContext, ProjectInfo};
use daw_standalone::sync::Standalone;

fn seeded() -> (Standalone, String) {
    let daw = Standalone::new();
    let guid = daw.seed_project(ProjectInfo {
        guid: "test-proj".into(),
        name: "test".into(),
        path: String::new(),
    });
    (daw, guid)
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn soft_clock_advances_playhead_when_playing() {
    let (daw, _guid) = seeded();
    let ctx = ProjectContext::Current;

    assert_eq!(daw.get_position(ctx.clone()), 0.0);
    Transport::play(&daw, ctx.clone()).unwrap();
    tokio::time::sleep(Duration::from_millis(120)).await;

    let pos = daw.get_position(ctx);
    assert!(
        pos > 0.05,
        "expected playhead to have advanced ~100ms, got {pos}s"
    );
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn stop_freezes_playhead() {
    let (daw, _guid) = seeded();
    let ctx = ProjectContext::Current;

    Transport::play(&daw, ctx.clone()).unwrap();
    tokio::time::sleep(Duration::from_millis(80)).await;
    Transport::stop(&daw, ctx.clone()).unwrap();
    let a = daw.get_position(ctx.clone());
    tokio::time::sleep(Duration::from_millis(80)).await;
    let b = daw.get_position(ctx);
    assert!(
        (b - a).abs() < 0.01,
        "expected playhead frozen after stop: a={a}, b={b}"
    );
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn varispeed_doubles_advance_rate() {
    let (daw, _guid) = seeded();
    let ctx = ProjectContext::Current;

    Transport::set_playrate(&daw, ctx.clone(), 2.0).unwrap();
    Transport::play(&daw, ctx.clone()).unwrap();
    tokio::time::sleep(Duration::from_millis(120)).await;
    let pos = daw.get_position(ctx);
    // At 2x for ~100ms expect ~0.2s.
    assert!(pos > 0.12, "expected >0.12s at 2x for 120ms, got {pos}s");
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn time_selection_loop_wraps_playhead() {
    let (daw, _guid) = seeded();
    let ctx = ProjectContext::Current;

    Transport::set_time_selection(&daw, ctx.clone(), 0.0, 0.1).unwrap();
    Transport::set_loop(&daw, ctx.clone(), true).unwrap();
    Transport::play(&daw, ctx.clone()).unwrap();
    // ~300ms of playback against a 100ms loop should keep playhead
    // inside [0, 0.1].
    tokio::time::sleep(Duration::from_millis(300)).await;
    let pos = daw.get_position(ctx);
    assert!(
        (0.0..=0.11).contains(&pos),
        "expected playhead wrapped within loop, got {pos}s"
    );
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn tempo_map_dynamic_drives_musical_position() {
    use daw_proto::TempoMap;

    let (daw, _guid) = seeded();
    let ctx = ProjectContext::Current;

    // Two segments: 120 BPM from 0..2s, 60 BPM thereafter.
    TempoMap::add_tempo_point(&daw, ctx.clone(), 0.0, 120.0).unwrap();
    TempoMap::add_tempo_point(&daw, ctx.clone(), 2.0, 60.0).unwrap();

    // At t=1s (mid-first-segment) we expect 2 beats.
    let (_m1, _b1, _f1) = TempoMap::time_to_musical(&daw, ctx.clone(), 1.0);
    // Reading via the engine directly through subscribe path:
    let bundle = daw.transport_engine_for("test-proj");
    let map = bundle.dynamic_tempo().expect("dynamic map installed");
    let clock = daw_standalone::transport_engine::SampleClock::new(bundle.shared.sample_rate());
    // At t=3s = 4 beats (first 2s) + 1 beat (60BPM for 1s).
    let s = clock.seconds_to_samples(daw_standalone::transport_engine::InstantSeconds(3.0));
    let mu = map.samples_to_musical(s, 1.0, &clock);
    assert!(
        (mu.0 - 5.0).abs() < 1e-6,
        "expected 5 beats at t=3s, got {}",
        mu.0
    );
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn tempo_map_falls_back_to_static_with_zero_or_one_points() {
    use daw_proto::TempoMap;

    let (daw, _guid) = seeded();
    let ctx = ProjectContext::Current;

    // No points → static fallback.
    let bundle = daw.transport_engine_for("test-proj");
    assert!(bundle.dynamic_tempo().is_none());

    // One point → still falls back to static, but BPM is mirrored.
    TempoMap::add_tempo_point(&daw, ctx.clone(), 0.0, 90.0).unwrap();
    assert!(bundle.dynamic_tempo().is_none());
    assert!((bundle.shared.tempo_bpm() - 90.0).abs() < 1e-9);

    // Second point → dynamic map installs.
    TempoMap::add_tempo_point(&daw, ctx.clone(), 1.0, 180.0).unwrap();
    assert!(bundle.dynamic_tempo().is_some());
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn get_state_mirrors_engine_playhead() {
    let (daw, _guid) = seeded();
    let ctx = ProjectContext::Current;

    Transport::play(&daw, ctx.clone()).unwrap();
    tokio::time::sleep(Duration::from_millis(100)).await;
    let state = daw.get_state(ctx);
    assert!(matches!(state.play_state, PlayState::Playing));
    let secs = state.playhead_position.time.as_ref().unwrap().as_seconds();
    assert!(secs > 0.05, "state-mirrored playhead = {secs}");
}
