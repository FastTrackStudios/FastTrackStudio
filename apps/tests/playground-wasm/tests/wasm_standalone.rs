//! WASM integration tests for daw-standalone.
//!
//! These tests run in a real browser via `wasm-bindgen-test`,
//! verifying that daw-standalone's transport, tracks, markers,
//! regions, and tempo map all work correctly in WASM.
//!
//! Run with:
//!   wasm-pack test --headless --chrome -p playground-wasm
//!
//! Or with cargo directly:
//!   cargo test --target wasm32-unknown-unknown -p playground-wasm

use wasm_bindgen_test::*;

wasm_bindgen_test_configure!(run_in_browser);

use daw::service::{
    PlayState, ProjectContext, ProjectService, TransportService,
    marker::MarkerService,
    region::RegionService,
    tempo_map::TempoMapService,
    track::{TrackRef, TrackService},
};
use daw::standalone::{
    StandaloneMarker, StandaloneProject, StandaloneRegion, StandaloneTempoMap, StandaloneTrack,
    StandaloneTransport,
};

// ─── Transport Tests ─────────────────────────────────────────────────────────

#[wasm_bindgen_test]
async fn transport_starts_stopped() {
    let project = StandaloneProject::new();
    let transport = StandaloneTransport::new(project.shared_state());

    let state = transport.get_play_state(ProjectContext::Current).await;
    assert_eq!(state, PlayState::Stopped);
}

#[wasm_bindgen_test]
async fn transport_play_stop_cycle() {
    let project = StandaloneProject::new();
    let transport = StandaloneTransport::new(project.shared_state());

    // Play
    transport.play(ProjectContext::Current).await;
    let state = transport.get_play_state(ProjectContext::Current).await;
    assert_eq!(state, PlayState::Playing);

    // Stop
    transport.stop(ProjectContext::Current).await;
    let state = transport.get_play_state(ProjectContext::Current).await;
    assert_eq!(state, PlayState::Stopped);
}

#[wasm_bindgen_test]
async fn transport_seek_position() {
    let project = StandaloneProject::new();
    let transport = StandaloneTransport::new(project.shared_state());

    transport.set_position(ProjectContext::Current, 42.5).await;
    let pos = transport.get_position(ProjectContext::Current).await;
    assert!((pos - 42.5).abs() < 0.01, "Expected ~42.5, got {pos}");
}

#[wasm_bindgen_test]
async fn transport_position_advances_during_playback() {
    let project = StandaloneProject::new();
    let transport = StandaloneTransport::new(project.shared_state());

    transport.set_position(ProjectContext::Current, 0.0).await;
    transport.play(ProjectContext::Current).await;

    // Wait ~100ms for position to advance
    gloo_timers::future::sleep(std::time::Duration::from_millis(100)).await;

    let pos = transport.get_position(ProjectContext::Current).await;
    transport.stop(ProjectContext::Current).await;

    assert!(pos > 0.05, "Position should have advanced, got {pos}");
}

#[wasm_bindgen_test]
async fn transport_play_pause_preserves_position() {
    let project = StandaloneProject::new();
    let transport = StandaloneTransport::new(project.shared_state());

    transport.set_position(ProjectContext::Current, 10.0).await;
    transport.play(ProjectContext::Current).await;
    gloo_timers::future::sleep(std::time::Duration::from_millis(50)).await;
    transport.pause(ProjectContext::Current).await;

    let pos = transport.get_position(ProjectContext::Current).await;
    assert!(pos >= 10.0, "Position should be >= 10.0 after play+pause, got {pos}");
}

#[wasm_bindgen_test]
async fn transport_tempo_get_set() {
    let project = StandaloneProject::new();
    let transport = StandaloneTransport::new(project.shared_state());

    transport.set_tempo(ProjectContext::Current, 140.0).await;
    let tempo = transport.get_tempo(ProjectContext::Current).await;
    assert!((tempo - 140.0).abs() < 0.01, "Expected 140 BPM, got {tempo}");
}

#[wasm_bindgen_test]
async fn transport_playrate() {
    let project = StandaloneProject::new();
    let transport = StandaloneTransport::new(project.shared_state());

    transport.set_playrate(ProjectContext::Current, 2.0).await;
    let rate = transport.get_playrate(ProjectContext::Current).await;
    assert!((rate - 2.0).abs() < 0.01, "Expected playrate 2.0, got {rate}");
}

#[wasm_bindgen_test]
async fn transport_loop_toggle() {
    let project = StandaloneProject::new();
    let transport = StandaloneTransport::new(project.shared_state());

    assert!(!transport.is_looping(ProjectContext::Current).await);
    transport.toggle_loop(ProjectContext::Current).await;
    assert!(transport.is_looping(ProjectContext::Current).await);
    transport.toggle_loop(ProjectContext::Current).await;
    assert!(!transport.is_looping(ProjectContext::Current).await);
}

// ─── Project Tests ───────────────────────────────────────────────────────────

#[wasm_bindgen_test]
async fn project_lists_mock_songs() {
    let project = StandaloneProject::new();
    let list = project.list().await;
    assert_eq!(list.len(), 3, "Expected 3 mock projects");
}

#[wasm_bindgen_test]
async fn project_get_current() {
    let project = StandaloneProject::new();
    let current = project.get_current().await;
    assert!(current.is_some(), "Should have a current project");
    assert_eq!(current.unwrap().name, "How Great is Our God");
}

#[wasm_bindgen_test]
async fn project_select_switches_current() {
    let project = StandaloneProject::new();
    let list = project.list().await;
    let second = &list[1];

    let ok = project.select(second.guid.clone()).await;
    assert!(ok, "Select should succeed");

    let current = project.get_current().await.unwrap();
    assert_eq!(current.guid, second.guid);
}

// ─── Track Tests ─────────────────────────────────────────────────────────────

#[wasm_bindgen_test]
async fn track_list_default() {
    let tracks = StandaloneTrack::new();
    let list = tracks.get_tracks(ProjectContext::Current).await;
    assert_eq!(list.len(), 4, "Expected 4 default tracks");
}

#[wasm_bindgen_test]
async fn track_mute_solo() {
    let tracks = StandaloneTrack::new();
    let list = tracks.get_tracks(ProjectContext::Current).await;
    let guid = list[0].guid.clone();

    // Mute
    tracks
        .set_muted(ProjectContext::Current, TrackRef::Guid(guid.clone()), true)
        .await;
    let track = tracks
        .get_track(ProjectContext::Current, TrackRef::Guid(guid.clone()))
        .await
        .unwrap();
    assert!(track.muted, "Track should be muted");

    // Solo
    tracks
        .set_soloed(ProjectContext::Current, TrackRef::Guid(guid.clone()), true)
        .await;
    let track = tracks
        .get_track(ProjectContext::Current, TrackRef::Guid(guid.clone()))
        .await
        .unwrap();
    assert!(track.soloed, "Track should be soloed");
}

#[wasm_bindgen_test]
async fn track_volume() {
    let tracks = StandaloneTrack::new();
    let list = tracks.get_tracks(ProjectContext::Current).await;
    let guid = list[0].guid.clone();

    tracks
        .set_volume(ProjectContext::Current, TrackRef::Guid(guid.clone()), 0.75)
        .await;
    let track = tracks
        .get_track(ProjectContext::Current, TrackRef::Guid(guid))
        .await
        .unwrap();
    assert!((track.volume - 0.75).abs() < 0.01);
}

// ─── Marker Tests ────────────────────────────────────────────────────────────

#[wasm_bindgen_test]
async fn markers_load_mock_data() {
    let markers = StandaloneMarker::new();
    // Default project (Current) should have markers
    let list = markers.get_markers(ProjectContext::Current).await;
    assert!(!list.is_empty(), "Should have mock markers");
}

#[wasm_bindgen_test]
async fn marker_add_and_count() {
    use daw::standalone::project_guids;
    let markers = StandaloneMarker::new();
    let ctx = ProjectContext::Project(project_guids::SONG1.to_string());
    let initial = markers.marker_count(ctx.clone()).await;

    markers
        .add_marker(ctx.clone(), 99.0, "Test Marker".to_string())
        .await;

    let after = markers.marker_count(ctx).await;
    assert_eq!(after, initial + 1);
}

// ─── Region Tests ────────────────────────────────────────────────────────────

#[wasm_bindgen_test]
async fn regions_load_mock_data() {
    let regions = StandaloneRegion::new();
    let list = regions.get_regions(ProjectContext::Current).await;
    assert!(!list.is_empty(), "Should have mock regions");
}

#[wasm_bindgen_test]
async fn region_at_position() {
    let regions = StandaloneRegion::new();
    // Position 50s should be in a region (mock data has sections from 0-240s)
    let region = regions.get_region_at(ProjectContext::Current, 50.0).await;
    assert!(region.is_some(), "Should find a region at 50s");
}

// ─── Tempo Map Tests ─────────────────────────────────────────────────────────

#[wasm_bindgen_test]
async fn tempo_map_get_points() {
    let tempo_map = StandaloneTempoMap::new();
    let points = tempo_map
        .get_tempo_points(ProjectContext::Current)
        .await;
    assert!(!points.is_empty(), "Should have tempo points");
}

#[wasm_bindgen_test]
async fn tempo_map_get_tempo_at() {
    let tempo_map = StandaloneTempoMap::new();
    // At time 0, default mock data is 120 BPM
    let bpm = tempo_map
        .get_tempo_at(ProjectContext::Current, 0.0)
        .await;
    assert!((bpm - 120.0).abs() < 0.01, "Expected 120 BPM at t=0, got {bpm}");
}

#[wasm_bindgen_test]
async fn tempo_map_time_to_musical() {
    let tempo_map = StandaloneTempoMap::new();
    let (measure, beat, _frac) = tempo_map
        .time_to_musical(ProjectContext::Current, 0.0)
        .await;
    assert_eq!(measure, 0, "Measure should be 0 at t=0");
    assert_eq!(beat, 0, "Beat should be 0 at t=0");
}

// ─── Cross-component Integration ─────────────────────────────────────────────

#[wasm_bindgen_test]
async fn transport_works_across_project_switch() {
    let project = StandaloneProject::new();
    let transport = StandaloneTransport::new(project.shared_state());

    // Play on project 1
    transport.play(ProjectContext::Current).await;
    assert!(transport.is_playing(ProjectContext::Current).await);

    // Switch to project 2
    let list = project.list().await;
    project.select(list[1].guid.clone()).await;

    // New current project should not be playing
    let state = transport.get_play_state(ProjectContext::Current).await;
    assert_eq!(state, PlayState::Stopped, "New project should start stopped");

    // Original project should still be playing (independent transport)
    let state = transport
        .get_play_state(ProjectContext::Project(list[0].guid.clone()))
        .await;
    assert_eq!(state, PlayState::Playing, "Original project should still be playing");

    // Clean up
    transport.stop(ProjectContext::Project(list[0].guid.clone())).await;
}

// ─── Audio Engine Tests ──────────────────────────────────────────────────────

use daw::standalone::audio_engine::{self, AudioEngine, DecodedAudio, test_tone};

#[wasm_bindgen_test]
fn test_tone_sine_wave() {
    let audio = test_tone::sine_wave(440.0, 1.0, 44100);
    assert_eq!(audio.channels, 1);
    assert_eq!(audio.sample_rate, 44100);
    assert_eq!(audio.frame_count(), 44100);
    assert!((audio.duration_seconds() - 1.0).abs() < 0.001);
}

#[wasm_bindgen_test]
fn test_tone_stereo() {
    let audio = test_tone::sine_wave_stereo(440.0, 2.0, 48000, 0.0);
    assert_eq!(audio.channels, 2);
    assert_eq!(audio.sample_rate, 48000);
    assert_eq!(audio.frame_count(), 96000);
    assert!((audio.duration_seconds() - 2.0).abs() < 0.001);
}

#[wasm_bindgen_test]
fn test_tone_chord() {
    let audio = test_tone::chord(&[261.63, 329.63, 392.0], 1.0, 44100);
    assert_eq!(audio.channels, 1);
    assert_eq!(audio.frame_count(), 44100);
    // Verify samples are non-zero (actually has audio content)
    let max_abs = audio.samples.iter().map(|s| s.abs()).fold(0.0f32, f32::max);
    assert!(max_abs > 0.1, "Chord should produce audible samples, max={max_abs}");
}

#[wasm_bindgen_test]
fn test_tone_click_track() {
    let audio = test_tone::click_track(120.0, 2.0, 44100);
    assert_eq!(audio.channels, 1);
    assert!((audio.duration_seconds() - 2.0).abs() < 0.001);
    // At 120 BPM, 2 seconds = 4 clicks. Verify there's some non-zero content.
    let non_zero = audio.samples.iter().filter(|&&s| s.abs() > 0.01).count();
    assert!(non_zero > 100, "Click track should have audible clicks");
}

#[wasm_bindgen_test]
fn test_tone_demo_tracks() {
    let tracks = test_tone::demo_tracks(5.0, 44100);
    assert_eq!(tracks.len(), 4);
    for (name, audio) in &tracks {
        assert!(!name.is_empty());
        assert!(audio.frame_count() > 0, "Track '{name}' should have audio");
        assert!((audio.duration_seconds() - 5.0).abs() < 0.01);
    }
}

#[wasm_bindgen_test]
fn decoded_audio_seconds_to_frame() {
    let audio = DecodedAudio {
        samples: vec![0.0; 44100],
        channels: 1,
        sample_rate: 44100,
    };
    assert_eq!(audio.seconds_to_frame(0.5), 22050);
    assert_eq!(audio.seconds_to_frame(1.0), 44100);
}

#[wasm_bindgen_test]
fn audio_engine_creates_in_browser() {
    // This test verifies the AudioEngine can be constructed in WASM.
    // Note: In headless browser tests, this may fail due to autoplay policy.
    // That's expected — the test documents the behavior.
    match AudioEngine::new() {
        Ok(engine) => {
            // Engine created successfully — verify basic operations
            let tone = test_tone::sine_wave(440.0, 1.0, engine.sample_rate());
            let handle = engine.add_track(tone);
            assert_eq!(engine.track_count(), 1);

            engine.set_track_gain(handle, 0.5);
            assert!((engine.track_gain(handle) - 0.5).abs() < 0.01);

            engine.play();
            assert!(engine.is_playing());

            engine.seek(0.5);
            assert!((engine.position_seconds() - 0.5).abs() < 0.1);

            engine.pause();
            assert!(!engine.is_playing());

            engine.stop();
            assert_eq!(engine.track_count(), 1);

            engine.clear_tracks();
            assert_eq!(engine.track_count(), 0);
        }
        Err(e) => {
            // Expected in headless mode — autoplay policy blocks AudioContext creation
            // This is not a test failure, just a browser limitation
            assert!(
                e.contains("audio") || e.contains("Audio") || e.contains("device") || e.contains("NotAllowed"),
                "Unexpected error: {e}"
            );
        }
    }
}
