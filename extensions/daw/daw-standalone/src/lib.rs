//! DAW Standalone Service - In-Memory Implementation
//!
//! This provides a standalone DAW implementation that works in WASM and native.
//! Perfect for testing UI code without requiring REAPER.
//!
//! # Features
//!
//! - ✅ Works in WASM (browser) and native
//! - ✅ In-memory transport state
//! - ✅ Same service traits as real DAW
//! - ✅ Simulated playback with timer

mod local_client;
pub use local_client::LocalDawClient;

use daw_proto::*;
use parking_lot::Mutex;
use roam_session::Context;
use std::sync::Arc;
use wasm_timer::Instant;  // Works in both WASM and native
use tracing::info;

/// In-memory DAW state
#[derive(Debug, Clone)]
struct DawState {
    is_playing: bool,
    is_recording: bool,
    position_seconds: f64,
    tempo_bpm: f64,
    start_time: Option<Instant>,
}

impl Default for DawState {
    fn default() -> Self {
        Self {
            is_playing: false,
            is_recording: false,
            position_seconds: 0.0,
            tempo_bpm: 120.0,
            start_time: None,
        }
    }
}

/// Standalone DAW transport service
#[derive(Clone)]
pub struct DawStandaloneTransport {
    state: Arc<Mutex<DawState>>,
}

impl DawStandaloneTransport {
    /// Create a new standalone transport service
    pub fn new() -> Self {
        info!("Creating standalone DAW transport service");
        Self {
            state: Arc::new(Mutex::new(DawState::default())),
        }
    }

    /// Update position based on elapsed time (for simulated playback)
    fn update_position(&self) {
        let mut state = self.state.lock();
        if state.is_playing {
            if let Some(start_time) = state.start_time {
                let elapsed = start_time.elapsed().as_secs_f64();
                state.position_seconds += elapsed;
                state.start_time = Some(Instant::now());
            }
        }
    }
}

impl Default for DawStandaloneTransport {
    fn default() -> Self {
        Self::new()
    }
}

impl TransportService for DawStandaloneTransport {
    async fn play(&self, _cx: &Context) -> TransportResult {
        info!("Standalone DAW: play");
        let mut state = self.state.lock();
        state.is_playing = true;
        state.is_recording = false;
        state.start_time = Some(Instant::now());
        TransportResult::Success
    }

    async fn stop(&self, _cx: &Context) -> TransportResult {
        info!("Standalone DAW: stop");
        self.update_position();
        let mut state = self.state.lock();
        state.is_playing = false;
        state.is_recording = false;
        state.start_time = None;
        TransportResult::Success
    }

    async fn record(&self, _cx: &Context) -> TransportResult {
        info!("Standalone DAW: record");
        let mut state = self.state.lock();
        state.is_playing = true;
        state.is_recording = true;
        state.start_time = Some(Instant::now());
        TransportResult::Success
    }

    async fn get_state(&self, _cx: &Context) -> PlaybackState {
        self.update_position();
        let state = self.state.lock();
        PlaybackState {
            is_playing: state.is_playing,
            is_recording: state.is_recording,
            position_seconds: state.position_seconds,
            tempo_bpm: state.tempo_bpm,
        }
    }

    async fn set_position(&self, _cx: &Context, seconds: f64) -> TransportResult {
        info!("Standalone DAW: set_position to {:.2}s", seconds);
        let mut state = self.state.lock();
        state.position_seconds = seconds;
        if state.is_playing {
            state.start_time = Some(Instant::now());
        }
        TransportResult::Success
    }
}

// Stub implementations for other services
#[derive(Clone)]
pub struct DawStandaloneMarkers;

impl MarkerRegionService for DawStandaloneMarkers {
    async fn add_marker(&self, _cx: &Context, position: f64, name: String) -> MarkerResult {
        info!("Standalone DAW: add_marker at {:.2}s, name: {}", position, name);
        MarkerResult::Error {
            message: "Standalone service - markers not persisted".to_string(),
        }
    }

    async fn get_markers(&self, _cx: &Context) -> MarkerResult {
        MarkerResult::SuccessList { markers: vec![] }
    }

    async fn delete_marker(&self, _cx: &Context, _index: usize) -> MarkerResult {
        MarkerResult::Error {
            message: "Standalone service - no markers to delete".to_string(),
        }
    }
}

#[derive(Clone)]
pub struct DawStandaloneTracks;

impl TrackService for DawStandaloneTracks {
    async fn get_track_count(&self, _cx: &Context) -> usize {
        0
    }

    async fn get_track(&self, _cx: &Context, index: usize) -> TrackResult {
        TrackResult::Error {
            message: format!("Standalone service - track {} not found", index),
        }
    }

    async fn get_all_tracks(&self, _cx: &Context) -> TrackResult {
        TrackResult::SuccessList { tracks: vec![] }
    }

    async fn set_track_name(&self, _cx: &Context, index: usize, _name: String) -> TrackResult {
        TrackResult::Error {
            message: format!("Standalone service - track {} not found", index),
        }
    }
}
