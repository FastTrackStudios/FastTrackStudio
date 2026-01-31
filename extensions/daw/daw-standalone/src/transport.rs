//! Transport Service Implementation for Standalone

use daw_proto::*;
use parking_lot::Mutex;
use roam_session::Context;
use std::sync::Arc;
use wasm_timer::Instant;
use tracing::info;

/// In-memory transport state
#[derive(Debug, Clone)]
struct TransportState {
    is_playing: bool,
    is_paused: bool,
    is_recording: bool,
    position_seconds: f64,
    tempo_bpm: f64,
    start_time: Option<Instant>,
}

impl Default for TransportState {
    fn default() -> Self {
        Self {
            is_playing: false,
            is_paused: false,
            is_recording: false,
            position_seconds: 0.0,
            tempo_bpm: 120.0,
            start_time: None,
        }
    }
}

/// Standalone transport service
#[derive(Clone)]
pub struct DawStandaloneTransport {
    state: Arc<Mutex<TransportState>>,
}

impl DawStandaloneTransport {
    pub fn new() -> Self {
        info!("Creating standalone transport service");
        Self {
            state: Arc::new(Mutex::new(TransportState::default())),
        }
    }

    fn update_position(&self) {
        let mut state = self.state.lock();
        if state.is_playing && !state.is_paused {
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

impl Transport for DawStandaloneTransport {
    async fn play(&self, _cx: &Context) -> TransportResult {
        info!("Standalone: play");
        let mut state = self.state.lock();
        state.is_playing = true;
        state.is_paused = false;
        state.start_time = Some(Instant::now());
        TransportResult::Success
    }

    async fn stop(&self, _cx: &Context) -> TransportResult {
        info!("Standalone: stop");
        let mut state = self.state.lock();
        state.is_playing = false;
        state.is_paused = false;
        state.is_recording = false;
        state.position_seconds = 0.0;
        state.start_time = None;
        TransportResult::Success
    }

    async fn pause(&self, _cx: &Context) -> TransportResult {
        info!("Standalone: pause");
        self.update_position();
        let mut state = self.state.lock();
        state.is_paused = !state.is_paused;
        if !state.is_paused {
            state.start_time = Some(Instant::now());
        }
        TransportResult::Success
    }

    async fn record(&self, _cx: &Context) -> TransportResult {
        info!("Standalone: record");
        let mut state = self.state.lock();
        state.is_playing = true;
        state.is_recording = !state.is_recording;
        state.is_paused = false;
        state.start_time = Some(Instant::now());
        TransportResult::Success
    }

    async fn get_state(&self, _cx: &Context) -> PlaybackState {
        self.update_position();
        let state = self.state.lock();
        PlaybackState {
            is_playing: state.is_playing,
            is_paused: state.is_paused,
            is_recording: state.is_recording,
            position_seconds: state.position_seconds,
            tempo_bpm: state.tempo_bpm,
        }
    }

    async fn set_position(&self, _cx: &Context, seconds: f64) -> TransportResult {
        info!("Standalone: set_position to {:.2}s", seconds);
        let mut state = self.state.lock();
        state.position_seconds = seconds;
        if state.is_playing {
            state.start_time = Some(Instant::now());
        }
        TransportResult::Success
    }
}
