//! DAW Service Protocol Definitions - Transport Only
//!
//! Minimal protocol for transport control.
//! DAW-agnostic interface - implementations provided by daw-reaper, daw-standalone, etc.

use facet::Facet;

// ============================================================================
// Transport Service
// ============================================================================

#[derive(Debug, Clone, Facet)]
#[repr(u8)]
pub enum TransportResult {
    Success,
    Error { message: String },
}

#[derive(Debug, Clone, PartialEq, Facet)]
pub struct PlaybackState {
    pub is_playing: bool,
    pub is_paused: bool,
    pub is_recording: bool,
    pub position_seconds: f64,
    pub tempo_bpm: f64,
}

#[roam::service]
pub trait Transport {
    async fn play(&self) -> TransportResult;
    async fn stop(&self) -> TransportResult;
    async fn pause(&self) -> TransportResult;
    async fn record(&self) -> TransportResult;
    async fn get_state(&self) -> PlaybackState;
    async fn set_position(&self, seconds: f64) -> TransportResult;
}
