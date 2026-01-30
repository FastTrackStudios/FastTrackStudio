//! DAW Service Protocol Definitions
//!
//! Multiple service traits for different DAW subsystems.
//! All implemented by daw-reaper (or daw-native, etc.)
//!
//! This is the ONLY interface business logic extensions use to interact with the DAW.
//! NO reaper-rs dependencies - completely DAW-agnostic.

// ============================================================================
// Transport Service
// ============================================================================

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, facet::Facet)]
#[repr(u8)]
pub enum TransportResult {
    Success,
    Error { message: String },
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, facet::Facet)]
pub struct PlaybackState {
    pub is_playing: bool,
    pub is_recording: bool,
    pub position_seconds: f64,
    pub tempo_bpm: f64,
}

#[roam::service]
pub trait TransportService {
    async fn play(&self) -> TransportResult;
    async fn stop(&self) -> TransportResult;
    async fn record(&self) -> TransportResult;
    async fn get_state(&self) -> PlaybackState;
    async fn set_position(&self, seconds: f64) -> TransportResult;
}

// ============================================================================
// Marker/Region Service
// ============================================================================

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, facet::Facet)]
pub struct Marker {
    pub index: usize,
    pub position: f64,
    pub name: String,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, facet::Facet)]
#[repr(u8)]
pub enum MarkerResult {
    Success { marker: Marker },
    SuccessList { markers: Vec<Marker> },
    Error { message: String },
}

#[roam::service]
pub trait MarkerRegionService {
    async fn add_marker(&self, position: f64, name: String) -> MarkerResult;
    async fn get_markers(&self) -> MarkerResult;
    async fn delete_marker(&self, index: usize) -> MarkerResult;
}

// ============================================================================
// Track Service
// ============================================================================

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, facet::Facet)]
pub struct Track {
    pub index: usize,
    pub name: String,
    pub is_selected: bool,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, facet::Facet)]
#[repr(u8)]
pub enum TrackResult {
    Success { track: Track },
    SuccessList { tracks: Vec<Track> },
    Error { message: String },
}

#[roam::service]
pub trait TrackService {
    async fn get_track_count(&self) -> usize;
    async fn get_track(&self, index: usize) -> TrackResult;
    async fn get_all_tracks(&self) -> TrackResult;
    async fn set_track_name(&self, index: usize, name: String) -> TrackResult;
}

// ============================================================================
// Logger Service
// ============================================================================

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, facet::Facet)]
#[repr(u8)]
pub enum LoggerResult {
    Success,
    Error { message: String },
}

#[roam::service]
pub trait DawLoggerService {
    async fn log(&self, message: String) -> LoggerResult;
}
