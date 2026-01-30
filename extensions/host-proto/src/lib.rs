//! Unified host service protocol for FastTrackStudio extensions.
//!
//! This crate defines a single `HostService` trait that combines all host-facing
//! RPC methods. Every extension connects to the same host service - no per-extension
//! dispatcher configuration needed.
//!
//! Following dodeca's architecture pattern.

use facet::Facet;
use roam::Tx;

// Re-export types from extension protocols
pub use daw_proto::{
    DawLoggerService, LoggerResult,
    TransportService, TransportResult, PlaybackState,
    MarkerRegionService, MarkerResult, Marker,
    TrackService, TrackResult, Track,
};

// ============================================================================
// Extension Lifecycle
// ============================================================================

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, Facet)]
pub struct ReadyMsg {
    pub extension_name: String,
    pub peer_id: u8,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, Facet)]
pub struct ReadyAck {
    pub ok: bool,
}

// ============================================================================
// Unified Host Service
// ============================================================================

/// Unified host service that all extensions can call.
///
/// This combines all host-facing RPC methods into a single service:
/// - Extension lifecycle (readiness handshake)
/// - DAW services (transport, logging, markers, tracks)
/// - Future: lyrics, chart, setlist services
///
/// Following dodeca's pattern: all cross-extension RPC goes through the host.
#[roam::service]
pub trait HostService {
    // =========================================================================
    // Extension Lifecycle
    // =========================================================================

    /// Extension calls this after starting to signal it's ready for RPC requests.
    async fn ready(&self, msg: ReadyMsg) -> ReadyAck;

    // =========================================================================
    // DAW Transport Service (forwarded to daw-reaper)
    // =========================================================================

    /// Play transport
    async fn play(&self) -> TransportResult;

    /// Stop transport
    async fn stop(&self) -> TransportResult;

    /// Start recording
    async fn record(&self) -> TransportResult;

    /// Get current playback state
    async fn get_state(&self) -> PlaybackState;

    /// Set playback position
    async fn set_position(&self, seconds: f64) -> TransportResult;

    /// Subscribe to transport state updates (~60fps).
    ///
    /// Pushes `PlaybackState` through the channel until closed.
    /// Call this once; the server will continuously push updates.
    async fn subscribe_transport(&self, output: Tx<PlaybackState>);

    // =========================================================================
    // DAW Logger Service (forwarded to daw-reaper)
    // =========================================================================

    /// Log a message to REAPER console
    async fn log(&self, message: String) -> LoggerResult;

    // =========================================================================
    // DAW Marker/Region Service (forwarded to daw-reaper)
    // =========================================================================

    /// Add a marker at the given position
    async fn add_marker(&self, position: f64, name: String) -> MarkerResult;

    /// Get all markers
    async fn get_markers(&self) -> MarkerResult;

    /// Delete a marker by index
    async fn delete_marker(&self, index: usize) -> MarkerResult;

    // =========================================================================
    // DAW Track Service (forwarded to daw-reaper)
    // =========================================================================

    /// Get track count
    async fn get_track_count(&self) -> usize;

    /// Get track by index
    async fn get_track(&self, index: usize) -> TrackResult;

    /// Get all tracks
    async fn get_all_tracks(&self) -> TrackResult;

    /// Set track name
    async fn set_track_name(&self, index: usize, name: String) -> TrackResult;
}
