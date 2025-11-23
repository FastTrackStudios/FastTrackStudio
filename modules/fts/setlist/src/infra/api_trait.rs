//! SetlistAPI trait - Abstract interface for getting setlist and transport state
//!
//! This trait defines how to GET the setlist and transport state from different sources.
//! The actual computation of derived fields (songs, active song, etc.) is done
//! directly on the Setlist struct itself.
//!
//! Multiple implementations can exist:
//! - Dioxus signals implementation (for UI)
//! - HTTP implementation (for REST API)
//! - OSC implementation (for Open Sound Control)
//! - iroh-docs implementation (for P2P sync)

use crate::core::Setlist;

/// Transport state needed for computing active song/section
#[derive(Debug, Clone)]
pub struct TransportState {
    pub is_playing: bool,
    pub is_recording: bool,
    pub current_position_seconds: f64,
    pub current_position_beats: f64,
    pub tempo: f64,
    pub time_sig_numerator: i32,
    pub time_sig_denominator: i32,
}

/// App state (counting in, loop, etc.)
#[derive(Debug, Clone)]
pub struct AppState {
    pub is_counting_in: bool,
    pub loop_enabled: bool,
    pub loop_start: f64,
    pub loop_end: f64,
    pub queued_name: (String, String),
    pub queued_index: (usize, usize),
}

/// Trait for getting setlist and transport state from different sources
///
/// The actual computation of derived fields is done on the Setlist struct itself.
pub trait SetlistDataSource {
    /// Get the setlist (if available)
    fn get_setlist(&self) -> Option<Setlist>;
    
    /// Get the active song index (if known)
    fn get_active_song_index(&self) -> Option<usize>;
    
    /// Get transport state
    fn get_transport_state(&self) -> TransportState;
    
    /// Get app state
    fn get_app_state(&self) -> AppState;
}
