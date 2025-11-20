//! Shared WebSocket message types
//!
//! These types are used by both client and server for type-safe WebSocket communication

use serde::{Deserialize, Serialize};
use setlist::Setlist;
use std::collections::HashMap;

/// Messages that can be sent over WebSocket
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum SetlistMessage {
    /// Full setlist update
    SetlistUpdate {
        setlist: Setlist,
        /// Index of the active song in the setlist (None if no active song)
        active_song_index: Option<usize>,
    },
    /// Transport state update for a specific project (playing, position, etc.)
    TransportUpdate {
        project_name: String,
        is_active: bool,
        playing: bool,
        position: f64,
        tempo: f64,
        /// Progress for each song in this project (0-1), keyed by song name
        song_progress: HashMap<String, f64>,
        /// Progress for each section in each song (0-1), keyed by "song_name:section_name"
        section_progress: HashMap<String, f64>,
    },
    /// Client request for setlist update
    RequestSetlist,
    /// Ping message for connection health
    Ping,
    /// Pong response to ping
    Pong,
}

