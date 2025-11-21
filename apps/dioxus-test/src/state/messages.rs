//! Shared message types for WebSocket communication
//!
//! These match the SetlistMessage enum from reaper_extension
//! Split into ClientEvent (sent from client) and ServerEvent (sent from server)

use setlist::Setlist;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Events sent from client to server
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ClientEvent {
    /// Client request for setlist update
    RequestSetlist,
    /// Client request to switch to a project by name
    SwitchToProject {
        project_name: String,
    },
    /// Client request to seek to a section's start position
    SeekToSection {
        project_name: String,
        song_name: String,
        section_name: String,
    },
    /// Navigate to next section
    NavigateNextSection,
    /// Navigate to previous section
    NavigatePreviousSection,
    /// Navigate to next song
    NavigateNextSong,
    /// Navigate to previous song
    NavigatePreviousSong,
    /// Toggle play/pause
    TogglePlayPause,
    /// Toggle loop
    ToggleLoop,
    /// Ping message for connection health
    Ping,
}

/// Events sent from server to client
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ServerEvent {
    /// Full setlist update
    SetlistUpdate {
        setlist: Setlist,
        /// Index of the active song in the setlist (None if no active song)
        active_song_index: Option<usize>,
    },
    /// Transport state update for a specific project (playing, position, etc.)
    /// Includes progress values (0-1) for active song and section, similar to AbleSet
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
    /// Pong response to ping
    Pong,
}

