//! Command types for setlist operations
//!
//! These types are shared between the iroh streaming module and the reaper feature.
//! They define the basic command interfaces that can be used without the iroh feature.

use facet::Facet;
use serde::{Deserialize, Serialize};

/// Transport control commands
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
#[repr(u8)]
pub enum TransportCommand {
    Play,
    Pause,
    Stop,
    TogglePlayPause,
}

/// Navigation commands
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
#[repr(u8)]
pub enum NavigationCommand {
    NextSectionOrSong,
    PreviousSectionOrSong,
}

/// Lyrics state response
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct LyricsState {
    pub current_line_index: Option<usize>,
    pub current_syllable_index: Option<usize>,
    pub total_syllables: usize,
    pub line_text: Option<String>,
    pub has_lyrics: bool,
}
