//! Group mode definitions
//!
//! Defines the different view modes for groups (Full, Recording, Midi, Minimal).
//! These modes determine which tracks are visible in different views.

use serde::{Deserialize, Serialize};

/// Group mode/view type
///
/// Determines which tracks are visible in different views:
/// - Full: All tracks in the group
/// - Recording: Only recording-related tracks (audio inputs, mics)
/// - Midi: Only MIDI-related tracks
/// - Minimal: Only essential tracks (usually just the main group track)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum GroupMode {
    /// Full view - all tracks visible
    Full,
    /// Recording view - only recording-related tracks
    Recording,
    /// MIDI view - only MIDI-related tracks
    Midi,
    /// Minimal view - only essential tracks
    Minimal,
}

impl GroupMode {
    /// Get all available modes
    pub fn all() -> Vec<GroupMode> {
        vec![
            GroupMode::Full,
            GroupMode::Recording,
            GroupMode::Midi,
            GroupMode::Minimal,
        ]
    }
    
    /// Get mode name as string
    pub fn as_str(&self) -> &'static str {
        match self {
            GroupMode::Full => "full",
            GroupMode::Recording => "recording",
            GroupMode::Midi => "midi",
            GroupMode::Minimal => "minimal",
        }
    }
    
    /// Parse mode from string
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "full" => Some(GroupMode::Full),
            "recording" => Some(GroupMode::Recording),
            "midi" => Some(GroupMode::Midi),
            "minimal" => Some(GroupMode::Minimal),
            _ => None,
        }
    }
}

impl Default for GroupMode {
    fn default() -> Self {
        GroupMode::Full
    }
}
