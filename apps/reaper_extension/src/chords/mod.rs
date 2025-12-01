//! Chord detection and display module
//!
//! Reads MIDI data from REAPER tracks, detects chords using keyflow,
//! and displays them in multiple notation formats.

pub mod read;
pub mod detection;
pub mod display;
pub mod types;
pub mod service;
pub mod reactive;
pub mod reactive_irpc;

// Re-export for convenience
pub use reactive_irpc::{ChordsApi, ChordsUpdateMessage};
pub use types::ChordsData;

use keyflow::Key;
use tracing::info;
use crate::chords::types::{ChordAnalysis, ChordConfig, ChordData, ChordsData};
use crate::chords::read::read_midi_notes_from_reaper;
use crate::chords::detection::detect_chords;
use crate::chords::display::format_chord_analysis;

/// Read and analyze chords from a REAPER track
pub fn analyze_chords_from_track(
    track_name: &str,
    key: Option<Key>,
) -> Result<ChordsData, ChordError> {
    info!(track_name = track_name, "Reading chords from track");

    // Read MIDI notes
    let notes = read_midi_notes_from_reaper(track_name)
        .map_err(ChordError::ReadError)?;

    // Create config
    let mut config = ChordConfig::default();
    config.track_name = track_name.to_string();
    config.key = key;

    // Detect chords
    let chord_data = detect_chords(&notes, &config)
        .map_err(ChordError::DetectionError)?;

    // Format chords with all display formats
    let chord_analyses: Vec<ChordAnalysis> = chord_data
        .iter()
        .map(|data| format_chord_analysis(data, config.key.as_ref()))
        .collect();

    Ok(ChordsData {
        chords: chord_analyses,
        track_name: track_name.to_string(),
        key: config.key,
    })
}

/// Parse a key from a string (e.g., "C", "Am", "G#", "bBb")
pub fn parse_key(key_str: &str) -> Result<Key, String> {
    Key::parse(key_str)
}

#[derive(Debug, thiserror::Error)]
pub enum ChordError {
    #[error("Failed to read MIDI data: {0}")]
    ReadError(#[from] crate::chords::read::ChordReadError),
    #[error("Failed to detect chords: {0}")]
    DetectionError(#[from] crate::chords::detection::ChordDetectionError),
    #[error("Invalid key signature: {0}")]
    InvalidKey(String),
}

