//! Data structures for chord detection and display
//!
//! Uses keyflow types for all musical structure. Only timing
//! and configuration data is defined here.

use keyflow::{Chord, Key, MusicalNote};
use serde::{Deserialize, Serialize};

/// Display format for chords
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ChordDisplayFormat {
    /// Standard chord names: Amaj7, D/F#, Gm7b5
    Name,
    /// Nashville Number System: 1maj7, 1/3, 5m7
    Nashville,
    /// Roman Numerals: Imaj7, V/V7, V64, ivm7
    Roman,
}

/// A detected chord with timing information
///
/// The musical structure (chord, root note, bass note) comes from keyflow.
/// Only timing and MIDI data are DAW-specific.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChordData {
    /// The detected chord (from keyflow)
    pub chord: Chord,
    /// Start time in project time (seconds)
    pub start_time: f64,
    /// End time in project time (seconds)
    pub end_time: f64,
    /// Start position in PPQ (pulses per quarter)
    pub start_ppq: i32,
    /// End position in PPQ
    pub end_ppq: i32,
    /// Root note (from keyflow)
    pub root_note: MusicalNote,
    /// Bass note if different from root (for inversions, from keyflow)
    pub bass_note: Option<MusicalNote>,
}

/// Complete chord analysis with all display formats
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChordAnalysis {
    /// The chord data with timing
    pub data: ChordData,
    /// Display in standard chord name format (from keyflow's Display trait)
    pub name: String,
    /// Display in Nashville Number System format (requires key context)
    pub nashville: Option<String>,
    /// Display in Roman Numeral format (requires key context)
    pub roman: Option<String>,
}

/// Collection of detected chords
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChordsData {
    /// All detected chords in chronological order
    pub chords: Vec<ChordAnalysis>,
    /// Track name from which chords were read
    pub track_name: String,
    /// Key context used for analysis (if available)
    pub key: Option<Key>,
}

/// Configuration for chord detection
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChordConfig {
    /// Track name to read chords from (default: "CHORDS")
    pub track_name: String,
    /// Minimum note duration in PPQ to consider (default: 180)
    pub min_note_duration_ppq: i32,
    /// Minimum chord duration in PPQ to keep (default: 240)
    pub min_chord_duration_ppq: i32,
    /// Key signature for scale degree/roman numeral conversion (from keyflow)
    pub key: Option<Key>,
}

impl Default for ChordConfig {
    fn default() -> Self {
        Self {
            track_name: "CHORDS".to_string(),
            min_note_duration_ppq: 180,
            min_chord_duration_ppq: 240,
            key: None,
        }
    }
}

/// MIDI note event for tracking active notes
///
/// This is DAW-specific timing data. The pitch can be converted to
/// keyflow's MusicalNote when needed.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MidiNoteEvent {
    /// MIDI note number (0-127)
    pub pitch: u8,
    /// Start position in PPQ
    pub start_ppq: i32,
    /// End position in PPQ
    pub end_ppq: i32,
    /// Whether note is selected
    pub selected: bool,
    /// Whether note is muted
    pub muted: bool,
}
