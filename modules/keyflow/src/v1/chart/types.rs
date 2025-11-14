//! Core types for chart parsing
//!
//! This module defines the fundamental types used throughout chart parsing,
//! including chord instances, measures, sections, and key changes.

use crate::chord::chord::ChordData;
use crate::key::keys::Key;
use crate::rhythm::chord_rhythm::{ChordRhythm, TimingModifier};
use crate::sections::Section;
use crate::time::{MusicalDuration, MusicalPosition};

/// Represents a root note with both its converted note name and original format
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RootNote {
    pub note_name: String,       // Converted note name for parsing/memory (e.g., "G", "C", "E")
    pub original_format: String, // Original format as written (e.g., "1", "I", "g", "G")
}

impl RootNote {
    pub fn new(note_name: String, original_format: String) -> Self {
        Self {
            note_name,
            original_format,
        }
    }

    /// Get the display format (original format if available, otherwise note name)
    pub fn display(&self) -> &str {
        if !self.original_format.is_empty() {
            &self.original_format
        } else {
            &self.note_name
        }
    }
}

/// Represents a chord instance with its remembered quality
#[derive(Debug, Clone)]
pub struct ChordInstance {
    pub root: RootNote,                          // Root note with both converted name and original format
    pub full_symbol: String,                     // E.g., "Gmaj7", "Cadd9", "Em7"
    pub parsed: ChordData,                       // Full parsed representation
    pub rhythm: ChordRhythm,                     // Rhythm notation (duration, slashes, etc.)
    pub timing_modifier: Option<TimingModifier>, // Push/pull timing
    pub original_token: String,                  // Original token before processing (e.g., "1", "I", "G")
    pub duration: MusicalDuration,               // Duration in measure.beats.subdivision format
    pub position: MusicalPosition,               // Position in the song
}

impl crate::rhythm::Rhythm for ChordInstance {
    fn duration(&self) -> &MusicalDuration {
        &self.duration
    }

    fn position(&self) -> &MusicalPosition {
        &self.position
    }

    fn set_position(&mut self, position: MusicalPosition) {
        self.position = position;
    }
}

/// Represents a single measure with multiple content types
#[derive(Debug, Clone)]
pub struct Measure {
    pub chords: Vec<ChordInstance>,
    pub melody: Option<String>, // Simplified melody representation for now
    pub text_cues: Vec<String>, // Simplified text cues for now
    pub key: Option<Key>,       // Key change within this measure
}

impl Measure {
    pub fn new() -> Self {
        Self {
            chords: Vec::new(),
            melody: None,
            text_cues: Vec::new(),
            key: None,
        }
    }

    pub fn with_chords(chords: Vec<ChordInstance>) -> Self {
        Self {
            chords,
            melody: None,
            text_cues: Vec::new(),
            key: None,
        }
    }
}

/// Represents a section with its content
#[derive(Debug, Clone)]
pub struct ChartSection {
    pub section: Section,
    pub measures: Vec<Measure>,
    pub from_template: bool, // true if measures came from template, false if explicitly defined
}

/// Represents a position in the chart using duration-based coordinates
/// 
/// NOTE: This may be deprecated in favor of MusicalPosition
#[derive(Debug, Clone, PartialEq)]
pub struct Position {
    pub section_index: usize,
    pub measure_index: usize,
    pub beat_position: MusicalDuration, // Position within the measure (e.g., 0.0, 1.0, 2.5)
}

/// Represents a key change event
#[derive(Debug, Clone)]
pub struct KeyChange {
    pub position: MusicalPosition,
    pub from_key: Option<Key>,
    pub to_key: Key,
}

