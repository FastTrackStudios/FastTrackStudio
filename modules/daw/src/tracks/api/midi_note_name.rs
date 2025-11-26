//! MIDI note name settings

use serde::{Deserialize, Serialize};

/// MIDI note name (MIDINOTENAMES)
/// 
/// Custom MIDI note names applied to Piano roll in all MIDI items on the track.
/// Field 1: MIDI channel number, -1 = Omni
/// Field 2: 0-based note number
/// Field 3: Note name (string)
/// Field 4: Unknown field
/// Field 5: Note number (duplicate?)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MidiNoteName {
    /// MIDI channel number (-1 = Omni)
    pub channel: i32,
    /// 0-based note number
    pub note: i32,
    /// Note name (string)
    pub name: String,
    /// Unknown field
    pub unknown_field_4: i32,
    /// Note number (duplicate of note field?)
    pub note_2: i32,
}

impl MidiNoteName {
    /// Create a new MIDI note name
    pub fn new(channel: i32, note: i32, name: String, unknown_field_4: i32, note_2: i32) -> Self {
        Self {
            channel,
            note,
            name,
            unknown_field_4,
            note_2,
        }
    }
}

