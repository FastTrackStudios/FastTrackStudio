//! Input quantize settings

use serde::{Deserialize, Serialize};
use std::fmt;

/// Quantize to position
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum QuantizeToPos {
    /// Previous position
    Previous = -1,
    /// Nearest position
    Nearest = 0,
    /// Next position
    Next = 1,
}

impl QuantizeToPos {
    /// Converts an integer to a quantize position
    pub fn from_raw(value: i32) -> Self {
        match value {
            -1 => QuantizeToPos::Previous,
            0 => QuantizeToPos::Nearest,
            1 => QuantizeToPos::Next,
            _ => QuantizeToPos::Nearest, // Default to nearest for unknown values
        }
    }

    /// Converts this value to an integer
    pub fn to_raw(self) -> i32 {
        self as i32
    }
}

impl fmt::Display for QuantizeToPos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            QuantizeToPos::Previous => write!(f, "Previous"),
            QuantizeToPos::Nearest => write!(f, "Nearest"),
            QuantizeToPos::Next => write!(f, "Next"),
        }
    }
}

impl Default for QuantizeToPos {
    fn default() -> Self {
        QuantizeToPos::Nearest
    }
}

/// Record path
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RecordPath {
    /// Primary path
    Primary = 0,
    /// Secondary path
    Secondary = 1,
    /// Both paths
    Both = 2,
}

impl RecordPath {
    /// Converts an integer to a record path
    pub fn from_raw(value: i32) -> Self {
        match value {
            0 => RecordPath::Primary,
            1 => RecordPath::Secondary,
            2 => RecordPath::Both,
            _ => RecordPath::Primary, // Default to primary for unknown values
        }
    }

    /// Converts this value to an integer
    pub fn to_raw(self) -> i32 {
        self as i32
    }
}

impl fmt::Display for RecordPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RecordPath::Primary => write!(f, "Primary"),
            RecordPath::Secondary => write!(f, "Secondary"),
            RecordPath::Both => write!(f, "Both"),
        }
    }
}

impl Default for RecordPath {
    fn default() -> Self {
        RecordPath::Primary
    }
}

/// Input quantize settings for tracks
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct InputQuantize {
    /// Quantize MIDI (field 1)
    pub quantize_midi: bool,
    /// Quantize to position (field 2) - -1=prev, 0=nearest, 1=next
    pub quantize_to_pos: QuantizeToPos,
    /// Quantize note-offs (field 3)
    pub quantize_note_offs: bool,
    /// Quantize to (field 4) - fraction of beat
    pub quantize_to: f64,
    /// Quantize strength (field 5) - percentage
    pub quantize_strength: i32,
    /// Swing strength (field 6) - percentage
    pub swing_strength: i32,
    /// Quantize range min (field 7) - percentage
    pub quantize_range_min: i32,
    /// Quantize range max (field 8) - percentage
    pub quantize_range_max: i32,
}

impl Default for InputQuantize {
    fn default() -> Self {
        Self {
            quantize_midi: false,
            quantize_to_pos: QuantizeToPos::default(),
            quantize_note_offs: false,
            quantize_to: 0.25, // Default to quarter note
            quantize_strength: 100,
            swing_strength: 0,
            quantize_range_min: 0,
            quantize_range_max: 100,
        }
    }
}

