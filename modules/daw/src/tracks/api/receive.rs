//! Track receive settings

use crate::tracks::api::automation::AutomationMode;
use crate::tracks::envelope::Envelope;
use serde::{Deserialize, Serialize};

/// Wrapper for unknown enum values to preserve them during serialization
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Hidden<T>(pub T);

/// Receive mode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ReceiveMode {
    /// Post Fader (Post Pan)
    PostFader,
    /// Pre FX
    PreFx,
    /// Pre Fader (Post FX)
    PreFader,
    /// Represents a variant unknown to this library
    Unknown(Hidden<i32>),
}

impl ReceiveMode {
    /// Converts an integer as returned by the low-level API to a receive mode
    pub fn from_raw(value: i32) -> Self {
        match value {
            0 => ReceiveMode::PostFader,
            1 => ReceiveMode::PreFx,
            3 => ReceiveMode::PreFader,
            x => ReceiveMode::Unknown(Hidden(x)),
        }
    }

    /// Converts this value to an integer as expected by the low-level API
    pub fn to_raw(self) -> i32 {
        match self {
            ReceiveMode::PostFader => 0,
            ReceiveMode::PreFx => 1,
            ReceiveMode::PreFader => 3,
            ReceiveMode::Unknown(Hidden(x)) => x,
        }
    }
}

impl From<i32> for ReceiveMode {
    fn from(value: i32) -> Self {
        Self::from_raw(value)
    }
}

/// Track receive settings
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TrackReceive {
    pub source_track_index: i32, // field 1 - source track index (zero based)
    pub mode: ReceiveMode,       // field 2 - mode
    pub volume: f64,             // field 3 - volume
    pub pan: f64,                // field 4 - pan
    pub mute: bool,              // field 5 - mute
    pub mono_sum: bool,          // field 6 - mono sum
    pub invert_polarity: bool,   // field 7 - invert polarity
    pub source_audio_channels: i32, // field 8 - source audio channels
    pub dest_audio_channels: i32, // field 9 - dest audio channels
    pub pan_law: f64,            // field 10 - panlaw
    pub midi_channels: i32,      // field 11 - midi channels
    pub automation_mode: Option<AutomationMode>, // field 12 - automation mode (None = use track mode, -1)
    /// Envelopes for this receive (pan, mute, etc.)
    pub envelopes: Vec<Envelope>,
}
