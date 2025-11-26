//! Hardware output and MIDI settings

use serde::{Deserialize, Serialize};
use crate::tracks::api::automation::AutomationMode;
use crate::tracks::api::send_mode::SendMode;

/// MIDI hardware output settings
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MidiOutputSettings {
    pub device: i32,               // device index (floor(val / 32))
    pub channel: i32,              // channel number (val & 0x1F)
}

/// Master/parent send settings
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MasterSendSettings {
    pub enabled: bool,             // field 1 - enabled
    pub unknown_field_2: i32,      // field 2 - unknown
}

/// Hardware output settings
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct HardwareOutputSettings {
    pub output_index: i32,         // field 1 - output index
    pub send_mode: SendMode,       // field 2 - send mode
    pub volume: f64,               // field 3 - volume
    pub pan: f64,                  // field 4 - pan
    pub mute: bool,                // field 5 - mute
    pub invert_polarity: bool,     // field 6 - invert polarity
    pub send_source_channel: i32,  // field 7 - send source channel
    pub unknown_field_8: i32,      // field 8 - unknown
    pub automation_mode: Option<AutomationMode>, // field 9 - automation mode (None = use track mode, -1)
}

