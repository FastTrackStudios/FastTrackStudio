//! Record settings for tracks

use crate::tracks::api::quantize::RecordPath;
use serde::{Deserialize, Serialize};
use std::fmt;

/// Wrapper for unknown enum values to preserve them during serialization
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Hidden<T>(pub T);

/// Record mode for tracks
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RecordMode {
    Input,
    OutputStereo,
    DisableMonitor,
    OutputStereoLatencyComp,
    OutputMidi,
    OutputMono,
    OutputMonoLatencyComp,
    MidiOverdub,
    MidiReplace,
    MidiTouchReplace,
    OutputMultichannel,
    OutputMultichannelLatencyComp,
    /// Represents a variant unknown to this library
    Unknown(Hidden<i32>),
}

impl RecordMode {
    /// Converts an integer as returned by the low-level API to a recording mode
    pub fn from_raw(value: i32) -> Self {
        match value {
            0 => RecordMode::Input,
            1 => RecordMode::OutputStereo,
            2 => RecordMode::DisableMonitor,
            3 => RecordMode::OutputStereoLatencyComp,
            4 => RecordMode::OutputMidi,
            5 => RecordMode::OutputMono,
            6 => RecordMode::OutputMonoLatencyComp,
            7 => RecordMode::MidiOverdub,
            8 => RecordMode::MidiReplace,
            9 => RecordMode::MidiTouchReplace,
            10 => RecordMode::OutputMultichannel,
            11 => RecordMode::OutputMultichannelLatencyComp,
            x => RecordMode::Unknown(Hidden(x)),
        }
    }

    /// Converts this value to an integer as expected by the low-level API
    pub fn to_raw(self) -> i32 {
        match self {
            RecordMode::Input => 0,
            RecordMode::OutputStereo => 1,
            RecordMode::DisableMonitor => 2,
            RecordMode::OutputStereoLatencyComp => 3,
            RecordMode::OutputMidi => 4,
            RecordMode::OutputMono => 5,
            RecordMode::OutputMonoLatencyComp => 6,
            RecordMode::MidiOverdub => 7,
            RecordMode::MidiReplace => 8,
            RecordMode::MidiTouchReplace => 9,
            RecordMode::OutputMultichannel => 10,
            RecordMode::OutputMultichannelLatencyComp => 11,
            RecordMode::Unknown(Hidden(x)) => x,
        }
    }
}

impl From<i32> for RecordMode {
    fn from(value: i32) -> Self {
        Self::from_raw(value)
    }
}

impl fmt::Display for RecordMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RecordMode::Input => write!(f, "Input"),
            RecordMode::OutputStereo => write!(f, "Output (Stereo)"),
            RecordMode::DisableMonitor => write!(f, "Disable (Monitor)"),
            RecordMode::OutputStereoLatencyComp => write!(f, "Output (Stereo, Latency Comp)"),
            RecordMode::OutputMidi => write!(f, "Output (MIDI)"),
            RecordMode::OutputMono => write!(f, "Output (Mono)"),
            RecordMode::OutputMonoLatencyComp => write!(f, "Output (Mono, Latency Comp)"),
            RecordMode::MidiOverdub => write!(f, "MIDI Overdub"),
            RecordMode::MidiReplace => write!(f, "MIDI Replace"),
            RecordMode::MidiTouchReplace => write!(f, "MIDI Touch Replace"),
            RecordMode::OutputMultichannel => write!(f, "Output (Multichannel)"),
            RecordMode::OutputMultichannelLatencyComp => {
                write!(f, "Output (Multichannel, Latency Comp)")
            }
            RecordMode::Unknown(Hidden(val)) => write!(f, "Unknown({})", val),
        }
    }
}

/// Monitor mode for tracks
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum MonitorMode {
    Off,
    On,
    Auto,
    /// Represents a variant unknown to this library
    Unknown(Hidden<i32>),
}

impl MonitorMode {
    /// Converts an integer as returned by the low-level API to a monitor mode
    pub fn from_raw(value: i32) -> Self {
        match value {
            0 => MonitorMode::Off,
            1 => MonitorMode::On,
            2 => MonitorMode::Auto,
            x => MonitorMode::Unknown(Hidden(x)),
        }
    }

    /// Converts this value to an integer as expected by the low-level API
    pub fn to_raw(self) -> i32 {
        match self {
            MonitorMode::Off => 0,
            MonitorMode::On => 1,
            MonitorMode::Auto => 2,
            MonitorMode::Unknown(Hidden(x)) => x,
        }
    }
}

impl From<i32> for MonitorMode {
    fn from(value: i32) -> Self {
        Self::from_raw(value)
    }
}

impl fmt::Display for MonitorMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MonitorMode::Off => write!(f, "Off"),
            MonitorMode::On => write!(f, "On"),
            MonitorMode::Auto => write!(f, "Auto"),
            MonitorMode::Unknown(Hidden(val)) => write!(f, "Unknown({})", val),
        }
    }
}

/// Record settings for tracks
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RecordSettings {
    pub armed: bool,                // field 1 - armed
    pub input: i32,                 // field 2 - input (device + channel coded)
    pub monitor: MonitorMode,       // field 3 - monitor mode
    pub record_mode: RecordMode,    // field 4 - record mode
    pub monitor_track_media: bool,  // field 5 - monitor track media while recording
    pub preserve_pdc_delayed: bool, // field 6 - preserve PDC delayed monitoring
    pub record_path: RecordPath,    // field 7 - record path
}
