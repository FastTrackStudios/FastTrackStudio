//! Solo mode for tracks

use serde::{Deserialize, Serialize};
use std::fmt;

/// Wrapper for unknown enum values to preserve them during serialization
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Hidden<T>(pub T);

/// Track solo mode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Default)]
pub enum SoloMode {
    /// No solo
    #[default]
    Off,
    /// Solo (default mode)
    Solo,
    /// Solo in place
    SoloInPlace,
    /// Safe solo (non-solo-in-place) - value 5
    SafeSolo,
    /// Safe solo in place - value 6
    SafeSoloInPlace,
    /// Represents a variant unknown to this library
    Unknown(Hidden<i32>),
}

impl SoloMode {
    /// Converts an integer as returned by the low-level API to a solo mode
    pub fn from_raw(v: i32) -> SoloMode {
        use SoloMode::*;
        match v {
            0 => Off,
            1 => Solo,
            2 => SoloInPlace,
            5 => SafeSolo,
            6 => SafeSoloInPlace,
            x => Unknown(Hidden(x)),
        }
    }

    /// Converts this value to an integer as expected by the low-level API
    pub fn to_raw(self) -> i32 {
        use SoloMode::*;
        match self {
            Off => 0,
            Solo => 1,
            SoloInPlace => 2,
            SafeSolo => 5,
            SafeSoloInPlace => 6,
            Unknown(Hidden(x)) => x,
        }
    }
}

impl fmt::Display for SoloMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SoloMode::Off => write!(f, "Off"),
            SoloMode::Solo => write!(f, "Solo"),
            SoloMode::SoloInPlace => write!(f, "Solo In Place"),
            SoloMode::SafeSolo => write!(f, "Safe Solo"),
            SoloMode::SafeSoloInPlace => write!(f, "Safe Solo In Place"),
            SoloMode::Unknown(Hidden(x)) => write!(f, "Unknown({})", x),
        }
    }
}
