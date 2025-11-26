//! Send mode for hardware outputs

use serde::{Deserialize, Serialize};
use std::fmt;

/// Wrapper for unknown enum values to preserve them during serialization
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Hidden<T>(pub T);

/// Send mode for hardware outputs
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SendMode {
    /// Post-Fader (Post-Pan)
    PostFader,
    /// Pre-Fader (Pre-FX)
    PreFader,
    /// Pre-Fader (Post-FX)
    PreFaderPostFx,
    /// Represents a variant unknown to this library
    Unknown(Hidden<i32>),
}

impl SendMode {
    /// Converts an integer as returned by the low-level API to a send mode
    pub fn from_raw(value: i32) -> Self {
        match value {
            0 => SendMode::PostFader,
            1 => SendMode::PreFader,
            3 => SendMode::PreFaderPostFx,
            x => SendMode::Unknown(Hidden(x)),
        }
    }

    /// Converts this value to an integer as expected by the low-level API
    pub fn to_raw(self) -> i32 {
        match self {
            SendMode::PostFader => 0,
            SendMode::PreFader => 1,
            SendMode::PreFaderPostFx => 3,
            SendMode::Unknown(Hidden(x)) => x,
        }
    }
}

impl fmt::Display for SendMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SendMode::PostFader => write!(f, "Post-Fader (Post-Pan)"),
            SendMode::PreFader => write!(f, "Pre-Fader (Pre-FX)"),
            SendMode::PreFaderPostFx => write!(f, "Pre-Fader (Post-FX)"),
            SendMode::Unknown(Hidden(val)) => write!(f, "Unknown({})", val),
        }
    }
}

