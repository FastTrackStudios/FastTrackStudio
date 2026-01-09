//! Automation mode for tracks

use serde::{Deserialize, Serialize};
use std::fmt;

/// Wrapper for unknown enum values to preserve them during serialization
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Hidden<T>(pub T);

/// Automation mode for tracks
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AutomationMode {
    TrimRead,
    Read,
    Touch,
    Write,
    Latch,
    LatchPreview,
    /// Represents a variant unknown to this library
    Unknown(Hidden<i32>),
}

impl AutomationMode {
    /// Converts an integer as returned by the low-level API to an automation mode
    pub fn from_raw(value: i32) -> Self {
        match value {
            0 => AutomationMode::TrimRead,
            1 => AutomationMode::Read,
            2 => AutomationMode::Touch,
            3 => AutomationMode::Write,
            4 => AutomationMode::Latch,
            5 => AutomationMode::LatchPreview,
            x => AutomationMode::Unknown(Hidden(x)),
        }
    }

    /// Converts this value to an integer as expected by the low-level API
    pub fn to_raw(self) -> i32 {
        match self {
            AutomationMode::TrimRead => 0,
            AutomationMode::Read => 1,
            AutomationMode::Touch => 2,
            AutomationMode::Write => 3,
            AutomationMode::Latch => 4,
            AutomationMode::LatchPreview => 5,
            AutomationMode::Unknown(Hidden(x)) => x,
        }
    }
}

impl From<i32> for AutomationMode {
    fn from(value: i32) -> Self {
        Self::from_raw(value)
    }
}

impl fmt::Display for AutomationMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AutomationMode::TrimRead => write!(f, "Trim/Read"),
            AutomationMode::Read => write!(f, "Read"),
            AutomationMode::Touch => write!(f, "Touch"),
            AutomationMode::Write => write!(f, "Write"),
            AutomationMode::Latch => write!(f, "Latch"),
            AutomationMode::LatchPreview => write!(f, "Latch Preview"),
            AutomationMode::Unknown(Hidden(val)) => write!(f, "Unknown({})", val),
        }
    }
}
