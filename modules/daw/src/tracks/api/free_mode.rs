//! Free item positioning mode

use serde::{Deserialize, Serialize};
use std::fmt;

/// Wrapper for unknown enum values to preserve them during serialization
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Hidden<T>(pub T);

/// Free item positioning mode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum FreeMode {
    Disabled,
    FreeItemPositioning,
    FixedItemLanes,
    /// Represents a variant unknown to this library
    Unknown(Hidden<i32>),
}

impl FreeMode {
    /// Converts an integer as returned by the low-level API to a free mode
    pub fn from_raw(value: i32) -> Self {
        match value {
            0 => FreeMode::Disabled,
            1 => FreeMode::FreeItemPositioning,
            2 => FreeMode::FixedItemLanes,
            x => FreeMode::Unknown(Hidden(x)),
        }
    }

    /// Converts this value to an integer as expected by the low-level API
    pub fn to_raw(self) -> i32 {
        match self {
            FreeMode::Disabled => 0,
            FreeMode::FreeItemPositioning => 1,
            FreeMode::FixedItemLanes => 2,
            FreeMode::Unknown(Hidden(x)) => x,
        }
    }
}

impl From<i32> for FreeMode {
    fn from(value: i32) -> Self {
        Self::from_raw(value)
    }
}

impl fmt::Display for FreeMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FreeMode::Disabled => write!(f, "Disabled"),
            FreeMode::FreeItemPositioning => write!(f, "Free Item Positioning"),
            FreeMode::FixedItemLanes => write!(f, "Fixed Item Lanes"),
            FreeMode::Unknown(Hidden(val)) => write!(f, "Unknown({})", val),
        }
    }
}

