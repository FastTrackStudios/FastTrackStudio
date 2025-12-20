//! Track timebase settings

use serde::{Deserialize, Serialize};
use std::fmt;

/// Track timebase (BEAT) - how the track handles time
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[derive(Default)]
pub enum TrackTimebase {
    /// Use project default timebase (-1)
    #[default]
    ProjectDefault,
    /// Time-based (0)
    Time,
    /// Beats-based (1)
    Beats,
    /// Represents a variant unknown to this library
    Unknown(i32),
}

impl TrackTimebase {
    /// Converts an integer to a track timebase
    pub fn from_raw(value: i32) -> Self {
        match value {
            -1 => TrackTimebase::ProjectDefault,
            0 => TrackTimebase::Time,
            1 => TrackTimebase::Beats,
            _ => TrackTimebase::Unknown(value),
        }
    }

    /// Converts this value to an integer
    pub fn to_raw(self) -> i32 {
        match self {
            TrackTimebase::ProjectDefault => -1,
            TrackTimebase::Time => 0,
            TrackTimebase::Beats => 1,
            TrackTimebase::Unknown(val) => val,
        }
    }
}

impl From<i32> for TrackTimebase {
    fn from(value: i32) -> Self {
        Self::from_raw(value)
    }
}

impl fmt::Display for TrackTimebase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TrackTimebase::ProjectDefault => write!(f, "Project Default"),
            TrackTimebase::Time => write!(f, "Time"),
            TrackTimebase::Beats => write!(f, "Beats"),
            TrackTimebase::Unknown(val) => write!(f, "Unknown({})", val),
        }
    }
}


