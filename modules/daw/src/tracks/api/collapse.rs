//! Collapse/folder state types

use serde::{Deserialize, Serialize};
use std::fmt;

/// Wrapper for unknown enum values to preserve them during serialization
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Hidden<T>(pub T);

/// Collapse state in Arrange view
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ArrangeCollapseState {
    /// Not collapsed
    NotCollapsed,
    /// Collapsed medium
    CollapsedMedium,
    /// Collapsed small
    CollapsedSmall,
    /// Represents a variant unknown to this library
    Unknown(Hidden<i32>),
}

impl ArrangeCollapseState {
    /// Converts an integer to a collapse state
    pub fn from_raw(v: i32) -> Self {
        use ArrangeCollapseState::*;
        match v {
            0 => NotCollapsed,
            1 => CollapsedMedium,
            2 => CollapsedSmall,
            x => Unknown(Hidden(x)),
        }
    }

    /// Converts this value to an integer
    pub fn to_raw(self) -> i32 {
        use ArrangeCollapseState::*;
        match self {
            NotCollapsed => 0,
            CollapsedMedium => 1,
            CollapsedSmall => 2,
            Unknown(Hidden(x)) => x,
        }
    }
}

impl fmt::Display for ArrangeCollapseState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ArrangeCollapseState::NotCollapsed => write!(f, "Not Collapsed"),
            ArrangeCollapseState::CollapsedMedium => write!(f, "Collapsed Medium"),
            ArrangeCollapseState::CollapsedSmall => write!(f, "Collapsed Small"),
            ArrangeCollapseState::Unknown(Hidden(x)) => write!(f, "Unknown({})", x),
        }
    }
}

/// Collapse state in Mixer
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum MixerCollapseState {
    /// Not collapsed
    NotCollapsed,
    /// Collapsed
    Collapsed,
    /// Represents a variant unknown to this library
    Unknown(Hidden<i32>),
}

impl MixerCollapseState {
    /// Converts an integer to a collapse state
    pub fn from_raw(v: i32) -> Self {
        use MixerCollapseState::*;
        match v {
            0 => NotCollapsed,
            1 => Collapsed,
            x => Unknown(Hidden(x)),
        }
    }

    /// Converts this value to an integer
    pub fn to_raw(self) -> i32 {
        use MixerCollapseState::*;
        match self {
            NotCollapsed => 0,
            Collapsed => 1,
            Unknown(Hidden(x)) => x,
        }
    }
}

impl fmt::Display for MixerCollapseState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MixerCollapseState::NotCollapsed => write!(f, "Not Collapsed"),
            MixerCollapseState::Collapsed => write!(f, "Collapsed"),
            MixerCollapseState::Unknown(Hidden(x)) => write!(f, "Unknown({})", x),
        }
    }
}

/// Collapse state in track wiring
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum WiringCollapseState {
    /// Not collapsed
    NotCollapsed,
    /// Collapsed
    Collapsed,
    /// Represents a variant unknown to this library
    Unknown(Hidden<i32>),
}

impl WiringCollapseState {
    /// Converts an integer to a collapse state
    pub fn from_raw(v: i32) -> Self {
        use WiringCollapseState::*;
        match v {
            0 => NotCollapsed,
            1 => Collapsed,
            x => Unknown(Hidden(x)),
        }
    }

    /// Converts this value to an integer
    pub fn to_raw(self) -> i32 {
        use WiringCollapseState::*;
        match self {
            NotCollapsed => 0,
            Collapsed => 1,
            Unknown(Hidden(x)) => x,
        }
    }
}

impl fmt::Display for WiringCollapseState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WiringCollapseState::NotCollapsed => write!(f, "Not Collapsed"),
            WiringCollapseState::Collapsed => write!(f, "Collapsed"),
            WiringCollapseState::Unknown(Hidden(x)) => write!(f, "Unknown({})", x),
        }
    }
}

/// Bus compact settings (BUSCOMP) - collapse folder settings
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BusCompactSettings {
    /// Collapse state in Arrange view (field 1)
    pub arrange: ArrangeCollapseState,
    /// Collapse state in Mixer (field 2)
    pub mixer: MixerCollapseState,
    /// Collapse state in track wiring (field 3)
    pub wiring: WiringCollapseState,
    /// Track wiring routing window x position (field 4)
    pub wiring_window_x: i32,
    /// Track wiring routing window y position (field 5)
    pub wiring_window_y: i32,
}

impl Default for BusCompactSettings {
    fn default() -> Self {
        Self {
            arrange: ArrangeCollapseState::NotCollapsed,
            mixer: MixerCollapseState::NotCollapsed,
            wiring: WiringCollapseState::NotCollapsed,
            wiring_window_x: 0,
            wiring_window_y: 0,
        }
    }
}
