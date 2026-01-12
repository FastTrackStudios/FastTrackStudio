//! Duration representation for music notation.

use serde::{Deserialize, Serialize};

/// Duration kind (note value).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum DurationKind {
    /// Whole note (semibreve)
    Whole,
    /// Half note (minim)
    Half,
    /// Quarter note (crotchet)
    Quarter,
    /// Eighth note (quaver)
    Eighth,
    /// Sixteenth note (semiquaver)
    Sixteenth,
    /// Thirty-second note (demisemiquaver)
    ThirtySecond,
    /// Sixty-fourth note (hemidemisemiquaver)
    SixtyFourth,
}

impl DurationKind {
    /// Get the duration in quarter notes (1.0 = quarter note).
    #[must_use]
    pub const fn quarters(self) -> f64 {
        match self {
            Self::Whole => 4.0,
            Self::Half => 2.0,
            Self::Quarter => 1.0,
            Self::Eighth => 0.5,
            Self::Sixteenth => 0.25,
            Self::ThirtySecond => 0.125,
            Self::SixtyFourth => 0.0625,
        }
    }

    /// Get the number of flags/beams for this duration.
    #[must_use]
    pub const fn flags(self) -> u8 {
        match self {
            Self::Whole | Self::Half | Self::Quarter => 0,
            Self::Eighth => 1,
            Self::Sixteenth => 2,
            Self::ThirtySecond => 3,
            Self::SixtyFourth => 4,
        }
    }
}

/// Complete duration with dots.
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Duration {
    /// The base duration kind
    pub kind: DurationKind,
    /// Number of dots (0-2)
    pub dots: u8,
}

impl Duration {
    /// Create a new duration.
    #[must_use]
    pub const fn new(kind: DurationKind) -> Self {
        Self { kind, dots: 0 }
    }

    /// Create a dotted duration.
    #[must_use]
    pub const fn dotted(kind: DurationKind) -> Self {
        Self { kind, dots: 1 }
    }

    /// Create a double-dotted duration.
    #[must_use]
    pub const fn double_dotted(kind: DurationKind) -> Self {
        Self { kind, dots: 2 }
    }

    /// Get the total duration in quarter notes.
    #[must_use]
    pub fn quarters(&self) -> f64 {
        let base = self.kind.quarters();
        match self.dots {
            0 => base,
            1 => base * 1.5,
            2 => base * 1.75,
            _ => base, // More than 2 dots is rare
        }
    }

    // Common durations
    pub const WHOLE: Self = Self::new(DurationKind::Whole);
    pub const HALF: Self = Self::new(DurationKind::Half);
    pub const QUARTER: Self = Self::new(DurationKind::Quarter);
    pub const EIGHTH: Self = Self::new(DurationKind::Eighth);
    pub const SIXTEENTH: Self = Self::new(DurationKind::Sixteenth);
}

impl Default for Duration {
    fn default() -> Self {
        Self::QUARTER
    }
}
