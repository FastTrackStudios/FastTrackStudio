//! High-level notation API for automatic music layout.
//!
//! This module provides a simple, declarative API for creating music notation.
//! You specify the musical content (clef, time signature, rhythms) and the
//! system automatically handles:
//! - Segment creation with proper tick values
//! - Spring-based horizontal spacing
//! - Automatic beaming based on time signature
//! - Collision detection and minimum distances
//!
//! # Example
//!
//! ```ignore
//! use engraver::notation::{MeasureBuilder, NotationMode, Duration};
//!
//! let scene = MeasureBuilder::new()
//!     .clef(ClefType::Treble)
//!     .time_signature(4, 4)
//!     .mode(NotationMode::Rhythmic)
//!     .rhythm(vec![
//!         Duration::Quarter,
//!         Duration::Quarter,
//!         Duration::Eighth,
//!         Duration::Eighth,
//!         Duration::Quarter,
//!     ])
//!     .build(&ctx);
//! ```

mod builder;
mod mode;

pub use builder::{MeasureBuilder, MeasureScene, SystemBuilder};
pub use mode::NotationMode;

/// Duration values in ticks (480 ticks = quarter note, standard MIDI resolution).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Duration {
    /// Whole note (4 beats in 4/4)
    Whole,
    /// Half note (2 beats)
    Half,
    /// Dotted half note (3 beats)
    DottedHalf,
    /// Quarter note (1 beat)
    Quarter,
    /// Dotted quarter (1.5 beats)
    DottedQuarter,
    /// Eighth note (0.5 beats)
    Eighth,
    /// Dotted eighth (0.75 beats)
    DottedEighth,
    /// Sixteenth note (0.25 beats)
    Sixteenth,
    /// Dotted sixteenth
    DottedSixteenth,
    /// Thirty-second note
    ThirtySecond,
}

impl Duration {
    /// Get the duration in ticks (480 = quarter note).
    #[must_use]
    pub const fn ticks(self) -> i32 {
        match self {
            Self::Whole => 1920,
            Self::Half => 960,
            Self::DottedHalf => 1440,
            Self::Quarter => 480,
            Self::DottedQuarter => 720,
            Self::Eighth => 240,
            Self::DottedEighth => 360,
            Self::Sixteenth => 120,
            Self::DottedSixteenth => 180,
            Self::ThirtySecond => 60,
        }
    }

    /// Check if this duration is dotted.
    #[must_use]
    pub const fn is_dotted(self) -> bool {
        matches!(
            self,
            Self::DottedHalf
                | Self::DottedQuarter
                | Self::DottedEighth
                | Self::DottedSixteenth
        )
    }

    /// Get the number of dots.
    #[must_use]
    pub const fn dots(self) -> u8 {
        if self.is_dotted() { 1 } else { 0 }
    }

    /// Convert to NoteDuration enum.
    #[must_use]
    pub const fn to_note_duration(self) -> crate::layout::tlayout::NoteDuration {
        use crate::layout::tlayout::NoteDuration;
        match self {
            Self::Whole => NoteDuration::Whole,
            Self::Half | Self::DottedHalf => NoteDuration::Half,
            Self::Quarter | Self::DottedQuarter => NoteDuration::Quarter,
            Self::Eighth | Self::DottedEighth => NoteDuration::Eighth,
            Self::Sixteenth | Self::DottedSixteenth => NoteDuration::Sixteenth,
            Self::ThirtySecond => NoteDuration::ThirtySecond,
        }
    }

    /// Check if this duration needs a flag (when not beamed).
    #[must_use]
    pub const fn needs_flag(self) -> bool {
        matches!(
            self,
            Self::Eighth
                | Self::DottedEighth
                | Self::Sixteenth
                | Self::DottedSixteenth
                | Self::ThirtySecond
        )
    }

    /// Get the number of beams needed for this duration.
    #[must_use]
    pub const fn beam_count(self) -> u8 {
        match self {
            Self::Whole | Self::Half | Self::DottedHalf | Self::Quarter | Self::DottedQuarter => 0,
            Self::Eighth | Self::DottedEighth => 1,
            Self::Sixteenth | Self::DottedSixteenth => 2,
            Self::ThirtySecond => 3,
        }
    }
}

/// Time signature representation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TimeSignature {
    pub numerator: u8,
    pub denominator: u8,
}

impl TimeSignature {
    /// Create a new time signature.
    #[must_use]
    pub const fn new(numerator: u8, denominator: u8) -> Self {
        Self { numerator, denominator }
    }

    /// Common time (4/4).
    pub const COMMON: Self = Self::new(4, 4);
    /// Cut time (2/2).
    pub const CUT: Self = Self::new(2, 2);
    /// Waltz time (3/4).
    pub const WALTZ: Self = Self::new(3, 4);

    /// Get the number of ticks in one measure.
    #[must_use]
    pub const fn measure_ticks(&self) -> i32 {
        let beat_ticks = 1920 / self.denominator as i32; // 1920 = whole note
        beat_ticks * self.numerator as i32
    }

    /// Get the number of ticks per beat.
    #[must_use]
    pub const fn beat_ticks(&self) -> i32 {
        1920 / self.denominator as i32
    }

    /// Get beam groupings for this time signature.
    /// Returns a list of tick counts for each beam group.
    #[must_use]
    pub fn beam_groups(&self) -> Vec<i32> {
        let beat = self.beat_ticks();
        match (self.numerator, self.denominator) {
            // 4/4: beam in groups of 2 beats (or 1 beat for eighths)
            (4, 4) => vec![beat, beat, beat, beat],
            // 3/4: beam each beat separately
            (3, 4) => vec![beat, beat, beat],
            // 6/8: beam in groups of 3 eighths
            (6, 8) => vec![beat * 3, beat * 3],
            // 2/4: beam in groups of 1 beat
            (2, 4) => vec![beat, beat],
            // Default: beam each beat
            _ => vec![beat; self.numerator as usize],
        }
    }
}

impl Default for TimeSignature {
    fn default() -> Self {
        Self::COMMON
    }
}
