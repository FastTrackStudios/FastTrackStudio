//! Time System - Musical durations and positions
//!
//! This module re-exports DAW primitives for time-related types.
//! All time primitives come from the DAW module to avoid duplication.

// Re-export DAW primitives
pub use daw::primitives::{
    Duration, MusicalDuration, MusicalPosition, PPQDuration, PPQPosition, Position, TimeDuration,
    TimePosition, TimeSignature,
};
pub use daw::transport::Tempo;

use serde::{Deserialize, Serialize};
use std::fmt;

/// Chart-specific position that includes section index
/// This wraps DAW's Position with section tracking for chart parsing
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AbsolutePosition {
    /// The total duration from the start of the song (using DAW's MusicalPosition)
    pub total_duration: MusicalPosition,
    /// Which section this position is in
    pub section_index: usize,
}

impl AbsolutePosition {
    pub fn new(total_duration: MusicalPosition, section_index: usize) -> Self {
        Self {
            total_duration,
            section_index,
        }
    }

    /// Create a position at the start of a section
    pub fn at_section_start(total_duration: MusicalPosition, section_index: usize) -> Self {
        Self {
            total_duration,
            section_index,
        }
    }

    /// Create a position at the very beginning of the song
    pub fn at_beginning() -> Self {
        Self {
            total_duration: MusicalPosition::start(),
            section_index: 0,
        }
    }

    /// Get measures as u32 (for backward compatibility)
    pub fn measures(&self) -> u32 {
        self.total_duration.measure.max(0) as u32
    }

    /// Get beats as u32 (for backward compatibility)
    pub fn beats(&self) -> u32 {
        self.total_duration.beat.max(0) as u32
    }

    /// Get subdivisions as u32 (for backward compatibility)
    pub fn subdivisions(&self) -> u32 {
        self.total_duration.subdivision.clamp(0, 999) as u32
    }
}

impl fmt::Display for AbsolutePosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Section {} @ {}",
            self.section_index, self.total_duration
        )
    }
}

// Legacy Duration trait - kept for backward compatibility
// This will be removed once all code is migrated

/// Trait for musical durations (legacy - use MusicalDuration directly)
pub trait DurationTrait: fmt::Debug + fmt::Display {
    /// Get the number of whole measures
    fn measures(&self) -> u32;

    /// Get the number of beats within the current measure
    fn beats(&self) -> u32;

    /// Get the subdivisions (e.g., 50 = half beat, 25 = quarter beat)
    fn subdivisions(&self) -> u32;

    /// Convert to total beats as f64
    fn to_beats(&self, time_sig: TimeSignature) -> f64;

    /// Add two durations together, respecting time signature
    fn add(&self, other: &dyn DurationTrait, time_sig: TimeSignature) -> Box<dyn DurationTrait>;

    /// Clone as a boxed trait object
    fn clone_box(&self) -> Box<dyn DurationTrait>;
}

// Implement DurationTrait for MusicalDuration for backward compatibility
impl DurationTrait for MusicalDuration {
    fn measures(&self) -> u32 {
        self.measure.max(0) as u32
    }

    fn beats(&self) -> u32 {
        self.beat.max(0) as u32
    }

    fn subdivisions(&self) -> u32 {
        self.subdivision.clamp(0, 999) as u32
    }

    fn to_beats(&self, time_sig: TimeSignature) -> f64 {
        let beats_per_measure = time_sig.numerator as f64;
        self.measure.max(0) as f64 * beats_per_measure
            + self.beat.max(0) as f64
            + self.subdivision.clamp(0, 999) as f64 / 1000.0
    }

    fn add(&self, other: &dyn DurationTrait, time_sig: TimeSignature) -> Box<dyn DurationTrait> {
        let total_beats = self.to_beats(time_sig) + other.to_beats(time_sig);
        let beats_per_measure = time_sig.numerator as f64;
        let measures = (total_beats / beats_per_measure).floor() as i32;
        let remaining_beats = total_beats - (measures as f64 * beats_per_measure);
        let beats = remaining_beats.floor() as i32;
        let subdivisions = ((remaining_beats - beats as f64) * 1000.0).round() as i32;
        Box::new(
            MusicalDuration::try_new(measures, beats, subdivisions.clamp(0, 999))
                .unwrap_or_else(|_| MusicalDuration::zero()),
        )
    }

    fn clone_box(&self) -> Box<dyn DurationTrait> {
        Box::new(self.clone())
    }
}

// Note: DurationTrait already implements Display via the trait definition

// Helper functions for conversion
pub mod conversion {
    use super::*;

    /// Convert u32-based duration to DAW's MusicalDuration (i32-based)
    pub fn u32_to_musical_duration(
        measures: u32,
        beats: u32,
        subdivisions: u32,
    ) -> MusicalDuration {
        MusicalDuration::try_new(measures as i32, beats as i32, subdivisions as i32)
            .unwrap_or_else(|_| MusicalDuration::zero())
    }

    /// Convert DAW's MusicalDuration (i32-based) to u32 values
    pub fn musical_duration_to_u32(md: &MusicalDuration) -> (u32, u32, u32) {
        (
            md.measure.max(0) as u32,
            md.beat.max(0) as u32,
            md.subdivision.clamp(0, 999) as u32,
        )
    }
}
