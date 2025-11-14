//! Time and position primitives
//!
//! This module provides fundamental types for representing time and musical positions
//! that can be reused across different domain modules (transport, marker_region, etc.)

use ts_rs::TS;

/// Musical position in measures.beats.subdivisions format
///
/// Subdivisions are 0-999, where 1000 would equal a full beat.
/// The beat value changes based on the time signature.
///
/// Examples:
/// - `MusicalPosition { measure: 0, beat: 0, subdivision: 500 }` = 0.0.500 (eighth note in first beat)
/// - `MusicalPosition { measure: 4, beat: 3, subdivision: 400 }` = 4.3.400 (measure 4, beat 3, 40% through the beat)
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize, TS,
)]
#[ts(export)]
pub struct MusicalPosition {
    /// Measure number (0-indexed)
    pub measure: i32,
    /// Beat within the measure (0-indexed)
    pub beat: i32,
    /// Subdivision within the beat (0-999, where 1000 = full beat)
    pub subdivision: i32,
}

impl MusicalPosition {
    /// Create a new musical position
    ///
    /// # Panics
    ///
    /// Panics if subdivision is not in the range 0-999
    pub fn new(measure: i32, beat: i32, subdivision: i32) -> Self {
        assert!(
            subdivision >= 0 && subdivision <= 999,
            "Subdivision must be in range 0-999, got {}",
            subdivision
        );
        Self {
            measure,
            beat,
            subdivision,
        }
    }

    /// Create a new musical position with validation
    ///
    /// Returns an error if subdivision is not in the range 0-999
    pub fn try_new(measure: i32, beat: i32, subdivision: i32) -> Result<Self, String> {
        if subdivision < 0 || subdivision > 999 {
            return Err(format!(
                "Subdivision must be in range 0-999, got {}",
                subdivision
            ));
        }
        Ok(Self {
            measure,
            beat,
            subdivision,
        })
    }

    /// Get the position at the start (measure 0, beat 0, subdivision 0)
    pub fn start() -> Self {
        Self {
            measure: 0,
            beat: 0,
            subdivision: 0,
        }
    }
}

impl Default for MusicalPosition {
    fn default() -> Self {
        Self::start()
    }
}

/// Time position in minutes:seconds:milliseconds format
///
/// This represents an absolute time position, independent of tempo and time signature.
///
/// Examples:
/// - `TimePosition { minutes: 0, seconds: 5, milliseconds: 250 }` = 0:05.250 (5.25 seconds)
/// - `TimePosition { minutes: 2, seconds: 30, milliseconds: 500 }` = 2:30.500 (2 minutes, 30.5 seconds)
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize, TS,
)]
#[ts(export)]
pub struct TimePosition {
    /// Minutes component
    pub minutes: i32,
    /// Seconds component (0-59)
    pub seconds: i32,
    /// Milliseconds component (0-999)
    pub milliseconds: i32,
}

impl TimePosition {
    /// Create a new time position
    ///
    /// # Panics
    ///
    /// Panics if seconds is not in the range 0-59 or milliseconds is not in the range 0-999
    pub fn new(minutes: i32, seconds: i32, milliseconds: i32) -> Self {
        assert!(
            seconds >= 0 && seconds <= 59,
            "Seconds must be in range 0-59, got {}",
            seconds
        );
        assert!(
            milliseconds >= 0 && milliseconds <= 999,
            "Milliseconds must be in range 0-999, got {}",
            milliseconds
        );
        Self {
            minutes,
            seconds,
            milliseconds,
        }
    }

    /// Create a new time position with validation
    ///
    /// Returns an error if seconds or milliseconds are out of range
    pub fn try_new(minutes: i32, seconds: i32, milliseconds: i32) -> Result<Self, String> {
        if seconds < 0 || seconds > 59 {
            return Err(format!("Seconds must be in range 0-59, got {}", seconds));
        }
        if milliseconds < 0 || milliseconds > 999 {
            return Err(format!(
                "Milliseconds must be in range 0-999, got {}",
                milliseconds
            ));
        }
        Ok(Self {
            minutes,
            seconds,
            milliseconds,
        })
    }

    /// Create a TimePosition from total seconds (as f64)
    ///
    /// This converts a floating-point seconds value into minutes:seconds:milliseconds format.
    pub fn from_seconds(total_seconds: f64) -> Self {
        let total_ms = (total_seconds * 1000.0) as i64;
        let minutes = (total_ms / 60_000) as i32;
        let remaining_ms = total_ms % 60_000;
        let seconds = (remaining_ms / 1000) as i32;
        let milliseconds = (remaining_ms % 1000) as i32;
        Self {
            minutes,
            seconds,
            milliseconds,
        }
    }

    /// Convert to total seconds as f64
    pub fn to_seconds(&self) -> f64 {
        self.minutes as f64 * 60.0 + self.seconds as f64 + self.milliseconds as f64 / 1000.0
    }

    /// Get the position at the start (0:00.000)
    pub fn start() -> Self {
        Self {
            minutes: 0,
            seconds: 0,
            milliseconds: 0,
        }
    }
}

impl Default for TimePosition {
    fn default() -> Self {
        Self::start()
    }
}

/// Position that contains both musical and time-based representations
///
/// This allows transport systems to provide position information in multiple formats,
/// making it easier to integrate with different DAWs that may provide different position types.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize, TS,
)]
#[ts(export)]
pub struct Position {
    /// Musical position (measure.beat.subdivision)
    pub musical: MusicalPosition,
    /// Time position (minutes:seconds:milliseconds)
    pub time: TimePosition,
}

impl Position {
    /// Create a new position with both musical and time components
    pub fn new(musical: MusicalPosition, time: TimePosition) -> Self {
        Self { musical, time }
    }

    /// Create a position from musical position only (time will be default/zero)
    pub fn from_musical(musical: MusicalPosition) -> Self {
        Self {
            musical,
            time: TimePosition::start(),
        }
    }

    /// Create a position from time position only (musical will be default/zero)
    pub fn from_time(time: TimePosition) -> Self {
        Self {
            musical: MusicalPosition::start(),
            time,
        }
    }

    /// Create a position from total seconds (musical will be default/zero)
    pub fn from_seconds(total_seconds: f64) -> Self {
        Self {
            musical: MusicalPosition::start(),
            time: TimePosition::from_seconds(total_seconds),
        }
    }

    /// Get the position at the start (measure 0, beat 0, subdivision 0, time 0:00.000)
    pub fn start() -> Self {
        Self {
            musical: MusicalPosition::start(),
            time: TimePosition::start(),
        }
    }

    /// Get musical position as a string in REAPER format (measure.beat.subdivision)
    pub fn musical_position_string(&self) -> String {
        format!("{}.{}.{:03}",
                self.musical.measure + 1,  // Convert to 1-based for display
                self.musical.beat + 1,     // Convert to 1-based for display
                self.musical.subdivision)
    }
}

impl Default for Position {
    fn default() -> Self {
        Self::start()
    }
}

/// Time range representing a selection or loop region
///
/// Contains both start and end positions, each with musical and time representations.
/// This is the base type used for time selections and loop selections in the transport.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
pub struct TimeRange {
    /// Start position of the range
    pub start: Position,
    /// End position of the range
    pub end: Position,
}

impl TimeRange {
    /// Create a new time range
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    /// Create a time range from start and end positions (musical only)
    pub fn from_musical(start: MusicalPosition, end: MusicalPosition) -> Self {
        Self {
            start: Position::from_musical(start),
            end: Position::from_musical(end),
        }
    }

    /// Create a time range from start and end positions (time only)
    pub fn from_time(start: TimePosition, end: TimePosition) -> Self {
        Self {
            start: Position::from_time(start),
            end: Position::from_time(end),
        }
    }

    /// Create a time range from start and end seconds
    pub fn from_seconds(start_seconds: f64, end_seconds: f64) -> Self {
        Self {
            start: Position::from_seconds(start_seconds),
            end: Position::from_seconds(end_seconds),
        }
    }

    /// Get the length of the range in seconds
    pub fn length_seconds(&self) -> f64 {
        self.end.time.to_seconds() - self.start.time.to_seconds()
    }

    /// Check if the range is valid (end > start)
    pub fn is_valid(&self) -> bool {
        self.end.time.to_seconds() > self.start.time.to_seconds()
    }
}

/// Time selection range
///
/// This represents the currently selected time range in the timeline.
/// It's a type alias for `TimeRange` to provide semantic clarity.
pub type TimeSelection = TimeRange;

/// Loop points range
///
/// This represents the range that will loop when looping is enabled.
/// It's a type alias for `TimeRange` to provide semantic clarity.
pub type LoopPoints = TimeRange;

/// Time signature representation
///
/// Examples:
/// - `TimeSignature { numerator: 4, denominator: 4 }` for 4/4 time
/// - `TimeSignature { numerator: 6, denominator: 8 }` for 6/8 time
/// - `TimeSignature { numerator: 3, denominator: 4 }` for 3/4 time
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize, TS)]
#[ts(export)]
pub struct TimeSignature {
    /// Numerator (e.g., 4 for 4/4, 6 for 6/8)
    pub numerator: i32,
    /// Denominator (e.g., 4 for 4/4, 8 for 6/8)
    pub denominator: i32,
}

impl TimeSignature {
    /// Create a new time signature
    pub fn new(numerator: i32, denominator: i32) -> Self {
        Self {
            numerator,
            denominator,
        }
    }

    /// Get the default 4/4 time signature
    pub fn four_four() -> Self {
        Self {
            numerator: 4,
            denominator: 4,
        }
    }
}

impl Default for TimeSignature {
    fn default() -> Self {
        Self::four_four()
    }
}
