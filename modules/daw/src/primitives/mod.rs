use serde::{Deserialize, Serialize};
use specta::Type;
use std::fmt;

#[derive(Type, Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
pub struct MusicalPosition {
    pub measure: i32,
    pub beat: i32,
    pub subdivision: i32,
}

impl MusicalPosition {
    pub fn new(measure: i32, beat: i32, subdivision: i32) -> Self {
        assert!(
            (0..=999).contains(&subdivision),
            "Subdivision must be in range 0-999, got {}",
            subdivision
        );
        Self {
            measure,
            beat,
            subdivision,
        }
    }

    pub fn try_new(measure: i32, beat: i32, subdivision: i32) -> Result<Self, String> {
        if !(0..=999).contains(&subdivision) {
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

    pub fn start() -> Self {
        Self {
            measure: 0,
            beat: 0,
            subdivision: 0,
        }
    }

    /// Convert musical position to time position using BPM and time signature
    ///
    /// Formula:
    /// - Total beats = measure * beats_per_measure + beat + subdivision/1000
    /// - Seconds = total_beats * (60 / BPM)
    ///
    /// # Arguments
    /// * `bpm` - Beats per minute (tempo)
    /// * `time_signature` - Time signature (numerator/denominator)
    ///
    /// # Returns
    /// A `TimePosition` representing the equivalent time position
    pub fn to_time_position(&self, bpm: f64, time_signature: TimeSignature) -> TimePosition {
        // Calculate beats per measure from time signature
        let beats_per_measure = time_signature.numerator as f64;

        // Calculate total beats
        // Subdivision is in thousandths (0-999), so divide by 1000 to get fractional beats
        let total_beats = self.measure as f64 * beats_per_measure
            + self.beat as f64
            + self.subdivision as f64 / 1000.0;

        // Convert beats to seconds: seconds = beats * (60 / BPM)
        let total_seconds = total_beats * (60.0 / bpm);

        TimePosition::from_seconds(total_seconds)
    }
}

impl Default for MusicalPosition {
    fn default() -> Self {
        Self::start()
    }
}

impl fmt::Display for MusicalPosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}.{}.{:03}",
            self.measure + 1,
            self.beat + 1,
            self.subdivision
        )
    }
}

#[derive(Type, Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TimePosition {
    pub minutes: i32,
    pub seconds: i32,
    pub milliseconds: i32,
}

impl TimePosition {
    pub fn new(minutes: i32, seconds: i32, milliseconds: i32) -> Self {
        assert!(
            (0..=59).contains(&seconds),
            "Seconds must be in range 0-59, got {}",
            seconds
        );
        assert!(
            (0..=999).contains(&milliseconds),
            "Milliseconds must be in range 0-999, got {}",
            milliseconds
        );
        Self {
            minutes,
            seconds,
            milliseconds,
        }
    }

    pub fn try_new(minutes: i32, seconds: i32, milliseconds: i32) -> Result<Self, String> {
        if !(0..=59).contains(&seconds) {
            return Err(format!("Seconds must be in range 0-59, got {}", seconds));
        }
        if !(0..=999).contains(&milliseconds) {
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

    pub fn to_seconds(&self) -> f64 {
        self.minutes as f64 * 60.0 + self.seconds as f64 + self.milliseconds as f64 / 1000.0
    }

    pub fn start() -> Self {
        Self {
            minutes: 0,
            seconds: 0,
            milliseconds: 0,
        }
    }

    /// Convert time position to musical position using BPM and time signature
    ///
    /// Formula (reverse of musical to time):
    /// - Total beats = seconds * (BPM / 60)
    /// - Measure = floor(total_beats / beats_per_measure)
    /// - Beat = floor(total_beats % beats_per_measure)
    /// - Subdivision = ((total_beats % beats_per_measure) - beat) * 1000
    ///
    /// # Arguments
    /// * `bpm` - Beats per minute (tempo)
    /// * `time_signature` - Time signature (numerator/denominator)
    ///
    /// # Returns
    /// A `MusicalPosition` representing the equivalent musical position
    pub fn to_musical_position(&self, bpm: f64, time_signature: TimeSignature) -> MusicalPosition {
        let total_seconds = self.to_seconds();
        let beats_per_measure = time_signature.numerator as f64;

        // Calculate total beats from seconds
        let total_beats = total_seconds * (bpm / 60.0);

        // Calculate measure (floor division)
        let measure = (total_beats / beats_per_measure).floor() as i32;

        // Calculate beat within the measure
        let beats_in_measure = total_beats % beats_per_measure;
        let beat = beats_in_measure.floor() as i32;

        // Calculate subdivision (thousandths of a beat)
        let subdivision = ((beats_in_measure - beat as f64) * 1000.0).round() as i32;

        MusicalPosition::try_new(measure, beat, subdivision.clamp(0, 999))
            .unwrap_or_else(|_| MusicalPosition::start())
    }
}

impl Default for TimePosition {
    fn default() -> Self {
        Self::start()
    }
}

impl fmt::Display for TimePosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{:02}.{:03}",
            self.minutes, self.seconds, self.milliseconds
        )
    }
}

/// PPQ (Parts Per Quarter note) position
/// Represents a position in PPQ ticks, commonly used in MIDI sequencing
#[derive(Type, Serialize, Deserialize, Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct PPQPosition {
    pub ppq: i64,
}

impl PPQPosition {
    pub fn new(ppq: i64) -> Self {
        Self { ppq }
    }

    pub fn zero() -> Self {
        Self { ppq: 0 }
    }

    /// Convert PPQ position to musical position using PPQ resolution and time signature
    ///
    /// # Arguments
    /// * `ppq_resolution` - PPQ resolution (typically 480 or 960)
    /// * `time_signature` - Time signature (numerator/denominator)
    ///
    /// # Returns
    /// A `MusicalPosition` representing the equivalent musical position
    pub fn to_musical_position(
        &self,
        ppq_resolution: f64,
        time_signature: TimeSignature,
    ) -> MusicalPosition {
        let beats_per_measure = time_signature.numerator as f64;
        let total_beats = self.ppq as f64 / ppq_resolution;

        let measures = (total_beats / beats_per_measure).floor() as i32;
        let beats_in_measure = (total_beats % beats_per_measure).floor() as i32;
        let subdivision = ((total_beats % 1.0) * 1000.0).round() as i32;

        MusicalPosition::try_new(measures, beats_in_measure, subdivision.clamp(0, 999))
            .unwrap_or_else(|_| MusicalPosition::start())
    }

    /// Convert PPQ position to time position using PPQ resolution and BPM
    ///
    /// # Arguments
    /// * `ppq_resolution` - PPQ resolution (typically 480 or 960)
    /// * `bpm` - Beats per minute (tempo)
    ///
    /// # Returns
    /// A `TimePosition` representing the equivalent time position
    pub fn to_time_position(&self, ppq_resolution: f64, bpm: f64) -> TimePosition {
        let total_beats = self.ppq as f64 / ppq_resolution;
        let total_seconds = total_beats * (60.0 / bpm);
        TimePosition::from_seconds(total_seconds)
    }
}

impl fmt::Display for PPQPosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} PPQ", self.ppq)
    }
}

#[derive(Type, Serialize, Deserialize, Debug, Clone)]
pub struct Position {
    pub musical: MusicalPosition,
    pub time: TimePosition,
    pub ppq: Option<PPQPosition>,
}

impl Position {
    pub fn new(musical: MusicalPosition, time: TimePosition) -> Self {
        Self {
            musical,
            time,
            ppq: None,
        }
    }

    pub fn from_musical(musical: MusicalPosition) -> Self {
        Self {
            musical,
            time: TimePosition::start(),
            ppq: None,
        }
    }

    pub fn from_time(time: TimePosition) -> Self {
        Self {
            musical: MusicalPosition::start(),
            time,
            ppq: None,
        }
    }

    pub fn from_seconds(total_seconds: f64) -> Self {
        Self {
            musical: MusicalPosition::start(),
            ppq: None,
            time: TimePosition::from_seconds(total_seconds),
        }
    }

    pub fn start() -> Self {
        Self {
            musical: MusicalPosition::start(),
            time: TimePosition::start(),
            ppq: None,
        }
    }

    pub fn musical_position_string(&self) -> String {
        format!(
            "{}.{}.{:03}",
            self.musical.measure + 1, // Convert to 1-based for display
            self.musical.beat + 1,    // Convert to 1-based for display
            self.musical.subdivision
        )
    }
}

impl Default for Position {
    fn default() -> Self {
        Self::start()
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.musical_position_string(), self.time)
    }
}

impl PartialEq for Position {
    fn eq(&self, other: &Self) -> bool {
        self.time.to_seconds() == other.time.to_seconds()
    }
}

impl Eq for Position {}

impl PartialOrd for Position {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.time.to_seconds().partial_cmp(&other.time.to_seconds())
    }
}

#[derive(Type, Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
pub struct TimeRange {
    pub start: Position,
    pub end: Position,
}

impl TimeRange {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn from_musical(start: MusicalPosition, end: MusicalPosition) -> Self {
        Self {
            start: Position::from_musical(start),
            end: Position::from_musical(end),
        }
    }

    pub fn from_time(start: TimePosition, end: TimePosition) -> Self {
        Self {
            start: Position::from_time(start),
            end: Position::from_time(end),
        }
    }

    pub fn from_seconds(start_seconds: f64, end_seconds: f64) -> Self {
        Self {
            start: Position::from_seconds(start_seconds),
            end: Position::from_seconds(end_seconds),
        }
    }

    pub fn length_seconds(&self) -> f64 {
        self.end.time.to_seconds() - self.start.time.to_seconds()
    }

    pub fn is_valid(&self) -> bool {
        self.end.time.to_seconds() > self.start.time.to_seconds()
    }
}

pub type TimeSelection = TimeRange;

pub type LoopPoints = TimeRange;

#[derive(Type, Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TimeSignature {
    pub numerator: i32,
    pub denominator: i32,
}

impl TimeSignature {
    pub fn new(numerator: i32, denominator: i32) -> Self {
        Self {
            numerator,
            denominator,
        }
    }

    pub fn common_time() -> Self {
        Self::new(4, 4)
    }
}

impl Default for TimeSignature {
    fn default() -> Self {
        Self::new(4, 4)
    }
}

impl fmt::Display for TimeSignature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}", self.numerator, self.denominator)
    }
}

// ============================================================================
// Duration Types - How long something lasts (mirrors Position types)
// ============================================================================

/// Musical duration - how long something lasts in musical time
/// Uses the same structure as MusicalPosition but represents a duration rather than a position
#[derive(Type, Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
pub struct MusicalDuration {
    pub measure: i32,
    pub beat: i32,
    pub subdivision: i32,
}

impl MusicalDuration {
    pub fn new(measure: i32, beat: i32, subdivision: i32) -> Self {
        assert!(
            (0..=999).contains(&subdivision),
            "Subdivision must be in range 0-999, got {}",
            subdivision
        );
        Self {
            measure,
            beat,
            subdivision,
        }
    }

    pub fn try_new(measure: i32, beat: i32, subdivision: i32) -> Result<Self, String> {
        if !(0..=999).contains(&subdivision) {
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

    pub fn zero() -> Self {
        Self {
            measure: 0,
            beat: 0,
            subdivision: 0,
        }
    }

    /// Convert musical duration to time duration using BPM and time signature
    ///
    /// Formula:
    /// - Total beats = measure * beats_per_measure + beat + subdivision/1000
    /// - Seconds = total_beats * (60 / BPM)
    ///
    /// # Arguments
    /// * `bpm` - Beats per minute (tempo)
    /// * `time_signature` - Time signature (numerator/denominator)
    ///
    /// # Returns
    /// A `TimeDuration` representing the equivalent time duration
    pub fn to_time_duration(&self, bpm: f64, time_signature: TimeSignature) -> TimeDuration {
        // Calculate beats per measure from time signature
        let beats_per_measure = time_signature.numerator as f64;

        // Calculate total beats
        // Subdivision is in thousandths (0-999), so divide by 1000 to get fractional beats
        let total_beats = self.measure as f64 * beats_per_measure
            + self.beat as f64
            + self.subdivision as f64 / 1000.0;

        // Convert beats to seconds: seconds = beats * (60 / BPM)
        let total_seconds = total_beats * (60.0 / bpm);

        TimeDuration::from_seconds(total_seconds)
    }

    /// Convert musical duration to total beats as f64
    ///
    /// # Arguments
    /// * `time_signature` - Time signature (numerator/denominator)
    ///
    /// # Returns
    /// Total beats as f64
    pub fn to_beats(&self, time_signature: TimeSignature) -> f64 {
        let beats_per_measure = time_signature.numerator as f64;
        self.measure.max(0) as f64 * beats_per_measure
            + self.beat.max(0) as f64
            + self.subdivision.clamp(0, 999) as f64 / 1000.0
    }

    /// Create a musical duration from total beats
    ///
    /// # Arguments
    /// * `total_beats` - Total beats as f64
    /// * `time_signature` - Time signature (numerator/denominator)
    ///
    /// # Returns
    /// A `MusicalDuration` representing the equivalent musical duration
    pub fn from_beats(total_beats: f64, time_signature: TimeSignature) -> Self {
        let beats_per_measure = time_signature.numerator as f64;
        let measures = (total_beats / beats_per_measure).floor() as i32;
        let remaining_beats = total_beats - (measures as f64 * beats_per_measure);
        let beats = remaining_beats.floor() as i32;
        let subdivisions = ((remaining_beats - beats as f64) * 1000.0).round() as i32;
        Self::try_new(measures, beats, subdivisions.clamp(0, 999)).unwrap_or_else(|_| Self::zero())
    }
}

impl Default for MusicalDuration {
    fn default() -> Self {
        Self::zero()
    }
}

impl fmt::Display for MusicalDuration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}.{:03}", self.measure, self.beat, self.subdivision)
    }
}

/// PPQ (Parts Per Quarter note) duration
/// Represents a duration in PPQ ticks, commonly used in MIDI sequencing
#[derive(Type, Serialize, Deserialize, Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct PPQDuration {
    pub ppq: i64,
}

impl PPQDuration {
    pub fn new(ppq: i64) -> Self {
        Self { ppq }
    }

    pub fn zero() -> Self {
        Self { ppq: 0 }
    }

    /// Convert PPQ duration to musical duration using PPQ resolution and time signature
    ///
    /// # Arguments
    /// * `ppq_resolution` - PPQ resolution (typically 480 or 960)
    /// * `time_signature` - Time signature (numerator/denominator)
    ///
    /// # Returns
    /// A `MusicalDuration` representing the equivalent musical duration
    pub fn to_musical_duration(
        &self,
        ppq_resolution: f64,
        time_signature: TimeSignature,
    ) -> MusicalDuration {
        let beats_per_measure = time_signature.numerator as f64;
        let total_beats = self.ppq as f64 / ppq_resolution;

        let measures = (total_beats / beats_per_measure).floor() as i32;
        let beats_in_measure = (total_beats % beats_per_measure).floor() as i32;
        let subdivision = ((total_beats % 1.0) * 1000.0).round() as i32;

        MusicalDuration::try_new(measures, beats_in_measure, subdivision.clamp(0, 999))
            .unwrap_or_else(|_| MusicalDuration::zero())
    }

    /// Convert PPQ duration to time duration using PPQ resolution and BPM
    ///
    /// # Arguments
    /// * `ppq_resolution` - PPQ resolution (typically 480 or 960)
    /// * `bpm` - Beats per minute (tempo)
    ///
    /// # Returns
    /// A `TimeDuration` representing the equivalent time duration
    pub fn to_time_duration(&self, ppq_resolution: f64, bpm: f64) -> TimeDuration {
        let total_beats = self.ppq as f64 / ppq_resolution;
        let total_seconds = total_beats * (60.0 / bpm);
        TimeDuration::from_seconds(total_seconds)
    }
}

impl fmt::Display for PPQDuration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} PPQ", self.ppq)
    }
}

/// Time duration - how long something lasts in real time
/// Uses the same structure as TimePosition but represents a duration rather than a position
#[derive(Type, Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TimeDuration {
    pub minutes: i32,
    pub seconds: i32,
    pub milliseconds: i32,
}

impl TimeDuration {
    pub fn new(minutes: i32, seconds: i32, milliseconds: i32) -> Self {
        assert!(
            (0..=59).contains(&seconds),
            "Seconds must be in range 0-59, got {}",
            seconds
        );
        assert!(
            (0..=999).contains(&milliseconds),
            "Milliseconds must be in range 0-999, got {}",
            milliseconds
        );
        Self {
            minutes,
            seconds,
            milliseconds,
        }
    }

    pub fn try_new(minutes: i32, seconds: i32, milliseconds: i32) -> Result<Self, String> {
        if !(0..=59).contains(&seconds) {
            return Err(format!("Seconds must be in range 0-59, got {}", seconds));
        }
        if !(0..=999).contains(&milliseconds) {
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

    pub fn to_seconds(&self) -> f64 {
        self.minutes as f64 * 60.0 + self.seconds as f64 + self.milliseconds as f64 / 1000.0
    }

    pub fn zero() -> Self {
        Self {
            minutes: 0,
            seconds: 0,
            milliseconds: 0,
        }
    }

    /// Convert time duration to musical duration using BPM and time signature
    ///
    /// Formula (reverse of musical to time):
    /// - Total beats = seconds * (BPM / 60)
    /// - Measure = floor(total_beats / beats_per_measure)
    /// - Beat = floor(total_beats % beats_per_measure)
    /// - Subdivision = ((total_beats % beats_per_measure) - beat) * 1000
    ///
    /// # Arguments
    /// * `bpm` - Beats per minute (tempo)
    /// * `time_signature` - Time signature (numerator/denominator)
    ///
    /// # Returns
    /// A `MusicalDuration` representing the equivalent musical duration
    pub fn to_musical_duration(&self, bpm: f64, time_signature: TimeSignature) -> MusicalDuration {
        let total_seconds = self.to_seconds();
        let beats_per_measure = time_signature.numerator as f64;

        // Calculate total beats from seconds
        let total_beats = total_seconds * (bpm / 60.0);

        // Calculate measure (floor division)
        let measure = (total_beats / beats_per_measure).floor() as i32;

        // Calculate beat within the measure
        let beats_in_measure = total_beats % beats_per_measure;
        let beat = beats_in_measure.floor() as i32;

        // Calculate subdivision (thousandths of a beat)
        let subdivision = ((beats_in_measure - beat as f64) * 1000.0).round() as i32;

        MusicalDuration::try_new(measure, beat, subdivision.clamp(0, 999))
            .unwrap_or_else(|_| MusicalDuration::zero())
    }
}

impl Default for TimeDuration {
    fn default() -> Self {
        Self::zero()
    }
}

impl fmt::Display for TimeDuration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{:02}.{:03}",
            self.minutes, self.seconds, self.milliseconds
        )
    }
}

/// Combined duration - represents both musical and time duration
/// Similar to Position but for durations
#[derive(Type, Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
pub struct Duration {
    pub musical: MusicalDuration,
    pub time: TimeDuration,
    pub ppq: Option<PPQDuration>,
}

impl Duration {
    pub fn new(musical: MusicalDuration, time: TimeDuration) -> Self {
        Self {
            musical,
            time,
            ppq: None,
        }
    }

    pub fn from_musical(musical: MusicalDuration) -> Self {
        Self {
            musical,
            time: TimeDuration::zero(),
            ppq: None,
        }
    }

    pub fn from_time(time: TimeDuration) -> Self {
        Self {
            musical: MusicalDuration::zero(),
            time,
            ppq: None,
        }
    }

    pub fn from_seconds(total_seconds: f64) -> Self {
        Self {
            musical: MusicalDuration::zero(),
            time: TimeDuration::from_seconds(total_seconds),
            ppq: None,
        }
    }

    pub fn zero() -> Self {
        Self {
            musical: MusicalDuration::zero(),
            time: TimeDuration::zero(),
            ppq: None,
        }
    }

    pub fn musical_duration_string(&self) -> String {
        format!(
            "{}.{}.{:03}",
            self.musical.measure, self.musical.beat, self.musical.subdivision
        )
    }
}

impl Default for Duration {
    fn default() -> Self {
        Self::zero()
    }
}

impl fmt::Display for Duration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.musical_duration_string(), self.time)
    }
}
