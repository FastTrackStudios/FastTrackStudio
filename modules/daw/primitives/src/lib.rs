use serde::{Deserialize, Serialize};
use specta::Type;

#[derive(Type, Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
pub struct MusicalPosition {
    pub measure: i32,
    pub beat: i32,
    pub subdivision: i32,
}

impl MusicalPosition {
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

#[derive(Type, Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TimePosition {
    pub minutes: i32,
    pub seconds: i32,
    pub milliseconds: i32,
}

impl TimePosition {
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

#[derive(Type, Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
pub struct Position {
    pub musical: MusicalPosition,
    pub time: TimePosition,
}

impl Position {
    pub fn new(musical: MusicalPosition, time: TimePosition) -> Self {
        Self { musical, time }
    }

    pub fn from_musical(musical: MusicalPosition) -> Self {
        Self {
            musical,
            time: TimePosition::start(),
        }
    }

    pub fn from_time(time: TimePosition) -> Self {
        Self {
            musical: MusicalPosition::start(),
            time,
        }
    }

    pub fn from_seconds(total_seconds: f64) -> Self {
        Self {
            musical: MusicalPosition::start(),
            time: TimePosition::from_seconds(total_seconds),
        }
    }

    pub fn start() -> Self {
        Self {
            musical: MusicalPosition::start(),
            time: TimePosition::start(),
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
}

impl Default for TimeSignature {
    fn default() -> Self {
        Self::new(4, 4)
    }
}
