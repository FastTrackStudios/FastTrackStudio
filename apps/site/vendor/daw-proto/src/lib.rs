use facet::Facet;
use serde::{Deserialize, Serialize};
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Facet, Serialize, Deserialize)]
pub struct TimeSignature {
    pub numerator: u32,
    pub denominator: u32,
}

impl TimeSignature {
    pub const COMMON_TIME: Self = Self {
        numerator: 4,
        denominator: 4,
    };

    pub fn new(numerator: u32, denominator: u32) -> Self {
        assert!(numerator > 0, "Time signature numerator cannot be 0");
        assert!(denominator > 0, "Time signature denominator cannot be 0");
        Self {
            numerator,
            denominator,
        }
    }

    pub fn numerator(&self) -> u32 {
        self.numerator
    }

    pub fn denominator(&self) -> u32 {
        self.denominator
    }

    pub fn beats_per_measure(&self) -> f64 {
        self.numerator as f64
    }
}

impl Default for TimeSignature {
    fn default() -> Self {
        Self::COMMON_TIME
    }
}

impl fmt::Display for TimeSignature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}", self.numerator, self.denominator)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Facet, Serialize, Deserialize)]
pub struct Tempo {
    pub bpm: f64,
}

impl Tempo {
    pub const ONE_TWENTY: Self = Self { bpm: 120.0 };

    pub fn from_bpm(bpm: f64) -> Self {
        assert!(bpm > 0.0, "BPM must be positive, got {}", bpm);
        Self { bpm }
    }

    pub fn try_from_bpm(bpm: f64) -> Result<Self, String> {
        if bpm <= 0.0 {
            Err(format!("BPM must be positive, got {}", bpm))
        } else {
            Ok(Self { bpm })
        }
    }

    pub fn bpm(&self) -> f64 {
        self.bpm
    }
}

impl Default for Tempo {
    fn default() -> Self {
        Self::ONE_TWENTY
    }
}

impl fmt::Display for Tempo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:.2} BPM", self.bpm)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Facet, Serialize, Deserialize)]
pub struct TimePosition {
    seconds: f64,
}

impl TimePosition {
    pub const ZERO: Self = Self { seconds: 0.0 };

    pub fn from_seconds(seconds: f64) -> Self {
        Self { seconds }
    }

    pub fn as_seconds(&self) -> f64 {
        self.seconds
    }

    pub fn to_musical(&self, tempo: Tempo, time_signature: TimeSignature) -> MusicalPosition {
        let beats_per_measure = time_signature.numerator() as f64;
        let total_beats = self.seconds * (tempo.bpm() / 60.0);
        let measure = (total_beats / beats_per_measure).floor() as i32;
        let beats_in_measure = total_beats % beats_per_measure;
        let beat = beats_in_measure.floor() as i32;
        let subdivision = ((beats_in_measure - beat as f64) * 1000.0).round() as i32;

        MusicalPosition::new(measure, beat, subdivision.clamp(0, 999))
    }
}

impl Default for TimePosition {
    fn default() -> Self {
        Self::ZERO
    }
}

impl fmt::Display for TimePosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let abs_seconds = self.seconds.abs();
        let sign = if self.seconds < 0.0 { "-" } else { "" };
        let minutes = (abs_seconds / 60.0).floor() as i32;
        let secs = abs_seconds % 60.0;
        write!(f, "{}{}:{:06.3}", sign, minutes, secs)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Facet, Serialize, Deserialize)]
pub struct MusicalPosition {
    pub measure: i32,
    pub beat: i32,
    pub subdivision: i32,
}

impl MusicalPosition {
    pub const ZERO: Self = Self {
        measure: 0,
        beat: 0,
        subdivision: 0,
    };

    pub fn new(measure: i32, beat: i32, subdivision: i32) -> Self {
        assert!(
            (0..=999).contains(&subdivision),
            "Subdivision must be 0-999, got {}",
            subdivision
        );
        Self {
            measure,
            beat,
            subdivision,
        }
    }
}

impl Default for MusicalPosition {
    fn default() -> Self {
        Self::ZERO
    }
}

impl fmt::Display for MusicalPosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let sign = if self.measure < 0 || self.beat < 0 {
            "-"
        } else {
            ""
        };
        write!(
            f,
            "{}{}.{}.{:03}",
            sign,
            self.measure.abs() + 1,
            self.beat.abs() + 1,
            self.subdivision.abs()
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Facet, Serialize, Deserialize)]
pub struct MidiPosition {
    ppq: i64,
}

impl MidiPosition {
    pub const ZERO: Self = Self { ppq: 0 };

    pub fn from_ppq(ppq: i64) -> Self {
        Self { ppq }
    }

    pub fn as_ppq(&self) -> i64 {
        self.ppq
    }
}

impl Default for MidiPosition {
    fn default() -> Self {
        Self::ZERO
    }
}

impl fmt::Display for MidiPosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ppq", self.ppq)
    }
}

#[derive(Debug, Clone, PartialEq, Facet, Serialize, Deserialize)]
pub struct Position {
    pub musical: Option<MusicalPosition>,
    pub time: Option<TimePosition>,
    pub midi: Option<MidiPosition>,
}

impl Position {
    pub fn new(
        musical: Option<MusicalPosition>,
        time: Option<TimePosition>,
        midi: Option<MidiPosition>,
    ) -> Self {
        Self {
            musical,
            time,
            midi,
        }
    }

    pub fn from_musical(musical: MusicalPosition) -> Self {
        Self::new(Some(musical), None, None)
    }

    pub fn from_time(time: TimePosition) -> Self {
        Self::new(None, Some(time), None)
    }

    pub fn from_midi(midi: MidiPosition) -> Self {
        Self::new(None, None, Some(midi))
    }

    pub fn start() -> Self {
        Self::new(
            Some(MusicalPosition::ZERO),
            Some(TimePosition::ZERO),
            Some(MidiPosition::ZERO),
        )
    }
}

impl Default for Position {
    fn default() -> Self {
        Self::start()
    }
}

#[derive(Debug, Clone, PartialEq, Facet, Serialize, Deserialize, Default)]
pub struct TimeRange {
    pub start: Position,
    pub end: Position,
}

impl TimeRange {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn from_seconds(start: f64, end: f64) -> Self {
        Self {
            start: Position::from_time(TimePosition::from_seconds(start)),
            end: Position::from_time(TimePosition::from_seconds(end)),
        }
    }

    pub fn start_seconds(&self) -> f64 {
        self.start.time.map(|t| t.as_seconds()).unwrap_or(0.0)
    }

    pub fn end_seconds(&self) -> f64 {
        self.end.time.map(|t| t.as_seconds()).unwrap_or(0.0)
    }
}
