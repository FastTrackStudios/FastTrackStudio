//! Core Chart Types
//!
//! Defines the core data structures for chart representation

use super::commands::Command;
use super::cues::TextCue;
use crate::chord::{Chord, ChordRhythm, PushPullAmount};
use crate::primitives::RootNotation;
use crate::sections::Section;
use crate::time::{AbsolutePosition, MusicalDuration};
use serde::{Deserialize, Serialize};

/// Represents a chord instance with position and timing information
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ChordInstance {
    /// Root notation (preserves original format: note, degree, or roman)
    pub root: RootNotation,

    /// Full chord symbol for display (e.g., "Gmaj7", "C9", "Em7")
    pub full_symbol: String,

    /// Parsed chord data
    pub parsed: Chord,

    /// Rhythm notation (slashes, duration, etc.)
    pub rhythm: ChordRhythm,

    /// Original token before processing (e.g., "1", "I", "g", "Gmaj7")
    pub original_token: String,

    /// Duration in measure.beats.subdivision format
    pub duration: MusicalDuration,

    /// Position in the song
    pub position: AbsolutePosition,

    /// Push/pull timing adjustment: (is_push, amount)
    /// - true = push (anticipate, play earlier)
    /// - false = pull (delay, play later)
    pub push_pull: Option<(bool, PushPullAmount)>,

    /// Commands applied to this chord (fermata, accent, etc.)
    pub commands: Vec<Command>,
}

impl ChordInstance {
    pub fn new(
        root: RootNotation,
        full_symbol: String,
        parsed: Chord,
        rhythm: ChordRhythm,
        original_token: String,
        duration: MusicalDuration,
        position: AbsolutePosition,
    ) -> Self {
        Self {
            root,
            full_symbol,
            parsed,
            rhythm,
            original_token,
            duration,
            position,
            push_pull: None,
            commands: Vec::new(),
        }
    }

    pub fn with_push_pull(mut self, push_pull: Option<(bool, PushPullAmount)>) -> Self {
        self.push_pull = push_pull;
        self
    }

    pub fn with_commands(mut self, commands: Vec<Command>) -> Self {
        self.commands = commands;
        self
    }

    pub fn add_command(mut self, command: Command) -> Self {
        self.commands.push(command);
        self
    }

    /// Convert this chord instance to LilyPond chordmode notation
    /// 
    /// # Arguments
    /// * `key` - Optional key context for resolving scale degrees and roman numerals
    /// 
    /// # Returns
    /// LilyPond chord notation with optional duration (e.g., "cis:maj74", "des:m78", "c:maj72.")
    /// In LilyPond, durations are appended directly: c4, c2, c1, c4. (for dotted)
    pub fn to_lilypond(&self, key: Option<&crate::key::Key>) -> String {
        // Convert chord to LilyPond format
        let chord_str = self.parsed.to_lilypond(key);
        
        // Add duration if specified
        // In LilyPond chordmode, duration comes directly after the chord: c4, c2, c1, c4.
        if let Some(duration) = self.rhythm_to_lilypond_duration() {
            format!("{}{}", chord_str, duration)
        } else {
            chord_str
        }
    }

    /// Convert rhythm to LilyPond duration notation
    /// Returns duration string that can be appended directly to chord (e.g., "4", "2", "1", "4.")
    fn rhythm_to_lilypond_duration(&self) -> Option<String> {
        use crate::chord::{ChordRhythm, LilySyntax};
        
        match &self.rhythm {
            ChordRhythm::Lily { duration, dotted, .. } => {
                let dur_str = match duration {
                    LilySyntax::Whole => "1",
                    LilySyntax::Half => "2",
                    LilySyntax::Quarter => "4",
                    LilySyntax::Eighth => "8",
                    LilySyntax::Sixteenth => "16",
                    LilySyntax::ThirtySecond => "32",
                };
                if *dotted {
                    // Dotted duration: "4." not ".4"
                    Some(format!("{}.", dur_str))
                } else {
                    Some(dur_str.to_string())
                }
            }
            _ => None,
        }
    }
}

/// Represents a single measure with chords
///
/// A measure is ALWAYS 1.0.0 in duration (one measure).
/// The time signature defines how many beats are in that measure.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Measure {
    /// Chords in this measure
    pub chords: Vec<ChordInstance>,

    /// Time signature for this measure (defines beats per measure)
    pub time_signature: (u8, u8),

    /// Repeat count (1 = no repeat, 2+ = repeat this measure n times)
    pub repeat_count: usize,

    /// Text cues for instrument directions
    pub text_cues: Vec<TextCue>,
}

impl Measure {
    pub fn new() -> Self {
        Self {
            chords: Vec::new(),
            time_signature: (4, 4), // Default to 4/4
            repeat_count: 1,        // Default to no repeat
            text_cues: Vec::new(),
        }
    }

    pub fn with_chords(mut self, chords: Vec<ChordInstance>) -> Self {
        self.chords = chords;
        self
    }

    pub fn with_time_signature(mut self, time_signature: (u8, u8)) -> Self {
        self.time_signature = time_signature;
        self
    }

    /// Get the duration of this measure (always 1.0.0)
    pub fn duration(&self) -> MusicalDuration {
        MusicalDuration::new(1, 0, 0)
    }

    /// Get the number of beats in this measure based on time signature
    pub fn beats_per_measure(&self) -> u8 {
        self.time_signature.0
    }
}

impl Default for Measure {
    fn default() -> Self {
        Self::new()
    }
}

/// Represents a section with its measures
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ChartSection {
    /// Section information (type, number, etc.)
    pub section: Section,

    /// All measures in this section
    pub measures: Vec<Measure>,

    /// True if this section was recalled from a template
    pub from_template: bool,
}

impl ChartSection {
    pub fn new(section: Section) -> Self {
        Self {
            section,
            measures: Vec::new(),
            from_template: false,
        }
    }

    pub fn with_measures(mut self, measures: Vec<Measure>) -> Self {
        self.measures = measures;
        self
    }

    pub fn from_template(section: Section, measures: Vec<Measure>) -> Self {
        Self {
            section,
            measures,
            from_template: true,
        }
    }
}

/// Represents a key change event in the chart
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct KeyChange {
    /// Position where the key change occurs
    pub position: AbsolutePosition,

    /// The key before the change (None if this is the initial key)
    pub from_key: Option<crate::key::Key>,

    /// The new key after the change
    pub to_key: crate::key::Key,

    /// Section index where this occurs
    pub section_index: usize,
}

impl KeyChange {
    pub fn new(
        position: AbsolutePosition,
        from_key: Option<crate::key::Key>,
        to_key: crate::key::Key,
        section_index: usize,
    ) -> Self {
        Self {
            position,
            from_key,
            to_key,
            section_index,
        }
    }
}

/// Represents a time signature change at a specific position
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TimeSignatureChange {
    /// Position where the time signature change occurs
    pub position: AbsolutePosition,

    /// The new time signature
    pub time_signature: crate::time::TimeSignature,

    /// Section index where this occurs
    pub section_index: usize,
}

impl TimeSignatureChange {
    pub fn new(
        position: AbsolutePosition,
        time_signature: crate::time::TimeSignature,
        section_index: usize,
    ) -> Self {
        Self {
            position,
            time_signature,
            section_index,
        }
    }
}
