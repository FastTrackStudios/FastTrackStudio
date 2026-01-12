//! Measure (bar) representation.

use super::element::{KeySignature, TimeSignature};
use super::Voice;
use serde::{Deserialize, Serialize};

/// A measure (bar) of music.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Measure {
    /// Measure number (1-indexed)
    pub number: u32,
    /// Time signature (if changed at this measure)
    pub time_signature: Option<TimeSignature>,
    /// Key signature (if changed at this measure)
    pub key_signature: Option<KeySignature>,
    /// Voices in this measure (typically 1-4)
    pub voices: Vec<Voice>,
}

impl Measure {
    /// Create a new empty measure with the given number.
    #[must_use]
    pub fn new(number: u32) -> Self {
        Self {
            number,
            time_signature: None,
            key_signature: None,
            voices: vec![Voice::new()],
        }
    }

    /// Create a measure with time and key signature changes.
    #[must_use]
    pub fn with_signatures(
        number: u32,
        time_signature: Option<TimeSignature>,
        key_signature: Option<KeySignature>,
    ) -> Self {
        Self {
            number,
            time_signature,
            key_signature,
            voices: vec![Voice::new()],
        }
    }

    /// Get the primary voice (voice 0).
    #[must_use]
    pub fn primary_voice(&self) -> Option<&Voice> {
        self.voices.first()
    }

    /// Get a mutable reference to the primary voice.
    pub fn primary_voice_mut(&mut self) -> &mut Voice {
        if self.voices.is_empty() {
            self.voices.push(Voice::new());
        }
        &mut self.voices[0]
    }

    /// Add a voice to this measure.
    pub fn add_voice(&mut self, voice: Voice) {
        self.voices.push(voice);
    }

    /// Check if this measure is empty (no elements in any voice).
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.voices.iter().all(|v| v.elements.is_empty())
    }
}

impl Default for Measure {
    fn default() -> Self {
        Self::new(1)
    }
}
