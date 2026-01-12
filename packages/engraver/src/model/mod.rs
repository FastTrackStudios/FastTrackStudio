//! Score data model for music notation.
//!
//! This module defines the core data structures for representing musical scores,
//! from individual notes to complete multi-part compositions.

mod duration;
mod element;
mod measure;
mod note;
mod part;
mod pitch;
mod score;

pub use duration::{Duration, DurationKind};
pub use element::{KeySignature, MusicElement, TimeSignature};
pub use measure::Measure;
pub use note::{Accidental, Note, NoteHead, Stem};
pub use part::{Part, PartId};
pub use pitch::{Octave, Pitch, PitchClass};
pub use score::{LayoutSettings, Score, ScoreMetadata};

use serde::{Deserialize, Serialize};

/// Voice within a measure - a single melodic/rhythmic line.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Voice {
    /// Elements in this voice (notes, rests, chords, etc.)
    pub elements: Vec<MusicElement>,
}

impl Voice {
    /// Create a new empty voice.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Add an element to this voice.
    pub fn add(&mut self, element: MusicElement) {
        self.elements.push(element);
    }
}
