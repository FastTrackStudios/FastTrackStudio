//! Augmented chord test cases

use super::{ChordTestCase, semitones_from_intervals, root_interval_names, sequential_interval_names};
use crate::primitives::note::Note;
use crate::primitives::intervals::Interval;
use crate::chord::chord::Chord;

pub fn get_augmented_chords() -> Vec<ChordTestCase> {
    vec![
        ChordTestCase {
            name: "Augmented triad".to_string(),
            chord: "C+".to_string(),
            constructor: || Chord::aug().with_root(Note::c()).to_string(),
            semitone_sequence: semitones_from_intervals(&[Interval::Unison, Interval::MajorThird, Interval::AugmentedFifth]),
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::AugmentedFifth]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::AugmentedFifth]),
        },
    ]
}
