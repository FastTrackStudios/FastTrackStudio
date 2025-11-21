//! Altered chord test cases

use super::{ChordTestCase, semitones_from_intervals, root_interval_names, sequential_interval_names};
use crate::primitives::note::Note;
use crate::primitives::intervals::Interval;
use crate::chord::chord::Chord;

pub fn get_altered_chords() -> Vec<ChordTestCase> {
    vec![
        ChordTestCase {
            name: "Altered dominant".to_string(),
            chord: "Calt".to_string(),
            constructor: || Chord::dom7().alt().with_root(Note::c()).to_string(),
            semitone_sequence: semitones_from_intervals(&[Interval::Unison, Interval::MajorThird, Interval::MinorSeventh, Interval::DiminishedFifth, Interval::AugmentedFifth, Interval::FlatNinth, Interval::SharpNinth, Interval::SharpEleventh, Interval::FlatThirteenth]),
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::MinorSeventh, Interval::DiminishedFifth, Interval::AugmentedFifth, Interval::FlatNinth, Interval::SharpNinth, Interval::SharpEleventh, Interval::FlatThirteenth]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::MinorSeventh, Interval::DiminishedFifth, Interval::AugmentedFifth, Interval::FlatNinth, Interval::SharpNinth, Interval::SharpEleventh, Interval::FlatThirteenth]),
        },
    ]
}
