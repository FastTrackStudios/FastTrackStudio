//! Diminished chord test cases

use super::{ChordTestCase, semitones_from_intervals, root_interval_names, sequential_interval_names};
use crate::primitives::note::Note;
use crate::primitives::intervals::Interval;
use crate::chord::chord::Chord;

pub fn get_diminished_chords() -> Vec<ChordTestCase> {
    vec![
        ChordTestCase {
            name: "Diminished triad".to_string(),
            chord: "Cdim".to_string(),
            constructor: || Chord::dim().with_root(Note::c()).to_string(),
            semitone_sequence: semitones_from_intervals(&[Interval::Unison, Interval::MinorThird, Interval::DiminishedFifth]),
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MinorThird, Interval::DiminishedFifth]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MinorThird, Interval::DiminishedFifth]),
        },
        ChordTestCase {
            name: "Diminished 7th".to_string(),
            chord: "Cdim7".to_string(),
            constructor: || Chord::dim7().with_root(Note::c()).to_string(),
            semitone_sequence: semitones_from_intervals(&[Interval::Unison, Interval::MinorThird, Interval::DiminishedFifth, Interval::DiminishedSeventh]),
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MinorThird, Interval::DiminishedFifth, Interval::DiminishedSeventh]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MinorThird, Interval::DiminishedFifth, Interval::DiminishedSeventh]),
        },
    ]
}
