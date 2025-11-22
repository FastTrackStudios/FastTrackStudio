//! Sixth chord test cases

use super::{ChordTestCase, semitones_from_intervals, root_interval_names, sequential_interval_names};
use crate::primitives::note::Note;
use crate::primitives::intervals::Interval;
use crate::chord::chord::Chord;

pub fn get_sixth_chords() -> Vec<ChordTestCase> {
    vec![
        ChordTestCase {
            name: "Major 6th".to_string(),
            chord: "Cmaj6".to_string(),
            constructor: || Chord::maj6().with_root(Note::c()).to_string(),
            semitone_sequence: semitones_from_intervals(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MajorSixth]),
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MajorSixth]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MajorSixth]),
        },
    ]
}
