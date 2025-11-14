//! Omitted interval chord test cases

use super::{ChordTestCase, semitones_from_intervals, root_interval_names, sequential_interval_names};
use crate::primitives::note::Note;
use crate::primitives::intervals::Interval;
use crate::chord::chord::Chord;

pub fn get_omitted_chords() -> Vec<ChordTestCase> {
    vec![
        ChordTestCase {
            name: "Major omit 3rd".to_string(),
            chord: "C5".to_string(),
            constructor: || Chord::maj().omit_3().with_root(Note::c()).to_string(),
            semitone_sequence: semitones_from_intervals(&[Interval::Unison, Interval::PerfectFifth]),
            root_intervals: root_interval_names(&[Interval::Unison, Interval::PerfectFifth]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::PerfectFifth]),
        },
    ]
}
