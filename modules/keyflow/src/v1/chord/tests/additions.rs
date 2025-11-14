//! Addition chord test cases

use super::{ChordTestCase, semitones_from_intervals, root_interval_names, sequential_interval_names};
use crate::primitives::note::Note;
use crate::primitives::intervals::Interval;
use crate::chord::chord::Chord;

pub fn get_addition_chords() -> Vec<ChordTestCase> {
    vec![
        ChordTestCase {
            name: "Major add 9".to_string(),
            chord: "Cadd9".to_string(),
            constructor: || Chord::maj().add_9().with_root(Note::c()).to_string(),
            semitone_sequence: semitones_from_intervals(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::Ninth]),
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::Ninth]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::Ninth]),
        },
    ]
}
