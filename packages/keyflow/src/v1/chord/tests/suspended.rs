//! Suspended chord test cases

use super::{ChordTestCase, semitones_from_intervals, root_interval_names, sequential_interval_names};
use crate::primitives::note::Note;
use crate::primitives::intervals::Interval;
use crate::chord::chord::Chord;

pub fn get_suspended_chords() -> Vec<ChordTestCase> {
    vec![
        ChordTestCase {
            name: "Suspended 4th".to_string(),
            chord: "Csus4".to_string(),
            constructor: || Chord::maj().sus4().with_root(Note::c()).to_string(),
            semitone_sequence: semitones_from_intervals(&[Interval::Unison, Interval::PerfectFourth, Interval::PerfectFifth]),
            root_intervals: root_interval_names(&[Interval::Unison, Interval::PerfectFourth, Interval::PerfectFifth]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::PerfectFourth, Interval::PerfectFifth]),
        },
        ChordTestCase {
            name: "Suspended 2nd".to_string(),
            chord: "Csus2".to_string(),
            constructor: || Chord::maj().sus2().with_root(Note::c()).to_string(),
            semitone_sequence: semitones_from_intervals(&[Interval::Unison, Interval::MajorSecond, Interval::PerfectFifth]),
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MajorSecond, Interval::PerfectFifth]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MajorSecond, Interval::PerfectFifth]),
        },
    ]
}
