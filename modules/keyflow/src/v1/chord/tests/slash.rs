//! Slash chord test cases

use super::{ChordTestCase, semitones_from_intervals, root_interval_names, sequential_interval_names};
use crate::primitives::note::Note;
use crate::primitives::intervals::Interval;
use crate::chord::chord::Chord;

pub fn get_slash_chords() -> Vec<ChordTestCase> {
    vec![
        ChordTestCase {
            name: "C major over E".to_string(),
            chord: "C/E".to_string(),
            constructor: || Chord::maj().slash_bass(Note::e()).with_root(Note::c()).to_string(),
            semitone_sequence: semitones_from_intervals(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth]),
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth]),
        },
    ]
}
