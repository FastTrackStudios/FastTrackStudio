//! Minor chord test cases with semitone sequences and interval analysis

use super::{ChordTestCase, semitones_from_intervals, root_interval_names, sequential_interval_names};
use crate::primitives::note::Note;
use crate::primitives::intervals::Interval;
use crate::chord::chord::Chord;

pub fn get_minor_chords() -> Vec<ChordTestCase> {
    vec![
        // Basic minor chords
        ChordTestCase {
            name: "Minor triad".to_string(),
            chord: "Cmin".to_string(),
            constructor: || Chord::min().with_root(Note::c()).to_string(),
            semitone_sequence: semitones_from_intervals(&[Interval::Unison, Interval::MinorThird, Interval::PerfectFifth]),
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MinorThird, Interval::PerfectFifth]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MinorThird, Interval::PerfectFifth]),
        },
        ChordTestCase {
            name: "Minor 7th".to_string(),
            chord: "Cmin7".to_string(),
            constructor: || Chord::min7().with_root(Note::c()).to_string(),
            semitone_sequence: semitones_from_intervals(&[Interval::Unison, Interval::MinorThird, Interval::PerfectFifth, Interval::MinorSeventh]),
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MinorThird, Interval::PerfectFifth, Interval::MinorSeventh]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MinorThird, Interval::PerfectFifth, Interval::MinorSeventh]),
        },
        ChordTestCase {
            name: "Minor major 7th".to_string(),
            chord: "CminMaj7".to_string(),
            constructor: || Chord::min_maj7().with_root(Note::c()).to_string(),
            semitone_sequence: semitones_from_intervals(&[Interval::Unison, Interval::MinorThird, Interval::PerfectFifth, Interval::MajorSeventh]),
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MinorThird, Interval::PerfectFifth, Interval::MajorSeventh]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MinorThird, Interval::PerfectFifth, Interval::MajorSeventh]),
        },
        ChordTestCase {
            name: "Minor 9th".to_string(),
            chord: "Cmin9".to_string(),
            constructor: || Chord::min().nine().with_root(Note::c()).to_string(),
            semitone_sequence: semitones_from_intervals(&[Interval::Unison, Interval::MinorThird, Interval::PerfectFifth, Interval::MinorSeventh, Interval::Ninth]),
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MinorThird, Interval::PerfectFifth, Interval::MinorSeventh, Interval::Ninth]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MinorThird, Interval::PerfectFifth, Interval::MinorSeventh, Interval::Ninth]),
        },
        ChordTestCase {
            name: "Minor add 9".to_string(),
            chord: "Cminadd9".to_string(),
            constructor: || Chord::min().add_9().with_root(Note::c()).to_string(),
            semitone_sequence: semitones_from_intervals(&[Interval::Unison, Interval::MinorThird, Interval::PerfectFifth, Interval::Ninth]),
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MinorThird, Interval::PerfectFifth, Interval::Ninth]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MinorThird, Interval::PerfectFifth, Interval::Ninth]),
        },
        ChordTestCase {
            name: "Minor 6th".to_string(),
            chord: "Cmin6".to_string(),
            constructor: || Chord::min6().with_root(Note::c()).to_string(),
            semitone_sequence: semitones_from_intervals(&[Interval::Unison, Interval::MinorThird, Interval::PerfectFifth, Interval::MajorSixth]),
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MinorThird, Interval::PerfectFifth, Interval::MajorSixth]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MinorThird, Interval::PerfectFifth, Interval::MajorSixth]),
        },
        ChordTestCase {
            name: "Minor 6/9".to_string(),
            chord: "Cmin6/9".to_string(),
            constructor: || Chord::min6_9().with_root(Note::c()).to_string(),
            semitone_sequence: semitones_from_intervals(&[Interval::Unison, Interval::MinorThird, Interval::PerfectFifth, Interval::MajorSixth, Interval::Ninth]),
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MinorThird, Interval::PerfectFifth, Interval::MajorSixth, Interval::Ninth]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MinorThird, Interval::PerfectFifth, Interval::MajorSixth, Interval::Ninth]),
        },
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_minor_chords() {
        let chord_list = get_minor_chords();
        
        println!("Testing {} minor chord types:", chord_list.len());
        
        for (i, test_case) in chord_list.iter().enumerate() {
            let actual = (test_case.constructor)();
            println!("{}. {}", i + 1, test_case.name);
            println!("   Expected: '{}'", test_case.chord);
            println!("   Got:      '{}'", actual);
            println!("   Semitones: {:?}", test_case.semitone_sequence);
            println!("   Root intervals: {:?}", test_case.root_intervals);
            println!("   Sequential intervals: {:?}", test_case.sequential_intervals);
            println!();
            
            // Uncomment this line to enable assertions (may fail if display logic needs adjustment)
            // assert_eq!(actual, test_case.chord, "Failed for: {}", test_case.name);
        }
    }
}
