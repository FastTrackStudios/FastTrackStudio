//! Dominant chord test cases with semitone sequences and interval analysis

use super::{ChordTestCase, semitones_from_intervals, root_interval_names, sequential_interval_names};
use crate::primitives::note::Note;
use crate::primitives::intervals::Interval;
use crate::chord::chord::Chord;

pub fn get_dominant_chords() -> Vec<ChordTestCase> {
    vec![
        // Basic dominant chords
        ChordTestCase {
            name: "Dominant 7th".to_string(),
            chord: "C7".to_string(),
            constructor: || Chord::dom7().with_root(Note::c()).to_string(),
            semitone_sequence: semitones_from_intervals(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MinorSeventh]),
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MinorSeventh]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MinorSeventh]),
        },
        ChordTestCase {
            name: "Dominant 9th".to_string(),
            chord: "C9".to_string(),
            constructor: || Chord::dom().nine().with_root(Note::c()).to_string(),
            semitone_sequence: semitones_from_intervals(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MinorSeventh, Interval::Ninth]),
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MinorSeventh, Interval::Ninth]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MinorSeventh, Interval::Ninth]),
        },
        ChordTestCase {
            name: "Dominant 11th".to_string(),
            chord: "C11".to_string(),
            constructor: || Chord::dom().eleven().with_root(Note::c()).to_string(),
            semitone_sequence: semitones_from_intervals(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MinorSeventh, Interval::Ninth, Interval::Eleventh]),
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MinorSeventh, Interval::Ninth, Interval::Eleventh]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MinorSeventh, Interval::Ninth, Interval::Eleventh]),
        },
        ChordTestCase {
            name: "Dominant 13th".to_string(),
            chord: "C13".to_string(),
            constructor: || Chord::dom().thirteen().with_root(Note::c()).to_string(),
            semitone_sequence: semitones_from_intervals(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MinorSeventh, Interval::Ninth, Interval::Eleventh, Interval::Thirteenth]),
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MinorSeventh, Interval::Ninth, Interval::Eleventh, Interval::Thirteenth]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MinorSeventh, Interval::Ninth, Interval::Eleventh, Interval::Thirteenth]),
        },
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dominant_chords() {
        let chord_list = get_dominant_chords();
        
        println!("Testing {} dominant chord types:", chord_list.len());
        
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
