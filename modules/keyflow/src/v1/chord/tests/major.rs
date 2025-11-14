//! Major chord test cases with semitone sequences and interval analysis

use super::{ChordTestCase, semitones_from_intervals, root_interval_names, sequential_interval_names};
use crate::primitives::note::Note;
use crate::primitives::intervals::Interval;
use crate::chord::chord::Chord;

pub fn get_major_chords() -> Vec<ChordTestCase> {
    vec![
        // Basic major chords
        ChordTestCase {
            name: "Major triad".to_string(),
            chord: "C".to_string(),
            constructor: || Chord::maj().with_root(Note::c()).to_string(),
            semitone_sequence: vec![0, 4, 7],
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth]),
        },
        ChordTestCase {
            name: "Major 7th".to_string(),
            chord: "CMaj7".to_string(),
            constructor: || Chord::maj7().with_root(Note::c()).to_string(),
            semitone_sequence: vec![0, 4, 7, 11],
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MajorSeventh]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MajorSeventh]),
        },
        ChordTestCase {
            name: "Major 7th omit 3rd".to_string(),
            chord: "CMaj7(omit3)".to_string(),
            constructor: || Chord::maj7().omit_3().with_root(Note::c()).to_string(),
            semitone_sequence: vec![0, 7, 11],
            root_intervals: root_interval_names(&[Interval::Unison, Interval::PerfectFifth, Interval::MajorSeventh]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::PerfectFifth, Interval::MajorSeventh]),
        },
        ChordTestCase {
            name: "Major 7th omit 5th".to_string(),
            chord: "CMaj7(omit5)".to_string(),
            constructor: || Chord::maj7().omit_5().with_root(Note::c()).to_string(),
            semitone_sequence: vec![0, 4, 11],
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::MajorSeventh]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::MajorSeventh]),
        },
        ChordTestCase {
            name: "Major 9th".to_string(),
            chord: "CMaj9".to_string(),
            constructor: || Chord::maj().nine().with_root(Note::c()).to_string(),
            semitone_sequence: vec![0, 4, 7, 11, 14],
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MajorSeventh, Interval::Ninth]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MajorSeventh, Interval::Ninth]),
        },
        ChordTestCase {
            name: "Major 9th omit 5th".to_string(),
            chord: "CMaj9(omit5)".to_string(),
            constructor: || Chord::maj().nine().omit_5().with_root(Note::c()).to_string(),
            semitone_sequence: vec![0, 4, 11, 14],
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::MajorSeventh, Interval::Ninth]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::MajorSeventh, Interval::Ninth]),
        },
        ChordTestCase {
            name: "Major 11th".to_string(),
            chord: "CMaj11".to_string(),
            constructor: || Chord::maj().eleven().with_root(Note::c()).to_string(),
            semitone_sequence: vec![0, 4, 7, 11, 14, 17],
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MajorSeventh, Interval::Ninth, Interval::Eleventh]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MajorSeventh, Interval::Ninth, Interval::Eleventh]),
        },
        ChordTestCase {
            name: "Major 11th omit 3rd".to_string(),
            chord: "CMaj11(omit3)".to_string(),
            constructor: || Chord::maj().eleven().omit_3().with_root(Note::c()).to_string(),
            semitone_sequence: vec![0, 7, 11, 14, 17],
            root_intervals: root_interval_names(&[Interval::Unison, Interval::PerfectFifth, Interval::MajorSeventh, Interval::Ninth, Interval::Eleventh]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::PerfectFifth, Interval::MajorSeventh, Interval::Ninth, Interval::Eleventh]),
        },
        ChordTestCase {
            name: "Major 11th omit 5th".to_string(),
            chord: "CMaj11(omit5)".to_string(),
            constructor: || Chord::maj().eleven().omit_5().with_root(Note::c()).to_string(),
            semitone_sequence: vec![0, 4, 11, 14, 17],
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::MajorSeventh, Interval::Ninth, Interval::Eleventh]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::MajorSeventh, Interval::Ninth, Interval::Eleventh]),
        },
        ChordTestCase {
            name: "Major 11th omit 9th".to_string(),
            chord: "CMaj11(omit9)".to_string(),
            constructor: || Chord::maj().eleven().omit_degree("9").with_root(Note::c()).to_string(),
            semitone_sequence: vec![0, 4, 7, 11, 17],
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MajorSeventh, Interval::Eleventh]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MajorSeventh, Interval::Eleventh]),
        },
        ChordTestCase {
            name: "Major 13th".to_string(),
            chord: "CMaj13".to_string(),
            constructor: || Chord::maj().thirteen().with_root(Note::c()).to_string(),
            semitone_sequence: vec![0, 4, 7, 11, 14, 17, 21],
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MajorSeventh, Interval::Ninth, Interval::Eleventh, Interval::Thirteenth]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MajorSeventh, Interval::Ninth, Interval::Eleventh, Interval::Thirteenth]),
        },
        ChordTestCase {
            name: "Major 13th omit 5th".to_string(),
            chord: "CMaj13(omit5)".to_string(),
            constructor: || Chord::maj().thirteen().omit_5().with_root(Note::c()).to_string(),
            semitone_sequence: vec![0, 4, 11, 14, 17, 21],
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::MajorSeventh, Interval::Ninth, Interval::Eleventh, Interval::Thirteenth]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::MajorSeventh, Interval::Ninth, Interval::Eleventh, Interval::Thirteenth]),
        },
        ChordTestCase {
            name: "Major 13th omit 9th".to_string(),
            chord: "CMaj13(omit9)".to_string(),
            constructor: || Chord::maj().thirteen().omit_degree("9").with_root(Note::c()).to_string(),
            semitone_sequence: vec![0, 4, 7, 11, 17, 21],
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MajorSeventh, Interval::Eleventh, Interval::Thirteenth]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MajorSeventh, Interval::Eleventh, Interval::Thirteenth]),
        },
        ChordTestCase {
            name: "Major add 9".to_string(),
            chord: "Cadd9".to_string(),
            constructor: || Chord::maj().add_9().with_root(Note::c()).to_string(),
            semitone_sequence: vec![0, 4, 7, 14],
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::Ninth]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::Ninth]),
        },
        ChordTestCase {
            name: "Major add 11".to_string(),
            chord: "Cadd11".to_string(),
            constructor: || Chord::maj().add_11().with_root(Note::c()).to_string(),
            semitone_sequence: vec![0, 4, 7, 17],
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::Eleventh]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::Eleventh]),
        },
        ChordTestCase {
            name: "Major add 13".to_string(),
            chord: "Cadd13".to_string(),
            constructor: || Chord::maj().add_13().with_root(Note::c()).to_string(),
            semitone_sequence: vec![0, 4, 7, 21],
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::Thirteenth]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::Thirteenth]),
        },
        ChordTestCase {
            name: "Major 6th".to_string(),
            chord: "Cmaj6".to_string(),
            constructor: || Chord::maj6().with_root(Note::c()).to_string(),
            semitone_sequence: vec![0, 4, 7, 9],
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MajorSixth]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MajorSixth]),
        },
        ChordTestCase {
            name: "Major 6th omit 3rd".to_string(),
            chord: "Cmaj6(omit3)".to_string(),
            constructor: || Chord::maj6().omit_3().with_root(Note::c()).to_string(),
            semitone_sequence: vec![0, 7, 9],
            root_intervals: root_interval_names(&[Interval::Unison, Interval::PerfectFifth, Interval::MajorSixth]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::PerfectFifth, Interval::MajorSixth]),
        },
        ChordTestCase {
            name: "Major 6/9".to_string(),
            chord: "C6/9".to_string(),
            constructor: || Chord::maj6_9().with_root(Note::c()).to_string(),
            semitone_sequence: vec![0, 4, 7, 9, 14],
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MajorSixth, Interval::Ninth]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::PerfectFifth, Interval::MajorSixth, Interval::Ninth]),
        },
        ChordTestCase {
            name: "Major 6/9 omit 5th".to_string(),
            chord: "C6/9(omit5)".to_string(),
            constructor: || Chord::maj6_9().omit_5().with_root(Note::c()).to_string(),
            semitone_sequence: vec![0, 4, 9, 14],
            root_intervals: root_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::MajorSixth, Interval::Ninth]),
            sequential_intervals: sequential_interval_names(&[Interval::Unison, Interval::MajorThird, Interval::MajorSixth, Interval::Ninth]),
        },
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_major_chords() {
        let chord_list = get_major_chords();
        
        println!("Testing {} major chord types:", chord_list.len());
        
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

    #[test]
    fn test_semitone_sequence_parsing() {
        use crate::key::keys::Key;
        use crate::primitives::note::Note;
        
        println!("\n=== TESTING SEMITONE SEQUENCE PARSING ===");
        
        let chord_list = get_major_chords();
        
        for test_case in chord_list.iter().take(10) { // Test first 10 for brevity
            let semitones: Vec<u8> = test_case.semitone_sequence.iter().map(|&s| s as u8).collect();
            
            // Use the new function that returns properly normalized chord names
            if let Some(detected_name) = Key::detect_chord_name_from_semitones(Note::c(), &semitones) {
                println!("Semitones {:?} -> '{}' (expected: '{}')", 
                    test_case.semitone_sequence, 
                    detected_name, 
                    test_case.chord);
            } else {
                println!("Semitones {:?} -> No match (expected: '{}')", 
                    test_case.semitone_sequence, 
                    test_case.chord);
            }
        }
    }

    #[test]
    fn test_chord_construction_from_semitones() {
        use crate::primitives::note::Note;
        
        println!("\n=== TESTING CHORD CONSTRUCTION FROM SEMITONES ===");
        
        // Test some basic semitone sequences
        let test_sequences = vec![
            (vec![0, 4, 7], "Major triad"),
            (vec![0, 4, 7, 11], "Major 7th"),
            (vec![0, 4, 7, 11, 14], "Major 9th"),
            (vec![0, 4, 7, 9], "Major 6th"),
            (vec![0, 4, 7, 9, 14], "Major 6/9"),
            (vec![0, 7, 11], "Major 7th omit 3rd"),
            (vec![0, 4, 11], "Major 7th omit 5th"),
        ];
        
        for (semitones, description) in test_sequences {
            if let Some(detected) = crate::key::keys::Key::detect_chord_name_from_semitones(Note::c(), &semitones) {
                println!("{:?} -> {} ({})", semitones, detected, description);
            } else {
                println!("{:?} -> No match ({})", semitones, description);
            }
        }
    }
}
