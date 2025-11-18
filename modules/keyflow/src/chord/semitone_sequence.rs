//! Chord construction from semitone sequences
//!
//! Provides utilities to analyze semitone patterns and construct appropriate Chord objects

use super::{
    Alteration, Chord, ChordDegree, ChordFamily, ChordQuality, ExtensionQuality, Extensions,
    SuspendedType,
};
use crate::primitives::{Interval, RootNotation};
use std::collections::HashSet;

/// Error type for semitone sequence analysis
#[derive(Debug, Clone, PartialEq)]
pub enum SemitoneSequenceError {
    /// Empty semitone sequence
    EmptySequence,
    /// No root note (0) in sequence
    MissingRoot,
    /// Unrecognizable chord pattern
    UnrecognizedPattern(Vec<u8>),
    /// Ambiguous chord (multiple interpretations possible)
    AmbiguousChord(Vec<String>),
}

impl std::fmt::Display for SemitoneSequenceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EmptySequence => write!(f, "Empty semitone sequence"),
            Self::MissingRoot => write!(f, "Semitone sequence must contain root (0)"),
            Self::UnrecognizedPattern(seq) => write!(f, "Unrecognized chord pattern: {:?}", seq),
            Self::AmbiguousChord(options) => {
                write!(f, "Ambiguous chord, possibilities: {:?}", options)
            }
        }
    }
}

impl std::error::Error for SemitoneSequenceError {}

/// Result type for semitone sequence operations
pub type Result<T> = std::result::Result<T, SemitoneSequenceError>;

/// Analyze a semitone sequence and construct a Chord
///
/// # Arguments
/// * `semitones` - A slice of semitone values (0-11 or extended for octave info)
/// * `root` - The root notation for the chord
///
/// # Examples
/// ```
/// use keyflow::chord::{from_semitones, Chord};
/// use keyflow::primitives::{RootNotation, MusicalNote};
///
/// let root = RootNotation::from_note_name(MusicalNote::c());
/// // Major triad: C E G
/// let chord = from_semitones(&[0, 4, 7], root.clone()).unwrap();
///
/// // Dominant 7th: C E G Bb
/// let chord = from_semitones(&[0, 4, 7, 10], root.clone()).unwrap();
///
/// // Major 9th: C E G B D (with octave)
/// let chord = from_semitones(&[0, 4, 7, 11, 14], root).unwrap();
/// ```
pub fn from_semitones(semitones: &[u8], root: RootNotation) -> Result<Chord> {
    if semitones.is_empty() {
        return Err(SemitoneSequenceError::EmptySequence);
    }

    // Normalize to pitch classes (0-11) while preserving extensions
    let pitch_classes = normalize_semitones(semitones);

    if !pitch_classes.contains(&0) {
        return Err(SemitoneSequenceError::MissingRoot);
    }

    // Try to identify the chord structure
    let chord_info = analyze_chord_structure(&pitch_classes)?;

    // Build the chord
    build_chord(root, chord_info, semitones)
}

/// Normalize semitones, preserving both pitch classes and octave information
fn normalize_semitones(semitones: &[u8]) -> Vec<u8> {
    let mut normalized: Vec<u8> = semitones.to_vec();
    normalized.sort_unstable();
    normalized.dedup();
    normalized
}

/// Information extracted from analyzing a semitone pattern
#[derive(Debug, Clone)]
struct ChordInfo {
    quality: ChordQuality,
    family: Option<ChordFamily>,
    extensions: Extensions,
    alterations: Vec<Alteration>,
}

/// Analyze a normalized semitone sequence to determine chord structure
fn analyze_chord_structure(semitones: &[u8]) -> Result<ChordInfo> {
    // Get pitch classes only (mod 12)
    let pitch_classes: HashSet<u8> = semitones.iter().map(|s| s % 12).collect();

    // Extract the intervals relative to root
    let mut intervals: Vec<u8> = pitch_classes.iter().filter(|&&s| s != 0).copied().collect();
    intervals.sort_unstable();

    // Identify third (if present)
    let has_minor_third = pitch_classes.contains(&3);
    let has_major_third = pitch_classes.contains(&4);
    let has_sus2 = pitch_classes.contains(&2);
    let has_sus4 =
        pitch_classes.contains(&5) && !pitch_classes.contains(&4) && !pitch_classes.contains(&3);

    // Identify fifth
    let has_perfect_fifth = pitch_classes.contains(&7);
    let has_dim_fifth = pitch_classes.contains(&6);
    let has_aug_fifth = pitch_classes.contains(&8);

    // Identify seventh
    let has_dim_seventh = pitch_classes.contains(&9);
    let has_minor_seventh = pitch_classes.contains(&10);
    let has_major_seventh = pitch_classes.contains(&11);

    // Identify extensions (9, 11, 13)
    let has_minor_ninth = semitones.contains(&13);
    let has_ninth = semitones.contains(&14);
    let has_sharp_ninth = semitones.contains(&15);
    let has_eleventh = semitones.contains(&17);
    let has_sharp_eleventh = semitones.contains(&18);
    let has_flat_thirteenth = semitones.contains(&20);
    let has_thirteenth = semitones.contains(&21);

    // Determine quality
    let quality = if has_sus2 && !has_major_third && !has_minor_third {
        ChordQuality::Suspended(SuspendedType::Second)
    } else if has_sus4 {
        ChordQuality::Suspended(SuspendedType::Fourth)
    } else if !has_major_third && !has_minor_third && has_perfect_fifth {
        ChordQuality::Power
    } else if has_dim_fifth && has_minor_third && !has_aug_fifth && !has_perfect_fifth {
        ChordQuality::Diminished
    } else if has_aug_fifth
        && has_major_third
        && !has_perfect_fifth
        && !has_minor_seventh
        && !has_major_seventh
    {
        // Only treat as augmented if it's a pure triad without seventh
        ChordQuality::Augmented
    } else if has_minor_third {
        ChordQuality::Minor
    } else if has_major_third {
        ChordQuality::Major
    } else {
        return Err(SemitoneSequenceError::UnrecognizedPattern(intervals));
    };

    // Determine family (seventh type)
    let family = match quality {
        ChordQuality::Major => {
            if has_major_seventh {
                Some(ChordFamily::Major7)
            } else if has_minor_seventh {
                Some(ChordFamily::Dominant7)
            } else if has_dim_seventh {
                Some(ChordFamily::Dominant7) // Rare but possible
            } else {
                None
            }
        }
        ChordQuality::Minor => {
            if has_major_seventh {
                Some(ChordFamily::MinorMajor7)
            } else if has_minor_seventh {
                Some(ChordFamily::Minor7)
            } else if has_dim_seventh {
                Some(ChordFamily::Minor7) // Rare
            } else {
                None
            }
        }
        ChordQuality::Diminished => {
            if has_dim_seventh {
                Some(ChordFamily::FullyDiminished)
            } else if has_minor_seventh {
                Some(ChordFamily::HalfDiminished)
            } else {
                None
            }
        }
        ChordQuality::Augmented => {
            if has_major_seventh {
                Some(ChordFamily::Major7) // Aug maj7
            } else if has_minor_seventh {
                Some(ChordFamily::Dominant7) // Aug 7
            } else {
                None
            }
        }
        _ => {
            if has_minor_seventh {
                Some(ChordFamily::Dominant7)
            } else {
                None
            }
        }
    };

    // Determine extensions
    let mut extensions = Extensions::none();

    if has_thirteenth {
        extensions.thirteenth = Some(ExtensionQuality::Natural);
    }

    if has_eleventh {
        extensions.eleventh = Some(ExtensionQuality::Natural);
    }

    if has_ninth {
        extensions.ninth = Some(ExtensionQuality::Natural);
    }

    // Handle alterations
    let mut alterations = Vec::new();

    // Altered fifth (only if not part of the base quality)
    if has_dim_fifth && quality != ChordQuality::Diminished {
        alterations.push(Alteration {
            degree: ChordDegree::Fifth,
            interval: Interval::DiminishedFifth,
        });
    }

    if has_aug_fifth && quality != ChordQuality::Augmented {
        alterations.push(Alteration {
            degree: ChordDegree::Fifth,
            interval: Interval::AugmentedFifth,
        });
    }

    // Altered ninths
    if has_minor_ninth {
        alterations.push(Alteration {
            degree: ChordDegree::Ninth,
            interval: Interval::FlatNinth,
        });
    }

    if has_sharp_ninth {
        alterations.push(Alteration {
            degree: ChordDegree::Ninth,
            interval: Interval::SharpNinth,
        });
    }

    // Altered eleventh
    if has_sharp_eleventh {
        alterations.push(Alteration {
            degree: ChordDegree::Eleventh,
            interval: Interval::SharpEleventh,
        });
    }

    // Altered thirteenth
    if has_flat_thirteenth {
        alterations.push(Alteration {
            degree: ChordDegree::Thirteenth,
            interval: Interval::FlatThirteenth,
        });
    }

    Ok(ChordInfo {
        quality,
        family,
        extensions,
        alterations,
    })
}

/// Build a Chord from the analyzed information
fn build_chord(root: RootNotation, info: ChordInfo, original_semitones: &[u8]) -> Result<Chord> {
    let mut chord = if let Some(family) = info.family {
        Chord::with_family(root, info.quality, family)
    } else {
        Chord::new(root, info.quality)
    };

    // Apply extensions
    chord.extensions = info.extensions;

    // Apply alterations
    chord.alterations = info.alterations;

    // Recompute intervals with the new extensions and alterations
    chord.compute_intervals();
    chord.normalize();

    // Set origin to indicate this was created from semitones
    chord.origin = format!("from_semitones({:?})", original_semitones);

    Ok(chord)
}

/// Get the chord quality name from a semitone sequence
///
/// This is a convenience function that returns just the quality name without
/// constructing a full Chord object.
///
/// # Examples
/// ```
/// use keyflow::chord::quality_from_semitones;
///
/// assert_eq!(quality_from_semitones(&[0, 4, 7]), Some("Major"));
/// assert_eq!(quality_from_semitones(&[0, 3, 7]), Some("Minor"));
/// assert_eq!(quality_from_semitones(&[0, 3, 6]), Some("Diminished"));
/// assert_eq!(quality_from_semitones(&[0, 4, 8]), Some("Augmented"));
/// ```
pub fn quality_from_semitones(semitones: &[u8]) -> Option<&'static str> {
    let pitch_classes: HashSet<u8> = semitones.iter().map(|s| s % 12).collect();

    if !pitch_classes.contains(&0) {
        return None;
    }

    let has_minor_third = pitch_classes.contains(&3);
    let has_major_third = pitch_classes.contains(&4);
    let has_sus2 = pitch_classes.contains(&2);
    let has_sus4 =
        pitch_classes.contains(&5) && !pitch_classes.contains(&4) && !pitch_classes.contains(&3);
    let has_perfect_fifth = pitch_classes.contains(&7);
    let has_dim_fifth = pitch_classes.contains(&6);
    let has_aug_fifth = pitch_classes.contains(&8);

    if has_sus2 && !has_major_third && !has_minor_third {
        Some("Sus2")
    } else if has_sus4 {
        Some("Sus4")
    } else if !has_major_third && !has_minor_third && has_perfect_fifth {
        Some("Power")
    } else if has_dim_fifth && has_minor_third && !has_aug_fifth && !has_perfect_fifth {
        Some("Diminished")
    } else if has_aug_fifth && has_major_third && !has_perfect_fifth {
        Some("Augmented")
    } else if has_minor_third {
        Some("Minor")
    } else if has_major_third {
        Some("Major")
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::primitives::MusicalNote;

    fn c_root() -> RootNotation {
        let c_note = MusicalNote::from_string("C").unwrap();
        RootNotation::from_note_name(c_note)
    }

    #[test]
    fn test_major_triad() {
        let chord = from_semitones(&[0, 4, 7], c_root()).unwrap();
        assert_eq!(chord.quality, ChordQuality::Major);
        assert_eq!(chord.family, None);
        assert_eq!(chord.semitone_sequence(), vec![0, 4, 7]);
    }

    #[test]
    fn test_minor_triad() {
        let chord = from_semitones(&[0, 3, 7], c_root()).unwrap();
        assert_eq!(chord.quality, ChordQuality::Minor);
        assert_eq!(chord.family, None);
        assert_eq!(chord.semitone_sequence(), vec![0, 3, 7]);
    }

    #[test]
    fn test_diminished_triad() {
        let chord = from_semitones(&[0, 3, 6], c_root()).unwrap();
        assert_eq!(chord.quality, ChordQuality::Diminished);
        assert_eq!(chord.family, None);
        assert_eq!(chord.semitone_sequence(), vec![0, 3, 6]);
    }

    #[test]
    fn test_augmented_triad() {
        let chord = from_semitones(&[0, 4, 8], c_root()).unwrap();
        assert_eq!(chord.quality, ChordQuality::Augmented);
        assert_eq!(chord.family, None);
        assert_eq!(chord.semitone_sequence(), vec![0, 4, 8]);
    }

    #[test]
    fn test_suspended_2() {
        let chord = from_semitones(&[0, 2, 7], c_root()).unwrap();
        assert_eq!(
            chord.quality,
            ChordQuality::Suspended(SuspendedType::Second)
        );
    }

    #[test]
    fn test_suspended_4() {
        let chord = from_semitones(&[0, 5, 7], c_root()).unwrap();
        assert_eq!(
            chord.quality,
            ChordQuality::Suspended(SuspendedType::Fourth)
        );
    }

    #[test]
    fn test_power_chord() {
        let chord = from_semitones(&[0, 7], c_root()).unwrap();
        assert_eq!(chord.quality, ChordQuality::Power);
    }

    #[test]
    fn test_dominant_seventh() {
        let chord = from_semitones(&[0, 4, 7, 10], c_root()).unwrap();
        assert_eq!(chord.quality, ChordQuality::Major);
        assert_eq!(chord.family, Some(ChordFamily::Dominant7));
        assert_eq!(chord.semitone_sequence(), vec![0, 4, 7, 10]);
    }

    #[test]
    fn test_major_seventh() {
        let chord = from_semitones(&[0, 4, 7, 11], c_root()).unwrap();
        assert_eq!(chord.quality, ChordQuality::Major);
        assert_eq!(chord.family, Some(ChordFamily::Major7));
        assert_eq!(chord.semitone_sequence(), vec![0, 4, 7, 11]);
    }

    #[test]
    fn test_minor_seventh() {
        let chord = from_semitones(&[0, 3, 7, 10], c_root()).unwrap();
        assert_eq!(chord.quality, ChordQuality::Minor);
        assert_eq!(chord.family, Some(ChordFamily::Minor7));
        assert_eq!(chord.semitone_sequence(), vec![0, 3, 7, 10]);
    }

    #[test]
    fn test_minor_major_seventh() {
        let chord = from_semitones(&[0, 3, 7, 11], c_root()).unwrap();
        assert_eq!(chord.quality, ChordQuality::Minor);
        assert_eq!(chord.family, Some(ChordFamily::MinorMajor7));
        assert_eq!(chord.semitone_sequence(), vec![0, 3, 7, 11]);
    }

    #[test]
    fn test_half_diminished_seventh() {
        let chord = from_semitones(&[0, 3, 6, 10], c_root()).unwrap();
        assert_eq!(chord.quality, ChordQuality::Diminished);
        assert_eq!(chord.family, Some(ChordFamily::HalfDiminished));
        assert_eq!(chord.semitone_sequence(), vec![0, 3, 6, 10]);
    }

    #[test]
    fn test_diminished_seventh() {
        let chord = from_semitones(&[0, 3, 6, 9], c_root()).unwrap();
        assert_eq!(chord.quality, ChordQuality::Diminished);
        assert_eq!(chord.family, Some(ChordFamily::FullyDiminished));
        assert_eq!(chord.semitone_sequence(), vec![0, 3, 6, 9]);
    }

    #[test]
    fn test_dominant_ninth() {
        let chord = from_semitones(&[0, 4, 7, 10, 14], c_root()).unwrap();
        assert_eq!(chord.quality, ChordQuality::Major);
        assert_eq!(chord.family, Some(ChordFamily::Dominant7));
        assert!(chord.extensions.ninth.is_some());
        assert_eq!(chord.semitone_sequence(), vec![0, 4, 7, 10, 14]);
    }

    #[test]
    fn test_major_ninth() {
        let chord = from_semitones(&[0, 4, 7, 11, 14], c_root()).unwrap();
        assert_eq!(chord.quality, ChordQuality::Major);
        assert_eq!(chord.family, Some(ChordFamily::Major7));
        assert!(chord.extensions.ninth.is_some());
        assert_eq!(chord.semitone_sequence(), vec![0, 4, 7, 11, 14]);
    }

    #[test]
    fn test_dominant_thirteenth() {
        let chord = from_semitones(&[0, 4, 7, 10, 14, 17, 21], c_root()).unwrap();
        assert_eq!(chord.quality, ChordQuality::Major);
        assert_eq!(chord.family, Some(ChordFamily::Dominant7));
        assert!(chord.extensions.ninth.is_some());
        assert!(chord.extensions.eleventh.is_some());
        assert!(chord.extensions.thirteenth.is_some());
    }

    #[test]
    fn test_altered_chord_b5() {
        let chord = from_semitones(&[0, 4, 6, 10], c_root()).unwrap();
        assert_eq!(chord.quality, ChordQuality::Major);
        assert_eq!(chord.family, Some(ChordFamily::Dominant7));
        assert_eq!(chord.alterations.len(), 1);
        assert_eq!(chord.alterations[0].degree, ChordDegree::Fifth);
    }

    #[test]
    fn test_altered_chord_sharp5() {
        let chord = from_semitones(&[0, 4, 8, 10], c_root()).unwrap();
        assert_eq!(chord.quality, ChordQuality::Major);
        assert_eq!(chord.family, Some(ChordFamily::Dominant7));
        assert_eq!(chord.alterations.len(), 1);
        assert_eq!(chord.alterations[0].degree, ChordDegree::Fifth);
    }

    #[test]
    fn test_quality_from_semitones() {
        assert_eq!(quality_from_semitones(&[0, 4, 7]), Some("Major"));
        assert_eq!(quality_from_semitones(&[0, 3, 7]), Some("Minor"));
        assert_eq!(quality_from_semitones(&[0, 3, 6]), Some("Diminished"));
        assert_eq!(quality_from_semitones(&[0, 4, 8]), Some("Augmented"));
        assert_eq!(quality_from_semitones(&[0, 7]), Some("Power"));
    }

    #[test]
    fn test_empty_sequence() {
        let result = from_semitones(&[], c_root());
        assert!(matches!(result, Err(SemitoneSequenceError::EmptySequence)));
    }

    #[test]
    fn test_missing_root() {
        let result = from_semitones(&[4, 7], c_root());
        assert!(matches!(result, Err(SemitoneSequenceError::MissingRoot)));
    }

    #[test]
    fn test_unordered_input() {
        // Should handle unordered input
        let chord = from_semitones(&[7, 0, 4], c_root()).unwrap();
        assert_eq!(chord.quality, ChordQuality::Major);
        assert_eq!(chord.semitone_sequence(), vec![0, 4, 7]);
    }

    #[test]
    fn test_with_duplicates() {
        // Should handle duplicates
        let chord = from_semitones(&[0, 4, 4, 7, 0], c_root()).unwrap();
        assert_eq!(chord.quality, ChordQuality::Major);
        assert_eq!(chord.semitone_sequence(), vec![0, 4, 7]);
    }

    #[test]
    fn test_round_trip_major_triad() {
        // Test: semitones -> chord -> semitones produces same result
        let original_semitones = vec![0, 4, 7];
        let chord = from_semitones(&original_semitones, c_root()).unwrap();
        let recovered_semitones = chord.semitone_sequence();
        assert_eq!(recovered_semitones, original_semitones);
    }

    #[test]
    fn test_round_trip_minor_seventh() {
        let original_semitones = vec![0, 3, 7, 10];
        let chord = from_semitones(&original_semitones, c_root()).unwrap();
        let recovered_semitones = chord.semitone_sequence();
        assert_eq!(recovered_semitones, original_semitones);
    }

    #[test]
    fn test_round_trip_dominant_ninth() {
        let original_semitones = vec![0, 4, 7, 10, 14];
        let chord = from_semitones(&original_semitones, c_root()).unwrap();
        let recovered_semitones = chord.semitone_sequence();
        assert_eq!(recovered_semitones, original_semitones);
    }

    #[test]
    fn test_round_trip_dominant_thirteenth() {
        let original_semitones = vec![0, 4, 7, 10, 14, 17, 21];
        let chord = from_semitones(&original_semitones, c_root()).unwrap();
        let recovered_semitones = chord.semitone_sequence();
        assert_eq!(recovered_semitones, original_semitones);
    }

    #[test]
    fn test_round_trip_altered_chord() {
        // C7#5: C E G# Bb
        let original_semitones = vec![0, 4, 8, 10];
        let chord = from_semitones(&original_semitones, c_root()).unwrap();
        let recovered_semitones = chord.semitone_sequence();
        assert_eq!(recovered_semitones, original_semitones);
    }

    #[test]
    fn test_round_trip_diminished_seventh() {
        let original_semitones = vec![0, 3, 6, 9];
        let chord = from_semitones(&original_semitones, c_root()).unwrap();
        let recovered_semitones = chord.semitone_sequence();
        assert_eq!(recovered_semitones, original_semitones);
    }

    #[test]
    fn test_round_trip_suspended() {
        // Csus4
        let original_semitones = vec![0, 5, 7];
        let chord = from_semitones(&original_semitones, c_root()).unwrap();
        let recovered_semitones = chord.semitone_sequence();
        assert_eq!(recovered_semitones, original_semitones);
    }

    #[test]
    fn test_round_trip_augmented() {
        // Caug (pure triad)
        let original_semitones = vec![0, 4, 8];
        let chord = from_semitones(&original_semitones, c_root()).unwrap();
        let recovered_semitones = chord.semitone_sequence();
        assert_eq!(recovered_semitones, original_semitones);
    }

    #[test]
    fn test_round_trip_with_unordered_input() {
        // Input is unordered, but output should be sorted
        let unordered_input = vec![7, 0, 10, 4];
        let expected_output = vec![0, 4, 7, 10];

        let chord = from_semitones(&unordered_input, c_root()).unwrap();
        let recovered_semitones = chord.semitone_sequence();

        // Should match the sorted version
        assert_eq!(recovered_semitones, expected_output);
    }
}
