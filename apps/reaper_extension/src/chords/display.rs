//! Chord display format conversion using keyflow

use keyflow::{Chord, Key, RootNotation, RomanCase};
use tracing::debug;
use crate::chords::types::{ChordAnalysis, ChordData, ChordDisplayFormat};

/// Convert chord data to all display formats
pub fn format_chord_analysis(
    chord_data: &ChordData,
    key: Option<&Key>,
) -> ChordAnalysis {
    // Standard chord name (always available)
    let name = format!("{}", chord_data.chord);

    // Nashville and Roman require key context
    let (nashville, roman) = if let Some(key) = key {
        (
            format_chord_nashville(&chord_data.chord, &chord_data.root_note, key),
            format_chord_roman(&chord_data.chord, &chord_data.root_note, key),
        )
    } else {
        (None, None)
    };

    ChordAnalysis {
        data: chord_data.clone(),
        name,
        nashville,
        roman,
    }
}

/// Format chord in Nashville Number System (e.g., "1maj7", "1/3", "5m7")
fn format_chord_nashville(
    chord: &Chord,
    root_note: &keyflow::MusicalNote,
    key: &Key,
) -> Option<String> {
    // Get scale degree of root note
    let degree = key.degree_of_note(root_note)?;

    // Create root notation from scale degree
    let root_notation = RootNotation::from_scale_degree(degree, None);

    // Build the chord descriptor (everything after the root)
    let descriptor = build_chord_descriptor(chord);

    // Format: degree + descriptor
    Some(format!("{}{}", degree, descriptor))
}

/// Format chord in Roman Numeral notation (e.g., "Imaj7", "V/V7", "V64", "ivm7")
fn format_chord_roman(
    chord: &Chord,
    root_note: &keyflow::MusicalNote,
    key: &Key,
) -> Option<String> {
    // Get scale degree of root note
    let degree = key.degree_of_note(root_note)?;

    // Determine case based on chord quality
    // Major chords use uppercase, minor/diminished use lowercase
    let case = match chord.quality {
        keyflow::ChordQuality::Major | keyflow::ChordQuality::Power => RomanCase::Upper,
        keyflow::ChordQuality::Minor
        | keyflow::ChordQuality::Diminished
        | keyflow::ChordQuality::Suspended(_) => RomanCase::Lower,
        keyflow::ChordQuality::Augmented => {
            // Augmented can be either, but typically uppercase
            RomanCase::Upper
        }
    };

    // Create root notation from roman numeral
    let root_notation = RootNotation::from_roman_numeral(degree, case);

    // Build the chord descriptor
    let descriptor = build_chord_descriptor(chord);

    // Handle inversions (bass note different from root)
    let inversion_suffix = if let Some(bass_note) = &chord.bass {
        if let Some(bass_degree) = key.degree_of_note(
            &bass_note
                .resolve(Some(key))
                .unwrap_or_else(|| root_note.clone()),
        ) {
            // Determine inversion suffix based on bass degree
            // 6 = first inversion (third in bass)
            // 64 = second inversion (fifth in bass)
            if bass_degree == 3 {
                Some("6".to_string())
            } else if bass_degree == 5 {
                Some("64".to_string())
            } else {
                // Other inversions: show as slash chord
                Some(format!("/{}", bass_degree))
            }
        } else {
            None
        }
    } else {
        None
    };

    // Format: roman numeral + descriptor + inversion
    let mut result = format!("{}{}", root_notation, descriptor);
    if let Some(suffix) = inversion_suffix {
        result.push_str(&suffix);
    }

    Some(result)
}

/// Build the chord descriptor (quality, family, extensions, alterations)
/// This is everything after the root in the chord symbol
/// Uses keyflow's built-in descriptor field
fn build_chord_descriptor(chord: &Chord) -> String {
    // Use the chord's descriptor field which contains everything after the root
    if !chord.descriptor.is_empty() {
        return chord.descriptor.clone();
    }

    // Fallback: extract from normalized string by removing the root
    // The normalized string is the full chord symbol
    let normalized = &chord.normalized;
    let root_str = format!("{}", chord.root);
    
    if normalized.starts_with(&root_str) {
        normalized[root_str.len()..].to_string()
    } else {
        // If we can't extract, use the normalized string as-is
        normalized.clone()
    }
}

/// Get display string for a chord in the specified format
pub fn format_chord(
    analysis: &ChordAnalysis,
    format: ChordDisplayFormat,
) -> String {
    match format {
        ChordDisplayFormat::Name => analysis.name.clone(),
        ChordDisplayFormat::Nashville => {
            analysis.nashville.clone().unwrap_or_else(|| analysis.name.clone())
        }
        ChordDisplayFormat::Roman => {
            analysis.roman.clone().unwrap_or_else(|| analysis.name.clone())
        }
    }
}

