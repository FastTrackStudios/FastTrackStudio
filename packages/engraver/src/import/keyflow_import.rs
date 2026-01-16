//! Import from keyflow Chart format.
//!
//! Converts keyflow's lead sheet representation (chords, sections) into
//! a renderable Score format with:
//! - Rhythm slashes for chord durations
//! - Chord symbols above the staff
//! - Section labels as rehearsal marks
//! - Key signatures from the chart's key context

use crate::model::{
    ChordSymbol, Duration, DurationKind, KeySignature,
    LayoutBreak, Measure as EngraverMeasure, MusicElement, Note, NoteHead, Octave, Part, PartId,
    Pitch, PitchClass, RehearsalMark, RehearsalMarkStyle, Score, ScoreMetadata, Stem,
    TimeSignature as EngraverTimeSig, Voice,
};
use keyflow::{DurationTrait, Note as KeyflowNote};

// region:    --- From<Chart> for Score

impl From<keyflow::Chart> for Score {
    fn from(chart: keyflow::Chart) -> Self {
        import_chart(&chart)
    }
}

impl From<&keyflow::Chart> for Score {
    fn from(chart: &keyflow::Chart) -> Self {
        import_chart(chart)
    }
}

// endregion: --- From<Chart> for Score

// region:    --- Import Functions

/// Import a keyflow Chart into a Score.
///
/// This converts the lead sheet representation (chords, sections) into
/// a renderable score format with:
/// - Rhythm slashes for chord durations
/// - Chord symbols above the staff
/// - Section labels as rehearsal marks
/// - Key signatures
#[must_use]
pub fn import_chart(chart: &keyflow::Chart) -> Score {
    let metadata = ScoreMetadata {
        title: chart.metadata.title.clone(),
        composer: chart.metadata.artist.clone(),
        arranger: chart.metadata.arranger.clone(),
        lyricist: chart.metadata.lyricist.clone(),
        copyright: chart.metadata.copyright.clone(),
        ..Default::default()
    };

    // Create a single part for the lead sheet
    let mut part = Part::new(PartId(1), "Lead Sheet");

    // Convert time signature
    let time_sig = chart
        .time_signature
        .as_ref()
        .map_or(EngraverTimeSig::COMMON, |ts| {
            EngraverTimeSig::new(
                u8::try_from(ts.numerator).unwrap_or(4),
                u8::try_from(ts.denominator).unwrap_or(4),
            )
        });

    // Get initial key signature
    let initial_key_sig = chart.initial_key.as_ref().map(key_to_signature);

    // Process each section
    let mut measure_num = 1u32;
    let mut is_first_measure = true;

    for (section_idx, chart_section) in chart.sections.iter().enumerate() {
        for (measure_idx, chart_measure) in chart_section.measures.iter().enumerate() {
            let is_section_start = measure_idx == 0;

            // Create measure with signatures on first measure
            let mut measure = EngraverMeasure::with_signatures(
                measure_num,
                if is_first_measure {
                    Some(time_sig)
                } else {
                    None
                },
                if is_first_measure {
                    initial_key_sig
                } else {
                    None
                },
            );

            // Add section label as rehearsal mark at the start of each section
            if is_section_start {
                let section_name = format_section_name(&chart_section.section);
                measure = measure.with_rehearsal_mark(
                    RehearsalMark::new(&section_name)
                        .with_style(RehearsalMarkStyle::Capsule)
                        .with_break(section_idx > 0), // Force break for all but first section
                );
            }

            // Convert chords to rhythm slashes with chord symbols
            let mut voice = Voice::new();

            for chord_instance in &chart_measure.chords {
                // Add chord symbol
                let chord_symbol = chord_instance_to_symbol(chord_instance);
                voice.add(MusicElement::ChordSymbol(chord_symbol));

                // Add rhythm slash note
                let duration = chord_duration_to_engraver(&chord_instance.duration);
                let note = create_rhythm_slash(duration);
                voice.add(MusicElement::Note(note));
            }

            // If measure is empty, add a whole rest worth of slashes
            if chart_measure.chords.is_empty() {
                let duration = Duration::new(DurationKind::Whole);
                let note = create_rhythm_slash(duration);
                voice.add(MusicElement::Note(note));
            }

            measure.voices = vec![voice];

            // Add section break at end of section (except last section)
            if measure_idx == chart_section.measures.len() - 1
                && section_idx < chart.sections.len() - 1
            {
                measure = measure.with_layout_break(LayoutBreak::Section);
            }

            part.add_measure(measure);
            measure_num += 1;
            is_first_measure = false;
        }
    }

    Score {
        metadata,
        parts: vec![part],
        time_signature: time_sig,
        tempo_bpm: chart.tempo.as_ref().map(|t| f64::from(t.bpm)),
        ..Default::default()
    }
}

// endregion: --- Import Functions

// region:    --- Conversion Helpers

/// Convert a keyflow Key to an engraver KeySignature.
///
/// Maps the key root and mode to the circle of fifths position.
fn key_to_signature(key: &keyflow::Key) -> KeySignature {
    // Get the semitone value of the root note using the Note trait
    let root_semitone = key.root.semitone();

    // Check if the mode is minor-like (aeolian/natural minor, harmonic minor, melodic minor)
    // These modes share key signatures with their relative major (3 semitones up)
    let mode_name = key.mode.name().to_lowercase();
    let is_minor = mode_name.contains("minor")
        || mode_name.contains("aeolian")
        || mode_name.contains("dorian")
        || mode_name.contains("phrygian")
        || mode_name.contains("locrian");

    let major_root_semitone = if is_minor {
        (root_semitone + 3) % 12 // Minor to relative major
    } else {
        root_semitone
    };

    // Map major key root to fifths count
    // C=0, G=1, D=2, A=3, E=4, B=5, F#/Gb=6/-6, F=-1, Bb=-2, Eb=-3, Ab=-4, Db=-5
    let fifths = match major_root_semitone {
        0 => 0,   // C
        1 => -5,  // Db (or C#=7, but we prefer flats for ambiguous cases)
        2 => 2,   // D
        3 => -3,  // Eb
        4 => 4,   // E
        5 => -1,  // F
        6 => 6,   // F# (or Gb=-6)
        7 => 1,   // G
        8 => -4,  // Ab
        9 => 3,   // A
        10 => -2, // Bb
        11 => 5,  // B
        _ => 0,
    };

    KeySignature::new(fifths)
}

/// Format a section name for display.
fn format_section_name(section: &keyflow::Section) -> String {
    let base_name = section.section_type.abbreviation();

    if let Some(number) = section.number {
        if let Some(letter) = section.split_letter {
            format!("{} {}{}", base_name, number, letter)
        } else {
            format!("{} {}", base_name, number)
        }
    } else if let Some(letter) = section.split_letter {
        format!("{}{}", base_name, letter)
    } else {
        base_name
    }
}

/// Convert a ChordInstance to a ChordSymbol.
fn chord_instance_to_symbol(chord: &keyflow::ChordInstance) -> ChordSymbol {
    // Use the full_symbol which is already formatted (e.g., "Gmaj7", "Dm7")
    let symbol = &chord.full_symbol;

    // Extract root from the chord instance using Display trait
    let root = chord.root.to_string();

    // Parse root accidental from the symbol
    let root_accidental = if symbol.len() > 1 {
        let second_char = symbol.chars().nth(1);
        match second_char {
            Some('#') => "#".to_string(),
            Some('b') => "b".to_string(),
            _ => String::new(),
        }
    } else {
        String::new()
    };

    // Extract quality and extension from full_symbol
    let (quality, extension) = parse_chord_quality_extension(symbol, &root, &root_accidental);

    // Check for bass note (slash chord)
    let (bass, bass_accidental) = if let Some(slash_pos) = symbol.find('/') {
        let bass_part = &symbol[slash_pos + 1..];
        if bass_part.len() > 1
            && (bass_part.chars().nth(1) == Some('#') || bass_part.chars().nth(1) == Some('b'))
        {
            (
                Some(bass_part[..1].to_string()),
                bass_part[1..2].to_string(),
            )
        } else {
            (Some(bass_part.to_string()), String::new())
        }
    } else {
        (None, String::new())
    };

    ChordSymbol {
        symbol: symbol.clone(),
        root,
        root_accidental,
        quality,
        extension,
        alterations: Vec::new(), // TODO: Parse alterations like b5, #9
        bass,
        bass_accidental,
    }
}

/// Parse quality and extension from a chord symbol.
fn parse_chord_quality_extension(
    symbol: &str,
    root: &str,
    root_accidental: &str,
) -> (String, String) {
    // Remove root and accidental to get the rest
    let start = root.len() + root_accidental.len();
    if start >= symbol.len() {
        return (String::new(), String::new());
    }

    // Remove bass note if present
    let rest = if let Some(slash_pos) = symbol[start..].find('/') {
        &symbol[start..start + slash_pos]
    } else {
        &symbol[start..]
    };

    // Parse quality (m, dim, aug, etc.) and extension (7, maj7, 9, etc.)
    let rest_lower = rest.to_lowercase();

    if rest_lower.starts_with("maj") {
        if rest_lower.len() > 3 {
            (String::new(), rest.to_string()) // e.g., "maj7" -> quality="", extension="maj7"
        } else {
            (String::new(), String::new()) // Just "maj" means major
        }
    } else if rest_lower.starts_with("min")
        || rest.starts_with('m') && !rest_lower.starts_with("maj")
    {
        let m_len = if rest_lower.starts_with("min") { 3 } else { 1 };
        ("m".to_string(), rest[m_len..].to_string())
    } else if rest_lower.starts_with("dim") {
        ("dim".to_string(), rest[3..].to_string())
    } else if rest_lower.starts_with("aug") {
        ("aug".to_string(), rest[3..].to_string())
    } else if rest_lower.starts_with("sus") {
        ("sus".to_string(), rest[3..].to_string())
    } else if rest.starts_with('+') {
        ("aug".to_string(), rest[1..].to_string())
    } else if rest.starts_with('o') || rest.starts_with('°') {
        ("dim".to_string(), rest[1..].to_string())
    } else {
        // No quality prefix, rest is the extension (e.g., "7", "9")
        (String::new(), rest.to_string())
    }
}

/// Create a rhythm slash note.
fn create_rhythm_slash(duration: Duration) -> Note {
    // Use B4 (middle of treble staff) for rhythm slashes
    let pitch = Pitch::new(PitchClass::B, Octave::new(4));
    let head = NoteHead::slash_for_duration(duration.kind);

    Note {
        pitch,
        duration,
        accidental: crate::model::Accidental::None,
        head,
        stem: if duration.kind == DurationKind::Whole {
            Stem::None
        } else {
            Stem::Up
        },
        tie_forward: false,
        tie_back: false,
    }
}

/// Convert a keyflow duration to an engraver Duration.
fn chord_duration_to_engraver(duration: &keyflow::MusicalDuration) -> Duration {
    let beats = duration.beats();

    // Handle subdivision for more precise durations
    let subdivision = duration.subdivisions();

    if beats >= 4 {
        Duration::new(DurationKind::Whole)
    } else if beats >= 2 {
        if subdivision >= 50 {
            // 2.5+ beats = dotted half
            Duration::dotted(DurationKind::Half)
        } else {
            Duration::new(DurationKind::Half)
        }
    } else if beats >= 1 {
        if subdivision >= 50 {
            // 1.5+ beats = dotted quarter
            Duration::dotted(DurationKind::Quarter)
        } else {
            Duration::new(DurationKind::Quarter)
        }
    } else if subdivision >= 50 {
        // 0.5+ beats = dotted eighth
        Duration::dotted(DurationKind::Eighth)
    } else if subdivision >= 25 {
        // 0.25+ beats = sixteenth
        Duration::new(DurationKind::Sixteenth)
    } else {
        // Default to eighth note for very short durations
        Duration::new(DurationKind::Eighth)
    }
}

// endregion: --- Conversion Helpers

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_section_name() {
        use keyflow::SectionType;

        let section = keyflow::Section {
            section_type: SectionType::Verse,
            number: Some(1),
            split_letter: None,
            measures: 4,
        };
        assert_eq!(format_section_name(&section), "V 1");

        let section = keyflow::Section {
            section_type: SectionType::Chorus,
            number: Some(2),
            split_letter: Some('a'),
            measures: 8,
        };
        assert_eq!(format_section_name(&section), "CH 2a");
    }

    #[test]
    fn test_parse_chord_quality_extension() {
        let (q, e) = parse_chord_quality_extension("Cmaj7", "C", "");
        assert_eq!(q, "");
        assert_eq!(e, "maj7");

        let (q, e) = parse_chord_quality_extension("Dm7", "D", "");
        assert_eq!(q, "m");
        assert_eq!(e, "7");

        let (q, e) = parse_chord_quality_extension("G7", "G", "");
        assert_eq!(q, "");
        assert_eq!(e, "7");

        let (q, e) = parse_chord_quality_extension("Bdim7", "B", "");
        assert_eq!(q, "dim");
        assert_eq!(e, "7");
    }

    #[test]
    fn test_chord_duration_to_engraver() {
        // Whole note (4 beats)
        let dur = keyflow::MusicalDuration::new(0, 4, 0);
        let result = chord_duration_to_engraver(&dur);
        assert_eq!(result.kind, DurationKind::Whole);

        // Half note (2 beats)
        let dur = keyflow::MusicalDuration::new(0, 2, 0);
        let result = chord_duration_to_engraver(&dur);
        assert_eq!(result.kind, DurationKind::Half);

        // Quarter note (1 beat)
        let dur = keyflow::MusicalDuration::new(0, 1, 0);
        let result = chord_duration_to_engraver(&dur);
        assert_eq!(result.kind, DurationKind::Quarter);
    }
}
