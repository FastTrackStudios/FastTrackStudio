//! Import from keyflow Chart format.

use crate::model::{
    Duration, DurationKind, Measure as EngraverMeasure, MusicElement, Note, Octave, Part, PartId,
    Pitch, PitchClass, Score, ScoreMetadata, TimeSignature as EngraverTimeSig, Voice,
};
use keyflow::DurationTrait;

/// Import a keyflow Chart into a Score.
///
/// This converts the lead sheet representation (chords, sections) into
/// a renderable score format.
#[must_use]
pub fn import_chart(chart: &keyflow::Chart) -> Score {
    let metadata = ScoreMetadata {
        title: chart.metadata.title.clone(),
        composer: chart.metadata.artist.clone(),
        ..Default::default()
    };

    // Create a single part for the lead sheet
    let mut part = Part::new(PartId(1), "Lead Sheet");

    // Convert time signature
    let time_sig = chart
        .time_signature
        .as_ref()
        .map_or(EngraverTimeSig::COMMON, |ts| {
            EngraverTimeSig::new(ts.numerator as u8, ts.denominator as u8)
        });

    // Process each section
    let mut measure_num = 1u32;
    for section in &chart.sections {
        for chart_measure in &section.measures {
            let mut measure = EngraverMeasure::with_signatures(
                measure_num,
                if measure_num == 1 {
                    Some(time_sig)
                } else {
                    None
                },
                None, // TODO: Handle key signatures
            );

            // Convert chords to rhythm slashes or chord symbols
            let mut voice = Voice::new();
            for chord_instance in &chart_measure.chords {
                // For now, create rhythm slashes for each chord
                // In a full implementation, we'd render chord symbols above
                let duration = chord_duration_to_engraver(&chord_instance.duration);

                // Create a rhythm slash note (middle line, slash notehead)
                let note = Note::new(
                    Pitch::new(PitchClass::B, Octave::new(4)), // Middle line for treble
                    duration,
                );

                voice.add(MusicElement::Note(note));
            }

            measure.voices = vec![voice];
            part.add_measure(measure);
            measure_num += 1;
        }
    }

    Score {
        metadata,
        parts: vec![part],
        time_signature: time_sig,
        tempo_bpm: chart.tempo.as_ref().map(|t| t.bpm),
        ..Default::default()
    }
}

/// Convert a keyflow duration to an engraver Duration.
fn chord_duration_to_engraver(duration: &keyflow::MusicalDuration) -> Duration {
    // Map based on beats (simplified)
    let beats = duration.beats();

    if beats >= 4 {
        Duration::new(DurationKind::Whole)
    } else if beats >= 2 {
        Duration::new(DurationKind::Half)
    } else if beats >= 1 {
        Duration::new(DurationKind::Quarter)
    } else {
        // For sub-beat durations, default to eighth note
        Duration::new(DurationKind::Eighth)
    }
}
