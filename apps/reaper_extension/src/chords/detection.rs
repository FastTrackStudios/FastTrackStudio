//! Chord detection from MIDI notes using keyflow

use keyflow::{chord::from_semitones, MusicalNote, RootNotation};
use tracing::{debug, trace};
use crate::chords::types::{ChordConfig, ChordData, MidiNoteEvent};

/// Detect chords from MIDI notes
pub fn detect_chords(
    notes: &[MidiNoteEvent],
    config: &ChordConfig,
) -> Result<Vec<ChordData>, ChordDetectionError> {
    if notes.is_empty() {
        return Ok(Vec::new());
    }

    // Sort notes by start time
    let mut sorted_notes = notes.to_vec();
    sorted_notes.sort_by_key(|n| n.start_ppq);

    // Track active notes over time
    let mut active_notes: Vec<&MidiNoteEvent> = Vec::new();
    let mut chords = Vec::new();
    let mut current_time = sorted_notes[0].start_ppq;

    // Process notes in chronological order
    for note in &sorted_notes {
        // Remove notes that have ended before this note starts
        active_notes.retain(|n| n.end_ppq > note.start_ppq);

        // If this note starts after the current time, analyze the active chord
        if note.start_ppq > current_time && active_notes.len() >= 2 {
            if let Some(chord) = detect_chord_from_notes(
                &active_notes,
                current_time,
                note.start_ppq,
                config,
            )? {
                chords.push(chord);
            }
        }

        // Add this note to active notes
        active_notes.push(note);
        current_time = note.start_ppq;
    }

    // Analyze final chord if there are active notes
    if active_notes.len() >= 2 {
        // Find the end time (latest end_ppq of active notes)
        let end_ppq = active_notes
            .iter()
            .map(|n| n.end_ppq)
            .max()
            .unwrap_or(current_time);

        if let Some(chord) = detect_chord_from_notes(
            &active_notes,
            current_time,
            end_ppq,
            config,
        )? {
            chords.push(chord);
        }
    }

    // Merge consecutive identical chords
    let merged_chords = merge_consecutive_chords(chords, config);

    debug!(
        detected_count = merged_chords.len(),
        "Detected {} chords from {} notes",
        merged_chords.len(),
        notes.len()
    );

    Ok(merged_chords)
}

/// Detect a chord from a set of active notes at a specific time
fn detect_chord_from_notes(
    notes: &[&MidiNoteEvent],
    start_ppq: i32,
    end_ppq: i32,
    config: &ChordConfig,
) -> Result<Option<ChordData>, ChordDetectionError> {
    // Filter out very short notes (likely arpeggiated)
    let filtered_notes: Vec<_> = notes
        .iter()
        .filter(|n| (n.end_ppq - n.start_ppq) >= config.min_note_duration_ppq)
        .copied()
        .collect();

    if filtered_notes.len() < 2 {
        return Ok(None);
    }

    // Extract MIDI pitches
    let mut pitches: Vec<u8> = filtered_notes.iter().map(|n| n.pitch).collect();
    pitches.sort();

    // Find root (lowest note)
    let root_midi = pitches[0];
    let root_semitone = (root_midi % 12) as u8;

    // Convert to semitones relative to root
    let semitones: Vec<u8> = pitches
        .iter()
        .map(|&pitch| {
            let relative = (pitch as i16 - root_midi as i16) % 12;
            (relative + 12) as u8 % 12
        })
        .collect();

    trace!(
        pitches = ?pitches,
        semitones = ?semitones,
        root_midi = root_midi,
        "Analyzing chord from notes"
    );

    // Create root note from MIDI number
    // Use sharp preference (can be made configurable later)
    let root_note = MusicalNote::from_semitone(root_semitone, true);

    // Create root notation
    let root_notation = RootNotation::from_note_name(root_note.clone());

    // Use keyflow to detect chord from semitones
    let chord = from_semitones(&semitones, root_notation)
        .map_err(|e| ChordDetectionError::ChordDetectionFailed {
            semitones: semitones.clone(),
            error: format!("{}", e),
        })?;

    // Detect bass note (lowest note, might be different from root if inverted)
    let bass_midi = pitches[0];
    let bass_semitone = (bass_midi % 12) as u8;
    let bass_note = if bass_semitone != root_note.semitone {
        Some(MusicalNote::from_semitone(bass_semitone, true))
    } else {
        None
    };

    // Convert PPQ to project time (approximate - would need take context for exact conversion)
    // For now, we'll use PPQ directly and convert later if needed
    let start_time = start_ppq as f64 / 960.0; // Assuming 960 PPQ
    let end_time = end_ppq as f64 / 960.0;

    // Check minimum duration
    if (end_ppq - start_ppq) < config.min_chord_duration_ppq {
        return Ok(None);
    }

    Ok(Some(ChordData {
        chord,
        start_time,
        end_time,
        start_ppq,
        end_ppq,
        root_note,
        bass_note,
    }))
}

/// Merge consecutive identical chords
fn merge_consecutive_chords(
    mut chords: Vec<ChordData>,
    config: &ChordConfig,
) -> Vec<ChordData> {
    if chords.is_empty() {
        return chords;
    }

    let mut merged = Vec::new();
    let mut current = chords[0].clone();

    for next in chords.into_iter().skip(1) {
        // Check if chords are identical (same normalized form)
        if current.chord.normalized == next.chord.normalized
            && current.root_note.semitone == next.root_note.semitone
        {
            // Merge: extend end time
            current.end_time = next.end_time;
            current.end_ppq = next.end_ppq;
        } else {
            // Different chord: save current and start new
            merged.push(current);
            current = next;
        }
    }

    // Add the last chord
    merged.push(current);

    merged
}

#[derive(Debug, thiserror::Error)]
pub enum ChordDetectionError {
    #[error("Failed to detect chord from semitones {semitones:?}: {error}")]
    ChordDetectionFailed {
        semitones: Vec<u8>,
        error: String,
    },
    #[error("Invalid note data: {message}")]
    InvalidNoteData { message: String },
}

