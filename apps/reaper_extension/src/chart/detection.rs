//! Chord detection using keyflow's MIDI integration
//!
//! This module re-exports keyflow's MIDI chord detection functionality
//! for use with REAPER-specific MIDI note reading.

pub use keyflow::chord::midi::{DetectedChord, detect_chords_from_midi_notes};

// Re-export MidiNote from keyflow, but also provide a conversion from REAPER's MidiNote
use super::read::MidiNote as ReaperMidiNote;
use keyflow::chord::midi::MidiNote;

/// Convert REAPER MIDI note to keyflow MIDI note
impl From<ReaperMidiNote> for MidiNote {
    fn from(note: ReaperMidiNote) -> Self {
        MidiNote::new(
            note.pitch,
            note.start_ppq,
            note.end_ppq,
            note.channel,
            note.velocity,
        )
    }
}

/// Detect chords from REAPER MIDI notes
///
/// This is a convenience wrapper that converts REAPER MIDI notes to keyflow MIDI notes
/// and then calls the keyflow chord detection function.
pub fn detect_chords_from_reaper_midi_notes(
    notes: &[ReaperMidiNote],
    min_chord_duration_ppq: i64,
) -> Vec<DetectedChord> {
    let keyflow_notes: Vec<MidiNote> = notes
        .iter()
        .map(|n| MidiNote::new(n.pitch, n.start_ppq, n.end_ppq, n.channel, n.velocity))
        .collect();
    detect_chords_from_midi_notes(&keyflow_notes, min_chord_duration_ppq)
}
