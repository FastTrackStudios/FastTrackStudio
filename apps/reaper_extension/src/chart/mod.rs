//! Chart module for reading and detecting chords from REAPER MIDI data

pub mod actions;
pub mod build;
pub mod detection;
pub mod read;

pub use detection::{
    DetectedChord, detect_chords_from_midi_notes, detect_chords_from_reaper_midi_notes,
};
pub use read::{MidiNote, find_track_by_name, read_key_from_track, read_midi_notes_from_track};
