//! Chart module for reading and detecting chords from REAPER MIDI data

pub mod read;
pub mod detection;
pub mod build;
pub mod actions;

pub use read::{MidiNote, find_track_by_name, read_midi_notes_from_track, read_key_from_track};
pub use detection::{DetectedChord, detect_chords_from_midi_notes, detect_chords_from_reaper_midi_notes};

