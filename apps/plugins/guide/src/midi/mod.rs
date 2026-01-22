//! MIDI event handling
//!
//! Processes incoming MIDI events and sends NoteOn/NoteOff events for triggered samples.

pub mod notes;

pub use notes::{MIDI_NOTES_COUNT, get_midi_note_for_section_type};
