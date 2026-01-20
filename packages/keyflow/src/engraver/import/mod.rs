//! Import functionality for various music formats.
//!
//! Currently supports:
//! - keyflow Chart format (always available with `engraver` feature)
//! - MIDI files (requires `midi-import` feature)
//!
//! Planned:
//! - MusicXML

mod keyflow_import;

#[cfg(feature = "midi-import")]
mod midi_import;

pub use keyflow_import::import_chart;

#[cfg(feature = "midi-import")]
pub use midi_import::{
    ChordMarker, MarkerEvent, MarkerType, MidiFile, MidiImportConfig, MidiNote, MidiTrack,
    MusicalPosition, PushPull, PushPullAmount, SectionMarker, SectionType, TempoEvent,
    TimeSignatureEvent,
};
