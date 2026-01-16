//! Import functionality for various music formats.
//!
//! Currently supports:
//! - keyflow Chart format (requires `keyflow-import` feature)
//!
//! Planned:
//! - MusicXML
//! - MIDI (basic)

#[cfg(feature = "keyflow-import")]
mod keyflow_import;

#[cfg(feature = "keyflow-import")]
pub use keyflow_import::import_chart;
