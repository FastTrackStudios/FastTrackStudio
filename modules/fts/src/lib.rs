//! FastTrackStudio Core Modules
//!
//! This crate contains all FTS-specific functionality:
//! - Setlist management
//! - Lyrics sync and display
//! - Chord detection and display
//! - Smart templates (includes naming conventions and track templates)
//! - DAW reactive REAPER implementations (when `reaper` feature is enabled)

pub mod chords;
pub mod lyrics;
pub mod setlist;

/// DAW Reactive REAPER implementations
///
/// Contains REAPER-specific implementations of the DAW reactive services
/// (transport, tracks). Only available when the `reaper` feature is enabled.
#[cfg(feature = "reaper")]
pub mod daw_reactive;

// Re-export commonly used types
pub use lyrics::Lyrics;
pub use setlist::{Setlist, SetlistApi, Song};
