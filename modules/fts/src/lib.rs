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
/// (transport, tracks). Only available when both `reaper` and `iroh` features are enabled
/// (requires rxrust for reactive streams).
#[cfg(all(feature = "reaper", feature = "iroh"))]
pub mod daw_reactive;

// Re-export commonly used types
pub use lyrics::Lyrics;
pub use setlist::{LyricsState, NavigationCommand, Setlist, SetlistApi, Song, TransportCommand};
