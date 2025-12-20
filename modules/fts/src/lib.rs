//! FastTrackStudio Core Modules
//!
//! This crate contains all FTS-specific functionality:
//! - Setlist management
//! - Lyrics sync and display
//! - Chord detection and display
//! - Smart templates (includes naming conventions and track templates)

pub mod chords;
pub mod lyrics;
pub mod setlist;

// Re-export commonly used types
pub use lyrics::Lyrics;
pub use setlist::{Setlist, SetlistApi, Song};
