//! FastTrackStudio Core Modules
//!
//! This crate contains all FTS-specific functionality:
//! - Setlist management
//! - Lyrics sync and display
//! - Chord detection and display
//! - Smart templates (includes naming conventions and track templates)

pub mod setlist;
pub mod lyrics;
pub mod chords;
pub mod smart_template;

// Re-export commonly used types
pub use setlist::{Setlist, SetlistApi, Song};
pub use lyrics::Lyrics;
pub use smart_template::{Template, Group, Parser, Matcher};
