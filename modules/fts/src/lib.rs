//! FastTrackStudio Core Modules
//!
//! This crate contains all FTS-specific functionality:
//! - Setlist management
//! - Lyrics sync and display
//! - Chord detection and display
//! - Naming conventions
//! - Track templates
//! - Smart templates

pub mod setlist;
pub mod lyrics;
pub mod chords;
pub mod naming_convention;
pub mod track_template;
pub mod smart_template;

// Re-export commonly used types
pub use setlist::*;
pub use lyrics::*;
pub use chords::*;
pub use naming_convention::*;
pub use track_template::*;

