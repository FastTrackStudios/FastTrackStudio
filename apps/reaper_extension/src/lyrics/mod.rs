//! REAPER integration for lyrics functionality
//!
//! This module provides functions to read and write lyrics data from/to REAPER projects.
//! It handles finding the LYRICS folder, SLIDES track, and MIDI tracks with syllable data.

pub mod read;
pub mod write;
mod actions;
#[cfg(debug_assertions)]
mod dev_actions;
pub mod stream;

pub use read::*;
pub use write::*;
pub use actions::register_lyrics_actions;
#[cfg(debug_assertions)]
pub use dev_actions::register_lyrics_dev_actions;
pub use stream::*;

