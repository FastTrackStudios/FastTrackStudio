//! REAPER implementation of lyrics infrastructure
//!
//! This module provides REAPER-specific implementations of lyrics reading and writing.
//! It's only available when the `reaper` feature is enabled.

#![allow(unsafe_code)] // Required for low-level REAPER API calls

pub mod read;
pub mod write;
pub mod stream;

pub use read::{read_lyrics_from_project, read_lyrics_from_reaper, convert_lyrics_data_to_lyrics, LyricsData, LyricsReadError};
pub use write::{create_text_items_from_lyrics, update_lyrics_in_reaper};
