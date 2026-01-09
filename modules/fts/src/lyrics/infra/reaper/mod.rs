//! REAPER implementation of lyrics infrastructure
//!
//! This module provides REAPER-specific implementations of lyrics reading and writing.
//! It's only available when the `reaper` feature is enabled.

#![allow(unsafe_code)] // Required for low-level REAPER API calls

pub mod read;
pub mod stream;
pub mod write;

pub use read::{
    LyricsData, LyricsReadError, convert_lyrics_data_to_lyrics, read_lyrics_from_project,
    read_lyrics_from_reaper,
};
pub use write::{create_text_items_from_lyrics, update_lyrics_in_reaper};
