//! SetlistApi - API wrapper for Setlist with computed/derived fields
//!
//! This module provides a `SetlistApi` struct that wraps a `Setlist` and provides
//! computed fields like active song, next/previous song, next/previous section, etc.
//! This is what gets sent over the stream instead of the raw `Setlist`.

use serde::{Deserialize, Serialize};

use super::{Setlist, Song, Section};

/// API wrapper for Setlist with computed/derived fields
/// 
/// This struct wraps a `Setlist` and provides computed fields that are derived
/// from REAPER's transport state (active song, active section, etc.).
/// This is what gets sent over the stream and stored in the global signal.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SetlistApi {
    /// The underlying setlist
    pub setlist: Setlist,
    /// Index of the currently active song (derived from REAPER's transport state)
    pub active_song_index: Option<usize>,
    /// Index of the currently active section within the active song
    pub active_section_index: Option<usize>,
    /// Index of the currently active slide within the active section (for lyrics view)
    pub active_slide_index: Option<usize>,
}

impl SetlistApi {
    /// Create a new SetlistApi from a Setlist with active indices
    pub fn new(setlist: Setlist, active_song_index: Option<usize>, active_section_index: Option<usize>, active_slide_index: Option<usize>) -> Self {
        Self {
            setlist,
            active_song_index,
            active_section_index,
            active_slide_index,
        }
    }

    /// Get the underlying setlist
    pub fn get_setlist(&self) -> &Setlist {
        &self.setlist
    }

    /// Get the currently active song
    pub fn active_song(&self) -> Option<&Song> {
        self.active_song_index
            .and_then(|idx| self.setlist.songs.get(idx))
    }

    /// Get the next song after the active song
    pub fn next_song(&self) -> Option<&Song> {
        self.active_song_index
            .and_then(|idx| self.setlist.next_song(idx))
    }

    /// Get the previous song before the active song
    pub fn previous_song(&self) -> Option<&Song> {
        self.active_song_index
            .and_then(|idx| self.setlist.previous_song(idx))
    }

    /// Get the currently active section
    pub fn active_section(&self) -> Option<&Section> {
        self.active_song()
            .and_then(|song| {
                self.active_section_index
                    .and_then(|idx| song.sections.get(idx))
            })
    }

    /// Get the next section after the active section
    pub fn next_section(&self) -> Option<&Section> {
        self.active_song()
            .and_then(|song| {
                self.active_section_index
                    .and_then(|idx| song.next_section(idx))
            })
    }

    /// Get the previous section before the active section
    pub fn previous_section(&self) -> Option<&Section> {
        self.active_song()
            .and_then(|song| {
                self.active_section_index
                    .and_then(|idx| song.previous_section(idx))
            })
    }

    /// Get the active song index
    pub fn active_song_index(&self) -> Option<usize> {
        self.active_song_index
    }

    /// Get the active section index
    pub fn active_section_index(&self) -> Option<usize> {
        self.active_section_index
    }

    /// Get the active slide index
    pub fn active_slide_index(&self) -> Option<usize> {
        self.active_slide_index
    }
}

