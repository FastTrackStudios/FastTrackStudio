//! Reactive streams and services for setlist domain types
//!
//! This module provides reactive streams and services for setlist, song, and lyrics state.
//! Each domain type can define its own reactive behavior, making the system composable.

use crate::lyrics::{Lyrics, LyricsAnnotations};
use crate::setlist::core::{Section, Setlist, Song};
use rxrust::prelude::*;
use std::cell::RefCell;

/// Re-export EventStreamSubject for convenience
pub type EventStreamSubject<T> = RefCell<LocalSubject<'static, T, ()>>;

pub mod service;
pub use service::{SetlistReactiveService, SetlistReactiveState};

#[cfg(not(target_arch = "wasm32"))]
pub mod irpc;

/// Reactive streams for setlist structure
#[derive(Clone, Default, Debug)]
pub struct SetlistStreams {
    /// Setlist structure changed (songs, sections, metadata)
    pub setlist_structure_changed: EventStreamSubject<Setlist>,

    /// Song added to setlist
    pub song_added: EventStreamSubject<(usize, Song)>,

    /// Song removed from setlist
    pub song_removed: EventStreamSubject<usize>,

    /// Songs reordered
    pub songs_reordered: EventStreamSubject<Setlist>,
}

impl SetlistStreams {
    pub fn new() -> Self {
        fn default<T>() -> EventStreamSubject<T> {
            RefCell::new(LocalSubject::new())
        }
        Self {
            setlist_structure_changed: default(),
            song_added: default(),
            song_removed: default(),
            songs_reordered: default(),
        }
    }
}

/// Reactive streams for song state
#[derive(Clone, Default, Debug)]
pub struct SongStreams {
    /// Song metadata changed (name, markers, etc.)
    pub song_changed: EventStreamSubject<(usize, Song)>,

    /// Section added to a song
    pub section_added: EventStreamSubject<(usize, usize, Section)>, // song_index, section_index, section

    /// Section removed from a song
    pub section_removed: EventStreamSubject<(usize, usize)>, // song_index, section_index

    /// Section changed in a song
    pub section_changed: EventStreamSubject<(usize, usize, Section)>, // song_index, section_index, section
}

impl SongStreams {
    pub fn new() -> Self {
        fn default<T>() -> EventStreamSubject<T> {
            RefCell::new(LocalSubject::new())
        }
        Self {
            song_changed: default(),
            section_added: default(),
            section_removed: default(),
            section_changed: default(),
        }
    }
}

/// Reactive streams for lyrics state
#[derive(Clone, Default, Debug)]
pub struct LyricsStreams {
    /// Lyrics changed for a specific song (full lyrics structure)
    /// Note: Lyrics are part of the Song in the setlist, but this stream allows
    /// granular updates when only lyrics change without the whole setlist changing
    pub lyrics_changed: EventStreamSubject<(usize, Lyrics)>,

    /// Lyrics annotations changed for a specific song (timing, MIDI notes, slide breaks)
    /// Note: Annotations may be stored separately, but are associated with a song
    pub lyrics_annotations_changed: EventStreamSubject<(usize, LyricsAnnotations)>,
}

impl LyricsStreams {
    pub fn new() -> Self {
        fn default<T>() -> EventStreamSubject<T> {
            RefCell::new(LocalSubject::new())
        }
        Self {
            lyrics_changed: default(),
            lyrics_annotations_changed: default(),
        }
    }
}

/// Reactive streams for active indices (navigation state)
#[derive(Clone, Default, Debug)]
pub struct ActiveIndicesStreams {
    /// Active indices changed (song/section/slide)
    pub active_indices_changed: EventStreamSubject<(Option<usize>, Option<usize>, Option<usize>)>,

    /// Active song index changed
    pub active_song_index_changed: EventStreamSubject<Option<usize>>,

    /// Active section index changed
    pub active_section_index_changed: EventStreamSubject<Option<usize>>,

    /// Active slide index changed
    pub active_slide_index_changed: EventStreamSubject<Option<usize>>,
}

impl ActiveIndicesStreams {
    pub fn new() -> Self {
        fn default<T>() -> EventStreamSubject<T> {
            RefCell::new(LocalSubject::new())
        }
        Self {
            active_indices_changed: default(),
            active_song_index_changed: default(),
            active_section_index_changed: default(),
            active_slide_index_changed: default(),
        }
    }
}

/// All setlist reactive streams composed together
#[derive(Clone, Default, Debug)]
pub struct SetlistReactiveStreams {
    pub setlist: SetlistStreams,
    pub song: SongStreams,
    pub lyrics: LyricsStreams,
    pub active_indices: ActiveIndicesStreams,
}

impl SetlistReactiveStreams {
    pub fn new() -> Self {
        Self {
            setlist: SetlistStreams::new(),
            song: SongStreams::new(),
            lyrics: LyricsStreams::new(),
            active_indices: ActiveIndicesStreams::new(),
        }
    }
}
