//! Setlist functionality for FastTrackStudio
//!
//! This module provides setlist management with clean architecture.

pub mod application;
pub mod core;
pub mod infra;
pub mod mock;
pub mod order;
pub mod service;

#[cfg(all(feature = "iroh", not(target_arch = "wasm32")))]
pub mod reactive;

// Command types are always available (shared between iroh and reaper features)
pub use infra::commands::{LyricsState, NavigationCommand, TransportCommand};

// Trait types are always available (for non-wasm targets)
#[cfg(not(target_arch = "wasm32"))]
pub use infra::traits::{SetlistCommandHandler, SetlistStateProvider};

#[cfg(all(feature = "iroh", not(target_arch = "wasm32")))]
pub use infra::stream::{
    SeekToSection, SeekToSong, SeekToTime, SetlistStreamApi, SetlistUpdateMessage, ToggleLoop,
};

#[cfg(feature = "dioxus")]
pub use infra::dioxus::{
    ACTIVE_INDICES, ACTIVE_SLIDE_INDEX, CURRENT_POSITION_SECONDS, IS_PLAYING, PLAYBACK_POSITION,
    ProjectTransportInfo, SECTION_PROGRESS, SETLIST, SETLIST_STRUCTURE, SONG_PROGRESS, SONG_TRACKS,
    SONG_TRANSPORT, TRANSPORT_INFO,
};

pub use core::{
    Section, SectionType, Setlist, SetlistApi, SetlistEntry, SetlistError, SetlistOrder,
    SetlistSummary, Song, SongSummary,
};

// Roam service exports
pub use service::{
    ActiveIndices, LocalSetlistClient, SectionInfo, SetlistCommand, SetlistEvent, SetlistInfo,
    SetlistService as RoamSetlistService, SongInfo,
};

// Mock implementation
pub use mock::MockSetlist;

#[cfg(all(feature = "iroh", not(target_arch = "wasm32")))]
pub use reactive::{
    ActiveIndicesStreams, EventStreamSubject, LyricsStreams, SetlistReactiveService,
    SetlistReactiveState, SetlistReactiveStreams, SetlistStreams, SongStreams,
};

#[cfg(all(feature = "iroh", not(target_arch = "wasm32")))]
pub use reactive::irpc::{SetlistReactiveApi, SetlistReactiveProtocol};

pub fn default_setlist() -> Result<Setlist, SetlistError> {
    Setlist::default_app_setlist()
}

pub fn sample_setlist() -> Result<Setlist, SetlistError> {
    Setlist::sample_concert_setlist()
}

pub fn load_setlist_from_path<P: AsRef<std::path::Path>>(path: P) -> Result<Setlist, SetlistError> {
    Setlist::load_from_path(path)
}

pub fn save_setlist_to_path<P: AsRef<std::path::Path>>(
    setlist: &Setlist,
    path: P,
) -> Result<(), SetlistError> {
    setlist.save_to_path(path)
}

pub fn validate_setlist(setlist: &Setlist) -> Result<(), SetlistError> {
    setlist.validate()
}

pub fn parse_section_type(input: &str) -> Result<SectionType, SetlistError> {
    SectionType::parse(input).map_err(|e| SetlistError::invalid_section(&e))
}

pub fn create_setlist(name: &str) -> Result<Setlist, SetlistError> {
    Setlist::new(name.to_string())
}

pub fn create_song(name: &str) -> Result<Song, SetlistError> {
    Song::new(name.to_string())
}

pub fn create_section(
    section_type: SectionType,
    start_seconds: f64,
    end_seconds: f64,
    name: &str,
) -> Result<Section, SetlistError> {
    use crate::setlist::core::section::section_from_seconds;
    section_from_seconds(
        section_type,
        start_seconds,
        end_seconds,
        name.to_string(),
        None,
    )
}

pub const VERSION: &str = env!("CARGO_PKG_VERSION");
pub const MODULE_NAME: &str = "setlist";
