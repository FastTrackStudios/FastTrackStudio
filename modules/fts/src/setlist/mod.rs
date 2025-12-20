//! Setlist functionality for FastTrackStudio
//!
//! This module provides setlist management with clean architecture.

pub mod application;
pub mod core;
pub mod infra;
pub mod order;

#[cfg(not(target_arch = "wasm32"))]
pub mod reactive;

#[cfg(not(target_arch = "wasm32"))]
pub use infra::stream::{
    SetlistStreamApi, SetlistStateProvider, SetlistUpdateMessage, SetlistCommandHandler,
    TransportCommand, NavigationCommand, SeekToSection, SeekToSong, SeekToTime, ToggleLoop,
};

#[cfg(feature = "dioxus")]
pub use infra::dioxus::{
    SETLIST, TRANSPORT_INFO, ProjectTransportInfo, CURRENT_POSITION_SECONDS, ACTIVE_SLIDE_INDEX,
    SONG_TRACKS, SONG_TRANSPORT, SETLIST_STRUCTURE, ACTIVE_INDICES,
};

pub use core::{
    Section, SectionType, Setlist, SetlistApi, SetlistEntry, SetlistError, SetlistOrder, SetlistSummary, Song,
    SongSummary,
};
pub use reactive::{
    SetlistReactiveStreams, SetlistStreams, SongStreams, LyricsStreams, ActiveIndicesStreams, EventStreamSubject,
    SetlistReactiveService, SetlistReactiveState,
};

#[cfg(not(target_arch = "wasm32"))]
pub use reactive::irpc::{SetlistReactiveProtocol, SetlistReactiveApi};

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
    SectionType::parse(input)
        .map_err(|e| SetlistError::invalid_section(&e))
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

