//! Lyrics sync engine for FastTrackStudio
//!
//! This module provides functionality for parsing, managing, and exporting lyrics
//! with integration to the setlist system for automatic section matching.

pub mod core;
pub mod infra;
pub mod output;
pub mod parser;
pub mod source;
pub mod syllables;

#[cfg(feature = "iroh")]
pub mod reactive;

pub use core::*;
pub use output::*;
pub use parser::{
    BackgroundVocalPattern, ParseError, ParserConfig, parse_lyrics, parse_lyrics_with_config,
};
pub use source::*;
pub use syllables::*;

#[cfg(all(feature = "iroh", not(target_arch = "wasm32")))]
pub use reactive::{
    DefaultLyricsReactiveService, EventStreamSubject, LyricsReactiveService, LyricsReactiveState,
    LyricsStreams,
};

#[cfg(all(feature = "iroh", not(target_arch = "wasm32")))]
pub use reactive::irpc::{LyricsApi, LyricsProtocol, LyricsUpdateMessage};
