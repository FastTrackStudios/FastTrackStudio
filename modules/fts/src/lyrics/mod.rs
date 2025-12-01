//! Lyrics sync engine for FastTrackStudio
//!
//! This module provides functionality for parsing, managing, and exporting lyrics
//! with integration to the setlist system for automatic section matching.

pub mod core;
pub mod parser;
pub mod output;
pub mod syllables;
pub mod source;
pub mod infra;

#[cfg(not(target_arch = "wasm32"))]
pub mod reactive;

pub use core::*;
pub use parser::{parse_lyrics, parse_lyrics_with_config, ParserConfig, BackgroundVocalPattern, ParseError};
pub use output::*;
pub use syllables::*;
pub use source::*;

#[cfg(not(target_arch = "wasm32"))]
pub use reactive::{
    LyricsStreams, LyricsReactiveState, LyricsReactiveService,
    DefaultLyricsReactiveService, EventStreamSubject,
};

#[cfg(not(target_arch = "wasm32"))]
pub use reactive::irpc::{LyricsApi, LyricsProtocol, LyricsUpdateMessage};

