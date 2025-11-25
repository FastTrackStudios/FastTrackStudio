//! Lyrics sync engine for FastTrackStudio
//!
//! This module provides functionality for parsing, managing, and exporting lyrics
//! with integration to the setlist system for automatic section matching.

pub mod core;
pub mod parser;
pub mod output;
pub mod sync;
pub mod syllables;

pub use core::*;
pub use parser::{parse_lyrics, parse_lyrics_with_config, ParserConfig, BackgroundVocalPattern, ParseError};
pub use output::*;
pub use sync::*;
pub use syllables::*;
