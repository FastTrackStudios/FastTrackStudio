//! Trait definitions for lyrics infrastructure
//!
//! These traits define abstract interfaces for DAW-agnostic lyrics operations.
//! Implementations are provided in the `reaper` module behind a feature flag.

use crate::lyrics::core::Lyrics;

/// Trait for reading lyrics from a DAW project
pub trait LyricsReader {
    /// Read lyrics from a project
    fn read_lyrics_from_project(
        &self,
        project: &dyn std::any::Any,
    ) -> Result<LyricsData, LyricsReadError>;
}

/// Trait for writing lyrics to a DAW project
pub trait LyricsWriter {
    /// Write lyrics to a project
    fn write_lyrics_to_project(
        &self,
        project: &dyn std::any::Any,
        lyrics: &Lyrics,
    ) -> Result<(), LyricsWriteError>;

    /// Update lyrics in a project for a specific song
    fn update_lyrics_in_project(
        &self,
        song_index: usize,
        lyrics: Lyrics,
    ) -> Result<(), LyricsWriteError>;
}

/// Data structure for lyrics read from a DAW
#[derive(Debug, Clone)]
pub struct LyricsData {
    pub slides: Vec<SlideData>,
    pub midi_tracks: Vec<MidiTrackData>,
}

#[derive(Debug, Clone)]
pub struct SlideData {
    pub text: String,
    pub position: f64,
    pub length: f64,
}

#[derive(Debug, Clone)]
pub struct MidiTrackData {
    pub name: String,
    pub items: Vec<MidiItemData>,
}

#[derive(Debug, Clone)]
pub struct MidiItemData {
    pub position: f64,
    pub length: f64,
    pub events: Vec<MidiEventData>,
}

#[derive(Debug, Clone)]
pub struct MidiEventData {
    pub offset_ticks: i32,
    pub flag: u8,
    pub message: Vec<u8>,
}

#[derive(Debug, thiserror::Error)]
pub enum LyricsReadError {
    #[error("LYRICS folder not found")]
    LyricsFolderNotFound,
    #[error("SLIDES track not found in LYRICS folder")]
    SlidesTrackNotFound,
    #[error("Failed to read MIDI events")]
    MidiReadFailed,
}

#[derive(Debug, thiserror::Error)]
pub enum LyricsWriteError {
    #[error("Failed to write lyrics: {0}")]
    WriteFailed(String),
    #[error("Project not found: {0}")]
    ProjectNotFound(String),
}
