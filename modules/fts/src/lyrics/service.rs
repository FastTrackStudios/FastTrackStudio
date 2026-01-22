//! LyricsService - Roam service for lyrics operations
//!
//! This service provides RPC access to lyrics display and synchronization.
//! Commands are executed via the service implementation.
//!
//! Note: This module uses simplified RPC types that can be easily serialized.

use facet::Facet;
use roam::Tx;

// region:    --- RPC Types

/// Simplified lyrics information for RPC communication.
#[derive(Debug, Clone, Facet)]
pub struct LyricsInfo {
    /// Song name these lyrics belong to
    pub song_name: String,
    /// Number of sections
    pub section_count: usize,
    /// Total number of lines across all sections
    pub line_count: usize,
    /// Whether lyrics have timing information
    pub has_timing: bool,
}

/// A lyric section (verse, chorus, etc.)
#[derive(Debug, Clone, Facet)]
pub struct LyricSectionInfo {
    /// Section index
    pub index: usize,
    /// Section name (e.g., "Verse 1", "Chorus")
    pub name: String,
    /// Start time in seconds (if timed)
    pub start: Option<f64>,
    /// End time in seconds (if timed)
    pub end: Option<f64>,
    /// Lines in this section
    pub lines: Vec<LyricLineInfo>,
}

/// A single line of lyrics
#[derive(Debug, Clone, Facet)]
pub struct LyricLineInfo {
    /// Line index within the section
    pub index: usize,
    /// Full text of the line
    pub text: String,
    /// Start time in seconds (if timed)
    pub start: Option<f64>,
    /// End time in seconds (if timed)
    pub end: Option<f64>,
    /// Whether this is a background vocal line
    pub is_background: bool,
}

/// Current active lyric state
#[derive(Debug, Clone, Default, Facet)]
pub struct ActiveLyricState {
    /// Currently active section index
    pub section_index: Option<usize>,
    /// Currently active line index within the section
    pub line_index: Option<usize>,
    /// Currently active word index within the line (for word-by-word highlighting)
    pub word_index: Option<usize>,
    /// The current line text (for convenience)
    pub current_line: Option<String>,
    /// The next line text (for teleprompter view)
    pub next_line: Option<String>,
}

/// Commands that can be executed on lyrics
#[repr(u8)]
#[derive(Debug, Clone, Facet)]
pub enum LyricsCommand {
    /// Load lyrics for a specific song
    LoadForSong { song_name: String },
    /// Clear current lyrics
    Clear,
    /// Jump to a specific section
    GoToSection { index: usize },
    /// Jump to a specific line
    GoToLine { section_index: usize, line_index: usize },
    /// Scroll lyrics by offset (for manual control)
    Scroll { offset: i32 },
}

// endregion: --- RPC Types

// region:    --- Events

/// Events emitted by the lyrics service
#[repr(u8)]
#[derive(Debug, Clone, Facet)]
pub enum LyricsEvent {
    /// Lyrics loaded for a song
    LyricsLoaded { song_name: String, lyrics: LyricsInfo },
    /// Lyrics cleared
    LyricsCleared,
    /// Active lyric state changed
    ActiveStateChanged(ActiveLyricState),
    /// Section entered
    SectionEntered { index: usize, section: LyricSectionInfo },
    /// Line entered (for highlighting)
    LineEntered { section_index: usize, line_index: usize, text: String },
    /// Word entered (for word-by-word highlighting)
    WordEntered { section_index: usize, line_index: usize, word_index: usize },
}

// endregion: --- Events

// region:    --- Service Trait

/// LyricsService provides RPC access to lyrics display and synchronization.
///
/// This trait defines the contract for lyrics services.
#[roam::service]
pub trait LyricsService {
    /// Get lyrics info for the current song
    async fn get_lyrics(&self) -> Option<LyricsInfo>;

    /// Get lyrics info for a specific song
    async fn get_lyrics_for_song(&self, song_name: String) -> Option<LyricsInfo>;

    /// Get all sections for current lyrics
    async fn get_sections(&self) -> Vec<LyricSectionInfo>;

    /// Get a specific section
    async fn get_section(&self, index: usize) -> Option<LyricSectionInfo>;

    /// Get all lines for a section
    async fn get_lines(&self, section_index: usize) -> Vec<LyricLineInfo>;

    /// Get the current active lyric state
    async fn get_active_state(&self) -> ActiveLyricState;

    /// Get the line at a specific time position
    async fn get_line_at(&self, seconds: f64) -> Option<LyricLineInfo>;

    /// Execute a lyrics command
    async fn execute(&self, cmd: LyricsCommand);

    /// Subscribe to lyrics events
    async fn subscribe(&self, events: Tx<LyricsEvent>);

    /// Subscribe to active state changes
    async fn subscribe_active(&self, state: Tx<ActiveLyricState>);
}

// endregion: --- Service Trait
