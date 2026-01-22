//! SetlistService - Roam service for setlist operations
//!
//! This service provides RPC access to setlist state and navigation.
//! Commands are executed via the service implementation (Mock or REAPER).
//!
//! Note: This module uses simplified RPC types that can be easily serialized.
//! Service implementations convert between these and the full domain types.

use facet::Facet;
use roam::Tx;

// region:    --- RPC Types

/// Simplified setlist information for RPC communication.
#[derive(Debug, Clone, Facet)]
pub struct SetlistInfo {
    /// Setlist name
    pub name: String,
    /// Number of songs
    pub song_count: usize,
    /// Total duration in seconds
    pub total_duration: f64,
}

/// Simplified song information for RPC communication.
#[derive(Debug, Clone, Facet)]
pub struct SongInfo {
    /// Song index in setlist
    pub index: usize,
    /// Song name
    pub name: String,
    /// Start time in seconds (effective start, accounting for count-in)
    pub start: f64,
    /// End time in seconds (effective end)
    pub end: f64,
    /// Duration in seconds
    pub duration: f64,
    /// Starting tempo (BPM)
    pub tempo: Option<f64>,
    /// Starting time signature as "numerator/denominator"
    pub time_signature: Option<String>,
    /// Number of sections
    pub section_count: usize,
    /// Whether this song has lyrics
    pub has_lyrics: bool,
    /// Whether this song has a chart
    pub has_chart: bool,
}

/// Simplified section information for RPC communication.
#[derive(Debug, Clone, Facet)]
pub struct SectionInfo {
    /// Section index within the song
    pub index: usize,
    /// Section type (e.g., "Verse", "Chorus", "Bridge")
    pub section_type: String,
    /// Section name (e.g., "Verse 1", "Chorus")
    pub name: String,
    /// Start time in seconds (relative to project)
    pub start: f64,
    /// End time in seconds
    pub end: f64,
    /// Duration in seconds
    pub duration: f64,
    /// Section number (e.g., 1 for "Verse 1")
    pub number: Option<u32>,
}

/// Active indices for current position in setlist
#[derive(Debug, Clone, Default, Facet)]
pub struct ActiveIndices {
    /// Currently active song index (None if between songs)
    pub song_index: Option<usize>,
    /// Currently active section index within the song
    pub section_index: Option<usize>,
    /// Currently active slide/lyric index
    pub slide_index: Option<usize>,
    /// Progress through the current song (0.0 to 1.0)
    pub song_progress: Option<f64>,
    /// Progress through the current section (0.0 to 1.0)
    pub section_progress: Option<f64>,
}

/// Commands that can be executed on the setlist
#[repr(u8)]
#[derive(Debug, Clone, Facet)]
pub enum SetlistCommand {
    /// Navigate to a specific song by index
    GoToSong { index: usize },
    /// Navigate to the next song
    NextSong,
    /// Navigate to the previous song
    PreviousSong,
    /// Navigate to a specific section within the current song
    GoToSection { index: usize },
    /// Navigate to the next section
    NextSection,
    /// Navigate to the previous section
    PreviousSection,
    /// Navigate to a specific time position
    SeekTo { seconds: f64 },
    /// Toggle loop for the current song
    ToggleSongLoop,
    /// Toggle loop for the current section
    ToggleSectionLoop,
}

// endregion: --- RPC Types

// region:    --- Events

/// Events emitted by the setlist service
#[repr(u8)]
#[derive(Debug, Clone, Facet)]
pub enum SetlistEvent {
    /// The setlist structure changed
    SetlistChanged(SetlistInfo),
    /// Active indices changed (song/section/slide position)
    ActiveIndicesChanged(ActiveIndices),
    /// A song was entered
    SongEntered { index: usize, song: SongInfo },
    /// A song was exited
    SongExited { index: usize },
    /// A section was entered
    SectionEntered {
        song_index: usize,
        section_index: usize,
        section: SectionInfo,
    },
    /// A section was exited
    SectionExited {
        song_index: usize,
        section_index: usize,
    },
    /// Playback position updated (high frequency)
    PositionChanged {
        seconds: f64,
        indices: ActiveIndices,
    },
}

// endregion: --- Events

// region:    --- Service Trait

/// SetlistService provides RPC access to setlist operations.
///
/// This trait defines the contract for setlist management services.
/// Implementations can be for REAPER, mock testing, or standalone apps.
#[roam::service]
pub trait SetlistService {
    /// Get the current setlist info
    async fn get_setlist(&self) -> Option<SetlistInfo>;

    /// Get all songs in the setlist
    async fn get_songs(&self) -> Vec<SongInfo>;

    /// Get a specific song by index
    async fn get_song(&self, index: usize) -> Option<SongInfo>;

    /// Get sections for a specific song
    async fn get_sections(&self, song_index: usize) -> Vec<SectionInfo>;

    /// Get a specific section
    async fn get_section(&self, song_index: usize, section_index: usize) -> Option<SectionInfo>;

    /// Get the current active indices
    async fn get_active_indices(&self) -> ActiveIndices;

    /// Get the currently active song (if any)
    async fn get_active_song(&self) -> Option<SongInfo>;

    /// Get the currently active section (if any)
    async fn get_active_section(&self) -> Option<SectionInfo>;

    /// Get the song at a specific time position
    async fn get_song_at(&self, seconds: f64) -> Option<SongInfo>;

    /// Get the section at a specific time position within a song
    async fn get_section_at(&self, seconds: f64) -> Option<SectionInfo>;

    /// Execute a setlist command
    async fn execute(&self, cmd: SetlistCommand);

    /// Subscribe to setlist events
    async fn subscribe(&self, events: Tx<SetlistEvent>);

    /// Subscribe to active indices changes (position updates)
    async fn subscribe_active(&self, indices: Tx<ActiveIndices>);
}

// endregion: --- Service Trait
