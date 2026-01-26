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
    /// Start measure number (0-based)
    pub start_measure: Option<i32>,
    /// End measure number (0-based)
    pub end_measure: Option<i32>,
}

/// Measure position information for RPC communication.
#[derive(Debug, Clone, Facet)]
pub struct MeasureInfo {
    /// Measure number (0-based internally, display as 1-based)
    pub measure: i32,
    /// Time position in seconds (relative to song start)
    pub time_seconds: f64,
    /// Time signature numerator
    pub time_sig_numerator: i32,
    /// Time signature denominator
    pub time_sig_denominator: i32,
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
    /// Whether playback is currently active
    pub is_playing: bool,
    /// Whether looping is enabled
    pub looping: bool,
    /// Loop region (if set) - start and end positions
    pub loop_selection: Option<daw::primitives::TimeSelection>,
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
    /// Toggle loop for the current song (sets loop to song boundaries)
    ToggleSongLoop,
    /// Toggle loop for the current section (sets loop to section boundaries)
    ToggleSectionLoop,
    /// Set custom loop region (start and end in seconds, relative to song start)
    SetLoopRegion { start_seconds: f64, end_seconds: f64 },
    /// Clear the loop region
    ClearLoop,
    /// Toggle playback (play/pause)
    TogglePlayback,
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

    /// Get measure positions for a specific song
    async fn get_measures(&self, song_index: usize) -> Vec<MeasureInfo>;

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

// region:    --- Type Conversions

use super::core::{Section, Setlist, Song};

impl SetlistInfo {
    /// Convert from domain Setlist to RPC SetlistInfo
    pub fn from_setlist(s: &Setlist) -> Self {
        Self {
            name: s.name.clone(),
            song_count: s.songs.len(),
            total_duration: s.total_duration(),
        }
    }
}

impl SongInfo {
    /// Convert from domain Song to RPC SongInfo
    pub fn from_song(index: usize, song: &Song) -> Self {
        Self {
            index,
            name: song.name.clone(),
            start: song.effective_start(),
            end: song.effective_end(),
            duration: song.duration(),
            tempo: song.starting_tempo,
            time_signature: song
                .starting_time_signature
                .map(|ts| format!("{}/{}", ts.numerator, ts.denominator)),
            section_count: song.sections.len(),
            has_lyrics: song.lyrics.is_some(),
            has_chart: song.chart.is_some(),
        }
    }
}

impl SectionInfo {
    /// Convert from domain Section to RPC SectionInfo
    pub fn from_section(index: usize, section: &Section) -> Self {
        Self {
            index,
            section_type: section.section_type.full_name(),
            name: section.display_name(),
            start: section.start_seconds().unwrap_or(0.0),
            end: section.end_seconds().unwrap_or(0.0),
            duration: section.duration_seconds().unwrap_or(0.0),
            number: section.number,
            start_measure: section.start_position.as_ref().map(|p| p.musical.measure),
            end_measure: section.end_position.as_ref().map(|p| p.musical.measure),
        }
    }
}

// endregion: --- Type Conversions

// region:    --- Local Client

use roam::Context;
use std::sync::Arc;

/// Create a dummy context for local/in-process calls.
///
/// This creates a minimal Context suitable for calling service methods
/// directly without going through the network transport. The context
/// values are placeholders since there's no actual RPC request.
fn local_context() -> Context {
    use roam::wire::{ConnectionId, MethodId, RequestId};
    Context::new(
        ConnectionId(0), // conn_id: no connection
        RequestId(0),    // request_id: no request
        MethodId(0),     // method_id: varies per call, but not used by handlers
        vec![],          // metadata: empty
        vec![],          // channels: no streams for simple calls
    )
}

/// A local client for in-process SetlistService calls.
///
/// This wraps any `SetlistService` implementation and provides a convenient
/// interface for calling service methods without needing a transport layer.
/// Use this for local/in-process usage (e.g., web app with MockSetlist).
///
/// # Why this exists
///
/// ROAM is designed for RPC communication over various transports (TCP, WebSocket,
/// shared memory). The `#[roam::service]` macro generates a `SetlistServiceClient<C>`
/// that works with any `Caller` implementation.
///
/// For **in-process** calls (like using MockSetlist in the web app), going through
/// ROAM's full serialization/deserialization pipeline is unnecessary overhead.
/// This `LocalSetlistClient` provides the same API as `SetlistServiceClient` but
/// calls the service implementation directly.
///
/// # Swappability
///
/// For remote calls (e.g., to REAPER via IPC), use:
/// ```ignore
/// let client = SetlistServiceClient::new(connection_handle);
/// ```
///
/// For local calls (e.g., mock in web app), use:
/// ```ignore
/// let client = LocalSetlistClient::new(Arc::new(MockSetlist::with_sample_data()));
/// ```
///
/// Both expose the same method signatures, enabling swappable service implementations.
#[derive(Clone)]
pub struct LocalSetlistClient<S: SetlistService + Send + Sync + 'static> {
    service: Arc<S>,
}

impl<S: SetlistService + Send + Sync + 'static> LocalSetlistClient<S> {
    /// Create a new local client wrapping the given service.
    pub fn new(service: Arc<S>) -> Self {
        Self { service }
    }

    /// Get the current setlist info
    pub async fn get_setlist(&self) -> Option<SetlistInfo> {
        self.service.get_setlist(&local_context()).await
    }

    /// Get all songs in the setlist
    pub async fn get_songs(&self) -> Vec<SongInfo> {
        self.service.get_songs(&local_context()).await
    }

    /// Get a specific song by index
    pub async fn get_song(&self, index: usize) -> Option<SongInfo> {
        self.service.get_song(&local_context(), index).await
    }

    /// Get sections for a specific song
    pub async fn get_sections(&self, song_index: usize) -> Vec<SectionInfo> {
        self.service.get_sections(&local_context(), song_index).await
    }

    /// Get a specific section
    pub async fn get_section(&self, song_index: usize, section_index: usize) -> Option<SectionInfo> {
        self.service
            .get_section(&local_context(), song_index, section_index)
            .await
    }

    /// Get measure positions for a specific song
    pub async fn get_measures(&self, song_index: usize) -> Vec<MeasureInfo> {
        self.service.get_measures(&local_context(), song_index).await
    }

    /// Get the current active indices
    pub async fn get_active_indices(&self) -> ActiveIndices {
        self.service.get_active_indices(&local_context()).await
    }

    /// Get the currently active song (if any)
    pub async fn get_active_song(&self) -> Option<SongInfo> {
        self.service.get_active_song(&local_context()).await
    }

    /// Get the currently active section (if any)
    pub async fn get_active_section(&self) -> Option<SectionInfo> {
        self.service.get_active_section(&local_context()).await
    }

    /// Get the song at a specific time position
    pub async fn get_song_at(&self, seconds: f64) -> Option<SongInfo> {
        self.service.get_song_at(&local_context(), seconds).await
    }

    /// Get the section at a specific time position within a song
    pub async fn get_section_at(&self, seconds: f64) -> Option<SectionInfo> {
        self.service.get_section_at(&local_context(), seconds).await
    }

    /// Execute a setlist command
    pub async fn execute(&self, cmd: SetlistCommand) {
        self.service.execute(&local_context(), cmd).await;
    }

    /// Subscribe to setlist events
    pub async fn subscribe(&self, events: Tx<SetlistEvent>) {
        self.service.subscribe(&local_context(), events).await;
    }

    /// Subscribe to active indices changes (position updates)
    pub async fn subscribe_active(&self, indices: Tx<ActiveIndices>) {
        self.service.subscribe_active(&local_context(), indices).await;
    }
}

// Implement PartialEq for Dioxus component props
impl<S: SetlistService + Send + Sync + 'static> PartialEq for LocalSetlistClient<S> {
    fn eq(&self, other: &Self) -> bool {
        // Compare by Arc pointer - same service instance means equal
        Arc::ptr_eq(&self.service, &other.service)
    }
}

// endregion: --- Local Client
