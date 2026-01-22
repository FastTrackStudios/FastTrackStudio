//! TrackService - Roam service for track operations
//!
//! This service provides RPC access to track state and operations.
//! Commands are executed via the service implementation (Mock or REAPER).
//!
//! Note: This module uses simplified RPC types (TrackInfo, TrackSoloMode) that can be
//! easily serialized. Service implementations convert between these and the full
//! domain types (Track, SoloMode) as needed.

use facet::Facet;
use roam::Tx;

// region:    --- RPC Types

/// Simplified track information for RPC communication.
///
/// This is a flat DTO optimized for serialization. For the full domain type
/// with all REAPER fields, see `Track`.
#[derive(Debug, Clone, Facet)]
pub struct TrackInfo {
    /// Track GUID (unique identifier)
    pub guid: String,
    /// Track name
    pub name: String,
    /// Track index in project
    pub index: usize,
    /// Volume (0.0 to 4.0, where 1.0 = 0dB)
    pub volume: f64,
    /// Pan (-1.0 = left, 0.0 = center, 1.0 = right)
    pub pan: f64,
    /// Whether the track is muted
    pub muted: bool,
    /// Solo mode (0=off, 1=solo, 2=solo-in-place, 5=safe-solo, 6=safe-sip)
    pub solo: u8,
    /// Whether this is a folder track
    pub is_folder: bool,
    /// Folder depth (0 = top level)
    pub folder_depth: u32,
    /// Whether the folder is collapsed (for folder tracks)
    pub folder_collapsed: bool,
    /// Whether the track is selected
    pub selected: bool,
    /// Track color as RGB (0xRRGGBB)
    pub color: Option<u32>,
}

/// Commands that can be executed on tracks
#[repr(u8)]
#[derive(Debug, Clone, Facet)]
pub enum TrackCommand {
    /// Set the mute state of a track
    SetMute { guid: String, muted: bool },
    /// Set the solo state of a track (0=off, 1=solo, 2=sip, 5=safe, 6=safe-sip)
    SetSolo { guid: String, solo: u8 },
    /// Set the volume of a track (0.0 to 4.0)
    SetVolume { guid: String, volume: f64 },
    /// Set the pan of a track (-1.0 to 1.0)
    SetPan { guid: String, pan: f64 },
    /// Set the name of a track
    SetName { guid: String, name: String },
    /// Toggle the mute state of a track
    ToggleMute { guid: String },
    /// Toggle the solo state of a track
    ToggleSolo { guid: String },
    /// Select a track (optionally exclusively)
    Select { guid: String, exclusive: bool },
    /// Deselect a track
    Deselect { guid: String },
    /// Toggle folder collapse state
    ToggleFolderCollapse { guid: String },
}

// endregion: --- RPC Types

// region:    --- Events

/// Events emitted by the track service
#[repr(u8)]
#[derive(Debug, Clone, Facet)]
pub enum TrackEvent {
    /// A track was added
    TrackAdded(TrackInfo),
    /// A track was removed (by GUID)
    TrackRemoved(String),
    /// A track's properties changed
    TrackChanged(TrackInfo),
    /// Multiple tracks changed (bulk update)
    TracksChanged(Vec<TrackInfo>),
    /// Track selection changed (list of selected GUIDs)
    SelectionChanged(Vec<String>),
    /// A track's mute state changed
    MuteChanged { guid: String, muted: bool },
    /// A track's solo state changed
    SoloChanged { guid: String, solo: u8 },
    /// A track's volume changed
    VolumeChanged { guid: String, volume: f64 },
    /// A track's pan changed
    PanChanged { guid: String, pan: f64 },
}

// endregion: --- Events

// region:    --- Service Trait

/// TrackService provides RPC access to track operations.
///
/// This trait defines the contract for track management services.
/// Implementations can be for REAPER, mock testing, or other DAWs.
///
/// Note: This service uses simplified RPC types. Implementations should
/// convert between TrackInfo and the full Track domain type.
#[roam::service]
pub trait TrackService {
    /// Get all tracks in the current project
    async fn get_tracks(&self) -> Vec<TrackInfo>;

    /// Get a specific track by GUID
    async fn get_track(&self, guid: String) -> Option<TrackInfo>;

    /// Get tracks by name (partial match)
    async fn find_tracks_by_name(&self, name: String) -> Vec<TrackInfo>;

    /// Get selected tracks
    async fn get_selected_tracks(&self) -> Vec<TrackInfo>;

    /// Get the number of tracks
    async fn track_count(&self) -> usize;

    /// Execute a track command
    async fn execute(&self, cmd: TrackCommand);

    /// Subscribe to track events
    async fn subscribe(&self, events: Tx<TrackEvent>);

    /// Check if a track is muted
    async fn is_muted(&self, guid: String) -> bool;

    /// Check if a track is soloed
    async fn is_soloed(&self, guid: String) -> bool;

    /// Get a track's volume (0.0 to 4.0)
    async fn get_volume(&self, guid: String) -> f64;

    /// Get a track's pan (-1.0 to 1.0)
    async fn get_pan(&self, guid: String) -> f64;
}

// endregion: --- Service Trait
