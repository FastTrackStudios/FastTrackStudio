//! Snapshot domain model
//! 
//! Snapshots represent saved states of track visibility and layouts.
//! They can be saved/restored independently for TCP and MCP.

use super::track_scope::TrackIdentifier;

/// Unique identifier for a snapshot
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SnapshotId(pub String);

impl SnapshotId {
    pub fn new(id: impl Into<String>) -> Self {
        Self(id.into())
    }
    
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<&str> for SnapshotId {
    fn from(s: &str) -> Self {
        Self::new(s)
    }
}

/// Type of snapshot (TCP or MCP)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SnapshotType {
    /// Track Control Panel (arrange view)
    Tcp,
    
    /// Mixer Control Panel (mixer view)
    Mcp,
}

impl SnapshotType {
    pub fn as_str(&self) -> &'static str {
        match self {
            SnapshotType::Tcp => "TCP",
            SnapshotType::Mcp => "MCP",
        }
    }
    
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_uppercase().as_str() {
            "TCP" => Some(SnapshotType::Tcp),
            "MCP" => Some(SnapshotType::Mcp),
            _ => None,
        }
    }
}

/// Metadata about a snapshot
#[derive(Debug, Clone)]
pub struct SnapshotMetadata {
    /// Snapshot name
    pub name: String,
    
    /// Snapshot type (TCP or MCP)
    pub snapshot_type: SnapshotType,
    
    /// Group this snapshot belongs to
    pub group_id: String,
    
    /// Icon identifier (optional)
    pub icon: Option<String>,
    
    /// Creation timestamp (optional)
    pub created_at: Option<String>,
    
    /// Last modified timestamp (optional)
    pub modified_at: Option<String>,
    
    /// Description/notes
    pub description: Option<String>,
}

/// Represents a snapshot of track states
#[derive(Debug, Clone)]
pub struct Snapshot {
    /// Unique identifier
    pub id: SnapshotId,
    
    /// Metadata
    pub metadata: SnapshotMetadata,
    
    /// Tracks included in this snapshot
    pub tracks: Vec<TrackIdentifier>,
    
    /// Chunk data for each track (DAW-agnostic representation)
    /// This will be handled by intelligent chunk management
    pub track_data: Vec<TrackSnapshotData>,
}

/// Represents snapshot data for a single track
#[derive(Debug, Clone)]
pub struct TrackSnapshotData {
    /// Track identifier
    pub track: TrackIdentifier,
    
    /// Raw chunk data (if needed for full save/restore)
    pub raw_chunk: Option<String>,
    
    /// Intelligent chunk representation (structured data)
    pub intelligent_data: Option<IntelligentChunkData>,
    
    /// Visibility state
    pub visible_tcp: Option<bool>,
    pub visible_mcp: Option<bool>,
    
    /// Height states
    pub tcp_height: Option<i32>,
    pub mcp_height: Option<f64>,
    
    /// Folder states
    pub folder_compact_tcp: Option<i32>,
    pub folder_compact_mcp: Option<i32>,
    
    /// Layout identifiers
    pub tcp_layout: Option<String>,
    pub mcp_layout: Option<String>,
}

/// Intelligent chunk data - structured representation instead of raw chunk
#[derive(Debug, Clone)]
pub struct IntelligentChunkData {
    /// Track parameters that matter for visibility/layout
    pub visibility: VisibilityState,
    pub layout: LayoutState,
    pub routing: Option<RoutingState>,
    pub fx: Option<FxState>,
}

/// Visibility state for a track
#[derive(Debug, Clone)]
pub struct VisibilityState {
    pub tcp: bool,
    pub mcp: bool,
    pub height_locked: bool,
}

/// Layout state for a track
#[derive(Debug, Clone)]
pub struct LayoutState {
    pub tcp_layout: Option<String>,
    pub mcp_layout: Option<String>,
    pub tcp_height: Option<i32>,
    pub mcp_height: Option<f64>,
}

/// Routing state (optional, for advanced snapshot features)
#[derive(Debug, Clone)]
pub struct RoutingState {
    pub sends: Vec<SendInfo>,
    pub receives: Vec<ReceiveInfo>,
}

/// Send information
#[derive(Debug, Clone)]
pub struct SendInfo {
    pub destination: TrackIdentifier,
    pub level: f64,
    pub pan: f64,
    pub muted: bool,
}

/// Receive information
#[derive(Debug, Clone)]
pub struct ReceiveInfo {
    pub source: TrackIdentifier,
    pub level: f64,
    pub pan: f64,
    pub muted: bool,
}

/// FX state (optional, for advanced snapshot features)
#[derive(Debug, Clone)]
pub struct FxState {
    pub enabled: bool,
    pub chain: Vec<FxItem>,
}

/// FX item information
#[derive(Debug, Clone)]
pub struct FxItem {
    pub name: String,
    pub enabled: bool,
    pub bypassed: bool,
}

