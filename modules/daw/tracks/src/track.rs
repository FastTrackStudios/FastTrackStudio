use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use crate::api::folder::{TcpFolderState, McpFolderState};
use crate::item::Item;

/// Represents a track in a project
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Track {
    /// Unique identifier for this track
    pub id: Option<uuid::Uuid>,
    /// Track name
    pub name: String,
    /// Track index in the project
    pub index: Option<usize>,
    /// Track GUID (from REAPER)
    pub guid: Option<String>,
    /// Volume (0.0 to 1.0, or in REAPER units)
    pub volume: f64,
    /// Pan (-1.0 to 1.0, or in REAPER units)
    pub pan: f64,
    /// Width (stereo width, typically 0.0 to 1.0)
    pub width: f64,
    /// Whether the track is muted
    pub muted: bool,
    /// Whether the track is soloed
    pub soloed: bool,
    /// Whether the track is record armed
    pub record_armed: bool,
    /// Input monitoring mode
    pub input_monitoring_mode: Option<String>,
    /// Recording mode
    pub recording_mode: Option<String>,
    /// Whether the track has FX enabled
    pub has_fx: bool,
    /// Whether the track is selected
    pub selected: bool,
    /// Track color (RGB value)
    pub color: Option<u32>,
    /// Folder state for TCP (Track Control Panel)
    pub folder_state_tcp: Option<TcpFolderState>,
    /// Folder state for MCP (Mixer Control Panel)
    pub folder_state_mcp: Option<McpFolderState>,
    /// Track depth (for folder hierarchy)
    pub track_depth: i32,
    /// Whether this track is a folder
    pub is_folder: bool,
    /// Optional metadata
    pub metadata: HashMap<String, String>,
    /// Items on this track (audio and MIDI items)
    pub items: Vec<Item>,
}

impl Track {
    /// Create a new track
    pub fn new(name: String) -> Self {
        Self {
            id: None,
            name,
            index: None,
            guid: None,
            volume: 1.0,
            pan: 0.0,
            width: 1.0,
            muted: false,
            soloed: false,
            record_armed: false,
            input_monitoring_mode: None,
            recording_mode: None,
            has_fx: false,
            selected: false,
            color: None,
            folder_state_tcp: None,
            folder_state_mcp: None,
            track_depth: 0,
            is_folder: false,
            metadata: HashMap::new(),
            items: Vec::new(),
        }
    }

    /// Create a new track with ID
    pub fn with_id(id: uuid::Uuid, name: String) -> Self {
        let mut track = Self::new(name);
        track.id = Some(id);
        track
    }

    /// Get metadata value
    pub fn get_metadata(&self, key: &str) -> Option<&String> {
        self.metadata.get(key)
    }

    /// Set metadata value
    pub fn set_metadata<K: Into<String>, V: Into<String>>(&mut self, key: K, value: V) {
        self.metadata.insert(key.into(), value.into());
    }

    /// Remove metadata value
    pub fn remove_metadata(&mut self, key: &str) -> Option<String> {
        self.metadata.remove(key)
    }
}

