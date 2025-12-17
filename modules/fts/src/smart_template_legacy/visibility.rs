//! Visibility Manager abstraction
//!
//! Provides a trait-based interface for track visibility and parameter management
//! that can be implemented for different DAWs (REAPER, etc.)

use std::collections::HashMap;

/// Track identifier - platform-specific (e.g., MediaTrack* pointer in REAPER, GUID string, etc.)
pub type TrackId = String;

/// Envelope information for a track
#[derive(Debug, Clone, PartialEq)]
pub struct EnvelopeState {
    /// Envelope name
    pub name: String,
    
    /// Envelope height in TCP
    pub height: f64,
    
    /// Whether envelope is visible/active
    pub active: bool,
}

/// Send configuration between tracks
#[derive(Debug, Clone, PartialEq)]
pub struct SendConfig {
    /// Destination track ID
    pub destination_track_id: TrackId,
    
    /// Send chunk data (platform-specific format)
    pub chunk_data: Vec<String>,
}

/// Receive configuration from tracks
#[derive(Debug, Clone, PartialEq)]
pub struct ReceiveConfig {
    /// Source track ID
    pub source_track_id: TrackId,
    
    /// Receive chunk data (platform-specific format)
    pub chunk_data: Vec<String>,
}

/// Complete track parameter state
#[derive(Debug, Clone, PartialEq)]
pub struct TrackParameters {
    /// Track name
    pub name: String,
    
    /// Track ID (platform-specific identifier)
    pub track_id: TrackId,
    
    // Visibility
    /// TCP (Track Control Panel) visibility
    pub tcp_visibility: bool,
    
    /// MCP (Mixer Control Panel) visibility
    pub mcp_visibility: bool,
    
    // Heights
    /// TCP height in pixels
    pub tcp_height: f64,
    
    /// MCP height (0.0 to 1.0, normalized)
    pub mcp_height: Option<f64>,
    
    /// Height lock state
    pub height_lock: bool,
    
    /// Height override value
    pub height_override: Option<i32>,
    
    // Folder states
    /// Folder depth (0 = not a folder, >0 = folder depth)
    pub folder_depth: i32,
    
    /// TCP folder compact state (0 = unfolded, 1 = compact, 2 = compacted)
    pub tcp_folder_state: i32,
    
    /// MCP folder state (from BUSCOMP in chunk)
    pub mcp_folder_state: Option<i32>,
    
    // Layouts
    /// TCP layout name
    pub tcp_layout: String,
    
    /// MCP layout name
    pub mcp_layout: String,
    
    // Envelopes
    /// Envelope states
    pub envelopes: Vec<EnvelopeState>,
    
    // Track type/color (from chunk or track properties)
    /// Track color (ARGB format)
    pub color: Option<u32>,
    
    /// Track type (e.g., "BUS", "SUM", "AUX")
    pub track_type: Option<String>,
    
    // Full state chunk (platform-specific)
    /// Complete track state chunk (for full state restoration)
    pub chunk: Option<String>,
}

impl Default for TrackParameters {
    fn default() -> Self {
        Self {
            name: String::new(),
            track_id: String::new(),
            tcp_visibility: true,
            mcp_visibility: true,
            tcp_height: 0.0,
            mcp_height: None,
            height_lock: false,
            height_override: None,
            folder_depth: 0,
            tcp_folder_state: 0,
            mcp_folder_state: None,
            tcp_layout: "Default".to_string(),
            mcp_layout: "Default".to_string(),
            envelopes: Vec::new(),
            color: None,
            track_type: None,
            chunk: None,
        }
    }
}

/// Snapshot of track states for a group
#[derive(Debug, Clone)]
pub struct TrackSnapshot {
    /// Snapshot name
    pub name: String,
    
    /// Group name this snapshot belongs to
    pub group: String,
    
    /// Sub-group type ("TCP" or "MCP")
    pub sub_group: String,
    
    /// Track IDs in this snapshot
    pub track_ids: Vec<TrackId>,
    
    /// Track parameters indexed by track ID
    pub track_parameters: HashMap<TrackId, TrackParameters>,
    
    /// Send configurations: source_track_id -> dest_track_id -> [send chunks]
    pub sends: HashMap<TrackId, HashMap<TrackId, Vec<String>>>,
    
    /// Receive configurations: dest_track_id -> source_track_id -> [receive chunks]
    pub receives: HashMap<TrackId, HashMap<TrackId, Vec<String>>>,
    
    /// Whether tracks in snapshot are selected
    pub selected: bool,
    
    /// Whether snapshot is visible
    pub visible: bool,
    
    /// Whether any tracks are missing
    pub miss_track: bool,
    
    /// Vertical zoom level (platform-specific)
    pub vertical_zoom: Option<i32>,
    
    /// Icon for UI display
    pub icon: Option<String>,
}

impl TrackSnapshot {
    /// Create a new empty snapshot
    pub fn new(name: impl Into<String>, group: impl Into<String>, sub_group: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            group: group.into(),
            sub_group: sub_group.into(),
            track_ids: Vec::new(),
            track_parameters: HashMap::new(),
            sends: HashMap::new(),
            receives: HashMap::new(),
            selected: false,
            visible: true,
            miss_track: false,
            vertical_zoom: None,
            icon: None,
        }
    }
    
    /// Add a track to the snapshot
    pub fn add_track(&mut self, track_id: TrackId, params: TrackParameters) {
        if !self.track_ids.contains(&track_id) {
            self.track_ids.push(track_id.clone());
        }
        self.track_parameters.insert(track_id, params);
    }
    
    /// Add a send configuration
    pub fn add_send(&mut self, source_track_id: TrackId, dest_track_id: TrackId, chunk_data: Vec<String>) {
        self.sends
            .entry(source_track_id)
            .or_insert_with(HashMap::new)
            .entry(dest_track_id)
            .or_insert_with(Vec::new)
            .extend(chunk_data);
    }
    
    /// Add a receive configuration
    pub fn add_receive(&mut self, dest_track_id: TrackId, source_track_id: TrackId, chunk_data: Vec<String>) {
        self.receives
            .entry(dest_track_id)
            .or_insert_with(HashMap::new)
            .entry(source_track_id)
            .or_insert_with(Vec::new)
            .extend(chunk_data);
    }
}

/// Trait for visibility manager implementations
/// This allows different DAW backends (REAPER, etc.) to implement track visibility and parameter management
pub trait VisibilityManager {
    /// Error type for visibility operations
    type Error: std::error::Error + Send + Sync;
    
    // Track identification
    
    /// Get a track ID from a track name or identifier
    fn get_track_id(&self, track_identifier: &str) -> Result<TrackId, Self::Error>;
    
    /// Get all track IDs in the project
    fn get_all_track_ids(&self) -> Result<Vec<TrackId>, Self::Error>;
    
    /// Get child track IDs for a parent track
    fn get_child_track_ids(&self, parent_track_id: &TrackId) -> Result<Vec<TrackId>, Self::Error>;
    
    // Track parameter operations
    
    /// Get all parameters for a track
    fn get_track_parameters(&self, track_id: &TrackId) -> Result<TrackParameters, Self::Error>;
    
    /// Set all parameters for a track
    fn set_track_parameters(&self, track_id: &TrackId, params: &TrackParameters) -> Result<(), Self::Error>;
    
    /// Get track name
    fn get_track_name(&self, track_id: &TrackId) -> Result<String, Self::Error>;
    
    // Visibility operations
    
    /// Show a track in TCP
    fn show_track_tcp(&self, track_id: &TrackId) -> Result<(), Self::Error>;
    
    /// Hide a track in TCP
    fn hide_track_tcp(&self, track_id: &TrackId) -> Result<(), Self::Error>;
    
    /// Show a track in MCP
    fn show_track_mcp(&self, track_id: &TrackId) -> Result<(), Self::Error>;
    
    /// Hide a track in MCP
    fn hide_track_mcp(&self, track_id: &TrackId) -> Result<(), Self::Error>;
    
    /// Show multiple tracks
    fn show_tracks(&self, track_ids: &[TrackId]) -> Result<(), Self::Error>;
    
    /// Hide multiple tracks
    fn hide_tracks(&self, track_ids: &[TrackId]) -> Result<(), Self::Error>;
    
    /// Show all tracks
    fn show_all_tracks(&self) -> Result<(), Self::Error>;
    
    /// Hide all tracks
    fn hide_all_tracks(&self) -> Result<(), Self::Error>;
    
    // Track selection
    
    /// Select a track
    fn select_track(&self, track_id: &TrackId) -> Result<(), Self::Error>;
    
    /// Deselect a track
    fn deselect_track(&self, track_id: &TrackId) -> Result<(), Self::Error>;
    
    /// Select multiple tracks
    fn select_tracks(&self, track_ids: &[TrackId]) -> Result<(), Self::Error>;
    
    /// Deselect all tracks
    fn deselect_all_tracks(&self) -> Result<(), Self::Error>;
    
    /// Get selected track IDs
    fn get_selected_track_ids(&self) -> Result<Vec<TrackId>, Self::Error>;
    
    // Snapshot operations
    
    /// Save a snapshot of current track states
    fn save_snapshot(&self, snapshot: &mut TrackSnapshot) -> Result<(), Self::Error>;
    
    /// Load/apply a snapshot
    fn load_snapshot(&self, snapshot: &TrackSnapshot) -> Result<(), Self::Error>;
    
    // Send/Receive operations
    
    /// Get send configurations for a track
    fn get_track_sends(&self, track_id: &TrackId) -> Result<HashMap<TrackId, Vec<String>>, Self::Error>;
    
    /// Get receive configurations for a track
    fn get_track_receives(&self, track_id: &TrackId) -> Result<HashMap<TrackId, Vec<String>>, Self::Error>;
    
    /// Set send configurations for a track
    fn set_track_sends(&self, track_id: &TrackId, sends: &HashMap<TrackId, Vec<String>>) -> Result<(), Self::Error>;
    
    /// Set receive configurations for a track
    fn set_track_receives(&self, track_id: &TrackId, receives: &HashMap<TrackId, Vec<String>>) -> Result<(), Self::Error>;
    
    // Height operations
    
    /// Set TCP height for a track
    fn set_tcp_height(&self, track_id: &TrackId, height: f64) -> Result<(), Self::Error>;
    
    /// Set MCP height for a track
    fn set_mcp_height(&self, track_id: &TrackId, height: f64) -> Result<(), Self::Error>;
    
    /// Set tracks to minimum height
    fn set_tracks_minimum_height(&self, track_ids: &[TrackId]) -> Result<(), Self::Error>;
    
    // Folder operations
    
    /// Set folder state for a track
    fn set_folder_state(&self, track_id: &TrackId, depth: i32, tcp_compact: i32, mcp_compact: Option<i32>) -> Result<(), Self::Error>;
    
    /// Collapse/unfold a folder track
    fn set_folder_compact(&self, track_id: &TrackId, compact: bool) -> Result<(), Self::Error>;
    
    /// Collapse all top-level tracks
    fn collapse_top_level_tracks(&self) -> Result<(), Self::Error>;
    
    // Layout operations
    
    /// Get TCP layout for a track
    fn get_tcp_layout(&self, track_id: &TrackId) -> Result<String, Self::Error>;
    
    /// Get MCP layout for a track
    fn get_mcp_layout(&self, track_id: &TrackId) -> Result<String, Self::Error>;
    
    /// Set layouts for a track
    fn set_track_layouts(&self, track_id: &TrackId, tcp_layout: &str, mcp_layout: &str) -> Result<(), Self::Error>;
    
    // Envelope operations
    
    /// Get envelope states for a track
    fn get_track_envelopes(&self, track_id: &TrackId) -> Result<Vec<EnvelopeState>, Self::Error>;
    
    /// Set envelope states for a track
    fn set_track_envelopes(&self, track_id: &TrackId, envelopes: &[EnvelopeState]) -> Result<(), Self::Error>;
    
    // Zoom operations
    
    /// Get vertical zoom level
    fn get_vertical_zoom(&self) -> Result<i32, Self::Error>;
    
    /// Set vertical zoom level
    fn set_vertical_zoom(&self, zoom: i32) -> Result<(), Self::Error>;
    
    // UI operations
    
    /// Scroll TCP to a track
    fn scroll_tcp_to_track(&self, track_id: &TrackId) -> Result<(), Self::Error>;
    
    /// Scroll MCP to a track
    fn scroll_mcp_to_track(&self, track_id: &TrackId) -> Result<(), Self::Error>;
    
    /// Refresh all layouts/UI
    fn refresh_layouts(&self) -> Result<(), Self::Error>;
}

// Mock implementation for testing and development outside of REAPER

use std::sync::{Arc, Mutex};

/// Error type for mock visibility manager
#[derive(Debug, thiserror::Error)]
pub enum MockVisibilityError {
    #[error("Track not found: {0}")]
    TrackNotFound(String),
    
    #[error("Invalid operation: {0}")]
    InvalidOperation(String),
}

/// Mock implementation of VisibilityManager for testing and development
/// Stores all track data in memory without requiring a real DAW connection
///
/// # Example
/// ```
/// use fts::smart_template::visibility::{MockVisibilityManager, VisibilityManager};
///
/// let manager = MockVisibilityManager::new();
/// 
/// // Add some tracks
/// manager.add_track("track-1", "Kick Drum")?;
/// manager.add_track("track-2", "Snare Drum")?;
///
/// // Show/hide tracks
/// manager.show_track_tcp(&"track-1".to_string())?;
/// manager.hide_track_tcp(&"track-2".to_string())?;
///
/// // Get track parameters
/// let params = manager.get_track_parameters(&"track-1".to_string())?;
/// assert_eq!(params.name, "Kick Drum");
/// assert!(params.tcp_visibility);
/// # Ok::<(), fts::smart_template::visibility::MockVisibilityError>(())
/// ```
#[derive(Debug, Clone)]
pub struct MockVisibilityManager {
    /// Track parameters indexed by track ID
    tracks: Arc<Mutex<HashMap<TrackId, TrackParameters>>>,
    
    /// Track hierarchy: parent -> children
    track_hierarchy: Arc<Mutex<HashMap<TrackId, Vec<TrackId>>>>,
    
    /// Selected track IDs
    selected_tracks: Arc<Mutex<Vec<TrackId>>>,
    
    /// Send configurations: source -> dest -> chunks
    sends: Arc<Mutex<HashMap<TrackId, HashMap<TrackId, Vec<String>>>>>,
    
    /// Receive configurations: dest -> source -> chunks
    receives: Arc<Mutex<HashMap<TrackId, HashMap<TrackId, Vec<String>>>>>,
    
    /// Vertical zoom level
    vertical_zoom: Arc<Mutex<i32>>,
    
    /// Track name to ID mapping
    name_to_id: Arc<Mutex<HashMap<String, TrackId>>>,
}

impl MockVisibilityManager {
    /// Create a new mock visibility manager
    pub fn new() -> Self {
        Self {
            tracks: Arc::new(Mutex::new(HashMap::new())),
            track_hierarchy: Arc::new(Mutex::new(HashMap::new())),
            selected_tracks: Arc::new(Mutex::new(Vec::new())),
            sends: Arc::new(Mutex::new(HashMap::new())),
            receives: Arc::new(Mutex::new(HashMap::new())),
            vertical_zoom: Arc::new(Mutex::new(0)),
            name_to_id: Arc::new(Mutex::new(HashMap::new())),
        }
    }
    
    /// Add a track to the mock manager
    pub fn add_track(&self, track_id: impl Into<TrackId>, name: impl Into<String>) -> Result<(), MockVisibilityError> {
        let track_id = track_id.into();
        let name = name.into();
        
        let mut tracks = self.tracks.lock().unwrap();
        let mut name_to_id = self.name_to_id.lock().unwrap();
        
        let mut params = TrackParameters::default();
        params.track_id = track_id.clone();
        params.name = name.clone();
        
        tracks.insert(track_id.clone(), params);
        name_to_id.insert(name, track_id);
        
        Ok(())
    }
    
    /// Add a child track to a parent
    pub fn add_child_track(&self, parent_id: &TrackId, child_id: TrackId) -> Result<(), MockVisibilityError> {
        let mut hierarchy = self.track_hierarchy.lock().unwrap();
        hierarchy
            .entry(parent_id.clone())
            .or_insert_with(Vec::new)
            .push(child_id);
        Ok(())
    }
    
    /// Get all tracks (for inspection)
    pub fn get_all_tracks(&self) -> HashMap<TrackId, TrackParameters> {
        self.tracks.lock().unwrap().clone()
    }
    
    /// Build default track list from track structures
    /// Creates all tracks with proper hierarchy based on default groups
    pub fn build_default_track_list(&self) -> Result<(), MockVisibilityError> {
        use crate::smart_template::naming_convention::{
            default_groups::bass::create_bass_track_structure,
            track_structure::TrackStructure,
        };
        
        // Build track structures for all groups in the correct order
        // Order: Drums, Bass, GTR Electric, GTR Acoustic, Keys, Synths, Orchestra, Vox, BGVs, Choir
        let mut drums = TrackStructure::new("Drums");
        drums.add_child(Self::create_kick_track_structure_simple());
        drums.add_child(Self::create_snare_track_structure_simple());
        drums.add_child(Self::create_tom_track_structure_simple());
        drums.add_child(Self::create_cymbals_track_structure_simple());
        
        let bass = create_bass_track_structure();
        let gtr_electric = Self::create_gtr_electric_track_structure();
        let gtr_acoustic = Self::create_gtr_acoustic_track_structure();
        let keys = Self::create_keys_track_structure();
        let synths = Self::create_synths_track_structure();
        let orchestra = Self::create_orchestra_track_structure();
        let vox = Self::create_vox_track_structure();
        let bgvs = Self::create_bgvs_track_structure();
        let choir = Self::create_choir_track_structure();
        
        // Add all tracks to the manager in the correct order
        self.add_track_structure(&drums, None)?;
        self.add_track_structure(&bass, None)?;
        self.add_track_structure(&gtr_electric, None)?;
        self.add_track_structure(&gtr_acoustic, None)?;
        self.add_track_structure(&keys, None)?;
        self.add_track_structure(&synths, None)?;
        self.add_track_structure(&orchestra, None)?;
        self.add_track_structure(&vox, None)?;
        self.add_track_structure(&bgvs, None)?;
        self.add_track_structure(&choir, None)?;
        
        Ok(())
    }
    
    /// Recursively add a track structure and its children to the manager
    fn add_track_structure(&self, structure: &crate::smart_template::naming_convention::track_structure::TrackStructure, parent_id: Option<&TrackId>) -> Result<(), MockVisibilityError> {
        
        // Create track ID that includes type to avoid conflicts (e.g., "Kick (BUS)" vs "Kick (SUM)")
        let track_id = if let Some(track_type) = &structure.track_type {
            format!("{} ({})", structure.name, track_type)
        } else {
            structure.name.clone()
        };
        
        // Use the structure name as the track name (without type in the name)
        // The track type will be stored separately in TrackParameters
        let track_name = structure.name.clone();
        
        // Add the track
        self.add_track(&track_id, &track_name)?;
        
        // Set track type if specified
        if let Some(track_type) = &structure.track_type {
            let mut tracks = self.tracks.lock().unwrap();
            if let Some(track) = tracks.get_mut(&track_id) {
                track.track_type = Some(track_type.clone());
            }
        }
        
        // Set folder depth based on whether it has children
        if structure.has_children() {
            let mut tracks = self.tracks.lock().unwrap();
            if let Some(track) = tracks.get_mut(&track_id) {
                track.folder_depth = 1; // Folder track
            }
        }
        
        // Set up parent-child relationship
        if let Some(parent) = parent_id {
            self.add_child_track(parent, track_id.clone())?;
        }
        
        // Recursively add children
        for child in &structure.children {
            self.add_track_structure(child, Some(&track_id))?;
        }
        
        Ok(())
    }
    
    /// Create track structure for GTR Electric
    fn create_gtr_electric_track_structure() -> crate::smart_template::naming_convention::track_structure::TrackStructure {
        crate::smart_template::naming_convention::track_structure::TrackStructure::new("GTR Electric")
    }
    
    /// Create track structure for GTR Acoustic
    fn create_gtr_acoustic_track_structure() -> crate::smart_template::naming_convention::track_structure::TrackStructure {
        crate::smart_template::naming_convention::track_structure::TrackStructure::new("GTR Acoustic")
    }
    
    /// Create track structure for Keys
    fn create_keys_track_structure() -> crate::smart_template::naming_convention::track_structure::TrackStructure {
        crate::smart_template::naming_convention::track_structure::TrackStructure::new("Keys")
    }
    
    /// Create track structure for Synths
    fn create_synths_track_structure() -> crate::smart_template::naming_convention::track_structure::TrackStructure {
        crate::smart_template::naming_convention::track_structure::TrackStructure::new("Synths")
    }
    
    /// Create track structure for Orchestra
    fn create_orchestra_track_structure() -> crate::smart_template::naming_convention::track_structure::TrackStructure {
        crate::smart_template::naming_convention::track_structure::TrackStructure::new("Orchestra")
    }
    
    /// Create track structure for Vox
    fn create_vox_track_structure() -> crate::smart_template::naming_convention::track_structure::TrackStructure {
        crate::smart_template::naming_convention::track_structure::TrackStructure::new("Vox")
    }
    
    /// Create track structure for BGVs
    fn create_bgvs_track_structure() -> crate::smart_template::naming_convention::track_structure::TrackStructure {
        crate::smart_template::naming_convention::track_structure::TrackStructure::new("BGVs")
    }
    
    /// Create track structure for Choir
    fn create_choir_track_structure() -> crate::smart_template::naming_convention::track_structure::TrackStructure {
        crate::smart_template::naming_convention::track_structure::TrackStructure::new("Choir")
    }
    
    // Helper functions for creating drum structures when default-groups feature is not enabled
    fn create_kick_track_structure_simple() -> crate::smart_template::naming_convention::track_structure::TrackStructure {
        use crate::smart_template::naming_convention::track_structure::TrackStructure;
        let mut kick_bus = TrackStructure::with_track_type("Kick", "BUS");
        let mut kick_sum = TrackStructure::with_track_type("Kick", "SUM");
        kick_sum.add_child(TrackStructure::new("Kick In"));
        kick_sum.add_child(TrackStructure::new("Kick Out"));
        kick_sum.add_child(TrackStructure::new("Kick Trig"));
        kick_bus.add_child(kick_sum);
        kick_bus.add_child(TrackStructure::new("Kick Sub"));
        kick_bus.add_child(TrackStructure::new("Kick Ambient"));
        kick_bus
    }
    
    fn create_snare_track_structure_simple() -> crate::smart_template::naming_convention::track_structure::TrackStructure {
        use crate::smart_template::naming_convention::track_structure::TrackStructure;
        let mut snare_bus = TrackStructure::with_track_type("Snare", "BUS");
        let mut snare_sum = TrackStructure::with_track_type("Snare", "SUM");
        snare_sum.add_child(TrackStructure::new("Snare Top"));
        snare_sum.add_child(TrackStructure::new("Snare Bottom"));
        snare_sum.add_child(TrackStructure::new("Snare Trig"));
        snare_bus.add_child(snare_sum);
        snare_bus.add_child(TrackStructure::new("Snare Verb"));
        snare_bus
    }
    
    fn create_tom_track_structure_simple() -> crate::smart_template::naming_convention::track_structure::TrackStructure {
        use crate::smart_template::naming_convention::track_structure::TrackStructure;
        let mut tom_bus = TrackStructure::with_track_type("Tom", "BUS");
        tom_bus.add_child(TrackStructure::new("Tom 1"));
        tom_bus.add_child(TrackStructure::new("Tom 2"));
        tom_bus.add_child(TrackStructure::new("Tom 3"));
        tom_bus
    }
    
    fn create_cymbals_track_structure_simple() -> crate::smart_template::naming_convention::track_structure::TrackStructure {
        use crate::smart_template::naming_convention::track_structure::TrackStructure;
        let mut cymbals_bus = TrackStructure::with_track_type("Cymbals", "BUS");
        cymbals_bus.add_child(TrackStructure::new("Hi-Hat"));
        cymbals_bus.add_child(TrackStructure::new("Ride"));
        cymbals_bus.add_child(TrackStructure::new("OH"));
        cymbals_bus
    }
    
    /// Convert tracks from MockVisibilityManager to a TrackList for display
    /// This allows using the existing TrackList Display implementation
    pub fn to_track_list(&self) -> Result<crate::smart_template::track_template::TrackList, MockVisibilityError> {
        use crate::smart_template::track_template::{TrackList, Track};
        use std::collections::HashSet;
        
        let tracks = self.tracks.lock().unwrap();
        let hierarchy = self.track_hierarchy.lock().unwrap();
        
        let mut track_list = TrackList::new();
        let mut visited = HashSet::new();
        
        // Find root tracks (tracks with no parent) and sort them in the correct order
        let all_track_ids: Vec<TrackId> = tracks.keys().cloned().collect();
        let mut child_tracks: HashSet<TrackId> = hierarchy.values()
            .flat_map(|children| children.iter().cloned())
            .collect();
        
        let mut root_tracks: Vec<TrackId> = all_track_ids.iter()
            .filter(|id| !child_tracks.contains(*id))
            .cloned()
            .collect();
        
        // Sort root tracks in the correct order: Drums, Bass, GTR Electric, GTR Acoustic, Keys, Synths, Orchestra, Vox, BGVs, Choir
        let order = vec!["Drums", "Bass", "GTR Electric", "GTR Acoustic", "Keys", "Synths", "Orchestra", "Vox", "BGVs", "Choir"];
        root_tracks.sort_by(|a, b| {
            let a_name = tracks.get(a).map(|p| &p.name).unwrap_or(a);
            let b_name = tracks.get(b).map(|p| &p.name).unwrap_or(b);
            let a_pos = order.iter().position(|&n| a_name == n).unwrap_or(999);
            let b_pos = order.iter().position(|&n| b_name == n).unwrap_or(999);
            a_pos.cmp(&b_pos)
        });
        
        // Add tracks recursively, starting from roots in the correct order
        for root_id in root_tracks {
            self.add_track_to_list_recursive(&mut track_list, &root_id, &tracks, &hierarchy, &mut visited)?;
        }
        
        Ok(track_list)
    }
    
    fn add_track_to_list_recursive(
        &self,
        track_list: &mut crate::smart_template::track_template::TrackList,
        track_id: &TrackId,
        tracks: &std::sync::MutexGuard<'_, HashMap<TrackId, TrackParameters>>,
        hierarchy: &std::sync::MutexGuard<'_, HashMap<TrackId, Vec<TrackId>>>,
        visited: &mut std::collections::HashSet<TrackId>,
    ) -> Result<(), MockVisibilityError> {
        use crate::smart_template::track_template::Track;
        
        // Prevent infinite recursion
        if visited.contains(track_id) {
            return Ok(());
        }
        visited.insert(track_id.clone());
        
        let params = tracks.get(track_id)
            .ok_or_else(|| MockVisibilityError::TrackNotFound(track_id.clone()))?;
        
        // Check if there's another track with the same name but different type
        // If so, we need to make the name unique by including the type in the name
        let mut track_name = params.name.clone();
        let has_name_conflict = tracks.values()
            .any(|other| other.name == params.name && 
                 other.track_id != *track_id &&
                 other.track_type != params.track_type);
        
        // If there's a conflict, include the type in the name to make it unique
        // But don't pass the type to Track::with_type since it's already in the name
        let mut track = if has_name_conflict {
            if let Some(track_type) = &params.track_type {
                track_name = format!("{} ({})", params.name, track_type);
                // Create track without type since it's already in the name
                Track::new(&track_name)
            } else {
                Track::new(&track_name)
            }
        } else if let Some(track_type) = &params.track_type {
            Track::with_type(&track_name, track_type)
        } else {
            Track::new(&track_name)
        };
        
        // Set parent by checking hierarchy (find which parent has this track as a child)
        for (parent_id, children) in hierarchy.iter() {
            if children.contains(track_id) {
                let parent_params = tracks.get(parent_id)
                    .ok_or_else(|| MockVisibilityError::TrackNotFound(parent_id.clone()))?;
                
                // Check if parent also has a name conflict and needs type in name
                let parent_name = if tracks.values()
                    .any(|other| other.name == parent_params.name && 
                         other.track_id != *parent_id &&
                         other.track_type != parent_params.track_type) {
                    // Parent has conflict, so its name in TrackList includes the type
                    if let Some(parent_type) = &parent_params.track_type {
                        format!("{} ({})", parent_params.name, parent_type)
                    } else {
                        parent_params.name.clone()
                    }
                } else {
                    // No conflict, use just the name (TrackList will format with type if needed)
                    parent_params.name.clone()
                };
                
                track.set_parent(&parent_name);
                break;
            }
        }
        
        track_list.add_track(track);
        
        // Recursively add children
        if let Some(children) = hierarchy.get(track_id) {
            for child_id in children {
                self.add_track_to_list_recursive(track_list, child_id, tracks, hierarchy, visited)?;
            }
        }
        
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_build_default_track_list() {
        let manager = MockVisibilityManager::new();
        
        // Build the default track list
        manager.build_default_track_list().expect("Failed to build default track list");
        
        // Convert to TrackList for display
        let track_list = manager.to_track_list().expect("Failed to convert to TrackList");
        
        // Print the track list
        println!("\n=== Default Track List ===");
        println!("{}", track_list);
        println!("=== End Track List ===\n");
        
        // Verify we have tracks
        let all_tracks = manager.get_all_track_ids().unwrap();
        assert!(!all_tracks.is_empty(), "Should have created tracks");
        
        // Verify we have the expected top-level groups
        let track_names: Vec<String> = all_tracks.iter()
            .map(|id| manager.get_track_name(id).unwrap())
            .collect();
        
        // Check for expected top-level groups
        assert!(track_names.iter().any(|n| n.contains("Drums")), "Should have Drums");
        assert!(track_names.iter().any(|n| n.contains("Bass")), "Should have Bass");
        assert!(track_names.iter().any(|n| n.contains("GTR Electric")), "Should have GTR Electric");
        assert!(track_names.iter().any(|n| n.contains("GTR Acoustic")), "Should have GTR Acoustic");
        assert!(track_names.iter().any(|n| n.contains("Keys")), "Should have Keys");
        assert!(track_names.iter().any(|n| n.contains("Synths")), "Should have Synths");
        assert!(track_names.iter().any(|n| n.contains("Orchestra")), "Should have Orchestra");
        assert!(track_names.iter().any(|n| n.contains("Vox")), "Should have Vox");
        assert!(track_names.iter().any(|n| n.contains("BGVs")), "Should have BGVs");
        assert!(track_names.iter().any(|n| n.contains("Choir")), "Should have Choir");
    }
}

impl Default for MockVisibilityManager {
    fn default() -> Self {
        Self::new()
    }
}

impl VisibilityManager for MockVisibilityManager {
    type Error = MockVisibilityError;
    
    fn get_track_id(&self, track_identifier: &str) -> Result<TrackId, Self::Error> {
        let name_to_id = self.name_to_id.lock().unwrap();
        name_to_id
            .get(track_identifier)
            .cloned()
            .or_else(|| {
                // Try as direct ID
                let tracks = self.tracks.lock().unwrap();
                if tracks.contains_key(track_identifier) {
                    Some(track_identifier.to_string())
                } else {
                    None
                }
            })
            .ok_or_else(|| MockVisibilityError::TrackNotFound(track_identifier.to_string()))
    }
    
    fn get_all_track_ids(&self) -> Result<Vec<TrackId>, Self::Error> {
        let tracks = self.tracks.lock().unwrap();
        Ok(tracks.keys().cloned().collect())
    }
    
    fn get_child_track_ids(&self, parent_track_id: &TrackId) -> Result<Vec<TrackId>, Self::Error> {
        let hierarchy = self.track_hierarchy.lock().unwrap();
        Ok(hierarchy
            .get(parent_track_id)
            .cloned()
            .unwrap_or_default())
    }
    
    fn get_track_parameters(&self, track_id: &TrackId) -> Result<TrackParameters, Self::Error> {
        let tracks = self.tracks.lock().unwrap();
        tracks
            .get(track_id)
            .cloned()
            .ok_or_else(|| MockVisibilityError::TrackNotFound(track_id.clone()))
    }
    
    fn set_track_parameters(&self, track_id: &TrackId, params: &TrackParameters) -> Result<(), Self::Error> {
        let mut tracks = self.tracks.lock().unwrap();
        if let Some(existing) = tracks.get_mut(track_id) {
            *existing = params.clone();
            Ok(())
        } else {
            Err(MockVisibilityError::TrackNotFound(track_id.clone()))
        }
    }
    
    fn get_track_name(&self, track_id: &TrackId) -> Result<String, Self::Error> {
        let tracks = self.tracks.lock().unwrap();
        tracks
            .get(track_id)
            .map(|p| p.name.clone())
            .ok_or_else(|| MockVisibilityError::TrackNotFound(track_id.clone()))
    }
    
    fn show_track_tcp(&self, track_id: &TrackId) -> Result<(), Self::Error> {
        let mut tracks = self.tracks.lock().unwrap();
        if let Some(track) = tracks.get_mut(track_id) {
            track.tcp_visibility = true;
            Ok(())
        } else {
            Err(MockVisibilityError::TrackNotFound(track_id.clone()))
        }
    }
    
    fn hide_track_tcp(&self, track_id: &TrackId) -> Result<(), Self::Error> {
        let mut tracks = self.tracks.lock().unwrap();
        if let Some(track) = tracks.get_mut(track_id) {
            track.tcp_visibility = false;
            Ok(())
        } else {
            Err(MockVisibilityError::TrackNotFound(track_id.clone()))
        }
    }
    
    fn show_track_mcp(&self, track_id: &TrackId) -> Result<(), Self::Error> {
        let mut tracks = self.tracks.lock().unwrap();
        if let Some(track) = tracks.get_mut(track_id) {
            track.mcp_visibility = true;
            Ok(())
        } else {
            Err(MockVisibilityError::TrackNotFound(track_id.clone()))
        }
    }
    
    fn hide_track_mcp(&self, track_id: &TrackId) -> Result<(), Self::Error> {
        let mut tracks = self.tracks.lock().unwrap();
        if let Some(track) = tracks.get_mut(track_id) {
            track.mcp_visibility = false;
            Ok(())
        } else {
            Err(MockVisibilityError::TrackNotFound(track_id.clone()))
        }
    }
    
    fn show_tracks(&self, track_ids: &[TrackId]) -> Result<(), Self::Error> {
        for track_id in track_ids {
            self.show_track_tcp(track_id)?;
            self.show_track_mcp(track_id)?;
        }
        Ok(())
    }
    
    fn hide_tracks(&self, track_ids: &[TrackId]) -> Result<(), Self::Error> {
        for track_id in track_ids {
            self.hide_track_tcp(track_id)?;
            self.hide_track_mcp(track_id)?;
        }
        Ok(())
    }
    
    fn show_all_tracks(&self) -> Result<(), Self::Error> {
        let track_ids = self.get_all_track_ids()?;
        self.show_tracks(&track_ids)
    }
    
    fn hide_all_tracks(&self) -> Result<(), Self::Error> {
        let track_ids = self.get_all_track_ids()?;
        self.hide_tracks(&track_ids)
    }
    
    fn select_track(&self, track_id: &TrackId) -> Result<(), Self::Error> {
        let mut selected = self.selected_tracks.lock().unwrap();
        if !selected.contains(track_id) {
            selected.push(track_id.clone());
        }
        Ok(())
    }
    
    fn deselect_track(&self, track_id: &TrackId) -> Result<(), Self::Error> {
        let mut selected = self.selected_tracks.lock().unwrap();
        selected.retain(|id| id != track_id);
        Ok(())
    }
    
    fn select_tracks(&self, track_ids: &[TrackId]) -> Result<(), Self::Error> {
        let mut selected = self.selected_tracks.lock().unwrap();
        for track_id in track_ids {
            if !selected.contains(track_id) {
                selected.push(track_id.clone());
            }
        }
        Ok(())
    }
    
    fn deselect_all_tracks(&self) -> Result<(), Self::Error> {
        let mut selected = self.selected_tracks.lock().unwrap();
        selected.clear();
        Ok(())
    }
    
    fn get_selected_track_ids(&self) -> Result<Vec<TrackId>, Self::Error> {
        let selected = self.selected_tracks.lock().unwrap();
        Ok(selected.clone())
    }
    
    fn save_snapshot(&self, snapshot: &mut TrackSnapshot) -> Result<(), Self::Error> {
        let tracks = self.tracks.lock().unwrap();
        let sends = self.sends.lock().unwrap();
        let receives = self.receives.lock().unwrap();
        let selected = self.selected_tracks.lock().unwrap();
        let zoom = self.vertical_zoom.lock().unwrap();
        
        snapshot.track_ids.clear();
        snapshot.track_parameters.clear();
        snapshot.sends.clear();
        snapshot.receives.clear();
        
        // Save all tracks or just selected ones based on snapshot configuration
        let tracks_to_save = if snapshot.selected {
            selected.clone()
        } else {
            tracks.keys().cloned().collect()
        };
        
        for track_id in &tracks_to_save {
            if let Some(params) = tracks.get(track_id) {
                snapshot.track_ids.push(track_id.clone());
                snapshot.track_parameters.insert(track_id.clone(), params.clone());
            }
        }
        
        // Save sends
        for (source_id, dests) in sends.iter() {
            if tracks_to_save.contains(source_id) {
                snapshot.sends.insert(source_id.clone(), dests.clone());
            }
        }
        
        // Save receives
        for (dest_id, sources) in receives.iter() {
            if tracks_to_save.contains(dest_id) {
                snapshot.receives.insert(dest_id.clone(), sources.clone());
            }
        }
        
        snapshot.vertical_zoom = Some(*zoom);
        snapshot.selected = !selected.is_empty();
        
        Ok(())
    }
    
    fn load_snapshot(&self, snapshot: &TrackSnapshot) -> Result<(), Self::Error> {
        let mut tracks = self.tracks.lock().unwrap();
        let mut sends = self.sends.lock().unwrap();
        let mut receives = self.receives.lock().unwrap();
        let mut selected = self.selected_tracks.lock().unwrap();
        let mut zoom = self.vertical_zoom.lock().unwrap();
        
        // Restore track parameters
        for (track_id, params) in &snapshot.track_parameters {
            if let Some(track) = tracks.get_mut(track_id) {
                *track = params.clone();
            } else {
                // Track doesn't exist, create it
                tracks.insert(track_id.clone(), params.clone());
            }
        }
        
        // Restore sends
        for (source_id, dests) in &snapshot.sends {
            sends.insert(source_id.clone(), dests.clone());
        }
        
        // Restore receives
        for (dest_id, sources) in &snapshot.receives {
            receives.insert(dest_id.clone(), sources.clone());
        }
        
        // Restore selection
        if snapshot.selected {
            *selected = snapshot.track_ids.clone();
        }
        
        // Restore zoom
        if let Some(zoom_val) = snapshot.vertical_zoom {
            *zoom = zoom_val;
        }
        
        Ok(())
    }
    
    fn get_track_sends(&self, track_id: &TrackId) -> Result<HashMap<TrackId, Vec<String>>, Self::Error> {
        let sends = self.sends.lock().unwrap();
        Ok(sends
            .get(track_id)
            .cloned()
            .unwrap_or_default())
    }
    
    fn get_track_receives(&self, track_id: &TrackId) -> Result<HashMap<TrackId, Vec<String>>, Self::Error> {
        let receives = self.receives.lock().unwrap();
        Ok(receives
            .get(track_id)
            .cloned()
            .unwrap_or_default())
    }
    
    fn set_track_sends(&self, track_id: &TrackId, sends: &HashMap<TrackId, Vec<String>>) -> Result<(), Self::Error> {
        let mut track_sends = self.sends.lock().unwrap();
        track_sends.insert(track_id.clone(), sends.clone());
        Ok(())
    }
    
    fn set_track_receives(&self, track_id: &TrackId, receives: &HashMap<TrackId, Vec<String>>) -> Result<(), Self::Error> {
        let mut track_receives = self.receives.lock().unwrap();
        track_receives.insert(track_id.clone(), receives.clone());
        Ok(())
    }
    
    fn set_tcp_height(&self, track_id: &TrackId, height: f64) -> Result<(), Self::Error> {
        let mut tracks = self.tracks.lock().unwrap();
        if let Some(track) = tracks.get_mut(track_id) {
            track.tcp_height = height;
            Ok(())
        } else {
            Err(MockVisibilityError::TrackNotFound(track_id.clone()))
        }
    }
    
    fn set_mcp_height(&self, track_id: &TrackId, height: f64) -> Result<(), Self::Error> {
        let mut tracks = self.tracks.lock().unwrap();
        if let Some(track) = tracks.get_mut(track_id) {
            track.mcp_height = Some(height);
            Ok(())
        } else {
            Err(MockVisibilityError::TrackNotFound(track_id.clone()))
        }
    }
    
    fn set_tracks_minimum_height(&self, track_ids: &[TrackId]) -> Result<(), Self::Error> {
        for track_id in track_ids {
            self.set_tcp_height(track_id, 0.0)?;
        }
        Ok(())
    }
    
    fn set_folder_state(&self, track_id: &TrackId, depth: i32, tcp_compact: i32, mcp_compact: Option<i32>) -> Result<(), Self::Error> {
        let mut tracks = self.tracks.lock().unwrap();
        if let Some(track) = tracks.get_mut(track_id) {
            track.folder_depth = depth;
            track.tcp_folder_state = tcp_compact;
            track.mcp_folder_state = mcp_compact;
            Ok(())
        } else {
            Err(MockVisibilityError::TrackNotFound(track_id.clone()))
        }
    }
    
    fn set_folder_compact(&self, track_id: &TrackId, compact: bool) -> Result<(), Self::Error> {
        let mut tracks = self.tracks.lock().unwrap();
        if let Some(track) = tracks.get_mut(track_id) {
            track.tcp_folder_state = if compact { 1 } else { 0 };
            Ok(())
        } else {
            Err(MockVisibilityError::TrackNotFound(track_id.clone()))
        }
    }
    
    fn collapse_top_level_tracks(&self) -> Result<(), Self::Error> {
        let tracks = self.tracks.lock().unwrap();
        let track_ids: Vec<TrackId> = tracks
            .iter()
            .filter(|(_, params)| params.folder_depth > 0)
            .map(|(id, _)| id.clone())
            .collect();
        drop(tracks);
        
        for track_id in track_ids {
            self.set_folder_compact(&track_id, true)?;
        }
        Ok(())
    }
    
    fn get_tcp_layout(&self, track_id: &TrackId) -> Result<String, Self::Error> {
        let tracks = self.tracks.lock().unwrap();
        tracks
            .get(track_id)
            .map(|p| p.tcp_layout.clone())
            .ok_or_else(|| MockVisibilityError::TrackNotFound(track_id.clone()))
    }
    
    fn get_mcp_layout(&self, track_id: &TrackId) -> Result<String, Self::Error> {
        let tracks = self.tracks.lock().unwrap();
        tracks
            .get(track_id)
            .map(|p| p.mcp_layout.clone())
            .ok_or_else(|| MockVisibilityError::TrackNotFound(track_id.clone()))
    }
    
    fn set_track_layouts(&self, track_id: &TrackId, tcp_layout: &str, mcp_layout: &str) -> Result<(), Self::Error> {
        let mut tracks = self.tracks.lock().unwrap();
        if let Some(track) = tracks.get_mut(track_id) {
            track.tcp_layout = tcp_layout.to_string();
            track.mcp_layout = mcp_layout.to_string();
            Ok(())
        } else {
            Err(MockVisibilityError::TrackNotFound(track_id.clone()))
        }
    }
    
    fn get_track_envelopes(&self, track_id: &TrackId) -> Result<Vec<EnvelopeState>, Self::Error> {
        let tracks = self.tracks.lock().unwrap();
        tracks
            .get(track_id)
            .map(|p| p.envelopes.clone())
            .ok_or_else(|| MockVisibilityError::TrackNotFound(track_id.clone()))
    }
    
    fn set_track_envelopes(&self, track_id: &TrackId, envelopes: &[EnvelopeState]) -> Result<(), Self::Error> {
        let mut tracks = self.tracks.lock().unwrap();
        if let Some(track) = tracks.get_mut(track_id) {
            track.envelopes = envelopes.to_vec();
            Ok(())
        } else {
            Err(MockVisibilityError::TrackNotFound(track_id.clone()))
        }
    }
    
    fn get_vertical_zoom(&self) -> Result<i32, Self::Error> {
        let zoom = self.vertical_zoom.lock().unwrap();
        Ok(*zoom)
    }
    
    fn set_vertical_zoom(&self, zoom: i32) -> Result<(), Self::Error> {
        let mut zoom_val = self.vertical_zoom.lock().unwrap();
        *zoom_val = zoom;
        Ok(())
    }
    
    fn scroll_tcp_to_track(&self, _track_id: &TrackId) -> Result<(), Self::Error> {
        // Mock: no-op
        Ok(())
    }
    
    fn scroll_mcp_to_track(&self, _track_id: &TrackId) -> Result<(), Self::Error> {
        // Mock: no-op
        Ok(())
    }
    
    fn refresh_layouts(&self) -> Result<(), Self::Error> {
        // Mock: no-op
        Ok(())
    }
}
