use serde::{Deserialize, Serialize};
use derive_builder::Builder;
use std::collections::HashMap;
use crate::tracks::api::folder::{TcpFolderState, McpFolderState, FolderDepthChange};
use crate::tracks::api::automation::AutomationMode;
use crate::tracks::api::free_mode::FreeMode;
use crate::tracks::api::fixed_lanes::{FixedLanesSettings, LaneSoloSettings, LaneRecordSettings, LaneNameSettings};
use crate::tracks::api::record::{RecordSettings, RecordMode, MonitorMode};
use crate::tracks::api::receive::{TrackReceive, ReceiveMode};
use crate::tracks::api::hardware::{HardwareOutputSettings, MidiOutputSettings, MasterSendSettings};
use crate::tracks::api::solo::SoloMode;
use crate::tracks::api::collapse::{ArrangeCollapseState, MixerCollapseState, WiringCollapseState, BusCompactSettings};
use crate::tracks::api::quantize::InputQuantize;
use crate::tracks::api::timebase::TrackTimebase;
use crate::tracks::api::midi_note_name::MidiNoteName;
use crate::tracks::envelope::ExtensionData;
use crate::tracks::item::Item;
use crate::tracks::envelope::Envelope;
use crate::tracks::fx_chain::FxChain;
use crate::tracks::types::{TrackName, TrackGuid, MetadataKey};

/// Represents a track in a project
/// 
/// This struct contains all fields from the REAPER track state chunk,
/// including basic properties, volume/pan, mute/solo, folder settings,
/// fixed lanes, record settings, receives, hardware outputs, and more.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Builder)]
#[builder(setter(into, strip_option), build_fn(skip))]
pub struct Track {
    // === Basic Track Properties ===
    /// Unique identifier for this track
    pub id: Option<uuid::Uuid>,
    /// Track name (NAME)
    pub name: TrackName,
    /// Track index in the project
    pub index: Option<usize>,
    /// Track GUID (TRACKID) - REAPER track id
    pub guid: Option<TrackGuid>,
    /// Whether track controls are locked (LOCK)
    pub locked: bool,
    /// Peak color (PEAKCOL) - default is 16576
    pub peak_color: Option<i32>,
    /// Track timebase (BEAT) - -1 = project default
    pub beat: Option<TrackTimebase>,
    
    // === Volume and Pan ===
    /// Volume (0.0 to 1.0, or in REAPER units) - from VOLPAN field 1
    pub volume: f64,
    /// Pan (-1.0 to 1.0, or in REAPER units) - from VOLPAN field 2
    pub pan: f64,
    /// Pan law - from VOLPAN field 3
    pub pan_law: Option<f64>,
    /// Width (stereo width, typically 0.0 to 1.0)
    pub width: f64,
    
    // === Mute and Solo ===
    /// Whether the track is muted - from MUTESOLO field 1
    pub muted: bool,
    /// Solo state - from MUTESOLO field 2
    pub solo_state: SoloMode,
    /// Solo defeat - from MUTESOLO field 3
    pub solo_defeat: bool,
    
    // === Automation ===
    /// Automation mode (AUTOMODE)
    pub automation_mode: AutomationMode,
    
    // === Phase and Folder ===
    /// Invert phase (IPHASE)
    pub invert_phase: bool,
    /// Folder state for TCP (Track Control Panel)
    pub folder_state_tcp: Option<TcpFolderState>,
    /// Folder state for MCP (Mixer Control Panel)
    pub folder_state_mcp: Option<McpFolderState>,
    /// Folder depth change - relative change from previous track (from ISBUS field 2)
    /// - 0 = normal track
    /// - 1 = folder parent (starts new folder)
    /// - -1 = closes one folder level
    /// - -2 = closes two folder levels, etc.
    pub folder_depth_change: FolderDepthChange,
    /// Whether this track is a folder
    pub is_folder: bool,
    /// Bus compact settings (BUSCOMP) - collapse folder settings
    pub bus_compact: Option<BusCompactSettings>,
    
    // === Show in Mixer ===
    /// Show in mixer (SHOWINMIX field 1)
    pub show_in_mixer: bool,
    /// Show in track list (SHOWINMIX field 4)
    pub show_in_track_list: bool,
    
    // === Free Item Positioning / Fixed Item Lanes ===
    /// Free item positioning mode (FREEMODE) - 0=disabled, 1=FIP, 2=FIL
    pub free_mode: Option<FreeMode>,
    /// Fixed lanes settings (FIXEDLANES) - REAPER 7+
    pub fixed_lanes: Option<FixedLanesSettings>,
    /// Lane solo settings (LANESOLO) - REAPER 7+
    pub lane_solo: Option<LaneSoloSettings>,
    /// Lane record settings (LANEREC) - REAPER 7+
    pub lane_record: Option<LaneRecordSettings>,
    /// Lane name settings (LANENAME) - REAPER 7+
    pub lane_names: Option<LaneNameSettings>,
    
    // === Record Settings ===
    /// Record settings (REC)
    pub record_settings: Option<RecordSettings>,
    /// Whether the track is record armed (legacy field, also in record_settings)
    pub record_armed: bool,
    /// Input monitoring mode (legacy field, also in record_settings)
    pub input_monitoring_mode: Option<String>,
    /// Recording mode (legacy field, also in record_settings)
    pub recording_mode: Option<String>,
    
    // === Track Height ===
    /// Track height in TCP (TRACKHEIGHT field 1) - height in pixels
    pub track_height: Option<i32>,
    /// Folder override (TRACKHEIGHT field 2) - collapsed
    pub track_height_folder_override: Option<bool>,
    
    // === Input Quantize ===
    /// Input quantize settings (INQ)
    pub input_quantize: Option<InputQuantize>,
    
    // === Channel Count ===
    /// Number of track channels (NCHAN)
    pub channel_count: u32,
    
    // === Recording Format ===
    /// Recording format data (RECCFG) - binary data
    pub rec_cfg: Option<String>,
    
    // === MIDI ===
    /// MIDI color map file path (MIDICOLORMAPFN)
    pub midi_color_map_fn: Option<String>,
    /// MIDI hardware output settings (MIDIOUT)
    pub midi_output: Option<MidiOutputSettings>,
    /// Custom note order (CUSTOM_NOTE_ORDER)
    pub custom_note_order: Option<Vec<i32>>,
    /// MIDI note names (MIDINOTENAMES)
    pub midi_note_names: Vec<MidiNoteName>,
    
    // === FX ===
    /// Whether the track has FX enabled (FX) - 0=bypassed, 1=active
    pub has_fx: bool,
    /// FX chain for this track
    pub fx_chain: Option<FxChain>,
    /// Input FX chain (FXCHAIN_REC)
    pub input_fx_chain: Option<FxChain>,
    
    // === Performance ===
    /// Performance options (PERF) - bitwise flags
    pub perf: Option<i32>,
    
    // === Layouts ===
    /// Active TCP and MCP layouts (LAYOUTS)
    pub layouts_tcp: Option<String>,
    pub layouts_mcp: Option<String>,
    
    // === Extension Data ===
    /// Extension-specific persistent data (EXT)
    pub extension_data: Vec<ExtensionData>,
    
    // === Receives ===
    /// Track receives (AUXRECV)
    pub receives: Vec<TrackReceive>,
    
    // === Master Send ===
    /// Master/parent send (MAINSEND)
    pub master_send: Option<MasterSendSettings>,
    
    // === Hardware Outputs ===
    /// Hardware output sends (HWOUT)
    pub hardware_outputs: Vec<HardwareOutputSettings>,
    
    // === Track State ===
    /// Whether the track is selected
    pub selected: bool,
    /// Track color (RGB value)
    pub color: Option<u32>,
    
    // === Nested Content ===
    /// Items on this track (audio and MIDI items)
    pub items: Vec<Item>,
    /// Automation envelopes on this track (volume, pan, etc.)
    pub envelopes: Vec<Envelope>,
    
    // === Metadata ===
    /// Optional metadata
    pub metadata: HashMap<MetadataKey, String>,
    
    // === Extension State ===
    /// Extension-specific state data (ExtState)
    /// This is a flexible metadata field that can store arbitrary data.
    /// Typically used for storing structured data like "FTS-Item-Properties" as JSON or other formats.
    pub ext_state: Option<String>,
}

impl Default for Track {
    fn default() -> Self {
        Self::new(TrackName::default())
    }
}

impl Track {
    /// Calculate the track depth by summing folder_depth_change values from previous tracks
    /// 
    /// This calculates the absolute cumulative nesting level by iterating through
    /// all tracks from the beginning up to (but not including) the current track,
    /// and summing their folder_depth_change values.
    /// 
    /// The depth represents how many folder levels deep this track is:
    /// - 0 = top-level track (not in any folder)
    /// - 1 = inside one folder
    /// - 2 = inside two nested folders
    /// - etc.
    /// 
    /// # Arguments
    /// * `tracks` - The full list of tracks (must include this track)
    /// * `index` - The index of this track in the list
    /// 
    /// # Returns
    /// The calculated depth (0 = top-level, 1 = inside one folder, etc.)
    pub fn calculate_depth(tracks: &[Track], index: usize) -> u32 {
        let mut depth = 0i32;
        // Sum folder_depth_change from all previous tracks (not including current)
        for i in 0..index.min(tracks.len()) {
            depth += tracks[i].folder_depth_change.to_reaper_value();
        }
        depth.max(0) as u32
    }
    
    /// Calculate the track depth for this track within a track list
    /// 
    /// This is a convenience method that finds this track in the list and calculates its depth.
    /// 
    /// # Arguments
    /// * `tracks` - The full list of tracks (must include this track)
    /// 
    /// # Returns
    /// The calculated depth, or 0 if the track is not found
    pub fn depth_in_list(&self, tracks: &[Track]) -> u32 {
        tracks.iter()
            .position(|t| std::ptr::eq(t, self))
            .map(|idx| Self::calculate_depth(tracks, idx))
            .unwrap_or(0)
    }
    
    /// Create a new track builder
    /// 
    /// This provides a fluent interface for building tracks:
    /// ```rust
    /// let track = Track::builder()
    ///     .name("My Track")
    ///     .id(some_uuid)
    ///     .metadata(some_metadata)
    ///     .build()
    ///     .unwrap();
    /// ```
    pub fn builder() -> TrackBuilder {
        TrackBuilder::default()
    }
    
    /// Create a new track
    pub fn new(name: impl Into<TrackName>) -> Self {
        Self {
            // Basic properties
            id: None,
            name: name.into(),
            index: None,
            guid: None,
            locked: false,
            peak_color: None,
            beat: None,
            
            // Volume and pan
            volume: 1.0,
            pan: 0.0,
            pan_law: None,
            width: 1.0,
            
            // Mute and solo
            muted: false,
            solo_state: SoloMode::Off,
            solo_defeat: false,
            
            // Automation
            automation_mode: AutomationMode::TrimRead,
            
            // Phase and folder
            invert_phase: false,
            folder_state_tcp: None,
            folder_state_mcp: None,
            folder_depth_change: FolderDepthChange::Normal,
            is_folder: false,
            bus_compact: None,
            
            // Show in mixer
            show_in_mixer: true,
            show_in_track_list: true,
            
            // Free item positioning / Fixed lanes
            free_mode: None,
            fixed_lanes: None,
            lane_solo: None,
            lane_record: None,
            lane_names: None,
            
            // Record settings
            record_settings: None,
            record_armed: false,
            input_monitoring_mode: None,
            recording_mode: None,
            
            // Track height
            track_height: None,
            track_height_folder_override: None,
            
            // Input quantize
            input_quantize: None,
            
            // Channel count
            channel_count: 2,
            
            // Recording format
            rec_cfg: None,
            
            // MIDI
            midi_color_map_fn: None,
            midi_output: None,
            custom_note_order: None,
            midi_note_names: Vec::new(),
            
            // FX
            has_fx: false,
            fx_chain: None,
            input_fx_chain: None,
            
            // Performance
            perf: None,
            
            // Layouts
            layouts_tcp: None,
            layouts_mcp: None,
            
            // Extension data
            extension_data: Vec::new(),
            
            // Receives
            receives: Vec::new(),
            
            // Master send
            master_send: None,
            
            // Hardware outputs
            hardware_outputs: Vec::new(),
            
            // Track state
            selected: false,
            color: None,
            
            // Nested content
            items: Vec::new(),
            envelopes: Vec::new(),
            
            // Metadata
            metadata: HashMap::new(),
            
            // Extension state
            ext_state: None,
        }
    }

    /// Create a new track with ID
    pub fn with_id(id: uuid::Uuid, name: impl Into<TrackName>) -> Self {
        let mut track = Self::new(name);
        track.id = Some(id);
        track
    }

    /// Get metadata value
    pub fn get_metadata(&self, key: impl Into<MetadataKey>) -> Option<&String> {
        self.metadata.get(&key.into())
    }

    /// Set metadata value
    pub fn set_metadata<K: Into<MetadataKey>, V: Into<String>>(&mut self, key: K, value: V) {
        self.metadata.insert(key.into(), value.into());
    }

    /// Remove metadata value
    pub fn remove_metadata(&mut self, key: impl Into<MetadataKey>) -> Option<String> {
        self.metadata.remove(&key.into())
    }

    /// Get FTS-Item-Properties from ext_state
    /// 
    /// This retrieves the FTS-Item-Properties data stored in the ext_state field.
    /// The data is stored as a string (typically JSON) and can be parsed as needed.
    pub fn get_fts_item_properties(&self) -> Option<&str> {
        self.ext_state.as_deref()
    }

    /// Set FTS-Item-Properties in ext_state
    /// 
    /// This stores FTS-Item-Properties data in the ext_state field.
    /// The data should be serialized (e.g., as JSON) before being stored.
    pub fn set_fts_item_properties<S: Into<String>>(&mut self, properties: S) {
        self.ext_state = Some(properties.into());
    }

    /// Remove FTS-Item-Properties from ext_state
    pub fn remove_fts_item_properties(&mut self) -> Option<String> {
        self.ext_state.take()
    }
}

impl TrackBuilder {
    /// Build a Track with sensible defaults
    /// 
    /// This custom build method uses `Track::default()` to set all defaults,
    /// then applies any values provided via the builder.
    /// 
    /// Panics if required fields (like `name`) are not set. This makes the API
    /// infallible when all required fields are provided.
    pub fn build(self) -> Track {
        let name = self.name.expect("Track name is required - use .name() to set it");
        
        // Start with defaults
        let mut track = Track::default();
        track.name = name;
        
        // Override with any builder-provided values
        // Builder fields for Option<T> are Option<Option<T>> (outer Option = was it set, inner = the value)
        // We need to flatten them
        if let Some(id) = self.id {
            track.id = id;
        }
        
        if let Some(guid) = self.guid {
            track.guid = guid;
        }

        // Merge metadata if provided
        if let Some(metadata) = self.metadata {
            for (k, v) in metadata {
                track.metadata.insert(k, v);
            }
        }
        
        track
    }
    
    /// Add child tracks using folder properties
    ///
    /// This method automatically builds the parent track, then adds children with proper
    /// folder depth properties. Accepts either a single Track or Vec<Track>.
    /// Returns a Vec<Track> in the correct order.
    ///
    /// # Example
    /// ```rust
    /// use daw::tracks::AddChild;
    /// 
    /// let kick_in = Track::builder().name("Kick In").add_child(vec![]); // Empty vec for leaf
    /// let kick_bus = Track::builder().name("Kick").add_child(kick_in);
    /// let drum_bus = Track::builder().name("Drums").add_child(kick_bus);
    /// ```
    pub fn add_child<T>(self, children: T) -> Vec<Track>
    where
        T: Into<Vec<Track>>,
    {
        use crate::tracks::AddChild;
        let parent = self.build();
        parent.add_child(children)
    }
}

