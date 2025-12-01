use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use crate::tracks::api::folder::{TcpFolderState, McpFolderState, TrackDepth, FolderDepthChange};
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

/// Represents a track in a project
/// 
/// This struct contains all fields from the REAPER track state chunk,
/// including basic properties, volume/pan, mute/solo, folder settings,
/// fixed lanes, record settings, receives, hardware outputs, and more.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Track {
    // === Basic Track Properties ===
    /// Unique identifier for this track
    pub id: Option<uuid::Uuid>,
    /// Track name (NAME)
    pub name: String,
    /// Track index in the project
    pub index: Option<usize>,
    /// Track GUID (TRACKID) - REAPER track id
    pub guid: Option<String>,
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
    /// Track depth (for folder hierarchy) - absolute cumulative nesting level
    /// This is calculated by summing all folder_depth_change values from track 0
    pub track_depth: TrackDepth,
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
    pub metadata: HashMap<String, String>,
}

impl Track {
    /// Create a new track
    pub fn new(name: String) -> Self {
        Self {
            // Basic properties
            id: None,
            name,
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
            track_depth: TrackDepth::default(),
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

