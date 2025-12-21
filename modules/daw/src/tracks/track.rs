use serde::{Deserialize, Serialize};
use derive_builder::Builder;
use std::collections::HashMap;
use colored::Colorize;
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

/// Display a track list in hierarchical format
/// 
/// Prints and returns a String representation of tracks with their items in a tree structure:
/// ```
/// Trackname: [Item1, Item2]
/// -Trackname: [Item3]
/// --Trackname: [Item4, Item5]
/// ```
/// 
/// This function both prints the output and returns the string, so you can use it directly
/// in tests or capture the output for further processing.
/// 
/// This function is designed to be extended with colors and other formatting features.
/// 
/// # Example
/// ```rust
/// let tracks = vec![
///     Track::builder().name("Drums").add_child(vec![
///         Track::builder().name("Kick").add_child(vec![
///             Track::new("Kick In")
///         ])
///     ])
/// ];
/// display_tracklist(&tracks); // Prints and returns the string
/// ```
pub fn display_tracklist(tracks: &[Track]) -> String {
    let mut output = String::new();
    let mut depth = 0i32;
    
    for track in tracks {
        // Calculate indentation prefix (depth represents how many levels deep we are)
        // Depth is calculated BEFORE applying this track's folder_depth_change
        let prefix = if depth > 0 {
            "-".repeat(depth as usize)
        } else {
            String::new()
        };
        
        // Color track name (cyan for tracks, can be customized later based on track type)
        let track_name = track.name.0.color("cyan").bold();
        output.push_str(&format!("{}{}", prefix, track_name));
        
        // Add items if any
        if !track.items.is_empty() {
            output.push_str(&": [".to_string());
            for (j, item) in track.items.iter().enumerate() {
                if j > 0 {
                    output.push_str(", ");
                }
                // Color item names (yellow for items, can be customized later based on item type)
                let item_name = if !item.name.is_empty() {
                    item.name.color("yellow")
                } else {
                    format!("Item {}", j + 1).color("yellow")
                };
                output.push_str(&item_name.to_string());
            }
            output.push_str(&"]".to_string());
        }
        
        output.push('\n');
        
        // Update depth AFTER printing (for next track)
        depth += track.folder_depth_change.to_reaper_value();
    }
    
    // Print the output
    print!("{}", output);
    
    output
}

/// Helper trait to convert single items or collections into an iterator of Items
/// 
/// This allows the builder to accept both single items and collections:
/// - Single item: `"Kick In"` 
/// - Collection: `vec!["Kick In", "Kick Out"]`
pub trait IntoItems {
    type Iter: Iterator<Item = Item>;
    fn into_items(self) -> Self::Iter;
}

// Implementation for single &str
impl IntoItems for &str {
    type Iter = std::iter::Once<Item>;
    fn into_items(self) -> Self::Iter {
        std::iter::once(Item::from(self))
    }
}

// Implementation for single String
impl IntoItems for String {
    type Iter = std::iter::Once<Item>;
    fn into_items(self) -> Self::Iter {
        std::iter::once(Item::from(self))
    }
}

// Implementation for single Item
impl IntoItems for Item {
    type Iter = std::iter::Once<Item>;
    fn into_items(self) -> Self::Iter {
        std::iter::once(self)
    }
}

// Implementation for Vec<T> where T: Into<Item>
impl<T> IntoItems for Vec<T>
where
    T: Into<Item>,
{
    type Iter = std::iter::Map<std::vec::IntoIter<T>, fn(T) -> Item>;
    fn into_items(self) -> Self::Iter {
        self.into_iter().map(|item| item.into())
    }
}

// Implementation for arrays [T; N] where T: Into<Item>
impl<T, const N: usize> IntoItems for [T; N]
where
    T: Into<Item>,
{
    type Iter = std::iter::Map<std::array::IntoIter<T, N>, fn(T) -> Item>;
    fn into_items(self) -> Self::Iter {
        self.into_iter().map(|item| item.into())
    }
}

/// Builder for constructing expected track structures in tests
/// 
/// # Example
/// ```rust
/// let expected = TrackStructureBuilder::new()
///     .track("Kick", "Kick In")  // Single item
///     .build();
/// 
/// // Or with multiple items:
/// let expected = TrackStructureBuilder::new()
///     .track("Kick", vec!["Kick In", "Kick Out"])
///     .build();
/// 
/// // Or with folders:
/// let expected = TrackStructureBuilder::new()
///     .folder("Kick")
///         .track("In", "Kick In")
///         .track("Out", "Kick Out")
///     .end()
///     .build();
/// ```
pub struct TrackStructureBuilder {
    tracks: Vec<Track>,
    folder_stack: Vec<usize>, // Indices of folder tracks that are still open
}

impl TrackStructureBuilder {
    pub fn new() -> Self {
        Self {
            tracks: Vec::new(),
            folder_stack: Vec::new(),
        }
    }
    
    /// Add a track with items
    /// 
    /// Items can be a single item or a collection of items, both implementing Into<Item>
    /// 
    /// # Example
    /// ```rust
    /// .track("Kick", "Kick In")  // Single item (string)
    /// .track("Snare", vec!["Snare Top", "Snare Bottom"])  // Multiple items
    /// ```
    pub fn track<I>(mut self, name: impl Into<TrackName>, items: I) -> Self
    where
        I: IntoItems,
    {
        let mut track = Track::new(name);
        track.items = items.into_items().collect();
        self.tracks.push(track);
        self
    }
    
    /// Start a folder (creates a folder track)
    pub fn folder(mut self, name: impl Into<TrackName>) -> Self {
        let mut track = Track::new(name);
        track.is_folder = true;
        track.folder_depth_change = FolderDepthChange::FolderStart;
        let index = self.tracks.len();
        self.tracks.push(track);
        self.folder_stack.push(index);
        self
    }
    
    /// End the current folder (closes the most recent folder)
    pub fn end(mut self) -> Self {
        if let Some(_folder_index) = self.folder_stack.pop() {
            // The folder is already marked as a folder start
            // Set the last track to close the folder
            if let Some(last_track) = self.tracks.last_mut() {
                match last_track.folder_depth_change {
                    FolderDepthChange::Normal => {
                        last_track.folder_depth_change = FolderDepthChange::ClosesLevels(-1);
                    }
                    FolderDepthChange::ClosesLevels(n) => {
                        // Already closing levels - increment to also close this folder
                        last_track.folder_depth_change = FolderDepthChange::ClosesLevels(n - 1);
                    }
                    _ => {
                        // If it's FolderStart or something else, set to close 1 level
                        last_track.folder_depth_change = FolderDepthChange::ClosesLevels(-1);
                    }
                }
            }
        }
        self
    }
    
    /// Build the final Vec<Track>
    pub fn build(self) -> Vec<Track> {
        self.tracks
    }
}

impl Default for TrackStructureBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Compare two track lists, focusing only on structural aspects that matter for organization
/// 
/// This function compares:
/// - Track names
/// - Items (by name only, ignoring IDs and other metadata)
/// - Folder structure (is_folder, folder_depth_change)
/// - Overall hierarchy structure
/// 
/// It ignores:
/// - IDs, GUIDs, and other metadata
/// - Volume, pan, mute, solo, and other audio settings
/// - Item positions, lengths, and other item metadata
/// 
/// Returns `Ok(())` if tracks match, or `Err(String)` with a detailed description of differences.
/// 
/// # Example
/// ```rust
/// use daw::tracks::assert_tracks_equal;
/// 
/// let actual = vec![Track::new("Kick")];
/// let expected = vec![Track::new("Kick")];
/// assert_tracks_equal(&actual, &expected).unwrap();
/// ```
pub fn assert_tracks_equal(actual: &[Track], expected: &[Track]) -> Result<(), String> {
    if actual.len() != expected.len() {
        return Err(format!(
            "Track count mismatch: expected {} tracks, got {}",
            expected.len(),
            actual.len()
        ));
    }

    let mut all_differences = Vec::new();

    for (i, (actual_track, expected_track)) in actual.iter().zip(expected.iter()).enumerate() {
        let path = format!("track[{}]", i);
        if let Err(diff) = compare_track(actual_track, expected_track, &path) {
            all_differences.push(diff);
        }
    }

    if !all_differences.is_empty() {
        return Err(format!(
            "Track structure differences:\n{}\n\nActual structure:\n{}\nExpected structure:\n{}",
            all_differences.join("\n"),
            format_track_list(actual),
            format_track_list(expected)
        ));
    }

    Ok(())
}

/// Compare a single track's relevant properties
fn compare_track(actual: &Track, expected: &Track, path: &str) -> Result<(), String> {
    let mut differences = Vec::new();

    // Compare name
    if actual.name != expected.name {
        differences.push(format!(
            "  {}: name - expected '{}', got '{}'",
            path, expected.name.0, actual.name.0
        ));
    }

    // Compare folder properties
    if actual.is_folder != expected.is_folder {
        differences.push(format!(
            "  {}: is_folder - expected {}, got {}",
            path, expected.is_folder, actual.is_folder
        ));
    }

    if actual.folder_depth_change != expected.folder_depth_change {
        differences.push(format!(
            "  {}: folder_depth_change - expected {:?}, got {:?}",
            path, expected.folder_depth_change, actual.folder_depth_change
        ));
    }

    // Compare items (by name only, ignoring IDs and other metadata)
    let actual_item_names: Vec<&str> = actual.items.iter().map(|i| i.name.as_str()).collect();
    let expected_item_names: Vec<&str> = expected.items.iter().map(|i| i.name.as_str()).collect();

    if actual_item_names != expected_item_names {
        differences.push(format!(
            "  {}: items - expected {:?}, got {:?}",
            path, expected_item_names, actual_item_names
        ));
    }

    if !differences.is_empty() {
        return Err(differences.join("\n"));
    }

    Ok(())
}

/// Format a track list for display in error messages
fn format_track_list(tracks: &[Track]) -> String {
    let mut output = String::new();
    let mut depth = 0i32;
    
    for track in tracks {
        let prefix = if depth > 0 {
            "-".repeat(depth as usize)
        } else {
            String::new()
        };
        
        output.push_str(&format!("{}{}", prefix, track.name.0));
        
        if !track.items.is_empty() {
            let item_names: Vec<&str> = track.items.iter().map(|i| i.name.as_str()).collect();
            output.push_str(&format!(": [{:?}]", item_names));
        }
        
        if track.is_folder {
            output.push_str(" (folder)");
        }
        
        output.push('\n');
        
        depth += track.folder_depth_change.to_reaper_value();
    }
    
    output
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tracks::item::Item;
    use crate::tracks::AddChild;
    
    /// Strip ANSI color codes from a string for testing
    fn strip_ansi_codes(s: &str) -> String {
        // Remove ANSI escape sequences manually (simpler than regex dependency)
        let mut result = String::new();
        let mut chars = s.chars().peekable();
        
        while let Some(ch) = chars.next() {
            if ch == '\x1b' || ch == '\u{1b}' {
                // Skip until we find 'm' (end of ANSI sequence)
                while let Some(&next) = chars.peek() {
                    if next == 'm' {
                        chars.next(); // consume 'm'
                        break;
                    }
                    chars.next();
                }
            } else {
                result.push(ch);
            }
        }
        result
    }

    #[test]
    fn test_display_tracklist_simple() {
        let tracks = TrackStructureBuilder::new()
            .track("Kick In", vec!["Kick In.wav", "Kick In Alt.wav"])
            .build();
        
        let output = display_tracklist(&tracks);
        
        // Strip ANSI codes for comparison
        let stripped = strip_ansi_codes(&output);
        assert_eq!(stripped, "Kick In: [Kick In.wav, Kick In Alt.wav]\n");
    }

    #[test]
    fn test_display_tracklist_hierarchy() {
        // Create a hierarchy: Drums -> Kick -> Kick In
        let tracks = TrackStructureBuilder::new()
            .folder("Drums")
                .folder("Kick")
                    .track("Kick In", "Kick In.wav")
                .end()
            .end()
            .build();
        
        let output = display_tracklist(&tracks);
        
        // Expected output:
        // Drums
        // -Kick
        // --Kick In: [Kick In.wav]
        let expected = "Drums\n-Kick\n--Kick In: [Kick In.wav]\n";
        // Strip ANSI codes for comparison
        let stripped = strip_ansi_codes(&output);
        assert_eq!(stripped, expected);
    }

    #[test]
    fn test_display_tracklist_multiple_items() {
        // Create tracks with multiple items
        let tracks = TrackStructureBuilder::new()
            .folder("Drums")
                .track("Kick", "Kick In.wav")
                .track("Snare", vec!["Snare Top.wav", "Snare Bottom.wav", "Snare Room.wav"])
            .end()
            .build();
        
        let output = display_tracklist(&tracks);
        
        // Expected output:
        // Drums
        // -Snare: [Snare Top.wav, Snare Bottom.wav, Snare Room.wav]
        // -Kick: [Kick In.wav]
        let expected = "Drums\n-Snare: [Snare Top.wav, Snare Bottom.wav, Snare Room.wav]\n-Kick: [Kick In.wav]\n";
        // Strip ANSI codes for comparison
        let stripped = strip_ansi_codes(&output);
        assert_eq!(stripped, expected);
    }

    #[test]
    fn test_display_tracklist_empty_items() {
        // Create a track with no items
        let tracks = TrackStructureBuilder::new()
            .track("Empty Track", Vec::<&str>::new())
            .build();
        
        let output = display_tracklist(&tracks);
        
        // Strip ANSI codes for comparison
        let stripped = strip_ansi_codes(&output);
        assert_eq!(stripped, "Empty Track\n");
    }

    #[test]
    fn test_display_tracklist_nested_folders() {
        // Create a deeply nested structure: Drums -> Percussion -> Shakers -> Shaker 1
        let tracks = TrackStructureBuilder::new()
            .folder("Drums")
                .folder("Percussion")
                    .folder("Shakers")
                        .track("Shaker 1", "Shaker 1.wav")
                    .end()
                .end()
            .end()
            .build();
        
        let output = display_tracklist(&tracks);
        
        // Expected output:
        // Drums
        // -Percussion
        // --Shakers
        // ---Shaker 1: [Shaker 1.wav]
        let expected = "Drums\n-Percussion\n--Shakers\n---Shaker 1: [Shaker 1.wav]\n";
        // Strip ANSI codes for comparison
        let stripped = strip_ansi_codes(&output);
        assert_eq!(stripped, expected);
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

