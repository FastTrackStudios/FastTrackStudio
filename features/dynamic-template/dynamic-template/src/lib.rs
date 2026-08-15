//! DAW binding for the [`music_convention`] classifier.
//!
//! The naming-convention half — instrument groups, item metadata, tempo/song
//! name/Pro Tools/equipment parsing, colors, icons, layouts, track schema and
//! visibility rules — lives in the DAW-free `music-convention` crate and is
//! re-exported here unchanged. What this crate adds is everything that knows
//! about a DAW: turning a [`monarchy::Structure`] into a `daw_proto`
//! [`TrackHierarchy`], the golden session template, and the REAPER module.

use daw_proto::{FolderDepthChange, TrackHierarchy, TrackNode};
use monarchy::*;

// region: --- Modules

pub mod daw_module;
pub mod golden;

pub use golden::golden_template;

// -- Re-exports from music-convention, so `use dynamic_template::*` keeps
//    resolving everything it used to.
pub use music_convention::{
    colors, equipment, error, groups, icons, item_metadata, layouts, metadata_patterns, protools,
    song_name, tempo, track_schema, visibility_rules,
};

pub use music_convention::default_config;
pub use music_convention::is_stem_split_set;
pub use music_convention::ItemMetadata;
pub use music_convention::{
    detect_song_names, detect_song_names_with_config, strip_song_names, SongNameConfig,
};
pub use music_convention::{extract_protools_metadata, strip_protools_markers, ProToolsMetadata};
pub use music_convention::{extract_tempo, strip_tempo};
pub use music_convention::{
    Bass, Choir, Drums, Guide, Guitars, Harmonica, Horns, Keys, Orchestra, Percussion, Reference,
    StemSplit, Strings, Synths, Tracks, Vocals, SFX,
};
pub use music_convention::{Error, Result};

// Re-export monarchy types needed for direct classification
pub use monarchy::{monarchy_sort, Structure};

// endregion: --- Modules

/// Type alias for our standard Config with ItemMetadata
pub type DynamicTemplateConfig = music_convention::MusicConventionConfig;

/// Dynamic template system for organizing DAW items
#[derive(Default)]
pub struct DynamicTemplate;

impl DynamicTemplate {
    pub fn new() -> Self {
        Self
    }
}

/// Options for organizing items into tracks
#[derive(Clone)]
pub struct OrganizeOptions {
    /// Expand multiple items on a node into individual child tracks
    pub expand_items: bool,
    /// Clean up display names by stripping redundant context
    pub cleanup_names: bool,
    /// Collapse single-child intermediate folders (e.g., Cymbals/OH -> OH)
    pub collapse_single_child: bool,
}

impl Default for OrganizeOptions {
    /// Default options enable expansion, cleanup, and collapse
    fn default() -> Self {
        Self {
            expand_items: true,
            cleanup_names: true,
            collapse_single_child: true,
        }
    }
}

impl OrganizeOptions {
    /// Create options with no transformations (raw monarchy sort output)
    pub fn none() -> Self {
        Self {
            expand_items: false,
            cleanup_names: false,
            collapse_single_child: false,
        }
    }
}

/// Trait for organizing item names into a TrackHierarchy using monarchy sort
///
/// This trait accepts anything that can be converted into item names (strings),
/// allowing you to pass strings directly.
pub trait OrganizeIntoTracks {
    /// Organize items into tracks using the provided configuration
    ///
    /// If `existing_tracks` is provided, items will be matched to existing tracks
    /// and new tracks will only be created when needed.
    fn organize_into_tracks(
        self,
        config: &DynamicTemplateConfig,
        existing_tracks: Option<&TrackHierarchy>,
    ) -> monarchy::Result<TrackHierarchy>;

    /// Organize items into tracks with additional options
    ///
    /// Use `OrganizeOptions::with_expansion()` to enable item expansion and name cleanup.
    fn organize_into_tracks_with_options(
        self,
        config: &DynamicTemplateConfig,
        existing_tracks: Option<&TrackHierarchy>,
        options: OrganizeOptions,
    ) -> monarchy::Result<TrackHierarchy>;
}

impl<T> OrganizeIntoTracks for Vec<T>
where
    T: Into<String>,
{
    fn organize_into_tracks(
        self,
        config: &DynamicTemplateConfig,
        existing_tracks: Option<&TrackHierarchy>,
    ) -> monarchy::Result<TrackHierarchy> {
        // Default: no expansion or cleanup (backwards compatible)
        self.organize_into_tracks_with_options(config, existing_tracks, OrganizeOptions::default())
    }

    fn organize_into_tracks_with_options(
        self,
        config: &DynamicTemplateConfig,
        existing_tracks: Option<&TrackHierarchy>,
        options: OrganizeOptions,
    ) -> monarchy::Result<TrackHierarchy> {
        // Convert input to strings
        let input_strings: Vec<String> = self.into_iter().map(|t| t.into()).collect();

        // Detect song/project names that appear in 80%+ of inputs
        let song_name_config = song_name::SongNameConfig::default().with_threshold(0.8);
        let detected_song_names =
            song_name::detect_song_names_with_config(&input_strings, &song_name_config);

        // Perform monarchy sort (clone config since monarchy_sort takes ownership)
        let mut structure = monarchy_sort(input_strings, config)?;

        // Extract tempo from item names and store in metadata
        extract_tempo_from_structure(&mut structure);

        // Apply optional transformations
        if options.collapse_single_child {
            collapse_single_child_folders(&mut structure);
        }
        if options.expand_items {
            expand_items_to_children(&mut structure);
        }
        if options.cleanup_names {
            cleanup_display_names(&mut structure);
            // Strip detected song names from display names (after standard cleanup)
            if !detected_song_names.is_empty() {
                strip_song_names_from_structure(&mut structure, &detected_song_names);
            }
            // Strip tempo/BPM markers from display names
            strip_tempo_from_structure(&mut structure);
            // Strip Pro Tools playlist/take markers from display names
            strip_protools_from_structure(&mut structure);
            // Strip equipment names (mics, preamps, synth hardware) from display names
            strip_equipment_from_structure(&mut structure);
        }

        // Convert Structure to TrackHierarchy
        let mut hierarchy = structure_to_hierarchy(&structure);

        // Merge with existing tracks if provided
        if let Some(existing) = existing_tracks {
            hierarchy = merge_hierarchies(existing, hierarchy);
        }

        Ok(hierarchy)
    }
}

// `impl monarchy::Target<ItemMetadata> for daw_proto::TrackHierarchy` used to
// live here. It is gone: with `ItemMetadata` now owned by `music-convention`,
// the trait, the metadata type and `TrackHierarchy` are all foreign to this
// crate and the impl is an orphan-rule violation. Nothing in the tree called
// `Target::existing_items` — `organize_into_tracks` reads existing tracks
// directly via `merge_hierarchies`. If a caller ever needs it, wrap
// `TrackHierarchy` in a local newtype here and implement `Target` on that.

/// Merge new hierarchy with existing hierarchy, matching by name and adding items to existing tracks
fn merge_hierarchies(existing: &TrackHierarchy, new: TrackHierarchy) -> TrackHierarchy {
    let mut result_tracks: Vec<TrackNode> = existing.tracks.clone();
    let mut existing_names: std::collections::HashSet<String> =
        existing.tracks.iter().map(|t| t.name.clone()).collect();

    for new_track in new.tracks {
        if existing_names.contains(&new_track.name) {
            // Find existing track and merge items
            if let Some(existing_track) =
                result_tracks.iter_mut().find(|t| t.name == new_track.name)
            {
                existing_track.items.extend(new_track.items);
            }
        } else {
            // New track, add it
            existing_names.insert(new_track.name.clone());
            result_tracks.push(new_track);
        }
    }

    TrackHierarchy {
        tracks: result_tracks,
    }
}

/// Trait for converting monarchy Structure to TrackHierarchy
///
/// This recursively converts the hierarchical Structure into a flat list of TrackNodes
/// where folder relationships are represented using folder_depth_change.
pub trait IntoTracks<M: Metadata> {
    /// Convert the Structure to a TrackHierarchy
    ///
    /// # Example
    /// ```ignore
    /// use dynamic_template::{default_config, IntoTracks};
    /// use monarchy::monarchy_sort;
    ///
    /// let structure = monarchy_sort(vec!["Kick In"], default_config())?;
    /// let hierarchy = structure.to_tracks();
    /// ```
    fn to_tracks(self) -> TrackHierarchy;
}

impl<M: Metadata + ToDisplayName> IntoTracks<M> for Structure<M> {
    fn to_tracks(self) -> TrackHierarchy {
        structure_to_hierarchy(&self)
    }
}

/// Convert a monarchy Structure to a TrackHierarchy
fn structure_to_hierarchy<M: Metadata + ToDisplayName>(structure: &Structure<M>) -> TrackHierarchy {
    let mut tracks = Vec::new();
    structure_to_tracks_recursive(structure, &mut tracks, true, &[]);
    TrackHierarchy { tracks }
}

/// Helper function to recursively convert Structure to TrackNodes
///
/// The `group_path` parameter tracks the hierarchy of group names from root to
/// current node, used for color lookup via `colors::color_for_path()`.
fn structure_to_tracks_recursive<M: Metadata + ToDisplayName>(
    structure: &Structure<M>,
    tracks: &mut Vec<TrackNode>,
    skip_root: bool,
    group_path: &[&str],
) {
    // Skip root if it's just a container (name is "root" or empty)
    if skip_root && (structure.name == "root" || structure.name.is_empty()) {
        // Process all children
        for child in &structure.children {
            structure_to_tracks_recursive(child, tracks, false, group_path);
        }
        return;
    }

    // Build the path for this node (used for color lookup)
    let mut current_path: Vec<&str> = group_path.to_vec();
    current_path.push(&structure.name);

    // Create a track node for this structure
    let mut track = TrackNode::new(structure.name.clone());

    // Look up color from the hierarchical path
    track.color = colors::color_for_path(&current_path).map(|c| c.to_hex());

    // Add items from monarchy structure to the track
    for monarchy_item in &structure.items {
        track.items.push(monarchy_item.original.clone());
    }

    // If this structure has children, it's a folder
    if !structure.children.is_empty() {
        track.is_folder = true;
        track.folder_depth_change = FolderDepthChange::FolderStart;
        tracks.push(track);

        // Recursively convert children
        let num_children = structure.children.len();
        for (i, child) in structure.children.iter().enumerate() {
            let tracks_before = tracks.len();
            structure_to_tracks_recursive(child, tracks, false, &current_path);

            // If this is the last child, apply folder closing to the last track added
            if i == num_children - 1 && tracks.len() > tracks_before {
                if let Some(last_track) = tracks.last_mut() {
                    let current = last_track.folder_depth_change.to_raw_value();
                    last_track.folder_depth_change = FolderDepthChange::from_raw_value(current - 1);
                }
            }
        }
    } else {
        // Leaf track
        tracks.push(track);
    }
}

/// Extract tempo from all items in a structure and store in their metadata
///
/// This function recursively traverses the structure and extracts tempo (BPM)
/// from each item's original name, storing it in the item's metadata.
fn extract_tempo_from_structure(structure: &mut Structure<ItemMetadata>) {
    // Extract tempo for items at this level
    for item in &mut structure.items {
        if item.metadata.tempo.is_none() {
            if let Some(tempo) = tempo::extract_tempo(&item.original) {
                item.metadata.tempo = Some(tempo);
            }
        }
    }

    // Recursively process children
    for child in &mut structure.children {
        extract_tempo_from_structure(child);
    }
}

/// Strip detected song names from structure display names
///
/// This function recursively traverses the structure and strips song/project names
/// from each node's display name. Song names are detected as strings that appear
/// in ALL input items and don't match known audio production patterns.
fn strip_song_names_from_structure(
    structure: &mut Structure<ItemMetadata>,
    song_names: &std::collections::HashSet<String>,
) {
    // Strip song names from this node's display name
    if !structure.name.is_empty() {
        let cleaned = song_name::strip_song_names(&structure.name, song_names);
        if !cleaned.is_empty() && cleaned != structure.name {
            structure.name = cleaned;
        }
    }

    // Recursively process children
    for child in &mut structure.children {
        strip_song_names_from_structure(child, song_names);
    }
}

/// Strip tempo/BPM markers from structure display names
///
/// This function recursively traverses the structure and strips tempo patterns
/// like "126bpm", "83.5BPM" from each node's display name.
fn strip_tempo_from_structure(structure: &mut Structure<ItemMetadata>) {
    // Strip tempo from this node's display name
    if !structure.name.is_empty() {
        let cleaned = tempo::strip_tempo(&structure.name);
        if !cleaned.is_empty() && cleaned != structure.name {
            structure.name = cleaned;
        }
    }

    // Recursively process children
    for child in &mut structure.children {
        strip_tempo_from_structure(child);
    }
}

/// Strip Pro Tools playlist/take markers from structure display names
///
/// This function recursively traverses the structure and strips Pro Tools
/// session metadata like "_01", ".01", "_01-02" from each node's display name.
fn strip_protools_from_structure(structure: &mut Structure<ItemMetadata>) {
    // Strip Pro Tools markers from this node's display name
    if !structure.name.is_empty() {
        let cleaned = protools::strip_protools_markers(&structure.name);
        if !cleaned.is_empty() && cleaned != structure.name {
            structure.name = cleaned;
        }
    }

    // Recursively process children
    for child in &mut structure.children {
        strip_protools_from_structure(child);
    }
}

/// Strip equipment names from structure display names
///
/// This function recursively traverses the structure and strips equipment-related
/// metadata like mic models (U47, SM57), preamp models (OptoComp, 1176),
/// processing markers (LIM, EDT), and synth hardware (CASIO, Roland FA06).
fn strip_equipment_from_structure(structure: &mut Structure<ItemMetadata>) {
    // Strip equipment from this node's display name
    if !structure.name.is_empty() {
        let cleaned = equipment::strip_equipment(&structure.name);
        if !cleaned.is_empty() && cleaned != structure.name {
            structure.name = cleaned;
        }
    }

    // Recursively process children
    for child in &mut structure.children {
        strip_equipment_from_structure(child);
    }
}
