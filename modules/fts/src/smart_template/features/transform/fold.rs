//! Template Fold Pattern
//!
//! Provides a functional, immutable way to transform templates based on context.
//! The fold pattern allows transformations that can read from the original template
//! (context) while producing a new template (immutable and stateless).

use crate::smart_template::core::models::template::Template;
use crate::smart_template::utils::track_helpers::TrackExt;
use crate::smart_template::core::models::types::TrackName;
use daw::tracks::Track;
use std::collections::{HashMap, HashSet};

/// Context for folding operations
///
/// Provides read-only access to the original template and tracks,
/// allowing folders to make context-aware decisions.
#[derive(Debug, Clone)]
pub struct FoldContext<'a> {
    /// THE original template being folded
    pub original_template: &'a Template,
    
    /// Map of track name -> track for quick lookup
    pub tracks_by_name: HashMap<TrackName, &'a Track>,
    
    /// Map of parent name -> children tracks
    pub children_by_parent: HashMap<String, Vec<&'a Track>>,
    
    /// Set of all track names in the original template
    pub track_names: HashSet<TrackName>,
    
    /// Map of normalized track names (without numbers/playlists) -> tracks
    pub tracks_by_base_name: HashMap<String, Vec<&'a Track>>,
}

impl<'a> FoldContext<'a> {
    /// Create a new fold context from a template
    pub fn new(template: &'a Template) -> Self {
        let mut tracks_by_name = HashMap::new();
        let mut children_by_parent = HashMap::new();
        let mut track_names = HashSet::new();
        let mut tracks_by_base_name = HashMap::new();
        
        for track in &template.tracks {
            tracks_by_name.insert(track.name.clone(), track);
            track_names.insert(track.name.clone());
            
            // Index by parent
            if let Some(parent) = track.parent_name() {
                children_by_parent
                    .entry(parent.to_string())
                    .or_insert_with(Vec::new)
                    .push(track);
            }
            
            // Index by base name (normalized)
            let base_name = Self::normalize_base_name(&track.name.0);
            tracks_by_base_name
                .entry(base_name)
                .or_insert_with(Vec::new)
                .push(track);
        }
        
        Self {
            original_template: template,
            tracks_by_name,
            children_by_parent,
            track_names,
            tracks_by_base_name,
        }
    }
    
    /// Normalize a track name to its base (removing numbers, playlists, etc.)
    ///
    /// Examples:
    /// - "Snare" -> "Snare"
    /// - "Snare 2" -> "Snare"
    /// - "Snare .1" -> "Snare"
    /// - "Snare Top" -> "Snare Top"
    fn normalize_base_name(name: &str) -> String {
        // Remove playlist suffix (e.g., ".1", ".2", ".A")
        let without_playlist = if let Some(dot_pos) = name.rfind('.') {
            if dot_pos < name.len() - 1 {
                let after_dot = &name[dot_pos + 1..];
                if after_dot.chars().all(|c| c.is_ascii_digit() || c.is_ascii_alphabetic()) {
                    name[..dot_pos].trim_end().to_string()
                } else {
                    name.to_string()
                }
            } else {
                name.to_string()
            }
        } else {
            name.to_string()
        };
        
        // Remove trailing numbers (e.g., "Snare 2" -> "Snare")
        // But preserve numbers in the middle (e.g., "Track 1 Top" -> "Track 1 Top")
        let parts: Vec<&str> = without_playlist.split_whitespace().collect();
        if parts.len() > 1 {
            // Check if last part is a number
            if let Some(last) = parts.last() {
                if last.chars().all(|c| c.is_ascii_digit()) {
                    // Last part is a number, remove it
                    parts[..parts.len() - 1].join(" ")
                } else {
                    without_playlist
                }
            } else {
                without_playlist
            }
        } else {
            without_playlist
        }
    }
    
    /// Check if a track name exists in the original template
    pub fn has_track(&self, name: &TrackName) -> bool {
        self.track_names.contains(name)
    }
    
    /// Get a track by name from the original template
    pub fn get_track(&self, name: &TrackName) -> Option<&'a Track> {
        self.tracks_by_name.get(name).copied()
    }
    
    /// Get all tracks with the same base name (e.g., "Snare", "Snare 2" both have base "Snare")
    pub fn get_tracks_by_base_name(&self, base_name: &str) -> Vec<&'a Track> {
        let normalized = Self::normalize_base_name(base_name);
        self.tracks_by_base_name
            .get(&normalized)
            .map(|tracks| tracks.clone())
            .unwrap_or_default()
    }
    
    /// Get children of a track
    pub fn get_children(&self, parent_name: &TrackName) -> Vec<&'a Track> {
        self.children_by_parent
            .get(&parent_name.0)
            .map(|children| children.clone())
            .unwrap_or_default()
    }
    
    /// Count how many tracks exist with the same base name
    pub fn count_by_base_name(&self, base_name: &str) -> usize {
        self.get_tracks_by_base_name(base_name).len()
    }
}

/// Trait for folding (transforming) templates
///
/// The fold pattern allows you to transform templates in an immutable, functional way.
/// Folders can read from the original template (via FoldContext) to make context-aware
/// decisions while producing a new template.
pub trait TemplateFolder {
    /// Transform a single track
    ///
    /// This is called for each track in the template. The default implementation
    /// recursively folds children. Override this to transform individual tracks.
    ///
    /// # Arguments
    /// * `track` - The track to transform
    /// * `context` - Read-only access to the original template
    ///
    /// # Returns
    /// The transformed track, or None if the track should be filtered out
    fn fold_track(&mut self, track: Track, context: &FoldContext) -> Option<Track> {
        // Default: just transform the track itself
        Some(self.transform_track(track, context))
    }
    
    /// Transform a single track without considering children
    ///
    /// Override this to change track properties, names, etc.
    fn transform_track(&mut self, track: Track, _context: &FoldContext) -> Track {
        track // Default: no transformation
    }
    
    /// Fold an entire template
    ///
    /// The default implementation:
    /// 1. Builds a context from the original template
    /// 2. Folds each track individually
    /// 3. Collects all folded tracks (including children)
    /// 4. Builds a new template with the transformed tracks
    fn fold_template(&mut self, template: Template) -> Template {
        let context = FoldContext::new(&template);
        
        // Fold all tracks, maintaining the flat structure
        let mut folded_tracks = Vec::new();
        let mut processed = HashSet::new();
        
        // Process tracks in order, but handle hierarchy by processing parents first
        // We'll do multiple passes: first roots, then their children
        let mut to_process: Vec<&Track> = template.tracks
            .iter()
            .filter(|t| t.parent_name().is_none())
            .collect();
        
        while !to_process.is_empty() {
            let mut next_batch = Vec::new();
            
            for track in to_process {
                if processed.contains(&track.name) {
                    continue;
                }
                
                if let Some(folded) = self.fold_track(track.clone(), &context) {
                    // Get children before we modify the track name
                    let children = context.get_children(&track.name);
                    
                    // Add children to next batch
                    for child in children {
                        if !processed.contains(&child.name) {
                            next_batch.push(child);
                        }
                    }
                    
                    folded_tracks.push(folded);
                    processed.insert(track.name.clone());
                }
            }
            
            to_process = next_batch;
        }
        
        // Also process any remaining tracks that weren't reached via hierarchy
        for track in &template.tracks {
            if !processed.contains(&track.name) {
                if let Some(folded) = self.fold_track(track.clone(), &context) {
                    folded_tracks.push(folded);
                }
            }
        }
        
        Template {
            name: self.transform_template_name(&template.name, &context),
            tracks: folded_tracks,
        }
    }
    
    /// Transform template name
    fn transform_template_name(&mut self, name: &str, _context: &FoldContext) -> String {
        name.to_string() // Default: no change
    }
}

/// Folder that auto-increments tracks into separate groups
///
/// If a track like "Snare" exists and we encounter "Snare 2", this folder
/// will automatically put "Snare 2" into a separate group.
///
/// Example:
/// - Original: "Snare" (in group "Snare")
/// - New track: "Snare 2"
/// - Result: "Snare 2" gets its own group "Snare 2" instead of being under "Snare"
pub struct AutoIncrementGroupFolder {
    /// Whether to create new groups for incremented tracks
    pub create_separate_groups: bool,
}

impl AutoIncrementGroupFolder {
    pub fn new() -> Self {
        Self {
            create_separate_groups: true,
        }
    }
    
    pub fn with_separate_groups(mut self, separate: bool) -> Self {
        self.create_separate_groups = separate;
        self
    }
}

impl Default for AutoIncrementGroupFolder {
    fn default() -> Self {
        Self::new()
    }
}

impl TemplateFolder for AutoIncrementGroupFolder {
    fn transform_track(&mut self, mut track: Track, context: &FoldContext) -> Track {
        if !self.create_separate_groups {
            return track;
        }
        
        let base_name = FoldContext::normalize_base_name(&track.name.0);
        let existing_tracks = context.get_tracks_by_base_name(&base_name);
        
        // Check if this track name ends with a number (e.g., "Snare 2")
        let parts: Vec<&str> = track.name.0.split_whitespace().collect();
        let is_incremented = parts.len() > 1 
            && parts.last()
                .map(|last| last.chars().all(|c| c.is_ascii_digit()))
                .unwrap_or(false);
        
        // If this is an incremented track (e.g., "Snare 2") and the base track exists
        if is_incremented && existing_tracks.len() > 1 {
            // Check if the base track (without number) exists
            let base_track_exists = existing_tracks.iter()
                .any(|t| FoldContext::normalize_base_name(&t.name.0) == base_name 
                     && t.name.0 == base_name);
            
            if base_track_exists {
                // Make this track a root (separate group) by removing parent
                // Remove the parent metadata entirely to make it a root
                track.remove_metadata(daw::tracks::MetadataKey::PARENT);
                
                // Optionally, set the track type to "BUS" to indicate it's a group
                if track.track_type().is_none() {
                    track.set_track_type("BUS");
                }
            }
        }
        
        track
    }
}

/// Folder that handles playlist matching intelligently
///
/// If tracks with the same base name exist with different playlists (e.g., "Snare .1", "Snare .2"),
/// this folder ensures they're properly grouped and marked for playlist usage.
pub struct PlaylistMatchingFolder {
    /// Whether to use playlists (fixed item lanes) for matching tracks
    pub use_playlists: bool,
}

impl PlaylistMatchingFolder {
    pub fn new() -> Self {
        Self {
            use_playlists: true,
        }
    }
    
    pub fn with_playlists(mut self, use_playlists: bool) -> Self {
        self.use_playlists = use_playlists;
        self
    }
}

impl Default for PlaylistMatchingFolder {
    fn default() -> Self {
        Self::new()
    }
}

impl TemplateFolder for PlaylistMatchingFolder {
    fn transform_track(&mut self, mut track: Track, context: &FoldContext) -> Track {
        if !self.use_playlists {
            return track;
        }
        
        // Extract base name and playlist
        let base_name = FoldContext::normalize_base_name(&track.name.0);
        let tracks_with_same_base = context.get_tracks_by_base_name(&base_name);
        
        // If there are multiple tracks with the same base name, they might be playlists
        if tracks_with_same_base.len() > 1 {
            // Check if this track has a playlist suffix
            if track.name.0.contains('.') {
                // Mark this track for playlist usage
                // Store this in metadata for later use
                track.set_metadata(daw::tracks::MetadataKey::USE_PLAYLIST, "true");
            }
        }
        
        track
    }
}

/// Folder that filters tracks by group mode
///
/// This is a refactored version of `filter_by_mode` using the fold pattern.
pub struct ModeFilterFolder {
    mode: crate::smart_template::core::models::GroupMode,
    include_parents: bool,
}

impl ModeFilterFolder {
    pub fn new(mode: crate::smart_template::core::models::GroupMode) -> Self {
        Self {
            mode,
            include_parents: true,
        }
    }
    
    pub fn with_parents(mut self, include: bool) -> Self {
        self.include_parents = include;
        self
    }
}

impl TemplateFolder for ModeFilterFolder {
    fn fold_track(&mut self, track: Track, context: &FoldContext) -> Option<Track> {
        use crate::smart_template::utils::track_helpers::TrackExt;
        
        let modes = track.get_modes();
        let should_include = modes.is_empty() || modes.contains(&self.mode);
        
        if should_include {
            Some(track)
        } else {
            // Check if we should include parent tracks that have included children
            if self.include_parents {
                let children = context.get_children(&track.name);
                let has_included_child = children.iter().any(|child| {
                    let child_modes = child.get_modes();
                    child_modes.is_empty() || child_modes.contains(&self.mode)
                });
                
                if has_included_child {
                    return Some(track);
                }
            }
            
            None // Filter out this track
        }
    }
    
    fn transform_template_name(&mut self, name: &str, _context: &FoldContext) -> String {
        format!("{} ({})", name, self.mode.as_str())
    }
}

/// Folder that renames tracks
pub struct TrackRenamerFolder {
    rename_map: HashMap<String, String>,
}

impl TrackRenamerFolder {
    pub fn new() -> Self {
        Self {
            rename_map: HashMap::new(),
        }
    }
    
    pub fn add_rename(&mut self, from: impl Into<String>, to: impl Into<String>) {
        self.rename_map.insert(from.into(), to.into());
    }
}

impl Default for TrackRenamerFolder {
    fn default() -> Self {
        Self::new()
    }
}

impl TemplateFolder for TrackRenamerFolder {
    fn transform_track(&mut self, mut track: Track, _context: &FoldContext) -> Track {
        if let Some(new_name) = self.rename_map.get(&track.name) {
            track.name = new_name.clone();
        }
        track
    }
}

/// Folder that modifies track properties
pub struct TrackPropertyModifier {
    volume_multiplier: Option<f64>,
    mute_all: bool,
}

impl TrackPropertyModifier {
    pub fn new() -> Self {
        Self {
            volume_multiplier: None,
            mute_all: false,
        }
    }
    
    pub fn with_volume_multiplier(mut self, multiplier: f64) -> Self {
        self.volume_multiplier = Some(multiplier);
        self
    }
    
    pub fn with_mute_all(mut self, mute: bool) -> Self {
        self.mute_all = mute;
        self
    }
}

impl Default for TrackPropertyModifier {
    fn default() -> Self {
        Self::new()
    }
}

impl TemplateFolder for TrackPropertyModifier {
    fn transform_track(&mut self, mut track: Track, _context: &FoldContext) -> Track {
        if let Some(multiplier) = self.volume_multiplier {
            track.volume *= multiplier;
        }
        
        if self.mute_all {
            track.muted = true;
        }
        
        track
    }
}

/// Composable folder that chains multiple folders
///
/// This allows you to apply multiple transformations in sequence.
pub struct ComposedFolder<F1, F2> {
    first: F1,
    second: F2,
}

impl<F1: TemplateFolder, F2: TemplateFolder> TemplateFolder for ComposedFolder<F1, F2> {
    fn fold_template(&mut self, template: Template) -> Template {
        let intermediate = self.first.fold_template(template);
        self.second.fold_template(intermediate)
    }
}

/// Extension trait for Template to add fold methods
impl Template {
    /// Fold this template using a folder
    pub fn fold<F: TemplateFolder>(self, mut folder: F) -> Template {
        folder.fold_template(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::smart_template::utils::track_helpers::create_track;
    use crate::smart_template::core::models::GroupMode;
    
    #[test]
    fn test_auto_increment_group_folder() {
        // Create a template with "Snare" and "Snare 2"
        let mut template = Template {
            name: "Test".to_string(),
            tracks: vec![
                create_track("Snare", Some("BUS"), None, &[]),
                create_track("Snare 2", None, Some("Snare"), &[]),
            ],
        };
        
        // Apply auto-increment folder
        let folder = AutoIncrementGroupFolder::new();
        let result = template.fold(folder);
        
        // "Snare 2" should now be a separate group (no parent or parent is itself)
        let snare_2 = result.tracks.iter().find(|t| t.name == "Snare 2").unwrap();
        // The parent should be cleared or set to itself
        assert!(snare_2.parent_name().is_none() || snare_2.parent_name() == Some("Snare 2"));
    }
    
    #[test]
    fn test_mode_filter_folder() {
        let template = Template {
            name: "Test".to_string(),
            tracks: vec![
                create_track("Track 1", None, None, &[GroupMode::Full]),
                create_track("Track 2", None, None, &[GroupMode::Recording]),
            ],
        };
        
        let folder = ModeFilterFolder::new(GroupMode::Recording);
        let result = template.fold(folder);
        
        assert_eq!(result.tracks.len(), 1);
        assert_eq!(result.tracks[0].name, "Track 2");
    }
    
    #[test]
    fn test_composed_folders() {
        let template = Template {
            name: "Test".to_string(),
            tracks: vec![
                create_track("Old Name", None, None, &[]),
            ],
        };
        
        let mut renamer = TrackRenamerFolder::new();
        renamer.add_rename("Old Name", "New Name");
        
        let modifier = TrackPropertyModifier::new()
            .with_volume_multiplier(0.8);
        
        // Compose: first rename, then modify
        let mut composed = ComposedFolder {
            first: renamer,
            second: modifier,
        };
        
        let result = composed.fold_template(template);
        
        assert_eq!(result.tracks[0].name, "New Name");
        assert_eq!(result.tracks[0].volume, 0.8);
    }
}
