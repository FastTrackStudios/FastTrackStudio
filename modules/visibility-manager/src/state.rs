//! Visibility Manager state
//!
//! The main state machine for managing track visibility. Unlike the original
//! Lua implementation, this version dynamically analyzes tracks and creates
//! groups based on actual content rather than hardcoded presets.

use crate::classification::{classify_tracks, known_groups, TrackClassification};
use crate::error::{Error, Result};
use crate::group::{color_for_category, VisibilityGroup, VisibilityGroupId};
use crate::snapshot::{Snapshot, SnapshotId, SnapshotStore};
use crate::visibility::{TrackVisibility, VisibilityChanges, VisibilityTarget};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// View modes that control how groups interact
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
pub enum ViewMode {
    /// Toggle mode: groups can be independently toggled on/off
    #[default]
    Toggle,

    /// Exclusive mode: only one group can be active at a time
    Exclusive,

    /// Limited exclusive: collapsed view showing only folders
    LimitedExclusive,

    /// Show all: all tracks visible regardless of group state
    ShowAll,
}

impl ViewMode {
    pub fn is_exclusive(&self) -> bool {
        matches!(self, ViewMode::Exclusive | ViewMode::LimitedExclusive)
    }
}

/// The main visibility manager state
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VisibilityManager {
    /// All visibility groups (both auto-generated and custom)
    groups: HashMap<VisibilityGroupId, VisibilityGroup>,

    /// Order of groups for UI display
    group_order: Vec<VisibilityGroupId>,

    /// Current view mode
    view_mode: ViewMode,

    /// Visibility target (TCP, MCP, or Both)
    target: VisibilityTarget,

    /// Snapshot store
    snapshots: SnapshotStore,

    /// Track classifications (cached from last analysis)
    classifications: HashMap<String, TrackClassification>,

    /// Track visibility states
    track_visibility: HashMap<usize, TrackVisibility>,

    /// Total number of tracks in the project
    track_count: usize,
}

impl Default for VisibilityManager {
    fn default() -> Self {
        Self::new()
    }
}

impl VisibilityManager {
    /// Create a new visibility manager
    pub fn new() -> Self {
        Self {
            groups: HashMap::new(),
            group_order: Vec::new(),
            view_mode: ViewMode::Toggle,
            target: VisibilityTarget::Both,
            snapshots: SnapshotStore::new(),
            classifications: HashMap::new(),
            track_visibility: HashMap::new(),
            track_count: 0,
        }
    }

    /// Analyze tracks and create groups based on their classifications
    ///
    /// This is the main entry point for dynamically creating visibility groups
    /// based on the actual tracks in the project.
    pub fn analyze_tracks(&mut self, tracks: &[daw::tracks::Track]) {
        self.track_count = tracks.len();

        // Extract track names
        let track_names: Vec<&str> = tracks.iter().map(|t| t.name.0.as_str()).collect();

        // Classify all tracks
        self.classifications = classify_tracks(track_names);

        // Initialize track visibility from current state
        for (index, track) in tracks.iter().enumerate() {
            let visibility = TrackVisibility::from_track(index, track);
            self.track_visibility.insert(index, visibility);
        }

        // Create groups based on classifications
        self.create_groups_from_classifications(tracks);
    }

    /// Create visibility groups from track classifications
    fn create_groups_from_classifications(&mut self, tracks: &[daw::tracks::Track]) {
        // Clear existing auto-generated groups (keep custom ones)
        self.groups.retain(|_, g| g.is_custom);
        self.group_order.retain(|id| {
            self.groups.get(id).map(|g| g.is_custom).unwrap_or(false)
        });

        // Create groups for each known category
        for category in known_groups() {
            let mut group = VisibilityGroup::for_category(category);

            // Set color for the category
            if let Some(color) = color_for_category(category) {
                group.color = Some(color);
            }

            // Find tracks that belong to this category
            for (index, track) in tracks.iter().enumerate() {
                if let Some(classification) = self.classifications.get(&track.name.0) {
                    if classification.primary_group.as_deref() == Some(category) {
                        group.add_track(index);
                    }
                }
            }

            // Only add non-empty groups
            if !group.is_empty() {
                let id = group.id.clone();
                self.group_order.push(id.clone());
                self.groups.insert(id, group);
            }
        }

        // Create an "Unsorted" group for unclassified tracks
        let mut unsorted = VisibilityGroup::new("unsorted", "Unsorted");
        unsorted.color = Some(0x666666);

        for (index, track) in tracks.iter().enumerate() {
            let classification = self.classifications.get(&track.name.0);
            let is_classified = classification
                .map(|c| c.is_classified())
                .unwrap_or(false);

            if !is_classified {
                unsorted.add_track(index);
            }
        }

        if !unsorted.is_empty() {
            let id = unsorted.id.clone();
            self.group_order.push(id.clone());
            self.groups.insert(id, unsorted);
        }
    }

    /// Get all groups in display order
    pub fn groups(&self) -> Vec<&VisibilityGroup> {
        self.group_order
            .iter()
            .filter_map(|id| self.groups.get(id))
            .collect()
    }

    /// Get a group by ID
    pub fn get_group(&self, id: &VisibilityGroupId) -> Option<&VisibilityGroup> {
        self.groups.get(id)
    }

    /// Get a mutable group by ID
    pub fn get_group_mut(&mut self, id: &VisibilityGroupId) -> Option<&mut VisibilityGroup> {
        self.groups.get_mut(id)
    }

    /// Add a custom group
    pub fn add_custom_group(&mut self, mut group: VisibilityGroup) {
        group.is_custom = true;
        let id = group.id.clone();
        self.group_order.push(id.clone());
        self.groups.insert(id, group);
    }

    /// Remove a group
    pub fn remove_group(&mut self, id: &VisibilityGroupId) -> Option<VisibilityGroup> {
        self.group_order.retain(|i| i != id);
        self.groups.remove(id)
    }

    /// Get current view mode
    pub fn view_mode(&self) -> ViewMode {
        self.view_mode
    }

    /// Set view mode
    pub fn set_view_mode(&mut self, mode: ViewMode) {
        self.view_mode = mode;
    }

    /// Get visibility target
    pub fn target(&self) -> VisibilityTarget {
        self.target
    }

    /// Set visibility target
    pub fn set_target(&mut self, target: VisibilityTarget) {
        self.target = target;
    }

    /// Activate a group (show its tracks)
    ///
    /// In Exclusive mode, this also deactivates other groups.
    pub fn activate_group(&mut self, id: &VisibilityGroupId) -> Result<VisibilityChanges> {
        if !self.groups.contains_key(id) {
            return Err(Error::GroupNotFound(id.0.clone()));
        }

        let mut changes = VisibilityChanges::new(self.target);

        if self.view_mode.is_exclusive() {
            // Deactivate all other groups first
            for group in self.groups.values_mut() {
                if &group.id != id && group.active {
                    group.active = false;
                    changes.hide.extend(group.track_indices.iter().copied());
                }
            }
        }

        // Activate the target group
        if let Some(group) = self.groups.get_mut(id) {
            group.active = true;
            changes.show.extend(group.track_indices.iter().copied());

            // Update track visibility
            for &index in &group.track_indices {
                if let Some(vis) = self.track_visibility.get_mut(&index) {
                    vis.show(self.target);
                }
            }
        }

        Ok(changes)
    }

    /// Deactivate a group (hide its tracks)
    pub fn deactivate_group(&mut self, id: &VisibilityGroupId) -> Result<VisibilityChanges> {
        let group = self.groups.get_mut(id)
            .ok_or_else(|| Error::GroupNotFound(id.0.clone()))?;

        let mut changes = VisibilityChanges::new(self.target);

        group.active = false;
        changes.hide.extend(group.track_indices.iter().copied());

        // Update track visibility
        for &index in &group.track_indices {
            if let Some(vis) = self.track_visibility.get_mut(&index) {
                vis.hide(self.target);
            }
        }

        Ok(changes)
    }

    /// Toggle a group's visibility
    pub fn toggle_group(&mut self, id: &VisibilityGroupId) -> Result<VisibilityChanges> {
        let is_active = self.groups.get(id)
            .ok_or_else(|| Error::GroupNotFound(id.0.clone()))?
            .active;

        if is_active {
            self.deactivate_group(id)
        } else {
            self.activate_group(id)
        }
    }

    /// Show all tracks
    pub fn show_all(&mut self) -> VisibilityChanges {
        let mut changes = VisibilityChanges::new(self.target);

        // Activate all groups
        for group in self.groups.values_mut() {
            group.active = true;
        }

        // Show all tracks
        for vis in self.track_visibility.values_mut() {
            vis.show(self.target);
            changes.show.push(vis.track_index);
        }

        changes
    }

    /// Hide all tracks
    pub fn hide_all(&mut self) -> VisibilityChanges {
        let mut changes = VisibilityChanges::new(self.target);

        // Deactivate all groups
        for group in self.groups.values_mut() {
            group.active = false;
        }

        // Hide all tracks
        for vis in self.track_visibility.values_mut() {
            vis.hide(self.target);
            changes.hide.push(vis.track_index);
        }

        changes
    }

    /// Save current visibility state as a snapshot
    pub fn save_snapshot(&mut self, name: impl Into<String>) -> SnapshotId {
        let mut snapshot = Snapshot::new(name, self.target);

        for (index, vis) in &self.track_visibility {
            snapshot.set_track_visibility(*index, vis.clone());
        }

        let id = snapshot.id.clone();
        self.snapshots.add(snapshot);
        id
    }

    /// Restore visibility from a snapshot
    pub fn restore_snapshot(&mut self, id: &SnapshotId) -> Result<VisibilityChanges> {
        let snapshot = self.snapshots.get(id)
            .ok_or_else(|| Error::SnapshotNotFound(id.0.clone()))?
            .clone();

        let mut changes = VisibilityChanges::new(snapshot.target);

        // Apply snapshot visibility to track states
        for (index, vis) in snapshot.track_states {
            if let Some(current_vis) = self.track_visibility.get_mut(&index) {
                if vis.is_visible(snapshot.target) {
                    if !current_vis.is_visible(snapshot.target) {
                        changes.show.push(index);
                    }
                    current_vis.show(snapshot.target);
                } else {
                    if current_vis.is_visible(snapshot.target) {
                        changes.hide.push(index);
                    }
                    current_vis.hide(snapshot.target);
                }
            }
        }

        // Update group active states based on track visibility
        for group in self.groups.values_mut() {
            let all_visible = group.track_indices.iter().all(|&idx| {
                self.track_visibility.get(&idx)
                    .map(|v| v.is_visible(snapshot.target))
                    .unwrap_or(false)
            });
            group.active = all_visible;
        }

        Ok(changes)
    }

    /// Get the snapshot store
    pub fn snapshots(&self) -> &SnapshotStore {
        &self.snapshots
    }

    /// Get mutable snapshot store
    pub fn snapshots_mut(&mut self) -> &mut SnapshotStore {
        &mut self.snapshots
    }

    /// Get track visibility state
    pub fn get_track_visibility(&self, index: usize) -> Option<&TrackVisibility> {
        self.track_visibility.get(&index)
    }

    /// Get all visible track indices
    pub fn visible_tracks(&self) -> Vec<usize> {
        self.track_visibility
            .iter()
            .filter(|(_, v)| v.is_visible(self.target))
            .map(|(idx, _)| *idx)
            .collect()
    }

    /// Get all hidden track indices
    pub fn hidden_tracks(&self) -> Vec<usize> {
        self.track_visibility
            .iter()
            .filter(|(_, v)| !v.is_visible(self.target))
            .map(|(idx, _)| *idx)
            .collect()
    }

    /// Get the total number of tracks
    pub fn track_count(&self) -> usize {
        self.track_count
    }

    /// Get classification for a track by name
    pub fn get_classification(&self, track_name: &str) -> Option<&TrackClassification> {
        self.classifications.get(track_name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use daw::tracks::Track;

    fn create_test_tracks() -> Vec<Track> {
        vec![
            Track::new("Kick In"),
            Track::new("Kick Out"),
            Track::new("Snare Top"),
            Track::new("Bass DI"),
            Track::new("Electric Guitar"),
            Track::new("Lead Vox"),
            Track::new("Random Track"),
        ]
    }

    #[test]
    fn test_analyze_tracks() {
        let tracks = create_test_tracks();
        let mut manager = VisibilityManager::new();

        manager.analyze_tracks(&tracks);

        // Should have created groups
        assert!(!manager.groups.is_empty());

        // Drums group should exist and contain kick/snare
        let drums = manager.get_group(&VisibilityGroupId::new("drums"));
        assert!(drums.is_some());
        let drums = drums.unwrap();
        assert!(drums.track_count() >= 2); // At least kick and snare
    }

    #[test]
    fn test_toggle_group() {
        let tracks = create_test_tracks();
        let mut manager = VisibilityManager::new();
        manager.analyze_tracks(&tracks);

        let drums_id = VisibilityGroupId::new("drums");

        // Initially active
        assert!(manager.get_group(&drums_id).unwrap().active);

        // Toggle off
        let changes = manager.toggle_group(&drums_id).unwrap();
        assert!(!changes.hide.is_empty());
        assert!(!manager.get_group(&drums_id).unwrap().active);

        // Toggle on
        let changes = manager.toggle_group(&drums_id).unwrap();
        assert!(!changes.show.is_empty());
        assert!(manager.get_group(&drums_id).unwrap().active);
    }

    #[test]
    fn test_exclusive_mode() {
        let tracks = create_test_tracks();
        let mut manager = VisibilityManager::new();
        manager.analyze_tracks(&tracks);
        manager.set_view_mode(ViewMode::Exclusive);

        let drums_id = VisibilityGroupId::new("drums");
        let bass_id = VisibilityGroupId::new("bass");

        // Activate drums (should work)
        manager.activate_group(&drums_id).unwrap();
        assert!(manager.get_group(&drums_id).unwrap().active);

        // Activate bass (should deactivate drums)
        manager.activate_group(&bass_id).unwrap();
        assert!(manager.get_group(&bass_id).unwrap().active);
        assert!(!manager.get_group(&drums_id).unwrap().active);
    }

    #[test]
    fn test_snapshot() {
        let tracks = create_test_tracks();
        let mut manager = VisibilityManager::new();
        manager.analyze_tracks(&tracks);

        // Hide drums
        let drums_id = VisibilityGroupId::new("drums");
        manager.deactivate_group(&drums_id).unwrap();

        // Save snapshot
        let snapshot_id = manager.save_snapshot("My Layout");

        // Show drums again
        manager.activate_group(&drums_id).unwrap();

        // Restore snapshot (should hide drums again)
        let changes = manager.restore_snapshot(&snapshot_id).unwrap();
        assert!(!changes.hide.is_empty());
    }
}
