//! Visibility groups
//!
//! Groups are collections of tracks that can be shown/hidden together.
//! Unlike hardcoded presets, groups are dynamically created based on
//! the actual tracks in the project.

use serde::{Deserialize, Serialize};
use std::collections::HashSet;

/// Unique identifier for a visibility group
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct VisibilityGroupId(pub String);

impl VisibilityGroupId {
    pub fn new(id: impl Into<String>) -> Self {
        Self(id.into())
    }
}

impl From<&str> for VisibilityGroupId {
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl From<String> for VisibilityGroupId {
    fn from(s: String) -> Self {
        Self(s)
    }
}

impl AsRef<str> for VisibilityGroupId {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

/// A visibility group containing tracks that can be shown/hidden together
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VisibilityGroup {
    /// Unique identifier for this group
    pub id: VisibilityGroupId,

    /// Display name for the group
    pub name: String,

    /// Track indices that belong to this group (indices into the track list)
    pub track_indices: HashSet<usize>,

    /// Optional icon identifier
    pub icon: Option<String>,

    /// Color for UI display (RGB)
    pub color: Option<u32>,

    /// Whether this group is currently active (tracks are visible)
    pub active: bool,

    /// Parent group ID (for hierarchical grouping)
    pub parent_id: Option<VisibilityGroupId>,

    /// Whether this is a "global" group (affects all projects)
    pub is_global: bool,

    /// Custom user-defined group vs auto-generated from classification
    pub is_custom: bool,
}

impl VisibilityGroup {
    /// Create a new visibility group
    pub fn new(id: impl Into<VisibilityGroupId>, name: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            name: name.into(),
            track_indices: HashSet::new(),
            icon: None,
            color: None,
            active: true,
            parent_id: None,
            is_global: false,
            is_custom: false,
        }
    }

    /// Create a group for an instrument category (auto-generated)
    pub fn for_category(category: impl Into<String>) -> Self {
        let category = category.into();
        let id = VisibilityGroupId::new(category.to_lowercase().replace(' ', "_"));
        Self {
            id,
            name: category,
            track_indices: HashSet::new(),
            icon: None,
            color: None,
            active: true,
            parent_id: None,
            is_global: false,
            is_custom: false,
        }
    }

    /// Add a track index to this group
    pub fn add_track(&mut self, index: usize) {
        self.track_indices.insert(index);
    }

    /// Remove a track index from this group
    pub fn remove_track(&mut self, index: usize) {
        self.track_indices.remove(&index);
    }

    /// Check if this group contains a track
    pub fn contains_track(&self, index: usize) -> bool {
        self.track_indices.contains(&index)
    }

    /// Get the number of tracks in this group
    pub fn track_count(&self) -> usize {
        self.track_indices.len()
    }

    /// Check if this group is empty
    pub fn is_empty(&self) -> bool {
        self.track_indices.is_empty()
    }

    /// Set the group's active state
    pub fn set_active(&mut self, active: bool) {
        self.active = active;
    }

    /// Toggle the group's active state
    pub fn toggle(&mut self) -> bool {
        self.active = !self.active;
        self.active
    }

    /// Set the color
    pub fn with_color(mut self, color: u32) -> Self {
        self.color = Some(color);
        self
    }

    /// Set the icon
    pub fn with_icon(mut self, icon: impl Into<String>) -> Self {
        self.icon = Some(icon.into());
        self
    }

    /// Set as a child of another group
    pub fn with_parent(mut self, parent_id: impl Into<VisibilityGroupId>) -> Self {
        self.parent_id = Some(parent_id.into());
        self
    }

    /// Mark as a custom user-defined group
    pub fn as_custom(mut self) -> Self {
        self.is_custom = true;
        self
    }
}

/// Get the default color for a category
///
/// Delegates to `dynamic_template::colors::color_for_group` for consistent
/// color definitions across the codebase.
pub fn color_for_category(category: &str) -> Option<u32> {
    dynamic_template::colors::color_for_group(category)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_group() {
        let group = VisibilityGroup::new("drums", "Drums");
        assert_eq!(group.id.0, "drums");
        assert_eq!(group.name, "Drums");
        assert!(group.active);
        assert!(group.is_empty());
    }

    #[test]
    fn test_group_tracks() {
        let mut group = VisibilityGroup::new("drums", "Drums");
        group.add_track(0);
        group.add_track(1);
        group.add_track(2);

        assert_eq!(group.track_count(), 3);
        assert!(group.contains_track(1));
        assert!(!group.contains_track(5));

        group.remove_track(1);
        assert_eq!(group.track_count(), 2);
        assert!(!group.contains_track(1));
    }

    #[test]
    fn test_toggle() {
        let mut group = VisibilityGroup::new("drums", "Drums");
        assert!(group.active);

        let new_state = group.toggle();
        assert!(!new_state);
        assert!(!group.active);

        let new_state = group.toggle();
        assert!(new_state);
        assert!(group.active);
    }

    #[test]
    fn test_for_category() {
        let group = VisibilityGroup::for_category("Electric Guitars");
        assert_eq!(group.id.0, "electric_guitars");
        assert_eq!(group.name, "Electric Guitars");
    }
}
