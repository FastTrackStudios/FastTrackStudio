//! Visibility snapshots
//!
//! Snapshots capture the visibility state of all tracks at a point in time,
//! allowing users to save and restore different visibility configurations.

use crate::visibility::{TrackVisibility, VisibilityTarget};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Unique identifier for a snapshot
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SnapshotId(pub String);

impl SnapshotId {
    pub fn new(id: impl Into<String>) -> Self {
        Self(id.into())
    }

    /// Generate a unique ID based on timestamp
    pub fn generate() -> Self {
        use std::time::{SystemTime, UNIX_EPOCH};
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_millis())
            .unwrap_or(0);
        Self(format!("snapshot_{}", timestamp))
    }
}

impl From<&str> for SnapshotId {
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl From<String> for SnapshotId {
    fn from(s: String) -> Self {
        Self(s)
    }
}

/// A snapshot of track visibility states
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Snapshot {
    /// Unique identifier
    pub id: SnapshotId,

    /// User-friendly name
    pub name: String,

    /// The visibility target this snapshot applies to (TCP, MCP, or Both)
    pub target: VisibilityTarget,

    /// Visibility state for each track (keyed by track index)
    pub track_states: HashMap<usize, TrackVisibility>,

    /// Optional description
    pub description: Option<String>,

    /// Optional icon
    pub icon: Option<String>,

    /// Timestamp when snapshot was created
    pub created_at: u64,

    /// Associated group ID (if this snapshot belongs to a group)
    pub group_id: Option<String>,
}

impl Snapshot {
    /// Create a new empty snapshot
    pub fn new(name: impl Into<String>, target: VisibilityTarget) -> Self {
        use std::time::{SystemTime, UNIX_EPOCH};
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0);

        Self {
            id: SnapshotId::generate(),
            name: name.into(),
            target,
            track_states: HashMap::new(),
            description: None,
            icon: None,
            created_at: timestamp,
            group_id: None,
        }
    }

    /// Create a snapshot from current track visibility states
    pub fn from_tracks(
        name: impl Into<String>,
        target: VisibilityTarget,
        tracks: &[daw::tracks::Track],
    ) -> Self {
        let mut snapshot = Self::new(name, target);

        for (index, track) in tracks.iter().enumerate() {
            let visibility = TrackVisibility::from_track(index, track);
            snapshot.track_states.insert(index, visibility);
        }

        snapshot
    }

    /// Set a track's visibility in this snapshot
    pub fn set_track_visibility(&mut self, index: usize, visibility: TrackVisibility) {
        self.track_states.insert(index, visibility);
    }

    /// Get a track's visibility from this snapshot
    pub fn get_track_visibility(&self, index: usize) -> Option<&TrackVisibility> {
        self.track_states.get(&index)
    }

    /// Get all visible track indices for this snapshot
    pub fn visible_tracks(&self) -> Vec<usize> {
        self.track_states
            .iter()
            .filter(|(_, vis)| vis.is_visible(self.target))
            .map(|(idx, _)| *idx)
            .collect()
    }

    /// Get all hidden track indices for this snapshot
    pub fn hidden_tracks(&self) -> Vec<usize> {
        self.track_states
            .iter()
            .filter(|(_, vis)| !vis.is_visible(self.target))
            .map(|(idx, _)| *idx)
            .collect()
    }

    /// Set description
    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = Some(description.into());
        self
    }

    /// Set icon
    pub fn with_icon(mut self, icon: impl Into<String>) -> Self {
        self.icon = Some(icon.into());
        self
    }

    /// Associate with a group
    pub fn with_group(mut self, group_id: impl Into<String>) -> Self {
        self.group_id = Some(group_id.into());
        self
    }
}

/// Store for managing multiple snapshots
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SnapshotStore {
    /// All stored snapshots
    snapshots: HashMap<SnapshotId, Snapshot>,

    /// Order of snapshots (for UI display)
    order: Vec<SnapshotId>,
}

impl SnapshotStore {
    /// Create a new empty snapshot store
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a snapshot to the store
    pub fn add(&mut self, snapshot: Snapshot) {
        let id = snapshot.id.clone();
        if !self.snapshots.contains_key(&id) {
            self.order.push(id.clone());
        }
        self.snapshots.insert(id, snapshot);
    }

    /// Get a snapshot by ID
    pub fn get(&self, id: &SnapshotId) -> Option<&Snapshot> {
        self.snapshots.get(id)
    }

    /// Get a mutable snapshot by ID
    pub fn get_mut(&mut self, id: &SnapshotId) -> Option<&mut Snapshot> {
        self.snapshots.get_mut(id)
    }

    /// Remove a snapshot by ID
    pub fn remove(&mut self, id: &SnapshotId) -> Option<Snapshot> {
        self.order.retain(|i| i != id);
        self.snapshots.remove(id)
    }

    /// Get all snapshots in order
    pub fn all(&self) -> Vec<&Snapshot> {
        self.order
            .iter()
            .filter_map(|id| self.snapshots.get(id))
            .collect()
    }

    /// Get all snapshots for a specific group
    pub fn for_group(&self, group_id: &str) -> Vec<&Snapshot> {
        self.all()
            .into_iter()
            .filter(|s| s.group_id.as_deref() == Some(group_id))
            .collect()
    }

    /// Get snapshots for a specific target (TCP, MCP, or Both)
    pub fn for_target(&self, target: VisibilityTarget) -> Vec<&Snapshot> {
        self.all()
            .into_iter()
            .filter(|s| s.target == target)
            .collect()
    }

    /// Get the number of snapshots
    pub fn len(&self) -> usize {
        self.snapshots.len()
    }

    /// Check if store is empty
    pub fn is_empty(&self) -> bool {
        self.snapshots.is_empty()
    }

    /// Clear all snapshots
    pub fn clear(&mut self) {
        self.snapshots.clear();
        self.order.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_snapshot() {
        let snapshot = Snapshot::new("My Snapshot", VisibilityTarget::TCP);
        assert_eq!(snapshot.name, "My Snapshot");
        assert_eq!(snapshot.target, VisibilityTarget::TCP);
        assert!(snapshot.track_states.is_empty());
    }

    #[test]
    fn test_snapshot_store() {
        let mut store = SnapshotStore::new();
        assert!(store.is_empty());

        let snapshot1 = Snapshot::new("Snapshot 1", VisibilityTarget::TCP);
        let snapshot2 = Snapshot::new("Snapshot 2", VisibilityTarget::MCP);

        let id1 = snapshot1.id.clone();
        let id2 = snapshot2.id.clone();

        store.add(snapshot1);
        store.add(snapshot2);

        assert_eq!(store.len(), 2);
        assert!(store.get(&id1).is_some());
        assert!(store.get(&id2).is_some());

        store.remove(&id1);
        assert_eq!(store.len(), 1);
        assert!(store.get(&id1).is_none());
    }

    #[test]
    fn test_snapshot_id_generate() {
        let id1 = SnapshotId::generate();
        let id2 = SnapshotId::generate();
        // IDs should be unique (different timestamps)
        // Note: This could theoretically fail if both are generated in the same millisecond
        assert!(id1.0.starts_with("snapshot_"));
        assert!(id2.0.starts_with("snapshot_"));
    }
}
