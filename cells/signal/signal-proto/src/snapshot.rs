//! Snapshot types — captured parameter state at the Block and Module level.
//!
//! **`BlockSnapshot<Origin>`**: Full parameter state for a single block. Uses typestate
//! to distinguish original snapshots from forked ones:
//! - `BlockSnapshot<Original>` — created fresh, no parent link.
//! - `BlockSnapshot<Forked>` — derived from another snapshot, always has `parent_id()`.
//!
//! **`ModuleSnapshot`**: Composed of `VersionedRef<BlockSnapshotId>` references to
//! block snapshots. Tracks which block snapshots make up a module's state.
//!
//! # Typestate Proof: Cannot call parent_id() on Original snapshot
//!
//! ```compile_fail
//! use signal_proto::snapshot::BlockSnapshot;
//! use signal_proto::id::BlockId;
//! use signal_proto::block::PluginId;
//!
//! let snap = BlockSnapshot::new("Test", BlockId::new(), PluginId::vst3("x", "X"));
//! let _ = snap.parent_id(); // ERROR: no method `parent_id` on `BlockSnapshot<Original>`
//! ```

use std::marker::PhantomData;

use crate::block::PluginId;
use crate::id::{BlockId, BlockSnapshotId, ModuleSnapshotId};
use crate::parameter::ParameterValue;
use crate::tags::Tags;
use crate::version::VersionedRef;

// ─── Origin typestate markers ────────────────────────────────────

/// Snapshot was created fresh — no parent.
#[derive(..Marker)]
pub struct Original;

/// Snapshot was forked from another — always has a parent link.
#[derive(..Marker)]
pub struct Forked;

// ─── BlockSnapshot<Origin> ───────────────────────────────────────

/// Full parameter state for a single block.
///
/// The `Origin` type parameter enforces at compile time whether this snapshot
/// has a parent link:
/// - `BlockSnapshot<Original>` can be created with `new()` and forked via `fork()`.
/// - `BlockSnapshot<Forked>` always has `parent_id()` available.
///
/// Both share common accessors via the blanket `impl<O> BlockSnapshot<O>` block.
#[derive(Debug, Clone, ::facet::Facet)]
pub struct BlockSnapshot<Origin = Original> {
    id: BlockSnapshotId,
    name: String,
    block_id: BlockId,
    plugin_id: PluginId,
    parameters: Vec<ParameterValue>,
    bypassed: bool,
    parent_snapshot_id: Option<BlockSnapshotId>,
    tags: Tags,
    #[facet(skip)]
    _origin: PhantomData<Origin>,
}

impl BlockSnapshot<Original> {
    /// Create a new original block snapshot with no parameters.
    pub fn new(name: impl Into<String>, block_id: BlockId, plugin_id: PluginId) -> Self {
        Self {
            id: BlockSnapshotId::new(),
            name: name.into(),
            block_id,
            plugin_id,
            parameters: Vec::new(),
            bypassed: false,
            parent_snapshot_id: None,
            tags: Tags::new(),
            _origin: PhantomData,
        }
    }

    /// Fork this snapshot — creates a new `Forked` copy linked to this as parent.
    ///
    /// The forked snapshot gets a new ID and copies all parameter values.
    pub fn fork(&self, new_name: impl Into<String>) -> BlockSnapshot<Forked> {
        BlockSnapshot {
            id: BlockSnapshotId::new(),
            name: new_name.into(),
            block_id: self.block_id,
            plugin_id: self.plugin_id.clone(),
            parameters: self.parameters.clone(),
            bypassed: self.bypassed,
            parent_snapshot_id: Some(self.id),
            tags: self.tags.clone(),
            _origin: PhantomData,
        }
    }
}

impl BlockSnapshot<Forked> {
    /// The parent snapshot this was forked from. Always available on `Forked`.
    pub fn parent_id(&self) -> BlockSnapshotId {
        self.parent_snapshot_id
            .expect("forked snapshot always has a parent")
    }

    /// Check if the parent has been updated since this fork was created.
    ///
    /// `parent_version` is the current version of the parent; `saved_at` is the
    /// version this fork was created from. If parent is newer, the fork may be stale.
    pub fn needs_update(&self, parent_version: u32, saved_at: u32) -> bool {
        parent_version > saved_at
    }
}

/// Shared accessors available regardless of origin.
impl<O> BlockSnapshot<O> {
    pub fn id(&self) -> BlockSnapshotId {
        self.id
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn set_name(&mut self, name: impl Into<String>) {
        self.name = name.into();
    }

    pub fn block_id(&self) -> BlockId {
        self.block_id
    }

    pub fn plugin_id(&self) -> &PluginId {
        &self.plugin_id
    }

    pub fn parameters(&self) -> &[ParameterValue] {
        &self.parameters
    }

    pub fn parameters_mut(&mut self) -> &mut Vec<ParameterValue> {
        &mut self.parameters
    }

    pub fn set_parameters(&mut self, params: Vec<ParameterValue>) {
        self.parameters = params;
    }

    pub fn bypassed(&self) -> bool {
        self.bypassed
    }

    pub fn set_bypassed(&mut self, bypassed: bool) {
        self.bypassed = bypassed;
    }

    pub fn tags(&self) -> &Tags {
        &self.tags
    }

    pub fn tags_mut(&mut self) -> &mut Tags {
        &mut self.tags
    }

    /// Whether this snapshot was forked from another.
    pub fn is_forked(&self) -> bool {
        self.parent_snapshot_id.is_some()
    }

    /// Get the parent snapshot ID, if any. Returns `Some` for `Forked`, `None` for `Original`.
    pub fn parent_snapshot_id(&self) -> Option<BlockSnapshotId> {
        self.parent_snapshot_id
    }

    /// Builder: set parameters.
    #[must_use]
    pub fn with_parameters(mut self, params: Vec<ParameterValue>) -> Self {
        self.parameters = params;
        self
    }

    /// Builder: set bypassed.
    #[must_use]
    pub fn with_bypassed(mut self, bypassed: bool) -> Self {
        self.bypassed = bypassed;
        self
    }

    /// Find a parameter by index.
    pub fn parameter(&self, index: u32) -> Option<&ParameterValue> {
        self.parameters.iter().find(|p| p.index == index)
    }
}

// ─── ModuleSnapshot ──────────────────────────────────────────────

/// Composed block snapshot references for a module's state.
///
/// A module snapshot doesn't hold parameter values directly — it references
/// versioned block snapshots. This allows sharing and reusing block snapshots
/// across different module configurations.
#[derive(Debug, Clone, ::facet::Facet)]
pub struct ModuleSnapshot {
    pub id: ModuleSnapshotId,
    pub name: String,
    pub block_snapshots: Vec<VersionedRef<BlockSnapshotId>>,
    pub parent_snapshot_id: Option<ModuleSnapshotId>,
    pub tags: Tags,
}

impl ModuleSnapshot {
    /// Create a new module snapshot.
    pub fn new(
        name: impl Into<String>,
        block_snapshots: Vec<VersionedRef<BlockSnapshotId>>,
    ) -> Self {
        Self {
            id: ModuleSnapshotId::new(),
            name: name.into(),
            block_snapshots,
            parent_snapshot_id: None,
            tags: Tags::new(),
        }
    }

    /// Fork this module snapshot with a new name, linked to this as parent.
    pub fn fork(&self, new_name: impl Into<String>) -> Self {
        Self {
            id: ModuleSnapshotId::new(),
            name: new_name.into(),
            block_snapshots: self.block_snapshots.clone(),
            parent_snapshot_id: Some(self.id),
            tags: self.tags.clone(),
        }
    }

    /// Whether this is a forked snapshot.
    pub fn is_forked(&self) -> bool {
        self.parent_snapshot_id.is_some()
    }
}

// ─── Tests ───────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::PluginId;

    fn test_plugin() -> PluginId {
        PluginId::vst3("com.test.drive", "Test Drive")
    }

    #[test]
    fn original_snapshot_creation() {
        let block_id = BlockId::new();
        let snap = BlockSnapshot::new("Clean Tone", block_id, test_plugin());

        assert_eq!(snap.name(), "Clean Tone");
        assert_eq!(snap.block_id(), block_id);
        assert!(!snap.is_forked());
        assert!(snap.parent_snapshot_id().is_none());
        assert!(snap.parameters().is_empty());
        assert!(!snap.bypassed());
    }

    #[test]
    fn fork_creates_linked_copy() {
        let block_id = BlockId::new();
        let original = BlockSnapshot::new("Clean Tone", block_id, test_plugin())
            .with_parameters(vec![ParameterValue::new(0, 0.5)])
            .with_bypassed(true);
        let original_id = original.id();

        let forked = original.fork("Clean Tone (tweaked)");

        assert_eq!(forked.parent_id(), original_id);
        assert!(forked.is_forked());
        assert_eq!(forked.name(), "Clean Tone (tweaked)");
        assert_eq!(forked.block_id(), block_id);
        assert!(forked.bypassed());
        // Parameters are copied
        assert_eq!(forked.parameters().len(), 1);
        assert_eq!(forked.parameter(0).unwrap().value.get(), 0.5);
        // Different ID
        assert_ne!(forked.id(), original_id);
    }

    #[test]
    fn original_cannot_have_parent_id() {
        let snap = BlockSnapshot::new("Test", BlockId::new(), test_plugin());
        assert!(!snap.is_forked());
        assert!(snap.parent_snapshot_id().is_none());
    }

    #[test]
    fn forked_needs_update() {
        let original = BlockSnapshot::new("Test", BlockId::new(), test_plugin());
        let forked = original.fork("Tweaked");

        // Parent at version 3, forked was saved at version 2
        assert!(forked.needs_update(3, 2));
        // Parent at version 2, forked was saved at version 2
        assert!(!forked.needs_update(2, 2));
    }

    #[test]
    fn module_snapshot_creation() {
        let snap = ModuleSnapshot::new("Blues Stack", vec![]);
        assert_eq!(snap.name, "Blues Stack");
        assert!(!snap.is_forked());
        assert!(snap.block_snapshots.is_empty());
    }

    #[test]
    fn module_snapshot_with_block_refs() {
        let block_ref = VersionedRef::new(BlockSnapshotId::new(), 1);
        let snap = ModuleSnapshot::new("Full Drive", vec![block_ref]);
        assert_eq!(snap.block_snapshots.len(), 1);
    }

    #[test]
    fn module_snapshot_fork() {
        let snap = ModuleSnapshot::new("Original", vec![]);
        let snap_id = snap.id;
        let forked = snap.fork("Tweaked");

        assert!(forked.is_forked());
        assert_eq!(forked.parent_snapshot_id, Some(snap_id));
        assert_ne!(forked.id, snap_id);
    }
}
