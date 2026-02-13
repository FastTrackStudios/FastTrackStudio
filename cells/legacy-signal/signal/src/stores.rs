//! Scene and snapshot storage — trait abstraction + in-memory implementation.
//!
//! The [`SceneStore`] trait provides read access to scenes, snapshots, and
//! presets by ID. The resolver and engine use this to look up the state
//! referenced by a [`ScopedSceneRef`] and its children.
//!
//! [`InMemorySceneStore`] is the default implementation, backed by `HashMap`s.
//! It also maintains a reverse index from `ModuleSnapshotId` → `ModulePresetId`
//! so the resolver can construct [`ModuleTarget`]s efficiently.

use std::collections::HashMap;

use crate::id::*;
use crate::preset::ModulePreset;
use crate::scene::{EngineScene, LayerScene, RackScene, RigScene};
use crate::snapshot::{BlockSnapshot, ModuleSnapshot};

// ─── SceneStore trait ────────────────────────────────────────────

/// Read-only access to scenes, snapshots, and presets by ID.
///
/// Consumed by the resolver (to walk scene hierarchies) and the engine
/// (to load module presets and apply snapshots).
pub trait SceneStore: Send + Sync {
    fn layer_scene(&self, id: &LayerSceneId) -> Option<&LayerScene>;
    fn engine_scene(&self, id: &EngineSceneId) -> Option<&EngineScene>;
    fn rig_scene(&self, id: &RigSceneId) -> Option<&RigScene>;
    fn rack_scene(&self, id: &RackSceneId) -> Option<&RackScene>;
    fn module_snapshot(&self, id: &ModuleSnapshotId) -> Option<&ModuleSnapshot>;
    fn block_snapshot(&self, id: &BlockSnapshotId) -> Option<&BlockSnapshot>;
    fn module_preset(&self, id: &ModulePresetId) -> Option<&ModulePreset>;

    /// Reverse lookup: given a snapshot ID, find which module preset owns it.
    ///
    /// This bridges the gap between scene references (which use snapshot IDs)
    /// and engine targets (which need preset IDs and module types).
    fn module_preset_for_snapshot(&self, id: &ModuleSnapshotId) -> Option<&ModulePreset>;
}

// ─── InMemorySceneStore ──────────────────────────────────────────

/// HashMap-backed scene store with O(1) lookups.
///
/// Call `register_*` methods to populate the store. `register_module_preset`
/// automatically builds the reverse index from snapshot → preset.
#[derive(Debug, Clone, Default)]
pub struct InMemorySceneStore {
    layer_scenes: HashMap<LayerSceneId, LayerScene>,
    engine_scenes: HashMap<EngineSceneId, EngineScene>,
    rig_scenes: HashMap<RigSceneId, RigScene>,
    rack_scenes: HashMap<RackSceneId, RackScene>,
    module_snapshots: HashMap<ModuleSnapshotId, ModuleSnapshot>,
    block_snapshots: HashMap<BlockSnapshotId, BlockSnapshot>,
    module_presets: HashMap<ModulePresetId, ModulePreset>,
    /// Reverse index: snapshot ID → preset ID that owns it.
    snapshot_to_preset: HashMap<ModuleSnapshotId, ModulePresetId>,
}

impl InMemorySceneStore {
    pub fn new() -> Self {
        Self::default()
    }

    // ── Registration methods ─────────────────────────────────────

    pub fn register_layer_scene(&mut self, scene: LayerScene) {
        self.layer_scenes.insert(scene.id, scene);
    }

    pub fn register_engine_scene(&mut self, scene: EngineScene) {
        self.engine_scenes.insert(scene.id, scene);
    }

    pub fn register_rig_scene(&mut self, scene: RigScene) {
        self.rig_scenes.insert(scene.id, scene);
    }

    pub fn register_rack_scene(&mut self, scene: RackScene) {
        self.rack_scenes.insert(scene.id, scene);
    }

    pub fn register_module_snapshot(&mut self, snapshot: ModuleSnapshot) {
        self.module_snapshots.insert(snapshot.id, snapshot);
    }

    pub fn register_block_snapshot(&mut self, snapshot: BlockSnapshot) {
        self.block_snapshots.insert(snapshot.id(), snapshot);
    }

    // ── Read accessors (for repository bulk operations) ─────────

    pub fn layer_scenes(&self) -> &HashMap<LayerSceneId, LayerScene> {
        &self.layer_scenes
    }

    pub fn engine_scenes(&self) -> &HashMap<EngineSceneId, EngineScene> {
        &self.engine_scenes
    }

    pub fn rig_scenes(&self) -> &HashMap<RigSceneId, RigScene> {
        &self.rig_scenes
    }

    pub fn rack_scenes(&self) -> &HashMap<RackSceneId, RackScene> {
        &self.rack_scenes
    }

    pub fn module_presets(&self) -> &HashMap<ModulePresetId, ModulePreset> {
        &self.module_presets
    }

    pub fn module_snapshots_map(&self) -> &HashMap<ModuleSnapshotId, ModuleSnapshot> {
        &self.module_snapshots
    }

    pub fn block_snapshots_map(&self) -> &HashMap<BlockSnapshotId, BlockSnapshot> {
        &self.block_snapshots
    }

    /// Register a module preset and build the reverse index entry.
    ///
    /// The preset's embedded snapshot is used to create the
    /// `snapshot_id → preset_id` mapping. The snapshot itself is also
    /// registered automatically.
    pub fn register_module_preset(&mut self, preset: ModulePreset) {
        let preset_id = preset.id;
        let snapshot_id = preset.snapshot.id;

        // Build reverse index
        self.snapshot_to_preset.insert(snapshot_id, preset_id);

        // Also register the embedded snapshot
        self.module_snapshots
            .insert(snapshot_id, preset.snapshot.clone());

        self.module_presets.insert(preset_id, preset);
    }
}

impl SceneStore for InMemorySceneStore {
    fn layer_scene(&self, id: &LayerSceneId) -> Option<&LayerScene> {
        self.layer_scenes.get(id)
    }

    fn engine_scene(&self, id: &EngineSceneId) -> Option<&EngineScene> {
        self.engine_scenes.get(id)
    }

    fn rig_scene(&self, id: &RigSceneId) -> Option<&RigScene> {
        self.rig_scenes.get(id)
    }

    fn rack_scene(&self, id: &RackSceneId) -> Option<&RackScene> {
        self.rack_scenes.get(id)
    }

    fn module_snapshot(&self, id: &ModuleSnapshotId) -> Option<&ModuleSnapshot> {
        self.module_snapshots.get(id)
    }

    fn block_snapshot(&self, id: &BlockSnapshotId) -> Option<&BlockSnapshot> {
        self.block_snapshots.get(id)
    }

    fn module_preset(&self, id: &ModulePresetId) -> Option<&ModulePreset> {
        self.module_presets.get(id)
    }

    fn module_preset_for_snapshot(&self, id: &ModuleSnapshotId) -> Option<&ModulePreset> {
        let preset_id = self.snapshot_to_preset.get(id)?;
        self.module_presets.get(preset_id)
    }
}

// ─── Tests ───────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::category::PresetCategory;
    use crate::module::ModuleType;
    use crate::preset::PresetMetadata;
    use crate::scene::{EngineSceneBuilder, LayerSceneBuilder, LayerSceneEntry};
    use crate::snapshot::ModuleSnapshot;
    use crate::version::{LayerIndex, VersionedRef};

    fn make_module_preset(module_type: ModuleType) -> ModulePreset {
        let snapshot = ModuleSnapshot::new("Test Snapshot", vec![]);
        ModulePreset::new(
            PresetMetadata::new("Test Preset", PresetCategory::default()),
            module_type,
            snapshot,
        )
    }

    #[test]
    fn register_and_lookup_module_preset() {
        let mut store = InMemorySceneStore::new();
        let preset = make_module_preset(ModuleType::Drive);
        let preset_id = preset.id;

        store.register_module_preset(preset);

        let found = store.module_preset(&preset_id).unwrap();
        assert_eq!(found.id, preset_id);
        assert_eq!(found.module_type, ModuleType::Drive);
    }

    #[test]
    fn reverse_lookup_snapshot_to_preset() {
        let mut store = InMemorySceneStore::new();
        let preset = make_module_preset(ModuleType::Amp);
        let snapshot_id = preset.snapshot.id;
        let preset_id = preset.id;

        store.register_module_preset(preset);

        let found = store.module_preset_for_snapshot(&snapshot_id).unwrap();
        assert_eq!(found.id, preset_id);
        assert_eq!(found.module_type, ModuleType::Amp);
    }

    #[test]
    fn snapshot_auto_registered_with_preset() {
        let mut store = InMemorySceneStore::new();
        let preset = make_module_preset(ModuleType::Eq);
        let snapshot_id = preset.snapshot.id;

        store.register_module_preset(preset);

        // The embedded snapshot should be accessible directly
        let snap = store.module_snapshot(&snapshot_id).unwrap();
        assert_eq!(snap.id, snapshot_id);
    }

    #[test]
    fn not_found_returns_none() {
        let store = InMemorySceneStore::new();

        assert!(store.layer_scene(&LayerSceneId::new()).is_none());
        assert!(store.engine_scene(&EngineSceneId::new()).is_none());
        assert!(store.rig_scene(&RigSceneId::new()).is_none());
        assert!(store.rack_scene(&RackSceneId::new()).is_none());
        assert!(store.module_snapshot(&ModuleSnapshotId::new()).is_none());
        assert!(store.block_snapshot(&BlockSnapshotId::new()).is_none());
        assert!(store.module_preset(&ModulePresetId::new()).is_none());
        assert!(store
            .module_preset_for_snapshot(&ModuleSnapshotId::new())
            .is_none());
    }

    #[test]
    fn register_and_lookup_layer_scene() {
        let mut store = InMemorySceneStore::new();
        let scene = LayerSceneBuilder::new("Clean Verse")
            .modules(vec![])
            .no_standalone_blocks()
            .build();
        let scene_id = scene.id;

        store.register_layer_scene(scene);

        let found = store.layer_scene(&scene_id).unwrap();
        assert_eq!(found.name, "Clean Verse");
    }

    #[test]
    fn register_and_lookup_engine_scene() {
        let mut store = InMemorySceneStore::new();
        let layer_scene = LayerSceneBuilder::new("L1")
            .modules(vec![])
            .no_standalone_blocks()
            .build();
        let layer_scene_id = layer_scene.id;
        store.register_layer_scene(layer_scene);

        let engine_scene = EngineSceneBuilder::new("Main Guitar")
            .layers(vec![LayerSceneEntry {
                layer_index: LayerIndex::new(1),
                scene_ref: VersionedRef::new(layer_scene_id, 1),
            }])
            .build();
        let engine_scene_id = engine_scene.id;
        store.register_engine_scene(engine_scene);

        let found = store.engine_scene(&engine_scene_id).unwrap();
        assert_eq!(found.name, "Main Guitar");
        assert_eq!(found.layer_scenes.len(), 1);
    }

    #[test]
    fn multiple_presets_reverse_index() {
        let mut store = InMemorySceneStore::new();

        let drive_preset = make_module_preset(ModuleType::Drive);
        let amp_preset = make_module_preset(ModuleType::Amp);
        let eq_preset = make_module_preset(ModuleType::Eq);

        let drive_snap_id = drive_preset.snapshot.id;
        let amp_snap_id = amp_preset.snapshot.id;
        let eq_snap_id = eq_preset.snapshot.id;

        store.register_module_preset(drive_preset);
        store.register_module_preset(amp_preset);
        store.register_module_preset(eq_preset);

        assert_eq!(
            store
                .module_preset_for_snapshot(&drive_snap_id)
                .unwrap()
                .module_type,
            ModuleType::Drive
        );
        assert_eq!(
            store
                .module_preset_for_snapshot(&amp_snap_id)
                .unwrap()
                .module_type,
            ModuleType::Amp
        );
        assert_eq!(
            store
                .module_preset_for_snapshot(&eq_snap_id)
                .unwrap()
                .module_type,
            ModuleType::Eq
        );
    }
}
