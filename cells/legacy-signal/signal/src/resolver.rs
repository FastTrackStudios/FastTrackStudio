//! Module resolver — pure function that walks the scene hierarchy to produce
//! per-slot [`ModuleTarget`]s.
//!
//! This is the core algorithm of the engine. It takes a `ScopedSceneRef`
//! (pointing at any level: Layer, Engine, Rig, or Rack), walks down to
//! LayerScene level via the [`SceneStore`], correlates module snapshots
//! with the physical hierarchy, and produces `HashMap<ModuleType, ResolvedSlot>`.
//!
//! # Resolution Algorithm
//!
//! 1. Walk `ScopedSceneRef` down to `LayerScene` level via store lookups
//! 2. At each `LayerScene`, pair `module_snapshots` with `Layer.modules` by position
//! 3. For each snapshot ref, look up the owning `ModulePreset` via reverse index
//! 4. Construct `ModuleTarget { module_type, module_preset_id, module_snapshot_id }`
//! 5. Apply `SceneOverride<Validated>` entries (Disable, ReplaceSnapshot at module level)

use std::collections::HashMap;

use crate::engine::ModuleTarget;
use crate::module::ModuleType;
use crate::override_tree::{OverrideAction, OverridePathSegment, SceneOverride, Validated};
use crate::scene::ScopedSceneRef;
use crate::stores::SceneStore;
use signal_proto::engine::Engine as ProtoEngine;
use signal_proto::rig::Rig;

// ─── ResolvedSlot ────────────────────────────────────────────────

/// The resolved state of a single module slot.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedSlot {
    /// Load this module target.
    Active(ModuleTarget),
    /// This module is explicitly disabled by an override.
    Disabled,
}

impl ResolvedSlot {
    /// Get the target if this slot is active.
    pub fn target(&self) -> Option<&ModuleTarget> {
        match self {
            Self::Active(t) => Some(t),
            Self::Disabled => None,
        }
    }

    /// Whether this slot is disabled.
    pub fn is_disabled(&self) -> bool {
        matches!(self, Self::Disabled)
    }
}

/// The full set of resolved module slots.
pub type ResolvedModules = HashMap<ModuleType, ResolvedSlot>;

// ─── resolve_modules ─────────────────────────────────────────────

/// Resolve the full set of module targets for a scene reference.
///
/// Walks the scene hierarchy from `scene_ref` down to LayerScene level,
/// correlates module snapshots with the physical rig hierarchy, and
/// applies validated overrides.
///
/// # Arguments
///
/// * `scene_ref` — Entry point (can be at any level: Layer, Engine, Rig, Rack)
/// * `rig` — The physical rig hierarchy (engines → layers → modules)
/// * `store` — Scene/snapshot/preset store for lookups
/// * `overrides` — Validated overrides to apply after base resolution
pub fn resolve_modules(
    scene_ref: &ScopedSceneRef,
    rig: &Rig,
    store: &dyn SceneStore,
    overrides: &[SceneOverride<Validated>],
) -> ResolvedModules {
    let mut resolved = HashMap::new();

    // Walk the scene ref down to layer scenes
    let layer_resolutions = collect_layer_resolutions(scene_ref, rig, store);

    // Resolve each layer's modules
    for (layer_modules, module_snapshots) in &layer_resolutions {
        resolve_layer_modules(&mut resolved, layer_modules, module_snapshots, store);
    }

    // Apply overrides
    apply_overrides(&mut resolved, overrides, store);

    resolved
}

/// Pairs of (physical modules from layer, module snapshot IDs from scene)
/// for each layer that was resolved.
type LayerResolution = (
    Vec<(ModuleType, crate::id::ModuleId)>,
    Vec<crate::id::ModuleSnapshotId>,
);

/// Walk the scene hierarchy down to layer scenes and collect
/// (physical modules, scene snapshot refs) pairs.
fn collect_layer_resolutions(
    scene_ref: &ScopedSceneRef,
    rig: &Rig,
    store: &dyn SceneStore,
) -> Vec<LayerResolution> {
    match scene_ref {
        ScopedSceneRef::Layer(layer_ref) => {
            let layer_scene = match store.layer_scene(layer_ref.target_id()) {
                Some(s) => s,
                None => return vec![],
            };
            // Find the first layer in the first engine to get physical modules.
            // For a Layer-scoped ref, we assume it targets the first engine's first layer.
            let engine = rig.engines.first();
            let layer = engine.layers.first();
            let modules: Vec<_> = layer
                .modules
                .iter()
                .map(|m| (m.module_type, m.id))
                .collect();
            let snap_ids: Vec<_> = layer_scene
                .module_snapshots
                .iter()
                .map(|r| *r.target_id())
                .collect();
            vec![(modules, snap_ids)]
        }
        ScopedSceneRef::Engine(engine_ref) => {
            let engine_scene = match store.engine_scene(engine_ref.target_id()) {
                Some(s) => s,
                None => return vec![],
            };
            // Use the first engine from the rig
            let engine = rig.engines.first();
            resolve_engine_layers(engine, engine_scene, store)
        }
        ScopedSceneRef::Rig(rig_ref) => {
            let rig_scene = match store.rig_scene(rig_ref.target_id()) {
                Some(s) => s,
                None => return vec![],
            };
            let mut results = Vec::new();
            for entry in &rig_scene.engine_scenes {
                if let Some(engine) = rig.engine(entry.engine_id) {
                    if let Some(engine_scene) = store.engine_scene(entry.scene_ref.target_id()) {
                        results.extend(resolve_engine_layers(engine, engine_scene, store));
                    }
                }
            }
            results
        }
        ScopedSceneRef::Rack(rack_ref) => {
            let rack_scene = match store.rack_scene(rack_ref.target_id()) {
                Some(s) => s,
                None => return vec![],
            };
            let mut results = Vec::new();
            // For rack-level, we'd need a Rack reference. Since our current
            // API takes a Rig, we handle the first rig scene entry that matches.
            for rig_entry in &rack_scene.rig_scenes {
                if let Some(rig_scene) = store.rig_scene(rig_entry.scene_ref.target_id()) {
                    for engine_entry in &rig_scene.engine_scenes {
                        if let Some(engine) = rig.engine(engine_entry.engine_id) {
                            if let Some(engine_scene) =
                                store.engine_scene(engine_entry.scene_ref.target_id())
                            {
                                results.extend(resolve_engine_layers(engine, engine_scene, store));
                            }
                        }
                    }
                }
            }
            results
        }
    }
}

/// Resolve layers within a single engine scene.
fn resolve_engine_layers(
    engine: &ProtoEngine,
    engine_scene: &crate::scene::EngineScene,
    store: &dyn SceneStore,
) -> Vec<LayerResolution> {
    let mut results = Vec::new();

    for entry in &engine_scene.layer_scenes {
        let layer = match engine.layer(entry.layer_index) {
            Some(l) => l,
            None => continue,
        };
        let layer_scene = match store.layer_scene(entry.scene_ref.target_id()) {
            Some(s) => s,
            None => continue,
        };

        let modules: Vec<_> = layer
            .modules
            .iter()
            .map(|m| (m.module_type, m.id))
            .collect();
        let snap_ids: Vec<_> = layer_scene
            .module_snapshots
            .iter()
            .map(|r| *r.target_id())
            .collect();
        results.push((modules, snap_ids));
    }

    results
}

/// Resolve a single layer's modules by pairing physical modules with snapshot refs.
fn resolve_layer_modules(
    resolved: &mut ResolvedModules,
    layer_modules: &[(ModuleType, crate::id::ModuleId)],
    snapshot_ids: &[crate::id::ModuleSnapshotId],
    store: &dyn SceneStore,
) {
    // Pair by position: module[i] gets snapshot[i]
    for (i, (module_type, _module_id)) in layer_modules.iter().enumerate() {
        let snapshot_id = match snapshot_ids.get(i) {
            Some(id) => *id,
            None => continue, // No snapshot for this module position
        };

        // Look up the owning preset via reverse index
        let preset = match store.module_preset_for_snapshot(&snapshot_id) {
            Some(p) => p,
            None => continue, // Orphaned snapshot ref — skip
        };

        let target = ModuleTarget::new(*module_type, preset.id).with_snapshot(snapshot_id);
        resolved.insert(*module_type, ResolvedSlot::Active(target));
    }
}

/// Apply validated overrides to the resolved module map.
fn apply_overrides(
    resolved: &mut ResolvedModules,
    overrides: &[SceneOverride<Validated>],
    store: &dyn SceneStore,
) {
    for ov in overrides {
        // Find the module type targeted by this override
        let module_type = match find_module_type_in_path(ov.path()) {
            Some(mt) => mt,
            None => continue,
        };

        match ov.action() {
            OverrideAction::Disable => {
                resolved.insert(module_type, ResolvedSlot::Disabled);
            }
            OverrideAction::ReplaceSnapshot(snapshot_ref) => {
                if let crate::override_tree::SnapshotRef::Module(vref) = snapshot_ref {
                    if let Some(preset) = store.module_preset_for_snapshot(vref.target_id()) {
                        let target = ModuleTarget::new(module_type, preset.id)
                            .with_snapshot(*vref.target_id());
                        resolved.insert(module_type, ResolvedSlot::Active(target));
                    }
                }
            }
            OverrideAction::SetBypassed(_) | OverrideAction::ReplaceParameters(_) => {
                // These are parameter-level overrides, not slot-level.
                // The engine handles them separately after slot resolution.
            }
        }
    }
}

/// Extract the ModuleType from an override path, if present.
fn find_module_type_in_path(path: &crate::override_tree::OverridePath) -> Option<ModuleType> {
    for segment in &path.segments {
        if let OverridePathSegment::Module(mt) = segment {
            return Some(*mt);
        }
    }
    None
}

// ─── Tests ───────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::category::PresetCategory;
    use crate::id::*;
    use crate::module::{Module, ModuleType};
    use crate::override_tree::{
        OverrideAction, OverridePath, OverridePathSegment, SceneOverride, SnapshotRef, Unvalidated,
    };
    use crate::preset::{ModulePreset, PresetMetadata};
    use crate::scene::{
        EngineSceneBuilder, EngineSceneEntry, LayerSceneBuilder, LayerSceneEntry, RigSceneBuilder,
    };
    use crate::snapshot::ModuleSnapshot;
    use crate::stores::InMemorySceneStore;
    use crate::version::{LayerIndex, VersionedRef};
    use signal_proto::engine::Engine as ProtoEngine;
    use signal_proto::layer::Layer;
    use signal_proto::rig::{InstrumentType, Rig};

    /// Helper: create a module preset registered in the store, returning IDs.
    fn register_preset(
        store: &mut InMemorySceneStore,
        module_type: ModuleType,
        name: &str,
    ) -> (ModulePresetId, ModuleSnapshotId) {
        let snapshot = ModuleSnapshot::new(name, vec![]);
        let snapshot_id = snapshot.id;
        let preset = ModulePreset::new(
            PresetMetadata::new(name, PresetCategory::default()),
            module_type,
            snapshot,
        );
        let preset_id = preset.id;
        store.register_module_preset(preset);
        (preset_id, snapshot_id)
    }

    /// Helper: build a simple rig with one engine, one layer, N modules.
    fn make_rig(module_types: &[ModuleType]) -> Rig {
        let mut layer = Layer::new("Main", LayerIndex::new(1));
        for mt in module_types {
            layer.add_module(Module::new(mt.display_name(), *mt));
        }
        let engine = ProtoEngine::new("Guitar", InstrumentType::Guitar, layer);
        Rig::new("Test Rig", InstrumentType::Guitar, engine)
    }

    /// Helper: build a layer scene referencing snapshot IDs by position.
    fn make_layer_scene(
        store: &mut InMemorySceneStore,
        snap_ids: &[ModuleSnapshotId],
    ) -> LayerSceneId {
        let refs: Vec<_> = snap_ids
            .iter()
            .map(|id| VersionedRef::new(*id, 1))
            .collect();
        let scene = LayerSceneBuilder::new("Test Layer Scene")
            .modules(refs)
            .no_standalone_blocks()
            .build();
        let scene_id = scene.id;
        store.register_layer_scene(scene);
        scene_id
    }

    /// Helper: build an engine scene wrapping one layer scene.
    fn make_engine_scene(
        store: &mut InMemorySceneStore,
        layer_scene_id: LayerSceneId,
    ) -> EngineSceneId {
        let scene = EngineSceneBuilder::new("Test Engine Scene")
            .layers(vec![LayerSceneEntry {
                layer_index: LayerIndex::new(1),
                scene_ref: VersionedRef::new(layer_scene_id, 1),
            }])
            .build();
        let scene_id = scene.id;
        store.register_engine_scene(scene);
        scene_id
    }

    #[test]
    fn basic_layer_scene_resolution() {
        let mut store = InMemorySceneStore::new();
        let (drive_preset_id, drive_snap_id) =
            register_preset(&mut store, ModuleType::Drive, "Klon");
        let (amp_preset_id, amp_snap_id) = register_preset(&mut store, ModuleType::Amp, "Deluxe");

        let rig = make_rig(&[ModuleType::Drive, ModuleType::Amp]);
        let layer_scene_id = make_layer_scene(&mut store, &[drive_snap_id, amp_snap_id]);

        let scene_ref = ScopedSceneRef::Layer(VersionedRef::new(layer_scene_id, 1));
        let resolved = resolve_modules(&scene_ref, &rig, &store, &[]);

        assert_eq!(resolved.len(), 2);
        let drive = resolved[&ModuleType::Drive].target().unwrap();
        assert_eq!(drive.module_preset_id, drive_preset_id);
        assert_eq!(drive.module_snapshot_id, Some(drive_snap_id));

        let amp = resolved[&ModuleType::Amp].target().unwrap();
        assert_eq!(amp.module_preset_id, amp_preset_id);
        assert_eq!(amp.module_snapshot_id, Some(amp_snap_id));
    }

    #[test]
    fn engine_scene_resolution() {
        let mut store = InMemorySceneStore::new();
        let (_drive_pid, drive_snap_id) = register_preset(&mut store, ModuleType::Drive, "Klon");
        let (_amp_pid, amp_snap_id) = register_preset(&mut store, ModuleType::Amp, "Deluxe");

        let rig = make_rig(&[ModuleType::Drive, ModuleType::Amp]);
        let layer_scene_id = make_layer_scene(&mut store, &[drive_snap_id, amp_snap_id]);
        let engine_scene_id = make_engine_scene(&mut store, layer_scene_id);

        let scene_ref = ScopedSceneRef::Engine(VersionedRef::new(engine_scene_id, 1));
        let resolved = resolve_modules(&scene_ref, &rig, &store, &[]);

        assert_eq!(resolved.len(), 2);
        assert!(resolved[&ModuleType::Drive].target().is_some());
        assert!(resolved[&ModuleType::Amp].target().is_some());
    }

    #[test]
    fn rig_scene_resolution() {
        let mut store = InMemorySceneStore::new();
        let (_drive_pid, drive_snap_id) = register_preset(&mut store, ModuleType::Drive, "Klon");

        let rig = make_rig(&[ModuleType::Drive]);
        let engine_id = rig.engines.first().id;
        let layer_scene_id = make_layer_scene(&mut store, &[drive_snap_id]);
        let engine_scene_id = make_engine_scene(&mut store, layer_scene_id);

        let rig_scene = RigSceneBuilder::new("Full Rig")
            .engines(vec![EngineSceneEntry {
                engine_id,
                scene_ref: VersionedRef::new(engine_scene_id, 1),
            }])
            .build();
        let rig_scene_id = rig_scene.id;
        store.register_rig_scene(rig_scene);

        let scene_ref = ScopedSceneRef::Rig(VersionedRef::new(rig_scene_id, 1));
        let resolved = resolve_modules(&scene_ref, &rig, &store, &[]);

        assert_eq!(resolved.len(), 1);
        assert!(resolved[&ModuleType::Drive].target().is_some());
    }

    #[test]
    fn override_disable_module() {
        let mut store = InMemorySceneStore::new();
        let (_drive_pid, drive_snap_id) = register_preset(&mut store, ModuleType::Drive, "Klon");
        let (_amp_pid, amp_snap_id) = register_preset(&mut store, ModuleType::Amp, "Deluxe");

        let rig = make_rig(&[ModuleType::Drive, ModuleType::Amp]);
        let engine = rig.engines.first();
        let layer_scene_id = make_layer_scene(&mut store, &[drive_snap_id, amp_snap_id]);

        let scene_ref = ScopedSceneRef::Layer(VersionedRef::new(layer_scene_id, 1));

        // Create a validated override that disables Drive
        let raw_override = SceneOverride::<Unvalidated>::new(
            OverridePath::new(vec![OverridePathSegment::Module(ModuleType::Drive)]),
            OverrideAction::Disable,
        );
        let validated = raw_override.validate_for_engine(engine).unwrap();

        let resolved = resolve_modules(&scene_ref, &rig, &store, &[validated]);

        assert!(resolved[&ModuleType::Drive].is_disabled());
        assert!(resolved[&ModuleType::Amp].target().is_some());
    }

    #[test]
    fn override_replace_snapshot() {
        let mut store = InMemorySceneStore::new();
        let (_drive_pid, drive_snap_id) = register_preset(&mut store, ModuleType::Drive, "Klon");
        let (alt_drive_pid, alt_drive_snap_id) =
            register_preset(&mut store, ModuleType::Drive, "Tubescreamer");

        let rig = make_rig(&[ModuleType::Drive]);
        let engine = rig.engines.first();
        let layer_scene_id = make_layer_scene(&mut store, &[drive_snap_id]);

        let scene_ref = ScopedSceneRef::Layer(VersionedRef::new(layer_scene_id, 1));

        // Override: replace Drive snapshot with Tubescreamer
        let raw_override = SceneOverride::<Unvalidated>::new(
            OverridePath::new(vec![OverridePathSegment::Module(ModuleType::Drive)]),
            OverrideAction::ReplaceSnapshot(SnapshotRef::Module(VersionedRef::new(
                alt_drive_snap_id,
                1,
            ))),
        );
        let validated = raw_override.validate_for_engine(engine).unwrap();

        let resolved = resolve_modules(&scene_ref, &rig, &store, &[validated]);

        let drive = resolved[&ModuleType::Drive].target().unwrap();
        assert_eq!(drive.module_preset_id, alt_drive_pid);
        assert_eq!(drive.module_snapshot_id, Some(alt_drive_snap_id));
    }

    #[test]
    fn missing_scene_returns_empty() {
        let store = InMemorySceneStore::new();
        let rig = make_rig(&[ModuleType::Drive]);

        let scene_ref = ScopedSceneRef::Layer(VersionedRef::new(LayerSceneId::new(), 1));
        let resolved = resolve_modules(&scene_ref, &rig, &store, &[]);

        assert!(resolved.is_empty());
    }

    #[test]
    fn fewer_snapshots_than_modules() {
        let mut store = InMemorySceneStore::new();
        let (_drive_pid, drive_snap_id) = register_preset(&mut store, ModuleType::Drive, "Klon");

        // Rig has 3 modules but scene only has 1 snapshot
        let rig = make_rig(&[ModuleType::Drive, ModuleType::Amp, ModuleType::Eq]);
        let layer_scene_id = make_layer_scene(&mut store, &[drive_snap_id]);

        let scene_ref = ScopedSceneRef::Layer(VersionedRef::new(layer_scene_id, 1));
        let resolved = resolve_modules(&scene_ref, &rig, &store, &[]);

        // Only Drive is resolved; Amp and EQ have no matching snapshot
        assert_eq!(resolved.len(), 1);
        assert!(resolved[&ModuleType::Drive].target().is_some());
    }

    #[test]
    fn orphaned_snapshot_ref_skipped() {
        let mut store = InMemorySceneStore::new();
        // Register a snapshot that has no owning preset
        let orphan_id = ModuleSnapshotId::new();
        let orphan_snapshot = ModuleSnapshot::new("Orphan", vec![]);
        // Manually override the ID (we need a specific one)
        // Instead, just don't register a preset for this snapshot
        store.register_module_snapshot(orphan_snapshot);

        let rig = make_rig(&[ModuleType::Drive]);
        // Layer scene refs a snapshot with no preset
        let refs = vec![VersionedRef::new(orphan_id, 1)];
        let scene = LayerSceneBuilder::new("Test")
            .modules(refs)
            .no_standalone_blocks()
            .build();
        let scene_id = scene.id;
        store.register_layer_scene(scene);

        let scene_ref = ScopedSceneRef::Layer(VersionedRef::new(scene_id, 1));
        let resolved = resolve_modules(&scene_ref, &rig, &store, &[]);

        // Orphan snapshot is skipped
        assert!(resolved.is_empty());
    }

    #[test]
    fn empty_rig_produces_empty_resolution() {
        let mut store = InMemorySceneStore::new();
        // Rig with no modules in the layer
        let rig = make_rig(&[]);
        let layer_scene_id = make_layer_scene(&mut store, &[]);

        let scene_ref = ScopedSceneRef::Layer(VersionedRef::new(layer_scene_id, 1));
        let resolved = resolve_modules(&scene_ref, &rig, &store, &[]);

        assert!(resolved.is_empty());
    }

    #[test]
    fn parameter_override_does_not_affect_slot_resolution() {
        use crate::override_tree::ParameterOverride;

        let mut store = InMemorySceneStore::new();
        let (drive_pid, drive_snap_id) = register_preset(&mut store, ModuleType::Drive, "Klon");

        let rig = make_rig(&[ModuleType::Drive]);
        let engine = rig.engines.first();
        let layer_scene_id = make_layer_scene(&mut store, &[drive_snap_id]);

        let scene_ref = ScopedSceneRef::Layer(VersionedRef::new(layer_scene_id, 1));

        // Parameter override should not change the resolved slot
        let raw_override = SceneOverride::<Unvalidated>::new(
            OverridePath::new(vec![
                OverridePathSegment::Module(ModuleType::Drive),
                OverridePathSegment::Parameter(0),
            ]),
            OverrideAction::ReplaceParameters(vec![ParameterOverride::new(0, 0.5)]),
        );
        let validated = raw_override.validate_for_engine(engine).unwrap();

        let resolved = resolve_modules(&scene_ref, &rig, &store, &[validated]);

        let drive = resolved[&ModuleType::Drive].target().unwrap();
        assert_eq!(drive.module_preset_id, drive_pid);
    }
}
