//! Mock engine implementations — in-memory `ModuleSlot` and `RigEngine`
//! for testing without a DAW backend.
//!
//! `MockModuleSlot` simulates plugin loading with configurable delays.
//! `MockRigEngine` wires up mock slots and implements the full resolution,
//! diff, and switching pipeline using the new hierarchical scene model.

use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::Mutex;
use std::time::Duration;

use async_trait::async_trait;

use crate::engine::{
    EngineError, InstanceHandle, InstanceState, ModuleTarget, PresetLoadHandle, PresetReadiness,
};
use crate::module::ModuleType;
use crate::override_tree::{SceneOverride, Validated};
use crate::preset::ModulePreset;
use crate::rig::Rig;
use crate::scene::ScopedSceneRef;
use crate::snapshot::ModuleSnapshot;
use crate::stores::SceneStore;

use super::diff::{self, SlotState};
use super::resolver::{self, ResolvedModules};
use super::rig_engine::{RigEngine, TransitionResult};
use super::slot::{ActivateResult, LoadResult, ModuleSlot};

// ─── MockInstance ────────────────────────────────────────────────

/// A simulated plugin instance.
#[derive(Debug, Clone)]
struct MockInstance {
    handle: InstanceHandle,
    target: ModuleTarget,
    state: InstanceState,
}

// ─── MockModuleSlot ──────────────────────────────────────────────

/// In-memory module slot with configurable simulated load delay.
pub struct MockModuleSlot {
    module_type: ModuleType,
    instances: Mutex<Vec<MockInstance>>,
    handle_counter: AtomicU64,
    disabled: AtomicBool,
    /// Simulated load duration (default: 0 = instant).
    pub load_delay: Duration,
}

impl MockModuleSlot {
    /// Create a new mock slot for the given module type.
    pub fn new(module_type: ModuleType) -> Self {
        Self {
            module_type,
            instances: Mutex::new(Vec::new()),
            handle_counter: AtomicU64::new(0),
            disabled: AtomicBool::new(false),
            load_delay: Duration::ZERO,
        }
    }

    /// Create a mock slot with a simulated load delay.
    pub fn with_delay(module_type: ModuleType, delay: Duration) -> Self {
        Self {
            load_delay: delay,
            ..Self::new(module_type)
        }
    }

    fn next_handle(&self) -> InstanceHandle {
        let raw = self.handle_counter.fetch_add(1, Ordering::Relaxed);
        InstanceHandle::from_raw(raw)
    }
}

#[async_trait]
impl ModuleSlot for MockModuleSlot {
    fn module_type(&self) -> ModuleType {
        self.module_type
    }

    async fn load(&self, target: &ModuleTarget, _preset: &ModulePreset) -> LoadResult {
        // Check if already loaded (lock scope)
        {
            let instances = self.instances.lock().unwrap();
            for inst in instances.iter() {
                if inst.target == *target
                    && matches!(inst.state, InstanceState::Ready | InstanceState::Active)
                {
                    return LoadResult::AlreadyLoaded(inst.handle);
                }
            }
        }

        // Simulate load delay (no lock held)
        if !self.load_delay.is_zero() {
            tokio::time::sleep(self.load_delay).await;
        }

        let handle = self.next_handle();
        let instance = MockInstance {
            handle,
            target: target.clone(),
            state: InstanceState::Ready,
        };

        self.instances.lock().unwrap().push(instance);
        LoadResult::Loaded(handle)
    }

    async fn activate(&self, handle: InstanceHandle) -> ActivateResult {
        let mut instances = self.instances.lock().unwrap();

        // Find the instance to activate
        let target_idx = instances.iter().position(|i| i.handle == handle);
        let Some(idx) = target_idx else {
            return ActivateResult::Failed(EngineError::InvalidState {
                expected: InstanceState::Ready,
                actual: InstanceState::Unloaded,
            });
        };

        if instances[idx].state != InstanceState::Ready {
            return ActivateResult::Failed(EngineError::InvalidState {
                expected: InstanceState::Ready,
                actual: instances[idx].state,
            });
        }

        // Set previous active to tailing
        let mut previous = None;
        for inst in instances.iter_mut() {
            if inst.state == InstanceState::Active {
                inst.state = InstanceState::Tailing;
                previous = Some(inst.handle);
            }
        }

        // Activate the new instance
        instances[idx].state = InstanceState::Active;

        ActivateResult::Activated {
            new_active: handle,
            previous,
        }
    }

    async fn apply_snapshot(
        &self,
        handle: InstanceHandle,
        _snapshot: &ModuleSnapshot,
    ) -> Result<(), EngineError> {
        let instances = self.instances.lock().unwrap();
        let inst = instances.iter().find(|i| i.handle == handle);

        match inst {
            Some(i) if i.state == InstanceState::Active => Ok(()),
            Some(i) => Err(EngineError::InvalidState {
                expected: InstanceState::Active,
                actual: i.state,
            }),
            None => Err(EngineError::InvalidState {
                expected: InstanceState::Active,
                actual: InstanceState::Unloaded,
            }),
        }
    }

    fn instance_state(&self, handle: InstanceHandle) -> Option<InstanceState> {
        self.instances
            .lock()
            .unwrap()
            .iter()
            .find(|i| i.handle == handle)
            .map(|i| i.state)
    }

    fn active_instance(&self) -> Option<InstanceHandle> {
        self.instances
            .lock()
            .unwrap()
            .iter()
            .find(|i| i.state == InstanceState::Active)
            .map(|i| i.handle)
    }

    fn loaded_instances(&self) -> Vec<(InstanceHandle, InstanceState)> {
        self.instances
            .lock()
            .unwrap()
            .iter()
            .map(|i| (i.handle, i.state))
            .collect()
    }

    async fn unload(&self, handle: InstanceHandle) -> Result<(), EngineError> {
        let mut instances = self.instances.lock().unwrap();
        if let Some(inst) = instances.iter_mut().find(|i| i.handle == handle) {
            inst.state = InstanceState::Unloaded;
            Ok(())
        } else {
            Err(EngineError::InvalidState {
                expected: InstanceState::Active,
                actual: InstanceState::Unloaded,
            })
        }
    }

    async fn cleanup_tails(&self) {
        let mut instances = self.instances.lock().unwrap();
        instances.retain(|i| i.state != InstanceState::Tailing);
    }

    async fn disable(&self) {
        self.disabled.store(true, Ordering::Relaxed);
    }

    async fn enable(&self) {
        self.disabled.store(false, Ordering::Relaxed);
    }

    fn is_disabled(&self) -> bool {
        self.disabled.load(Ordering::Relaxed)
    }
}

// ─── MockRigEngine ───────────────────────────────────────────────

/// Action to execute for a slot diff (avoids holding mutex across await).
enum DiffAction {
    LoadAndActivate {
        module_type: ModuleType,
        target: ModuleTarget,
    },
    Activate {
        module_type: ModuleType,
        handle: InstanceHandle,
    },
    ApplySnapshot {
        module_type: ModuleType,
    },
    Disable {
        module_type: ModuleType,
    },
    Enable {
        module_type: ModuleType,
    },
}

/// In-memory rig engine for testing the full resolution/diff/switch pipeline.
pub struct MockRigEngine {
    slots: Mutex<HashMap<ModuleType, MockModuleSlot>>,
    current_resolved: Mutex<ResolvedModules>,
    load_handle_counter: AtomicU64,
    /// Simulated load delay for new slots (default: 0 = instant).
    pub default_load_delay: Duration,
}

impl MockRigEngine {
    /// Create a new mock engine.
    pub fn new() -> Self {
        Self {
            slots: Mutex::new(HashMap::new()),
            current_resolved: Mutex::new(HashMap::new()),
            load_handle_counter: AtomicU64::new(0),
            default_load_delay: Duration::ZERO,
        }
    }

    fn next_load_handle(&self) -> PresetLoadHandle {
        let raw = self.load_handle_counter.fetch_add(1, Ordering::Relaxed);
        PresetLoadHandle::from_raw(raw)
    }

    /// Synchronously activate an instance on a mock slot (avoids async/await).
    fn sync_activate(
        slot: &MockModuleSlot,
        handle: InstanceHandle,
        module_type: ModuleType,
        slot_errors: &mut Vec<(ModuleType, EngineError)>,
    ) {
        let mut instances = slot.instances.lock().unwrap();

        let target_idx = instances.iter().position(|i| i.handle == handle);
        let Some(idx) = target_idx else {
            slot_errors.push((
                module_type,
                EngineError::InvalidState {
                    expected: InstanceState::Ready,
                    actual: InstanceState::Unloaded,
                },
            ));
            return;
        };

        if instances[idx].state != InstanceState::Ready {
            slot_errors.push((
                module_type,
                EngineError::InvalidState {
                    expected: InstanceState::Ready,
                    actual: instances[idx].state,
                },
            ));
            return;
        }

        // Set previous active to tailing
        for inst in instances.iter_mut() {
            if inst.state == InstanceState::Active {
                inst.state = InstanceState::Tailing;
            }
        }

        // Activate
        instances[idx].state = InstanceState::Active;
    }

    /// Get current slot states (public version for service layer).
    pub fn current_slot_states_public(&self) -> Vec<SlotState> {
        self.current_slot_states()
    }

    /// Disable a specific module slot.
    pub async fn disable_slot(&self, module_type: ModuleType) {
        let slots = self.slots.lock().unwrap();
        if let Some(slot) = slots.get(&module_type) {
            slot.disabled.store(true, Ordering::Relaxed);
        }
    }

    /// Enable a specific module slot.
    pub async fn enable_slot(&self, module_type: ModuleType) {
        let slots = self.slots.lock().unwrap();
        if let Some(slot) = slots.get(&module_type) {
            slot.disabled.store(false, Ordering::Relaxed);
        }
    }

    /// Get current slot states for diffing (acquires and drops both locks).
    fn current_slot_states(&self) -> Vec<SlotState> {
        let slots = self.slots.lock().unwrap();
        let resolved = self.current_resolved.lock().unwrap();

        slots
            .iter()
            .map(|(mt, slot)| SlotState {
                module_type: *mt,
                current: resolved.get(mt).cloned(),
                active_handle: slot.active_instance(),
                is_disabled: slot.is_disabled(),
            })
            .collect()
    }
}

impl Default for MockRigEngine {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl RigEngine for MockRigEngine {
    async fn initialize(&self, rig: &Rig) -> Result<(), EngineError> {
        let mut slots = self.slots.lock().unwrap();
        slots.clear();

        // Discover module types from the physical hierarchy:
        // rig.engines → layers → modules
        let mut module_types = Vec::new();
        for engine in rig.engines.iter() {
            for layer in engine.layers.iter() {
                for module in &layer.modules {
                    module_types.push(module.module_type);
                }
            }
        }
        module_types.sort_by_key(|mt| *mt as u8);
        module_types.dedup();

        for mt in module_types {
            slots.insert(mt, MockModuleSlot::with_delay(mt, self.default_load_delay));
        }

        Ok(())
    }

    async fn load_scene(
        &self,
        scene_ref: &ScopedSceneRef,
        rig: &Rig,
        store: &dyn SceneStore,
        overrides: &[SceneOverride<Validated>],
    ) -> TransitionResult {
        // 1. Resolve modules via hierarchical resolver
        let new_resolved = resolver::resolve_modules(scene_ref, rig, store, overrides);

        // 2. Diff against current
        let current_states = self.current_slot_states();
        let diffs = diff::compute_diff(&current_states, &new_resolved, &|_mt, _slot| None);

        // 3. Collect actions
        let actions: Vec<DiffAction> = diffs
            .iter()
            .filter_map(|d| match d {
                crate::engine::SlotDiff::LoadAndActivate {
                    module_type,
                    target,
                } => Some(DiffAction::LoadAndActivate {
                    module_type: *module_type,
                    target: target.clone(),
                }),
                crate::engine::SlotDiff::Activate {
                    module_type,
                    handle,
                } => Some(DiffAction::Activate {
                    module_type: *module_type,
                    handle: *handle,
                }),
                crate::engine::SlotDiff::ApplySnapshot { module_type, .. } => {
                    Some(DiffAction::ApplySnapshot {
                        module_type: *module_type,
                    })
                }
                crate::engine::SlotDiff::Disable { module_type } => Some(DiffAction::Disable {
                    module_type: *module_type,
                }),
                crate::engine::SlotDiff::Enable { module_type } => Some(DiffAction::Enable {
                    module_type: *module_type,
                }),
                crate::engine::SlotDiff::NoChange { .. } => None,
            })
            .collect();

        // 4. Execute actions synchronously
        let mut slot_errors = Vec::new();

        for action in &actions {
            match action {
                DiffAction::LoadAndActivate {
                    module_type,
                    target,
                } => {
                    let slots = self.slots.lock().unwrap();
                    if let Some(slot) = slots.get(module_type) {
                        // Check already loaded
                        let already = {
                            let instances = slot.instances.lock().unwrap();
                            instances
                                .iter()
                                .find(|i| {
                                    i.target == *target
                                        && matches!(
                                            i.state,
                                            InstanceState::Ready | InstanceState::Active
                                        )
                                })
                                .map(|i| i.handle)
                        };

                        let handle = if let Some(h) = already {
                            h
                        } else {
                            // Create instance synchronously
                            let h = slot.next_handle();
                            slot.instances.lock().unwrap().push(MockInstance {
                                handle: h,
                                target: target.clone(),
                                state: InstanceState::Ready,
                            });
                            h
                        };

                        Self::sync_activate(slot, handle, *module_type, &mut slot_errors);
                    }
                }
                DiffAction::Activate {
                    module_type,
                    handle,
                } => {
                    let slots = self.slots.lock().unwrap();
                    if let Some(slot) = slots.get(module_type) {
                        Self::sync_activate(slot, *handle, *module_type, &mut slot_errors);
                    }
                }
                DiffAction::ApplySnapshot { module_type } => {
                    let slots = self.slots.lock().unwrap();
                    if let Some(slot) = slots.get(module_type) {
                        let instances = slot.instances.lock().unwrap();
                        let has_active = instances.iter().any(|i| i.state == InstanceState::Active);
                        if !has_active {
                            slot_errors.push((
                                *module_type,
                                EngineError::SlotNotInitialized {
                                    module_type: *module_type,
                                },
                            ));
                        }
                    }
                }
                DiffAction::Disable { module_type } => {
                    let slots = self.slots.lock().unwrap();
                    if let Some(slot) = slots.get(module_type) {
                        slot.disabled.store(true, Ordering::Relaxed);
                    }
                }
                DiffAction::Enable { module_type } => {
                    let slots = self.slots.lock().unwrap();
                    if let Some(slot) = slots.get(module_type) {
                        slot.disabled.store(false, Ordering::Relaxed);
                    }
                }
            }
        }

        // 5. Update current resolved state
        *self.current_resolved.lock().unwrap() = new_resolved;

        if slot_errors.is_empty() {
            TransitionResult::completed()
        } else {
            let mut result = TransitionResult::completed();
            result.slot_errors = slot_errors;
            result
        }
    }

    async fn apply_snapshot(
        &self,
        module_type: ModuleType,
        _snapshot: &ModuleSnapshot,
    ) -> Result<(), EngineError> {
        let slots = self.slots.lock().unwrap();
        let slot = slots
            .get(&module_type)
            .ok_or(EngineError::SlotNotInitialized { module_type })?;

        let instances = slot.instances.lock().unwrap();
        let active = instances.iter().find(|i| i.state == InstanceState::Active);
        match active {
            Some(_) => Ok(()),
            None => Err(EngineError::SlotNotInitialized { module_type }),
        }
    }

    async fn preload(
        &self,
        _scene_ref: &ScopedSceneRef,
        _rig: &Rig,
        _store: &dyn SceneStore,
    ) -> PresetLoadHandle {
        self.next_load_handle()
    }

    fn check_readiness(&self, _handle: PresetLoadHandle) -> PresetReadiness {
        PresetReadiness::Ready
    }

    async fn wait_ready(&self, _handle: PresetLoadHandle) {
        // Mock: always ready
    }

    fn slot(&self, _module_type: ModuleType) -> Option<&dyn ModuleSlot> {
        None
    }

    async fn tick(&self) {
        let slots = self.slots.lock().unwrap();
        for slot in slots.values() {
            let mut instances = slot.instances.lock().unwrap();
            instances.retain(|i| i.state != InstanceState::Tailing);
        }
    }

    async fn shutdown(&self) {
        let slots = self.slots.lock().unwrap();
        for slot in slots.values() {
            let mut instances = slot.instances.lock().unwrap();
            for inst in instances.iter_mut() {
                if inst.state.is_alive() {
                    inst.state = InstanceState::Unloaded;
                }
            }
        }
    }
}

// ─── Tests ───────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::category::PresetCategory;
    use crate::id::*;
    use crate::module::{Module, ModuleType};
    use crate::preset::{ModulePreset, PresetMetadata};
    use crate::scene::LayerSceneBuilder;
    use crate::snapshot::ModuleSnapshot;
    use crate::stores::InMemorySceneStore;
    use crate::version::{LayerIndex, VersionedRef};
    use signal_proto::engine::Engine as ProtoEngine;
    use signal_proto::layer::Layer;
    use signal_proto::rig::InstrumentType;

    /// Helper: build a rig + store with registered presets, returning IDs.
    fn setup_rig_and_store(
        module_types: &[ModuleType],
    ) -> (
        Rig,
        InMemorySceneStore,
        Vec<(ModulePresetId, ModuleSnapshotId)>,
    ) {
        let mut store = InMemorySceneStore::new();
        let mut ids = Vec::new();

        let mut layer = Layer::new("Main", LayerIndex::new(1));
        for mt in module_types {
            layer.add_module(Module::new(mt.display_name(), *mt));

            let snapshot = ModuleSnapshot::new(format!("{} Snapshot", mt.display_name()), vec![]);
            let snap_id = snapshot.id;
            let preset = ModulePreset::new(
                PresetMetadata::new(
                    format!("{} Preset", mt.display_name()),
                    PresetCategory::default(),
                ),
                *mt,
                snapshot,
            );
            let preset_id = preset.id;
            store.register_module_preset(preset);
            ids.push((preset_id, snap_id));
        }

        let engine = ProtoEngine::new("Guitar", InstrumentType::Guitar, layer);
        let rig = Rig::new("Test Rig", InstrumentType::Guitar, engine);

        (rig, store, ids)
    }

    /// Helper: create a layer scene from snapshot IDs and register it.
    fn make_layer_scene(
        store: &mut InMemorySceneStore,
        snap_ids: &[ModuleSnapshotId],
    ) -> LayerSceneId {
        let refs: Vec<_> = snap_ids
            .iter()
            .map(|id| VersionedRef::new(*id, 1))
            .collect();
        let scene = LayerSceneBuilder::new("Test Scene")
            .modules(refs)
            .no_standalone_blocks()
            .build();
        let scene_id = scene.id;
        store.register_layer_scene(scene);
        scene_id
    }

    #[tokio::test]
    async fn mock_slot_load_and_activate() {
        let slot = MockModuleSlot::new(ModuleType::Drive);
        let preset_id = ModulePresetId::new();
        let target = ModuleTarget::new(ModuleType::Drive, preset_id);
        let module_preset = ModulePreset::new(
            PresetMetadata::new("Test", PresetCategory::default()),
            ModuleType::Drive,
            ModuleSnapshot::new("snap", vec![]),
        );

        let result = slot.load(&target, &module_preset).await;
        assert!(result.is_ok());
        let handle = result.handle().unwrap();

        assert_eq!(slot.instance_state(handle), Some(InstanceState::Ready));
        assert!(slot.active_instance().is_none());

        let act_result = slot.activate(handle).await;
        assert!(act_result.is_ok());
        assert_eq!(slot.instance_state(handle), Some(InstanceState::Active));
        assert_eq!(slot.active_instance(), Some(handle));
    }

    #[tokio::test]
    async fn mock_slot_gapless_switch() {
        let slot = MockModuleSlot::new(ModuleType::Drive);
        let module_preset = ModulePreset::new(
            PresetMetadata::new("Test", PresetCategory::default()),
            ModuleType::Drive,
            ModuleSnapshot::new("snap", vec![]),
        );

        let target1 = ModuleTarget::new(ModuleType::Drive, ModulePresetId::new());
        let result1 = slot.load(&target1, &module_preset).await;
        let handle1 = result1.handle().unwrap();
        slot.activate(handle1).await;

        let target2 = ModuleTarget::new(ModuleType::Drive, ModulePresetId::new());
        let result2 = slot.load(&target2, &module_preset).await;
        let handle2 = result2.handle().unwrap();
        let act = slot.activate(handle2).await;

        match act {
            ActivateResult::Activated {
                new_active,
                previous,
            } => {
                assert_eq!(new_active, handle2);
                assert_eq!(previous, Some(handle1));
            }
            _ => panic!("expected Activated"),
        }

        assert_eq!(slot.instance_state(handle1), Some(InstanceState::Tailing));
        assert_eq!(slot.instance_state(handle2), Some(InstanceState::Active));
    }

    #[tokio::test]
    async fn mock_slot_cleanup_tails() {
        let slot = MockModuleSlot::new(ModuleType::Drive);
        let module_preset = ModulePreset::new(
            PresetMetadata::new("Test", PresetCategory::default()),
            ModuleType::Drive,
            ModuleSnapshot::new("snap", vec![]),
        );

        let target1 = ModuleTarget::new(ModuleType::Drive, ModulePresetId::new());
        let r1 = slot.load(&target1, &module_preset).await;
        let h1 = r1.handle().unwrap();
        slot.activate(h1).await;

        let target2 = ModuleTarget::new(ModuleType::Drive, ModulePresetId::new());
        let r2 = slot.load(&target2, &module_preset).await;
        let h2 = r2.handle().unwrap();
        slot.activate(h2).await;

        assert_eq!(slot.loaded_instances().len(), 2);
        slot.cleanup_tails().await;
        assert_eq!(slot.loaded_instances().len(), 1);
        assert_eq!(slot.active_instance(), Some(h2));
    }

    #[tokio::test]
    async fn mock_slot_already_loaded() {
        let slot = MockModuleSlot::new(ModuleType::Drive);
        let preset_id = ModulePresetId::new();
        let target = ModuleTarget::new(ModuleType::Drive, preset_id);
        let module_preset = ModulePreset::new(
            PresetMetadata::new("Test", PresetCategory::default()),
            ModuleType::Drive,
            ModuleSnapshot::new("snap", vec![]),
        );

        let r1 = slot.load(&target, &module_preset).await;
        let h1 = r1.handle().unwrap();

        let r2 = slot.load(&target, &module_preset).await;
        match r2 {
            LoadResult::AlreadyLoaded(h) => assert_eq!(h, h1),
            _ => panic!("expected AlreadyLoaded"),
        }
    }

    #[tokio::test]
    async fn mock_slot_disable_enable() {
        let slot = MockModuleSlot::new(ModuleType::Drive);
        assert!(!slot.is_disabled());

        slot.disable().await;
        assert!(slot.is_disabled());

        slot.enable().await;
        assert!(!slot.is_disabled());
    }

    #[tokio::test]
    async fn mock_engine_initialize_and_load() {
        let (rig, mut store, ids) = setup_rig_and_store(&[ModuleType::Drive, ModuleType::Amp]);
        let engine = MockRigEngine::new();
        engine.initialize(&rig).await.unwrap();

        let snap_ids: Vec<_> = ids.iter().map(|(_, s)| *s).collect();
        let layer_scene_id = make_layer_scene(&mut store, &snap_ids);
        let scene_ref = ScopedSceneRef::Layer(VersionedRef::new(layer_scene_id, 1));

        let result = engine.load_scene(&scene_ref, &rig, &store, &[]).await;

        assert!(result.outcome.is_completed());
        assert!(result.slot_errors.is_empty());
    }

    #[tokio::test]
    async fn mock_engine_scene_switch_minimal_diff() {
        let (rig, mut store, ids) = setup_rig_and_store(&[ModuleType::Drive, ModuleType::Amp]);
        let engine = MockRigEngine::new();
        engine.initialize(&rig).await.unwrap();

        let snap_ids: Vec<_> = ids.iter().map(|(_, s)| *s).collect();
        let scene1_id = make_layer_scene(&mut store, &snap_ids);
        let scene_ref1 = ScopedSceneRef::Layer(VersionedRef::new(scene1_id, 1));

        engine.load_scene(&scene_ref1, &rig, &store, &[]).await;

        // Switch to same scene — should be all NoChange
        let result = engine.load_scene(&scene_ref1, &rig, &store, &[]).await;

        assert!(result.outcome.is_completed());
        assert!(result.slot_errors.is_empty());
    }

    #[tokio::test]
    async fn mock_engine_readiness() {
        let (rig, store, _) = setup_rig_and_store(&[ModuleType::Drive]);
        let engine = MockRigEngine::new();
        let scene_ref = ScopedSceneRef::Layer(VersionedRef::new(LayerSceneId::new(), 1));

        let handle = engine.preload(&scene_ref, &rig, &store).await;
        assert!(engine.check_readiness(handle).is_ready());
    }

    #[tokio::test]
    async fn mock_engine_tick_cleans_tails() {
        let (rig, mut store, ids) = setup_rig_and_store(&[ModuleType::Drive]);
        let engine = MockRigEngine::new();
        engine.initialize(&rig).await.unwrap();

        let snap_ids: Vec<_> = ids.iter().map(|(_, s)| *s).collect();
        let scene_id = make_layer_scene(&mut store, &snap_ids);
        let scene_ref = ScopedSceneRef::Layer(VersionedRef::new(scene_id, 1));

        engine.load_scene(&scene_ref, &rig, &store, &[]).await;
        engine.tick().await;
    }

    #[tokio::test]
    async fn mock_engine_shutdown() {
        let (rig, mut store, ids) = setup_rig_and_store(&[ModuleType::Drive, ModuleType::Amp]);
        let engine = MockRigEngine::new();
        engine.initialize(&rig).await.unwrap();

        let snap_ids: Vec<_> = ids.iter().map(|(_, s)| *s).collect();
        let scene_id = make_layer_scene(&mut store, &snap_ids);
        let scene_ref = ScopedSceneRef::Layer(VersionedRef::new(scene_id, 1));

        engine.load_scene(&scene_ref, &rig, &store, &[]).await;
        engine.shutdown().await;
    }
}
