//! Slot diff computation — compares current vs. new resolved modules
//! and produces the minimal set of changes for each module slot.
//!
//! This is a pure function: no async, no side effects. The engine
//! orchestrator uses the diff to decide which slots need loading,
//! activation, snapshot changes, or nothing at all.

use data::engine::{InstanceHandle, SlotDiff};
use data::module::ModuleType;

use super::resolver::{ResolvedModules, ResolvedSlot};

// ─────────────────────────────────────────────────────────────────────────────
// SlotState — current state of a module slot for diffing
// ─────────────────────────────────────────────────────────────────────────────

/// Snapshot of a module slot's current state, used as input to `compute_diff`.
#[derive(Debug, Clone)]
pub struct SlotState {
    /// The module type.
    pub module_type: ModuleType,
    /// The currently active resolved slot (if any).
    pub current: Option<ResolvedSlot>,
    /// The handle of the currently active instance (if any).
    pub active_handle: Option<InstanceHandle>,
    /// Whether the slot is currently disabled.
    pub is_disabled: bool,
}

// ─────────────────────────────────────────────────────────────────────────────
// PreloadedInstances — instances already loaded and ready
// ─────────────────────────────────────────────────────────────────────────────

/// Lookup function for preloaded instances.
///
/// Given a module type and resolved slot, returns the instance handle if
/// an exact match is already preloaded and in `Ready` state.
pub type PreloadLookup = dyn Fn(ModuleType, &ResolvedSlot) -> Option<InstanceHandle>;

// ─────────────────────────────────────────────────────────────────────────────
// compute_diff
// ─────────────────────────────────────────────────────────────────────────────

/// Compute the diff between current slot states and new resolved targets.
///
/// For each module type present in either `current_states` or `new_targets`,
/// produces a [`SlotDiff`] describing what the engine should do.
///
/// The `preload_lookup` function is called to check if a target is already
/// preloaded. If so, the diff produces `Activate` instead of `LoadAndActivate`.
///
/// # Arguments
///
/// * `current_states` — current state of each module slot
/// * `new_targets` — newly resolved module targets
/// * `preload_lookup` — checks if a target is already preloaded
pub fn compute_diff(
    current_states: &[SlotState],
    new_targets: &ResolvedModules,
    preload_lookup: &PreloadLookup,
) -> Vec<SlotDiff> {
    let mut diffs = Vec::new();

    // Build a lookup of current states by module type
    let current_by_type: std::collections::HashMap<ModuleType, &SlotState> = current_states
        .iter()
        .map(|s| (s.module_type, s))
        .collect();

    // Process all module types present in the new targets
    for (module_type, new_slot) in new_targets {
        let current = current_by_type.get(module_type);

        let diff = match new_slot {
            ResolvedSlot::Disabled => {
                // New state wants this slot disabled.
                if current.is_some_and(|s| s.is_disabled) {
                    // Already disabled — no change.
                    SlotDiff::NoChange {
                        module_type: *module_type,
                    }
                } else {
                    SlotDiff::Disable {
                        module_type: *module_type,
                    }
                }
            }
            ResolvedSlot::Active(new_target) => {
                match current {
                    Some(state) if state.is_disabled => {
                        // Slot was disabled, needs to be re-enabled and loaded.
                        // Check preload first.
                        if let Some(handle) = preload_lookup(*module_type, new_slot) {
                            SlotDiff::Activate {
                                module_type: *module_type,
                                handle,
                            }
                        } else {
                            SlotDiff::LoadAndActivate {
                                module_type: *module_type,
                                target: new_target.clone(),
                            }
                        }
                    }
                    Some(state) => {
                        match &state.current {
                            Some(ResolvedSlot::Active(current_target)) => {
                                if current_target == new_target {
                                    // Exact same preset + snapshot — no change.
                                    SlotDiff::NoChange {
                                        module_type: *module_type,
                                    }
                                } else if current_target.same_preset(new_target) {
                                    // Same preset, different snapshot — apply snapshot.
                                    if let Some(snap_id) = new_target.module_snapshot_id {
                                        SlotDiff::ApplySnapshot {
                                            module_type: *module_type,
                                            module_preset_id: new_target.module_preset_id,
                                            snapshot_id: snap_id,
                                        }
                                    } else {
                                        // Switching to "no snapshot" on same preset.
                                        // Treat as no-change (base parameters).
                                        SlotDiff::NoChange {
                                            module_type: *module_type,
                                        }
                                    }
                                } else {
                                    // Different preset — check preload, then load.
                                    if let Some(handle) =
                                        preload_lookup(*module_type, new_slot)
                                    {
                                        SlotDiff::Activate {
                                            module_type: *module_type,
                                            handle,
                                        }
                                    } else {
                                        SlotDiff::LoadAndActivate {
                                            module_type: *module_type,
                                            target: new_target.clone(),
                                        }
                                    }
                                }
                            }
                            Some(ResolvedSlot::Disabled) | None => {
                                // Was disabled or never assigned. Load fresh.
                                if let Some(handle) =
                                    preload_lookup(*module_type, new_slot)
                                {
                                    SlotDiff::Activate {
                                        module_type: *module_type,
                                        handle,
                                    }
                                } else {
                                    SlotDiff::LoadAndActivate {
                                        module_type: *module_type,
                                        target: new_target.clone(),
                                    }
                                }
                            }
                        }
                    }
                    None => {
                        // Module type not present in current state. Load fresh.
                        if let Some(handle) = preload_lookup(*module_type, new_slot) {
                            SlotDiff::Activate {
                                module_type: *module_type,
                                handle,
                            }
                        } else {
                            SlotDiff::LoadAndActivate {
                                module_type: *module_type,
                                target: new_target.clone(),
                            }
                        }
                    }
                }
            }
        };

        diffs.push(diff);
    }

    // Check for module types that exist in current but not in new targets.
    // These should be disabled (they were removed from the resolution).
    for state in current_states {
        if !new_targets.contains_key(&state.module_type) && !state.is_disabled {
            diffs.push(SlotDiff::Disable {
                module_type: state.module_type,
            });
        }
    }

    diffs
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use data::engine::{InstanceHandle, ModuleTarget};
    use data::id::{ModulePresetId, ModuleSnapshotId};

    fn no_preloads(_mt: ModuleType, _slot: &ResolvedSlot) -> Option<InstanceHandle> {
        None
    }

    fn make_target(module_type: ModuleType) -> (ModulePresetId, ModuleTarget) {
        let id = ModulePresetId::new();
        (id, ModuleTarget::new(module_type, id))
    }

    #[test]
    fn no_change_when_same_target() {
        let (drive_id, drive_target) = make_target(ModuleType::Drive);

        let current = vec![SlotState {
            module_type: ModuleType::Drive,
            current: Some(ResolvedSlot::Active(drive_target.clone())),
            active_handle: Some(InstanceHandle::from_raw(1)),
            is_disabled: false,
        }];
        let mut new_targets = ResolvedModules::new();
        new_targets.insert(ModuleType::Drive, ResolvedSlot::Active(drive_target));

        let diffs = compute_diff(&current, &new_targets, &no_preloads);

        assert_eq!(diffs.len(), 1);
        assert!(diffs[0].is_noop());
    }

    #[test]
    fn load_and_activate_for_new_preset() {
        let (_old_id, old_target) = make_target(ModuleType::Drive);
        let (_new_id, new_target) = make_target(ModuleType::Drive);

        let current = vec![SlotState {
            module_type: ModuleType::Drive,
            current: Some(ResolvedSlot::Active(old_target)),
            active_handle: Some(InstanceHandle::from_raw(1)),
            is_disabled: false,
        }];
        let mut new_targets = ResolvedModules::new();
        new_targets.insert(
            ModuleType::Drive,
            ResolvedSlot::Active(new_target.clone()),
        );

        let diffs = compute_diff(&current, &new_targets, &no_preloads);

        assert_eq!(diffs.len(), 1);
        assert!(diffs[0].requires_load());
        match &diffs[0] {
            SlotDiff::LoadAndActivate { target, .. } => {
                assert_eq!(*target, new_target);
            }
            _ => panic!("expected LoadAndActivate"),
        }
    }

    #[test]
    fn activate_preloaded_instance() {
        let (_old_id, old_target) = make_target(ModuleType::Drive);
        let (_new_id, new_target) = make_target(ModuleType::Drive);
        let preloaded_handle = InstanceHandle::from_raw(42);

        let current = vec![SlotState {
            module_type: ModuleType::Drive,
            current: Some(ResolvedSlot::Active(old_target)),
            active_handle: Some(InstanceHandle::from_raw(1)),
            is_disabled: false,
        }];
        let mut new_targets = ResolvedModules::new();
        new_targets.insert(
            ModuleType::Drive,
            ResolvedSlot::Active(new_target),
        );

        let diffs = compute_diff(&current, &new_targets, &move |_mt, _slot| {
            Some(preloaded_handle)
        });

        assert_eq!(diffs.len(), 1);
        match &diffs[0] {
            SlotDiff::Activate { handle, .. } => {
                assert_eq!(*handle, preloaded_handle);
            }
            _ => panic!("expected Activate"),
        }
    }

    #[test]
    fn apply_snapshot_for_same_preset_different_snap() {
        let preset_id = ModulePresetId::new();
        let snap_a = ModuleSnapshotId::new();
        let snap_b = ModuleSnapshotId::new();

        let old_target = ModuleTarget::new(ModuleType::Drive, preset_id).with_snapshot(snap_a);
        let new_target = ModuleTarget::new(ModuleType::Drive, preset_id).with_snapshot(snap_b);

        let current = vec![SlotState {
            module_type: ModuleType::Drive,
            current: Some(ResolvedSlot::Active(old_target)),
            active_handle: Some(InstanceHandle::from_raw(1)),
            is_disabled: false,
        }];
        let mut new_targets = ResolvedModules::new();
        new_targets.insert(ModuleType::Drive, ResolvedSlot::Active(new_target));

        let diffs = compute_diff(&current, &new_targets, &no_preloads);

        assert_eq!(diffs.len(), 1);
        match &diffs[0] {
            SlotDiff::ApplySnapshot {
                module_preset_id,
                snapshot_id,
                ..
            } => {
                assert_eq!(*module_preset_id, preset_id);
                assert_eq!(*snapshot_id, snap_b);
            }
            _ => panic!("expected ApplySnapshot"),
        }
    }

    #[test]
    fn disable_slot() {
        let (_drive_id, drive_target) = make_target(ModuleType::Drive);

        let current = vec![SlotState {
            module_type: ModuleType::Drive,
            current: Some(ResolvedSlot::Active(drive_target)),
            active_handle: Some(InstanceHandle::from_raw(1)),
            is_disabled: false,
        }];
        let mut new_targets = ResolvedModules::new();
        new_targets.insert(ModuleType::Drive, ResolvedSlot::Disabled);

        let diffs = compute_diff(&current, &new_targets, &no_preloads);

        assert_eq!(diffs.len(), 1);
        match &diffs[0] {
            SlotDiff::Disable { module_type } => {
                assert_eq!(*module_type, ModuleType::Drive);
            }
            _ => panic!("expected Disable"),
        }
    }

    #[test]
    fn already_disabled_is_noop() {
        let current = vec![SlotState {
            module_type: ModuleType::Drive,
            current: Some(ResolvedSlot::Disabled),
            active_handle: None,
            is_disabled: true,
        }];
        let mut new_targets = ResolvedModules::new();
        new_targets.insert(ModuleType::Drive, ResolvedSlot::Disabled);

        let diffs = compute_diff(&current, &new_targets, &no_preloads);

        assert_eq!(diffs.len(), 1);
        assert!(diffs[0].is_noop());
    }

    #[test]
    fn enable_previously_disabled_slot() {
        let (_drive_id, drive_target) = make_target(ModuleType::Drive);

        let current = vec![SlotState {
            module_type: ModuleType::Drive,
            current: Some(ResolvedSlot::Disabled),
            active_handle: None,
            is_disabled: true,
        }];
        let mut new_targets = ResolvedModules::new();
        new_targets.insert(
            ModuleType::Drive,
            ResolvedSlot::Active(drive_target.clone()),
        );

        let diffs = compute_diff(&current, &new_targets, &no_preloads);

        assert_eq!(diffs.len(), 1);
        assert!(diffs[0].requires_load());
    }

    #[test]
    fn new_module_type_loads_fresh() {
        let (_id, target) = make_target(ModuleType::Modulation);

        let current = vec![];
        let mut new_targets = ResolvedModules::new();
        new_targets.insert(
            ModuleType::Modulation,
            ResolvedSlot::Active(target.clone()),
        );

        let diffs = compute_diff(&current, &new_targets, &no_preloads);

        assert_eq!(diffs.len(), 1);
        assert!(diffs[0].requires_load());
    }

    #[test]
    fn removed_module_type_gets_disabled() {
        let (_id, target) = make_target(ModuleType::Drive);

        let current = vec![SlotState {
            module_type: ModuleType::Drive,
            current: Some(ResolvedSlot::Active(target)),
            active_handle: Some(InstanceHandle::from_raw(1)),
            is_disabled: false,
        }];
        // New targets don't include Drive at all
        let new_targets = ResolvedModules::new();

        let diffs = compute_diff(&current, &new_targets, &no_preloads);

        assert_eq!(diffs.len(), 1);
        match &diffs[0] {
            SlotDiff::Disable { module_type } => {
                assert_eq!(*module_type, ModuleType::Drive);
            }
            _ => panic!("expected Disable"),
        }
    }

    #[test]
    fn multiple_slots_independent_diffs() {
        let (_drive_id, drive_target) = make_target(ModuleType::Drive);
        let (_amp_id, amp_target) = make_target(ModuleType::Amp);
        let (_new_drive_id, new_drive_target) = make_target(ModuleType::Drive);

        let current = vec![
            SlotState {
                module_type: ModuleType::Drive,
                current: Some(ResolvedSlot::Active(drive_target)),
                active_handle: Some(InstanceHandle::from_raw(1)),
                is_disabled: false,
            },
            SlotState {
                module_type: ModuleType::Amp,
                current: Some(ResolvedSlot::Active(amp_target.clone())),
                active_handle: Some(InstanceHandle::from_raw(2)),
                is_disabled: false,
            },
        ];
        let mut new_targets = ResolvedModules::new();
        new_targets.insert(
            ModuleType::Drive,
            ResolvedSlot::Active(new_drive_target),
        );
        new_targets.insert(ModuleType::Amp, ResolvedSlot::Active(amp_target));

        let diffs = compute_diff(&current, &new_targets, &no_preloads);

        assert_eq!(diffs.len(), 2);

        let drive_diff = diffs.iter().find(|d| d.module_type() == ModuleType::Drive).unwrap();
        let amp_diff = diffs.iter().find(|d| d.module_type() == ModuleType::Amp).unwrap();

        assert!(drive_diff.requires_load());
        assert!(amp_diff.is_noop());
    }
}
