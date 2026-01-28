//! Module resolver — pure function that translates high-level preset/scene
//! data into per-slot [`ModuleTarget`]s.
//!
//! This is the core algorithm of the engine. It takes a preset, a scene,
//! and override layers, and produces a `HashMap<ModuleType, ResolvedSlot>`
//! telling each module slot exactly what to load.
//!
//! # Resolution Order (last wins)
//!
//! 1. Preset's `module_assignments` (base)
//! 2. Song-level `module_overrides`
//! 3. Scene-level `module_overrides`
//! 4. Rig's `global_module_overrides` (locked modules — always win)

use std::collections::HashMap;

use data::engine::ModuleTarget;
use data::module::ModuleType;
use data::module_preset::{ModuleOverride, ModuleOverrideType};
use data::performance::Scene;
use data::preset::Preset;
use data::rig::Rig;

// ─────────────────────────────────────────────────────────────────────────────
// ResolvedSlot
// ─────────────────────────────────────────────────────────────────────────────

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

// ─────────────────────────────────────────────────────────────────────────────
// resolve_modules
// ─────────────────────────────────────────────────────────────────────────────

/// Resolve the full set of module targets for a given preset+scene+overrides.
///
/// # Arguments
///
/// * `preset` — The top-level preset (provides base module assignments)
/// * `scene` — The active scene (may override modules)
/// * `rig` — The rig (provides global locked overrides)
/// * `song_overrides` — Song-level module overrides (applied after preset, before scene)
///
/// # Resolution Order (last wins)
///
/// 1. Preset's `module_assignments` (base)
/// 2. `song_overrides` (song-level)
/// 3. Scene's `module_overrides` (scene-level)
/// 4. Rig's `global_module_overrides` where `locked == true` (always win)
pub fn resolve_modules(
    preset: &Preset,
    scene: &Scene,
    rig: &Rig,
    song_overrides: &[ModuleOverride],
) -> ResolvedModules {
    let mut resolved = HashMap::new();

    // 1. Base: preset module assignments
    for assignment in &preset.module_assignments {
        if !assignment.enabled {
            resolved.insert(assignment.module_type, ResolvedSlot::Disabled);
            continue;
        }

        let mut target = ModuleTarget::new(
            assignment.module_type,
            assignment.module_preset_id,
        );
        if let Some(snap_id) = assignment.module_snapshot_id {
            target = target.with_snapshot(snap_id);
        }
        resolved.insert(assignment.module_type, ResolvedSlot::Active(target));
    }

    // 2. Song-level overrides
    apply_overrides(&mut resolved, song_overrides);

    // 3. Scene-level overrides
    apply_overrides(&mut resolved, &scene.module_overrides);

    // 4. Global locked overrides (always win)
    for global in &rig.global_module_overrides {
        if !global.locked {
            continue;
        }

        let mut target = ModuleTarget::new(
            global.module_type,
            global.module_preset_id,
        );
        if let Some(snap_id) = global.module_snapshot_id {
            target = target.with_snapshot(snap_id);
        }
        resolved.insert(global.module_type, ResolvedSlot::Active(target));
    }

    resolved
}

/// Apply a slice of module overrides to the resolved map.
fn apply_overrides(resolved: &mut ResolvedModules, overrides: &[ModuleOverride]) {
    for ov in overrides {
        match &ov.override_type {
            ModuleOverrideType::SwapPreset {
                module_preset_id,
                module_snapshot_id,
            } => {
                let mut target = ModuleTarget::new(ov.module_type, *module_preset_id);
                if let Some(snap_id) = module_snapshot_id {
                    target = target.with_snapshot(*snap_id);
                }
                resolved.insert(ov.module_type, ResolvedSlot::Active(target));
            }
            ModuleOverrideType::Custom(_module) => {
                // Custom inline modules are not yet supported by the engine.
                // For now, treat them as keeping the current assignment.
                // A future version will create ad-hoc ModulePresets from Custom modules.
            }
            ModuleOverrideType::Disable => {
                resolved.insert(ov.module_type, ResolvedSlot::Disabled);
            }
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    use data::category::{BaseTone, PresetCategory};
    use data::id::{ModulePresetId, ModuleSnapshotId, PresetId};
    use data::module_preset::{GlobalModuleOverride, ModuleAssignment, ModuleOverride};
    use data::normalized::Order;
    use data::rig::InstrumentType;

    fn test_preset(assignments: Vec<ModuleAssignment>) -> Preset {
        let mut preset = Preset::new(
            "Test",
            PresetCategory::Generic {
                base_tone: BaseTone::Clean,
            },
        );
        for a in assignments {
            preset.add_module_assignment(a);
        }
        preset
    }

    fn test_scene(preset_id: PresetId, overrides: Vec<ModuleOverride>) -> Scene {
        let mut scene = Scene::new("Test Scene", preset_id);
        for ov in overrides {
            scene.add_module_override(ov);
        }
        scene
    }

    fn test_rig() -> Rig {
        Rig::new("Test Rig", InstrumentType::Guitar)
    }

    #[test]
    fn base_assignments_only() {
        let drive_id = ModulePresetId::new();
        let amp_id = ModulePresetId::new();

        let preset = test_preset(vec![
            ModuleAssignment::new(ModuleType::Drive, drive_id, Order::new(0)),
            ModuleAssignment::new(ModuleType::Amp, amp_id, Order::new(1)),
        ]);
        let scene = test_scene(preset.id, vec![]);
        let rig = test_rig();

        let resolved = resolve_modules(&preset, &scene, &rig, &[]);

        assert_eq!(resolved.len(), 2);
        assert_eq!(
            resolved[&ModuleType::Drive].target().unwrap().module_preset_id,
            drive_id
        );
        assert_eq!(
            resolved[&ModuleType::Amp].target().unwrap().module_preset_id,
            amp_id
        );
    }

    #[test]
    fn disabled_assignment() {
        let drive_id = ModulePresetId::new();

        let preset = test_preset(vec![
            ModuleAssignment::new(ModuleType::Drive, drive_id, Order::new(0)).disabled(),
        ]);
        let scene = test_scene(preset.id, vec![]);
        let rig = test_rig();

        let resolved = resolve_modules(&preset, &scene, &rig, &[]);

        assert!(resolved[&ModuleType::Drive].is_disabled());
    }

    #[test]
    fn assignment_with_snapshot() {
        let drive_id = ModulePresetId::new();
        let snap_id = ModuleSnapshotId::new();

        let preset = test_preset(vec![
            ModuleAssignment::new(ModuleType::Drive, drive_id, Order::new(0))
                .with_snapshot(snap_id),
        ]);
        let scene = test_scene(preset.id, vec![]);
        let rig = test_rig();

        let resolved = resolve_modules(&preset, &scene, &rig, &[]);

        let target = resolved[&ModuleType::Drive].target().unwrap();
        assert_eq!(target.module_preset_id, drive_id);
        assert_eq!(target.module_snapshot_id, Some(snap_id));
    }

    #[test]
    fn scene_override_replaces_preset() {
        let drive_id = ModulePresetId::new();
        let override_id = ModulePresetId::new();

        let preset = test_preset(vec![
            ModuleAssignment::new(ModuleType::Drive, drive_id, Order::new(0)),
        ]);
        let scene = test_scene(
            preset.id,
            vec![ModuleOverride::swap_preset(
                ModuleType::Drive,
                override_id,
                None,
            )],
        );
        let rig = test_rig();

        let resolved = resolve_modules(&preset, &scene, &rig, &[]);

        assert_eq!(
            resolved[&ModuleType::Drive].target().unwrap().module_preset_id,
            override_id
        );
    }

    #[test]
    fn song_override_applied_before_scene() {
        let drive_id = ModulePresetId::new();
        let song_id = ModulePresetId::new();
        let scene_id = ModulePresetId::new();

        let preset = test_preset(vec![
            ModuleAssignment::new(ModuleType::Drive, drive_id, Order::new(0)),
        ]);
        // Scene override wins over song override (applied later).
        let scene = test_scene(
            preset.id,
            vec![ModuleOverride::swap_preset(
                ModuleType::Drive,
                scene_id,
                None,
            )],
        );
        let rig = test_rig();
        let song_overrides = vec![ModuleOverride::swap_preset(
            ModuleType::Drive,
            song_id,
            None,
        )];

        let resolved = resolve_modules(&preset, &scene, &rig, &song_overrides);

        // Scene override wins
        assert_eq!(
            resolved[&ModuleType::Drive].target().unwrap().module_preset_id,
            scene_id
        );
    }

    #[test]
    fn global_locked_override_always_wins() {
        let drive_id = ModulePresetId::new();
        let scene_id = ModulePresetId::new();
        let global_id = ModulePresetId::new();

        let preset = test_preset(vec![
            ModuleAssignment::new(ModuleType::Drive, drive_id, Order::new(0)),
        ]);
        let scene = test_scene(
            preset.id,
            vec![ModuleOverride::swap_preset(
                ModuleType::Drive,
                scene_id,
                None,
            )],
        );
        let mut rig = test_rig();
        rig.add_global_override(GlobalModuleOverride::locked(
            ModuleType::Drive,
            global_id,
        ));

        let resolved = resolve_modules(&preset, &scene, &rig, &[]);

        // Global locked override wins over scene override
        assert_eq!(
            resolved[&ModuleType::Drive].target().unwrap().module_preset_id,
            global_id
        );
    }

    #[test]
    fn unlocked_global_override_does_not_win() {
        let drive_id = ModulePresetId::new();
        let scene_id = ModulePresetId::new();
        let global_id = ModulePresetId::new();

        let preset = test_preset(vec![
            ModuleAssignment::new(ModuleType::Drive, drive_id, Order::new(0)),
        ]);
        let scene = test_scene(
            preset.id,
            vec![ModuleOverride::swap_preset(
                ModuleType::Drive,
                scene_id,
                None,
            )],
        );
        let mut rig = test_rig();
        let mut global = GlobalModuleOverride::locked(ModuleType::Drive, global_id);
        global.unlock();
        rig.add_global_override(global);

        let resolved = resolve_modules(&preset, &scene, &rig, &[]);

        // Scene override wins because global is unlocked
        assert_eq!(
            resolved[&ModuleType::Drive].target().unwrap().module_preset_id,
            scene_id
        );
    }

    #[test]
    fn scene_disable_override() {
        let drive_id = ModulePresetId::new();

        let preset = test_preset(vec![
            ModuleAssignment::new(ModuleType::Drive, drive_id, Order::new(0)),
        ]);
        let scene = test_scene(
            preset.id,
            vec![ModuleOverride::disable(ModuleType::Drive)],
        );
        let rig = test_rig();

        let resolved = resolve_modules(&preset, &scene, &rig, &[]);

        assert!(resolved[&ModuleType::Drive].is_disabled());
    }

    #[test]
    fn multiple_module_types_independently_resolved() {
        let drive_id = ModulePresetId::new();
        let amp_id = ModulePresetId::new();
        let eq_id = ModulePresetId::new();
        let new_drive_id = ModulePresetId::new();

        let preset = test_preset(vec![
            ModuleAssignment::new(ModuleType::Drive, drive_id, Order::new(0)),
            ModuleAssignment::new(ModuleType::Amp, amp_id, Order::new(1)),
            ModuleAssignment::new(ModuleType::Eq, eq_id, Order::new(2)),
        ]);
        let scene = test_scene(
            preset.id,
            vec![ModuleOverride::swap_preset(
                ModuleType::Drive,
                new_drive_id,
                None,
            )],
        );
        let rig = test_rig();

        let resolved = resolve_modules(&preset, &scene, &rig, &[]);

        assert_eq!(resolved.len(), 3);
        // Drive was overridden by scene
        assert_eq!(
            resolved[&ModuleType::Drive].target().unwrap().module_preset_id,
            new_drive_id
        );
        // Amp and EQ unchanged
        assert_eq!(
            resolved[&ModuleType::Amp].target().unwrap().module_preset_id,
            amp_id
        );
        assert_eq!(
            resolved[&ModuleType::Eq].target().unwrap().module_preset_id,
            eq_id
        );
    }

    #[test]
    fn empty_preset_produces_empty_resolution() {
        let preset = test_preset(vec![]);
        let scene = test_scene(preset.id, vec![]);
        let rig = test_rig();

        let resolved = resolve_modules(&preset, &scene, &rig, &[]);

        assert!(resolved.is_empty());
    }

    #[test]
    fn scene_adds_module_not_in_preset() {
        let new_mod_id = ModulePresetId::new();

        let preset = test_preset(vec![]);
        let scene = test_scene(
            preset.id,
            vec![ModuleOverride::swap_preset(
                ModuleType::Modulation,
                new_mod_id,
                None,
            )],
        );
        let rig = test_rig();

        let resolved = resolve_modules(&preset, &scene, &rig, &[]);

        assert_eq!(resolved.len(), 1);
        assert_eq!(
            resolved[&ModuleType::Modulation]
                .target()
                .unwrap()
                .module_preset_id,
            new_mod_id
        );
    }
}
