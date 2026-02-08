//! Snapshot diffing — compute differences between snapshot states.
//!
//! [`SnapshotDiff`] captures what changed between two snapshots:
//! parameter-level changes (same blocks, different knob positions) and
//! structural changes (different module assignments that prevent morphing).
//!
//! The diff drives downstream consumers:
//! - **Morphing**: When `structural_changes` is false, all changes are parameter-level
//!   and can be smoothly interpolated with a slider.
//! - **Crossfade**: When `structural_changes` is true, modules differ and the
//!   transition requires an audio crossfade.
//! - **Preview UI**: Color-coded display of what will change (green/red/blue).

use facet::Facet;

use crate::id::BlockId;
use crate::module::ModuleType;
use crate::module_preset::ModuleAssignment;
use crate::normalized::NormalizedF64;

use super::{BlockOverride, Snapshot};

// region: --- ParameterChange

/// A single parameter value change between two snapshots.
///
/// Tracks the block, parameter ID, and the from/to normalized values.
/// Used by the morph engine for interpolation and by the diff preview UI
/// for color-coded display.
#[derive(Debug, Clone, Facet)]
pub struct ParameterChange {
    /// The block containing the changed parameter.
    pub block_id: BlockId,
    /// Parameter identifier within the block (matches [`ParameterOverride::param_id`]).
    pub param_id: String,
    /// Value in the source/current snapshot (normalized [0.0, 1.0]).
    pub from_value: NormalizedF64,
    /// Value in the target snapshot (normalized [0.0, 1.0]).
    pub to_value: NormalizedF64,
}

impl ParameterChange {
    /// Create a new parameter change.
    pub fn new(
        block_id: BlockId,
        param_id: impl Into<String>,
        from_value: NormalizedF64,
        to_value: NormalizedF64,
    ) -> Self {
        Self {
            block_id,
            param_id: param_id.into(),
            from_value,
            to_value,
        }
    }

    /// The absolute magnitude of the change in normalized space.
    pub fn magnitude(&self) -> f64 {
        (self.to_value.get() - self.from_value.get()).abs()
    }
}

// endregion: --- ParameterChange

// region: --- BypassChange

/// A block bypass state change between two snapshots.
#[derive(Debug, Clone, Facet)]
pub struct BypassChange {
    /// The block whose bypass state changed.
    pub block_id: BlockId,
    /// Bypass state in the source snapshot (`None` = inherits from preset).
    pub from_bypassed: Option<bool>,
    /// Bypass state in the target snapshot (`None` = inherits from preset).
    pub to_bypassed: Option<bool>,
}

impl BypassChange {
    /// Create a new bypass change.
    pub fn new(block_id: BlockId, from_bypassed: Option<bool>, to_bypassed: Option<bool>) -> Self {
        Self {
            block_id,
            from_bypassed,
            to_bypassed,
        }
    }
}

// endregion: --- BypassChange

// region: --- ModuleChange

/// A structural module assignment change between two snapshots.
///
/// Detected when comparing the module assignments of two preset configurations.
/// Structural changes prevent parameter morphing (must crossfade instead).
#[derive(Debug, Clone, PartialEq, Eq, Facet)]
#[repr(u8)]
pub enum ModuleChange {
    /// A module type is present in the target but not the current state.
    Added(ModuleType),
    /// A module type is present in the current state but not the target.
    Removed(ModuleType),
    /// A module type exists in both, but the underlying preset changed
    /// (different blocks/plugins — can't morph parameters).
    PresetChanged(ModuleType),
}

// endregion: --- ModuleChange

// region: --- SnapshotDiff

/// The complete diff between two snapshot states.
///
/// Separates parameter-level changes (morphable) from structural changes
/// (require crossfade). Consumers check `structural_changes` to decide
/// the transition strategy.
///
/// # Examples
///
/// ```text
/// // Same preset, different snapshot → morphable
/// if !diff.structural_changes {
///     morph_engine.set_changes(&diff.changed_params, slider_position);
/// }
///
/// // Different modules → must crossfade
/// if diff.structural_changes {
///     crossfade_engine.transition(&diff.module_changes, duration);
/// }
/// ```
#[derive(Debug, Clone, Facet)]
pub struct SnapshotDiff {
    /// Parameter value changes (morphable when no structural changes).
    pub changed_params: Vec<ParameterChange>,
    /// Bypass state changes.
    pub bypass_changes: Vec<BypassChange>,
    /// Structural module changes (added/removed/preset-swapped).
    pub module_changes: Vec<ModuleChange>,
    /// `true` if any module assignments differ (blocks added/removed/swapped).
    /// When true, parameter morphing is not possible — must crossfade.
    pub structural_changes: bool,
}

impl SnapshotDiff {
    /// Whether this diff contains any changes at all.
    pub fn is_empty(&self) -> bool {
        self.changed_params.is_empty()
            && self.bypass_changes.is_empty()
            && self.module_changes.is_empty()
    }

    /// Whether parameter morphing is possible (no structural changes).
    pub fn can_morph(&self) -> bool {
        !self.structural_changes
    }

    /// Total number of individual changes across all categories.
    pub fn total_changes(&self) -> usize {
        self.changed_params.len() + self.bypass_changes.len() + self.module_changes.len()
    }
}

// endregion: --- SnapshotDiff

// region: --- diff_snapshots

/// Compute the parameter-level diff between two preset [`Snapshot`]s.
///
/// Compares block overrides from `current` and `target`, detecting:
/// - Parameters that changed value
/// - Bypass states that toggled
/// - Blocks overridden in one snapshot but not the other
///
/// This function only considers parameter/bypass changes within the same
/// preset. For structural (module assignment) changes, use
/// [`diff_module_assignments`].
pub fn diff_snapshots(current: &Snapshot, target: &Snapshot) -> SnapshotDiff {
    let mut changed_params = Vec::new();
    let mut bypass_changes = Vec::new();

    // Build a lookup of current block overrides by block_id
    let current_overrides: std::collections::HashMap<BlockId, &BlockOverride> = current
        .block_overrides
        .iter()
        .map(|o| (o.block_id, o))
        .collect();

    let target_overrides: std::collections::HashMap<BlockId, &BlockOverride> = target
        .block_overrides
        .iter()
        .map(|o| (o.block_id, o))
        .collect();

    // Collect all block IDs referenced by either snapshot
    let mut all_block_ids: Vec<BlockId> = current_overrides
        .keys()
        .chain(target_overrides.keys())
        .copied()
        .collect();
    all_block_ids.sort_by_key(|id| id.as_uuid());
    all_block_ids.dedup();

    for block_id in all_block_ids {
        let current_override = current_overrides.get(&block_id).copied();
        let target_override = target_overrides.get(&block_id).copied();

        diff_block_overrides(
            block_id,
            current_override,
            target_override,
            &mut changed_params,
            &mut bypass_changes,
        );
    }

    SnapshotDiff {
        changed_params,
        bypass_changes,
        module_changes: Vec::new(),
        structural_changes: false,
    }
}

/// Diff two optional block overrides for the same block.
fn diff_block_overrides(
    block_id: BlockId,
    current: Option<&BlockOverride>,
    target: Option<&BlockOverride>,
    changed_params: &mut Vec<ParameterChange>,
    bypass_changes: &mut Vec<BypassChange>,
) {
    let current_bypass = current.and_then(|o| o.bypassed);
    let target_bypass = target.and_then(|o| o.bypassed);

    // Check bypass state change
    if current_bypass != target_bypass {
        bypass_changes.push(BypassChange::new(block_id, current_bypass, target_bypass));
    }

    // Collect all param IDs from both sides
    let current_params: std::collections::HashMap<&str, NormalizedF64> = current
        .map(|o| {
            o.parameters
                .iter()
                .map(|p| (p.param_id.as_str(), p.value))
                .collect()
        })
        .unwrap_or_default();

    let target_params: std::collections::HashMap<&str, NormalizedF64> = target
        .map(|o| {
            o.parameters
                .iter()
                .map(|p| (p.param_id.as_str(), p.value))
                .collect()
        })
        .unwrap_or_default();

    let mut all_param_ids: Vec<&str> = current_params
        .keys()
        .chain(target_params.keys())
        .copied()
        .collect();
    all_param_ids.sort();
    all_param_ids.dedup();

    for param_id in all_param_ids {
        let from = current_params.get(param_id).copied();
        let to = target_params.get(param_id).copied();

        match (from, to) {
            (Some(from_val), Some(to_val)) if from_val != to_val => {
                changed_params.push(ParameterChange::new(block_id, param_id, from_val, to_val));
            }
            (Some(from_val), None) => {
                // Parameter override removed in target (reverts to preset default).
                // We use ZERO as sentinel for "no override" since the actual default
                // depends on the preset block, which we don't have access to here.
                changed_params.push(ParameterChange::new(
                    block_id,
                    param_id,
                    from_val,
                    NormalizedF64::ZERO,
                ));
            }
            (None, Some(to_val)) => {
                // Parameter override added in target (was using preset default).
                changed_params.push(ParameterChange::new(
                    block_id,
                    param_id,
                    NormalizedF64::ZERO,
                    to_val,
                ));
            }
            _ => {
                // Same value or both absent — no change.
            }
        }
    }
}

// endregion: --- diff_snapshots

// region: --- diff_module_assignments

/// Compute structural differences between two sets of module assignments.
///
/// Detects modules that were added, removed, or had their underlying preset
/// swapped. Any of these changes set `structural_changes = true`, meaning
/// parameter morphing is not possible and an audio crossfade is required.
///
/// The returned [`SnapshotDiff`] only contains `module_changes` — combine
/// with [`diff_snapshots`] for a complete picture.
pub fn diff_module_assignments(
    current: &[ModuleAssignment],
    target: &[ModuleAssignment],
) -> SnapshotDiff {
    let current_map: std::collections::HashMap<ModuleType, &ModuleAssignment> =
        current.iter().map(|a| (a.module_type, a)).collect();

    let target_map: std::collections::HashMap<ModuleType, &ModuleAssignment> =
        target.iter().map(|a| (a.module_type, a)).collect();

    let mut module_changes = Vec::new();

    // Check for removed and changed modules
    for (module_type, current_assignment) in &current_map {
        match target_map.get(module_type) {
            None => {
                module_changes.push(ModuleChange::Removed(*module_type));
            }
            Some(target_assignment) => {
                if current_assignment.module_preset_id != target_assignment.module_preset_id {
                    module_changes.push(ModuleChange::PresetChanged(*module_type));
                }
            }
        }
    }

    // Check for added modules
    for module_type in target_map.keys() {
        if !current_map.contains_key(module_type) {
            module_changes.push(ModuleChange::Added(*module_type));
        }
    }

    let structural_changes = !module_changes.is_empty();

    SnapshotDiff {
        changed_params: Vec::new(),
        bypass_changes: Vec::new(),
        module_changes,
        structural_changes,
    }
}

// endregion: --- diff_module_assignments

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::id::{BlockId, ModulePresetId};
    use crate::module::ModuleType;
    use crate::module_preset::ModuleAssignment;
    use crate::normalized::{NormalizedF64, Order};
    use crate::preset::{BlockOverride, Snapshot};

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    // -- Setup & Fixtures

    fn block_id_a() -> BlockId {
        BlockId::from_uuid(uuid::Uuid::from_bytes([1; 16]))
    }

    fn block_id_b() -> BlockId {
        BlockId::from_uuid(uuid::Uuid::from_bytes([2; 16]))
    }

    fn make_snapshot(
        name: &str,
        overrides: Vec<BlockOverride>,
    ) -> Snapshot {
        let mut snap = Snapshot::new(name);
        for o in overrides {
            snap.add_block_override(o);
        }
        snap
    }

    // -- diff_snapshots: identical snapshots

    #[test]
    fn diff_identical_empty_snapshots() -> Result<()> {
        // -- Setup & Fixtures
        let current = Snapshot::new("Verse");
        let target = Snapshot::new("Verse Copy");

        // -- Exec
        let diff = diff_snapshots(&current, &target);

        // -- Check
        assert!(diff.is_empty());
        assert!(diff.can_morph());
        assert!(!diff.structural_changes);
        assert_eq!(diff.total_changes(), 0);
        Ok(())
    }

    #[test]
    fn diff_identical_overrides_produce_no_changes() -> Result<()> {
        // -- Setup & Fixtures
        let current = make_snapshot("A", vec![
            BlockOverride::new(block_id_a())
                .with_bypass(true)
                .with_param("drive", NormalizedF64::new(0.5)),
        ]);
        let target = make_snapshot("B", vec![
            BlockOverride::new(block_id_a())
                .with_bypass(true)
                .with_param("drive", NormalizedF64::new(0.5)),
        ]);

        // -- Exec
        let diff = diff_snapshots(&current, &target);

        // -- Check
        assert!(diff.is_empty());
        Ok(())
    }

    // -- diff_snapshots: parameter changes

    #[test]
    fn diff_detects_parameter_value_change() -> Result<()> {
        // -- Setup & Fixtures
        let current = make_snapshot("Verse", vec![
            BlockOverride::new(block_id_a())
                .with_param("drive", NormalizedF64::new(0.3)),
        ]);
        let target = make_snapshot("Chorus", vec![
            BlockOverride::new(block_id_a())
                .with_param("drive", NormalizedF64::new(0.8)),
        ]);

        // -- Exec
        let diff = diff_snapshots(&current, &target);

        // -- Check
        assert_eq!(diff.changed_params.len(), 1);
        assert_eq!(diff.changed_params[0].block_id, block_id_a());
        assert_eq!(diff.changed_params[0].param_id, "drive");
        assert!((diff.changed_params[0].from_value.get() - 0.3).abs() < f64::EPSILON);
        assert!((diff.changed_params[0].to_value.get() - 0.8).abs() < f64::EPSILON);
        assert!(diff.can_morph());
        Ok(())
    }

    #[test]
    fn diff_detects_multiple_param_changes_across_blocks() -> Result<()> {
        // -- Setup & Fixtures
        let current = make_snapshot("Clean", vec![
            BlockOverride::new(block_id_a())
                .with_param("drive", NormalizedF64::new(0.2))
                .with_param("tone", NormalizedF64::new(0.5)),
            BlockOverride::new(block_id_b())
                .with_param("level", NormalizedF64::new(0.6)),
        ]);
        let target = make_snapshot("Dirty", vec![
            BlockOverride::new(block_id_a())
                .with_param("drive", NormalizedF64::new(0.9))
                .with_param("tone", NormalizedF64::new(0.5)), // unchanged
            BlockOverride::new(block_id_b())
                .with_param("level", NormalizedF64::new(0.9)),
        ]);

        // -- Exec
        let diff = diff_snapshots(&current, &target);

        // -- Check
        assert_eq!(diff.changed_params.len(), 2); // drive + level changed; tone unchanged
        let param_ids: Vec<&str> = diff.changed_params.iter().map(|p| p.param_id.as_str()).collect();
        assert!(param_ids.contains(&"drive"));
        assert!(param_ids.contains(&"level"));
        Ok(())
    }

    #[test]
    fn diff_detects_param_added_in_target() -> Result<()> {
        // -- Setup & Fixtures
        let current = Snapshot::new("Base");
        let target = make_snapshot("Override", vec![
            BlockOverride::new(block_id_a())
                .with_param("reverb", NormalizedF64::new(0.7)),
        ]);

        // -- Exec
        let diff = diff_snapshots(&current, &target);

        // -- Check
        assert_eq!(diff.changed_params.len(), 1);
        assert_eq!(diff.changed_params[0].param_id, "reverb");
        assert_eq!(diff.changed_params[0].from_value, NormalizedF64::ZERO);
        assert!((diff.changed_params[0].to_value.get() - 0.7).abs() < f64::EPSILON);
        Ok(())
    }

    #[test]
    fn diff_detects_param_removed_in_target() -> Result<()> {
        // -- Setup & Fixtures
        let current = make_snapshot("Full", vec![
            BlockOverride::new(block_id_a())
                .with_param("delay", NormalizedF64::new(0.4)),
        ]);
        let target = Snapshot::new("Minimal");

        // -- Exec
        let diff = diff_snapshots(&current, &target);

        // -- Check
        assert_eq!(diff.changed_params.len(), 1);
        assert_eq!(diff.changed_params[0].param_id, "delay");
        assert!((diff.changed_params[0].from_value.get() - 0.4).abs() < f64::EPSILON);
        assert_eq!(diff.changed_params[0].to_value, NormalizedF64::ZERO);
        Ok(())
    }

    // -- diff_snapshots: bypass changes

    #[test]
    fn diff_detects_bypass_change() -> Result<()> {
        // -- Setup & Fixtures
        let current = make_snapshot("On", vec![
            BlockOverride::new(block_id_a()).with_bypass(false),
        ]);
        let target = make_snapshot("Off", vec![
            BlockOverride::new(block_id_a()).with_bypass(true),
        ]);

        // -- Exec
        let diff = diff_snapshots(&current, &target);

        // -- Check
        assert_eq!(diff.bypass_changes.len(), 1);
        assert_eq!(diff.bypass_changes[0].block_id, block_id_a());
        assert_eq!(diff.bypass_changes[0].from_bypassed, Some(false));
        assert_eq!(diff.bypass_changes[0].to_bypassed, Some(true));
        Ok(())
    }

    #[test]
    fn diff_detects_bypass_added_in_target() -> Result<()> {
        // -- Setup & Fixtures
        let current = Snapshot::new("No Override");
        let target = make_snapshot("Bypassed", vec![
            BlockOverride::new(block_id_a()).with_bypass(true),
        ]);

        // -- Exec
        let diff = diff_snapshots(&current, &target);

        // -- Check
        assert_eq!(diff.bypass_changes.len(), 1);
        assert_eq!(diff.bypass_changes[0].from_bypassed, None);
        assert_eq!(diff.bypass_changes[0].to_bypassed, Some(true));
        Ok(())
    }

    // -- diff_snapshots: magnitude

    #[test]
    fn parameter_change_magnitude() -> Result<()> {
        // -- Setup & Fixtures
        let change = ParameterChange::new(
            block_id_a(),
            "gain",
            NormalizedF64::new(0.2),
            NormalizedF64::new(0.8),
        );

        // -- Check
        assert!((change.magnitude() - 0.6).abs() < f64::EPSILON);
        Ok(())
    }

    // -- diff_module_assignments: no structural changes

    #[test]
    fn diff_identical_assignments_no_structural_changes() -> Result<()> {
        // -- Setup & Fixtures
        let preset_id = ModulePresetId::new();
        let current = vec![
            ModuleAssignment::new(ModuleType::Drive, preset_id, Order::new(0)),
            ModuleAssignment::new(ModuleType::Amp, preset_id, Order::new(1)),
        ];
        let target = vec![
            ModuleAssignment::new(ModuleType::Drive, preset_id, Order::new(0)),
            ModuleAssignment::new(ModuleType::Amp, preset_id, Order::new(1)),
        ];

        // -- Exec
        let diff = diff_module_assignments(&current, &target);

        // -- Check
        assert!(diff.module_changes.is_empty());
        assert!(!diff.structural_changes);
        assert!(diff.can_morph());
        Ok(())
    }

    // -- diff_module_assignments: added module

    #[test]
    fn diff_detects_added_module() -> Result<()> {
        // -- Setup & Fixtures
        let preset_id = ModulePresetId::new();
        let current = vec![
            ModuleAssignment::new(ModuleType::Drive, preset_id, Order::new(0)),
        ];
        let target = vec![
            ModuleAssignment::new(ModuleType::Drive, preset_id, Order::new(0)),
            ModuleAssignment::new(ModuleType::Amp, preset_id, Order::new(1)),
        ];

        // -- Exec
        let diff = diff_module_assignments(&current, &target);

        // -- Check
        assert!(diff.structural_changes);
        assert!(!diff.can_morph());
        assert!(diff.module_changes.contains(&ModuleChange::Added(ModuleType::Amp)));
        Ok(())
    }

    // -- diff_module_assignments: removed module

    #[test]
    fn diff_detects_removed_module() -> Result<()> {
        // -- Setup & Fixtures
        let preset_id = ModulePresetId::new();
        let current = vec![
            ModuleAssignment::new(ModuleType::Drive, preset_id, Order::new(0)),
            ModuleAssignment::new(ModuleType::Eq, preset_id, Order::new(1)),
        ];
        let target = vec![
            ModuleAssignment::new(ModuleType::Drive, preset_id, Order::new(0)),
        ];

        // -- Exec
        let diff = diff_module_assignments(&current, &target);

        // -- Check
        assert!(diff.structural_changes);
        assert!(diff.module_changes.contains(&ModuleChange::Removed(ModuleType::Eq)));
        Ok(())
    }

    // -- diff_module_assignments: preset changed

    #[test]
    fn diff_detects_module_preset_change() -> Result<()> {
        // -- Setup & Fixtures
        let preset_a = ModulePresetId::new();
        let preset_b = ModulePresetId::new();
        let current = vec![
            ModuleAssignment::new(ModuleType::Drive, preset_a, Order::new(0)),
        ];
        let target = vec![
            ModuleAssignment::new(ModuleType::Drive, preset_b, Order::new(0)),
        ];

        // -- Exec
        let diff = diff_module_assignments(&current, &target);

        // -- Check
        assert!(diff.structural_changes);
        assert!(diff.module_changes.contains(&ModuleChange::PresetChanged(ModuleType::Drive)));
        Ok(())
    }

    // -- diff_module_assignments: same preset different snapshot is not structural

    #[test]
    fn diff_same_preset_different_snapshot_not_structural() -> Result<()> {
        // -- Setup & Fixtures
        let preset_id = ModulePresetId::new();
        let snap_a = crate::id::ModuleSnapshotId::new();
        let snap_b = crate::id::ModuleSnapshotId::new();
        let current = vec![
            ModuleAssignment::new(ModuleType::Amp, preset_id, Order::new(0))
                .with_snapshot(snap_a),
        ];
        let target = vec![
            ModuleAssignment::new(ModuleType::Amp, preset_id, Order::new(0))
                .with_snapshot(snap_b),
        ];

        // -- Exec
        let diff = diff_module_assignments(&current, &target);

        // -- Check
        assert!(!diff.structural_changes, "same preset with different snapshot should not be structural");
        assert!(diff.can_morph());
        Ok(())
    }

    // -- diff_module_assignments: empty assignments

    #[test]
    fn diff_empty_assignments() -> Result<()> {
        // -- Exec
        let diff = diff_module_assignments(&[], &[]);

        // -- Check
        assert!(diff.module_changes.is_empty());
        assert!(!diff.structural_changes);
        Ok(())
    }

    // -- SnapshotDiff: total_changes

    #[test]
    fn snapshot_diff_total_changes() -> Result<()> {
        // -- Setup & Fixtures
        let diff = SnapshotDiff {
            changed_params: vec![
                ParameterChange::new(block_id_a(), "drive", NormalizedF64::new(0.1), NormalizedF64::new(0.9)),
                ParameterChange::new(block_id_a(), "tone", NormalizedF64::new(0.3), NormalizedF64::new(0.7)),
            ],
            bypass_changes: vec![
                BypassChange::new(block_id_b(), Some(false), Some(true)),
            ],
            module_changes: vec![
                ModuleChange::Added(ModuleType::Time),
            ],
            structural_changes: true,
        };

        // -- Check
        assert_eq!(diff.total_changes(), 4);
        assert!(!diff.is_empty());
        assert!(!diff.can_morph());
        Ok(())
    }
}
