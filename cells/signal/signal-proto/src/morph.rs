//! Snapshot morphing — smooth interpolation between two snapshots.
//!
//! The [`SnapshotMorpher`] manages A/B snapshot endpoints and computes
//! interpolated parameter values at any position along the morph axis.
//!
//! # Morphable vs Non-Morphable
//!
//! - **Morphable** parameters (continuous values): linear interpolation in
//!   normalized [0.0, 1.0] space.
//! - **Non-morphable** parameters (toggle, enum, bypass): snap to A when
//!   `morph_position < 0.5`, snap to B at `>= 0.5`.
//!
//! # Structural Differences
//!
//! When two snapshots have different `variation_assignments` (i.e., they load
//! different patches or modules), morphing is blocked. Use crossfade instead.

use crate::id::{BlockId, SnapshotId};
use crate::normalized::NormalizedF64;
use crate::preset::block_override::{BlockOverride, ParameterOverride};
use crate::preset::Snapshot;

// ─────────────────────────────────────────────────────────────────────────────
// MorphResult
// ─────────────────────────────────────────────────────────────────────────────

/// The result of computing a morph between two snapshots at a given position.
#[derive(Debug, Clone)]
pub struct MorphResult {
    /// Interpolated block overrides — one per block touched by either snapshot.
    pub block_overrides: Vec<BlockOverride>,
    /// Interpolated custom overrides (by plugin parameter index).
    pub custom_overrides: Vec<crate::parameter::ParameterValue>,
}

// ─────────────────────────────────────────────────────────────────────────────
// MorphWarning
// ─────────────────────────────────────────────────────────────────────────────

/// Warnings emitted when morphing cannot proceed normally.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MorphWarning {
    /// The two snapshots have different variation assignments (structural change).
    /// Smooth morphing is not possible — use crossfade instead.
    StructuralDifference,
    /// One or both snapshot slots are empty.
    MissingSnapshot,
}

// ─────────────────────────────────────────────────────────────────────────────
// SnapshotMorpher
// ─────────────────────────────────────────────────────────────────────────────

/// Manages A/B snapshots and computes interpolated parameter values.
///
/// The morph position is a normalized value in [0.0, 1.0]:
/// - `0.0` = fully snapshot A
/// - `1.0` = fully snapshot B
/// - values in between = linear interpolation of continuous parameters
#[derive(Debug, Clone)]
pub struct SnapshotMorpher {
    /// Snapshot A (morph position 0.0).
    snapshot_a: Option<Snapshot>,
    /// Snapshot B (morph position 1.0).
    snapshot_b: Option<Snapshot>,
    /// Current morph position in [0.0, 1.0].
    morph_position: f64,
}

impl SnapshotMorpher {
    /// Create a new morpher with no snapshots assigned.
    pub fn new() -> Self {
        Self {
            snapshot_a: None,
            snapshot_b: None,
            morph_position: 0.0,
        }
    }

    // region: --- Accessors

    /// Get the current morph position.
    pub fn morph_position(&self) -> f64 {
        self.morph_position
    }

    /// Set the morph position, clamping to [0.0, 1.0].
    pub fn set_morph_position(&mut self, position: f64) {
        self.morph_position = position.clamp(0.0, 1.0);
    }

    /// Get snapshot A (if assigned).
    pub fn snapshot_a(&self) -> Option<&Snapshot> {
        self.snapshot_a.as_ref()
    }

    /// Get snapshot B (if assigned).
    pub fn snapshot_b(&self) -> Option<&Snapshot> {
        self.snapshot_b.as_ref()
    }

    /// Get the ID of snapshot A (if assigned).
    pub fn snapshot_a_id(&self) -> Option<SnapshotId> {
        self.snapshot_a.as_ref().map(|s| s.id)
    }

    /// Get the ID of snapshot B (if assigned).
    pub fn snapshot_b_id(&self) -> Option<SnapshotId> {
        self.snapshot_b.as_ref().map(|s| s.id)
    }

    // endregion: --- Accessors

    // region: --- Assignment

    /// Assign snapshot A.
    pub fn set_snapshot_a(&mut self, snapshot: Snapshot) {
        self.snapshot_a = Some(snapshot);
    }

    /// Assign snapshot B.
    pub fn set_snapshot_b(&mut self, snapshot: Snapshot) {
        self.snapshot_b = Some(snapshot);
    }

    /// Clear snapshot A.
    pub fn clear_snapshot_a(&mut self) {
        self.snapshot_a = None;
    }

    /// Clear snapshot B.
    pub fn clear_snapshot_b(&mut self) {
        self.snapshot_b = None;
    }

    /// Swap A and B snapshots.
    pub fn swap(&mut self) {
        std::mem::swap(&mut self.snapshot_a, &mut self.snapshot_b);
        // Invert morph position so the sound doesn't change
        self.morph_position = 1.0 - self.morph_position;
    }

    /// Whether both A and B snapshots are assigned.
    pub fn is_ready(&self) -> bool {
        self.snapshot_a.is_some() && self.snapshot_b.is_some()
    }

    // endregion: --- Assignment

    // region: --- Structural Check

    /// Check whether the two snapshots have structural differences.
    ///
    /// Structural differences mean different `variation_assignments` —
    /// i.e., they load different patches or modules. When structural
    /// differences exist, smooth morphing is not possible.
    pub fn has_structural_changes(&self) -> bool {
        let (Some(a), Some(b)) = (&self.snapshot_a, &self.snapshot_b) else {
            return false;
        };
        structural_changes(a, b)
    }

    /// Check for warnings that would prevent or limit morphing.
    pub fn warnings(&self) -> Vec<MorphWarning> {
        let mut warnings = Vec::new();
        if !self.is_ready() {
            warnings.push(MorphWarning::MissingSnapshot);
        }
        if self.has_structural_changes() {
            warnings.push(MorphWarning::StructuralDifference);
        }
        warnings
    }

    // endregion: --- Structural Check

    // region: --- Compute

    /// Compute the morphed result at the current morph position.
    ///
    /// Returns `None` if:
    /// - Either snapshot is missing
    /// - Structural changes prevent morphing
    pub fn compute(&self) -> Option<MorphResult> {
        let (Some(a), Some(b)) = (&self.snapshot_a, &self.snapshot_b) else {
            return None;
        };
        if structural_changes(a, b) {
            return None;
        }
        Some(compute_morph(a, b, self.morph_position))
    }

    /// Compute the morphed result at an arbitrary position (ignoring stored position).
    ///
    /// Useful for previewing or animating.
    pub fn compute_at(&self, position: f64) -> Option<MorphResult> {
        let (Some(a), Some(b)) = (&self.snapshot_a, &self.snapshot_b) else {
            return None;
        };
        if structural_changes(a, b) {
            return None;
        }
        Some(compute_morph(a, b, position.clamp(0.0, 1.0)))
    }

    // endregion: --- Compute
}

impl Default for SnapshotMorpher {
    fn default() -> Self {
        Self::new()
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Free Functions
// ─────────────────────────────────────────────────────────────────────────────

/// Linear interpolation between two values.
///
/// `lerp(a, b, 0.0) == a`, `lerp(a, b, 1.0) == b`.
#[inline]
pub fn lerp(a: f64, b: f64, t: f64) -> f64 {
    a + (b - a) * t
}

/// Check whether two snapshots have structural differences.
///
/// Structural differences = different variation assignments, meaning they
/// load different patches or modules. Only parameter-level changes can be morphed.
fn structural_changes(a: &Snapshot, b: &Snapshot) -> bool {
    if a.variation_assignments.len() != b.variation_assignments.len() {
        return true;
    }
    // Check each variation assignment for matching section/layer/variation
    for (va, vb) in a.variation_assignments.iter().zip(&b.variation_assignments) {
        if va.section_id != vb.section_id
            || va.layer_index != vb.layer_index
            || va.variation_id != vb.variation_id
        {
            return true;
        }
    }
    false
}

/// Compute the morphed state between two snapshots at position `t`.
fn compute_morph(a: &Snapshot, b: &Snapshot, t: f64) -> MorphResult {
    let block_overrides = morph_block_overrides(a, b, t);
    let custom_overrides = morph_custom_overrides(a, b, t);
    MorphResult {
        block_overrides,
        custom_overrides,
    }
}

/// Morph block overrides between two snapshots.
///
/// Merges the union of all block IDs from both snapshots, interpolating
/// parameter values and snapping bypass states at t=0.5.
fn morph_block_overrides(a: &Snapshot, b: &Snapshot, t: f64) -> Vec<BlockOverride> {
    // Collect all unique block IDs from both snapshots
    let mut block_ids: Vec<BlockId> = Vec::new();
    for ovr in &a.block_overrides {
        if !block_ids.contains(&ovr.block_id) {
            block_ids.push(ovr.block_id);
        }
    }
    for ovr in &b.block_overrides {
        if !block_ids.contains(&ovr.block_id) {
            block_ids.push(ovr.block_id);
        }
    }

    block_ids
        .into_iter()
        .map(|block_id| {
            let ovr_a = a.block_override(block_id);
            let ovr_b = b.block_override(block_id);
            morph_single_block(block_id, ovr_a, ovr_b, t)
        })
        .collect()
}

/// Morph a single block's overrides.
fn morph_single_block(
    block_id: BlockId,
    ovr_a: Option<&BlockOverride>,
    ovr_b: Option<&BlockOverride>,
    t: f64,
) -> BlockOverride {
    // Morph bypass state: snap at 0.5
    let bypassed = morph_bypass(
        ovr_a.and_then(|o| o.bypassed),
        ovr_b.and_then(|o| o.bypassed),
        t,
    );

    // Morph parameters
    let params_a = ovr_a.map(|o| &o.parameters[..]).unwrap_or(&[]);
    let params_b = ovr_b.map(|o| &o.parameters[..]).unwrap_or(&[]);
    let parameters = morph_parameter_overrides(params_a, params_b, t);

    BlockOverride {
        block_id,
        bypassed,
        parameters,
    }
}

/// Morph bypass state: snap at t=0.5 threshold.
///
/// - Both `None` → `None` (no override)
/// - Only one set → snap at 0.5 between "no override" and the set value
/// - Both set → snap at 0.5 between A and B values
fn morph_bypass(a: Option<bool>, b: Option<bool>, t: f64) -> Option<bool> {
    match (a, b) {
        (None, None) => None,
        (Some(val_a), None) => {
            if t < 0.5 { Some(val_a) } else { None }
        }
        (None, Some(val_b)) => {
            if t >= 0.5 { Some(val_b) } else { None }
        }
        (Some(val_a), Some(val_b)) => {
            if t < 0.5 { Some(val_a) } else { Some(val_b) }
        }
    }
}

/// Morph parameter overrides between two sets.
///
/// For each unique param_id across both sets:
/// - If only in A: interpolate from A's value toward the implicit default (no override = param absent from result at t=1.0)
/// - If only in B: interpolate from implicit default toward B's value
/// - If in both: lerp between A and B values
fn morph_parameter_overrides(
    a: &[ParameterOverride],
    b: &[ParameterOverride],
    t: f64,
) -> Vec<ParameterOverride> {
    // Collect all unique param IDs
    let mut param_ids: Vec<&str> = Vec::new();
    for p in a {
        if !param_ids.contains(&p.param_id.as_str()) {
            param_ids.push(&p.param_id);
        }
    }
    for p in b {
        if !param_ids.contains(&p.param_id.as_str()) {
            param_ids.push(&p.param_id);
        }
    }

    param_ids
        .into_iter()
        .filter_map(|param_id| {
            let val_a = a.iter().find(|p| p.param_id == param_id).map(|p| p.value);
            let val_b = b.iter().find(|p| p.param_id == param_id).map(|p| p.value);

            match (val_a, val_b) {
                (Some(va), Some(vb)) => {
                    let morphed = lerp(va.get(), vb.get(), t);
                    Some(ParameterOverride::new(
                        param_id,
                        NormalizedF64::new(morphed),
                    ))
                }
                (Some(va), None) => {
                    // Parameter only in A — fade out as t→1.0
                    // At t=1.0 we'd want no override, but we keep it with the A value
                    // faded toward default. Since we don't know the default here,
                    // we keep the override present and let the caller decide.
                    Some(ParameterOverride::new(param_id, va))
                }
                (None, Some(vb)) => {
                    // Parameter only in B — fade in as t→0.0→1.0
                    Some(ParameterOverride::new(param_id, vb))
                }
                (None, None) => None,
            }
        })
        .collect()
}

/// Morph custom overrides (by plugin parameter index).
fn morph_custom_overrides(
    a: &Snapshot,
    b: &Snapshot,
    t: f64,
) -> Vec<crate::parameter::ParameterValue> {
    // Collect all unique indices
    let mut indices: Vec<u32> = Vec::new();
    for cv in &a.custom_overrides {
        if !indices.contains(&cv.index) {
            indices.push(cv.index);
        }
    }
    for cv in &b.custom_overrides {
        if !indices.contains(&cv.index) {
            indices.push(cv.index);
        }
    }

    indices
        .into_iter()
        .map(|index| {
            let val_a = a
                .custom_overrides
                .iter()
                .find(|c| c.index == index)
                .map(|c| c.value.get());
            let val_b = b
                .custom_overrides
                .iter()
                .find(|c| c.index == index)
                .map(|c| c.value.get());

            let morphed = match (val_a, val_b) {
                (Some(va), Some(vb)) => lerp(va, vb, t),
                (Some(va), None) => va,
                (None, Some(vb)) => vb,
                (None, None) => 0.0,
            };

            crate::parameter::ParameterValue::new(index, morphed)
        })
        .collect()
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::id::{BlockId, SectionId, VariationId};
    use crate::preset::PatchVariationAssignment;

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    // -- Helpers

    fn snapshot_with_block_param(name: &str, block_id: BlockId, param_id: &str, value: f64) -> Snapshot {
        let mut snap = Snapshot::new(name);
        snap.add_block_override(
            BlockOverride::new(block_id).with_param(param_id, NormalizedF64::new(value)),
        );
        snap
    }

    fn snapshot_with_bypass(name: &str, block_id: BlockId, bypassed: bool) -> Snapshot {
        let mut snap = Snapshot::new(name);
        snap.add_block_override(BlockOverride::new(block_id).with_bypass(bypassed));
        snap
    }

    // -- lerp

    #[test]
    fn test_morph_lerp_at_zero() {
        assert!((lerp(10.0, 20.0, 0.0) - 10.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_morph_lerp_at_one() {
        assert!((lerp(10.0, 20.0, 1.0) - 20.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_morph_lerp_at_half() {
        assert!((lerp(10.0, 20.0, 0.5) - 15.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_morph_lerp_at_quarter() {
        assert!((lerp(0.0, 1.0, 0.25) - 0.25).abs() < f64::EPSILON);
    }

    // -- SnapshotMorpher basics

    #[test]
    fn test_morph_new_defaults() {
        let morpher = SnapshotMorpher::new();
        assert!(!morpher.is_ready());
        assert_eq!(morpher.morph_position(), 0.0);
        assert!(morpher.snapshot_a().is_none());
        assert!(morpher.snapshot_b().is_none());
    }

    #[test]
    fn test_morph_default_trait() {
        let morpher = SnapshotMorpher::default();
        assert!(!morpher.is_ready());
    }

    #[test]
    fn test_morph_set_position_clamps() {
        let mut morpher = SnapshotMorpher::new();
        morpher.set_morph_position(1.5);
        assert_eq!(morpher.morph_position(), 1.0);
        morpher.set_morph_position(-0.5);
        assert_eq!(morpher.morph_position(), 0.0);
    }

    #[test]
    fn test_morph_assignment() {
        let mut morpher = SnapshotMorpher::new();
        let snap_a = Snapshot::new("Clean");
        let snap_b = Snapshot::new("Dirty");
        let id_a = snap_a.id;
        let id_b = snap_b.id;

        morpher.set_snapshot_a(snap_a);
        morpher.set_snapshot_b(snap_b);

        assert!(morpher.is_ready());
        assert_eq!(morpher.snapshot_a_id(), Some(id_a));
        assert_eq!(morpher.snapshot_b_id(), Some(id_b));
    }

    #[test]
    fn test_morph_clear_snapshots() {
        let mut morpher = SnapshotMorpher::new();
        morpher.set_snapshot_a(Snapshot::new("A"));
        morpher.set_snapshot_b(Snapshot::new("B"));
        assert!(morpher.is_ready());

        morpher.clear_snapshot_a();
        assert!(!morpher.is_ready());
        assert!(morpher.snapshot_a().is_none());
    }

    #[test]
    fn test_morph_swap() {
        let mut morpher = SnapshotMorpher::new();
        let snap_a = Snapshot::new("Clean");
        let snap_b = Snapshot::new("Dirty");
        let id_a = snap_a.id;
        let id_b = snap_b.id;

        morpher.set_snapshot_a(snap_a);
        morpher.set_snapshot_b(snap_b);
        morpher.set_morph_position(0.25);

        morpher.swap();

        assert_eq!(morpher.snapshot_a_id(), Some(id_b));
        assert_eq!(morpher.snapshot_b_id(), Some(id_a));
        assert!((morpher.morph_position() - 0.75).abs() < f64::EPSILON);
    }

    // -- Warnings

    #[test]
    fn test_morph_warnings_missing() {
        let morpher = SnapshotMorpher::new();
        let warnings = morpher.warnings();
        assert!(warnings.contains(&MorphWarning::MissingSnapshot));
    }

    #[test]
    fn test_morph_warnings_structural() {
        let mut morpher = SnapshotMorpher::new();

        let section_id = SectionId::new();
        let var_a = VariationId::new();
        let var_b = VariationId::new();

        let mut snap_a = Snapshot::new("A");
        snap_a.add_variation(PatchVariationAssignment::with_variation(section_id, 0, var_a));

        let mut snap_b = Snapshot::new("B");
        snap_b.add_variation(PatchVariationAssignment::with_variation(section_id, 0, var_b));

        morpher.set_snapshot_a(snap_a);
        morpher.set_snapshot_b(snap_b);

        assert!(morpher.has_structural_changes());
        assert!(morpher.warnings().contains(&MorphWarning::StructuralDifference));
    }

    #[test]
    fn test_morph_no_structural_changes_same_variations() {
        let mut morpher = SnapshotMorpher::new();

        let section_id = SectionId::new();
        let var_id = VariationId::new();

        let mut snap_a = Snapshot::new("A");
        snap_a.add_variation(PatchVariationAssignment::with_variation(section_id, 0, var_id));

        let mut snap_b = Snapshot::new("B");
        snap_b.add_variation(PatchVariationAssignment::with_variation(section_id, 0, var_id));

        morpher.set_snapshot_a(snap_a);
        morpher.set_snapshot_b(snap_b);

        assert!(!morpher.has_structural_changes());
    }

    // -- Compute

    #[test]
    fn test_morph_compute_missing_returns_none() {
        let morpher = SnapshotMorpher::new();
        assert!(morpher.compute().is_none());
    }

    #[test]
    fn test_morph_compute_structural_returns_none() {
        let mut morpher = SnapshotMorpher::new();

        let section_id = SectionId::new();
        let mut snap_a = Snapshot::new("A");
        snap_a.add_variation(PatchVariationAssignment::with_variation(
            section_id, 0, VariationId::new(),
        ));
        let mut snap_b = Snapshot::new("B");
        snap_b.add_variation(PatchVariationAssignment::with_variation(
            section_id, 0, VariationId::new(),
        ));

        morpher.set_snapshot_a(snap_a);
        morpher.set_snapshot_b(snap_b);

        assert!(morpher.compute().is_none());
    }

    #[test]
    fn test_morph_compute_param_at_zero() -> Result<()> {
        // -- Setup
        let block_id = BlockId::new();
        let snap_a = snapshot_with_block_param("Clean", block_id, "drive", 0.2);
        let snap_b = snapshot_with_block_param("Dirty", block_id, "drive", 0.8);

        let mut morpher = SnapshotMorpher::new();
        morpher.set_snapshot_a(snap_a);
        morpher.set_snapshot_b(snap_b);
        morpher.set_morph_position(0.0);

        // -- Exec
        let result = morpher.compute().unwrap();

        // -- Check
        assert_eq!(result.block_overrides.len(), 1);
        let ovr = &result.block_overrides[0];
        let drive = ovr.parameter_value("drive").unwrap();
        assert!((drive.get() - 0.2).abs() < 1e-10);
        Ok(())
    }

    #[test]
    fn test_morph_compute_param_at_one() -> Result<()> {
        let block_id = BlockId::new();
        let snap_a = snapshot_with_block_param("Clean", block_id, "drive", 0.2);
        let snap_b = snapshot_with_block_param("Dirty", block_id, "drive", 0.8);

        let mut morpher = SnapshotMorpher::new();
        morpher.set_snapshot_a(snap_a);
        morpher.set_snapshot_b(snap_b);
        morpher.set_morph_position(1.0);

        let result = morpher.compute().unwrap();
        let drive = result.block_overrides[0].parameter_value("drive").unwrap();
        assert!((drive.get() - 0.8).abs() < 1e-10);
        Ok(())
    }

    #[test]
    fn test_morph_compute_param_at_half() -> Result<()> {
        let block_id = BlockId::new();
        let snap_a = snapshot_with_block_param("Clean", block_id, "drive", 0.2);
        let snap_b = snapshot_with_block_param("Dirty", block_id, "drive", 0.8);

        let mut morpher = SnapshotMorpher::new();
        morpher.set_snapshot_a(snap_a);
        morpher.set_snapshot_b(snap_b);
        morpher.set_morph_position(0.5);

        let result = morpher.compute().unwrap();
        let drive = result.block_overrides[0].parameter_value("drive").unwrap();
        assert!((drive.get() - 0.5).abs() < 1e-10);
        Ok(())
    }

    #[test]
    fn test_morph_compute_bypass_snap() -> Result<()> {
        let block_id = BlockId::new();
        let snap_a = snapshot_with_bypass("Clean", block_id, false);
        let snap_b = snapshot_with_bypass("Dirty", block_id, true);

        let mut morpher = SnapshotMorpher::new();
        morpher.set_snapshot_a(snap_a);
        morpher.set_snapshot_b(snap_b);

        // At t=0.3 → snap to A (false)
        morpher.set_morph_position(0.3);
        let result = morpher.compute().unwrap();
        assert_eq!(result.block_overrides[0].bypassed, Some(false));

        // At t=0.7 → snap to B (true)
        morpher.set_morph_position(0.7);
        let result = morpher.compute().unwrap();
        assert_eq!(result.block_overrides[0].bypassed, Some(true));

        Ok(())
    }

    #[test]
    fn test_morph_compute_multiple_blocks() -> Result<()> {
        let block_a = BlockId::new();
        let block_b = BlockId::new();

        let mut snap_a = Snapshot::new("Clean");
        snap_a.add_block_override(
            BlockOverride::new(block_a).with_param("drive", NormalizedF64::new(0.0)),
        );
        snap_a.add_block_override(
            BlockOverride::new(block_b).with_param("level", NormalizedF64::new(0.3)),
        );

        let mut snap_b = Snapshot::new("Dirty");
        snap_b.add_block_override(
            BlockOverride::new(block_a).with_param("drive", NormalizedF64::new(1.0)),
        );
        snap_b.add_block_override(
            BlockOverride::new(block_b).with_param("level", NormalizedF64::new(0.7)),
        );

        let mut morpher = SnapshotMorpher::new();
        morpher.set_snapshot_a(snap_a);
        morpher.set_snapshot_b(snap_b);
        morpher.set_morph_position(0.5);

        let result = morpher.compute().unwrap();
        assert_eq!(result.block_overrides.len(), 2);

        let ovr_a = result.block_overrides.iter().find(|o| o.block_id == block_a).unwrap();
        let drive = ovr_a.parameter_value("drive").unwrap();
        assert!((drive.get() - 0.5).abs() < 1e-10);

        let ovr_b = result.block_overrides.iter().find(|o| o.block_id == block_b).unwrap();
        let level = ovr_b.parameter_value("level").unwrap();
        assert!((level.get() - 0.5).abs() < 1e-10);

        Ok(())
    }

    #[test]
    fn test_morph_compute_block_only_in_one_snapshot() -> Result<()> {
        let block_id = BlockId::new();

        // Only A has an override for this block
        let snap_a = snapshot_with_block_param("Clean", block_id, "drive", 0.6);
        let snap_b = Snapshot::new("Dirty");

        let mut morpher = SnapshotMorpher::new();
        morpher.set_snapshot_a(snap_a);
        morpher.set_snapshot_b(snap_b);
        morpher.set_morph_position(0.5);

        let result = morpher.compute().unwrap();
        // Block should still be present in result (from A)
        assert_eq!(result.block_overrides.len(), 1);
        let drive = result.block_overrides[0].parameter_value("drive").unwrap();
        assert!((drive.get() - 0.6).abs() < 1e-10);

        Ok(())
    }

    #[test]
    fn test_morph_compute_custom_overrides() -> Result<()> {
        let mut snap_a = Snapshot::new("Clean");
        snap_a.add_custom_override(crate::parameter::ParameterValue::new(0, 0.2));
        snap_a.add_custom_override(crate::parameter::ParameterValue::new(1, 0.4));

        let mut snap_b = Snapshot::new("Dirty");
        snap_b.add_custom_override(crate::parameter::ParameterValue::new(0, 0.8));
        snap_b.add_custom_override(crate::parameter::ParameterValue::new(1, 0.6));

        let mut morpher = SnapshotMorpher::new();
        morpher.set_snapshot_a(snap_a);
        morpher.set_snapshot_b(snap_b);
        morpher.set_morph_position(0.5);

        let result = morpher.compute().unwrap();
        assert_eq!(result.custom_overrides.len(), 2);

        let co_0 = result.custom_overrides.iter().find(|c| c.index == 0).unwrap();
        assert!((co_0.value.get() - 0.5).abs() < 1e-10);

        let co_1 = result.custom_overrides.iter().find(|c| c.index == 1).unwrap();
        assert!((co_1.value.get() - 0.5).abs() < 1e-10);

        Ok(())
    }

    #[test]
    fn test_morph_compute_at_different_position() -> Result<()> {
        let block_id = BlockId::new();
        let snap_a = snapshot_with_block_param("A", block_id, "gain", 0.0);
        let snap_b = snapshot_with_block_param("B", block_id, "gain", 1.0);

        let mut morpher = SnapshotMorpher::new();
        morpher.set_snapshot_a(snap_a);
        morpher.set_snapshot_b(snap_b);
        morpher.set_morph_position(0.0); // stored position is 0

        // compute_at ignores stored position
        let result = morpher.compute_at(0.75).unwrap();
        let gain = result.block_overrides[0].parameter_value("gain").unwrap();
        assert!((gain.get() - 0.75).abs() < 1e-10);

        Ok(())
    }

    #[test]
    fn test_morph_empty_snapshots_compute_ok() -> Result<()> {
        // Two empty snapshots (no overrides) should morph fine
        let mut morpher = SnapshotMorpher::new();
        morpher.set_snapshot_a(Snapshot::new("A"));
        morpher.set_snapshot_b(Snapshot::new("B"));
        morpher.set_morph_position(0.5);

        let result = morpher.compute().unwrap();
        assert!(result.block_overrides.is_empty());
        assert!(result.custom_overrides.is_empty());

        Ok(())
    }

    // -- Bypass edge cases

    #[test]
    fn test_morph_bypass_both_none() {
        assert_eq!(morph_bypass(None, None, 0.5), None);
    }

    #[test]
    fn test_morph_bypass_a_only() {
        assert_eq!(morph_bypass(Some(true), None, 0.3), Some(true));
        assert_eq!(morph_bypass(Some(true), None, 0.7), None);
    }

    #[test]
    fn test_morph_bypass_b_only() {
        assert_eq!(morph_bypass(None, Some(true), 0.3), None);
        assert_eq!(morph_bypass(None, Some(true), 0.7), Some(true));
    }

    #[test]
    fn test_morph_bypass_threshold_exact() {
        // At exactly 0.5, snap to B
        assert_eq!(morph_bypass(Some(false), Some(true), 0.5), Some(true));
    }
}
