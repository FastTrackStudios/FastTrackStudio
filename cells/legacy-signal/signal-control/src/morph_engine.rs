//! Morph Engine — interpolate between two parameter snapshots with easing curves.
//!
//! The morph engine is intentionally synchronous: it computes intermediate
//! parameter values without any DAW I/O. This allows the morph slider to
//! update at frame rate (60fps) while a separate async step applies the
//! values to the DAW.
//!
//! # Usage
//!
//! ```ignore
//! let mut engine = MorphEngine::new();
//! engine.set_a(snapshot_a);
//! engine.set_b(snapshot_b);
//!
//! // Morph at 50% with ease-in-out
//! let changes = engine.morph(0.5, EasingCurve::EaseInOut);
//! // Apply changes to DAW...
//! ```

use std::collections::HashMap;

use crate::daw_bridge::{DawFxParameterState, DawParamChange, DawParameterSnapshot};

// ─────────────────────────────────────────────────────────────────────────────
// Easing Curves
// ─────────────────────────────────────────────────────────────────────────────

/// Easing curve for morph interpolation.
///
/// All curves map `t ∈ [0, 1]` → `[0, 1]` with `f(0) = 0` and `f(1) = 1`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EasingCurve {
    /// Linear interpolation: `f(t) = t`
    Linear,
    /// Quadratic ease-in (slow start): `f(t) = t²`
    EaseIn,
    /// Quadratic ease-out (slow end): `f(t) = 1 - (1-t)²`
    EaseOut,
    /// Quadratic ease-in-out (slow start + end): smooth S-curve
    EaseInOut,
}

impl EasingCurve {
    /// Apply the easing function to a linear `t` value in `[0, 1]`.
    ///
    /// Values outside `[0, 1]` are clamped.
    pub fn apply(&self, t: f64) -> f64 {
        let t = t.clamp(0.0, 1.0);
        match self {
            EasingCurve::Linear => t,
            EasingCurve::EaseIn => t * t,
            EasingCurve::EaseOut => 1.0 - (1.0 - t) * (1.0 - t),
            EasingCurve::EaseInOut => {
                if t < 0.5 {
                    2.0 * t * t
                } else {
                    1.0 - (-2.0 * t + 2.0).powi(2) / 2.0
                }
            }
        }
    }

    /// All available easing curves (for UI dropdowns).
    pub fn all() -> &'static [EasingCurve] {
        &[
            EasingCurve::Linear,
            EasingCurve::EaseIn,
            EasingCurve::EaseOut,
            EasingCurve::EaseInOut,
        ]
    }

    /// Human-readable label for display.
    pub fn label(&self) -> &'static str {
        match self {
            EasingCurve::Linear => "Linear",
            EasingCurve::EaseIn => "Ease In",
            EasingCurve::EaseOut => "Ease Out",
            EasingCurve::EaseInOut => "Ease In/Out",
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Morph Engine
// ─────────────────────────────────────────────────────────────────────────────

/// Index for fast parameter lookup: `(fx_guid, param_index) → value`.
type ParamMap = HashMap<(String, u32), ParamEntry>;

#[derive(Debug, Clone)]
struct ParamEntry {
    param_name: String,
    value: f64,
}

/// Interpolation engine for morphing between two parameter snapshots.
///
/// Precomputes a diff on `set_a`/`set_b` so that `morph()` is O(diff_count),
/// not O(total_params). This matters for real-time slider updates.
#[derive(Debug, Clone)]
pub struct MorphEngine {
    snapshot_a: Option<DawParameterSnapshot>,
    snapshot_b: Option<DawParameterSnapshot>,
    /// Pre-computed diff entries: only parameters that differ between A and B.
    diffs: Vec<MorphDiffEntry>,
}

#[derive(Debug, Clone)]
struct MorphDiffEntry {
    fx_guid: String,
    param_index: u32,
    param_name: String,
    value_a: f64,
    value_b: f64,
}

impl MorphEngine {
    /// Create a new empty morph engine.
    pub fn new() -> Self {
        Self {
            snapshot_a: None,
            snapshot_b: None,
            diffs: Vec::new(),
        }
    }

    /// Set snapshot A (the "from" endpoint, t=0).
    pub fn set_a(&mut self, snapshot: DawParameterSnapshot) {
        self.snapshot_a = Some(snapshot);
        self.recompute_diffs();
    }

    /// Set snapshot B (the "to" endpoint, t=1).
    pub fn set_b(&mut self, snapshot: DawParameterSnapshot) {
        self.snapshot_b = Some(snapshot);
        self.recompute_diffs();
    }

    /// Returns true if both A and B are loaded.
    pub fn is_ready(&self) -> bool {
        self.snapshot_a.is_some() && self.snapshot_b.is_some()
    }

    /// Number of parameters that differ between A and B.
    pub fn diff_count(&self) -> usize {
        self.diffs.len()
    }

    /// Interpolate between A and B at position `t` with the given easing curve.
    ///
    /// - `t = 0.0` → snapshot A
    /// - `t = 1.0` → snapshot B
    ///
    /// Returns only parameters that differ between A and B, with interpolated values.
    /// Returns an empty vec if either snapshot is missing.
    pub fn morph(&self, t: f64, easing: EasingCurve) -> Vec<DawParamChange> {
        if !self.is_ready() {
            return Vec::new();
        }

        let eased_t = easing.apply(t);

        self.diffs
            .iter()
            .map(|d| {
                let interpolated = d.value_a + eased_t * (d.value_b - d.value_a);
                DawParamChange {
                    fx_guid: d.fx_guid.clone(),
                    param_index: d.param_index,
                    param_name: d.param_name.clone(),
                    from_value: d.value_a,
                    to_value: interpolated,
                }
            })
            .collect()
    }

    /// Recompute the diff between A and B.
    fn recompute_diffs(&mut self) {
        self.diffs.clear();

        let (Some(a), Some(b)) = (&self.snapshot_a, &self.snapshot_b) else {
            return;
        };

        let a_map = build_param_map(&a.fx_states);
        let b_map = build_param_map(&b.fx_states);

        // Find all parameters in B and compute diff against A
        for ((guid, idx), b_entry) in &b_map {
            let a_value = a_map
                .get(&(guid.clone(), *idx))
                .map(|e| e.value)
                .unwrap_or(0.0);

            let delta = (a_value - b_entry.value).abs();
            if delta > 0.0001 {
                self.diffs.push(MorphDiffEntry {
                    fx_guid: guid.clone(),
                    param_index: *idx,
                    param_name: b_entry.param_name.clone(),
                    value_a: a_value,
                    value_b: b_entry.value,
                });
            }
        }

        // Find parameters only in A (not in B) — these morph from A value to 0.0
        for ((guid, idx), a_entry) in &a_map {
            if !b_map.contains_key(&(guid.clone(), *idx)) && a_entry.value.abs() > 0.0001 {
                self.diffs.push(MorphDiffEntry {
                    fx_guid: guid.clone(),
                    param_index: *idx,
                    param_name: a_entry.param_name.clone(),
                    value_a: a_entry.value,
                    value_b: 0.0,
                });
            }
        }
    }
}

impl Default for MorphEngine {
    fn default() -> Self {
        Self::new()
    }
}

/// Build a lookup map from a list of FX parameter states.
fn build_param_map(fx_states: &[DawFxParameterState]) -> ParamMap {
    let mut map = ParamMap::new();
    for fx in fx_states {
        for param in &fx.parameters {
            map.insert(
                (fx.fx_guid.clone(), param.param_index),
                ParamEntry {
                    param_name: param.param_name.clone(),
                    value: param.value,
                },
            );
        }
    }
    map
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::daw_bridge::{DawFxParameterState, DawParamValue, DawParameterSnapshot};

    fn make_snapshot(name: &str, fx_guid: &str, params: &[(u32, f64)]) -> DawParameterSnapshot {
        DawParameterSnapshot {
            name: name.to_string(),
            fx_states: vec![DawFxParameterState {
                fx_guid: fx_guid.to_string(),
                fx_index: 0,
                plugin_name: "TestPlugin".to_string(),
                enabled: true,
                parameters: params
                    .iter()
                    .map(|(idx, val)| DawParamValue {
                        param_index: *idx,
                        param_name: format!("Param {idx}"),
                        value: *val,
                        formatted: String::new(),
                        is_toggle: false,
                        step_count: None,
                        step_labels: Vec::new(),
                    })
                    .collect(),
            }],
        }
    }

    // -- Easing curve tests

    #[test]
    fn linear_easing_is_identity() {
        assert!((EasingCurve::Linear.apply(0.0) - 0.0).abs() < f64::EPSILON);
        assert!((EasingCurve::Linear.apply(0.5) - 0.5).abs() < f64::EPSILON);
        assert!((EasingCurve::Linear.apply(1.0) - 1.0).abs() < f64::EPSILON);
    }

    #[test]
    fn ease_in_starts_slow() {
        let mid = EasingCurve::EaseIn.apply(0.5);
        assert!(mid < 0.5, "ease-in at 0.5 should be < 0.5, got {mid}");
        assert!((mid - 0.25).abs() < f64::EPSILON); // t² = 0.25

        assert!((EasingCurve::EaseIn.apply(0.0) - 0.0).abs() < f64::EPSILON);
        assert!((EasingCurve::EaseIn.apply(1.0) - 1.0).abs() < f64::EPSILON);
    }

    #[test]
    fn ease_out_ends_slow() {
        let mid = EasingCurve::EaseOut.apply(0.5);
        assert!(mid > 0.5, "ease-out at 0.5 should be > 0.5, got {mid}");
        assert!((mid - 0.75).abs() < f64::EPSILON); // 1 - (1-0.5)² = 0.75

        assert!((EasingCurve::EaseOut.apply(0.0) - 0.0).abs() < f64::EPSILON);
        assert!((EasingCurve::EaseOut.apply(1.0) - 1.0).abs() < f64::EPSILON);
    }

    #[test]
    fn ease_in_out_is_symmetric() {
        assert!((EasingCurve::EaseInOut.apply(0.0) - 0.0).abs() < f64::EPSILON);
        assert!((EasingCurve::EaseInOut.apply(1.0) - 1.0).abs() < f64::EPSILON);
        assert!((EasingCurve::EaseInOut.apply(0.5) - 0.5).abs() < f64::EPSILON);

        // Symmetry: f(t) + f(1-t) = 1
        for t in [0.1, 0.2, 0.3, 0.4] {
            let sum = EasingCurve::EaseInOut.apply(t) + EasingCurve::EaseInOut.apply(1.0 - t);
            assert!(
                (sum - 1.0).abs() < 1e-10,
                "symmetry failed at t={t}: sum={sum}"
            );
        }
    }

    #[test]
    fn easing_clamps_out_of_range() {
        for curve in EasingCurve::all() {
            assert!(
                (curve.apply(-0.5) - 0.0).abs() < f64::EPSILON,
                "{curve:?} should clamp negative"
            );
            assert!(
                (curve.apply(1.5) - 1.0).abs() < f64::EPSILON,
                "{curve:?} should clamp above 1"
            );
        }
    }

    // -- Morph engine tests

    #[test]
    fn morph_at_zero_returns_a_values() {
        let mut engine = MorphEngine::new();
        engine.set_a(make_snapshot("A", "fx1", &[(0, 0.2), (1, 0.8)]));
        engine.set_b(make_snapshot("B", "fx1", &[(0, 0.9), (1, 0.1)]));

        let changes = engine.morph(0.0, EasingCurve::Linear);
        assert_eq!(changes.len(), 2);

        for c in &changes {
            assert!(
                (c.to_value - c.from_value).abs() < f64::EPSILON,
                "at t=0, to_value should equal from_value (A)"
            );
        }
    }

    #[test]
    fn morph_at_one_returns_b_values() {
        let mut engine = MorphEngine::new();
        engine.set_a(make_snapshot("A", "fx1", &[(0, 0.2)]));
        engine.set_b(make_snapshot("B", "fx1", &[(0, 0.9)]));

        let changes = engine.morph(1.0, EasingCurve::Linear);
        assert_eq!(changes.len(), 1);
        assert!((changes[0].to_value - 0.9).abs() < f64::EPSILON);
    }

    #[test]
    fn morph_linear_midpoint_is_average() {
        let mut engine = MorphEngine::new();
        engine.set_a(make_snapshot("A", "fx1", &[(0, 0.0)]));
        engine.set_b(make_snapshot("B", "fx1", &[(0, 1.0)]));

        let changes = engine.morph(0.5, EasingCurve::Linear);
        assert_eq!(changes.len(), 1);
        assert!((changes[0].to_value - 0.5).abs() < f64::EPSILON);
    }

    #[test]
    fn morph_only_includes_differing_params() {
        let mut engine = MorphEngine::new();
        // Param 0 differs, param 1 is the same
        engine.set_a(make_snapshot("A", "fx1", &[(0, 0.2), (1, 0.5)]));
        engine.set_b(make_snapshot("B", "fx1", &[(0, 0.8), (1, 0.5)]));

        let changes = engine.morph(0.5, EasingCurve::Linear);
        assert_eq!(changes.len(), 1, "should only include differing params");
        assert_eq!(changes[0].param_index, 0);
    }

    #[test]
    fn morph_with_ease_in_at_midpoint() {
        let mut engine = MorphEngine::new();
        engine.set_a(make_snapshot("A", "fx1", &[(0, 0.0)]));
        engine.set_b(make_snapshot("B", "fx1", &[(0, 1.0)]));

        // EaseIn at t=0.5: eased_t = 0.25, so value = 0.0 + 0.25 * 1.0 = 0.25
        let changes = engine.morph(0.5, EasingCurve::EaseIn);
        assert!((changes[0].to_value - 0.25).abs() < f64::EPSILON);
    }

    #[test]
    fn morph_empty_when_not_ready() {
        let mut engine = MorphEngine::new();
        engine.set_a(make_snapshot("A", "fx1", &[(0, 0.5)]));
        // B not set
        let changes = engine.morph(0.5, EasingCurve::Linear);
        assert!(changes.is_empty());
    }

    #[test]
    fn morph_handles_disjoint_fx() {
        let mut engine = MorphEngine::new();
        engine.set_a(make_snapshot("A", "fx1", &[(0, 0.8)]));
        engine.set_b(make_snapshot("B", "fx2", &[(0, 0.6)]));

        let changes = engine.morph(0.5, EasingCurve::Linear);
        // fx1 param in A but not B → morphs to 0.0
        // fx2 param in B but not A → morphs from 0.0
        assert_eq!(changes.len(), 2);
    }

    #[test]
    fn diff_count_reflects_differences() {
        let mut engine = MorphEngine::new();
        engine.set_a(make_snapshot("A", "fx1", &[(0, 0.1), (1, 0.5), (2, 0.9)]));
        engine.set_b(make_snapshot("B", "fx1", &[(0, 0.1), (1, 0.8), (2, 0.2)]));

        // Param 0 is same (0.1), params 1 and 2 differ
        assert_eq!(engine.diff_count(), 2);
    }
}
