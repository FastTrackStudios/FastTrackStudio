//! Snapshot crossfade — audio-level transitions for structural changes.
//!
//! When two snapshots have different module configurations (structural
//! differences), smooth parameter morphing is impossible. The
//! [`CrossfadeTransition`] handles this by orchestrating a timed crossfade:
//!
//! 1. **Fade out** the old signal chain
//! 2. **Swap** modules at the midpoint
//! 3. **Fade in** the new signal chain
//!
//! # Module Handling
//!
//! - **Modules in both snapshots**: morph parameters during the crossfade
//! - **New modules** (only in target): fade in from silence
//! - **Removed modules** (only in source): fade out to silence
//!
//! # Crossfade Curves
//!
//! Three curve shapes control the gain envelope:
//! - [`CrossfadeCurve::Linear`]: simple ramp (may cause slight dip at midpoint)
//! - [`CrossfadeCurve::EqualPower`]: constant-power crossfade (sqrt curve)
//! - [`CrossfadeCurve::SCurve`]: smooth S-curve (cubic Hermite, gentle transitions)

use crate::morph::{MorphResult, SnapshotMorpher};
use crate::preset::Snapshot;
use uuid::Uuid;

// ─────────────────────────────────────────────────────────────────────────────
// CrossfadeCurve
// ─────────────────────────────────────────────────────────────────────────────

/// Shape of the crossfade gain envelope.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CrossfadeCurve {
    /// Linear ramp: `gain = t`.
    Linear,
    /// Equal-power (constant power): `gain = sqrt(t)`.
    /// Prevents the perceived volume dip at the midpoint.
    EqualPower,
    /// S-curve (cubic Hermite): `gain = 3t² - 2t³`.
    /// Smooth acceleration and deceleration.
    SCurve,
}

impl CrossfadeCurve {
    /// Evaluate the curve at position `t` ∈ [0.0, 1.0].
    ///
    /// Returns the gain multiplier for the **incoming** signal.
    /// The outgoing signal uses `1.0 - apply(t)` for linear,
    /// or the complementary curve for equal-power/S-curve.
    pub fn apply(self, t: f64) -> f64 {
        let t = t.clamp(0.0, 1.0);
        match self {
            CrossfadeCurve::Linear => t,
            CrossfadeCurve::EqualPower => t.sqrt(),
            CrossfadeCurve::SCurve => 3.0 * t * t - 2.0 * t * t * t,
        }
    }

    /// Evaluate the complementary curve (for the outgoing signal).
    ///
    /// For equal-power: `sqrt(1 - t)` so that `out² + in² = 1`.
    /// For others: `1 - apply(t)`.
    pub fn apply_complement(self, t: f64) -> f64 {
        let t = t.clamp(0.0, 1.0);
        match self {
            CrossfadeCurve::EqualPower => (1.0 - t).sqrt(),
            _ => 1.0 - self.apply(t),
        }
    }
}

impl Default for CrossfadeCurve {
    fn default() -> Self {
        CrossfadeCurve::EqualPower
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// CrossfadePhase
// ─────────────────────────────────────────────────────────────────────────────

/// Current phase of the crossfade transition.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CrossfadePhase {
    /// Not transitioning — steady state.
    Idle,
    /// Fading out the source signal chain.
    FadingOut,
    /// Swapping module configuration at the midpoint.
    Swapping,
    /// Fading in the target signal chain.
    FadingIn,
    /// Transition complete.
    Complete,
}

// ─────────────────────────────────────────────────────────────────────────────
// ModuleTransition
// ─────────────────────────────────────────────────────────────────────────────

/// How a specific module should be handled during crossfade.
#[derive(Debug, Clone, PartialEq)]
pub enum ModuleTransition {
    /// Module exists in both snapshots — morph its parameters.
    Morph {
        module_id: Uuid,
        /// Current interpolated gain (for the morphing module, stays at 1.0).
        gain: f64,
    },
    /// Module only exists in the target — fade in from silence.
    FadeIn {
        module_id: Uuid,
        /// Current fade-in gain [0.0, 1.0].
        gain: f64,
    },
    /// Module only exists in the source — fade out to silence.
    FadeOut {
        module_id: Uuid,
        /// Current fade-out gain [1.0, 0.0].
        gain: f64,
    },
}

impl ModuleTransition {
    /// Get the module ID for this transition.
    pub fn module_id(&self) -> Uuid {
        match self {
            ModuleTransition::Morph { module_id, .. }
            | ModuleTransition::FadeIn { module_id, .. }
            | ModuleTransition::FadeOut { module_id, .. } => *module_id,
        }
    }

    /// Get the current gain level.
    pub fn gain(&self) -> f64 {
        match self {
            ModuleTransition::Morph { gain, .. }
            | ModuleTransition::FadeIn { gain, .. }
            | ModuleTransition::FadeOut { gain, .. } => *gain,
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// CrossfadeState
// ─────────────────────────────────────────────────────────────────────────────

/// Snapshot of the crossfade state at a given moment.
#[derive(Debug, Clone)]
pub struct CrossfadeState {
    /// Current phase of the transition.
    pub phase: CrossfadePhase,
    /// Normalized progress through the entire crossfade [0.0, 1.0].
    pub progress: f64,
    /// Gain for the outgoing (source) signal chain.
    pub source_gain: f64,
    /// Gain for the incoming (target) signal chain.
    pub target_gain: f64,
    /// Per-module transition details.
    pub module_transitions: Vec<ModuleTransition>,
    /// Morphed parameters for modules that exist in both snapshots (if any).
    pub morph_result: Option<MorphResult>,
}

// ─────────────────────────────────────────────────────────────────────────────
// CrossfadeTransition
// ─────────────────────────────────────────────────────────────────────────────

/// Duration range for crossfade: 0ms to 500ms.
pub const CROSSFADE_DURATION_MIN_MS: u64 = 0;
/// Maximum crossfade duration.
pub const CROSSFADE_DURATION_MAX_MS: u64 = 500;
/// Default crossfade duration (50ms — short enough to be imperceptible
/// but long enough to avoid clicks).
pub const CROSSFADE_DURATION_DEFAULT_MS: u64 = 50;

/// Orchestrates an audio crossfade between two snapshot configurations.
///
/// The transition proceeds through phases:
/// 1. `Idle` → `FadingOut` (triggered by [`start`](Self::start))
/// 2. `FadingOut` → `Swapping` (at midpoint)
/// 3. `Swapping` → `FadingIn` (immediate)
/// 4. `FadingIn` → `Complete` (at end)
///
/// Call [`tick`](Self::tick) with elapsed milliseconds to advance the transition.
#[derive(Debug, Clone)]
pub struct CrossfadeTransition {
    /// Crossfade duration in milliseconds.
    duration_ms: u64,
    /// Shape of the gain envelope.
    curve: CrossfadeCurve,
    /// Source snapshot (fading out).
    source: Option<Snapshot>,
    /// Target snapshot (fading in).
    target: Option<Snapshot>,
    /// Current phase.
    phase: CrossfadePhase,
    /// Elapsed time in milliseconds since the transition started.
    elapsed_ms: f64,
    /// Module IDs that exist only in the source snapshot.
    source_only_modules: Vec<Uuid>,
    /// Module IDs that exist only in the target snapshot.
    target_only_modules: Vec<Uuid>,
    /// Module IDs that exist in both snapshots.
    shared_modules: Vec<Uuid>,
    /// Internal morpher for shared modules' parameters.
    morpher: SnapshotMorpher,
}

impl CrossfadeTransition {
    /// Create a new crossfade transition with default settings.
    pub fn new() -> Self {
        Self {
            duration_ms: CROSSFADE_DURATION_DEFAULT_MS,
            curve: CrossfadeCurve::default(),
            source: None,
            target: None,
            phase: CrossfadePhase::Idle,
            elapsed_ms: 0.0,
            source_only_modules: Vec::new(),
            target_only_modules: Vec::new(),
            shared_modules: Vec::new(),
            morpher: SnapshotMorpher::new(),
        }
    }

    // region: --- Builder / Settings

    /// Set the crossfade duration in milliseconds (clamped to valid range).
    pub fn with_duration_ms(mut self, ms: u64) -> Self {
        self.duration_ms = ms.clamp(CROSSFADE_DURATION_MIN_MS, CROSSFADE_DURATION_MAX_MS);
        self
    }

    /// Set the crossfade curve shape.
    pub fn with_curve(mut self, curve: CrossfadeCurve) -> Self {
        self.curve = curve;
        self
    }

    /// Get the crossfade duration in milliseconds.
    pub fn duration_ms(&self) -> u64 {
        self.duration_ms
    }

    /// Set the duration (mutable, for runtime adjustment).
    pub fn set_duration_ms(&mut self, ms: u64) {
        self.duration_ms = ms.clamp(CROSSFADE_DURATION_MIN_MS, CROSSFADE_DURATION_MAX_MS);
    }

    /// Get the current curve.
    pub fn curve(&self) -> CrossfadeCurve {
        self.curve
    }

    /// Set the curve (mutable, for runtime adjustment).
    pub fn set_curve(&mut self, curve: CrossfadeCurve) {
        self.curve = curve;
    }

    // endregion: --- Builder / Settings

    // region: --- Lifecycle

    /// Get the current phase.
    pub fn phase(&self) -> CrossfadePhase {
        self.phase
    }

    /// Whether the transition is currently active (not Idle or Complete).
    pub fn is_active(&self) -> bool {
        matches!(
            self.phase,
            CrossfadePhase::FadingOut | CrossfadePhase::Swapping | CrossfadePhase::FadingIn
        )
    }

    /// Normalized progress through the entire crossfade [0.0, 1.0].
    pub fn progress(&self) -> f64 {
        if self.duration_ms == 0 {
            return if self.phase == CrossfadePhase::Idle {
                0.0
            } else {
                1.0
            };
        }
        (self.elapsed_ms / self.duration_ms as f64).clamp(0.0, 1.0)
    }

    /// Start a crossfade from `source` to `target`.
    ///
    /// Analyzes the module differences and begins the fade-out phase.
    /// If duration is 0, immediately completes.
    pub fn start(&mut self, source: Snapshot, target: Snapshot) {
        // Analyze module structure to determine per-module transitions
        self.analyze_modules(&source, &target);

        // Set up the morpher for shared modules
        self.morpher = SnapshotMorpher::new();
        self.morpher.set_snapshot_a(source.clone());
        self.morpher.set_snapshot_b(target.clone());

        self.source = Some(source);
        self.target = Some(target);
        self.elapsed_ms = 0.0;

        if self.duration_ms == 0 {
            // Instant transition
            self.phase = CrossfadePhase::Complete;
        } else {
            self.phase = CrossfadePhase::FadingOut;
        }
    }

    /// Advance the transition by `delta_ms` milliseconds.
    ///
    /// Returns the current [`CrossfadeState`] after advancing.
    pub fn tick(&mut self, delta_ms: f64) -> CrossfadeState {
        if !self.is_active() {
            return self.current_state();
        }

        self.elapsed_ms += delta_ms;
        let progress = self.progress();

        // Phase transitions based on progress (check sequentially so a
        // large delta can advance through multiple phases in one tick)
        if self.phase == CrossfadePhase::FadingOut && progress >= 0.5 {
            self.phase = CrossfadePhase::Swapping;
            // Swap is instantaneous — immediately transition to FadingIn
            self.phase = CrossfadePhase::FadingIn;
        }
        if self.phase == CrossfadePhase::FadingIn && progress >= 1.0 {
            self.phase = CrossfadePhase::Complete;
        }

        self.current_state()
    }

    /// Reset the transition to idle state.
    pub fn reset(&mut self) {
        self.phase = CrossfadePhase::Idle;
        self.elapsed_ms = 0.0;
        self.source = None;
        self.target = None;
        self.source_only_modules.clear();
        self.target_only_modules.clear();
        self.shared_modules.clear();
        self.morpher = SnapshotMorpher::new();
    }

    // endregion: --- Lifecycle

    // region: --- State Query

    /// Get the current crossfade state snapshot.
    pub fn current_state(&self) -> CrossfadeState {
        let progress = self.progress();
        let source_gain = self.curve.apply_complement(progress);
        let target_gain = self.curve.apply(progress);

        // Build per-module transitions
        let module_transitions = self.compute_module_transitions(progress);

        // Compute morph result for shared modules
        let morph_result = if !self.shared_modules.is_empty() {
            self.morpher.compute_at(progress)
        } else {
            None
        };

        CrossfadeState {
            phase: self.phase,
            progress,
            source_gain,
            target_gain,
            module_transitions,
            morph_result,
        }
    }

    /// Get the source snapshot (if set).
    pub fn source(&self) -> Option<&Snapshot> {
        self.source.as_ref()
    }

    /// Get the target snapshot (if set).
    pub fn target(&self) -> Option<&Snapshot> {
        self.target.as_ref()
    }

    // endregion: --- State Query

    // region: --- Internal

    /// Analyze module differences between source and target snapshots.
    ///
    /// Compares `variation_assignments` to classify each module/section as:
    /// - shared (exists in both)
    /// - source-only (will be removed)
    /// - target-only (will be added)
    fn analyze_modules(&mut self, source: &Snapshot, target: &Snapshot) {
        self.source_only_modules.clear();
        self.target_only_modules.clear();
        self.shared_modules.clear();

        // Use section_id as the module identifier (each variation assignment
        // represents a section/layer in the rig)
        let source_ids: Vec<Uuid> = source
            .variation_assignments
            .iter()
            .map(|va| va.section_id.as_uuid())
            .collect();
        let target_ids: Vec<Uuid> = target
            .variation_assignments
            .iter()
            .map(|va| va.section_id.as_uuid())
            .collect();

        // Shared: in both
        for id in &source_ids {
            if target_ids.contains(id) && !self.shared_modules.contains(id) {
                self.shared_modules.push(*id);
            }
        }

        // Source-only: in source but not target
        for id in &source_ids {
            if !target_ids.contains(id) {
                self.source_only_modules.push(*id);
            }
        }

        // Target-only: in target but not source
        for id in &target_ids {
            if !source_ids.contains(id) {
                self.target_only_modules.push(*id);
            }
        }
    }

    /// Compute per-module transitions at the given progress.
    fn compute_module_transitions(&self, progress: f64) -> Vec<ModuleTransition> {
        let mut transitions = Vec::new();

        // Shared modules: morphing with full gain
        for &module_id in &self.shared_modules {
            transitions.push(ModuleTransition::Morph {
                module_id,
                gain: 1.0,
            });
        }

        // Source-only modules: fade out
        for &module_id in &self.source_only_modules {
            let gain = self.curve.apply_complement(progress);
            transitions.push(ModuleTransition::FadeOut { module_id, gain });
        }

        // Target-only modules: fade in
        for &module_id in &self.target_only_modules {
            let gain = self.curve.apply(progress);
            transitions.push(ModuleTransition::FadeIn { module_id, gain });
        }

        transitions
    }

    // endregion: --- Internal
}

impl Default for CrossfadeTransition {
    fn default() -> Self {
        Self::new()
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::id::{SectionId, VariationId};
    use crate::normalized::NormalizedF64;
    use crate::preset::PatchVariationAssignment;

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    // -- Helpers

    fn make_snapshot(name: &str, sections: &[(SectionId, Option<VariationId>)]) -> Snapshot {
        let mut snap = Snapshot::new(name);
        for &(section_id, var_id) in sections {
            snap.add_variation(PatchVariationAssignment::new(section_id, 0, var_id));
        }
        snap
    }

    // -- CrossfadeCurve

    #[test]
    fn test_curve_linear_endpoints() {
        assert!((CrossfadeCurve::Linear.apply(0.0)).abs() < f64::EPSILON);
        assert!((CrossfadeCurve::Linear.apply(1.0) - 1.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_curve_linear_midpoint() {
        assert!((CrossfadeCurve::Linear.apply(0.5) - 0.5).abs() < f64::EPSILON);
    }

    #[test]
    fn test_curve_equal_power_endpoints() {
        assert!((CrossfadeCurve::EqualPower.apply(0.0)).abs() < f64::EPSILON);
        assert!((CrossfadeCurve::EqualPower.apply(1.0) - 1.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_curve_equal_power_midpoint() {
        // sqrt(0.5) ≈ 0.7071
        let val = CrossfadeCurve::EqualPower.apply(0.5);
        assert!((val - 0.5_f64.sqrt()).abs() < 1e-10);
    }

    #[test]
    fn test_curve_equal_power_constant_power() {
        // For equal-power crossfade: out² + in² = 1 at all points
        for i in 0..=10 {
            let t = i as f64 / 10.0;
            let gain_in = CrossfadeCurve::EqualPower.apply(t);
            let gain_out = CrossfadeCurve::EqualPower.apply_complement(t);
            let total_power = gain_in * gain_in + gain_out * gain_out;
            assert!(
                (total_power - 1.0).abs() < 1e-10,
                "Power not constant at t={t}: {total_power}"
            );
        }
    }

    #[test]
    fn test_curve_scurve_endpoints() {
        assert!((CrossfadeCurve::SCurve.apply(0.0)).abs() < f64::EPSILON);
        assert!((CrossfadeCurve::SCurve.apply(1.0) - 1.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_curve_scurve_midpoint() {
        // S-curve at 0.5: 3*(0.25) - 2*(0.125) = 0.75 - 0.25 = 0.5
        assert!((CrossfadeCurve::SCurve.apply(0.5) - 0.5).abs() < f64::EPSILON);
    }

    #[test]
    fn test_curve_clamping() {
        // Out-of-range values should be clamped
        assert!((CrossfadeCurve::Linear.apply(-0.5)).abs() < f64::EPSILON);
        assert!((CrossfadeCurve::Linear.apply(1.5) - 1.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_curve_complement_linear() {
        for i in 0..=10 {
            let t = i as f64 / 10.0;
            let val = CrossfadeCurve::Linear.apply(t);
            let comp = CrossfadeCurve::Linear.apply_complement(t);
            assert!(
                (val + comp - 1.0).abs() < 1e-10,
                "Linear complement broken at t={t}"
            );
        }
    }

    #[test]
    fn test_curve_default_is_equal_power() {
        assert_eq!(CrossfadeCurve::default(), CrossfadeCurve::EqualPower);
    }

    // -- CrossfadeTransition basics

    #[test]
    fn test_transition_new_defaults() {
        let xfade = CrossfadeTransition::new();
        assert_eq!(xfade.duration_ms(), CROSSFADE_DURATION_DEFAULT_MS);
        assert_eq!(xfade.curve(), CrossfadeCurve::EqualPower);
        assert_eq!(xfade.phase(), CrossfadePhase::Idle);
        assert!(!xfade.is_active());
        assert!((xfade.progress()).abs() < f64::EPSILON);
    }

    #[test]
    fn test_transition_builder() {
        let xfade = CrossfadeTransition::new()
            .with_duration_ms(200)
            .with_curve(CrossfadeCurve::SCurve);
        assert_eq!(xfade.duration_ms(), 200);
        assert_eq!(xfade.curve(), CrossfadeCurve::SCurve);
    }

    #[test]
    fn test_transition_duration_clamp() {
        let xfade = CrossfadeTransition::new().with_duration_ms(9999);
        assert_eq!(xfade.duration_ms(), CROSSFADE_DURATION_MAX_MS);
    }

    #[test]
    fn test_transition_set_duration_runtime() {
        let mut xfade = CrossfadeTransition::new();
        xfade.set_duration_ms(100);
        assert_eq!(xfade.duration_ms(), 100);
        xfade.set_duration_ms(999);
        assert_eq!(xfade.duration_ms(), CROSSFADE_DURATION_MAX_MS);
    }

    // -- Start & tick lifecycle

    #[test]
    fn test_transition_start_begins_fading() {
        let mut xfade = CrossfadeTransition::new().with_duration_ms(100);
        let section = SectionId::new();
        let source = make_snapshot("A", &[(section, Some(VariationId::new()))]);
        let target = make_snapshot("B", &[(section, Some(VariationId::new()))]);

        xfade.start(source, target);
        assert_eq!(xfade.phase(), CrossfadePhase::FadingOut);
        assert!(xfade.is_active());
    }

    #[test]
    fn test_transition_zero_duration_instant() {
        let mut xfade = CrossfadeTransition::new().with_duration_ms(0);
        let section = SectionId::new();
        let source = make_snapshot("A", &[(section, None)]);
        let target = make_snapshot("B", &[(section, None)]);

        xfade.start(source, target);
        assert_eq!(xfade.phase(), CrossfadePhase::Complete);
        assert!(!xfade.is_active());
    }

    #[test]
    fn test_transition_tick_progress() {
        let mut xfade = CrossfadeTransition::new().with_duration_ms(100);
        let section = SectionId::new();
        let source = make_snapshot("A", &[(section, None)]);
        let target = make_snapshot("B", &[(section, None)]);

        xfade.start(source, target);

        let state = xfade.tick(25.0);
        assert!((state.progress - 0.25).abs() < 1e-10);
        assert_eq!(state.phase, CrossfadePhase::FadingOut);

        let state = xfade.tick(25.0);
        assert!((state.progress - 0.5).abs() < 1e-10);
        // At midpoint, transitions to FadingIn
        assert_eq!(state.phase, CrossfadePhase::FadingIn);
    }

    #[test]
    fn test_transition_tick_to_completion() {
        let mut xfade = CrossfadeTransition::new().with_duration_ms(100);
        let section = SectionId::new();
        let source = make_snapshot("A", &[(section, None)]);
        let target = make_snapshot("B", &[(section, None)]);

        xfade.start(source, target);

        // Tick past the end
        let state = xfade.tick(150.0);
        assert_eq!(state.phase, CrossfadePhase::Complete);
        assert!((state.progress - 1.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_transition_gains_at_start() {
        let mut xfade = CrossfadeTransition::new()
            .with_duration_ms(100)
            .with_curve(CrossfadeCurve::Linear);
        let section = SectionId::new();
        let source = make_snapshot("A", &[(section, None)]);
        let target = make_snapshot("B", &[(section, None)]);

        xfade.start(source, target);
        let state = xfade.current_state();

        // At t=0: source full, target silent
        assert!((state.source_gain - 1.0).abs() < 1e-10);
        assert!(state.target_gain.abs() < 1e-10);
    }

    #[test]
    fn test_transition_gains_at_end() {
        let mut xfade = CrossfadeTransition::new()
            .with_duration_ms(100)
            .with_curve(CrossfadeCurve::Linear);
        let section = SectionId::new();
        let source = make_snapshot("A", &[(section, None)]);
        let target = make_snapshot("B", &[(section, None)]);

        xfade.start(source, target);
        xfade.tick(100.0);
        let state = xfade.current_state();

        // At t=1: source silent, target full
        assert!(state.source_gain.abs() < 1e-10);
        assert!((state.target_gain - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_transition_equal_power_gains_midpoint() {
        let mut xfade = CrossfadeTransition::new()
            .with_duration_ms(100)
            .with_curve(CrossfadeCurve::EqualPower);
        let section = SectionId::new();
        let source = make_snapshot("A", &[(section, None)]);
        let target = make_snapshot("B", &[(section, None)]);

        xfade.start(source, target);
        xfade.tick(50.0);
        let state = xfade.current_state();

        // Equal power at midpoint: both at sqrt(0.5)
        let expected = 0.5_f64.sqrt();
        assert!((state.source_gain - expected).abs() < 1e-10);
        assert!((state.target_gain - expected).abs() < 1e-10);
    }

    // -- Module transitions

    #[test]
    fn test_transition_shared_modules() {
        let mut xfade = CrossfadeTransition::new().with_duration_ms(100);
        let section = SectionId::new();
        // Same section in both = shared module
        let source = make_snapshot("A", &[(section, None)]);
        let target = make_snapshot("B", &[(section, None)]);

        xfade.start(source, target);
        let state = xfade.tick(50.0);

        assert_eq!(state.module_transitions.len(), 1);
        match &state.module_transitions[0] {
            ModuleTransition::Morph { module_id, gain } => {
                assert_eq!(*module_id, section.as_uuid());
                assert!((gain - 1.0).abs() < f64::EPSILON);
            }
            other => panic!("Expected Morph, got {other:?}"),
        }
    }

    #[test]
    fn test_transition_new_module_fades_in() {
        let mut xfade = CrossfadeTransition::new()
            .with_duration_ms(100)
            .with_curve(CrossfadeCurve::Linear);
        let section_shared = SectionId::new();
        let section_new = SectionId::new();

        let source = make_snapshot("A", &[(section_shared, None)]);
        let target = make_snapshot("B", &[(section_shared, None), (section_new, None)]);

        xfade.start(source, target);
        let state = xfade.tick(50.0);

        let new_module = state
            .module_transitions
            .iter()
            .find(|m| m.module_id() == section_new.as_uuid())
            .expect("New module should have a FadeIn transition");

        match new_module {
            ModuleTransition::FadeIn { gain, .. } => {
                assert!((gain - 0.5).abs() < 1e-10);
            }
            other => panic!("Expected FadeIn, got {other:?}"),
        }
    }

    #[test]
    fn test_transition_removed_module_fades_out() {
        let mut xfade = CrossfadeTransition::new()
            .with_duration_ms(100)
            .with_curve(CrossfadeCurve::Linear);
        let section_shared = SectionId::new();
        let section_removed = SectionId::new();

        let source = make_snapshot("A", &[(section_shared, None), (section_removed, None)]);
        let target = make_snapshot("B", &[(section_shared, None)]);

        xfade.start(source, target);
        let state = xfade.tick(50.0);

        let removed_module = state
            .module_transitions
            .iter()
            .find(|m| m.module_id() == section_removed.as_uuid())
            .expect("Removed module should have a FadeOut transition");

        match removed_module {
            ModuleTransition::FadeOut { gain, .. } => {
                // Linear complement at t=0.5 = 0.5
                assert!((gain - 0.5).abs() < 1e-10);
            }
            other => panic!("Expected FadeOut, got {other:?}"),
        }
    }

    // -- Reset

    #[test]
    fn test_transition_reset() {
        let mut xfade = CrossfadeTransition::new().with_duration_ms(100);
        let section = SectionId::new();
        let source = make_snapshot("A", &[(section, None)]);
        let target = make_snapshot("B", &[(section, None)]);

        xfade.start(source, target);
        xfade.tick(50.0);
        assert!(xfade.is_active());

        xfade.reset();
        assert_eq!(xfade.phase(), CrossfadePhase::Idle);
        assert!(!xfade.is_active());
        assert!(xfade.source().is_none());
        assert!(xfade.target().is_none());
    }

    // -- Idle tick does nothing

    #[test]
    fn test_idle_tick_noop() {
        let mut xfade = CrossfadeTransition::new();
        let state = xfade.tick(100.0);
        assert_eq!(state.phase, CrossfadePhase::Idle);
        assert!((state.progress).abs() < f64::EPSILON);
    }

    // -- Morph during crossfade

    #[test]
    fn test_transition_morphs_shared_parameters() -> Result<()> {
        // -- Setup & Fixtures
        let mut xfade = CrossfadeTransition::new().with_duration_ms(100);
        let section = SectionId::new();
        let block_id = crate::id::BlockId::new();

        let mut source = make_snapshot("Clean", &[(section, None)]);
        source.add_block_override(
            crate::preset::block_override::BlockOverride::new(block_id)
                .with_param("drive", NormalizedF64::new(0.2)),
        );

        let mut target = make_snapshot("Dirty", &[(section, None)]);
        target.add_block_override(
            crate::preset::block_override::BlockOverride::new(block_id)
                .with_param("drive", NormalizedF64::new(0.8)),
        );

        // -- Exec
        xfade.start(source, target);
        let state = xfade.tick(50.0); // midpoint

        // -- Check
        let morph = state.morph_result.expect("Should have morph result for shared modules");
        assert_eq!(morph.block_overrides.len(), 1);
        let drive = morph.block_overrides[0]
            .parameter_value("drive")
            .expect("drive param");
        assert!((drive.get() - 0.5).abs() < 1e-10);
        Ok(())
    }
}
