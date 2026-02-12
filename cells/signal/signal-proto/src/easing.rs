//! Easing curves and time-based snapshot tweening.
//!
//! [`EasingCurve`] provides a library of standard easing functions that map
//! a linear progress `t ∈ [0.0, 1.0]` to a curved output. These extend the
//! simpler [`CrossfadeCurve`](crate::crossfade::CrossfadeCurve) with the full
//! set of easing functions familiar from CSS/animation.
//!
//! [`SnapshotTween`] wraps a duration + curve into a time-based animation.
//! Call [`advance(delta_ms)`](SnapshotTween::advance) each frame to get the
//! eased position, then feed it into [`SnapshotMorpher::compute_at()`](crate::morph::SnapshotMorpher::compute_at).


// ─────────────────────────────────────────────────────────────────────────────
// EasingCurve
// ─────────────────────────────────────────────────────────────────────────────

/// Standard easing functions for animation and transitions.
///
/// Each variant maps a linear `t ∈ [0.0, 1.0]` to a curved output in the
/// same range. "In" variants start slow, "Out" variants end slow, and
/// "InOut" variants do both.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, ::facet::Facet)]
#[repr(u8)]
pub enum EasingCurve {
    /// No easing: `f(t) = t`.
    Linear,
    /// Quadratic ease-in: `f(t) = t²`. Starts slow, accelerates.
    EaseIn,
    /// Quadratic ease-out: `f(t) = 1 - (1-t)²`. Starts fast, decelerates.
    EaseOut,
    /// Quadratic ease-in-out: smooth S-curve (cubic Hermite `3t² - 2t³`).
    #[default]
    EaseInOut,
    /// Cubic ease-in: `f(t) = t³`. More pronounced slow start.
    EaseInCubic,
    /// Cubic ease-out: `f(t) = 1 - (1-t)³`. More pronounced deceleration.
    EaseOutCubic,
    /// Cubic ease-in-out: smoother S with steeper midpoint.
    EaseInOutCubic,
    /// Exponential ease-in: `f(t) = 2^(10(t-1))`. Very slow start.
    EaseInExpo,
    /// Exponential ease-out: `f(t) = 1 - 2^(-10t)`. Very slow end.
    EaseOutExpo,
    /// Instant snap to target at `t = 0`. Useful for toggle-style changes.
    Snap,
}

impl EasingCurve {
    /// Evaluate the easing function at position `t ∈ [0.0, 1.0]`.
    ///
    /// Returns the eased value, also in `[0.0, 1.0]`.
    pub fn apply(self, t: f64) -> f64 {
        let t = t.clamp(0.0, 1.0);
        match self {
            Self::Linear => t,
            Self::EaseIn => t * t,
            Self::EaseOut => {
                let inv = 1.0 - t;
                1.0 - inv * inv
            }
            Self::EaseInOut => 3.0 * t * t - 2.0 * t * t * t,
            Self::EaseInCubic => t * t * t,
            Self::EaseOutCubic => {
                let inv = 1.0 - t;
                1.0 - inv * inv * inv
            }
            Self::EaseInOutCubic => {
                if t < 0.5 {
                    4.0 * t * t * t
                } else {
                    let inv = -2.0 * t + 2.0;
                    1.0 - inv * inv * inv / 2.0
                }
            }
            Self::EaseInExpo => {
                if t <= 0.0 {
                    0.0
                } else {
                    f64::powf(2.0, 10.0 * (t - 1.0))
                }
            }
            Self::EaseOutExpo => {
                if t >= 1.0 {
                    1.0
                } else {
                    1.0 - f64::powf(2.0, -10.0 * t)
                }
            }
            Self::Snap => 1.0,
        }
    }

    /// All available curves, useful for UI dropdowns.
    pub fn all() -> &'static [EasingCurve] {
        &[
            Self::Linear,
            Self::EaseIn,
            Self::EaseOut,
            Self::EaseInOut,
            Self::EaseInCubic,
            Self::EaseOutCubic,
            Self::EaseInOutCubic,
            Self::EaseInExpo,
            Self::EaseOutExpo,
            Self::Snap,
        ]
    }

    /// Human-readable name for display.
    pub fn display_name(self) -> &'static str {
        match self {
            Self::Linear => "Linear",
            Self::EaseIn => "Ease In",
            Self::EaseOut => "Ease Out",
            Self::EaseInOut => "Ease In/Out",
            Self::EaseInCubic => "Ease In (Cubic)",
            Self::EaseOutCubic => "Ease Out (Cubic)",
            Self::EaseInOutCubic => "Ease In/Out (Cubic)",
            Self::EaseInExpo => "Ease In (Expo)",
            Self::EaseOutExpo => "Ease Out (Expo)",
            Self::Snap => "Snap",
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// TweenState
// ─────────────────────────────────────────────────────────────────────────────

/// Lifecycle state of a [`SnapshotTween`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, ::facet::Facet)]
#[repr(u8)]
pub enum TweenState {
    /// Not yet started.
    Idle,
    /// Actively animating.
    Running,
    /// Reached the end of the duration.
    Complete,
}

// ─────────────────────────────────────────────────────────────────────────────
// SnapshotTween
// ─────────────────────────────────────────────────────────────────────────────

/// Time-based animated transition between two snapshot positions.
///
/// Wraps a duration and easing curve. Each frame, call [`advance`](Self::advance)
/// with the elapsed delta to get the current eased morph position. Feed this
/// into [`SnapshotMorpher::compute_at()`](crate::morph::SnapshotMorpher::compute_at)
/// for the interpolated parameter values.
///
/// # Example (pseudocode)
///
/// ```ignore
/// let mut tween = SnapshotTween::new(200.0, EasingCurve::EaseInOut);
/// tween.start();
///
/// // Each frame:
/// let eased = tween.advance(delta_ms);
/// let result = morpher.compute_at(eased);
/// apply_to_dsp(result);
/// ```
#[derive(Debug, Clone, ::facet::Facet)]
pub struct SnapshotTween {
    /// Total duration in milliseconds.
    pub duration_ms: f64,
    /// Easing curve shape.
    pub curve: EasingCurve,
    /// Elapsed time in milliseconds.
    elapsed_ms: f64,
    /// Current lifecycle state.
    state: TweenState,
}

impl SnapshotTween {
    /// Create a new tween. Starts in [`TweenState::Idle`].
    pub fn new(duration_ms: f64, curve: EasingCurve) -> Self {
        Self {
            duration_ms: duration_ms.max(0.0),
            curve,
            elapsed_ms: 0.0,
            state: TweenState::Idle,
        }
    }

    /// Begin the tween, resetting elapsed time and setting state to Running.
    pub fn start(&mut self) {
        self.elapsed_ms = 0.0;
        self.state = TweenState::Running;
    }

    /// Advance the tween by `delta_ms` and return the current eased position.
    ///
    /// Returns `0.0` if Idle, the eased value if Running, or `1.0` if Complete.
    pub fn advance(&mut self, delta_ms: f64) -> f64 {
        match self.state {
            TweenState::Idle => 0.0,
            TweenState::Complete => 1.0,
            TweenState::Running => {
                self.elapsed_ms += delta_ms;

                if self.duration_ms <= 0.0 || self.elapsed_ms >= self.duration_ms {
                    self.elapsed_ms = self.duration_ms;
                    self.state = TweenState::Complete;
                    1.0
                } else {
                    let linear_t = self.elapsed_ms / self.duration_ms;
                    self.curve.apply(linear_t)
                }
            }
        }
    }

    /// Whether the tween has finished.
    pub fn is_complete(&self) -> bool {
        self.state == TweenState::Complete
    }

    /// Whether the tween is actively running.
    pub fn is_running(&self) -> bool {
        self.state == TweenState::Running
    }

    /// Current lifecycle state.
    pub fn state(&self) -> TweenState {
        self.state
    }

    /// Elapsed time in milliseconds.
    pub fn elapsed_ms(&self) -> f64 {
        self.elapsed_ms
    }

    /// Current linear progress (not eased) in [0.0, 1.0].
    pub fn linear_progress(&self) -> f64 {
        if self.duration_ms <= 0.0 {
            return if self.state == TweenState::Idle {
                0.0
            } else {
                1.0
            };
        }
        (self.elapsed_ms / self.duration_ms).clamp(0.0, 1.0)
    }

    /// Reset to Idle with zero elapsed time.
    pub fn reset(&mut self) {
        self.elapsed_ms = 0.0;
        self.state = TweenState::Idle;
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ── EasingCurve ──────────────────────────────────────────────────

    #[test]
    fn all_curves_start_at_zero_end_at_one() {
        for curve in EasingCurve::all() {
            if *curve == EasingCurve::Snap {
                // Snap is always 1.0
                assert_eq!(curve.apply(0.0), 1.0, "{:?} at t=0", curve);
                assert_eq!(curve.apply(1.0), 1.0, "{:?} at t=1", curve);
                continue;
            }
            let at_zero = curve.apply(0.0);
            let at_one = curve.apply(1.0);
            assert!(at_zero.abs() < 1e-10, "{:?}: apply(0.0) = {at_zero}", curve);
            assert!(
                (at_one - 1.0).abs() < 1e-10,
                "{:?}: apply(1.0) = {at_one}",
                curve
            );
        }
    }

    #[test]
    fn all_curves_monotonically_increase() {
        for curve in EasingCurve::all() {
            let mut prev = curve.apply(0.0);
            for i in 1..=100 {
                let t = i as f64 / 100.0;
                let val = curve.apply(t);
                assert!(
                    val >= prev - 1e-10,
                    "{:?}: not monotonic at t={t}: {prev} -> {val}",
                    curve
                );
                prev = val;
            }
        }
    }

    #[test]
    fn clamps_input() {
        assert_eq!(EasingCurve::Linear.apply(-1.0), 0.0);
        assert_eq!(EasingCurve::Linear.apply(2.0), 1.0);
    }

    #[test]
    fn ease_in_is_below_linear() {
        for i in 1..10 {
            let t = i as f64 / 10.0;
            assert!(
                EasingCurve::EaseIn.apply(t) <= EasingCurve::Linear.apply(t) + 1e-10,
                "EaseIn should be <= Linear at t={t}"
            );
        }
    }

    #[test]
    fn ease_out_is_above_linear() {
        for i in 1..10 {
            let t = i as f64 / 10.0;
            assert!(
                EasingCurve::EaseOut.apply(t) >= EasingCurve::Linear.apply(t) - 1e-10,
                "EaseOut should be >= Linear at t={t}"
            );
        }
    }

    #[test]
    fn ease_in_out_midpoint_is_half() {
        let val = EasingCurve::EaseInOut.apply(0.5);
        assert!(
            (val - 0.5).abs() < 1e-10,
            "EaseInOut(0.5) = {val}, expected 0.5"
        );
    }

    #[test]
    fn ease_in_out_cubic_midpoint_is_half() {
        let val = EasingCurve::EaseInOutCubic.apply(0.5);
        assert!(
            (val - 0.5).abs() < 1e-10,
            "EaseInOutCubic(0.5) = {val}, expected 0.5"
        );
    }

    #[test]
    fn display_names_are_unique() {
        let names: Vec<&str> = EasingCurve::all()
            .iter()
            .map(|c| c.display_name())
            .collect();
        for (i, a) in names.iter().enumerate() {
            for (j, b) in names.iter().enumerate() {
                if i != j {
                    assert_ne!(a, b, "Duplicate display name");
                }
            }
        }
    }

    // ── SnapshotTween ────────────────────────────────────────────────

    #[test]
    fn idle_tween_returns_zero() {
        let mut tween = SnapshotTween::new(100.0, EasingCurve::Linear);
        assert_eq!(tween.advance(50.0), 0.0);
        assert_eq!(tween.state(), TweenState::Idle);
    }

    #[test]
    fn start_then_advance() {
        let mut tween = SnapshotTween::new(100.0, EasingCurve::Linear);
        tween.start();
        assert!(tween.is_running());

        let val = tween.advance(50.0);
        assert!((val - 0.5).abs() < 1e-10);
        assert!(tween.is_running());
    }

    #[test]
    fn completes_at_duration() {
        let mut tween = SnapshotTween::new(100.0, EasingCurve::Linear);
        tween.start();
        let val = tween.advance(100.0);
        assert_eq!(val, 1.0);
        assert!(tween.is_complete());
    }

    #[test]
    fn overshooting_duration_clamps() {
        let mut tween = SnapshotTween::new(100.0, EasingCurve::Linear);
        tween.start();
        let val = tween.advance(200.0);
        assert_eq!(val, 1.0);
        assert!(tween.is_complete());
    }

    #[test]
    fn complete_tween_always_returns_one() {
        let mut tween = SnapshotTween::new(100.0, EasingCurve::Linear);
        tween.start();
        tween.advance(100.0);
        assert_eq!(tween.advance(50.0), 1.0);
    }

    #[test]
    fn zero_duration_completes_immediately() {
        let mut tween = SnapshotTween::new(0.0, EasingCurve::EaseIn);
        tween.start();
        let val = tween.advance(0.0);
        assert_eq!(val, 1.0);
        assert!(tween.is_complete());
    }

    #[test]
    fn reset_returns_to_idle() {
        let mut tween = SnapshotTween::new(100.0, EasingCurve::Linear);
        tween.start();
        tween.advance(50.0);
        tween.reset();
        assert_eq!(tween.state(), TweenState::Idle);
        assert_eq!(tween.elapsed_ms(), 0.0);
    }

    #[test]
    fn eased_curve_applied_during_advance() {
        let mut tween = SnapshotTween::new(100.0, EasingCurve::EaseIn);
        tween.start();
        let val = tween.advance(50.0);
        // EaseIn(0.5) = 0.5² = 0.25
        assert!((val - 0.25).abs() < 1e-10, "got {val}");
    }

    #[test]
    fn linear_progress_tracks_time() {
        let mut tween = SnapshotTween::new(200.0, EasingCurve::EaseIn);
        tween.start();
        tween.advance(100.0);
        assert!((tween.linear_progress() - 0.5).abs() < 1e-10);
    }

    #[test]
    fn incremental_advance() {
        let mut tween = SnapshotTween::new(100.0, EasingCurve::Linear);
        tween.start();
        tween.advance(25.0);
        tween.advance(25.0);
        let val = tween.advance(0.0);
        assert!((val - 0.5).abs() < 1e-10, "got {val}");
    }
}
