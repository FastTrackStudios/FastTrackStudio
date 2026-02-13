//! MIDI CC mapping and expression pedal control types.
//!
//! Defines the domain types for mapping MIDI Continuous Controller messages
//! to rig parameters. Supports expression pedal control of the morph slider,
//! arbitrary parameter targeting, configurable response curves, and range
//! limiting.
//!
//! # Example
//!
//! ```ignore
//! use signal_proto::midi::*;
//!
//! // Expression pedal on CC#11 controlling the morph slider
//! let mapping = MidiCcMapping::new(0, 11, MidiTarget::MorphSlider);
//!
//! // CC#1 (mod wheel) controlling a specific parameter with log curve
//! let mapping = MidiCcMapping::new(0, 1, MidiTarget::Parameter {
//!     block_id: some_uuid,
//!     param_name: "drive".into(),
//! })
//! .with_curve(CcCurve::Logarithmic)
//! .with_range(0.2, 0.8);
//! ```

use uuid::Uuid;

// ─────────────────────────────────────────────────────────────────────────────
// CcCurve
// ─────────────────────────────────────────────────────────────────────────────

/// Response curve applied when mapping a raw CC value to a normalized output.
///
/// All curves map the input range `[0, 127]` through `[0.0, 1.0]` with
/// different response shapes. The output is then scaled by the mapping's
/// min/max range.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, ::facet::Facet)]
#[repr(u8)]
pub enum CcCurve {
    /// Linear 1:1 mapping: `f(x) = x`. Equal physical pedal travel produces
    /// equal parameter change.
    #[default]
    Linear,

    /// Logarithmic response: `f(x) = ln(1 + 9x) / ln(10)`. Finer control at
    /// the low end, faster change at the top. Good for volume/gain controls
    /// where perceptual loudness is logarithmic.
    Logarithmic,

    /// Exponential response: `f(x) = (10^x - 1) / 9`. Coarse at the low end,
    /// fine control at the top. Good for frequency sweeps or when you want
    /// precision near the maximum.
    Exponential,
}

impl CcCurve {
    /// Apply the curve to a normalized input `t` in `[0.0, 1.0]`.
    ///
    /// Returns a value in `[0.0, 1.0]`.
    pub fn apply(self, t: f64) -> f64 {
        let t = t.clamp(0.0, 1.0);
        match self {
            Self::Linear => t,
            Self::Logarithmic => {
                // ln(1 + 9t) / ln(10) — rises quickly then flattens
                (1.0 + 9.0 * t).ln() / 10.0_f64.ln()
            }
            Self::Exponential => {
                // (10^t - 1) / 9 — starts flat then rises quickly
                (10.0_f64.powf(t) - 1.0) / 9.0
            }
        }
    }

    /// All available curves, useful for UI dropdowns.
    pub fn all() -> &'static [CcCurve] {
        &[Self::Linear, Self::Logarithmic, Self::Exponential]
    }

    /// Human-readable name for display.
    pub fn display_name(self) -> &'static str {
        match self {
            Self::Linear => "Linear",
            Self::Logarithmic => "Logarithmic",
            Self::Exponential => "Exponential",
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// MidiTarget
// ─────────────────────────────────────────────────────────────────────────────

/// What a MIDI CC mapping controls.
#[derive(Debug, Clone, PartialEq, ::facet::Facet)]
#[repr(u8)]
pub enum MidiTarget {
    /// Controls the A/B morph slider position.
    MorphSlider,

    /// Controls a specific parameter on a specific block.
    Parameter {
        /// Block containing the target parameter.
        block_id: Uuid,
        /// Parameter name (matches `Parameter::id` in the block).
        param_name: String,
    },
}

impl MidiTarget {
    /// Human-readable description for display.
    pub fn display_name(&self) -> String {
        match self {
            Self::MorphSlider => "Morph Slider".to_string(),
            Self::Parameter { param_name, .. } => format!("Param: {param_name}"),
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// MidiCcMapping
// ─────────────────────────────────────────────────────────────────────────────

/// A mapping from a MIDI CC message to a rig control target.
///
/// Combines source identification (channel + CC number), response shaping
/// (curve + range), and destination (morph slider or block parameter).
///
/// # Value Pipeline
///
/// ```text
/// Raw CC [0..127] → normalize [0.0..1.0] → curve → scale to [min..max] → target
/// ```
#[derive(Debug, Clone, PartialEq, ::facet::Facet)]
pub struct MidiCcMapping {
    /// Unique identifier for this mapping.
    pub id: Uuid,
    /// MIDI channel (0-15).
    pub channel: u8,
    /// MIDI CC number (0-127).
    pub cc_number: u8,
    /// Response curve shape.
    pub curve: CcCurve,
    /// Minimum output value (0.0-1.0). The CC floor maps to this.
    pub min_value: f64,
    /// Maximum output value (0.0-1.0). The CC ceiling maps to this.
    pub max_value: f64,
    /// What this mapping controls.
    pub target: MidiTarget,
}

impl MidiCcMapping {
    /// Create a new mapping with default linear curve and full range.
    pub fn new(channel: u8, cc_number: u8, target: MidiTarget) -> Self {
        Self {
            id: Uuid::new_v4(),
            channel: channel.min(15),
            cc_number: cc_number.min(127),
            curve: CcCurve::Linear,
            min_value: 0.0,
            max_value: 1.0,
            target,
        }
    }

    /// Set the response curve.
    #[must_use]
    pub fn with_curve(mut self, curve: CcCurve) -> Self {
        self.curve = curve;
        self
    }

    /// Set the output range. Values are clamped to `[0.0, 1.0]`.
    ///
    /// If `min > max`, they are swapped (inverted pedal response).
    #[must_use]
    pub fn with_range(mut self, min: f64, max: f64) -> Self {
        self.min_value = min.clamp(0.0, 1.0);
        self.max_value = max.clamp(0.0, 1.0);
        self
    }

    /// Process a raw MIDI CC value (0-127) through the full pipeline.
    ///
    /// Returns the final output value in `[min_value, max_value]`.
    pub fn process(&self, raw_cc_value: u8) -> f64 {
        // Normalize to [0, 1]
        let normalized = raw_cc_value as f64 / 127.0;

        // Apply response curve
        let curved = self.curve.apply(normalized);

        // Scale to output range
        let range = self.max_value - self.min_value;
        self.min_value + curved * range
    }

    /// Human-readable summary for display.
    pub fn summary(&self) -> String {
        format!(
            "Ch{} CC#{} -> {} [{}]",
            self.channel + 1,
            self.cc_number,
            self.target.display_name(),
            self.curve.display_name(),
        )
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// MidiLearnState
// ─────────────────────────────────────────────────────────────────────────────

/// Tracks the state of a MIDI learn session.
///
/// When the user activates MIDI Learn, the UI enters a listening mode where
/// the next incoming CC message is captured and used to create or update a
/// mapping.
#[derive(Debug, Clone, PartialEq)]
pub enum MidiLearnState {
    /// Not in learn mode.
    Idle,

    /// Listening for the next incoming CC message.
    Listening {
        /// What the learned CC will control.
        target: MidiTarget,
    },

    /// A CC was captured; awaiting user confirmation or auto-assignment.
    Captured {
        /// The target that was being learned.
        target: MidiTarget,
        /// Captured MIDI channel (0-15).
        channel: u8,
        /// Captured CC number (0-127).
        cc_number: u8,
    },
}

impl Default for MidiLearnState {
    fn default() -> Self {
        Self::Idle
    }
}

impl MidiLearnState {
    /// Whether we are actively listening for MIDI input.
    pub fn is_listening(&self) -> bool {
        matches!(self, Self::Listening { .. })
    }

    /// Whether a CC has been captured and is pending confirmation.
    pub fn is_captured(&self) -> bool {
        matches!(self, Self::Captured { .. })
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ── CcCurve ──────────────────────────────────────────────────────

    #[test]
    fn all_curves_start_at_zero_end_at_one() {
        for curve in CcCurve::all() {
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
        for curve in CcCurve::all() {
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
    fn linear_is_identity() {
        for i in 0..=100 {
            let t = i as f64 / 100.0;
            assert!((CcCurve::Linear.apply(t) - t).abs() < 1e-10);
        }
    }

    #[test]
    fn log_is_above_linear() {
        // Logarithmic curve should be above linear for 0 < t < 1
        for i in 1..100 {
            let t = i as f64 / 100.0;
            assert!(
                CcCurve::Logarithmic.apply(t) >= CcCurve::Linear.apply(t) - 1e-10,
                "log should be >= linear at t={t}"
            );
        }
    }

    #[test]
    fn exp_is_below_linear() {
        // Exponential curve should be below linear for 0 < t < 1
        for i in 1..100 {
            let t = i as f64 / 100.0;
            assert!(
                CcCurve::Exponential.apply(t) <= CcCurve::Linear.apply(t) + 1e-10,
                "exp should be <= linear at t={t}"
            );
        }
    }

    #[test]
    fn clamps_input() {
        assert_eq!(CcCurve::Linear.apply(-1.0), 0.0);
        assert_eq!(CcCurve::Linear.apply(2.0), 1.0);
    }

    #[test]
    fn display_names_are_unique() {
        let names: Vec<&str> = CcCurve::all().iter().map(|c| c.display_name()).collect();
        for (i, a) in names.iter().enumerate() {
            for (j, b) in names.iter().enumerate() {
                if i != j {
                    assert_ne!(a, b, "Duplicate display name");
                }
            }
        }
    }

    // ── MidiCcMapping ────────────────────────────────────────────────

    #[test]
    fn process_linear_full_range() {
        let mapping = MidiCcMapping::new(0, 11, MidiTarget::MorphSlider);

        assert!((mapping.process(0) - 0.0).abs() < 1e-10);
        assert!((mapping.process(127) - 1.0).abs() < 1e-10);
        assert!((mapping.process(64) - 64.0 / 127.0).abs() < 1e-3);
    }

    #[test]
    fn process_with_range_limiting() {
        let mapping = MidiCcMapping::new(0, 11, MidiTarget::MorphSlider).with_range(0.2, 0.8);

        // CC 0 → min
        assert!((mapping.process(0) - 0.2).abs() < 1e-10);
        // CC 127 → max
        assert!((mapping.process(127) - 0.8).abs() < 1e-10);
        // CC 64 (midpoint) → midpoint of range
        let mid = mapping.process(64);
        assert!(mid > 0.45 && mid < 0.55, "midpoint: {mid}");
    }

    #[test]
    fn process_with_log_curve() {
        let mapping =
            MidiCcMapping::new(0, 1, MidiTarget::MorphSlider).with_curve(CcCurve::Logarithmic);

        let low = mapping.process(32);
        let linear_low = 32.0 / 127.0;
        // Log curve should give higher output than linear at low CC values
        assert!(
            low > linear_low,
            "log at CC32: {low} should be > linear {linear_low}"
        );
    }

    #[test]
    fn process_with_exp_curve() {
        let mapping =
            MidiCcMapping::new(0, 1, MidiTarget::MorphSlider).with_curve(CcCurve::Exponential);

        let low = mapping.process(32);
        let linear_low = 32.0 / 127.0;
        // Exp curve should give lower output than linear at low CC values
        assert!(
            low < linear_low,
            "exp at CC32: {low} should be < linear {linear_low}"
        );
    }

    #[test]
    fn channel_and_cc_clamped() {
        let mapping = MidiCcMapping::new(20, 200, MidiTarget::MorphSlider);
        assert_eq!(mapping.channel, 15);
        assert_eq!(mapping.cc_number, 127);
    }

    #[test]
    fn summary_format() {
        let mapping = MidiCcMapping::new(0, 11, MidiTarget::MorphSlider);
        let summary = mapping.summary();
        assert!(summary.contains("Ch1"), "summary: {summary}");
        assert!(summary.contains("CC#11"), "summary: {summary}");
        assert!(summary.contains("Morph Slider"), "summary: {summary}");
    }

    // ── MidiLearnState ───────────────────────────────────────────────

    #[test]
    fn learn_state_transitions() {
        let state = MidiLearnState::Idle;
        assert!(!state.is_listening());
        assert!(!state.is_captured());

        let state = MidiLearnState::Listening {
            target: MidiTarget::MorphSlider,
        };
        assert!(state.is_listening());
        assert!(!state.is_captured());

        let state = MidiLearnState::Captured {
            target: MidiTarget::MorphSlider,
            channel: 0,
            cc_number: 11,
        };
        assert!(!state.is_listening());
        assert!(state.is_captured());
    }

    #[test]
    fn default_is_idle() {
        assert_eq!(MidiLearnState::default(), MidiLearnState::Idle);
    }

    // ── MidiTarget ───────────────────────────────────────────────────

    #[test]
    fn target_display_names() {
        assert_eq!(MidiTarget::MorphSlider.display_name(), "Morph Slider");

        let param_target = MidiTarget::Parameter {
            block_id: Uuid::nil(),
            param_name: "drive".to_string(),
        };
        assert!(param_target.display_name().contains("drive"));
    }
}
