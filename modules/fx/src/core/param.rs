//! Parameter system with ranges, smoothing, and modulation.
//!
//! This module provides the [`Parameter`] type which represents a controllable
//! value in an effect block.
//!
//! ## Features
//!
//! - **Ranges**: Define min/max bounds with optional skew for logarithmic parameters
//! - **Smoothing**: Built-in smoothing to prevent zipper noise
//! - **Modulation**: Connect modulation sources (LFOs, envelopes) to parameters
//! - **Normalization**: Convert between normalized (0-1) and actual values
//!
//! ## Example
//!
//! ```ignore
//! use fts_fx::core::{Parameter, Range, Skew};
//!
//! // Create a frequency parameter (20Hz - 20kHz, logarithmic)
//! let range = Range::new(20.0, 20000.0).with_skew(Skew::Log);
//! let mut freq = Parameter::new(range, 1000.0, 44100.0, 5.0);
//!
//! // Set value (will be smoothed)
//! freq.set(2000.0);
//!
//! // Get smoothed value each sample
//! let current = freq.next();
//! ```

use super::modulation::ModSlots;
use crate::primitives::Smoother;

/// Skew type for parameter ranges.
///
/// Determines how normalized values (0-1) map to actual values.
#[derive(Clone, Copy, Debug, PartialEq, Default)]
pub enum Skew {
    /// Linear mapping (default)
    #[default]
    Linear,
    /// Logarithmic mapping - good for frequencies, times
    Log,
    /// Exponential mapping - inverse of log
    Exp,
    /// Symmetric around center - good for pan, pitch
    Symmetric,
    /// Custom skew factor (>1 = log-like, <1 = exp-like)
    Factor(f32),
}

impl Skew {
    /// Apply skew to a normalized value (0-1) → (0-1).
    #[must_use]
    pub fn apply(&self, normalized: f32) -> f32 {
        let n = normalized.clamp(0.0, 1.0);
        match self {
            Skew::Linear => n,
            Skew::Log => {
                // Attempt to handle log(0) gracefully
                if n <= 0.0 {
                    0.0
                } else {
                    // Map through log scale
                    let min_log = 0.001_f32.ln();
                    let max_log = 1.0_f32.ln();
                    let log_val = min_log + n * (max_log - min_log);
                    (log_val.exp() - 0.001) / (1.0 - 0.001)
                }
            }
            Skew::Exp => {
                // Inverse of log
                n * n
            }
            Skew::Symmetric => {
                // S-curve centered at 0.5
                if n < 0.5 {
                    0.5 - 0.5 * (1.0 - 2.0 * n).sqrt()
                } else {
                    0.5 + 0.5 * (2.0 * n - 1.0).sqrt()
                }
            }
            Skew::Factor(factor) => n.powf(*factor),
        }
    }

    /// Reverse skew from a normalized value (0-1) → (0-1).
    #[must_use]
    pub fn unapply(&self, skewed: f32) -> f32 {
        let s = skewed.clamp(0.0, 1.0);
        match self {
            Skew::Linear => s,
            Skew::Log => {
                if s <= 0.0 {
                    0.0
                } else {
                    let min_log = 0.001_f32.ln();
                    let max_log = 1.0_f32.ln();
                    let val = s * (1.0 - 0.001) + 0.001;
                    (val.ln() - min_log) / (max_log - min_log)
                }
            }
            Skew::Exp => s.sqrt(),
            Skew::Symmetric => {
                if s < 0.5 {
                    0.5 - 0.5 * (1.0 - 2.0 * s).powi(2)
                } else {
                    0.5 + 0.5 * (2.0 * s - 1.0).powi(2)
                }
            }
            Skew::Factor(factor) => {
                if *factor == 0.0 {
                    s
                } else {
                    s.powf(1.0 / *factor)
                }
            }
        }
    }
}

/// Parameter range definition.
#[derive(Clone, Copy, Debug)]
pub struct Range {
    /// Minimum value
    pub min: f32,
    /// Maximum value
    pub max: f32,
    /// Default value
    pub default: f32,
    /// Skew type for non-linear mapping
    pub skew: Skew,
}

impl Range {
    /// Create a new linear range.
    #[must_use]
    pub fn new(min: f32, max: f32) -> Self {
        Self {
            min,
            max,
            default: min,
            skew: Skew::Linear,
        }
    }

    /// Create a range with a default value.
    #[must_use]
    pub fn with_default(mut self, default: f32) -> Self {
        self.default = default.clamp(self.min, self.max);
        self
    }

    /// Set the skew type.
    #[must_use]
    pub fn with_skew(mut self, skew: Skew) -> Self {
        self.skew = skew;
        self
    }

    /// Create a frequency range (20Hz - 20kHz, log scale).
    #[must_use]
    pub fn frequency() -> Self {
        Self::new(20.0, 20000.0)
            .with_default(1000.0)
            .with_skew(Skew::Log)
    }

    /// Create a gain range in dB (-60 to +12).
    #[must_use]
    pub fn gain_db() -> Self {
        Self::new(-60.0, 12.0).with_default(0.0)
    }

    /// Create a percentage range (0-100).
    #[must_use]
    pub fn percentage() -> Self {
        Self::new(0.0, 100.0).with_default(50.0)
    }

    /// Create a 0-1 normalized range.
    #[must_use]
    pub fn normalized() -> Self {
        Self::new(0.0, 1.0).with_default(0.5)
    }

    /// Create a pan range (-1 to +1).
    #[must_use]
    pub fn pan() -> Self {
        Self::new(-1.0, 1.0)
            .with_default(0.0)
            .with_skew(Skew::Symmetric)
    }

    /// Create a Q/resonance range.
    #[must_use]
    pub fn q() -> Self {
        Self::new(0.1, 20.0)
            .with_default(0.707)
            .with_skew(Skew::Log)
    }

    /// Create a time range in milliseconds.
    #[must_use]
    pub fn time_ms(min: f32, max: f32) -> Self {
        Self::new(min, max)
            .with_default(min)
            .with_skew(Skew::Log)
    }

    /// Convert a normalized value (0-1) to actual value.
    #[must_use]
    pub fn denormalize(&self, normalized: f32) -> f32 {
        let skewed = self.skew.apply(normalized.clamp(0.0, 1.0));
        self.min + skewed * (self.max - self.min)
    }

    /// Convert an actual value to normalized (0-1).
    #[must_use]
    pub fn normalize(&self, value: f32) -> f32 {
        if (self.max - self.min).abs() < f32::EPSILON {
            return 0.0;
        }
        let linear = (value.clamp(self.min, self.max) - self.min) / (self.max - self.min);
        self.skew.unapply(linear)
    }

    /// Clamp a value to the range.
    #[must_use]
    pub fn clamp(&self, value: f32) -> f32 {
        value.clamp(self.min, self.max)
    }

    /// Get the span of the range.
    #[must_use]
    pub fn span(&self) -> f32 {
        self.max - self.min
    }
}

impl Default for Range {
    fn default() -> Self {
        Self::normalized()
    }
}

/// A modulatable, smoothed parameter.
///
/// Parameters are the controllable values in effect blocks.
/// They support:
/// - Automatic smoothing to prevent zipper noise
/// - Modulation from multiple sources (LFOs, envelopes)
/// - Range limiting and normalization
pub struct Parameter {
    /// The parameter range
    range: Range,
    /// Base value (before modulation), normalized 0-1
    base_normalized: f32,
    /// Smoother for the base value
    smoother: Smoother,
    /// Modulation slots
    mod_slots: ModSlots,
    /// Cached actual value (after smoothing + modulation)
    cached_value: f32,
    /// Sample rate
    sample_rate: f32,
}

impl Parameter {
    /// Create a new parameter.
    ///
    /// # Arguments
    /// * `range` - The parameter range
    /// * `initial` - Initial value (in actual units, not normalized)
    /// * `sample_rate` - Sample rate in Hz
    /// * `smoothing_ms` - Smoothing time in milliseconds
    #[must_use]
    pub fn new(range: Range, initial: f32, sample_rate: f32, smoothing_ms: f32) -> Self {
        let normalized = range.normalize(initial);
        let smoother = Smoother::with_value(sample_rate, smoothing_ms, normalized);
        let cached_value = range.denormalize(normalized);

        Self {
            range,
            base_normalized: normalized,
            smoother,
            mod_slots: ModSlots::new(),
            cached_value,
            sample_rate,
        }
    }

    /// Create a parameter with default value from range.
    #[must_use]
    pub fn from_range(range: Range, sample_rate: f32, smoothing_ms: f32) -> Self {
        Self::new(range, range.default, sample_rate, smoothing_ms)
    }

    /// Set the parameter value (in actual units).
    ///
    /// The value will be smoothed over time.
    pub fn set(&mut self, value: f32) {
        self.base_normalized = self.range.normalize(value);
        self.smoother.set_target(self.base_normalized);
    }

    /// Set the normalized parameter value (0-1).
    pub fn set_normalized(&mut self, normalized: f32) {
        self.base_normalized = normalized.clamp(0.0, 1.0);
        self.smoother.set_target(self.base_normalized);
    }

    /// Set the value immediately without smoothing.
    pub fn set_immediate(&mut self, value: f32) {
        self.base_normalized = self.range.normalize(value);
        self.smoother.set_immediate(self.base_normalized);
        self.cached_value = self.range.denormalize(self.base_normalized);
    }

    /// Get the next smoothed and modulated value.
    ///
    /// Call this once per sample.
    #[must_use]
    pub fn next(&mut self) -> f32 {
        // Get smoothed base value
        let base = self.smoother.next();

        // Add modulation
        let modulated = (base + self.mod_slots.next()).clamp(0.0, 1.0);

        // Convert to actual value
        self.cached_value = self.range.denormalize(modulated);
        self.cached_value
    }

    /// Get the current value without advancing smoothers/modulators.
    #[must_use]
    pub fn current(&self) -> f32 {
        self.cached_value
    }

    /// Get the target value (ignoring smoothing and modulation).
    #[must_use]
    pub fn target(&self) -> f32 {
        self.range.denormalize(self.base_normalized)
    }

    /// Get the normalized target value.
    #[must_use]
    pub fn target_normalized(&self) -> f32 {
        self.base_normalized
    }

    /// Check if the parameter has settled (smoothing complete).
    #[must_use]
    pub fn is_settled(&self) -> bool {
        self.smoother.is_settled()
    }

    /// Get the parameter range.
    #[must_use]
    pub fn range(&self) -> &Range {
        &self.range
    }

    /// Set the smoothing time.
    pub fn set_smoothing(&mut self, smoothing_ms: f32) {
        self.smoother.set_smoothing(self.sample_rate, smoothing_ms);
    }

    /// Update the sample rate.
    pub fn set_sample_rate(&mut self, sample_rate: f32) {
        self.sample_rate = sample_rate;
        self.mod_slots.set_sample_rate(sample_rate);
        // Note: smoother would need to be recreated to change sample rate
    }

    /// Reset smoothing and modulation.
    pub fn reset(&mut self) {
        self.smoother.set_immediate(self.base_normalized);
        self.mod_slots.reset();
        self.cached_value = self.range.denormalize(self.base_normalized);
    }

    /// Get the modulation slots for this parameter.
    #[must_use]
    pub fn mod_slots(&self) -> &ModSlots {
        &self.mod_slots
    }

    /// Get mutable access to modulation slots.
    pub fn mod_slots_mut(&mut self) -> &mut ModSlots {
        &mut self.mod_slots
    }

    /// Add a modulation slot.
    pub fn add_modulation(&mut self, slot: super::modulation::ModSlot) {
        self.mod_slots.add(slot);
    }

    /// Clear all modulation.
    pub fn clear_modulation(&mut self) {
        self.mod_slots.clear();
    }
}

impl core::fmt::Debug for Parameter {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("Parameter")
            .field("range", &self.range)
            .field("base_normalized", &self.base_normalized)
            .field("cached_value", &self.cached_value)
            .field("mod_slots", &self.mod_slots.len())
            .finish()
    }
}

/// Builder for creating parameters with a fluent API.
pub struct ParameterBuilder {
    range: Range,
    initial: Option<f32>,
    smoothing_ms: f32,
}

impl ParameterBuilder {
    /// Start building a parameter with the given range.
    #[must_use]
    pub fn new(range: Range) -> Self {
        Self {
            range,
            initial: None,
            smoothing_ms: 5.0,
        }
    }

    /// Set the initial value.
    #[must_use]
    pub fn initial(mut self, value: f32) -> Self {
        self.initial = Some(value);
        self
    }

    /// Set the smoothing time in milliseconds.
    #[must_use]
    pub fn smoothing_ms(mut self, ms: f32) -> Self {
        self.smoothing_ms = ms;
        self
    }

    /// Build the parameter.
    #[must_use]
    pub fn build(self, sample_rate: f32) -> Parameter {
        let initial = self.initial.unwrap_or(self.range.default);
        Parameter::new(self.range, initial, sample_rate, self.smoothing_ms)
    }
}
