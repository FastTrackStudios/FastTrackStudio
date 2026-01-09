//! Gain plugin parameter definitions.
//!
//! These are framework-agnostic parameter definitions that can be used
//! by both the NIH-plug wrapper and for state synchronization.

use serde::{Deserialize, Serialize};

/// Gain plugin parameter values (serializable snapshot).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct GainParamValues {
    /// Gain in dB (-60 to +24)
    pub gain_db: f32,
    /// Phase invert enabled
    pub phase_invert: bool,
}

impl Default for GainParamValues {
    fn default() -> Self {
        Self {
            gain_db: 0.0,
            phase_invert: false,
        }
    }
}

impl GainParamValues {
    /// Create from linear gain value.
    #[must_use]
    pub fn from_linear_gain(gain: f32, phase_invert: bool) -> Self {
        Self {
            gain_db: 20.0 * gain.max(1e-10).log10(),
            phase_invert,
        }
    }

    /// Get the linear gain value.
    #[must_use]
    pub fn linear_gain(&self) -> f32 {
        10.0_f32.powf(self.gain_db / 20.0)
    }

    /// Get the phase multiplier (-1.0 or 1.0).
    #[must_use]
    pub fn phase_multiplier(&self) -> f32 {
        if self.phase_invert { -1.0 } else { 1.0 }
    }
}

/// Parameter metadata for the gain plugin.
pub struct GainParamMeta;

impl GainParamMeta {
    /// Minimum gain in dB.
    pub const GAIN_MIN_DB: f32 = -60.0;
    /// Maximum gain in dB.
    pub const GAIN_MAX_DB: f32 = 24.0;
    /// Default gain in dB.
    pub const GAIN_DEFAULT_DB: f32 = 0.0;
}
