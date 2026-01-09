//! Gain plugin state types for hub communication and UI.

use atomic_float::AtomicF32;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::sync::atomic::Ordering;

use crate::params::GainParamValues;

/// Complete gain plugin state snapshot (for hub communication).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GainState {
    /// Current parameter values
    pub params: GainParamValues,
    /// Meter data
    pub meters: GainMeterData,
    /// Whether the plugin is bypassed
    pub bypassed: bool,
}

impl Default for GainState {
    fn default() -> Self {
        Self {
            params: GainParamValues::default(),
            meters: GainMeterData::default(),
            bypassed: false,
        }
    }
}

/// Meter data for the gain plugin.
#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize)]
pub struct GainMeterData {
    /// Input peak level (linear, 0.0-1.0+)
    pub input_peak: f32,
    /// Output peak level (linear, 0.0-1.0+)
    pub output_peak: f32,
}

impl GainMeterData {
    /// Convert to dB values.
    #[must_use]
    pub fn to_db(&self) -> (f32, f32) {
        let input_db = if self.input_peak > 0.0 {
            20.0 * self.input_peak.log10()
        } else {
            -60.0
        };
        let output_db = if self.output_peak > 0.0 {
            20.0 * self.output_peak.log10()
        } else {
            -60.0
        };
        (input_db, output_db)
    }
}

/// Thread-safe meter state for real-time updates.
///
/// This is shared between the audio thread and UI thread.
#[derive(Debug)]
pub struct GainMeterState {
    /// Input peak (linear)
    pub input_peak: Arc<AtomicF32>,
    /// Output peak (linear)
    pub output_peak: Arc<AtomicF32>,
}

impl Default for GainMeterState {
    fn default() -> Self {
        Self::new()
    }
}

impl GainMeterState {
    /// Create new meter state.
    #[must_use]
    pub fn new() -> Self {
        Self {
            input_peak: Arc::new(AtomicF32::new(0.0)),
            output_peak: Arc::new(AtomicF32::new(0.0)),
        }
    }

    /// Get a snapshot of the current meter data.
    #[must_use]
    pub fn snapshot(&self) -> GainMeterData {
        GainMeterData {
            input_peak: self.input_peak.load(Ordering::Relaxed),
            output_peak: self.output_peak.load(Ordering::Relaxed),
        }
    }

    /// Get dB values for display.
    #[must_use]
    pub fn to_db(&self) -> (f32, f32) {
        self.snapshot().to_db()
    }

    /// Update input peak (call from audio thread).
    pub fn update_input(&self, peak: f32, decay_weight: f32) {
        let current = self.input_peak.load(Ordering::Relaxed);
        let new_value = if peak > current {
            peak
        } else {
            current * decay_weight + peak * (1.0 - decay_weight)
        };
        self.input_peak.store(new_value, Ordering::Relaxed);
    }

    /// Update output peak (call from audio thread).
    pub fn update_output(&self, peak: f32, decay_weight: f32) {
        let current = self.output_peak.load(Ordering::Relaxed);
        let new_value = if peak > current {
            peak
        } else {
            current * decay_weight + peak * (1.0 - decay_weight)
        };
        self.output_peak.store(new_value, Ordering::Relaxed);
    }

    /// Reset meters to zero.
    pub fn reset(&self) {
        self.input_peak.store(0.0, Ordering::Relaxed);
        self.output_peak.store(0.0, Ordering::Relaxed);
    }
}

impl Clone for GainMeterState {
    fn clone(&self) -> Self {
        Self {
            input_peak: Arc::clone(&self.input_peak),
            output_peak: Arc::clone(&self.output_peak),
        }
    }
}
