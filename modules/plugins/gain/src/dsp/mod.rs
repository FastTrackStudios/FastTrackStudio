//! DSP processing for the gain plugin.

use crate::params::GainParamValues;
use crate::state::GainMeterState;

/// Gain processor - handles the audio processing.
pub struct GainProcessor {
    /// Current sample rate
    sample_rate: f32,
    /// Peak meter decay weight
    decay_weight: f32,
    /// Meter state (shared with UI)
    meters: GainMeterState,
}

impl GainProcessor {
    /// Peak meter decay time in milliseconds.
    const PEAK_METER_DECAY_MS: f64 = 150.0;

    /// Create a new gain processor.
    #[must_use]
    pub fn new(sample_rate: f32) -> Self {
        let decay_weight = Self::calculate_decay_weight(sample_rate);
        Self {
            sample_rate,
            decay_weight,
            meters: GainMeterState::new(),
        }
    }

    /// Calculate decay weight from sample rate.
    fn calculate_decay_weight(sample_rate: f32) -> f32 {
        0.25f64.powf((sample_rate as f64 * Self::PEAK_METER_DECAY_MS / 1000.0).recip()) as f32
    }

    /// Set the sample rate.
    pub fn set_sample_rate(&mut self, sample_rate: f32) {
        self.sample_rate = sample_rate;
        self.decay_weight = Self::calculate_decay_weight(sample_rate);
    }

    /// Get a reference to the meter state (for sharing with UI).
    #[must_use]
    pub fn meters(&self) -> &GainMeterState {
        &self.meters
    }

    /// Get a clone of the meter state (for sharing with UI).
    #[must_use]
    pub fn meters_clone(&self) -> GainMeterState {
        self.meters.clone()
    }

    /// Reset the processor state.
    pub fn reset(&mut self) {
        self.meters.reset();
    }

    /// Process a stereo buffer in-place.
    ///
    /// # Arguments
    /// * `left` - Left channel samples
    /// * `right` - Right channel samples  
    /// * `params` - Current parameter values
    /// * `update_meters` - Whether to update meter values (skip if UI closed)
    pub fn process_stereo(
        &mut self,
        left: &mut [f32],
        right: &mut [f32],
        params: &GainParamValues,
        update_meters: bool,
    ) {
        let gain = params.linear_gain();
        let phase_mult = params.phase_multiplier();
        let combined_gain = gain * phase_mult;

        let mut input_peak: f32 = 0.0;
        let mut output_peak: f32 = 0.0;

        // Process left channel
        for sample in left.iter_mut() {
            if update_meters {
                input_peak = input_peak.max(sample.abs());
            }
            *sample *= combined_gain;
            if update_meters {
                output_peak = output_peak.max(sample.abs());
            }
        }

        // Process right channel
        for sample in right.iter_mut() {
            if update_meters {
                input_peak = input_peak.max(sample.abs());
            }
            *sample *= combined_gain;
            if update_meters {
                output_peak = output_peak.max(sample.abs());
            }
        }

        // Update meters
        if update_meters {
            self.meters.update_input(input_peak, self.decay_weight);
            self.meters.update_output(output_peak, self.decay_weight);
        }
    }

    /// Process a mono buffer in-place.
    pub fn process_mono(
        &mut self,
        samples: &mut [f32],
        params: &GainParamValues,
        update_meters: bool,
    ) {
        let gain = params.linear_gain();
        let phase_mult = params.phase_multiplier();
        let combined_gain = gain * phase_mult;

        let mut input_peak: f32 = 0.0;
        let mut output_peak: f32 = 0.0;

        for sample in samples.iter_mut() {
            if update_meters {
                input_peak = input_peak.max(sample.abs());
            }
            *sample *= combined_gain;
            if update_meters {
                output_peak = output_peak.max(sample.abs());
            }
        }

        if update_meters {
            self.meters.update_input(input_peak, self.decay_weight);
            self.meters.update_output(output_peak, self.decay_weight);
        }
    }
}

impl Default for GainProcessor {
    fn default() -> Self {
        Self::new(48000.0)
    }
}
