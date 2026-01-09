//! Metering utilities for FTS plugins.

use atomic_float::AtomicF32;
use std::sync::atomic::Ordering;

/// Thread-safe peak meter with decay.
#[derive(Debug)]
pub struct PeakMeter {
    /// Current peak value (linear)
    peak: AtomicF32,
    /// Decay coefficient per sample
    decay_coeff: AtomicF32,
}

impl PeakMeter {
    /// Create a new peak meter.
    ///
    /// # Arguments
    /// * `decay_ms` - Time for peak to decay by ~63% in milliseconds
    /// * `sample_rate` - Sample rate in Hz
    #[must_use]
    pub fn new(decay_ms: f32, sample_rate: f32) -> Self {
        Self {
            peak: AtomicF32::new(0.0),
            decay_coeff: AtomicF32::new(Self::calc_decay_coeff(decay_ms, sample_rate)),
        }
    }

    /// Calculate decay coefficient from time and sample rate.
    fn calc_decay_coeff(decay_ms: f32, sample_rate: f32) -> f32 {
        (-1.0 / (decay_ms * 0.001 * sample_rate)).exp()
    }

    /// Update the sample rate (recalculates decay coefficient).
    pub fn set_sample_rate(&self, decay_ms: f32, sample_rate: f32) {
        self.decay_coeff.store(
            Self::calc_decay_coeff(decay_ms, sample_rate),
            Ordering::Relaxed,
        );
    }

    /// Process a single sample and update peak.
    /// Call this from the audio thread.
    #[inline]
    pub fn process(&self, sample: f32) {
        let abs_sample = sample.abs();
        let current = self.peak.load(Ordering::Relaxed);
        let decay = self.decay_coeff.load(Ordering::Relaxed);

        let new_peak = if abs_sample > current {
            abs_sample
        } else {
            current * decay
        };

        self.peak.store(new_peak, Ordering::Relaxed);
    }

    /// Process a buffer and update peak.
    #[inline]
    pub fn process_buffer(&self, buffer: &[f32]) {
        let mut max_sample = 0.0_f32;
        for &sample in buffer {
            max_sample = max_sample.max(sample.abs());
        }

        let current = self.peak.load(Ordering::Relaxed);
        let decay = self.decay_coeff.load(Ordering::Relaxed);

        // Apply decay for the buffer length, then check against max
        let decayed = current * decay.powi(buffer.len() as i32);
        let new_peak = max_sample.max(decayed);

        self.peak.store(new_peak, Ordering::Relaxed);
    }

    /// Get the current peak value (linear).
    #[inline]
    #[must_use]
    pub fn peak(&self) -> f32 {
        self.peak.load(Ordering::Relaxed)
    }

    /// Get the current peak value in decibels.
    #[inline]
    #[must_use]
    pub fn peak_db(&self) -> f32 {
        let peak = self.peak.load(Ordering::Relaxed);
        if peak > 0.0 {
            20.0 * peak.log10()
        } else {
            f32::NEG_INFINITY
        }
    }

    /// Reset the peak to zero.
    pub fn reset(&self) {
        self.peak.store(0.0, Ordering::Relaxed);
    }
}

impl Default for PeakMeter {
    fn default() -> Self {
        Self::new(300.0, 48000.0)
    }
}

impl Clone for PeakMeter {
    fn clone(&self) -> Self {
        Self {
            peak: AtomicF32::new(self.peak.load(Ordering::Relaxed)),
            decay_coeff: AtomicF32::new(self.decay_coeff.load(Ordering::Relaxed)),
        }
    }
}

/// Stereo peak meter.
#[derive(Debug, Clone)]
pub struct StereoPeakMeter {
    pub left: PeakMeter,
    pub right: PeakMeter,
}

impl StereoPeakMeter {
    /// Create a new stereo peak meter.
    #[must_use]
    pub fn new(decay_ms: f32, sample_rate: f32) -> Self {
        Self {
            left: PeakMeter::new(decay_ms, sample_rate),
            right: PeakMeter::new(decay_ms, sample_rate),
        }
    }

    /// Update sample rate for both channels.
    pub fn set_sample_rate(&self, decay_ms: f32, sample_rate: f32) {
        self.left.set_sample_rate(decay_ms, sample_rate);
        self.right.set_sample_rate(decay_ms, sample_rate);
    }

    /// Process stereo samples.
    #[inline]
    pub fn process(&self, left: f32, right: f32) {
        self.left.process(left);
        self.right.process(right);
    }

    /// Process stereo buffers.
    pub fn process_buffers(&self, left: &[f32], right: &[f32]) {
        self.left.process_buffer(left);
        self.right.process_buffer(right);
    }

    /// Get peak values (linear).
    #[must_use]
    pub fn peaks(&self) -> (f32, f32) {
        (self.left.peak(), self.right.peak())
    }

    /// Get peak values in decibels.
    #[must_use]
    pub fn peaks_db(&self) -> (f32, f32) {
        (self.left.peak_db(), self.right.peak_db())
    }

    /// Reset both channels.
    pub fn reset(&self) {
        self.left.reset();
        self.right.reset();
    }
}

impl Default for StereoPeakMeter {
    fn default() -> Self {
        Self::new(300.0, 48000.0)
    }
}

/// Gain reduction meter (for compressors).
#[derive(Debug)]
pub struct GainReductionMeter {
    /// Current gain reduction in dB (negative value)
    gr_db: AtomicF32,
}

impl GainReductionMeter {
    /// Create a new gain reduction meter.
    #[must_use]
    pub fn new() -> Self {
        Self {
            gr_db: AtomicF32::new(0.0),
        }
    }

    /// Update the gain reduction value.
    ///
    /// # Arguments
    /// * `gr_db` - Gain reduction in dB (should be negative or zero)
    #[inline]
    pub fn set(&self, gr_db: f32) {
        self.gr_db.store(gr_db, Ordering::Relaxed);
    }

    /// Update from linear gain value.
    #[inline]
    pub fn set_linear(&self, gain: f32) {
        let db = if gain > 0.0 {
            20.0 * gain.log10()
        } else {
            -60.0 // Floor at -60dB
        };
        self.gr_db.store(db.min(0.0), Ordering::Relaxed);
    }

    /// Get the current gain reduction in dB.
    #[inline]
    #[must_use]
    pub fn db(&self) -> f32 {
        self.gr_db.load(Ordering::Relaxed)
    }

    /// Reset to zero gain reduction.
    pub fn reset(&self) {
        self.gr_db.store(0.0, Ordering::Relaxed);
    }
}

impl Default for GainReductionMeter {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for GainReductionMeter {
    fn clone(&self) -> Self {
        Self {
            gr_db: AtomicF32::new(self.gr_db.load(Ordering::Relaxed)),
        }
    }
}
