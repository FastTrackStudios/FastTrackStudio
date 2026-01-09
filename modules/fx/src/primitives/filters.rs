//! Filter primitives.
//!
//! This module provides common filter implementations for audio DSP:
//!
//! - [`Biquad`] - Second-order IIR filter (the workhorse of audio DSP)
//! - [`OnePole`] - Simple first-order filter for smoothing
//! - [`Svf`] - State Variable Filter (better for modulation)
//!
//! ## Biquad Filter Types
//!
//! The biquad can be configured for various filter responses:
//! - Lowpass / Highpass / Bandpass / Notch / Allpass
//! - Peaking EQ (parametric)
//! - Low shelf / High shelf
//!
//! ## Example
//!
//! ```ignore
//! use fts_fx::primitives::filters::{Biquad, BiquadCoeffs};
//!
//! // Create a lowpass filter at 1kHz with Q=0.707
//! let coeffs = BiquadCoeffs::lowpass(44100.0, 1000.0, 0.707);
//! let mut filter = Biquad::new(coeffs);
//!
//! // Process samples
//! let output = filter.process(input_sample);
//! ```

use core::f32::consts::PI;

// ============================================================================
// Biquad Filter
// ============================================================================

/// Biquad filter coefficients.
///
/// The transfer function is:
/// ```text
/// H(z) = (b0 + b1*z^-1 + b2*z^-2) / (1 + a1*z^-1 + a2*z^-2)
/// ```
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct BiquadCoeffs {
    pub b0: f32,
    pub b1: f32,
    pub b2: f32,
    pub a1: f32,
    pub a2: f32,
}

impl Default for BiquadCoeffs {
    fn default() -> Self {
        Self::bypass()
    }
}

impl BiquadCoeffs {
    /// Create bypass coefficients (output = input).
    #[must_use]
    pub fn bypass() -> Self {
        Self {
            b0: 1.0,
            b1: 0.0,
            b2: 0.0,
            a1: 0.0,
            a2: 0.0,
        }
    }

    /// Lowpass filter.
    ///
    /// # Arguments
    /// * `sample_rate` - Sample rate in Hz
    /// * `freq` - Cutoff frequency in Hz
    /// * `q` - Q factor (0.707 for Butterworth response)
    #[must_use]
    pub fn lowpass(sample_rate: f32, freq: f32, q: f32) -> Self {
        let omega = 2.0 * PI * freq / sample_rate;
        let sin_omega = omega.sin();
        let cos_omega = omega.cos();
        let alpha = sin_omega / (2.0 * q);

        let b0 = (1.0 - cos_omega) / 2.0;
        let b1 = 1.0 - cos_omega;
        let b2 = (1.0 - cos_omega) / 2.0;
        let a0 = 1.0 + alpha;
        let a1 = -2.0 * cos_omega;
        let a2 = 1.0 - alpha;

        Self::normalize(b0, b1, b2, a0, a1, a2)
    }

    /// Highpass filter.
    #[must_use]
    pub fn highpass(sample_rate: f32, freq: f32, q: f32) -> Self {
        let omega = 2.0 * PI * freq / sample_rate;
        let sin_omega = omega.sin();
        let cos_omega = omega.cos();
        let alpha = sin_omega / (2.0 * q);

        let b0 = (1.0 + cos_omega) / 2.0;
        let b1 = -(1.0 + cos_omega);
        let b2 = (1.0 + cos_omega) / 2.0;
        let a0 = 1.0 + alpha;
        let a1 = -2.0 * cos_omega;
        let a2 = 1.0 - alpha;

        Self::normalize(b0, b1, b2, a0, a1, a2)
    }

    /// Bandpass filter (constant skirt gain, peak gain = Q).
    #[must_use]
    pub fn bandpass(sample_rate: f32, freq: f32, q: f32) -> Self {
        let omega = 2.0 * PI * freq / sample_rate;
        let sin_omega = omega.sin();
        let cos_omega = omega.cos();
        let alpha = sin_omega / (2.0 * q);

        let b0 = alpha;
        let b1 = 0.0;
        let b2 = -alpha;
        let a0 = 1.0 + alpha;
        let a1 = -2.0 * cos_omega;
        let a2 = 1.0 - alpha;

        Self::normalize(b0, b1, b2, a0, a1, a2)
    }

    /// Notch (band-reject) filter.
    #[must_use]
    pub fn notch(sample_rate: f32, freq: f32, q: f32) -> Self {
        let omega = 2.0 * PI * freq / sample_rate;
        let sin_omega = omega.sin();
        let cos_omega = omega.cos();
        let alpha = sin_omega / (2.0 * q);

        let b0 = 1.0;
        let b1 = -2.0 * cos_omega;
        let b2 = 1.0;
        let a0 = 1.0 + alpha;
        let a1 = -2.0 * cos_omega;
        let a2 = 1.0 - alpha;

        Self::normalize(b0, b1, b2, a0, a1, a2)
    }

    /// Allpass filter.
    #[must_use]
    pub fn allpass(sample_rate: f32, freq: f32, q: f32) -> Self {
        let omega = 2.0 * PI * freq / sample_rate;
        let sin_omega = omega.sin();
        let cos_omega = omega.cos();
        let alpha = sin_omega / (2.0 * q);

        let b0 = 1.0 - alpha;
        let b1 = -2.0 * cos_omega;
        let b2 = 1.0 + alpha;
        let a0 = 1.0 + alpha;
        let a1 = -2.0 * cos_omega;
        let a2 = 1.0 - alpha;

        Self::normalize(b0, b1, b2, a0, a1, a2)
    }

    /// Peaking EQ filter.
    ///
    /// # Arguments
    /// * `sample_rate` - Sample rate in Hz
    /// * `freq` - Center frequency in Hz
    /// * `q` - Q factor (bandwidth)
    /// * `gain_db` - Gain in dB (positive = boost, negative = cut)
    #[must_use]
    pub fn peaking(sample_rate: f32, freq: f32, q: f32, gain_db: f32) -> Self {
        let a = 10.0_f32.powf(gain_db / 40.0);
        let omega = 2.0 * PI * freq / sample_rate;
        let sin_omega = omega.sin();
        let cos_omega = omega.cos();
        let alpha = sin_omega / (2.0 * q);

        let b0 = 1.0 + alpha * a;
        let b1 = -2.0 * cos_omega;
        let b2 = 1.0 - alpha * a;
        let a0 = 1.0 + alpha / a;
        let a1 = -2.0 * cos_omega;
        let a2 = 1.0 - alpha / a;

        Self::normalize(b0, b1, b2, a0, a1, a2)
    }

    /// Low shelf filter.
    ///
    /// # Arguments
    /// * `sample_rate` - Sample rate in Hz
    /// * `freq` - Shelf frequency in Hz
    /// * `q` - Slope (use 0.707 for standard 6dB/octave)
    /// * `gain_db` - Shelf gain in dB
    #[must_use]
    pub fn low_shelf(sample_rate: f32, freq: f32, q: f32, gain_db: f32) -> Self {
        let a = 10.0_f32.powf(gain_db / 40.0);
        let omega = 2.0 * PI * freq / sample_rate;
        let sin_omega = omega.sin();
        let cos_omega = omega.cos();
        let alpha = sin_omega / (2.0 * q);
        let two_sqrt_a_alpha = 2.0 * a.sqrt() * alpha;

        let b0 = a * ((a + 1.0) - (a - 1.0) * cos_omega + two_sqrt_a_alpha);
        let b1 = 2.0 * a * ((a - 1.0) - (a + 1.0) * cos_omega);
        let b2 = a * ((a + 1.0) - (a - 1.0) * cos_omega - two_sqrt_a_alpha);
        let a0 = (a + 1.0) + (a - 1.0) * cos_omega + two_sqrt_a_alpha;
        let a1 = -2.0 * ((a - 1.0) + (a + 1.0) * cos_omega);
        let a2 = (a + 1.0) + (a - 1.0) * cos_omega - two_sqrt_a_alpha;

        Self::normalize(b0, b1, b2, a0, a1, a2)
    }

    /// High shelf filter.
    #[must_use]
    pub fn high_shelf(sample_rate: f32, freq: f32, q: f32, gain_db: f32) -> Self {
        let a = 10.0_f32.powf(gain_db / 40.0);
        let omega = 2.0 * PI * freq / sample_rate;
        let sin_omega = omega.sin();
        let cos_omega = omega.cos();
        let alpha = sin_omega / (2.0 * q);
        let two_sqrt_a_alpha = 2.0 * a.sqrt() * alpha;

        let b0 = a * ((a + 1.0) + (a - 1.0) * cos_omega + two_sqrt_a_alpha);
        let b1 = -2.0 * a * ((a - 1.0) + (a + 1.0) * cos_omega);
        let b2 = a * ((a + 1.0) + (a - 1.0) * cos_omega - two_sqrt_a_alpha);
        let a0 = (a + 1.0) - (a - 1.0) * cos_omega + two_sqrt_a_alpha;
        let a1 = 2.0 * ((a - 1.0) - (a + 1.0) * cos_omega);
        let a2 = (a + 1.0) - (a - 1.0) * cos_omega - two_sqrt_a_alpha;

        Self::normalize(b0, b1, b2, a0, a1, a2)
    }

    /// Normalize coefficients by a0.
    fn normalize(b0: f32, b1: f32, b2: f32, a0: f32, a1: f32, a2: f32) -> Self {
        Self {
            b0: b0 / a0,
            b1: b1 / a0,
            b2: b2 / a0,
            a1: a1 / a0,
            a2: a2 / a0,
        }
    }
}

/// Biquad filter processor (Direct Form II Transposed).
///
/// This is the standard second-order IIR filter used throughout audio DSP.
/// Direct Form II Transposed has better numerical properties than Direct Form I.
#[derive(Clone, Debug)]
pub struct Biquad {
    coeffs: BiquadCoeffs,
    z1: f32,
    z2: f32,
}

impl Biquad {
    /// Create a new biquad filter with the given coefficients.
    #[must_use]
    pub fn new(coeffs: BiquadCoeffs) -> Self {
        Self {
            coeffs,
            z1: 0.0,
            z2: 0.0,
        }
    }

    /// Create a bypass filter.
    #[must_use]
    pub fn bypass() -> Self {
        Self::new(BiquadCoeffs::bypass())
    }

    /// Update the filter coefficients.
    pub fn set_coeffs(&mut self, coeffs: BiquadCoeffs) {
        self.coeffs = coeffs;
    }

    /// Get the current coefficients.
    #[must_use]
    pub fn coeffs(&self) -> &BiquadCoeffs {
        &self.coeffs
    }

    /// Process a single sample.
    #[must_use]
    pub fn process(&mut self, input: f32) -> f32 {
        let output = self.coeffs.b0 * input + self.z1;
        self.z1 = self.coeffs.b1 * input - self.coeffs.a1 * output + self.z2;
        self.z2 = self.coeffs.b2 * input - self.coeffs.a2 * output;
        output
    }

    /// Process a buffer of samples in place.
    pub fn process_buffer(&mut self, buffer: &mut [f32]) {
        for sample in buffer {
            *sample = self.process(*sample);
        }
    }

    /// Reset the filter state (clear delay elements).
    pub fn reset(&mut self) {
        self.z1 = 0.0;
        self.z2 = 0.0;
    }
}

impl Default for Biquad {
    fn default() -> Self {
        Self::bypass()
    }
}

// ============================================================================
// One-Pole Filter
// ============================================================================

/// Simple one-pole (first-order) lowpass filter.
///
/// Excellent for parameter smoothing and simple filtering tasks.
/// Much cheaper than a biquad when you don't need steep rolloff.
#[derive(Clone, Debug)]
pub struct OnePole {
    a: f32,  // feedback coefficient
    b: f32,  // feedforward coefficient
    z1: f32, // state
}

impl OnePole {
    /// Create a new one-pole lowpass filter.
    ///
    /// # Arguments
    /// * `sample_rate` - Sample rate in Hz
    /// * `freq` - Cutoff frequency in Hz
    #[must_use]
    pub fn lowpass(sample_rate: f32, freq: f32) -> Self {
        let omega = 2.0 * PI * freq / sample_rate;
        let b = omega / (1.0 + omega);
        Self { a: 1.0 - b, b, z1: 0.0 }
    }

    /// Create a one-pole filter from a time constant.
    ///
    /// # Arguments
    /// * `sample_rate` - Sample rate in Hz
    /// * `time_constant` - Time constant in seconds (time to reach ~63% of target)
    #[must_use]
    pub fn from_time_constant(sample_rate: f32, time_constant: f32) -> Self {
        if time_constant <= 0.0 {
            return Self { a: 0.0, b: 1.0, z1: 0.0 };
        }
        let a = (-1.0 / (time_constant * sample_rate)).exp();
        Self { a, b: 1.0 - a, z1: 0.0 }
    }

    /// Set the cutoff frequency.
    pub fn set_freq(&mut self, sample_rate: f32, freq: f32) {
        let omega = 2.0 * PI * freq / sample_rate;
        self.b = omega / (1.0 + omega);
        self.a = 1.0 - self.b;
    }

    /// Process a single sample.
    #[must_use]
    pub fn process(&mut self, input: f32) -> f32 {
        self.z1 = self.b * input + self.a * self.z1;
        self.z1
    }

    /// Process a buffer in place.
    pub fn process_buffer(&mut self, buffer: &mut [f32]) {
        for sample in buffer {
            *sample = self.process(*sample);
        }
    }

    /// Reset filter state.
    pub fn reset(&mut self) {
        self.z1 = 0.0;
    }

    /// Set the current state (useful for initializing to a value).
    pub fn set_state(&mut self, value: f32) {
        self.z1 = value;
    }

    /// Get the current output value.
    #[must_use]
    pub fn current(&self) -> f32 {
        self.z1
    }
}

impl Default for OnePole {
    fn default() -> Self {
        Self { a: 0.0, b: 1.0, z1: 0.0 }
    }
}

// ============================================================================
// State Variable Filter (SVF)
// ============================================================================

/// State Variable Filter outputs.
///
/// The SVF produces all filter types simultaneously.
#[derive(Clone, Copy, Debug, Default)]
pub struct SvfOutput {
    pub lowpass: f32,
    pub highpass: f32,
    pub bandpass: f32,
    pub notch: f32,
}

/// State Variable Filter (Chamberlin topology).
///
/// Advantages over biquad:
/// - More stable when modulating cutoff frequency
/// - Produces LP, HP, BP, and notch outputs simultaneously
/// - Easier to understand topology
///
/// Good for synth filters and effects where cutoff is modulated.
#[derive(Clone, Debug)]
pub struct Svf {
    f: f32,  // frequency coefficient
    q: f32,  // damping (1/Q)
    low: f32,
    band: f32,
}

impl Svf {
    /// Create a new SVF.
    ///
    /// # Arguments
    /// * `sample_rate` - Sample rate in Hz
    /// * `freq` - Cutoff/center frequency in Hz
    /// * `q` - Q factor (resonance)
    #[must_use]
    pub fn new(sample_rate: f32, freq: f32, q: f32) -> Self {
        let mut svf = Self {
            f: 0.0,
            q: 0.0,
            low: 0.0,
            band: 0.0,
        };
        svf.set_params(sample_rate, freq, q);
        svf
    }

    /// Update filter parameters.
    pub fn set_params(&mut self, sample_rate: f32, freq: f32, q: f32) {
        // Limit frequency to Nyquist
        let freq = freq.min(sample_rate * 0.49);
        self.f = 2.0 * (PI * freq / sample_rate).sin();
        self.q = 1.0 / q;
    }

    /// Process a single sample, returning all outputs.
    #[must_use]
    pub fn process(&mut self, input: f32) -> SvfOutput {
        // Two-sample delay version for stability
        let low = self.low + self.f * self.band;
        let high = input - low - self.q * self.band;
        let band = self.f * high + self.band;
        let notch = high + low;

        self.low = low;
        self.band = band;

        SvfOutput {
            lowpass: low,
            highpass: high,
            bandpass: band,
            notch,
        }
    }

    /// Process and return only the lowpass output.
    #[must_use]
    pub fn process_lowpass(&mut self, input: f32) -> f32 {
        self.process(input).lowpass
    }

    /// Process and return only the highpass output.
    #[must_use]
    pub fn process_highpass(&mut self, input: f32) -> f32 {
        self.process(input).highpass
    }

    /// Process and return only the bandpass output.
    #[must_use]
    pub fn process_bandpass(&mut self, input: f32) -> f32 {
        self.process(input).bandpass
    }

    /// Process and return only the notch output.
    #[must_use]
    pub fn process_notch(&mut self, input: f32) -> f32 {
        self.process(input).notch
    }

    /// Reset filter state.
    pub fn reset(&mut self) {
        self.low = 0.0;
        self.band = 0.0;
    }
}

impl Default for Svf {
    fn default() -> Self {
        Self {
            f: 0.1,
            q: 1.0,
            low: 0.0,
            band: 0.0,
        }
    }
}

// ============================================================================
// DC Blocker
// ============================================================================

/// DC blocking filter.
///
/// Removes DC offset from a signal. Essential after waveshaping
/// or other nonlinear processing that can introduce DC.
#[derive(Clone, Debug)]
pub struct DcBlocker {
    x1: f32,
    y1: f32,
    r: f32,
}

impl DcBlocker {
    /// Create a new DC blocker.
    ///
    /// # Arguments
    /// * `r` - Pole radius (0.99-0.999 typical, higher = lower cutoff)
    #[must_use]
    pub fn new(r: f32) -> Self {
        Self { x1: 0.0, y1: 0.0, r }
    }

    /// Process a single sample.
    #[must_use]
    pub fn process(&mut self, input: f32) -> f32 {
        let output = input - self.x1 + self.r * self.y1;
        self.x1 = input;
        self.y1 = output;
        output
    }

    /// Process a buffer in place.
    pub fn process_buffer(&mut self, buffer: &mut [f32]) {
        for sample in buffer {
            *sample = self.process(*sample);
        }
    }

    /// Reset filter state.
    pub fn reset(&mut self) {
        self.x1 = 0.0;
        self.y1 = 0.0;
    }
}

impl Default for DcBlocker {
    fn default() -> Self {
        Self::new(0.995)
    }
}
