//! Common DSP utilities for FTS plugins.

use std::f32::consts::PI;

/// Convert decibels to linear gain.
#[inline]
#[must_use]
pub fn db_to_gain(db: f32) -> f32 {
    10.0_f32.powf(db / 20.0)
}

/// Convert linear gain to decibels.
#[inline]
#[must_use]
pub fn gain_to_db(gain: f32) -> f32 {
    20.0 * gain.max(1e-10).log10()
}

/// Compute the coefficient for a simple one-pole lowpass filter.
///
/// # Arguments
/// * `cutoff_hz` - Cutoff frequency in Hz
/// * `sample_rate` - Sample rate in Hz
///
/// # Returns
/// Filter coefficient (0.0 to 1.0)
#[inline]
#[must_use]
pub fn one_pole_coefficient(cutoff_hz: f32, sample_rate: f32) -> f32 {
    let omega = 2.0 * PI * cutoff_hz / sample_rate;
    1.0 - (-omega).exp()
}

/// Simple one-pole lowpass filter for smoothing.
#[derive(Debug, Clone, Copy)]
pub struct OnePole {
    state: f32,
    coefficient: f32,
}

impl OnePole {
    /// Create a new one-pole filter.
    #[must_use]
    pub fn new(cutoff_hz: f32, sample_rate: f32) -> Self {
        Self {
            state: 0.0,
            coefficient: one_pole_coefficient(cutoff_hz, sample_rate),
        }
    }

    /// Set the cutoff frequency.
    pub fn set_cutoff(&mut self, cutoff_hz: f32, sample_rate: f32) {
        self.coefficient = one_pole_coefficient(cutoff_hz, sample_rate);
    }

    /// Process a single sample.
    #[inline]
    pub fn process(&mut self, input: f32) -> f32 {
        self.state += self.coefficient * (input - self.state);
        self.state
    }

    /// Reset the filter state.
    pub fn reset(&mut self) {
        self.state = 0.0;
    }

    /// Set the state directly (useful for initialization).
    pub fn set_state(&mut self, state: f32) {
        self.state = state;
    }
}

impl Default for OnePole {
    fn default() -> Self {
        Self {
            state: 0.0,
            coefficient: 0.1,
        }
    }
}

/// Attack-release envelope follower.
#[derive(Debug, Clone, Copy)]
pub struct AttackRelease {
    state: f32,
    attack_coeff: f32,
    release_coeff: f32,
}

impl AttackRelease {
    /// Create a new attack-release envelope follower.
    ///
    /// # Arguments
    /// * `attack_ms` - Attack time in milliseconds
    /// * `release_ms` - Release time in milliseconds
    /// * `sample_rate` - Sample rate in Hz
    #[must_use]
    pub fn new(attack_ms: f32, release_ms: f32, sample_rate: f32) -> Self {
        Self {
            state: 0.0,
            attack_coeff: Self::time_to_coeff(attack_ms, sample_rate),
            release_coeff: Self::time_to_coeff(release_ms, sample_rate),
        }
    }

    /// Convert time in ms to filter coefficient.
    #[inline]
    fn time_to_coeff(time_ms: f32, sample_rate: f32) -> f32 {
        1.0 - (-2200.0 / (time_ms * sample_rate)).exp()
    }

    /// Set attack time.
    pub fn set_attack(&mut self, attack_ms: f32, sample_rate: f32) {
        self.attack_coeff = Self::time_to_coeff(attack_ms, sample_rate);
    }

    /// Set release time.
    pub fn set_release(&mut self, release_ms: f32, sample_rate: f32) {
        self.release_coeff = Self::time_to_coeff(release_ms, sample_rate);
    }

    /// Process a single sample.
    #[inline]
    pub fn process(&mut self, input: f32) -> f32 {
        let coeff = if input > self.state {
            self.attack_coeff
        } else {
            self.release_coeff
        };
        self.state += coeff * (input - self.state);
        self.state
    }

    /// Get the current state.
    #[inline]
    #[must_use]
    pub fn state(&self) -> f32 {
        self.state
    }

    /// Reset the envelope follower.
    pub fn reset(&mut self) {
        self.state = 0.0;
    }
}

impl Default for AttackRelease {
    fn default() -> Self {
        Self {
            state: 0.0,
            attack_coeff: 0.01,
            release_coeff: 0.001,
        }
    }
}

/// Biquad filter coefficients.
#[derive(Debug, Clone, Copy, Default)]
pub struct BiquadCoeffs {
    pub b0: f32,
    pub b1: f32,
    pub b2: f32,
    pub a1: f32,
    pub a2: f32,
}

impl BiquadCoeffs {
    /// Create lowpass filter coefficients.
    #[must_use]
    pub fn lowpass(freq_hz: f32, q: f32, sample_rate: f32) -> Self {
        let omega = 2.0 * PI * freq_hz / sample_rate;
        let sin_omega = omega.sin();
        let cos_omega = omega.cos();
        let alpha = sin_omega / (2.0 * q);

        let b0 = (1.0 - cos_omega) / 2.0;
        let b1 = 1.0 - cos_omega;
        let b2 = (1.0 - cos_omega) / 2.0;
        let a0 = 1.0 + alpha;
        let a1 = -2.0 * cos_omega;
        let a2 = 1.0 - alpha;

        Self {
            b0: b0 / a0,
            b1: b1 / a0,
            b2: b2 / a0,
            a1: a1 / a0,
            a2: a2 / a0,
        }
    }

    /// Create highpass filter coefficients.
    #[must_use]
    pub fn highpass(freq_hz: f32, q: f32, sample_rate: f32) -> Self {
        let omega = 2.0 * PI * freq_hz / sample_rate;
        let sin_omega = omega.sin();
        let cos_omega = omega.cos();
        let alpha = sin_omega / (2.0 * q);

        let b0 = (1.0 + cos_omega) / 2.0;
        let b1 = -(1.0 + cos_omega);
        let b2 = (1.0 + cos_omega) / 2.0;
        let a0 = 1.0 + alpha;
        let a1 = -2.0 * cos_omega;
        let a2 = 1.0 - alpha;

        Self {
            b0: b0 / a0,
            b1: b1 / a0,
            b2: b2 / a0,
            a1: a1 / a0,
            a2: a2 / a0,
        }
    }

    /// Create peaking EQ filter coefficients.
    #[must_use]
    pub fn peak(freq_hz: f32, q: f32, gain_db: f32, sample_rate: f32) -> Self {
        let a = db_to_gain(gain_db / 2.0);
        let omega = 2.0 * PI * freq_hz / sample_rate;
        let sin_omega = omega.sin();
        let cos_omega = omega.cos();
        let alpha = sin_omega / (2.0 * q);

        let b0 = 1.0 + alpha * a;
        let b1 = -2.0 * cos_omega;
        let b2 = 1.0 - alpha * a;
        let a0 = 1.0 + alpha / a;
        let a1 = -2.0 * cos_omega;
        let a2 = 1.0 - alpha / a;

        Self {
            b0: b0 / a0,
            b1: b1 / a0,
            b2: b2 / a0,
            a1: a1 / a0,
            a2: a2 / a0,
        }
    }

    /// Create low shelf filter coefficients.
    #[must_use]
    pub fn lowshelf(freq_hz: f32, q: f32, gain_db: f32, sample_rate: f32) -> Self {
        let a = db_to_gain(gain_db / 2.0);
        let omega = 2.0 * PI * freq_hz / sample_rate;
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

        Self {
            b0: b0 / a0,
            b1: b1 / a0,
            b2: b2 / a0,
            a1: a1 / a0,
            a2: a2 / a0,
        }
    }

    /// Create high shelf filter coefficients.
    #[must_use]
    pub fn highshelf(freq_hz: f32, q: f32, gain_db: f32, sample_rate: f32) -> Self {
        let a = db_to_gain(gain_db / 2.0);
        let omega = 2.0 * PI * freq_hz / sample_rate;
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

        Self {
            b0: b0 / a0,
            b1: b1 / a0,
            b2: b2 / a0,
            a1: a1 / a0,
            a2: a2 / a0,
        }
    }
}

/// Biquad filter state.
#[derive(Debug, Clone, Copy, Default)]
pub struct BiquadState {
    z1: f32,
    z2: f32,
}

impl BiquadState {
    /// Create a new biquad state.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Process a single sample through the filter.
    #[inline]
    pub fn process(&mut self, input: f32, coeffs: &BiquadCoeffs) -> f32 {
        let output = coeffs.b0 * input + self.z1;
        self.z1 = coeffs.b1 * input - coeffs.a1 * output + self.z2;
        self.z2 = coeffs.b2 * input - coeffs.a2 * output;
        output
    }

    /// Reset the filter state.
    pub fn reset(&mut self) {
        self.z1 = 0.0;
        self.z2 = 0.0;
    }
}
