//! Modulation sources (LFOs, envelopes, smoothers).
//!
//! This module provides modulation primitives for audio effects:
//!
//! - [`Lfo`] - Low Frequency Oscillator with multiple waveforms
//! - [`Smoother`] - Parameter smoothing to avoid zipper noise
//! - [`Adsr`] - Attack-Decay-Sustain-Release envelope generator
//!
//! ## Example
//!
//! ```ignore
//! use fts_fx::primitives::modulation::{Lfo, LfoWaveform};
//!
//! // Create a 2Hz sine LFO
//! let mut lfo = Lfo::new(44100.0, 2.0, LfoWaveform::Sine);
//!
//! // Get modulation values
//! for _ in 0..44100 {
//!     let mod_value = lfo.next(); // Returns -1.0 to 1.0
//! }
//! ```

use core::f32::consts::PI;

// ============================================================================
// LFO (Low Frequency Oscillator)
// ============================================================================

/// LFO waveform types.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum LfoWaveform {
    /// Smooth sinusoidal wave
    #[default]
    Sine,
    /// Linear triangle wave
    Triangle,
    /// Hard square wave
    Square,
    /// Rising sawtooth
    SawUp,
    /// Falling sawtooth
    SawDown,
    /// Random value held until next cycle
    SampleAndHold,
}

/// Low Frequency Oscillator.
///
/// Generates periodic modulation signals at sub-audio frequencies.
/// Output range is -1.0 to 1.0 for all waveforms.
#[derive(Clone, Debug)]
pub struct Lfo {
    phase: f32,
    phase_inc: f32,
    waveform: LfoWaveform,
    sample_rate: f32,
    // For sample & hold
    held_value: f32,
    prev_phase: f32,
}

impl Lfo {
    /// Create a new LFO.
    ///
    /// # Arguments
    /// * `sample_rate` - Sample rate in Hz
    /// * `freq` - LFO frequency in Hz
    /// * `waveform` - Waveform type
    #[must_use]
    pub fn new(sample_rate: f32, freq: f32, waveform: LfoWaveform) -> Self {
        Self {
            phase: 0.0,
            phase_inc: freq / sample_rate,
            waveform,
            sample_rate,
            held_value: 0.0,
            prev_phase: 0.0,
        }
    }

    /// Set the LFO frequency.
    pub fn set_freq(&mut self, freq: f32) {
        self.phase_inc = freq / self.sample_rate;
    }

    /// Set the waveform.
    pub fn set_waveform(&mut self, waveform: LfoWaveform) {
        self.waveform = waveform;
    }

    /// Set the phase (0.0 to 1.0).
    pub fn set_phase(&mut self, phase: f32) {
        self.phase = phase.fract();
        if self.phase < 0.0 {
            self.phase += 1.0;
        }
    }

    /// Get the current phase (0.0 to 1.0).
    #[must_use]
    pub fn phase(&self) -> f32 {
        self.phase
    }

    /// Reset the LFO to the start of its cycle.
    pub fn reset(&mut self) {
        self.phase = 0.0;
        self.prev_phase = 0.0;
    }

    /// Generate the next LFO sample.
    ///
    /// Returns a value in the range -1.0 to 1.0.
    #[must_use]
    pub fn next(&mut self) -> f32 {
        let output = self.sample_waveform();

        self.prev_phase = self.phase;
        self.phase += self.phase_inc;
        if self.phase >= 1.0 {
            self.phase -= 1.0;
        }

        output
    }

    /// Sample the current waveform value without advancing phase.
    #[must_use]
    pub fn current(&self) -> f32 {
        match self.waveform {
            LfoWaveform::Sine => (self.phase * 2.0 * PI).sin(),
            LfoWaveform::Triangle => {
                if self.phase < 0.25 {
                    self.phase * 4.0
                } else if self.phase < 0.75 {
                    2.0 - self.phase * 4.0
                } else {
                    self.phase * 4.0 - 4.0
                }
            }
            LfoWaveform::Square => {
                if self.phase < 0.5 {
                    1.0
                } else {
                    -1.0
                }
            }
            LfoWaveform::SawUp => self.phase * 2.0 - 1.0,
            LfoWaveform::SawDown => 1.0 - self.phase * 2.0,
            LfoWaveform::SampleAndHold => self.held_value,
        }
    }

    fn sample_waveform(&mut self) -> f32 {
        match self.waveform {
            LfoWaveform::SampleAndHold => {
                // Update held value when phase wraps
                if self.phase < self.prev_phase {
                    // Use accumulated phase count for variety
                    self.held_value = phase_hash(self.phase + self.held_value);
                }
                self.held_value
            }
            _ => self.current(),
        }
    }

    /// Get a unipolar output (0.0 to 1.0) instead of bipolar.
    #[must_use]
    pub fn next_unipolar(&mut self) -> f32 {
        (self.next() + 1.0) * 0.5
    }
}

impl Default for Lfo {
    fn default() -> Self {
        Self::new(44100.0, 1.0, LfoWaveform::Sine)
    }
}

// Simple deterministic "random" for S&H based on phase
// This is predictable but good enough for audio modulation
fn phase_hash(phase: f32) -> f32 {
    // Simple hash function based on phase
    let x = (phase * 12345.6789).to_bits();
    let x = x.wrapping_mul(0x9E37_79B9);
    let x = x ^ (x >> 16);
    (x as f32) / (u32::MAX as f32) * 2.0 - 1.0
}

// ============================================================================
// Parameter Smoother
// ============================================================================

/// Parameter smoother for avoiding zipper noise.
///
/// Uses a one-pole lowpass filter to smooth parameter changes.
/// Essential for any parameter that's updated at control rate.
#[derive(Clone, Debug)]
pub struct Smoother {
    current: f32,
    target: f32,
    coeff: f32,
}

impl Smoother {
    /// Create a new smoother.
    ///
    /// # Arguments
    /// * `sample_rate` - Sample rate in Hz
    /// * `smoothing_ms` - Smoothing time in milliseconds
    #[must_use]
    pub fn new(sample_rate: f32, smoothing_ms: f32) -> Self {
        let coeff = Self::calc_coeff(sample_rate, smoothing_ms);
        Self {
            current: 0.0,
            target: 0.0,
            coeff,
        }
    }

    /// Create a smoother initialized to a value.
    #[must_use]
    pub fn with_value(sample_rate: f32, smoothing_ms: f32, value: f32) -> Self {
        let mut s = Self::new(sample_rate, smoothing_ms);
        s.set_immediate(value);
        s
    }

    fn calc_coeff(sample_rate: f32, smoothing_ms: f32) -> f32 {
        if smoothing_ms <= 0.0 {
            return 1.0;
        }
        let samples = smoothing_ms * sample_rate / 1000.0;
        (-1.0 / samples).exp()
    }

    /// Set the smoothing time.
    pub fn set_smoothing(&mut self, sample_rate: f32, smoothing_ms: f32) {
        self.coeff = Self::calc_coeff(sample_rate, smoothing_ms);
    }

    /// Set a new target value (will smooth towards it).
    pub fn set_target(&mut self, target: f32) {
        self.target = target;
    }

    /// Set value immediately without smoothing.
    pub fn set_immediate(&mut self, value: f32) {
        self.current = value;
        self.target = value;
    }

    /// Get the next smoothed value.
    #[must_use]
    pub fn next(&mut self) -> f32 {
        self.current = self.target + self.coeff * (self.current - self.target);
        self.current
    }

    /// Get the current value without advancing.
    #[must_use]
    pub fn current(&self) -> f32 {
        self.current
    }

    /// Get the target value.
    #[must_use]
    pub fn target(&self) -> f32 {
        self.target
    }

    /// Check if the smoother has reached its target (within tolerance).
    #[must_use]
    pub fn is_settled(&self) -> bool {
        (self.current - self.target).abs() < 1e-6
    }

    /// Fill a buffer with smoothed values.
    pub fn process_buffer(&mut self, buffer: &mut [f32]) {
        for sample in buffer {
            *sample = self.next();
        }
    }
}

impl Default for Smoother {
    fn default() -> Self {
        Self::new(44100.0, 10.0)
    }
}

// ============================================================================
// ADSR Envelope
// ============================================================================

/// ADSR envelope stage.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum AdsrStage {
    /// Envelope is idle (not triggered)
    #[default]
    Idle,
    /// Attack phase - rising from 0 to 1
    Attack,
    /// Decay phase - falling from 1 to sustain level
    Decay,
    /// Sustain phase - holding at sustain level
    Sustain,
    /// Release phase - falling from current level to 0
    Release,
}

/// ADSR (Attack-Decay-Sustain-Release) envelope generator.
///
/// Standard envelope for synths and dynamics processing.
#[derive(Clone, Debug)]
pub struct Adsr {
    stage: AdsrStage,
    level: f32,
    attack_coeff: f32,
    decay_coeff: f32,
    sustain: f32,
    release_coeff: f32,
    sample_rate: f32,
}

impl Adsr {
    /// Create a new ADSR envelope.
    ///
    /// # Arguments
    /// * `sample_rate` - Sample rate in Hz
    /// * `attack_ms` - Attack time in milliseconds
    /// * `decay_ms` - Decay time in milliseconds
    /// * `sustain` - Sustain level (0.0 to 1.0)
    /// * `release_ms` - Release time in milliseconds
    #[must_use]
    pub fn new(
        sample_rate: f32,
        attack_ms: f32,
        decay_ms: f32,
        sustain: f32,
        release_ms: f32,
    ) -> Self {
        let mut adsr = Self {
            stage: AdsrStage::Idle,
            level: 0.0,
            attack_coeff: 0.0,
            decay_coeff: 0.0,
            sustain: sustain.clamp(0.0, 1.0),
            release_coeff: 0.0,
            sample_rate,
        };
        adsr.set_attack(attack_ms);
        adsr.set_decay(decay_ms);
        adsr.set_release(release_ms);
        adsr
    }

    fn time_to_coeff(sample_rate: f32, time_ms: f32) -> f32 {
        if time_ms <= 0.0 {
            return 0.0;
        }
        let samples = time_ms * sample_rate / 1000.0;
        (-1.0 / samples).exp()
    }

    /// Set attack time.
    pub fn set_attack(&mut self, attack_ms: f32) {
        self.attack_coeff = Self::time_to_coeff(self.sample_rate, attack_ms);
    }

    /// Set decay time.
    pub fn set_decay(&mut self, decay_ms: f32) {
        self.decay_coeff = Self::time_to_coeff(self.sample_rate, decay_ms);
    }

    /// Set sustain level.
    pub fn set_sustain(&mut self, sustain: f32) {
        self.sustain = sustain.clamp(0.0, 1.0);
    }

    /// Set release time.
    pub fn set_release(&mut self, release_ms: f32) {
        self.release_coeff = Self::time_to_coeff(self.sample_rate, release_ms);
    }

    /// Trigger the envelope (start attack phase).
    pub fn trigger(&mut self) {
        self.stage = AdsrStage::Attack;
    }

    /// Release the envelope (start release phase).
    pub fn release(&mut self) {
        if self.stage != AdsrStage::Idle {
            self.stage = AdsrStage::Release;
        }
    }

    /// Force the envelope to idle immediately.
    pub fn reset(&mut self) {
        self.stage = AdsrStage::Idle;
        self.level = 0.0;
    }

    /// Get the current stage.
    #[must_use]
    pub fn stage(&self) -> AdsrStage {
        self.stage
    }

    /// Check if the envelope is active (not idle).
    #[must_use]
    pub fn is_active(&self) -> bool {
        self.stage != AdsrStage::Idle
    }

    /// Get the current envelope level without advancing.
    #[must_use]
    pub fn current(&self) -> f32 {
        self.level
    }

    /// Generate the next envelope sample.
    #[must_use]
    pub fn next(&mut self) -> f32 {
        match self.stage {
            AdsrStage::Idle => {
                self.level = 0.0;
            }
            AdsrStage::Attack => {
                // Approach 1.0 asymptotically, using a target slightly above 1.0
                // to ensure we reach 1.0 in reasonable time
                const ATTACK_TARGET: f32 = 1.2;
                self.level = ATTACK_TARGET + self.attack_coeff * (self.level - ATTACK_TARGET);
                if self.level >= 1.0 {
                    self.level = 1.0;
                    self.stage = AdsrStage::Decay;
                }
            }
            AdsrStage::Decay => {
                self.level = self.sustain + self.decay_coeff * (self.level - self.sustain);
                if (self.level - self.sustain).abs() < 0.0001 {
                    self.level = self.sustain;
                    self.stage = AdsrStage::Sustain;
                }
            }
            AdsrStage::Sustain => {
                self.level = self.sustain;
            }
            AdsrStage::Release => {
                self.level = self.release_coeff * self.level;
                if self.level < 0.0001 {
                    self.level = 0.0;
                    self.stage = AdsrStage::Idle;
                }
            }
        }
        self.level
    }
}

impl Default for Adsr {
    fn default() -> Self {
        Self::new(44100.0, 10.0, 100.0, 0.7, 200.0)
    }
}

// ============================================================================
// AR Envelope (simpler Attack-Release)
// ============================================================================

/// Simple Attack-Release envelope.
///
/// A simplified envelope without decay/sustain - useful for
/// tremolo, ducking, and simple amplitude envelopes.
#[derive(Clone, Debug)]
pub struct ArEnvelope {
    level: f32,
    target: f32,
    attack_coeff: f32,
    release_coeff: f32,
    sample_rate: f32,
}

impl ArEnvelope {
    /// Create a new AR envelope.
    #[must_use]
    pub fn new(sample_rate: f32, attack_ms: f32, release_ms: f32) -> Self {
        let mut env = Self {
            level: 0.0,
            target: 0.0,
            attack_coeff: 0.0,
            release_coeff: 0.0,
            sample_rate,
        };
        env.set_attack(attack_ms);
        env.set_release(release_ms);
        env
    }

    fn time_to_coeff(sample_rate: f32, time_ms: f32) -> f32 {
        if time_ms <= 0.0 {
            return 0.0;
        }
        let samples = time_ms * sample_rate / 1000.0;
        (-1.0 / samples).exp()
    }

    /// Set attack time.
    pub fn set_attack(&mut self, attack_ms: f32) {
        self.attack_coeff = Self::time_to_coeff(self.sample_rate, attack_ms);
    }

    /// Set release time.
    pub fn set_release(&mut self, release_ms: f32) {
        self.release_coeff = Self::time_to_coeff(self.sample_rate, release_ms);
    }

    /// Set target level (envelope will smoothly approach this).
    pub fn set_target(&mut self, target: f32) {
        self.target = target;
    }

    /// Trigger (set target to 1.0).
    pub fn trigger(&mut self) {
        self.target = 1.0;
    }

    /// Release (set target to 0.0).
    pub fn release(&mut self) {
        self.target = 0.0;
    }

    /// Get current level.
    #[must_use]
    pub fn current(&self) -> f32 {
        self.level
    }

    /// Generate next sample.
    #[must_use]
    pub fn next(&mut self) -> f32 {
        let coeff = if self.target > self.level {
            self.attack_coeff
        } else {
            self.release_coeff
        };
        self.level = self.target + coeff * (self.level - self.target);
        self.level
    }

    /// Reset to zero.
    pub fn reset(&mut self) {
        self.level = 0.0;
        self.target = 0.0;
    }
}

impl Default for ArEnvelope {
    fn default() -> Self {
        Self::new(44100.0, 10.0, 100.0)
    }
}

// ============================================================================
// ModSource Implementations
// ============================================================================

use crate::core::ModSource;

impl ModSource for Lfo {
    fn next(&mut self) -> f32 {
        Lfo::next(self)
    }

    fn current(&self) -> f32 {
        Lfo::current(self)
    }

    fn reset(&mut self) {
        Lfo::reset(self);
    }

    fn set_sample_rate(&mut self, sample_rate: f32) {
        // Preserve current frequency when changing sample rate
        let freq = self.phase_inc * self.sample_rate;
        self.sample_rate = sample_rate;
        self.phase_inc = freq / sample_rate;
    }

    fn is_bipolar(&self) -> bool {
        true // LFO outputs -1 to 1
    }
}

impl ModSource for Adsr {
    fn next(&mut self) -> f32 {
        Adsr::next(self)
    }

    fn current(&self) -> f32 {
        Adsr::current(self)
    }

    fn reset(&mut self) {
        Adsr::reset(self);
    }

    fn set_sample_rate(&mut self, sample_rate: f32) {
        self.sample_rate = sample_rate;
        // Note: coefficients would need to be recalculated
        // This is a limitation - ideally we'd store times and recalculate
    }

    fn is_bipolar(&self) -> bool {
        false // ADSR outputs 0 to 1
    }
}

impl ModSource for ArEnvelope {
    fn next(&mut self) -> f32 {
        ArEnvelope::next(self)
    }

    fn current(&self) -> f32 {
        ArEnvelope::current(self)
    }

    fn reset(&mut self) {
        ArEnvelope::reset(self);
    }

    fn set_sample_rate(&mut self, sample_rate: f32) {
        self.sample_rate = sample_rate;
        // Note: coefficients would need to be recalculated
    }

    fn is_bipolar(&self) -> bool {
        false // AR outputs 0 to 1
    }
}
