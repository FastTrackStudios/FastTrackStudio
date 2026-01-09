//! Envelope detection primitives.
//!
//! This module provides types for detecting signal envelopes, useful for
//! dynamics processing (compressors, limiters, gates) and modulation effects.
//!
//! ## Components
//!
//! - **Rectifiers**: Convert bipolar signals to unipolar
//!   - [`FullWave`] - Absolute value (most common)
//!   - [`PositiveHalfWave`] - Only positive samples
//!   - [`NegativeHalfWave`] - Only negative samples
//!
//! - **Detectors**: Extract envelope from signal
//!   - [`PeakDetect`] - Peak-based detection wrapper
//!   - [`Rms`] - RMS (root mean square) detection
//!   - [`EnvelopeDetector`] - Combines detector with attack/release smoothing
//!
//! ## Example
//!
//! ```ignore
//! use fts_fx::primitives::envelope::{peak_detector_mono, time_to_frames};
//!
//! let sample_rate = 44100.0;
//! let attack_frames = time_to_frames(0.01, sample_rate);  // 10ms
//! let release_frames = time_to_frames(0.1, sample_rate);  // 100ms
//!
//! let mut env = peak_detector_mono(attack_frames, release_frames);
//! let envelope = env.next([0.5f32]); // Feed a sample, get envelope value
//! ```

use dasp_envelope::detect::{Detect, Detector, Peak};
use dasp_frame::Frame;
use dasp_ring_buffer as rb;

// Re-export rectifier types from dasp_peak
pub use dasp_peak::{FullWave, NegativeHalfWave, PositiveHalfWave, Rectifier};

// Re-export the core detect trait
pub use dasp_envelope::detect::Detect as DetectTrait;

/// Peak detector type - wraps a rectifier for peak detection.
pub type PeakDetect<R = FullWave> = Peak<R>;

/// RMS (root mean square) envelope detection.
///
/// Calculates the RMS of the signal over a sliding window.
/// The type parameter `S` is the ring buffer storage for the RMS window.
pub type Rms<F, S> = dasp_rms::Rms<F, S>;

/// Envelope detector combining detection with attack/release smoothing.
///
/// - `F` is the frame type (e.g., `[f32; 1]` for mono, `[f32; 2]` for stereo)
/// - `D` is the detector type (e.g., `PeakDetect<FullWave>`)
pub type EnvelopeDetector<F, D> = Detector<F, D>;

/// Create a peak envelope detector for mono signals.
///
/// # Arguments
/// * `attack_frames` - Attack time in frames (use `time_to_frames` to convert from seconds)
/// * `release_frames` - Release time in frames
#[must_use]
pub fn peak_detector_mono(attack_frames: f32, release_frames: f32) -> EnvelopeDetector<[f32; 1], PeakDetect<FullWave>> {
    Detector::peak(attack_frames, release_frames)
}

/// Create a peak envelope detector for stereo signals.
///
/// # Arguments
/// * `attack_frames` - Attack time in frames
/// * `release_frames` - Release time in frames
#[must_use]
pub fn peak_detector_stereo(attack_frames: f32, release_frames: f32) -> EnvelopeDetector<[f32; 2], PeakDetect<FullWave>> {
    Detector::peak(attack_frames, release_frames)
}

/// Create an RMS detector with the given window.
///
/// # Arguments
/// * `window` - Ring buffer for the RMS calculation window
///
/// Larger windows provide smoother envelope but introduce more latency.
#[must_use]
pub fn rms<F, S>(window: rb::Fixed<S>) -> Rms<F, S>
where
    F: Frame,
    S: rb::Slice<Element = F::Float>,
{
    Rms::new(window)
}

/// Convert time in seconds to frames.
///
/// # Arguments
/// * `time_seconds` - Time in seconds
/// * `sample_rate` - Sample rate in Hz
#[must_use]
pub fn time_to_frames(time_seconds: f32, sample_rate: f32) -> f32 {
    time_seconds * sample_rate
}

/// Convert RMS window time to sample count.
///
/// # Arguments
/// * `time_seconds` - Window duration in seconds
/// * `sample_rate` - Sample rate in Hz
#[must_use]
pub fn rms_window_samples(time_seconds: f32, sample_rate: f32) -> usize {
    (time_seconds * sample_rate).ceil() as usize
}
