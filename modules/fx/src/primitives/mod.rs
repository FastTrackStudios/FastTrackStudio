//! Low-level DSP primitives.
//!
//! These are the building blocks used to construct higher-level effect blocks.
//! Most primitives are thin wrappers around the `dasp` crate or custom implementations.
//!
//! ## Core Types (re-exported from dasp)
//!
//! - [`Sample`] - Trait for audio sample types (f32, i16, etc.)
//! - [`Frame`] - Multi-channel audio at a single point in time
//!
//! ## Primitives
//!
//! - [`delay`] - Delay lines and ring buffers
//! - [`resample`] - Sample rate interpolation (dasp)
//! - [`oversample`] - High-quality oversampling (rubato)
//! - [`envelope`] - Envelope detection (peak, RMS)
//! - [`filters`] - Biquad, SVF, one-pole filters
//! - [`modulation`] - LFOs, smoothers, ADSR envelopes

pub mod delay;
pub mod envelope;
pub mod filters;
pub mod modulation;
pub mod oversample;
pub mod resample;

// Re-export core dasp types that are fundamental to DSP
pub use dasp_frame::Frame;
pub use dasp_sample::Sample;

// Re-export delay primitives
pub use delay::{BoundedBuffer, FixedBuffer, RingBuffer};

// Re-export envelope primitives
pub use envelope::{EnvelopeDetector, FullWave, NegativeHalfWave, PeakDetect, PositiveHalfWave, Rms};

// Re-export resampling primitives
pub use resample::{FloorInterpolator, Interpolator, LinearInterpolator, SincInterpolator};

// Re-export filter primitives
pub use filters::{Biquad, BiquadCoeffs, DcBlocker, OnePole, Svf, SvfOutput};

// Re-export modulation primitives
pub use modulation::{Adsr, AdsrStage, ArEnvelope, Lfo, LfoWaveform, Smoother};

// Re-export oversampling primitives
pub use oversample::{OversampleFactor, OversampleQuality, Oversampler};
