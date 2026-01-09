//! Resampling primitives (upsampling, downsampling).
//!
//! This module provides interpolation types for sample rate conversion.
//! These are thin wrappers around `dasp_interpolate`, allowing implementation
//! swapping in the future.
//!
//! ## Interpolator Types
//!
//! - [`FloorInterpolator`] - Nearest-neighbor (lowest quality, fastest)
//! - [`LinearInterpolator`] - Linear interpolation (good balance)
//! - [`SincInterpolator`] - Sinc interpolation (highest quality, slowest)
//!
//! ## Example
//!
//! ```ignore
//! use fts_fx::primitives::resample::{Interpolator, LinearInterpolator};
//!
//! // Create a linear interpolator for mono audio
//! let mut interp = LinearInterpolator::new([0.0f32], [0.0f32]);
//!
//! // Feed source frames and get interpolated values
//! interp.next_source_frame([1.0]);
//! let interpolated = interp.interpolate(0.5); // Halfway between frames
//! ```

use dasp_frame::Frame;
use dasp_interpolate as interp;

/// Trait for sample rate interpolation.
///
/// Interpolators maintain state about surrounding samples and can produce
/// interpolated values at fractional positions between samples.
pub use interp::Interpolator;

/// Nearest-neighbor (floor) interpolation.
///
/// Simply returns the previous sample. Fastest but lowest quality.
/// Suitable for lo-fi effects or when CPU is constrained.
pub type FloorInterpolator<F> = interp::floor::Floor<F>;

/// Linear interpolation between samples.
///
/// Interpolates linearly between adjacent samples. Good balance of
/// quality and performance for most use cases.
pub type LinearInterpolator<F> = interp::linear::Linear<F>;

/// Sinc interpolation (windowed sinc).
///
/// Highest quality interpolation using a sinc function with a
/// configurable ring buffer for the interpolation window.
///
/// The type parameter `S` is the ring buffer storage type.
/// The frame type is inferred from `S::Element`.
pub type SincInterpolator<S> = interp::sinc::Sinc<S>;

/// Create a floor (nearest-neighbor) interpolator.
///
/// # Arguments
/// * `left` - The initial "left" frame (previous sample)
#[must_use]
pub fn floor<F: Frame>(left: F) -> FloorInterpolator<F> {
    FloorInterpolator::new(left)
}

/// Create a linear interpolator.
///
/// # Arguments
/// * `left` - The initial "left" frame (previous sample)
/// * `right` - The initial "right" frame (next sample)
#[must_use]
pub fn linear<F: Frame>(left: F, right: F) -> LinearInterpolator<F> {
    LinearInterpolator::new(left, right)
}

/// Create a sinc interpolator with the given ring buffer.
///
/// The ring buffer determines the quality of the interpolation -
/// larger buffers provide better quality but use more CPU.
/// The buffer length must be a multiple of 2.
///
/// # Arguments
/// * `ring_buffer` - A fixed ring buffer for the sinc window
#[must_use]
pub fn sinc<S>(ring_buffer: dasp_ring_buffer::Fixed<S>) -> SincInterpolator<S>
where
    S: dasp_ring_buffer::SliceMut,
    S::Element: Frame,
{
    SincInterpolator::new(ring_buffer)
}

/// Convenience function to create a mono linear interpolator starting at zero.
#[must_use]
pub fn linear_mono() -> LinearInterpolator<[f32; 1]> {
    LinearInterpolator::new([0.0], [0.0])
}

/// Convenience function to create a stereo linear interpolator starting at zero.
#[must_use]
pub fn linear_stereo() -> LinearInterpolator<[f32; 2]> {
    LinearInterpolator::new([0.0, 0.0], [0.0, 0.0])
}
