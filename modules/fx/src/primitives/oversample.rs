//! Oversampling for anti-aliased non-linear processing.
//!
//! Uses rubato for high-quality sinc interpolation with SIMD acceleration.
//! This is essential for distortion/saturation effects to prevent aliasing.
//!
//! ## Example
//!
//! ```ignore
//! use fts_fx::primitives::{Oversampler, OversampleFactor, OversampleQuality};
//!
//! // Create 4x oversampler for mono, max 512 sample blocks
//! let mut os = Oversampler::new(
//!     OversampleFactor::X4,
//!     OversampleQuality::Normal,
//!     512,
//!     1,
//! );
//!
//! // Process with a saturation function
//! os.process(&input, &mut output, |buf| {
//!     for sample in buf.iter_mut() {
//!         *sample = sample.tanh();
//!     }
//! });
//! ```

use rubato::{
    Async, FixedAsync, Resampler, SincInterpolationParameters, SincInterpolationType,
    WindowFunction,
};

/// Oversampling factors.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum OversampleFactor {
    /// 2x oversampling
    #[default]
    X2,
    /// 4x oversampling
    X4,
    /// 8x oversampling
    X8,
    /// 16x oversampling
    X16,
}

impl OversampleFactor {
    /// Get the numeric factor.
    #[must_use]
    pub const fn as_usize(self) -> usize {
        match self {
            Self::X2 => 2,
            Self::X4 => 4,
            Self::X8 => 8,
            Self::X16 => 16,
        }
    }
}

/// Quality presets for oversampling.
///
/// Higher quality = longer sinc filter = more latency + CPU.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum OversampleQuality {
    /// Fast - shorter sinc, linear interpolation.
    /// Good for real-time with tight CPU budget.
    Fast,
    /// Balanced quality/performance (default).
    /// Suitable for most real-time use cases.
    #[default]
    Normal,
    /// High quality - longer sinc, cubic interpolation.
    /// Use for offline processing or when quality is paramount.
    High,
}

impl OversampleQuality {
    /// Get sinc interpolation parameters for this quality level.
    fn sinc_params(self) -> SincInterpolationParameters {
        match self {
            Self::Fast => SincInterpolationParameters {
                sinc_len: 64,
                f_cutoff: 0.915,
                interpolation: SincInterpolationType::Linear,
                oversampling_factor: 128,
                window: WindowFunction::Blackman,
            },
            Self::Normal => SincInterpolationParameters {
                sinc_len: 128,
                f_cutoff: 0.925,
                interpolation: SincInterpolationType::Linear,
                oversampling_factor: 256,
                window: WindowFunction::BlackmanHarris2,
            },
            Self::High => SincInterpolationParameters {
                sinc_len: 256,
                f_cutoff: 0.95,
                interpolation: SincInterpolationType::Cubic,
                oversampling_factor: 256,
                window: WindowFunction::BlackmanHarris2,
            },
        }
    }
}

/// High-quality oversampler using rubato sinc interpolation.
///
/// Wraps upsample → process → downsample with proper anti-aliasing.
/// All buffers are pre-allocated for real-time safety.
pub struct Oversampler {
    factor: usize,
    channels: usize,
    max_block_size: usize,
    upsampler: Async<f32>,
    downsampler: Async<f32>,
    // Work buffers (channel-major: Vec<channel_data>)
    up_output: Vec<Vec<f32>>,
    down_output: Vec<Vec<f32>>,
}

impl Oversampler {
    /// Create a new oversampler.
    ///
    /// # Arguments
    /// * `factor` - Oversampling factor (2, 4, 8, or 16)
    /// * `quality` - Quality preset affecting latency and CPU usage
    /// * `max_block_size` - Maximum input block size to process
    /// * `channels` - Number of audio channels (1 = mono, 2 = stereo)
    ///
    /// # Panics
    /// Panics if `channels` is 0 or `max_block_size` is 0.
    #[must_use]
    pub fn new(
        factor: OversampleFactor,
        quality: OversampleQuality,
        max_block_size: usize,
        channels: usize,
    ) -> Self {
        assert!(channels > 0, "channels must be > 0");
        assert!(max_block_size > 0, "max_block_size must be > 0");

        let factor_val = factor.as_usize();
        let params = quality.sinc_params();

        // Upsampler: fixed input size, variable output
        // ratio = factor (e.g., 4.0 for 4x upsampling)
        let upsampler = Async::<f32>::new_sinc(
            factor_val as f64,
            1.1, // max_relative_ratio - small headroom
            &params,
            max_block_size,
            channels,
            FixedAsync::Input,
        )
        .expect("Failed to create upsampler");

        // Downsampler: variable input, fixed output size
        // ratio = 1/factor (e.g., 0.25 for 4x downsampling)
        let downsampler = Async::<f32>::new_sinc(
            1.0 / factor_val as f64,
            1.1,
            &params,
            max_block_size,
            channels,
            FixedAsync::Output,
        )
        .expect("Failed to create downsampler");

        // Pre-allocate output buffers
        let up_output_size = max_block_size * factor_val + params.sinc_len;
        let up_output = vec![vec![0.0; up_output_size]; channels];
        let down_output = vec![vec![0.0; max_block_size + params.sinc_len]; channels];

        Self {
            factor: factor_val,
            channels,
            max_block_size,
            upsampler,
            downsampler,
            up_output,
            down_output,
        }
    }

    /// Process mono audio through oversampling with a callback.
    ///
    /// The callback receives the oversampled buffer to process in-place.
    /// This is the main entry point for mono effects.
    ///
    /// # Arguments
    /// * `input` - Input samples (length <= max_block_size)
    /// * `output` - Output buffer (same length as input)
    /// * `process` - Callback that processes the oversampled data in-place
    ///
    /// # Panics
    /// Panics if this oversampler was created with channels != 1.
    pub fn process<F>(&mut self, input: &[f32], output: &mut [f32], mut process: F)
    where
        F: FnMut(&mut [f32]),
    {
        assert_eq!(self.channels, 1, "Use process_interleaved for multi-channel");
        assert!(
            input.len() <= self.max_block_size,
            "Input exceeds max_block_size"
        );

        // Wrap input in channel format
        let input_channels = [input];

        // Upsample
        let (_, up_frames) = self
            .upsampler
            .process_into_buffer(&input_channels, &mut self.up_output, None)
            .expect("Upsampling failed");

        // Process the oversampled buffer
        process(&mut self.up_output[0][..up_frames]);

        // Prepare downsampler input (slice to actual frames)
        let down_input: Vec<&[f32]> = self
            .up_output
            .iter()
            .map(|ch| &ch[..up_frames])
            .collect();

        // Downsample
        let (_, down_frames) = self
            .downsampler
            .process_into_buffer(&down_input, &mut self.down_output, None)
            .expect("Downsampling failed");

        // Copy to output
        let copy_len = output.len().min(down_frames);
        output[..copy_len].copy_from_slice(&self.down_output[0][..copy_len]);
    }

    /// Process stereo audio through oversampling.
    ///
    /// # Arguments
    /// * `input_l`, `input_r` - Input samples for left and right channels
    /// * `output_l`, `output_r` - Output buffers
    /// * `process` - Callback that processes oversampled L/R data in-place
    ///
    /// # Panics
    /// Panics if this oversampler was created with channels != 2.
    pub fn process_stereo<F>(
        &mut self,
        input_l: &[f32],
        input_r: &[f32],
        output_l: &mut [f32],
        output_r: &mut [f32],
        mut process: F,
    ) where
        F: FnMut(&mut [f32], &mut [f32]),
    {
        assert_eq!(self.channels, 2, "Oversampler not configured for stereo");
        assert!(
            input_l.len() <= self.max_block_size,
            "Input exceeds max_block_size"
        );

        // Wrap inputs in channel format
        let input_channels = [input_l, input_r];

        // Upsample
        let (_, up_frames) = self
            .upsampler
            .process_into_buffer(&input_channels, &mut self.up_output, None)
            .expect("Upsampling failed");

        // Process oversampled buffers
        process(
            &mut self.up_output[0][..up_frames],
            &mut self.up_output[1][..up_frames],
        );

        // Prepare downsampler input
        let down_input: Vec<&[f32]> = self
            .up_output
            .iter()
            .map(|ch| &ch[..up_frames])
            .collect();

        // Downsample
        let (_, down_frames) = self
            .downsampler
            .process_into_buffer(&down_input, &mut self.down_output, None)
            .expect("Downsampling failed");

        // Copy to outputs
        let copy_len = output_l.len().min(down_frames);
        output_l[..copy_len].copy_from_slice(&self.down_output[0][..copy_len]);
        output_r[..copy_len].copy_from_slice(&self.down_output[1][..copy_len]);
    }

    /// Get the latency in input samples.
    ///
    /// This is the delay introduced by the oversampling process.
    /// Use for delay compensation in plugin contexts.
    #[must_use]
    pub fn latency(&self) -> usize {
        // Latency comes from both the upsampler and downsampler filters
        // rubato reports delay in output frames, we need input frames
        let up_delay = self.upsampler.output_delay();
        let down_delay = self.downsampler.output_delay();

        // Upsampler delay is in upsampled frames, convert to input frames
        // Downsampler delay is already in output (input-rate) frames
        (up_delay / self.factor) + down_delay
    }

    /// Reset internal state.
    ///
    /// Call this when starting a new audio stream or after a discontinuity.
    pub fn reset(&mut self) {
        self.upsampler.reset();
        self.downsampler.reset();

        // Clear work buffers
        for ch in &mut self.up_output {
            ch.fill(0.0);
        }
        for ch in &mut self.down_output {
            ch.fill(0.0);
        }
    }

    /// Get the oversampling factor.
    #[must_use]
    pub const fn factor(&self) -> usize {
        self.factor
    }

    /// Get the number of channels.
    #[must_use]
    pub const fn channels(&self) -> usize {
        self.channels
    }

    /// Get the maximum block size.
    #[must_use]
    pub const fn max_block_size(&self) -> usize {
        self.max_block_size
    }
}

impl core::fmt::Debug for Oversampler {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("Oversampler")
            .field("factor", &self.factor)
            .field("channels", &self.channels)
            .field("max_block_size", &self.max_block_size)
            .field("latency", &self.latency())
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_oversampler_creation() {
        let os = Oversampler::new(OversampleFactor::X4, OversampleQuality::Normal, 512, 1);
        assert_eq!(os.factor(), 4);
        assert_eq!(os.channels(), 1);
        assert_eq!(os.max_block_size(), 512);
    }

    #[test]
    fn test_mono_passthrough() {
        let mut os = Oversampler::new(OversampleFactor::X2, OversampleQuality::Fast, 64, 1);

        let input = vec![0.5; 64];
        let mut output = vec![0.0; 64];

        // Process with identity function
        os.process(&input, &mut output, |_buf| {
            // No-op: just pass through
        });

        // Output should be close to input (some latency/filter artifacts expected)
        // This is a basic sanity check, not a precision test
        assert!(output.iter().any(|&x| x.abs() > 0.01));
    }

    #[test]
    fn test_stereo_creation() {
        let os = Oversampler::new(OversampleFactor::X4, OversampleQuality::High, 256, 2);
        assert_eq!(os.channels(), 2);
    }
}
