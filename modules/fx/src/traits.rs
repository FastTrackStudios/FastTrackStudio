//! Core traits for DSP blocks.

/// Core trait for DSP blocks.
///
/// A block is a self-contained audio processor that can be connected
/// in a chain with other blocks.
pub trait Block: Send {
    /// The parameter type for this block.
    type Params: Params;

    /// Process audio buffers.
    ///
    /// # Arguments
    /// * `input` - Input audio channels (may be empty for generators)
    /// * `output` - Output audio channels to fill
    /// * `params` - Current parameter values
    fn process(&mut self, input: &[&[f32]], output: &mut [&mut [f32]], params: &Self::Params);

    /// Latency in samples (for delay compensation).
    fn latency(&self) -> usize {
        0
    }

    /// Reset internal state (e.g., on transport stop or bypass toggle).
    fn reset(&mut self);

    /// Called when sample rate changes.
    fn set_sample_rate(&mut self, sample_rate: f32);

    /// Number of input channels this block expects.
    fn input_channels(&self) -> usize;

    /// Number of output channels this block produces.
    fn output_channels(&self) -> usize;
}

/// Parameters for a block.
///
/// Parameters define the controllable aspects of a block and support
/// preset loading.
pub trait Params: Default + Clone + Send + Sync {
    /// Load parameters from a preset name.
    ///
    /// Returns `None` if the preset doesn't exist.
    fn from_preset(preset: &str) -> Option<Self>;

    /// List available preset names.
    fn preset_names() -> &'static [&'static str];
}
