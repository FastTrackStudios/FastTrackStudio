//! Base sample loading utilities
//!
//! Provides generic sample loading functionality using Symphonium.

use std::error::Error;
use symphonium::{DecodedAudioF32, ResampleQuality, SymphoniumLoader};

/// Generic sample loader
pub struct SampleLoader;

impl SampleLoader {
    /// Load an audio file using Symphonium, which handles resampling automatically
    ///
    /// `target_sample_rate` should match the plugin's current sample rate
    pub fn load_file(
        path: &str,
        target_sample_rate: f32,
    ) -> Result<DecodedAudioF32, Box<dyn Error>> {
        let mut loader = SymphoniumLoader::new();

        // Load the audio file with resampling to match plugin sample rate
        let decoded_audio = loader.load_f32(
            path,
            Some(target_sample_rate as u32),
            ResampleQuality::default(),
            None, // Use default max size (1GB)
        )?;

        Ok(decoded_audio)
    }

    /// Load a count sample (1-8) from a file path
    pub fn load_count_sample(
        path: &str,
        target_sample_rate: f32,
    ) -> Result<DecodedAudioF32, Box<dyn Error>> {
        Self::load_file(path, target_sample_rate)
    }
}
