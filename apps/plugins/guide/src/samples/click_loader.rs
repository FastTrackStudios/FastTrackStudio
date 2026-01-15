//! Click sample loading logic
//!
//! Handles loading click samples (beat, eighth, sixteenth, triplet, accent) based on selected click sound.

use std::sync::{Arc, Mutex};
use symphonium::DecodedAudioF32;
use tracing::{debug, info, warn};

use crate::ClickSound;

/// Get the base path for click samples from environment or use default
fn get_click_base_path() -> String {
    std::env::var("FTS_GUIDE_CLICK_PATH").unwrap_or_else(|_| {
        // Default path - user should set FTS_GUIDE_CLICK_PATH environment variable
        "/Users/codywright/Music/Audiohaven/Sample Libraries/FTS-GUIDE/Click/".to_string()
    })
}

/// Click sample file paths for a specific click sound
#[derive(Debug, Clone)]
pub struct ClickSamplePaths {
    pub beat_path: String,
    pub eighth_path: String,
    pub sixteenth_path: String,
    pub accent_path: String,
}

impl ClickSamplePaths {
    /// Get file paths for a given click sound
    pub fn for_sound(click_sound: ClickSound) -> Self {
        let base_path = get_click_base_path();

        let (sound_dir, beat_file, eighth_file, sixteenth_file, accent_file) = match click_sound {
            ClickSound::Blip => (
                "Blip",
                "Blip-Quarter.wav",
                "Blip-Eighth.wav",
                "Blip-Sixteenth.wav",
                "Blip-Accent.wav",
            ),
            ClickSound::Classic => (
                "Classic",
                "Classic-Quarter.wav",
                "Classic-Eighth.wav",
                "Classic-Sixteenth.wav",
                "Classic-Accent.wav",
            ),
            ClickSound::Cowbell => (
                "Cowbell",
                "New Click -  Cowbell-quarter.wav",
                "New Click -  Cowbell-eighth.wav",
                "New Click -  Cowbell-sixteenth.wav",
                "New Click -  Cowbell-accents.wav",
            ),
            ClickSound::Digital => (
                "Digital",
                "New Click -  Digital-accents.wav",
                "New Click -  Digital-accents.wav",
                "New Click -  Digital-sixteenth.wav",
                "New Click -  Digital-accents.wav",
            ),
            ClickSound::Gentle => (
                "Gentle",
                "New Click -  Gentle-quarter.wav",
                "New Click -  Gentle-eighth.wav",
                "New Click -  Gentle-sixteenth.wav",
                "New Click -  Gentle-accents.wav",
            ),
            ClickSound::Percussive => (
                "Percussive",
                "New Click -  Percussive-quarter.wav",
                "New Click -  Percussive-eighth.wav",
                "New Click -  Percussive-sixteenth.wav",
                "New Click -  Percussive-accents.wav",
            ),
            ClickSound::Saw => (
                "Saw",
                "New Click -  Saw-quarter.wav",
                "New Click -  Saw-eighth.wav",
                "New Click -  Saw-quarter.wav",
                "New Click -  Saw-accents.wav",
            ),
            ClickSound::Woodblock => (
                "Woodblock",
                "New Click -  Woodblock-quarter.wav",
                "New Click -  Woodblock-eighth.wav",
                "New Click -  Woodblock-sixteenth.wav",
                "New Click -  Woodblock-accents.wav",
            ),
        };

        Self {
            beat_path: format!("{}{}/{}", base_path, sound_dir, beat_file),
            eighth_path: format!("{}{}/{}", base_path, sound_dir, eighth_file),
            sixteenth_path: format!("{}{}/{}", base_path, sound_dir, sixteenth_file),
            accent_path: format!("{}{}/{}", base_path, sound_dir, accent_file),
        }
    }
}

/// Click sample loader
pub struct ClickSampleLoader;

impl ClickSampleLoader {
    /// Load click samples based on selected click sound
    ///
    /// Loads beat, eighth, sixteenth, and accent samples into the provided storage locations.
    /// Uses a static cache to avoid reloading samples that have already been loaded.
    #[allow(clippy::too_many_arguments)]
    pub fn load_samples(
        click_sound: ClickSound,
        sample_rate: f32,
        sample_data_beat: &Arc<Mutex<Option<DecodedAudioF32>>>,
        sample_data_eighth: &Arc<Mutex<Option<DecodedAudioF32>>>,
        sample_data_sixteenth: &Arc<Mutex<Option<DecodedAudioF32>>>,
        sample_data_triplet: &Arc<Mutex<Option<DecodedAudioF32>>>,
        sample_data_measure_accent: &Arc<Mutex<Option<DecodedAudioF32>>>,
        num_channels: &mut u32,
        sample_length: &mut usize,
    ) {
        // Use cache to get or load samples
        let (beat, eighth, sixteenth, triplet, accent, cached_channels, cached_length) =
            super::cache::SampleCache::get_or_load_click_samples(click_sound, sample_rate, || {
                let paths = ClickSamplePaths::for_sound(click_sound);
                let mut beat = None;
                let mut eighth = None;
                let mut sixteenth = None;
                let mut triplet = None;
                let mut accent = None;
                let mut num_channels_local = 2;
                let mut sample_length_local = 0;

                // Load all click samples
                info!(
                    click_sound = ?click_sound,
                    sample_rate,
                    "Loading click samples for sound"
                );

                // Beat sample
                debug!(path = %paths.beat_path, "Loading beat sample");
                match super::loader::SampleLoader::load_file(&paths.beat_path, sample_rate) {
                    Ok(decoded_audio) => {
                        num_channels_local = decoded_audio.channels() as u32;
                        sample_length_local = decoded_audio.frames();
                        beat = Some(decoded_audio);
                        info!("Loaded beat sample");
                    }
                    Err(e) => {
                        warn!(error = %e, path = %paths.beat_path, "Failed to load beat sample");
                    }
                }

                // Eighth sample
                debug!(path = %paths.eighth_path, "Loading eighth sample");
                match super::loader::SampleLoader::load_file(&paths.eighth_path, sample_rate) {
                    Ok(decoded_audio) => {
                        eighth = Some(decoded_audio);
                        info!("Loaded eighth sample");
                    }
                    Err(e) => {
                        warn!(error = %e, path = %paths.eighth_path, "Failed to load eighth sample");
                    }
                }

                // Sixteenth sample
                debug!(path = %paths.sixteenth_path, "Loading sixteenth sample");
                match super::loader::SampleLoader::load_file(&paths.sixteenth_path, sample_rate) {
                    Ok(decoded_audio) => {
                        sixteenth = Some(decoded_audio);
                        info!("Loaded sixteenth sample");
                    }
                    Err(e) => {
                        warn!(error = %e, path = %paths.sixteenth_path, "Failed to load sixteenth sample");
                    }
                }

                // Triplet sample (use eighth as fallback)
                if let Some(ref eighth_audio) = eighth {
                    triplet = Some(eighth_audio.clone());
                }

                // Accent sample
                debug!(path = %paths.accent_path, "Loading accent sample");
                match super::loader::SampleLoader::load_file(&paths.accent_path, sample_rate) {
                    Ok(decoded_audio) => {
                        accent = Some(decoded_audio);
                        info!("Loaded accent sample");
                    }
                    Err(e) => {
                        warn!(error = %e, path = %paths.accent_path, "Failed to load accent sample");
                    }
                }

                (
                    beat,
                    eighth,
                    sixteenth,
                    triplet,
                    accent,
                    num_channels_local,
                    sample_length_local,
                )
            });

        // Store loaded samples
        *sample_data_beat.lock().unwrap() = beat;
        *sample_data_eighth.lock().unwrap() = eighth;
        *sample_data_sixteenth.lock().unwrap() = sixteenth;
        *sample_data_triplet.lock().unwrap() = triplet;
        *sample_data_measure_accent.lock().unwrap() = accent;
        *num_channels = cached_channels;
        *sample_length = cached_length;
    }
}
