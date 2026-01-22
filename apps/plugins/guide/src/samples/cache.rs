//! Sample cache management
//!
//! Provides utilities for managing cached audio samples.

use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use symphonium::DecodedAudioF32;
use tracing::info;

use crate::ClickSound;

/// Cache key for click samples: (click_sound, sample_rate)
#[derive(Hash, PartialEq, Eq, Clone, Debug)]
struct ClickSampleCacheKey(ClickSound, u32);

/// Cached click samples - stores all samples for a given (click_sound, sample_rate)
#[derive(Clone)]
struct CachedClickSamples {
    beat: DecodedAudioF32,
    eighth: DecodedAudioF32,
    sixteenth: DecodedAudioF32,
    triplet: DecodedAudioF32,
    accent: DecodedAudioF32,
    num_channels: u32,
    sample_length: usize,
}

/// Static cache for click samples - shared across all plugin instances
static CLICK_SAMPLE_CACHE: Lazy<Mutex<HashMap<ClickSampleCacheKey, CachedClickSamples>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));

/// Static cache for guide samples - shared across all plugin instances
/// Key: (filename, sample_rate), Value: DecodedAudioF32
static GUIDE_SAMPLE_CACHE: Lazy<Mutex<HashMap<(String, u32), DecodedAudioF32>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));

/// Thread-safe sample cache wrapper
pub struct SampleCache;

impl SampleCache {
    /// Get or load click samples from cache
    /// Returns (beat, eighth, sixteenth, triplet, accent, num_channels, sample_length)
    #[allow(clippy::type_complexity)]
    pub fn get_or_load_click_samples(
        click_sound: ClickSound,
        sample_rate: f32,
        loader_fn: impl FnOnce() -> (
            Option<DecodedAudioF32>,
            Option<DecodedAudioF32>,
            Option<DecodedAudioF32>,
            Option<DecodedAudioF32>,
            Option<DecodedAudioF32>,
            u32,
            usize,
        ),
    ) -> (
        Option<DecodedAudioF32>,
        Option<DecodedAudioF32>,
        Option<DecodedAudioF32>,
        Option<DecodedAudioF32>,
        Option<DecodedAudioF32>,
        u32,
        usize,
    ) {
        let key = ClickSampleCacheKey(click_sound, sample_rate as u32);

        // Check cache first
        {
            let cache = CLICK_SAMPLE_CACHE.lock().unwrap();
            if let Some(cached) = cache.get(&key) {
                info!(
                    click_sound = ?click_sound,
                    sample_rate,
                    "Using cached click samples"
                );
                return (
                    Some(cached.beat.clone()),
                    Some(cached.eighth.clone()),
                    Some(cached.sixteenth.clone()),
                    Some(cached.triplet.clone()),
                    Some(cached.accent.clone()),
                    cached.num_channels,
                    cached.sample_length,
                );
            }
        }

        // Cache miss - load samples
        info!(
            click_sound = ?click_sound,
            sample_rate,
            "Cache miss - loading click samples"
        );
        let (beat, eighth, sixteenth, triplet, accent, num_channels, sample_length) = loader_fn();

        // Store in cache if all samples loaded successfully
        if let (Some(beat), Some(eighth), Some(sixteenth), Some(triplet), Some(accent)) =
            (&beat, &eighth, &sixteenth, &triplet, &accent)
        {
            let mut cache = CLICK_SAMPLE_CACHE.lock().unwrap();
            cache.insert(
                key,
                CachedClickSamples {
                    beat: beat.clone(),
                    eighth: eighth.clone(),
                    sixteenth: sixteenth.clone(),
                    triplet: triplet.clone(),
                    accent: accent.clone(),
                    num_channels,
                    sample_length,
                },
            );
            info!(
                click_sound = ?click_sound,
                sample_rate,
                cache_size = cache.len(),
                "Cached click samples"
            );
        }

        (
            beat,
            eighth,
            sixteenth,
            triplet,
            accent,
            num_channels,
            sample_length,
        )
    }

    /// Clear all guide samples from the cache
    pub fn clear_guide_samples(guide_samples: &Arc<Mutex<HashMap<String, DecodedAudioF32>>>) {
        let mut guide_samples = guide_samples.lock().unwrap();
        guide_samples.clear();
    }

    /// Get the number of cached guide samples
    pub fn guide_sample_count(
        guide_samples: &Arc<Mutex<HashMap<String, DecodedAudioF32>>>,
    ) -> usize {
        guide_samples.lock().unwrap().len()
    }

    /// Check if a guide sample exists in the cache
    pub fn has_guide_sample(
        guide_samples: &Arc<Mutex<HashMap<String, DecodedAudioF32>>>,
        key: &str,
    ) -> bool {
        guide_samples.lock().unwrap().contains_key(key)
    }

    /// Get a guide sample from the cache
    pub fn get_guide_sample(
        guide_samples: &Arc<Mutex<HashMap<String, DecodedAudioF32>>>,
        key: &str,
    ) -> Option<DecodedAudioF32> {
        guide_samples.lock().unwrap().get(key).cloned()
    }

    /// Clear the click sample cache (useful for testing or if files change)
    pub fn clear_click_cache() {
        let mut cache = CLICK_SAMPLE_CACHE.lock().unwrap();
        let count = cache.len();
        cache.clear();
        info!(cleared_count = count, "Cleared click sample cache");
    }

    /// Get the number of cached click sample sets
    pub fn click_cache_size() -> usize {
        CLICK_SAMPLE_CACHE.lock().unwrap().len()
    }

    /// Get a guide sample from the static cache
    pub fn get_guide_sample_from_cache(
        filename: &str,
        sample_rate: f32,
    ) -> Option<DecodedAudioF32> {
        let key = (filename.to_string(), sample_rate as u32);
        GUIDE_SAMPLE_CACHE.lock().unwrap().get(&key).cloned()
    }

    /// Store a guide sample in the static cache
    pub fn cache_guide_sample(filename: String, sample_rate: f32, sample: DecodedAudioF32) {
        let key = (filename, sample_rate as u32);
        let mut cache = GUIDE_SAMPLE_CACHE.lock().unwrap();
        cache.insert(key.clone(), sample);
        info!(
            filename = %key.0,
            sample_rate = key.1,
            cache_size = cache.len(),
            "Cached guide sample"
        );
    }

    /// Get all guide samples from static cache (for a given sample rate)
    pub fn get_all_cached_guide_samples(sample_rate: f32) -> HashMap<String, DecodedAudioF32> {
        let cache = GUIDE_SAMPLE_CACHE.lock().unwrap();
        let mut result = HashMap::new();
        for ((filename, sr), sample) in cache.iter() {
            if *sr == sample_rate as u32 {
                result.insert(filename.clone(), sample.clone());
            }
        }
        result
    }

    /// Clear the guide sample cache
    pub fn clear_guide_cache() {
        let mut cache = GUIDE_SAMPLE_CACHE.lock().unwrap();
        let count = cache.len();
        cache.clear();
        info!(cleared_count = count, "Cleared guide sample cache");
    }
}
