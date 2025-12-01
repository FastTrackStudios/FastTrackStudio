//! Chord service for reading and caching chord data

use keyflow::Key;
use std::sync::{Arc, Mutex};
use tracing::{debug, info};
use crate::chords::types::{ChordConfig, ChordsData};
use crate::chords::{analyze_chords_from_track, parse_key, ChordError};

/// Service for managing chord detection and caching
#[derive(Debug)]
pub struct ChordService {
    /// Current configuration
    config: Arc<Mutex<ChordConfig>>,
    /// Cached chord data
    cached_chords: Arc<Mutex<Option<ChordsData>>>,
}

impl ChordService {
    /// Create a new chord service with default configuration
    pub fn new() -> Self {
        Self {
            config: Arc::new(Mutex::new(ChordConfig::default())),
            cached_chords: Arc::new(Mutex::new(None)),
        }
    }

    /// Get the current configuration
    pub fn get_config(&self) -> ChordConfig {
        self.config.lock().unwrap().clone()
    }

    /// Update the configuration
    pub fn set_config(&self, config: ChordConfig) {
        *self.config.lock().unwrap() = config;
        // Clear cache when config changes
        *self.cached_chords.lock().unwrap() = None;
    }

    /// Set the track name to read chords from
    pub fn set_track_name(&self, track_name: String) {
        let mut config = self.config.lock().unwrap();
        config.track_name = track_name;
        // Clear cache
        drop(config);
        *self.cached_chords.lock().unwrap() = None;
    }

    /// Set the key signature for scale degree/roman numeral conversion
    pub fn set_key(&self, key_str: &str) -> Result<(), ChordError> {
        let key = parse_key(key_str).map_err(|e| ChordError::InvalidKey(e))?;
        let mut config = self.config.lock().unwrap();
        config.key = Some(key);
        // Clear cache when key changes
        drop(config);
        *self.cached_chords.lock().unwrap() = None;
        Ok(())
    }

    /// Read and analyze chords from the configured track
    pub fn read_chords(&self) -> Result<ChordsData, ChordError> {
        let config = self.config.lock().unwrap().clone();
        let track_name = config.track_name.clone();
        let key = config.key.clone();

        info!(
            track_name = track_name,
            "Reading chords from track"
        );

        // Analyze chords
        let chords_data = analyze_chords_from_track(&track_name, key)?;

        // Cache the result
        *self.cached_chords.lock().unwrap() = Some(chords_data.clone());

        debug!(
            chord_count = chords_data.chords.len(),
            "Cached {} chords",
            chords_data.chords.len()
        );

        Ok(chords_data)
    }

    /// Get cached chords (if available)
    pub fn get_cached_chords(&self) -> Option<ChordsData> {
        self.cached_chords.lock().unwrap().clone()
    }

    /// Clear the cache
    pub fn clear_cache(&self) {
        *self.cached_chords.lock().unwrap() = None;
    }
}

impl Default for ChordService {
    fn default() -> Self {
        Self::new()
    }
}

