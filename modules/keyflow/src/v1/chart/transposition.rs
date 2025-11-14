//! Transposition trait
//!
//! This module defines the Transposition trait for transposing chords and handling key changes.

use crate::chart::chart::Chart;
use crate::chart::types::ChordInstance;
use crate::key::keys::Key;
use crate::time::MusicalDuration;

/// Trait for transposing chords and handling key changes
pub trait Transposition {
    /// Transpose chord memory when key changes
    fn transpose_chord_memory(&mut self, from_key: &Key, to_key: &Key);
    
    /// Transpose a chord instance to match the current key
    fn transpose_chord_to_key(
        &self,
        chord: &mut ChordInstance,
        to_key: &Key,
    ) -> Result<ChordInstance, String>;
    
    /// Calculate the total duration from the start of the song to a specific position
    fn calculate_total_duration(
        &self,
        section_index: usize,
        position_in_section: MusicalDuration,
    ) -> MusicalDuration;
}

// Note: Implementation is still in parser.rs
// This will be moved in a future step

