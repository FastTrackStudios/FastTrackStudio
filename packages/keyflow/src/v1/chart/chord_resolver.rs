//! Chord resolution trait
//!
//! This module defines the ChordResolver trait for resolving chords with memory.

use crate::chart::chart::Chart;
use crate::chart::types::{ChordInstance, RootNote};
use crate::chord::chord::ChordData;
use crate::sections::SectionType;
use crate::time::MusicalPosition;

/// Trait for resolving chords using memory and context
pub trait ChordResolver {
    /// Convert ChordData to ChordInstance with memory resolution
    fn chord_data_to_instance(
        &mut self,
        chord_data: ChordData,
        original_token: String,
        section_type: &SectionType,
        position: MusicalPosition,
        time_signature: (u8, u8),
    ) -> ChordInstance;
    
    /// Apply default quality for a note name using chord memory
    fn apply_default_quality_for_note_name(
        &self,
        note_name: &str,
        section_type: &SectionType,
    ) -> Option<ChordData>;
    
    /// Convert scale degree to actual note name
    fn scale_degree_to_note(&self, degree: u8) -> String;
    
    /// Convert scale degree or roman numeral to note name
    fn convert_scale_degree_or_roman_numeral(&self, root: &str) -> String;
    
    /// Check if a chord quality is complete (has extensions/modifiers)
    fn is_complete_chord_quality(&self, quality: &str) -> bool;
    
    /// Extract root note from a token
    fn extract_root(token: &str) -> String;
}

// Note: Implementation is still in parser.rs
// This will be moved in a future step

