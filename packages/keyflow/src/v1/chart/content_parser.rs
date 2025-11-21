//! Content parsing trait
//!
//! This module defines the ContentParser trait for parsing chord lines and other content.

use crate::chart::chart::Chart;
use crate::chart::types::Measure;
use crate::sections::Section;

/// Trait for parsing content within chart sections
pub trait ContentParser {
    /// Check if a line looks like content (chords, melody, etc.)
    fn looks_like_content(line: &str) -> bool;
    
    /// Parse content lines into measures
    fn parse_content_lines(
        &mut self,
        lines: &[&str],
        section: &Section,
    ) -> Result<Vec<Measure>, String>;
    
    /// Parse a chord line into measures
    fn parse_chord_line(
        &mut self,
        line: &str,
        section_type: &crate::sections::SectionType,
    ) -> Result<Vec<Measure>, String>;
    
    /// Expand repeated notation like "* 4"
    fn expand_repeated_notation(&self, line: &str) -> String;
    
    /// Adjust chord durations to fit the measure
    fn adjust_chord_durations(&mut self, measures: &mut [Measure], section: &Section);
    
    /// Calculate the duration of a chord based on context
    fn calculate_chord_duration(
        &self,
        chord_index: usize,
        total_chords: usize,
        beats_per_measure: f64,
    ) -> f64;
}

// Note: Implementation is still in parser.rs
// This will be moved in a future step

