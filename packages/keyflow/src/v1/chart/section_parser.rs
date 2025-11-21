//! Section parsing trait
//!
//! This module defines the SectionParser trait for parsing chart sections.

use crate::sections::Section;
use crate::chart::chart::Chart;
use crate::chart::types::Measure;

/// Trait for parsing sections in a chart
pub trait SectionParser {
    /// Check if a line is a section marker
    fn is_section_marker(line: &str) -> bool;
    
    /// Try to parse a section from a line
    fn try_parse_section(line: &str) -> Option<Section>;
    
    /// Parse a full section format like "Intro 4" or "Verse 8"
    fn parse_full_section(line: &str) -> Option<Section>;
    
    /// Parse an abbreviated section format like "vs", "ch", "br"
    fn parse_abbreviated_section(line: &str) -> Option<Section>;
    
    /// Parse pre/post sections which need lookahead
    fn parse_pre_post_section(&mut self, lines: &[&str], i: usize) -> Result<usize, String>;
    
    /// Parse the content of a section (chords, melody, etc.)
    fn parse_section_content(
        &mut self,
        lines: &[&str],
        start_index: usize,
        section: &Section,
    ) -> Result<usize, String>;
    
    /// Auto-number sections of the same type
    fn auto_number_sections(&mut self);
    
    /// Get the template for a section type
    fn get_section_template(&self, section_type: &crate::sections::SectionType) -> Vec<Measure>;
}

// Note: Implementation is still in parser.rs
// This will be moved in a future step

