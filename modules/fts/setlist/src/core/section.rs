//! Section domain types for setlist functionality
//!
//! This module contains the core domain types for managing song sections,
//! including section types, section metadata, and section validation.

use primitives::{MusicalPosition, Position, TimePosition, TimeRange, TimeSignature};
use serde::{Deserialize, Serialize};
use specta::Type;
use std::collections::HashMap;

use super::error::SetlistError;

/// Represents different types of song sections with their full names and abbreviations
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Type)]
pub enum SectionType {
    Verse,
    Chorus,
    Bridge,
    Intro,
    Outro,
    Instrumental,
    Custom, // Custom section type for unrecognized section names
    #[specta(skip)]
    Pre(Box<SectionType>), // Pre-Chorus, Pre-Verse, etc.
    #[specta(skip)]
    Post(Box<SectionType>), // Post-Chorus, Post-Verse, etc.
}

impl SectionType {
    /// Get the full name of the section (dynamic for Pre/Post)
    pub fn full_name(&self) -> String {
        match self {
            SectionType::Verse => "Verse".to_string(),
            SectionType::Chorus => "Chorus".to_string(),
            SectionType::Bridge => "Bridge".to_string(),
            SectionType::Intro => "Intro".to_string(),
            SectionType::Outro => "Outro".to_string(),
            SectionType::Instrumental => "Instrumental".to_string(),
            SectionType::Custom => "Custom".to_string(),
            SectionType::Pre(inner) => format!("Pre-{}", inner.full_name()),
            SectionType::Post(inner) => format!("Post-{}", inner.full_name()),
        }
    }

    /// Get the abbreviated name of the section (always capitalized)
    ///
    /// Returns uppercase abbreviations:
    /// - Verse -> "VS"
    /// - Chorus -> "CH"
    /// - Bridge -> "BR"
    /// - Intro -> "IN"
    /// - Outro -> "OUT"
    /// - Instrumental -> "INST"
    /// - Custom -> "CUSTOM" (but typically the section name is used directly)
    /// - Pre-Chorus -> "PRE-CH"
    /// - Post-Verse -> "POST-VS"
    pub fn abbreviation(&self) -> String {
        match self {
            SectionType::Verse => "VS".to_string(),
            SectionType::Chorus => "CH".to_string(),
            SectionType::Bridge => "BR".to_string(),
            SectionType::Intro => "IN".to_string(),
            SectionType::Outro => "OUT".to_string(),
            SectionType::Instrumental => "INST".to_string(),
            SectionType::Custom => "CUSTOM".to_string(),
            SectionType::Pre(inner) => format!("PRE-{}", inner.abbreviation()),
            SectionType::Post(inner) => format!("POST-{}", inner.abbreviation()),
        }
    }

    /// Try to parse a section type from a string (name or abbreviation)
    ///
    /// Handles case-insensitive matching and common typos/variations:
    /// - "verse", "Verse", "VERSE", "vs", "VS", "vErSe", "vrse" -> Verse
    /// - "chorus", "Chorus", "CHORUS", "ch", "CH", "chorous", "corus" -> Chorus
    /// - etc.
    pub fn from_str(s: &str) -> Result<Self, SetlistError> {
        let s_lower = s.to_lowercase();
        let s_lower = s_lower.trim();

        // Try exact matches first (case-insensitive)
        match s_lower {
            "verse" | "vs" | "v" => return Ok(SectionType::Verse),
            "chorus" | "ch" | "c" => return Ok(SectionType::Chorus),
            "bridge" | "br" | "b" => return Ok(SectionType::Bridge),
            "intro" | "in" | "i" => return Ok(SectionType::Intro),
            "outro" | "out" | "o" => return Ok(SectionType::Outro),
            "instrumental" | "inst" | "instrument" => return Ok(SectionType::Instrumental),
            _ => {}
        }

        // Try fuzzy matching for common typos and variations
        // Verse variations
        if Self::fuzzy_match(&s_lower, "verse", &["vrse", "verce", "vers", "versa"]) {
            return Ok(SectionType::Verse);
        }

        // Chorus variations
        if Self::fuzzy_match(
            &s_lower,
            "chorus",
            &["chorous", "corus", "chrous", "chors", "chor"],
        ) {
            return Ok(SectionType::Chorus);
        }

        // Bridge variations
        if Self::fuzzy_match(&s_lower, "bridge", &["bridg", "brige", "brid"]) {
            return Ok(SectionType::Bridge);
        }

        // Intro variations - handle "introduction", "intro", etc.
        if Self::fuzzy_match(
            &s_lower,
            "intro",
            &["intr", "int", "introo", "introduction"],
        ) {
            return Ok(SectionType::Intro);
        }
        // Also check if it starts with "introduction"
        if s_lower.starts_with("introduction") {
            return Ok(SectionType::Intro);
        }

        // Outro variations - handle "outroduction", "outro", etc.
        if Self::fuzzy_match(
            &s_lower,
            "outro",
            &["outr", "out", "outroo", "outroduction"],
        ) {
            return Ok(SectionType::Outro);
        }
        // Also check if it starts with "outroduction"
        if s_lower.starts_with("outroduction") {
            return Ok(SectionType::Outro);
        }

        // Instrumental variations
        if Self::fuzzy_match(
            &s_lower,
            "instrumental",
            &["instumental", "instrumantal", "instrument"],
        ) {
            return Ok(SectionType::Instrumental);
        }

        // Try to parse Pre/Post
        if let Some(rest) = s_lower.strip_prefix("pre-") {
            if let Ok(inner) = Self::from_str(rest) {
                return Ok(SectionType::Pre(Box::new(inner)));
            }
        }
        if let Some(rest) = s_lower.strip_prefix("post-") {
            if let Ok(inner) = Self::from_str(rest) {
                return Ok(SectionType::Post(Box::new(inner)));
            }
        }

        Err(SetlistError::section_parse_error(
            s,
            "Unknown section type - supported types: verse, chorus, bridge, intro, outro, instrumental, pre-*, post-*",
        ))
    }

    /// Fuzzy matching helper - checks if the input matches the target or any variations
    fn fuzzy_match(input: &str, target: &str, variations: &[&str]) -> bool {
        // Exact match with target
        if input == target {
            return true;
        }

        // Check if input starts with target (allows for trailing characters like numbers)
        if input.starts_with(target) {
            return true;
        }

        // Check variations
        for variation in variations {
            if input == *variation || input.starts_with(variation) {
                return true;
            }
        }

        // Check if input is close enough to target (simple edit distance check)
        // For very short strings, just check if first few chars match
        if input.len() >= 3 && target.len() >= 3 {
            let input_prefix = &input[..input.len().min(3)];
            let target_prefix = &target[..target.len().min(3)];
            if input_prefix == target_prefix {
                return true;
            }
        }

        false
    }

    /// Check if this section type should be numbered
    pub fn should_be_numbered(&self) -> bool {
        match self {
            SectionType::Intro | SectionType::Outro => false,
            SectionType::Pre(_) | SectionType::Post(_) => false,
            SectionType::Custom => false, // Custom sections should not be auto-numbered
            _ => true,
        }
    }

    /// Get default display preference (full name vs abbreviation)
    pub fn prefers_full_name(&self) -> bool {
        match self {
            SectionType::Intro | SectionType::Outro => true,
            _ => false,
        }
    }
}

impl std::fmt::Display for SectionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.full_name())
    }
}

/// Represents a section within a song
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Type)]
pub struct Section {
    /// Unique identifier for this section
    pub id: Option<uuid::Uuid>,
    /// Section type
    pub section_type: SectionType,
    /// Optional section number (e.g., 1, 2, 3 for Verse 1, Verse 2, etc.)
    pub number: Option<u32>,
    /// Split letter for consecutive sections (e.g., 'a', 'b', 'c')
    pub split_letter: Option<char>,
    /// Start position (contains both musical and time position)
    pub start_position: Position,
    /// End position (contains both musical and time position)
    pub end_position: Position,
    /// Section name (from region name)
    pub name: String,
    /// Color from the region (optional, for display purposes)
    pub color: Option<u32>,
    /// Optional metadata
    pub metadata: HashMap<String, String>,
}

impl Section {
    /// Create a new section
    pub fn new(
        section_type: SectionType,
        start_position: Position,
        end_position: Position,
        name: String,
        number: Option<u32>,
    ) -> Result<Self, SetlistError> {
        if start_position.time.to_seconds() >= end_position.time.to_seconds() {
            return Err(SetlistError::invalid_time_range(
                start_position.time.to_seconds(),
                end_position.time.to_seconds(),
            ));
        }

        if name.trim().is_empty() {
            return Err(SetlistError::invalid_section(
                "Section name cannot be empty",
            ));
        }

        Ok(Self {
            id: None,
            section_type,
            number,
            split_letter: None,
            start_position,
            end_position,
            name,
            color: None,
            metadata: HashMap::new(),
        })
    }

    /// Create a new section with ID
    pub fn with_id(
        id: uuid::Uuid,
        section_type: SectionType,
        start_position: Position,
        end_position: Position,
        name: String,
        number: Option<u32>,
    ) -> Result<Self, SetlistError> {
        let mut section = Self::new(section_type, start_position, end_position, name, number)?;
        section.id = Some(id);
        Ok(section)
    }

    /// Calculate the duration of the section in seconds
    pub fn duration(&self) -> f64 {
        self.end_position.time.to_seconds() - self.start_position.time.to_seconds()
    }

    /// Get start time in seconds
    pub fn start_seconds(&self) -> f64 {
        self.start_position.time.to_seconds()
    }

    /// Get end time in seconds
    pub fn end_seconds(&self) -> f64 {
        self.end_position.time.to_seconds()
    }

    /// Calculate the length of the section in measures
    /// Uses the musical positions from start_position and end_position
    /// Assumes 4/4 time signature (4 beats per measure)
    /// Returns None if musical positions are not available
    pub fn length_measures(&self) -> Option<f64> {
        let start_musical = &self.start_position.musical;
        let end_musical = &self.end_position.musical;
        
        // Calculate beats per measure from time signature (default to 4/4)
        // TODO: Get actual time signature from song/project metadata
        let beats_per_measure = 4.0;
        
        // Convert musical positions to total measures
        // measure + (beat + subdivision/1000) / beats_per_measure
        let start_measures = start_musical.measure as f64 
            + (start_musical.beat as f64 + start_musical.subdivision as f64 / 1000.0) / beats_per_measure;
        
        let end_measures = end_musical.measure as f64 
            + (end_musical.beat as f64 + end_musical.subdivision as f64 / 1000.0) / beats_per_measure;
        
        let length = end_measures - start_measures;
        if length > 0.0 {
            Some(length)
        } else {
            None
        }
    }

    /// Create a section from seconds (for compatibility)
    /// Uses default BPM (120) and time signature (4/4) to calculate musical positions
    pub fn from_seconds(
        section_type: SectionType,
        start_seconds: f64,
        end_seconds: f64,
        name: String,
        number: Option<u32>,
    ) -> Result<Self, SetlistError> {
        Self::from_seconds_with_tempo(
            section_type,
            start_seconds,
            end_seconds,
            name,
            number,
            120.0, // Default BPM
            TimeSignature::new(4, 4), // Default time signature
        )
    }

    /// Create a section from seconds with specified BPM and time signature
    pub fn from_seconds_with_tempo(
        section_type: SectionType,
        start_seconds: f64,
        end_seconds: f64,
        name: String,
        number: Option<u32>,
        bpm: f64,
        time_signature: primitives::TimeSignature,
    ) -> Result<Self, SetlistError> {
        let start_time = primitives::TimePosition::from_seconds(start_seconds);
        let end_time = primitives::TimePosition::from_seconds(end_seconds);
        
        // Calculate musical positions from time positions
        let start_musical = start_time.to_musical_position(bpm, time_signature);
        let end_musical = end_time.to_musical_position(bpm, time_signature);
        
        let start_position = primitives::Position::new(start_musical.clone(), start_time);
        let end_position = primitives::Position::new(end_musical, end_time);
        
        Ok(Self::new(
            section_type,
            start_position,
            end_position,
            name,
            number,
        )?)
    }

    /// Check if a time position is within this section
    pub fn contains_position(&self, seconds: f64) -> bool {
        seconds >= self.start_seconds() && seconds < self.end_seconds()
    }

    /// Check if a Position is within this section
    pub fn contains_position_exact(&self, position: Position) -> bool {
        position.time.to_seconds() >= self.start_seconds()
            && position.time.to_seconds() < self.end_seconds()
    }

    /// Check if this section overlaps with another time range
    pub fn overlaps_with_range(&self, start: f64, end: f64) -> bool {
        !(self.end_seconds() <= start || self.start_seconds() >= end)
    }

    /// Check if this section overlaps with another section
    pub fn overlaps_with_section(&self, other: &Section) -> bool {
        self.overlaps_with_range(other.start_seconds(), other.end_seconds())
    }

    /// Generate the display name for the section
    ///
    /// Rules:
    /// - Intro and Outro: Use full names ("Intro", "Outro")
    /// - Everything else: Use abbreviations ("VS 1", "CH 2", "BR")
    /// - Include number and split letter if present
    pub fn display_name(&self) -> String {
        // Custom sections always use the exact original name with no modifications
        if matches!(self.section_type, SectionType::Custom) {
            // Custom sections should never have numbers or split letters added
            // Just return the original name as-is
            self.name.clone()
        } else if matches!(self.section_type, SectionType::Instrumental) {
            // Instrumental sections - check if the name looks like a standard instrumental pattern
            let name_lower = self.name.to_lowercase();
            let is_standard_inst = name_lower.starts_with("inst") 
                || name_lower.starts_with("instrumental")
                || name_lower == "inst";
            
            // If it's not a standard instrumental name, use the original name
            if !is_standard_inst {
                // Return the original name, but add number/split letter if present
                let with_number = if let Some(num) = self.number {
                    format!("{} {}", self.name, num)
                } else {
                    self.name.clone()
                };
                
                if let Some(letter) = self.split_letter {
                    format!("{}{}", with_number, letter)
                } else {
                    with_number
                }
            } else {
                // Standard instrumental - use abbreviation
                let base_name = self.section_type.abbreviation();
                let with_number = if let Some(num) = self.number {
                    format!("{} {}", base_name, num)
                } else {
                    base_name
                };
                
                if let Some(letter) = self.split_letter {
                    format!("{}{}", with_number, letter)
                } else {
                    with_number
                }
            }
        } else {
            // Normal section type - use standard display logic
            let base_name = if self.section_type.prefers_full_name() {
                self.section_type.full_name()
            } else {
                self.section_type.abbreviation()
            };

            // Add number if present
            let with_number = if let Some(num) = self.number {
                format!("{} {}", base_name, num)
            } else {
                base_name
            };

            // Add split letter if present
            if let Some(letter) = self.split_letter {
                format!("{}{}", with_number, letter)
            } else {
                with_number
            }
        }
    }

    /// Get metadata value
    pub fn get_metadata(&self, key: &str) -> Option<&String> {
        self.metadata.get(key)
    }

    /// Set metadata value
    pub fn set_metadata<K: Into<String>, V: Into<String>>(&mut self, key: K, value: V) {
        self.metadata.insert(key.into(), value.into());
    }

    /// Remove metadata value
    pub fn remove_metadata(&mut self, key: &str) -> Option<String> {
        self.metadata.remove(key)
    }

    /// Validate section data
    pub fn validate(&self) -> Result<(), SetlistError> {
        if self.start_seconds() >= self.end_seconds() {
            return Err(SetlistError::invalid_time_range(
                self.start_seconds(),
                self.end_seconds(),
            ));
        }

        if self.name.trim().is_empty() {
            return Err(SetlistError::invalid_section(
                "Section name cannot be empty",
            ));
        }

        if self.start_seconds() < 0.0 {
            return Err(SetlistError::invalid_section(
                "Section start time cannot be negative",
            ));
        }

        if let Some(num) = self.number {
            if num == 0 {
                return Err(SetlistError::invalid_section(
                    "Section number must be greater than 0",
                ));
            }
        }

        Ok(())
    }

    /// Clone this section with a new time range
    pub fn with_time_range(
        &self,
        start_seconds: f64,
        end_seconds: f64,
    ) -> Result<Self, SetlistError> {
        if start_seconds >= end_seconds {
            return Err(SetlistError::invalid_time_range(start_seconds, end_seconds));
        }

        let start_pos = Position::from_seconds(start_seconds);
        Ok(Self {
            id: None, // New section gets new ID
            section_type: self.section_type.clone(),
            number: self.number,
            split_letter: self.split_letter,
            start_position: start_pos,
            end_position: Position::from_seconds(end_seconds),
            name: self.name.clone(),
            color: self.color,
            metadata: self.metadata.clone(),
        })
    }

    /// Clone this section with a new section type
    pub fn with_section_type(&self, section_type: SectionType) -> Self {
        Self {
            id: None, // New section gets new ID
            section_type,
            number: self.number,
            split_letter: self.split_letter,
            start_position: self.start_position.clone(),
            end_position: self.end_position.clone(),
            name: self.name.clone(),
            color: self.color,
            metadata: self.metadata.clone(),
        }
    }
}

impl std::fmt::Display for Section {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} ({:.1}s - {:.1}s)",
            self.display_name(),
            self.start_seconds(),
            self.end_seconds()
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_section_type_creation() {
        assert_eq!(SectionType::Verse.full_name(), "Verse");
        assert_eq!(SectionType::Chorus.abbreviation(), "CH");
        assert_eq!(
            SectionType::Pre(Box::new(SectionType::Chorus)).full_name(),
            "Pre-Chorus"
        );
        assert_eq!(
            SectionType::Post(Box::new(SectionType::Verse)).abbreviation(),
            "POST-VS"
        );
    }

    #[test]
    fn test_section_type_parsing() {
        assert_eq!(SectionType::from_str("verse").unwrap(), SectionType::Verse);
        assert_eq!(SectionType::from_str("CH").unwrap(), SectionType::Chorus);
        assert_eq!(
            SectionType::from_str("pre-chorus").unwrap(),
            SectionType::Pre(Box::new(SectionType::Chorus))
        );

        // Test fuzzy matching
        assert_eq!(SectionType::from_str("vrse").unwrap(), SectionType::Verse);
        assert_eq!(
            SectionType::from_str("chorous").unwrap(),
            SectionType::Chorus
        );

        // Test error case
        assert!(SectionType::from_str("invalid").is_err());
    }

    #[test]
    fn test_section_creation() {
        let section = Section::from_seconds(
            SectionType::Verse,
            10.0,
            40.0,
            "Verse 1".to_string(),
            Some(1),
        )
        .unwrap();

        assert_eq!(section.section_type, SectionType::Verse);
        assert_eq!(section.duration(), 30.0);
        assert_eq!(section.number, Some(1));
        assert_eq!(section.name, "Verse 1");
        assert!(section.contains_position(25.0));
        assert!(!section.contains_position(50.0));
    }

    #[test]
    fn test_section_validation() {
        // Valid section
        let section =
            Section::from_seconds(SectionType::Chorus, 10.0, 20.0, "Chorus".to_string(), None)
                .unwrap();
        assert!(section.validate().is_ok());

        // Invalid time range
        let invalid_section =
            Section::from_seconds(SectionType::Verse, 20.0, 10.0, "Invalid".to_string(), None);
        assert!(invalid_section.is_err());
    }

    #[test]
    fn test_display_names() {
        let verse = Section::from_seconds(
            SectionType::Verse,
            0.0,
            30.0,
            "Verse 1".to_string(),
            Some(1),
        )
        .unwrap();
        assert_eq!(verse.display_name(), "VS 1");

        let intro = Section::from_seconds(SectionType::Intro, 0.0, 10.0, "Intro".to_string(), None)
            .unwrap();
        assert_eq!(intro.display_name(), "Intro");

        let mut chorus_split = Section::from_seconds(
            SectionType::Chorus,
            30.0,
            60.0,
            "Chorus 1a".to_string(),
            Some(1),
        )
        .unwrap();
        chorus_split.split_letter = Some('a');
        assert_eq!(chorus_split.display_name(), "CH 1a");
    }

    #[test]
    fn test_section_overlaps() {
        let section1 =
            Section::from_seconds(SectionType::Verse, 10.0, 30.0, "Verse".to_string(), None)
                .unwrap();
        let section2 =
            Section::from_seconds(SectionType::Chorus, 25.0, 45.0, "Chorus".to_string(), None)
                .unwrap();
        let section3 =
            Section::from_seconds(SectionType::Bridge, 50.0, 70.0, "Bridge".to_string(), None)
                .unwrap();

        assert!(section1.overlaps_with_section(&section2));
        assert!(section2.overlaps_with_section(&section1));
        assert!(!section1.overlaps_with_section(&section3));
    }

    #[test]
    fn test_metadata() {
        let mut section =
            Section::from_seconds(SectionType::Verse, 0.0, 30.0, "Verse".to_string(), None)
                .unwrap();

        section.set_metadata("key", "C major");
        section.set_metadata("tempo", "120");

        assert_eq!(section.get_metadata("key"), Some(&"C major".to_string()));
        assert_eq!(section.get_metadata("tempo"), Some(&"120".to_string()));
        assert_eq!(section.get_metadata("nonexistent"), None);

        let removed = section.remove_metadata("key");
        assert_eq!(removed, Some("C major".to_string()));
        assert_eq!(section.get_metadata("key"), None);
    }

    #[test]
    fn test_section_cloning_operations() {
        let section = Section::from_seconds(
            SectionType::Verse,
            10.0,
            30.0,
            "Original".to_string(),
            Some(1),
        )
        .unwrap();

        let moved = section.with_time_range(40.0, 60.0).unwrap();
        assert_eq!(moved.start_seconds(), 40.0);
        assert_eq!(moved.end_seconds(), 60.0);
        assert_eq!(moved.name, "Original");
        assert_eq!(moved.number, Some(1));

        let changed_type = section.with_section_type(SectionType::Chorus);
        assert_eq!(changed_type.section_type, SectionType::Chorus);
        assert_eq!(changed_type.name, "Original");
    }
}
