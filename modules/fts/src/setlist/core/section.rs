//! Section domain types for setlist functionality
//!
//! This module re-exports keyflow's Section and SectionType.
//! All methods are available directly from keyflow::Section.
//! Helper functions are provided for SetlistError compatibility.

use daw::primitives::Position;
use keyflow::sections::{Section as KeyflowSection, SectionType as KeyflowSectionType};

use super::error::SetlistError;

// Re-export keyflow's SectionType
pub type SectionType = KeyflowSectionType;

// Re-export keyflow's Section
pub type Section = KeyflowSection;

// Extension trait for SectionType to add FTS-specific methods
pub trait SectionTypeExt {
    /// Check if this section type should be numbered (alias for should_number for compatibility)
    fn should_be_numbered(&self) -> bool;

    /// Get default display preference (full name vs abbreviation)
    fn prefers_full_name(&self) -> bool;
}

impl SectionTypeExt for SectionType {
    fn should_be_numbered(&self) -> bool {
        self.should_number()
    }

    fn prefers_full_name(&self) -> bool {
        match self {
            SectionType::Intro | SectionType::Outro => true,
            _ => false,
        }
    }
}

// Helper functions that wrap keyflow methods with SetlistError
// These are provided as module-level functions since we can't implement methods on type aliases

/// Create a section from seconds (wraps keyflow's method with SetlistError)
pub fn section_from_seconds(
    section_type: SectionType,
    start_seconds: f64,
    end_seconds: f64,
    name: String,
    number: Option<u32>,
) -> Result<Section, SetlistError> {
    KeyflowSection::from_seconds(section_type, start_seconds, end_seconds, name, number)
        .map_err(|e| SetlistError::invalid_section(&e))
}

/// Create a section from seconds with specified BPM and time signature
pub fn section_from_seconds_with_tempo(
    section_type: SectionType,
    start_seconds: f64,
    end_seconds: f64,
    name: String,
    number: Option<u32>,
    bpm: f64,
    time_signature: daw::primitives::TimeSignature,
) -> Result<Section, SetlistError> {
    KeyflowSection::from_seconds_with_tempo(
        section_type,
        start_seconds,
        end_seconds,
        name,
        number,
        bpm,
        time_signature,
    )
    .map_err(|e| SetlistError::invalid_section(&e))
}

/// Create a new section with DAW positions (for DAW integration)
pub fn section_new(
    section_type: SectionType,
    start_position: Position,
    end_position: Position,
    name: String,
    number: Option<u32>,
) -> Result<Section, SetlistError> {
    KeyflowSection::new_with_positions(section_type, start_position, end_position, name, number)
        .map_err(|e| SetlistError::invalid_section(&e))
}

/// Create a new section with ID
pub fn section_with_id(
    id: uuid::Uuid,
    section_type: SectionType,
    start_position: Position,
    end_position: Position,
    name: String,
    number: Option<u32>,
) -> Result<Section, SetlistError> {
    KeyflowSection::with_id(id, section_type, start_position, end_position, name, number)
        .map_err(|e| SetlistError::invalid_section(&e))
}

// Extension trait to add methods to Section instances
pub trait SectionExt {
    fn validate(&self) -> Result<(), SetlistError>;
    fn with_time_range(
        &self,
        start_seconds: f64,
        end_seconds: f64,
    ) -> Result<Section, SetlistError>;
}

/// Validate a section (module-level function for convenience)
pub fn validate(section: &Section) -> Result<(), SetlistError> {
    KeyflowSection::validate(section).map_err(|e| SetlistError::invalid_section(&e))
}

impl SectionExt for Section {
    fn validate(&self) -> Result<(), SetlistError> {
        KeyflowSection::validate(self).map_err(|e| SetlistError::invalid_section(&e))
    }

    fn with_time_range(
        &self,
        start_seconds: f64,
        end_seconds: f64,
    ) -> Result<Section, SetlistError> {
        KeyflowSection::with_time_range(self, start_seconds, end_seconds)
            .map_err(|e| SetlistError::invalid_section(&e))
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
        assert_eq!(SectionType::parse("verse").unwrap(), SectionType::Verse);
        assert_eq!(SectionType::parse("CH").unwrap(), SectionType::Chorus);
        assert_eq!(
            SectionType::parse("pre-chorus").unwrap(),
            SectionType::Pre(Box::new(SectionType::Chorus))
        );

        // Test fuzzy matching
        assert_eq!(SectionType::parse("vrse").unwrap(), SectionType::Verse);
        assert_eq!(SectionType::parse("chorous").unwrap(), SectionType::Chorus);

        // Test error case
        assert!(SectionType::parse("invalid").is_err());
    }

    #[test]
    fn test_section_creation() {
        let section = section_from_seconds(
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
        assert_eq!(section.name, Some("Verse 1".to_string()));
        assert!(section.contains_position(25.0));
        assert!(!section.contains_position(50.0));
    }

    #[test]
    fn test_section_validation() {
        // Valid section
        let section =
            section_from_seconds(SectionType::Chorus, 10.0, 20.0, "Chorus".to_string(), None)
                .unwrap();
        assert!(section.validate().is_ok());

        // Invalid time range
        let invalid_section =
            section_from_seconds(SectionType::Verse, 20.0, 10.0, "Invalid".to_string(), None);
        assert!(invalid_section.is_err());
    }

    #[test]
    fn test_display_names() {
        let verse = section_from_seconds(
            SectionType::Verse,
            0.0,
            30.0,
            "Verse 1".to_string(),
            Some(1),
        )
        .unwrap();
        assert_eq!(verse.display_name(), "Verse 1");

        let intro =
            section_from_seconds(SectionType::Intro, 0.0, 10.0, "Intro".to_string(), None).unwrap();
        assert_eq!(intro.display_name(), "Intro");

        let mut chorus_split = section_from_seconds(
            SectionType::Chorus,
            30.0,
            60.0,
            "Chorus 1a".to_string(),
            Some(1),
        )
        .unwrap();
        chorus_split.split_letter = Some('a');
        assert_eq!(chorus_split.display_name(), "Chorus 1a");
    }

    #[test]
    fn test_section_overlaps() {
        let section1 =
            section_from_seconds(SectionType::Verse, 10.0, 30.0, "Verse".to_string(), None)
                .unwrap();
        let section2 =
            section_from_seconds(SectionType::Chorus, 25.0, 45.0, "Chorus".to_string(), None)
                .unwrap();
        let section3 =
            section_from_seconds(SectionType::Bridge, 50.0, 70.0, "Bridge".to_string(), None)
                .unwrap();

        assert!(section1.overlaps_with_section(&section2));
        assert!(section2.overlaps_with_section(&section1));
        assert!(!section1.overlaps_with_section(&section3));
    }

    #[test]
    fn test_metadata() {
        let mut section =
            section_from_seconds(SectionType::Verse, 0.0, 30.0, "Verse".to_string(), None).unwrap();

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
        let section = section_from_seconds(
            SectionType::Verse,
            10.0,
            30.0,
            "Original".to_string(),
            Some(1),
        )
        .unwrap();

        let moved = section.with_time_range(40.0, 60.0).unwrap();
        assert_eq!(moved.start_seconds(), Some(40.0));
        assert_eq!(moved.end_seconds(), Some(60.0));
        assert_eq!(moved.name, Some("Original".to_string()));
        assert_eq!(moved.number, Some(1));

        let changed_type = section.with_section_type(SectionType::Chorus);
        assert_eq!(changed_type.section_type, SectionType::Chorus);
        assert_eq!(changed_type.name, Some("Original".to_string()));
    }
}
