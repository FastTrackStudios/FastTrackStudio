//! Mock setlist service implementation
//!
//! This module provides a mock implementation of the setlist service for testing
//! and development purposes. It stores data in memory and provides sample data.

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use crate::core::{
    Setlist, SetlistSource, SetlistError, Song, Section, SectionType, SourceInfo
};
use primitives::Position;
use crate::application::{ApplicationSetlistSource, SourceType};

use marker_region::{Marker, Region, MarkerRegionSource, MockMarkerRegionService, MarkerSource, RegionSource};

/// Mock implementation of setlist service for testing and development
pub struct MockSetlistService {
    inner: Arc<Mutex<MockSetlistServiceInner>>,
}

struct MockSetlistServiceInner {
    setlist: Option<Setlist>,
    marker_region_source: MockMarkerRegionService,
    error_mode: bool,
    metadata: HashMap<String, String>,
}

impl MockSetlistService {
    /// Create a new mock setlist service
    pub fn new() -> Self {
        Self {
            inner: Arc::new(Mutex::new(MockSetlistServiceInner {
                setlist: None,
                marker_region_source: MockMarkerRegionService::new(),
                error_mode: false,
                metadata: HashMap::new(),
            })),
        }
    }

    /// Create a mock setlist service with sample data
    pub fn with_sample_data() -> Self {
        let service = Self::new();
        service.initialize_sample_data();
        service
    }

    /// Create a mock setlist service with complex sample data
    pub fn with_complex_data() -> Self {
        let service = Self::new();
        service.initialize_complex_data();
        service
    }

    /// Initialize with sample data
    fn initialize_sample_data(&self) {
        let mut inner = self.inner.lock().unwrap();

        // Setup marker/region source with sample data
        inner.marker_region_source = MockMarkerRegionService::with_sample_data();

        // Create a sample setlist
        let mut setlist = Setlist::new("Sample Setlist".to_string()).unwrap();

        // Song 1: "Opening Song"
        let mut song1 = Song::new("Opening Song".to_string()).unwrap();
        song1.count_in_position = Some(Position::from_seconds(-4.0));
        song1.start_position = Some(Position::from_seconds(0.0));
        song1.song_end_position = Some(Position::from_seconds(180.0));
        song1.end_position = Some(Position::from_seconds(190.0));
        song1.set_metadata("key", "C major");
        song1.set_metadata("tempo", "120");

        // Add sections to song1
        let intro = Section::new(SectionType::Intro, Position::from_seconds(0.0), Position::from_seconds(15.0), "Intro".to_string(), None).unwrap();
        let verse1 = Section::new(SectionType::Verse, Position::from_seconds(15.0), Position::from_seconds(45.0), "Verse 1".to_string(), Some(1)).unwrap();
        let chorus1 = Section::new(SectionType::Chorus, Position::from_seconds(45.0), Position::from_seconds(75.0), "Chorus 1".to_string(), Some(1)).unwrap();
        let verse2 = Section::new(SectionType::Verse, Position::from_seconds(75.0), Position::from_seconds(105.0), "Verse 2".to_string(), Some(2)).unwrap();
        let chorus2 = Section::new(SectionType::Chorus, Position::from_seconds(105.0), Position::from_seconds(135.0), "Chorus 2".to_string(), Some(2)).unwrap();
        let bridge = Section::new(SectionType::Bridge, Position::from_seconds(135.0), Position::from_seconds(150.0), "Bridge".to_string(), None).unwrap();
        let chorus3 = Section::new(SectionType::Chorus, Position::from_seconds(150.0), Position::from_seconds(180.0), "Chorus 3".to_string(), Some(3)).unwrap();

        song1.add_section(intro).unwrap();
        song1.add_section(verse1).unwrap();
        song1.add_section(chorus1).unwrap();
        song1.add_section(verse2).unwrap();
        song1.add_section(chorus2).unwrap();
        song1.add_section(bridge).unwrap();
        song1.add_section(chorus3).unwrap();

        // Auto-number sections
        song1.auto_number_sections();

        setlist.add_song(song1).unwrap();

        // Song 2: "Ballad"
        let mut song2 = Song::new("Ballad".to_string()).unwrap();
        song2.count_in_position = Some(Position::from_seconds(194.0));
        song2.start_position = Some(Position::from_seconds(200.0));
        song2.song_end_position = Some(Position::from_seconds(320.0));
        song2.end_position = Some(Position::from_seconds(325.0));
        song2.set_metadata("key", "G major");
        song2.set_metadata("tempo", "80");

        // Add sections to song2
        let verse1_ballad = Section::new(SectionType::Verse, Position::from_seconds(200.0), Position::from_seconds(240.0), "Verse 1".to_string(), Some(1)).unwrap();
        let chorus1_ballad = Section::new(SectionType::Chorus, Position::from_seconds(240.0), Position::from_seconds(280.0), "Chorus 1".to_string(), Some(1)).unwrap();
        let verse2_ballad = Section::new(SectionType::Verse, Position::from_seconds(280.0), Position::from_seconds(300.0), "Verse 2".to_string(), Some(2)).unwrap();
        let outro = Section::new(SectionType::Outro, Position::from_seconds(300.0), Position::from_seconds(320.0), "Outro".to_string(), None).unwrap();

        song2.add_section(verse1_ballad).unwrap();
        song2.add_section(chorus1_ballad).unwrap();
        song2.add_section(verse2_ballad).unwrap();
        song2.add_section(outro).unwrap();

        setlist.add_song(song2).unwrap();

        // Song 3: "Instrumental"
        let mut song3 = Song::new("Instrumental Break".to_string()).unwrap();
        song3.start_position = Some(Position::from_seconds(330.0));
        song3.song_end_position = Some(Position::from_seconds(390.0));
        song3.end_position = Some(Position::from_seconds(395.0));
        song3.set_metadata("key", "E minor");
        song3.set_metadata("tempo", "140");

        let instrumental = Section::new(SectionType::Instrumental, Position::from_seconds(330.0), Position::from_seconds(390.0), "Instrumental".to_string(), None).unwrap();
        song3.add_section(instrumental).unwrap();

        setlist.add_song(song3).unwrap();

        // Set metadata on setlist
        setlist.set_metadata("venue", "Test Venue");
        setlist.set_metadata("date", "2024-01-15");
        setlist.set_metadata("sound_engineer", "Test Engineer");

        inner.setlist = Some(setlist);
    }

    /// Initialize with complex sample data
    fn initialize_complex_data(&self) {
        let mut inner = self.inner.lock().unwrap();

        // Setup marker/region source with complex data
        inner.marker_region_source = MockMarkerRegionService::with_complex_data();

        // Create a complex setlist
        let mut setlist = Setlist::new("Full Concert Setlist".to_string()).unwrap();

        // Song 1: Complex structure with pre/post sections
        let mut song1 = Song::new("Epic Opener".to_string()).unwrap();
        song1.count_in_position = Some(Position::from_seconds(-8.0));
        song1.start_position = Some(Position::from_seconds(0.0));
        song1.song_end_position = Some(Position::from_seconds(300.0));
        song1.end_position = Some(Position::from_seconds(310.0));
        song1.set_metadata("key", "D major");
        song1.set_metadata("tempo", "130");
        song1.set_metadata("time_signature", "4/4");

        // Complex section structure
        let sections = vec![
            Section::new(SectionType::Intro, Position::from_seconds(0.0), Position::from_seconds(20.0), "Intro".to_string(), None).unwrap(),
            Section::new(SectionType::Verse, Position::from_seconds(20.0), Position::from_seconds(50.0), "Verse 1".to_string(), Some(1)).unwrap(),
            Section::new(SectionType::Pre(Box::new(SectionType::Chorus)), Position::from_seconds(50.0), Position::from_seconds(65.0), "Pre-Chorus 1".to_string(), None).unwrap(),
            Section::new(SectionType::Chorus, Position::from_seconds(65.0), Position::from_seconds(95.0), "Chorus 1".to_string(), Some(1)).unwrap(),
            Section::new(SectionType::Verse, Position::from_seconds(95.0), Position::from_seconds(125.0), "Verse 2".to_string(), Some(2)).unwrap(),
            Section::new(SectionType::Pre(Box::new(SectionType::Chorus)), Position::from_seconds(125.0), Position::from_seconds(140.0), "Pre-Chorus 2".to_string(), None).unwrap(),
            Section::new(SectionType::Chorus, Position::from_seconds(140.0), Position::from_seconds(170.0), "Chorus 2".to_string(), Some(2)).unwrap(),
            Section::new(SectionType::Bridge, Position::from_seconds(170.0), Position::from_seconds(200.0), "Bridge".to_string(), None).unwrap(),
            Section::new(SectionType::Instrumental, Position::from_seconds(200.0), Position::from_seconds(240.0), "Solo".to_string(), None).unwrap(),
            Section::new(SectionType::Chorus, Position::from_seconds(240.0), Position::from_seconds(270.0), "Final Chorus".to_string(), Some(3)).unwrap(),
            Section::new(SectionType::Post(Box::new(SectionType::Chorus)), Position::from_seconds(270.0), Position::from_seconds(285.0), "Post-Chorus".to_string(), None).unwrap(),
            Section::new(SectionType::Outro, Position::from_seconds(285.0), Position::from_seconds(300.0), "Outro".to_string(), None).unwrap(),
        ];

        for section in sections {
            song1.add_section(section).unwrap();
        }

        setlist.add_song(song1).unwrap();

        // Song 2: Multiple consecutive sections
        let mut song2 = Song::new("Verse Heavy Song".to_string()).unwrap();
        song2.start_position = Some(Position::from_seconds(315.0));
        song2.song_end_position = Some(Position::from_seconds(500.0));
        song2.end_position = Some(Position::from_seconds(505.0));

        // Multiple consecutive verses
        song2.add_section(Section::new(SectionType::Verse, Position::from_seconds(315.0), Position::from_seconds(345.0), "Verse 1a".to_string(), None).unwrap()).unwrap();
        song2.add_section(Section::new(SectionType::Verse, Position::from_seconds(345.0), Position::from_seconds(375.0), "Verse 1b".to_string(), None).unwrap()).unwrap();
        song2.add_section(Section::new(SectionType::Chorus, Position::from_seconds(375.0), Position::from_seconds(405.0), "Chorus".to_string(), None).unwrap()).unwrap();
        song2.add_section(Section::new(SectionType::Verse, Position::from_seconds(405.0), Position::from_seconds(435.0), "Verse 2a".to_string(), None).unwrap()).unwrap();
        song2.add_section(Section::new(SectionType::Verse, Position::from_seconds(435.0), Position::from_seconds(465.0), "Verse 2b".to_string(), None).unwrap()).unwrap();
        song2.add_section(Section::new(SectionType::Verse, Position::from_seconds(465.0), Position::from_seconds(495.0), "Verse 2c".to_string(), None).unwrap()).unwrap();

        // This should create split letters automatically
        song2.auto_number_sections();

        setlist.add_song(song2).unwrap();

        // Song 3: Short transition
        let mut song3 = Song::new("Interlude".to_string()).unwrap();
        song3.start_position = Some(Position::from_seconds(510.0));
        song3.end_position = Some(Position::from_seconds(540.0));
        song3.add_section(Section::new(SectionType::Instrumental, Position::from_seconds(510.0), Position::from_seconds(540.0), "Transition".to_string(), None).unwrap()).unwrap();

        setlist.add_song(song3).unwrap();

        // Song 4: Final song with complex ending
        let mut song4 = Song::new("Grand Finale".to_string()).unwrap();
        song4.count_in_position = Some(Position::from_seconds(536.0));
        song4.start_position = Some(Position::from_seconds(540.0));
        song4.song_end_position = Some(Position::from_seconds(720.0));
        song4.end_position = Some(Position::from_seconds(730.0));
        song4.set_metadata("key", "A major");
        song4.set_metadata("tempo", "160");

        let finale_sections = vec![
            Section::new(SectionType::Intro, Position::from_seconds(540.0), Position::from_seconds(560.0), "Grand Intro".to_string(), None).unwrap(),
            Section::new(SectionType::Verse, Position::from_seconds(560.0), Position::from_seconds(590.0), "Final Verse 1".to_string(), Some(1)).unwrap(),
            Section::new(SectionType::Chorus, Position::from_seconds(590.0), Position::from_seconds(620.0), "Final Chorus 1".to_string(), Some(1)).unwrap(),
            Section::new(SectionType::Verse, Position::from_seconds(620.0), Position::from_seconds(650.0), "Final Verse 2".to_string(), Some(2)).unwrap(),
            Section::new(SectionType::Chorus, Position::from_seconds(650.0), Position::from_seconds(680.0), "Final Chorus 2".to_string(), Some(2)).unwrap(),
            Section::new(SectionType::Outro, Position::from_seconds(680.0), Position::from_seconds(720.0), "Tag Ending".to_string(), None).unwrap(),
        ];

        for section in finale_sections {
            song4.add_section(section).unwrap();
        }

        setlist.add_song(song4).unwrap();

        // Set comprehensive metadata
        setlist.set_metadata("venue", "Madison Square Garden");
        setlist.set_metadata("date", "2024-12-31");
        setlist.set_metadata("sound_engineer", "Pro Audio Engineer");
        setlist.set_metadata("lighting_designer", "Light Show Pro");
        setlist.set_metadata("stage_manager", "Stage Boss");
        setlist.set_metadata("total_expected_duration", "730");

        inner.setlist = Some(setlist);
    }

    /// Enable error simulation mode
    pub fn enable_error_mode(&self) {
        let mut inner = self.inner.lock().unwrap();
        inner.error_mode = true;
    }

    /// Disable error simulation mode
    pub fn disable_error_mode(&self) {
        let mut inner = self.inner.lock().unwrap();
        inner.error_mode = false;
    }

    /// Check if error mode is enabled
    pub fn is_error_mode(&self) -> bool {
        let inner = self.inner.lock().unwrap();
        inner.error_mode
    }

    /// Clear all data
    pub fn clear(&self) {
        let mut inner = self.inner.lock().unwrap();
        inner.setlist = None;
        inner.marker_region_source = MockMarkerRegionService::new();
        inner.metadata.clear();
    }

    /// Set custom setlist data
    pub fn set_setlist(&self, setlist: Setlist) -> Result<(), SetlistError> {
        setlist.validate()?;

        let mut inner = self.inner.lock().unwrap();
        inner.setlist = Some(setlist);
        Ok(())
    }

    /// Get current setlist (if any)
    pub fn get_current_setlist(&self) -> Option<Setlist> {
        let inner = self.inner.lock().unwrap();
        inner.setlist.clone()
    }

    /// Get the underlying marker/region source
    pub fn get_marker_region_source(&self) -> MockMarkerRegionService {
        // Create a new instance since MockMarkerRegionService doesn't implement Clone
        MockMarkerRegionService::new()
    }

    /// Set custom metadata
    pub fn set_service_metadata<K: Into<String>, V: Into<String>>(&self, key: K, value: V) {
        let mut inner = self.inner.lock().unwrap();
        inner.metadata.insert(key.into(), value.into());
    }

    /// Get service metadata
    pub fn get_service_metadata(&self, key: &str) -> Option<String> {
        let inner = self.inner.lock().unwrap();
        inner.metadata.get(key).cloned()
    }
}

impl Default for MockSetlistService {
    fn default() -> Self {
        Self::new()
    }
}

impl SetlistSource for MockSetlistService {
    fn build_setlist(&self) -> Result<Setlist, SetlistError> {
        let inner = self.inner.lock().unwrap();

        if inner.error_mode {
            return Err(SetlistError::source_error("Mock service is in error mode"));
        }

        match &inner.setlist {
            Some(setlist) => Ok(setlist.clone()),
            None => {
                // Build from marker/region source if no setlist is set
                drop(inner);
                self.build_from_markers_and_regions()
            }
        }
    }

    fn source_name(&self) -> &'static str {
        "Mock"
    }

    fn is_available(&self) -> bool {
        let inner = self.inner.lock().unwrap();
        !inner.error_mode
    }

    fn get_source_info(&self) -> SourceInfo {
        let inner = self.inner.lock().unwrap();

        let mut metadata = HashMap::new();
        metadata.insert("type".to_string(), "mock".to_string());
        metadata.insert("has_data".to_string(), inner.setlist.is_some().to_string());
        metadata.insert("error_mode".to_string(), inner.error_mode.to_string());

        // Add custom metadata
        for (k, v) in &inner.metadata {
            metadata.insert(k.clone(), v.clone());
        }

        SourceInfo {
            name: "Mock Setlist Service".to_string(),
            is_available: !inner.error_mode,
            source_type: "mock".to_string(),
            metadata,
        }
    }
}

impl ApplicationSetlistSource for MockSetlistService {
    fn refresh(&mut self) -> Result<(), SetlistError> {
        // For mock service, refresh just validates current state
        let inner = self.inner.lock().unwrap();

        if inner.error_mode {
            return Err(SetlistError::source_error("Cannot refresh: service is in error mode"));
        }

        if let Some(setlist) = &inner.setlist {
            setlist.validate()?;
        }

        Ok(())
    }

    fn supports_real_time_updates(&self) -> bool {
        true // Mock service can simulate real-time updates
    }

    fn set_real_time_updates(&mut self, enabled: bool) -> Result<(), SetlistError> {
        let mut inner = self.inner.lock().unwrap();
        inner.metadata.insert("real_time_updates".to_string(), enabled.to_string());
        Ok(())
    }
}

impl MockSetlistService {
    /// Build setlist from marker/region data
    fn build_from_markers_and_regions(&self) -> Result<Setlist, SetlistError> {
        let inner = self.inner.lock().unwrap();
        let marker_source = &inner.marker_region_source;

        // Get markers and regions
        let markers = marker_source.get_markers()
            .map_err(|e| SetlistError::source_error(format!("Failed to get markers: {}", e)))?;
        let regions = marker_source.get_regions()
            .map_err(|e| SetlistError::source_error(format!("Failed to get regions: {}", e)))?;

        drop(inner);

        // Build setlist from marker/region data
        let mut setlist = Setlist::new("Generated from Markers/Regions".to_string())?;

        // Find song regions (regions that contain the word "song" or are long enough)
        let song_regions: Vec<&Region> = regions.iter()
            .filter(|r| {
                r.name.to_lowercase().contains("song") ||
                r.duration_seconds() > 60.0 // Assume songs are longer than 1 minute
            })
            .collect();

        for song_region in song_regions {
            let mut song = Song::new(song_region.name.clone())?;

            // Set song boundaries
            song.song_region_start = Some(Position::from_seconds(song_region.start_seconds()));
            song.song_region_end = Some(Position::from_seconds(song_region.end_seconds()));

            // Find markers within this song region
            let song_markers: Vec<&Marker> = markers.iter()
                .filter(|m| {
                    let pos = m.position_seconds();
                    pos >= song_region.start_seconds() && pos <= song_region.end_seconds()
                })
                .collect();

            // Look for specific marker types
            for marker in song_markers {
                let name_lower = marker.name.to_lowercase();
                if name_lower.contains("count") && name_lower.contains("in") {
                    song.count_in_position = Some(Position::from_seconds(marker.position_seconds()));
                } else if name_lower.contains("songstart") || name_lower == "start" {
                    song.start_position = Some(Position::from_seconds(marker.position_seconds()));
                } else if name_lower.contains("songend") || (name_lower == "end" && !name_lower.starts_with("=")) {
                    song.song_end_position = Some(Position::from_seconds(marker.position_seconds()));
                } else if name_lower.starts_with("=end") || name_lower == "=end" {
                    song.end_position = Some(Position::from_seconds(marker.position_seconds()));
                }
            }

            // Find section regions within this song
            let section_regions: Vec<&Region> = regions.iter()
                .filter(|r| {
                    r.start_seconds() >= song_region.start_seconds() &&
                    r.end_seconds() <= song_region.end_seconds() &&
                    r != &song_region // Don't include the song region itself
                })
                .collect();

            // Convert regions to sections
            for section_region in section_regions {
                if let Ok(section_type) = SectionType::from_str(&section_region.name) {
                    let section = Section::new(
                        section_type,
                        Position::from_seconds(section_region.start_seconds()),
                        Position::from_seconds(section_region.end_seconds()),
                        section_region.name.clone(),
                        None, // Will be auto-numbered later
                    )?;

                    song.add_section(section).unwrap_or_default(); // Ignore overlaps for generated data
                }
            }

            // Auto-number sections
            song.auto_number_sections();

            setlist.add_song(song)?;
        }

        Ok(setlist)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mock_service_creation() {
        let service = MockSetlistService::new();
        assert_eq!(service.source_name(), "Mock");
        assert!(service.is_available());

        let info = service.get_source_info();
        assert_eq!(info.source_type, "mock");
        assert!(info.is_available);
    }

    #[test]
    fn test_sample_data() {
        let service = MockSetlistService::with_sample_data();
        let setlist = service.build_setlist().unwrap();

        assert_eq!(setlist.name, "Sample Setlist");
        assert_eq!(setlist.song_count(), 3);

        // Check first song structure
        let first_song = setlist.get_song(0).unwrap();
        assert_eq!(first_song.name, "Opening Song");
        assert!(first_song.count_in_position.is_some());
        assert!(first_song.sections.len() > 0);

        // Verify sections are properly numbered
        let verses: Vec<&Section> = first_song.sections_by_type(&SectionType::Verse);
        assert!(verses.len() >= 2);
    }

    #[test]
    fn test_complex_data() {
        let service = MockSetlistService::with_complex_data();
        let setlist = service.build_setlist().unwrap();

        assert_eq!(setlist.name, "Full Concert Setlist");
        assert_eq!(setlist.song_count(), 4);

        // Check that complex sections exist
        let first_song = setlist.get_song(0).unwrap();
        let has_pre_chorus = first_song.sections.iter().any(|s|
            matches!(s.section_type, SectionType::Pre(_))
        );
        assert!(has_pre_chorus);

        // Check auto-numbering worked on consecutive sections
        let second_song = setlist.get_song(1).unwrap();
        let has_split_letters = second_song.sections.iter().any(|s| s.split_letter.is_some());
        assert!(has_split_letters);
    }

    #[test]
    fn test_error_mode() {
        let service = MockSetlistService::with_sample_data();

        // Should work normally
        assert!(service.build_setlist().is_ok());

        // Enable error mode
        service.enable_error_mode();
        assert!(!service.is_available());

        let result = service.build_setlist();
        assert!(result.is_err());

        // Disable error mode
        service.disable_error_mode();
        assert!(service.is_available());
        assert!(service.build_setlist().is_ok());
    }

    #[test]
    fn test_custom_setlist() {
        let service = MockSetlistService::new();

        let mut custom_setlist = Setlist::new("Custom Test".to_string()).unwrap();
        let song = Song::new("Test Song".to_string()).unwrap();
        custom_setlist.add_song(song).unwrap();

        service.set_setlist(custom_setlist.clone()).unwrap();

        let retrieved = service.build_setlist().unwrap();
        assert_eq!(retrieved.name, "Custom Test");
        assert_eq!(retrieved.song_count(), 1);
    }

    #[test]
    fn test_application_source_trait() {
        let mut service = MockSetlistService::with_sample_data();

        // Test refresh
        assert!(service.refresh().is_ok());

        // Test real-time updates
        assert!(service.supports_real_time_updates());
        assert!(service.set_real_time_updates(true).is_ok());

        let info = service.get_detailed_info();
        assert!(info.metadata.contains_key("layer"));
    }

    #[test]
    fn test_metadata() {
        let service = MockSetlistService::new();

        service.set_service_metadata("test_key", "test_value");
        assert_eq!(service.get_service_metadata("test_key"), Some("test_value".to_string()));

        let info = service.get_source_info();
        assert_eq!(info.metadata.get("test_key"), Some(&"test_value".to_string()));
    }

    #[test]
    fn test_build_from_markers_regions() {
        let service = MockSetlistService::new();

        // This will build from the empty marker/region source
        let setlist = service.build_setlist().unwrap();
        assert_eq!(setlist.name, "Generated from Markers/Regions");

        // Should be empty since no marker/region data
        assert_eq!(setlist.song_count(), 0);
    }
}
