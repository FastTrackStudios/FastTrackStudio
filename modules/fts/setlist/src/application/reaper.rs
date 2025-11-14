//! REAPER setlist source implementation
//!
//! This module provides a REAPER-specific implementation of the setlist source
//! using the marker-region module's REAPER integration.

use std::collections::HashMap;

use crate::core::{
    Setlist, SetlistSource, SetlistError, Song, Section, SectionType, SourceInfo
};
use crate::application::{ApplicationSetlistSource, SourceType};

use marker_region::{Marker, Region, MarkerRegionSource, ReaperMarkerRegionSource};

/// REAPER implementation of setlist source
#[derive(Debug, Clone)]
pub struct ReaperSetlistSource {
    marker_region_source: ReaperMarkerRegionSource,
    metadata: HashMap<String, String>,
}

impl ReaperSetlistSource {
    /// Create a new REAPER setlist source
    pub fn new() -> Self {
        Self {
            marker_region_source: ReaperMarkerRegionSource::new(),
            metadata: HashMap::new(),
        }
    }

    /// Create a REAPER setlist source with custom marker/region source
    pub fn with_marker_region_source(source: ReaperMarkerRegionSource) -> Self {
        Self {
            marker_region_source: source,
            metadata: HashMap::new(),
        }
    }

    /// Build a song from markers and regions within a specific region
    fn build_song(&self, song_region: &Region, all_markers: &[Marker], all_regions: &[Region]) -> Result<Song, SetlistError> {
        let mut song = Song::new(song_region.name.clone())?;

        // Set song region boundaries
        song.song_region_start = Some(song_region.start_seconds());
        song.song_region_end = Some(song_region.end_seconds());

        // Find markers within this song region
        let song_markers: Vec<&Marker> = all_markers.iter()
            .filter(|m| {
                let pos = m.position_seconds();
                pos >= song_region.start_seconds() && pos <= song_region.end_seconds()
            })
            .collect();

        // Process markers to find song boundaries
        for marker in song_markers {
            let name_lower = marker.name.to_lowercase();
            let name_trimmed = name_lower.trim();

            if Self::is_count_in_marker(name_trimmed) {
                song.count_in_position = Some(marker.position_seconds());
            } else if Self::is_song_start_marker(name_trimmed) {
                song.start_position = Some(marker.position_seconds());
            } else if Self::is_song_end_marker(name_trimmed) {
                song.song_end_position = Some(marker.position_seconds());
            } else if Self::is_hard_end_marker(name_trimmed) {
                song.end_position = Some(marker.position_seconds());
            }
        }

        // Find section regions within this song region
        let section_regions: Vec<&Region> = all_regions.iter()
            .filter(|r| {
                r.start_seconds() >= song_region.start_seconds() &&
                r.end_seconds() <= song_region.end_seconds() &&
                r != &song_region && // Don't include the song region itself
                r.duration_seconds() > 1.0 // Filter out very short regions
            })
            .collect();

        // Convert regions to sections
        for section_region in section_regions {
            if let Some(section_type) = Self::parse_section_type_from_name(&section_region.name) {
                let section = Section::new(
                    section_type,
                    section_region.start_seconds(),
                    section_region.end_seconds(),
                    section_region.name.clone(),
                    None, // Will be auto-numbered later
                )?;

                // Try to add section, but don't fail if there are overlaps
                // (REAPER projects might have overlapping regions)
                if let Err(e) = song.add_section(section) {
                    // Log warning but continue
                    eprintln!("Warning: Failed to add section '{}' to song '{}': {}",
                             section_region.name, song.name, e);
                }
            }
        }

        // Auto-number sections
        song.auto_number_sections();

        Ok(song)
    }

    /// Check if a marker name indicates a count-in
    fn is_count_in_marker(name: &str) -> bool {
        name.contains("count") && (name.contains("in") || name.contains("-in")) ||
        name == "countin" ||
        name.starts_with("count") && name.len() <= 8
    }

    /// Check if a marker name indicates song start
    fn is_song_start_marker(name: &str) -> bool {
        name == "songstart" ||
        name == "song start" ||
        name == "start" ||
        name == "song-start" ||
        name.starts_with("songstart")
    }

    /// Check if a marker name indicates song end
    fn is_song_end_marker(name: &str) -> bool {
        name == "songend" ||
        name == "song end" ||
        name == "song-end" ||
        name.starts_with("songend") ||
        (name == "end" && !name.starts_with("="))
    }

    /// Check if a marker name indicates hard end
    fn is_hard_end_marker(name: &str) -> bool {
        name == "=end" ||
        name.starts_with("=end") ||
        name == "hardend" ||
        name == "hard end" ||
        name == "cut"
    }

    /// Try to parse a section type from a region name
    fn parse_section_type_from_name(name: &str) -> Option<SectionType> {
        // Try direct parsing first
        if let Ok(section_type) = SectionType::from_str(name) {
            return Some(section_type);
        }

        // Try to extract section type from name with numbers/letters
        // Split by whitespace and try each part
        let parts: Vec<&str> = name.split_whitespace().collect();
        if parts.is_empty() {
            return None;
        }

        // Try first part as section type
        if let Ok(section_type) = SectionType::from_str(parts[0]) {
            return Some(section_type);
        }

        // Try first two parts combined (e.g., "Pre Chorus")
        if parts.len() >= 2 {
            let combined = format!("{}-{}", parts[0], parts[1]);
            if let Ok(section_type) = SectionType::from_str(&combined) {
                return Some(section_type);
            }
        }

        // Try common patterns
        let name_lower = name.to_lowercase();

        // Handle numbered sections (e.g., "Verse 1", "Chorus 2")
        for section_word in ["verse", "chorus", "bridge", "intro", "outro", "instrumental"] {
            if name_lower.starts_with(section_word) {
                if let Ok(section_type) = SectionType::from_str(section_word) {
                    return Some(section_type);
                }
            }
        }

        // Handle abbreviated sections (e.g., "VS1", "CH2")
        if name_lower.starts_with("vs") || name_lower.starts_with("v") && name_lower.len() <= 3 {
            return Some(SectionType::Verse);
        }
        if name_lower.starts_with("ch") || name_lower.starts_with("c") && name_lower.len() <= 3 {
            return Some(SectionType::Chorus);
        }
        if name_lower.starts_with("br") || name_lower.starts_with("b") && name_lower.len() <= 3 {
            return Some(SectionType::Bridge);
        }

        None
    }

    /// Find regions that likely represent songs
    fn find_song_regions(&self, regions: &[Region]) -> Vec<&Region> {
        regions.iter()
            .filter(|r| {
                let name_lower = r.name.to_lowercase();
                let duration = r.duration_seconds();

                // Check if region name suggests it's a song
                let has_song_keyword = name_lower.contains("song") ||
                    name_lower.starts_with("track") ||
                    name_lower.contains("tune");

                // Check if it's long enough to be a song (typically > 30 seconds)
                let is_song_length = duration > 30.0;

                // Check if it contains typical song words
                let has_song_indicators = [
                    "ballad", "rock", "blues", "folk", "pop", "jazz", "country",
                    "acoustic", "electric", "slow", "fast", "up tempo", "downtempo"
                ].iter().any(|word| name_lower.contains(word));

                has_song_keyword || (is_song_length && !Self::is_likely_section_name(&r.name)) || has_song_indicators
            })
            .collect()
    }

    /// Check if a region name is likely a section rather than a song
    fn is_likely_section_name(name: &str) -> bool {
        let name_lower = name.to_lowercase();

        // Common section indicators
        let section_keywords = [
            "verse", "chorus", "bridge", "intro", "outro", "instrumental",
            "pre", "post", "break", "solo", "interlude", "tag", "coda",
            "vs", "ch", "br", "inst"
        ];

        section_keywords.iter().any(|keyword| {
            name_lower.starts_with(keyword) ||
            name_lower.contains(&format!(" {}", keyword)) ||
            name_lower.contains(&format!("{} ", keyword))
        })
    }

    /// Find the song region that contains a given marker
    fn find_song_region_for_marker<'a>(&self, marker: &Marker, song_regions: &[&'a Region]) -> Option<&'a Region> {
        let marker_pos = marker.position_seconds();

        // Find the song region that contains this marker
        song_regions.iter()
            .find(|region| {
                marker_pos >= region.start_seconds() && marker_pos <= region.end_seconds()
            })
            .copied()
    }

    /// Set custom metadata
    pub fn set_metadata<K: Into<String>, V: Into<String>>(&mut self, key: K, value: V) {
        self.metadata.insert(key.into(), value.into());
    }

    /// Get metadata value
    pub fn get_metadata(&self, key: &str) -> Option<&String> {
        self.metadata.get(key)
    }
}

impl Default for ReaperSetlistSource {
    fn default() -> Self {
        Self::new()
    }
}

impl SetlistSource for ReaperSetlistSource {
    fn build_setlist(&self) -> Result<Setlist, SetlistError> {
        // Get markers and regions from REAPER
        let markers = self.marker_region_source.get_markers()
            .map_err(|e| SetlistError::source_error(format!("Failed to get REAPER markers: {}", e)))?;

        let regions = self.marker_region_source.get_regions()
            .map_err(|e| SetlistError::source_error(format!("Failed to get REAPER regions: {}", e)))?;

        // Create setlist
        let mut setlist = Setlist::new("REAPER Project Setlist".to_string())?;

        // Find regions that represent songs
        let song_regions = self.find_song_regions(&regions);

        // If no clear song regions found, try to create songs from markers
        if song_regions.is_empty() {
            return self.build_setlist_from_markers_only(&markers, &regions);
        }

        // Build songs from song regions
        for song_region in song_regions {
            match self.build_song(song_region, &markers, &regions) {
                Ok(song) => {
                    if let Err(e) = setlist.add_song(song) {
                        eprintln!("Warning: Failed to add song '{}' to setlist: {}", song_region.name, e);
                    }
                }
                Err(e) => {
                    eprintln!("Warning: Failed to build song from region '{}': {}", song_region.name, e);
                }
            }
        }

        Ok(setlist)
    }

    fn source_name(&self) -> &'static str {
        "REAPER"
    }

    fn is_available(&self) -> bool {
        // Check if REAPER is available through the marker/region source
        // Note: This is a stub implementation - in real REAPER integration,
        // this would check if REAPER is running and accessible
        true
    }

    fn get_source_info(&self) -> SourceInfo {
        let mut metadata = HashMap::new();
        metadata.insert("type".to_string(), "reaper".to_string());
        metadata.insert("integration".to_string(), "marker-region".to_string());

        // Add custom metadata
        for (k, v) in &self.metadata {
            metadata.insert(k.clone(), v.clone());
        }

        SourceInfo {
            name: "REAPER DAW".to_string(),
            is_available: self.is_available(),
            source_type: "reaper".to_string(),
            metadata,
        }
    }
}

impl ApplicationSetlistSource for ReaperSetlistSource {
    fn refresh(&mut self) -> Result<(), SetlistError> {
        // For REAPER, refresh would reload project data
        // This is a stub implementation
        Ok(())
    }

    fn supports_real_time_updates(&self) -> bool {
        // REAPER could support real-time updates via its API
        false
    }
}

impl ReaperSetlistSource {
    /// Build setlist when no clear song regions exist - use markers to define songs
    fn build_setlist_from_markers_only(&self, markers: &[Marker], regions: &[Region]) -> Result<Setlist, SetlistError> {
        let mut setlist = Setlist::new("REAPER Project (Marker-based)".to_string())?;

        // Find song-related markers
        let mut song_starts: Vec<&Marker> = Vec::new();
        let mut song_ends: Vec<&Marker> = Vec::new();

        for marker in markers {
            let name_lower = marker.name.to_lowercase();

            if Self::is_song_start_marker(&name_lower) {
                song_starts.push(marker);
            } else if Self::is_song_end_marker(&name_lower) || Self::is_hard_end_marker(&name_lower) {
                song_ends.push(marker);
            }
        }

        // If we have song start/end markers, build songs from them
        if !song_starts.is_empty() {
            for (i, start_marker) in song_starts.iter().enumerate() {
                // Find the corresponding end marker
                let end_marker = song_ends.iter()
                    .find(|end| end.position_seconds() > start_marker.position_seconds())
                    .or_else(|| {
                        // If no explicit end, use next song start or project end
                        if i + 1 < song_starts.len() {
                            Some(song_starts[i + 1])
                        } else {
                            None
                        }
                    });

                let song_name = format!("Song {}", i + 1);
                let mut song = Song::new(song_name)?;

                song.start_position = Some(start_marker.position_seconds());

                if let Some(end) = end_marker {
                    if Self::is_hard_end_marker(&end.name.to_lowercase()) {
                        song.end_position = Some(end.position_seconds());
                    } else {
                        song.song_end_position = Some(end.position_seconds());
                    }
                }

                // Find sections between start and end
                let song_start = start_marker.position_seconds();
                let song_end = end_marker.map(|m| m.position_seconds()).unwrap_or(song_start + 180.0);

                let song_regions: Vec<&Region> = regions.iter()
                    .filter(|r| {
                        r.start_seconds() >= song_start &&
                        r.end_seconds() <= song_end &&
                        Self::parse_section_type_from_name(&r.name).is_some()
                    })
                    .collect();

                for region in song_regions {
                    if let Some(section_type) = Self::parse_section_type_from_name(&region.name) {
                        let section = Section::new(
                            section_type,
                            region.start_seconds(),
                            region.end_seconds(),
                            region.name.clone(),
                            None,
                        )?;

                        let _ = song.add_section(section); // Ignore overlaps
                    }
                }

                song.auto_number_sections();
                let _ = setlist.add_song(song); // Ignore duplicates
            }
        }

        Ok(setlist)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_reaper_source_creation() {
        let source = ReaperSetlistSource::new();
        assert_eq!(source.source_name(), "REAPER");
        assert!(source.is_available());

        let info = source.get_source_info();
        assert_eq!(info.source_type, "reaper");
    }

    #[test]
    fn test_marker_recognition() {
        assert!(ReaperSetlistSource::is_count_in_marker("count-in"));
        assert!(ReaperSetlistSource::is_count_in_marker("countin"));
        assert!(ReaperSetlistSource::is_count_in_marker("count in"));
        assert!(!ReaperSetlistSource::is_count_in_marker("countdown"));

        assert!(ReaperSetlistSource::is_song_start_marker("songstart"));
        assert!(ReaperSetlistSource::is_song_start_marker("song start"));
        assert!(ReaperSetlistSource::is_song_start_marker("start"));
        assert!(!ReaperSetlistSource::is_song_start_marker("restart"));

        assert!(ReaperSetlistSource::is_song_end_marker("songend"));
        assert!(ReaperSetlistSource::is_song_end_marker("song end"));
        assert!(ReaperSetlistSource::is_song_end_marker("end"));
        assert!(!ReaperSetlistSource::is_song_end_marker("=end"));

        assert!(ReaperSetlistSource::is_hard_end_marker("=end"));
        assert!(ReaperSetlistSource::is_hard_end_marker("hardend"));
        assert!(ReaperSetlistSource::is_hard_end_marker("cut"));
        assert!(!ReaperSetlistSource::is_hard_end_marker("end"));
    }

    #[test]
    fn test_section_type_parsing() {
        assert_eq!(
            ReaperSetlistSource::parse_section_type_from_name("Verse 1"),
            Some(SectionType::Verse)
        );
        assert_eq!(
            ReaperSetlistSource::parse_section_type_from_name("CH"),
            Some(SectionType::Chorus)
        );
        assert_eq!(
            ReaperSetlistSource::parse_section_type_from_name("Pre Chorus"),
            Some(SectionType::Pre(Box::new(SectionType::Chorus)))
        );
        assert_eq!(
            ReaperSetlistSource::parse_section_type_from_name("VS2"),
            Some(SectionType::Verse)
        );
        assert_eq!(
            ReaperSetlistSource::parse_section_type_from_name("Random Text"),
            None
        );
    }

    #[test]
    fn test_song_region_detection() {
        use marker_region::{Region};
        use primitives::Position;

        let regions = vec![
            Region::from_seconds(0.0, 180.0, "Song 1".to_string()).unwrap(),
            Region::from_seconds(10.0, 40.0, "Verse 1".to_string()).unwrap(),
            Region::from_seconds(40.0, 70.0, "Chorus 1".to_string()).unwrap(),
            Region::from_seconds(200.0, 320.0, "Track 2".to_string()).unwrap(),
            Region::from_seconds(220.0, 250.0, "VS".to_string()).unwrap(),
        ];

        let source = ReaperSetlistSource::new();
        let song_regions = source.find_song_regions(&regions);

        assert_eq!(song_regions.len(), 2);
        assert_eq!(song_regions[0].name, "Song 1");
        assert_eq!(song_regions[1].name, "Track 2");
    }

    #[test]
    fn test_section_name_detection() {
        assert!(ReaperSetlistSource::is_likely_section_name("Verse 1"));
        assert!(ReaperSetlistSource::is_likely_section_name("Chorus"));
        assert!(ReaperSetlistSource::is_likely_section_name("VS"));
        assert!(ReaperSetlistSource::is_likely_section_name("Pre-Chorus"));

        assert!(!ReaperSetlistSource::is_likely_section_name("Song Title"));
        assert!(!ReaperSetlistSource::is_likely_section_name("Track 1"));
        assert!(!ReaperSetlistSource::is_likely_section_name("My Song"));
    }

    #[test]
    fn test_application_source_trait() {
        let mut source = ReaperSetlistSource::new();

        // Test refresh
        assert!(source.refresh().is_ok());

        // Test real-time updates
        assert!(!source.supports_real_time_updates());

        let info = source.get_detailed_info();
        assert_eq!(info.metadata.get("layer"), Some(&"application".to_string()));
    }

    #[test]
    fn test_metadata() {
        let mut source = ReaperSetlistSource::new();

        source.set_metadata("project_path", "/path/to/project.rpp");
        assert_eq!(source.get_metadata("project_path"), Some(&"/path/to/project.rpp".to_string()));

        let info = source.get_source_info();
        assert_eq!(info.metadata.get("project_path"), Some(&"/path/to/project.rpp".to_string()));
    }

    #[test]
    fn test_empty_data_handling() {
        let source = ReaperSetlistSource::new();

        // This should work with empty marker/region data from the mock source
        let result = source.build_setlist();
        assert!(result.is_ok());

        let setlist = result.unwrap();
        assert_eq!(setlist.name, "REAPER Project Setlist");
        // May be empty if no data available
    }
}
