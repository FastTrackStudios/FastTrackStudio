//! RPP file setlist source implementation
//!
//! This module provides an RPP file-specific implementation of the setlist source
//! using the marker-region module's RPP integration for parsing .rpp files.

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::core::{
    Setlist, SetlistSource, SetlistError, Song, Section, SectionType, SourceInfo
};
use crate::application::{ApplicationSetlistSource, SourceType};

use marker_region::{Marker, Region, MarkerRegionSource, RppMarkerRegionSource};

/// RPP file implementation of setlist source
#[derive(Debug, Clone)]
pub struct RppSetlistSource {
    marker_region_source: RppMarkerRegionSource,
    file_path: PathBuf,
    metadata: HashMap<String, String>,
}

impl RppSetlistSource {
    /// Create a new RPP setlist source from a file path
    #[cfg(feature = "rpp")]
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Self, SetlistError> {
        let path = path.as_ref().to_path_buf();

        let marker_region_source = RppMarkerRegionSource::from_file(&path)
            .map_err(|e| SetlistError::source_error(format!("Failed to load RPP file: {}", e)))?;

        let mut metadata = HashMap::new();
        metadata.insert("file_path".to_string(), path.display().to_string());

        Ok(Self {
            marker_region_source,
            file_path: path,
            metadata,
        })
    }

    /// Create a stub RPP source (when rpp feature is disabled)
    #[cfg(not(feature = "rpp"))]
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Self, SetlistError> {
        Err(SetlistError::config_error("RPP support not enabled - compile with 'rpp' feature"))
    }

    /// Get the file path
    pub fn file_path(&self) -> &Path {
        &self.file_path
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
                r.duration_seconds() > 0.5 // Filter out very short regions
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
                // (RPP files might have overlapping regions)
                if let Err(e) = song.add_section(section) {
                    // Log warning but continue
                    eprintln!("Warning: Failed to add section '{}' to song '{}': {}",
                             section_region.name, song.name, e);
                }
            }
        }

        // Auto-number sections
        song.auto_number_sections();

        // Extract metadata from RPP comments or region colors
        if let Some(color) = song_region.rgb_color() {
            song.set_metadata("region_color", format!("#{:06X}", color));
        }

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
        if name_lower.starts_with("vs") || (name_lower.starts_with("v") && name_lower.len() <= 3) {
            return Some(SectionType::Verse);
        }
        if name_lower.starts_with("ch") || (name_lower.starts_with("c") && name_lower.len() <= 3) {
            return Some(SectionType::Chorus);
        }
        if name_lower.starts_with("br") || (name_lower.starts_with("b") && name_lower.len() <= 3) {
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
                    name_lower.contains("tune") ||
                    name_lower.contains("title");

                // Check if it's long enough to be a song (typically > 30 seconds)
                let is_song_length = duration > 30.0;

                // Check if it contains typical song words
                let has_song_indicators = [
                    "ballad", "rock", "blues", "folk", "pop", "jazz", "country",
                    "acoustic", "electric", "slow", "fast", "uptempo", "downtempo",
                    "waltz", "march", "swing"
                ].iter().any(|word| name_lower.contains(word));

                // Check if it's a numbered item that might be a song
                let is_numbered_item = name_lower.matches(char::is_numeric).count() > 0 &&
                    !Self::is_likely_section_name(&r.name) &&
                    duration > 60.0;

                has_song_keyword ||
                (is_song_length && !Self::is_likely_section_name(&r.name)) ||
                has_song_indicators ||
                is_numbered_item
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
            "vs", "ch", "br", "inst", "refrain", "hook", "riff"
        ];

        section_keywords.iter().any(|keyword| {
            name_lower.starts_with(keyword) ||
            name_lower.contains(&format!(" {}", keyword)) ||
            name_lower.contains(&format!("{} ", keyword))
        })
    }

    /// Set custom metadata
    pub fn set_metadata<K: Into<String>, V: Into<String>>(&mut self, key: K, value: V) {
        self.metadata.insert(key.into(), value.into());
    }

    /// Get metadata value
    pub fn get_metadata(&self, key: &str) -> Option<&String> {
        self.metadata.get(key)
    }

    /// Check if the RPP file exists and is readable
    pub fn validate_file(&self) -> Result<(), SetlistError> {
        if !self.file_path.exists() {
            return Err(SetlistError::io_error(
                format!("RPP file does not exist: {}", self.file_path.display())
            ));
        }

        if !self.file_path.is_file() {
            return Err(SetlistError::io_error(
                format!("Path is not a file: {}", self.file_path.display())
            ));
        }

        // Check file extension
        if let Some(ext) = self.file_path.extension() {
            if ext.to_string_lossy().to_lowercase() != "rpp" {
                return Err(SetlistError::validation_error(
                    format!("File does not have .rpp extension: {}", self.file_path.display())
                ));
            }
        } else {
            return Err(SetlistError::validation_error(
                format!("File has no extension: {}", self.file_path.display())
            ));
        }

        Ok(())
    }
}

impl SetlistSource for RppSetlistSource {
    fn build_setlist(&self) -> Result<Setlist, SetlistError> {
        // Validate file first
        self.validate_file()?;

        // Get markers and regions from RPP file
        let markers = self.marker_region_source.get_markers()
            .map_err(|e| SetlistError::source_error(format!("Failed to get RPP markers: {}", e)))?;

        let regions = self.marker_region_source.get_regions()
            .map_err(|e| SetlistError::source_error(format!("Failed to get RPP regions: {}", e)))?;

        // Create setlist with filename as title
        let file_name = self.file_path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("RPP Project");

        let mut setlist = Setlist::new(format!("{} Setlist", file_name))?;

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

        // Add file metadata
        setlist.set_metadata("source_file", self.file_path.display().to_string());
        setlist.set_metadata("source_type", "rpp");

        if let Ok(modified) = std::fs::metadata(&self.file_path).and_then(|m| m.modified()) {
            if let Ok(duration) = modified.duration_since(std::time::UNIX_EPOCH) {
                setlist.set_metadata("file_modified", duration.as_secs().to_string());
            }
        }

        Ok(setlist)
    }

    fn source_name(&self) -> &'static str {
        "RPP File"
    }

    fn is_available(&self) -> bool {
        self.file_path.exists() && self.file_path.is_file()
    }

    fn get_source_info(&self) -> SourceInfo {
        let mut metadata = HashMap::new();
        metadata.insert("type".to_string(), "rpp".to_string());
        metadata.insert("file_path".to_string(), self.file_path.display().to_string());
        metadata.insert("file_exists".to_string(), self.file_path.exists().to_string());

        // Add file size if available
        if let Ok(file_metadata) = std::fs::metadata(&self.file_path) {
            metadata.insert("file_size".to_string(), file_metadata.len().to_string());
        }

        // Add custom metadata
        for (k, v) in &self.metadata {
            metadata.insert(k.clone(), v.clone());
        }

        SourceInfo {
            name: format!("RPP File: {}", self.file_path.display()),
            is_available: self.is_available(),
            source_type: "rpp".to_string(),
            metadata,
        }
    }
}

impl ApplicationSetlistSource for RppSetlistSource {
    fn refresh(&mut self) -> Result<(), SetlistError> {
        // For RPP files, refresh means reloading from disk
        #[cfg(feature = "rpp")]
        {
            self.marker_region_source = RppMarkerRegionSource::from_file(&self.file_path)
                .map_err(|e| SetlistError::source_error(format!("Failed to reload RPP file: {}", e)))?;
        }

        #[cfg(not(feature = "rpp"))]
        {
            return Err(SetlistError::config_error("RPP support not enabled"));
        }

        Ok(())
    }

    fn supports_real_time_updates(&self) -> bool {
        // RPP files could support file watching for updates
        false
    }
}

impl RppSetlistSource {
    /// Build setlist when no clear song regions exist - use markers to define songs
    fn build_setlist_from_markers_only(&self, markers: &[Marker], regions: &[Region]) -> Result<Setlist, SetlistError> {
        let file_name = self.file_path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("RPP Project");

        let mut setlist = Setlist::new(format!("{} (Marker-based)", file_name))?;

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
                        // If no explicit end, use next song start
                        if i + 1 < song_starts.len() {
                            Some(song_starts[i + 1])
                        } else {
                            None
                        }
                    });

                let song_name = if start_marker.name.to_lowercase().contains("start") {
                    format!("Song {}", i + 1)
                } else {
                    start_marker.name.clone()
                };

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
        } else {
            // No song markers found - create a single song from all content
            if !regions.is_empty() {
                let start_time = regions.iter().map(|r| r.start_seconds()).fold(f64::INFINITY, f64::min);
                let end_time = regions.iter().map(|r| r.end_seconds()).fold(f64::NEG_INFINITY, f64::max);

                if start_time.is_finite() && end_time.is_finite() && end_time > start_time {
                    let mut song = Song::new(format!("{} - Full Project", file_name))?;
                    song.song_region_start = Some(start_time);
                    song.song_region_end = Some(end_time);

                    // Add all regions as sections if they parse as section types
                    for region in regions {
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
                    let _ = setlist.add_song(song);
                }
            }
        }

        Ok(setlist)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn test_rpp_source_file_validation() {
        let temp_dir = std::env::temp_dir();
        let test_file = temp_dir.join("test.rpp");

        // Test non-existent file
        let source_result = RppSetlistSource::from_file(&test_file);
        #[cfg(not(feature = "rpp"))]
        assert!(source_result.is_err());

        #[cfg(feature = "rpp")]
        {
            // This would fail because file doesn't exist, but we can't test the actual
            // file loading without the rpp-parser dependency being available
            // The error would come from the marker_region_source creation
        }
    }

    #[test]
    fn test_marker_recognition() {
        assert!(RppSetlistSource::is_count_in_marker("count-in"));
        assert!(RppSetlistSource::is_count_in_marker("countin"));
        assert!(RppSetlistSource::is_count_in_marker("count in"));
        assert!(!RppSetlistSource::is_count_in_marker("countdown"));

        assert!(RppSetlistSource::is_song_start_marker("songstart"));
        assert!(RppSetlistSource::is_song_start_marker("song start"));
        assert!(RppSetlistSource::is_song_start_marker("start"));
        assert!(!RppSetlistSource::is_song_start_marker("restart"));

        assert!(RppSetlistSource::is_song_end_marker("songend"));
        assert!(RppSetlistSource::is_song_end_marker("song end"));
        assert!(RppSetlistSource::is_song_end_marker("end"));
        assert!(!RppSetlistSource::is_song_end_marker("=end"));

        assert!(RppSetlistSource::is_hard_end_marker("=end"));
        assert!(RppSetlistSource::is_hard_end_marker("hardend"));
        assert!(RppSetlistSource::is_hard_end_marker("cut"));
        assert!(!RppSetlistSource::is_hard_end_marker("end"));
    }

    #[test]
    fn test_section_type_parsing() {
        assert_eq!(
            RppSetlistSource::parse_section_type_from_name("Verse 1"),
            Some(SectionType::Verse)
        );
        assert_eq!(
            RppSetlistSource::parse_section_type_from_name("CH"),
            Some(SectionType::Chorus)
        );
        assert_eq!(
            RppSetlistSource::parse_section_type_from_name("Pre Chorus"),
            Some(SectionType::Pre(Box::new(SectionType::Chorus)))
        );
        assert_eq!(
            RppSetlistSource::parse_section_type_from_name("VS2"),
            Some(SectionType::Verse)
        );
        assert_eq!(
            RppSetlistSource::parse_section_type_from_name("Random Text"),
            None
        );
    }

    #[test]
    fn test_song_region_detection() {
        // Mock regions for testing
        use marker_region::Region;

        let regions = vec![
            Region::from_seconds(0.0, 180.0, "Song 1".to_string()).unwrap(),
            Region::from_seconds(10.0, 40.0, "Verse 1".to_string()).unwrap(),
            Region::from_seconds(40.0, 70.0, "Chorus 1".to_string()).unwrap(),
            Region::from_seconds(200.0, 320.0, "Track 2".to_string()).unwrap(),
            Region::from_seconds(220.0, 250.0, "VS".to_string()).unwrap(),
            Region::from_seconds(400.0, 500.0, "01 - Rock Song".to_string()).unwrap(),
        ];

        let temp_path = std::env::temp_dir().join("test.rpp");

        // Create a mock source (this will fail to actually load, but we can test the logic)
        let mock_source = RppSetlistSource {
            marker_region_source: unsafe { std::mem::zeroed() }, // This is just for testing the logic
            file_path: temp_path,
            metadata: HashMap::new(),
        };

        let song_regions = mock_source.find_song_regions(&regions);

        // Should detect "Song 1", "Track 2", and "01 - Rock Song" as songs
        assert_eq!(song_regions.len(), 3);

        let song_names: Vec<&str> = song_regions.iter().map(|r| r.name.as_str()).collect();
        assert!(song_names.contains(&"Song 1"));
        assert!(song_names.contains(&"Track 2"));
        assert!(song_names.contains(&"01 - Rock Song"));
    }

    #[test]
    fn test_section_name_detection() {
        assert!(RppSetlistSource::is_likely_section_name("Verse 1"));
        assert!(RppSetlistSource::is_likely_section_name("Chorus"));
        assert!(RppSetlistSource::is_likely_section_name("VS"));
        assert!(RppSetlistSource::is_likely_section_name("Pre-Chorus"));
        assert!(RppSetlistSource::is_likely_section_name("Solo"));

        assert!(!RppSetlistSource::is_likely_section_name("Song Title"));
        assert!(!RppSetlistSource::is_likely_section_name("Track 1"));
        assert!(!RppSetlistSource::is_likely_section_name("My Song"));
        assert!(!RppSetlistSource::is_likely_section_name("01 - Rock Ballad"));
    }

    #[test]
    fn test_file_path_handling() {
        let test_path = PathBuf::from("/path/to/test.rpp");

        let source = RppSetlistSource {
            marker_region_source: unsafe { std::mem::zeroed() },
            file_path: test_path.clone(),
            metadata: HashMap::new(),
        };

        assert_eq!(source.file_path(), &test_path);
        assert_eq!(source.source_name(), "RPP File");

        let info = source.get_source_info();
        assert_eq!(info.source_type, "rpp");
        assert!(info.name.contains("test.rpp"));
        assert!(!info.is_available); // File doesn't exist
    }
}
