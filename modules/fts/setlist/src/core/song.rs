//! Song domain types for setlist functionality
//!
//! This module contains the core domain types for managing songs within a setlist,
//! including song metadata, sections, and marker positions.

use std::collections::HashMap;
use serde::{Deserialize, Serialize};
use ts_rs::TS;
use primitives::{Position, TimePosition, TimeRange};

use super::{Section, SectionType, SetlistError};

/// Represents a song in the setlist
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct Song {
    /// Unique identifier for this song
    pub id: Option<uuid::Uuid>,
    /// Song name (from the song region name)
    pub name: String,
    /// Count-In marker position (marker named "Count-In")
    pub count_in_position: Option<Position>,
    /// SONGSTART marker position (render start, where real song content begins)
    pub start_position: Option<Position>,
    /// SONGEND marker position (last hit of song, but may have audio after)
    pub song_end_position: Option<Position>,
    /// =END marker position (hard cut, no more audio)
    pub end_position: Option<Position>,
    /// Song region boundaries (start and end of the entire song region)
    pub song_region_start: Option<Position>,
    pub song_region_end: Option<Position>,
    /// List of sections in the song (from regions between SONGSTART and SONGEND)
    pub sections: Vec<Section>,
    /// Optional metadata (key, tempo, etc.)
    pub metadata: HashMap<String, String>,
}

impl Song {
    /// Create a new song
    pub fn new(name: String) -> Result<Self, SetlistError> {
        if name.trim().is_empty() {
            return Err(SetlistError::invalid_song("Song name cannot be empty"));
        }

        Ok(Self {
            id: None,
            name: name.trim().to_string(),
            count_in_position: None,
            start_position: None,
            song_end_position: None,
            end_position: None,
            song_region_start: None,
            song_region_end: None,
            sections: Vec::new(),
            metadata: HashMap::new(),
        })
    }

    /// Create a new song with ID
    pub fn with_id(id: uuid::Uuid, name: String) -> Result<Self, SetlistError> {
        let mut song = Self::new(name)?;
        song.id = Some(id);
        Ok(song)
    }

    /// Get the effective start position (either SONGSTART or song region start)
    pub fn effective_start(&self) -> f64 {
        self.start_position
            .or(self.song_region_start)
            .map(|p| p.time.to_seconds())
            .unwrap_or(0.0)
    }

    /// Get the effective end position (either =END or SONGEND or song region end)
    pub fn effective_end(&self) -> f64 {
        self.end_position
            .or(self.song_end_position)
            .or(self.song_region_end)
            .map(|p| p.time.to_seconds())
            .unwrap_or(0.0)
    }

    /// Get the render start position (SONGSTART or song region start if no SONGSTART)
    pub fn render_start(&self) -> f64 {
        self.start_position
            .or(self.song_region_start)
            .map(|p| p.time.to_seconds())
            .unwrap_or(0.0)
    }

    /// Get the render end position (SONGEND or song region end if no SONGEND)
    pub fn render_end(&self) -> f64 {
        self.song_end_position
            .or(self.song_region_end)
            .map(|p| p.time.to_seconds())
            .unwrap_or(0.0)
    }

    /// Get the hard cut position (=END or song region end if no =END)
    pub fn hard_cut(&self) -> f64 {
        self.end_position
            .or(self.song_region_end)
            .map(|p| p.time.to_seconds())
            .unwrap_or(0.0)
    }

    /// Get the total duration of the song
    pub fn duration(&self) -> f64 {
        let start = self.effective_start();
        let end = self.effective_end();
        (end - start).max(0.0)
    }

    /// Get the render duration (from render start to render end)
    pub fn render_duration(&self) -> f64 {
        let start = self.render_start();
        let end = self.render_end();
        (end - start).max(0.0)
    }

    /// Add a section to the song
    pub fn add_section(&mut self, section: Section) -> Result<(), SetlistError> {
        // Validate that the section doesn't have an invalid time range
        section.validate()?;

        // Check for overlapping sections if needed
        if self.has_overlapping_sections_with(&section) {
            return Err(SetlistError::validation_error(
                format!("Section '{}' overlaps with existing sections", section.name)
            ));
        }

        self.sections.push(section);
        Ok(())
    }

    /// Remove a section by index
    pub fn remove_section(&mut self, index: usize) -> Result<Section, SetlistError> {
        if index >= self.sections.len() {
            return Err(SetlistError::not_found("section", index.to_string()));
        }

        Ok(self.sections.remove(index))
    }

    /// Get a section by index
    pub fn get_section(&self, index: usize) -> Option<&Section> {
        self.sections.get(index)
    }

    /// Get a mutable reference to a section by index
    pub fn get_section_mut(&mut self, index: usize) -> Option<&mut Section> {
        self.sections.get_mut(index)
    }

    /// Find sections by type
    pub fn sections_by_type(&self, section_type: &SectionType) -> Vec<&Section> {
        self.sections
            .iter()
            .filter(|s| &s.section_type == section_type)
            .collect()
    }

    /// Find section at a specific time position
    pub fn section_at_position(&self, seconds: f64) -> Option<&Section> {
        self.sections
            .iter()
            .find(|s| s.contains_position(seconds))
    }

    /// Get all sections in time order
    pub fn sections_in_order(&self) -> Vec<&Section> {
        let mut sections = self.sections.iter().collect::<Vec<_>>();
        sections.sort_by(|a, b| a.start_seconds.partial_cmp(&b.start_seconds).unwrap());
        sections
    }

    /// Check if adding a section would create overlaps
    pub fn has_overlapping_sections_with(&self, new_section: &Section) -> bool {
        self.sections
            .iter()
            .any(|existing| existing.overlaps_with_section(new_section))
    }

    /// Auto-number sections that don't have numbers
    ///
    /// Rules:
    /// - Intro and Outro are never numbered
    /// - Sections of the same type are numbered sequentially (1, 2, 3, etc.)
    /// - If there are consecutive sections of the same type, they get split letters (a, b, c)
    pub fn auto_number_sections(&mut self) {
        // Sort sections by start time first
        self.sections.sort_by(|a, b| a.start_seconds.partial_cmp(&b.start_seconds).unwrap());

        // Count occurrences of each section type that should be numbered
        let mut type_counts: HashMap<SectionType, u32> = HashMap::new();
        for section in &self.sections {
            let stype = &section.section_type;
            if stype.should_be_numbered() {
                *type_counts.entry(stype.clone()).or_insert(0) += 1;
            }
        }

        // Assign numbers and split letters
        let mut counters: HashMap<SectionType, u32> = HashMap::new();
        let mut prev_type: Option<SectionType> = None;
        let mut consecutive_count = 0;
        let mut current_group_number: Option<u32> = None;

        for i in 0..self.sections.len() {
            let stype = self.sections[i].section_type.clone();

            // Don't number Intro/Outro/Pre/Post
            if !stype.should_be_numbered() {
                prev_type = Some(stype);
                consecutive_count = 0;
                current_group_number = None;
                continue;
            }

            // Only number if appears more than once
            let total_count = type_counts.get(&stype).copied().unwrap_or(0);
            let should_number = total_count > 1;

            // Check if consecutive
            let is_consecutive = prev_type.as_ref() == Some(&stype);

            if is_consecutive {
                consecutive_count += 1;

                // First consecutive: setup group
                if consecutive_count == 1 && i > 0 {
                    if let Some(prev_num) = self.sections[i - 1].number {
                        current_group_number = Some(prev_num);
                    } else if should_number {
                        let count = counters.entry(stype.clone()).or_insert(0);
                        *count += 1;
                        current_group_number = Some(*count);
                        self.sections[i - 1].number = Some(*count);
                    }
                    self.sections[i - 1].split_letter = Some('a');
                    consecutive_count = 2;
                }

                // Assign split letter
                let letter = (b'a' + consecutive_count as u8 - 1) as char;
                self.sections[i].split_letter = Some(letter);
                if should_number {
                    self.sections[i].number = current_group_number;
                }
            } else {
                // Not consecutive
                consecutive_count = 0;
                current_group_number = None;

                if should_number {
                    let count = counters.entry(stype.clone()).or_insert(0);
                    *count += 1;
                    self.sections[i].number = Some(*count);
                }
            }

            prev_type = Some(stype);
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

    /// Validate song data
    pub fn validate(&self) -> Result<(), SetlistError> {
        if self.name.trim().is_empty() {
            return Err(SetlistError::invalid_song("Song name cannot be empty"));
        }

        // Validate that marker positions make sense
        if let (Some(start), Some(end)) = (self.start_position, self.end_position) {
            let start_seconds = start.time.to_seconds();
            let end_seconds = end.time.to_seconds();
            if start_seconds >= end_seconds {
                return Err(SetlistError::invalid_song(
                    format!("Song start position ({}) must be before end position ({})", start_seconds, end_seconds)
                ));
            }
        }

        if let (Some(start), Some(song_end)) = (self.start_position, self.song_end_position) {
            let start_seconds = start.time.to_seconds();
            let song_end_seconds = song_end.time.to_seconds();
            if start_seconds >= song_end_seconds {
                return Err(SetlistError::invalid_song(
                    format!("Song start position ({}) must be before song end position ({})", start_seconds, song_end_seconds)
                ));
            }
        }

        if let (Some(song_end), Some(end)) = (self.song_end_position, self.end_position) {
            let song_end_seconds = song_end.time.to_seconds();
            let end_seconds = end.time.to_seconds();
            if song_end_seconds > end_seconds {
                return Err(SetlistError::invalid_song(
                    format!("Song end position ({}) must be before or equal to end position ({})", song_end_seconds, end_seconds)
                ));
            }
        }

        // Validate count-in position
        if let (Some(count_in), Some(start)) = (self.count_in_position, self.start_position) {
            let count_in_seconds = count_in.time.to_seconds();
            let start_seconds = start.time.to_seconds();
            if count_in_seconds >= start_seconds {
                return Err(SetlistError::invalid_song(
                    format!("Count-in position ({}) must be before start position ({})", count_in_seconds, start_seconds)
                ));
            }
        }

        // Validate all sections
        for (i, section) in self.sections.iter().enumerate() {
            section.validate().map_err(|e| {
                SetlistError::invalid_song(format!("Section {} is invalid: {}", i, e))
            })?;
        }

        // Check for overlapping sections
        for i in 0..self.sections.len() {
            for j in (i + 1)..self.sections.len() {
                if self.sections[i].overlaps_with_section(&self.sections[j]) {
                    return Err(SetlistError::invalid_song(
                        format!("Sections '{}' and '{}' overlap",
                            self.sections[i].name, self.sections[j].name)
                    ));
                }
            }
        }

        Ok(())
    }

    /// Get a summary of the song structure
    pub fn summary(&self) -> SongSummary {
        let section_counts = self.sections
            .iter()
            .fold(HashMap::new(), |mut acc, section| {
                *acc.entry(section.section_type.clone()).or_insert(0) += 1;
                acc
            });

        SongSummary {
            name: self.name.clone(),
            duration: self.duration(),
            render_duration: self.render_duration(),
            section_count: self.sections.len(),
            section_types: section_counts,
            has_count_in: self.count_in_position.is_some(),
            has_markers: self.start_position.is_some() || self.song_end_position.is_some() || self.end_position.is_some(),
        }
    }

    /// Clear all sections
    pub fn clear_sections(&mut self) {
        self.sections.clear();
    }

    /// Replace all sections
    pub fn set_sections(&mut self, sections: Vec<Section>) -> Result<(), SetlistError> {
        // Validate all sections first
        for section in &sections {
            section.validate()?;
        }

        // Check for overlaps
        for i in 0..sections.len() {
            for j in (i + 1)..sections.len() {
                if sections[i].overlaps_with_section(&sections[j]) {
                    return Err(SetlistError::invalid_song(
                        format!("Sections '{}' and '{}' overlap",
                            sections[i].name, sections[j].name)
                    ));
                }
            }
        }

        self.sections = sections;
        Ok(())
    }
}

impl std::fmt::Display for Song {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} ({:.1}s, {} sections)",
            self.name,
            self.duration(),
            self.sections.len()
        )
    }
}

/// Summary information about a song
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct SongSummary {
    pub name: String,
    pub duration: f64,
    pub render_duration: f64,
    pub section_count: usize,
    pub section_types: HashMap<SectionType, u32>,
    pub has_count_in: bool,
    pub has_markers: bool,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::Section;

    #[test]
    fn test_song_creation() {
        let song = Song::new("Test Song".to_string()).unwrap();
        assert_eq!(song.name, "Test Song");
        assert_eq!(song.sections.len(), 0);
        assert_eq!(song.duration(), 0.0);

        // Test empty name validation
        let empty_song = Song::new("".to_string());
        assert!(empty_song.is_err());
    }

    #[test]
    fn test_song_positions() {
        let mut song = Song::new("Test Song".to_string()).unwrap();
        song.start_position = Some(Position::from_seconds(10.0));
        song.song_end_position = Some(Position::from_seconds(180.0));
        song.end_position = Some(Position::from_seconds(200.0));
        song.song_region_start = Some(Position::from_seconds(5.0));
        song.song_region_end = Some(Position::from_seconds(220.0));

        assert_eq!(song.render_start(), 10.0); // Uses SONGSTART
        assert_eq!(song.render_end(), 180.0); // Uses SONGEND
        assert_eq!(song.effective_start(), 10.0); // Uses SONGSTART
        assert_eq!(song.effective_end(), 200.0); // Uses =END
        assert_eq!(song.hard_cut(), 200.0); // Uses =END
        assert_eq!(song.duration(), 190.0); // =END - SONGSTART
    }

    #[test]
    fn test_song_sections() {
        let mut song = Song::new("Test Song".to_string()).unwrap();

        let verse = Section::from_seconds(
            SectionType::Verse,
            10.0,
            40.0,
            "Verse 1".to_string(),
            Some(1),
        ).unwrap();

        let chorus = Section::from_seconds(
            SectionType::Chorus,
            50.0,
            80.0,
            "Chorus 1".to_string(),
            Some(1),
        ).unwrap();

        song.add_section(verse).unwrap();
        song.add_section(chorus).unwrap();

        assert_eq!(song.sections.len(), 2);
        assert!(song.section_at_position(25.0).is_some());
        assert!(song.section_at_position(45.0).is_none()); // Between sections

        let verse_sections = song.sections_by_type(&SectionType::Verse);
        assert_eq!(verse_sections.len(), 1);
    }

    #[test]
    fn test_section_overlaps() {
        let mut song = Song::new("Test Song".to_string()).unwrap();

        let section1 = Section::from_seconds(
            SectionType::Verse,
            10.0,
            30.0,
            "Verse 1".to_string(),
            Some(1),
        ).unwrap();

        let section2 = Section::from_seconds(
            SectionType::Chorus,
            25.0,
            45.0,
            "Chorus 1".to_string(),
            Some(1),
        ).unwrap();

        song.add_section(section1).unwrap();

        // Should fail due to overlap
        let result = song.add_section(section2);
        assert!(result.is_err());
    }

    #[test]
    fn test_auto_numbering() {
        let mut song = Song::new("Test Song".to_string()).unwrap();

        // Add sections out of order
        song.sections = vec![
            Section::from_seconds(SectionType::Intro, 0.0, 10.0, "Intro".to_string(), None).unwrap(),
            Section::from_seconds(SectionType::Verse, 50.0, 80.0, "Verse 2".to_string(), None).unwrap(),
            Section::from_seconds(SectionType::Chorus, 80.0, 110.0, "Chorus 1".to_string(), None).unwrap(),
            Section::from_seconds(SectionType::Chorus, 110.0, 140.0, "Chorus 2".to_string(), None).unwrap(),
            Section::from_seconds(SectionType::Verse, 10.0, 50.0, "Verse 1".to_string(), None).unwrap(),
            Section::from_seconds(SectionType::Outro, 140.0, 160.0, "Outro".to_string(), None).unwrap(),
        ];

        song.auto_number_sections();

        // Check that sections are sorted by time
        let ordered = song.sections_in_order();
        assert_eq!(ordered[0].section_type, SectionType::Intro);
        assert_eq!(ordered[1].section_type, SectionType::Verse);
        assert_eq!(ordered[2].section_type, SectionType::Verse);
        assert_eq!(ordered[3].section_type, SectionType::Chorus);
        assert_eq!(ordered[4].section_type, SectionType::Chorus);
        assert_eq!(ordered[5].section_type, SectionType::Outro);

        // Check numbering
        assert_eq!(song.sections[0].number, None); // Intro not numbered
        assert_eq!(song.sections[1].number, Some(1)); // First verse
        assert_eq!(song.sections[2].number, Some(1)); // Consecutive chorus
        assert_eq!(song.sections[3].number, Some(1)); // Consecutive chorus
        assert_eq!(song.sections[4].number, Some(2)); // Second verse
        assert_eq!(song.sections[5].number, None); // Outro not numbered

        // Check split letters for consecutive choruses
        assert_eq!(song.sections[2].split_letter, Some('a'));
        assert_eq!(song.sections[3].split_letter, Some('b'));
    }

    #[test]
    fn test_song_validation() {
        let mut song = Song::new("Test Song".to_string()).unwrap();

        // Valid song with proper marker positions
        song.start_position = Some(Position::from_seconds(10.0));
        song.song_end_position = Some(Position::from_seconds(100.0));
        song.end_position = Some(Position::from_seconds(110.0));

        assert!(song.validate().is_ok());

        // Invalid: start after end
        song.start_position = Some(Position::from_seconds(100.0));
        song.end_position = Some(Position::from_seconds(50.0));
        assert!(song.validate().is_err());
    }

    #[test]
    fn test_metadata() {
        let mut song = Song::new("Test Song".to_string()).unwrap();

        song.set_metadata("key", "C major");
        song.set_metadata("tempo", "120");
        song.set_metadata("time_signature", "4/4");

        assert_eq!(song.get_metadata("key"), Some(&"C major".to_string()));
        assert_eq!(song.get_metadata("tempo"), Some(&"120".to_string()));
        assert_eq!(song.get_metadata("nonexistent"), None);

        let removed = song.remove_metadata("key");
        assert_eq!(removed, Some("C major".to_string()));
        assert_eq!(song.get_metadata("key"), None);
    }

    #[test]
    fn test_song_summary() {
        let mut song = Song::new("Test Song".to_string()).unwrap();
        song.start_position = Some(Position::from_seconds(0.0));
        song.end_position = Some(Position::from_seconds(180.0));
        song.count_in_position = Some(Position::from_seconds(-4.0));

        let verse = Section::from_seconds(SectionType::Verse, 0.0, 60.0, "Verse".to_string(), None).unwrap();
        let chorus = Section::from_seconds(SectionType::Chorus, 60.0, 120.0, "Chorus".to_string(), None).unwrap();

        song.add_section(verse).unwrap();
        song.add_section(chorus).unwrap();

        let summary = song.summary();
        assert_eq!(summary.name, "Test Song");
        assert_eq!(summary.duration, 180.0);
        assert_eq!(summary.section_count, 2);
        assert_eq!(summary.section_types.get(&SectionType::Verse), Some(&1));
        assert_eq!(summary.section_types.get(&SectionType::Chorus), Some(&1));
        assert!(summary.has_count_in);
        assert!(summary.has_markers);
    }
}
