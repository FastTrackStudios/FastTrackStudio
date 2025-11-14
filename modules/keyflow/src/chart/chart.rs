//! Main Chart Structure
//!
//! The complete parsed chart with all sections and metadata

use super::types::{ChartSection, KeyChange, TimeSignatureChange};
use super::memory::ChordMemory;
use super::templates::TemplateManager;
use super::settings::ChartSettings;
use crate::metadata::SongMetadata;
use crate::key::Key;
use crate::time::{Tempo, TimeSignature, MusicalDuration, AbsolutePosition};

/// The complete parsed chart structure
#[derive(Debug, Clone)]
pub struct Chart {
    /// Song metadata (title, artist, etc.)
    pub metadata: SongMetadata,
    
    /// All sections in the chart
    pub sections: Vec<ChartSection>,
    
    /// Current key (last known key during parsing)
    pub current_key: Option<Key>,
    
    /// Initial key at the start of the song
    pub initial_key: Option<Key>,
    
    /// Ending key at the end of the song
    pub ending_key: Option<Key>,
    
    /// All key changes throughout the song
    pub key_changes: Vec<KeyChange>,
    
    /// Tempo in BPM
    pub tempo: Option<Tempo>,
    
    /// Current time signature (last known during parsing)
    pub time_signature: Option<TimeSignature>,
    
    /// Initial time signature at the start
    pub initial_time_signature: Option<TimeSignature>,
    
    /// All time signature changes throughout the song
    pub time_signature_changes: Vec<TimeSignatureChange>,
    
    /// Chord memory manager (internal)
    pub(crate) chord_memory: ChordMemory,
    
    /// Template manager (internal)
    pub(crate) templates: TemplateManager,
    
    /// Chart configuration settings
    pub settings: ChartSettings,
}

impl Chart {
    /// Create a new empty chart
    pub fn new() -> Self {
        Self {
            metadata: SongMetadata::new(),
            sections: Vec::new(),
            current_key: None,
            initial_key: None,
            ending_key: None,
            key_changes: Vec::new(),
            tempo: None,
            time_signature: None,
            initial_time_signature: None,
            time_signature_changes: Vec::new(),
            chord_memory: ChordMemory::new(),
            templates: TemplateManager::new(),
            settings: ChartSettings::new(),
        }
    }
    
    /// Register a key change at the current position
    pub fn add_key_change(&mut self, new_key: Key, position: AbsolutePosition, section_index: usize) {
        let from_key = self.current_key.clone();
        let key_change = KeyChange::new(position, from_key, new_key.clone(), section_index);
        
        self.key_changes.push(key_change);
        self.current_key = Some(new_key);
    }
    
    /// Register a time signature change at the current position
    pub fn add_time_signature_change(&mut self, new_time_sig: TimeSignature, position: AbsolutePosition, section_index: usize) {
        let change = TimeSignatureChange::new(position, new_time_sig, section_index);
        
        self.time_signature_changes.push(change);
        self.time_signature = Some(new_time_sig);
    }
    
    /// Get the active key at a specific position
    pub fn key_at_position(&self, position: &AbsolutePosition) -> Option<&Key> {
        // Start with initial key
        let mut current_key = self.initial_key.as_ref();
        
        // Apply key changes up to this position
        for change in &self.key_changes {
            if change.position.total_duration.measures <= position.total_duration.measures {
                current_key = Some(&change.to_key);
            } else {
                break;
            }
        }
        
        current_key
    }
    
    /// Get the active time signature at a specific position
    /// 
    /// Time signature changes take effect at the START of the measure they're marked at.
    /// So if checking position M.X.Y where M equals the change position measure,
    /// the change has already taken effect.
    pub fn time_signature_at_position(&self, position: &AbsolutePosition) -> Option<&TimeSignature> {
        // Start with initial time signature
        let mut current_ts = self.initial_time_signature.as_ref();
        
        // Apply time signature changes up to (and including) this position
        for change in &self.time_signature_changes {
            if change.position.total_duration.measures <= position.total_duration.measures {
                current_ts = Some(&change.time_signature);
            } else {
                break;
            }
        }
        
        current_ts
    }
    
    /// Calculate duration from start of song to a given section and measure
    pub fn calculate_position(&self, section_index: usize, measure_index: usize) -> AbsolutePosition {
        let mut total_duration = MusicalDuration::new(0, 0, 0);
        
        // Sum durations of all sections before this one
        for (idx, section) in self.sections.iter().enumerate() {
            if idx >= section_index {
                break;
            }
            
            // Sum all measures in this section (each measure is always 1.0.0)
            for _measure in &section.measures {
                // Each measure adds exactly 1.0.0 to the total duration
                total_duration = MusicalDuration::new(
                    total_duration.measures + 1,
                    total_duration.beats,
                    total_duration.subdivisions,
                );
            }
        }
        
        // Add measures in current section up to measure_index (each is 1.0.0)
        if let Some(section) = self.sections.get(section_index) {
            for _idx in 0..measure_index.min(section.measures.len()) {
                // Each measure adds exactly 1.0.0 to the total duration
                total_duration = MusicalDuration::new(
                    total_duration.measures + 1,
                    total_duration.beats,
                    total_duration.subdivisions,
                );
            }
        }
        
        AbsolutePosition::new(total_duration, section_index)
    }
}

impl Default for Chart {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::types::{Measure, ChartSection};
    use crate::sections::{Section, SectionType};
    use crate::primitives::MusicalNote;
    
    #[test]
    fn test_position_calculation_with_key_and_time_signature_changes() {
        let mut chart = Chart::new();
        
        // Setup: C major, 4/4 time
        let c_major = Key::major(MusicalNote::c());
        let time_4_4 = TimeSignature::new(4, 4);
        
        chart.initial_key = Some(c_major.clone());
        chart.current_key = Some(c_major.clone());
        chart.initial_time_signature = Some(time_4_4);
        chart.time_signature = Some(time_4_4);
        
        // Section 0: Intro - 2 measures in C major, 4/4
        let intro = Section::new(SectionType::Intro).with_measure_count(2);
        let intro_measures = vec![
            Measure::new().with_time_signature((4, 4)),
            Measure::new().with_time_signature((4, 4)),
        ];
        chart.sections.push(ChartSection::new(intro).with_measures(intro_measures));
        
        // Section 1: Verse - 4 measures, key changes to G major after 2 measures
        let verse = Section::new(SectionType::Verse).with_measure_count(4);
        let verse_measures = vec![
            Measure::new().with_time_signature((4, 4)), // Measure 0 in verse
            Measure::new().with_time_signature((4, 4)), // Measure 1 in verse
            Measure::new().with_time_signature((4, 4)), // Measure 2 (key change to G here)
            Measure::new().with_time_signature((4, 4)), // Measure 3
        ];
        chart.sections.push(ChartSection::new(verse).with_measures(verse_measures));
        
        // Add key change at measure 4 (start of measure 2 in verse = section 1, measure 2)
        let g_major = Key::major(MusicalNote::g());
        let key_change_position = AbsolutePosition::new(
            MusicalDuration::new(4, 0, 0), // After 4 measures total
            1 // Section 1 (verse)
        );
        chart.add_key_change(g_major.clone(), key_change_position.clone(), 1);
        
        // Section 2: Chorus - 4 measures in 6/8 time
        let chorus = Section::new(SectionType::Chorus).with_measure_count(4);
        let chorus_measures = vec![
            Measure::new().with_time_signature((6, 8)),
            Measure::new().with_time_signature((6, 8)),
            Measure::new().with_time_signature((6, 8)),
            Measure::new().with_time_signature((6, 8)),
        ];
        chart.sections.push(ChartSection::new(chorus).with_measures(chorus_measures));
        
        // Add time signature change at measure 6 (start of chorus = section 2, measure 0)
        let time_6_8 = TimeSignature::new(6, 8);
        let ts_change_position = AbsolutePosition::new(
            MusicalDuration::new(6, 0, 0), // After 6 measures total
            2 // Section 2 (chorus)
        );
        chart.add_time_signature_change(time_6_8, ts_change_position.clone(), 2);
        
        // Test 1: Position at start of song (section 0, measure 0)
        let pos_start = chart.calculate_position(0, 0);
        assert_eq!(pos_start.total_duration.measures, 0);
        assert_eq!(pos_start.section_index, 0);
        
        // Test 2: Position at end of intro (section 0, after 2 measures)
        let pos_end_intro = chart.calculate_position(0, 2);
        assert_eq!(pos_end_intro.total_duration.measures, 2);
        assert_eq!(pos_end_intro.section_index, 0);
        
        // Test 3: Position at start of verse (section 1, measure 0)
        let pos_start_verse = chart.calculate_position(1, 0);
        assert_eq!(pos_start_verse.total_duration.measures, 2);
        assert_eq!(pos_start_verse.section_index, 1);
        
        // Test 4: Position where key changes (section 1, measure 2)
        let pos_key_change = chart.calculate_position(1, 2);
        assert_eq!(pos_key_change.total_duration.measures, 4);
        assert_eq!(pos_key_change.section_index, 1);
        
        // Test 5: Position at end of verse (section 1, after 4 measures)
        let pos_end_verse = chart.calculate_position(1, 4);
        assert_eq!(pos_end_verse.total_duration.measures, 6);
        assert_eq!(pos_end_verse.section_index, 1);
        
        // Test 6: Position at start of chorus / time sig change (section 2, measure 0)
        let pos_start_chorus = chart.calculate_position(2, 0);
        assert_eq!(pos_start_chorus.total_duration.measures, 6);
        assert_eq!(pos_start_chorus.section_index, 2);
        
        // Test 7: Position in middle of chorus (section 2, measure 2)
        let pos_mid_chorus = chart.calculate_position(2, 2);
        assert_eq!(pos_mid_chorus.total_duration.measures, 8);
        assert_eq!(pos_mid_chorus.section_index, 2);
        
        // Test 8: Verify key_at_position works correctly
        let key_at_start = chart.key_at_position(&pos_start);
        assert_eq!(key_at_start, Some(&c_major));
        
        let key_before_change = chart.key_at_position(&AbsolutePosition::new(MusicalDuration::new(3, 0, 0), 1));
        assert_eq!(key_before_change, Some(&c_major));
        
        let key_after_change = chart.key_at_position(&pos_key_change);
        assert_eq!(key_after_change, Some(&g_major));
        
        let key_in_chorus = chart.key_at_position(&pos_mid_chorus);
        assert_eq!(key_in_chorus, Some(&g_major));
        
        // Test 9: Verify time_signature_at_position works correctly
        let ts_at_start = chart.time_signature_at_position(&pos_start);
        assert_eq!(ts_at_start, Some(&time_4_4));
        
        // Position 6 is exactly where chorus starts - the change happens AT this position
        // So at position 6, we should already be in 6/8
        let ts_at_change_point = chart.time_signature_at_position(&pos_start_chorus);
        assert_eq!(ts_at_change_point, Some(&time_6_8));
        
        // Just before the change (end of verse at position 6) - still in 4/4
        // Note: pos_end_verse is at 6.0.0, which is the same as pos_start_chorus!
        // So we need a position just before that
        let pos_before_change = AbsolutePosition::new(MusicalDuration::new(5, 0, 0), 1);
        let ts_before_change = chart.time_signature_at_position(&pos_before_change);
        assert_eq!(ts_before_change, Some(&time_4_4));
        
        let ts_in_chorus = chart.time_signature_at_position(&pos_mid_chorus);
        assert_eq!(ts_in_chorus, Some(&time_6_8));
    }
    
    #[test]
    fn test_multiple_key_changes() {
        let mut chart = Chart::new();
        
        let c_major = Key::major(MusicalNote::c());
        let g_major = Key::major(MusicalNote::g());
        let d_major = Key::major(MusicalNote::d());
        
        chart.initial_key = Some(c_major.clone());
        chart.current_key = Some(c_major.clone());
        
        // Add sections with measures
        let section1 = Section::new(SectionType::Verse);
        let measures1 = vec![Measure::new(), Measure::new()];
        chart.sections.push(ChartSection::new(section1).with_measures(measures1));
        
        let section2 = Section::new(SectionType::Chorus);
        let measures2 = vec![Measure::new(), Measure::new()];
        chart.sections.push(ChartSection::new(section2).with_measures(measures2));
        
        let section3 = Section::new(SectionType::Bridge);
        let measures3 = vec![Measure::new(), Measure::new()];
        chart.sections.push(ChartSection::new(section3).with_measures(measures3));
        
        // Key change at measure 2 (start of section 1)
        chart.add_key_change(
            g_major.clone(),
            AbsolutePosition::new(MusicalDuration::new(2, 0, 0), 1),
            1
        );
        
        // Key change at measure 4 (start of section 2)
        chart.add_key_change(
            d_major.clone(),
            AbsolutePosition::new(MusicalDuration::new(4, 0, 0), 2),
            2
        );
        
        // Test that we have 2 key changes recorded
        assert_eq!(chart.key_changes.len(), 2);
        
        // Test key at different positions
        let pos_0 = AbsolutePosition::new(MusicalDuration::new(0, 0, 0), 0);
        assert_eq!(chart.key_at_position(&pos_0), Some(&c_major));
        
        let pos_1 = AbsolutePosition::new(MusicalDuration::new(1, 0, 0), 0);
        assert_eq!(chart.key_at_position(&pos_1), Some(&c_major));
        
        let pos_2 = AbsolutePosition::new(MusicalDuration::new(2, 0, 0), 1);
        assert_eq!(chart.key_at_position(&pos_2), Some(&g_major));
        
        let pos_3 = AbsolutePosition::new(MusicalDuration::new(3, 0, 0), 1);
        assert_eq!(chart.key_at_position(&pos_3), Some(&g_major));
        
        let pos_4 = AbsolutePosition::new(MusicalDuration::new(4, 0, 0), 2);
        assert_eq!(chart.key_at_position(&pos_4), Some(&d_major));
        
        let pos_5 = AbsolutePosition::new(MusicalDuration::new(5, 0, 0), 2);
        assert_eq!(chart.key_at_position(&pos_5), Some(&d_major));
    }
    
    #[test]
    fn test_ending_key_tracking() {
        let mut chart = Chart::new();
        
        let c_major = Key::major(MusicalNote::c());
        let f_major = Key::major(MusicalNote::f());
        
        chart.initial_key = Some(c_major.clone());
        chart.current_key = Some(c_major.clone());
        
        // Add a section
        let section = Section::new(SectionType::Verse);
        chart.sections.push(ChartSection::new(section).with_measures(vec![Measure::new()]));
        
        // Add key change
        chart.add_key_change(
            f_major.clone(),
            AbsolutePosition::new(MusicalDuration::new(1, 0, 0), 0),
            0
        );
        
        // Verify current_key was updated
        assert_eq!(chart.current_key, Some(f_major));
    }
}
