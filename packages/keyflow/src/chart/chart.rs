//! Main Chart Structure
//!
//! The complete parsed chart with all sections and metadata

use super::memory::ChordMemory;
use super::settings::ChartSettings;
use super::templates::TemplateManager;
use super::types::{ChartSection, ChordInstance, KeyChange, TimeSignatureChange};
use crate::key::Key;
use crate::metadata::SongMetadata;
use crate::primitives::Note;
use crate::sections::Section;
use crate::time::{AbsolutePosition, MusicalDuration, Tempo, TimeSignature};

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
    pub fn add_key_change(
        &mut self,
        new_key: Key,
        position: AbsolutePosition,
        section_index: usize,
    ) {
        let from_key = self.current_key.clone();
        let key_change = KeyChange::new(position, from_key, new_key.clone(), section_index);

        self.key_changes.push(key_change);
        self.current_key = Some(new_key);
    }

    /// Register a time signature change at the current position
    pub fn add_time_signature_change(
        &mut self,
        new_time_sig: TimeSignature,
        position: AbsolutePosition,
        section_index: usize,
    ) {
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
    pub fn time_signature_at_position(
        &self,
        position: &AbsolutePosition,
    ) -> Option<&TimeSignature> {
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
    pub fn calculate_position(
        &self,
        section_index: usize,
        measure_index: usize,
    ) -> AbsolutePosition {
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

    /// Convert the chart back to text syntax
    /// 
    /// This produces a valid syntax string that, when parsed, will result in the same chart structure.
    /// The output may not match the original formatting exactly, but it will be functionally equivalent.
    pub fn to_syntax(&self) -> String {
        let mut output = String::new();

        // 1. Title and Artist
        if let Some(title) = &self.metadata.title {
            output.push_str(title);
            if let Some(artist) = &self.metadata.artist {
                output.push_str(" - ");
                output.push_str(artist);
            }
            output.push('\n');
        }

        // 2. Tempo, Time Signature, Key
        let mut metadata_parts = Vec::new();
        
        if let Some(tempo) = &self.tempo {
            metadata_parts.push(format!("{}bpm", tempo.bpm));
        }
        
        // Use initial_time_signature if available, otherwise fall back to current time_signature
        let ts = self.initial_time_signature.as_ref().or(self.time_signature.as_ref());
        if let Some(ts) = ts {
            metadata_parts.push(format!("{}/{}", ts.numerator, ts.denominator));
        }
        
        if let Some(key) = &self.initial_key {
            // Format key as #G, bBb, or #C (always use # prefix for major keys)
            let key_str = self.format_key_for_syntax(key);
            metadata_parts.push(key_str);
        }
        
        if !metadata_parts.is_empty() {
            output.push_str(&metadata_parts.join(" "));
            output.push('\n');
        }

        // 3. Settings
        if self.settings.smart_repeats() {
            output.push_str("/SMART_REPEATS=true\n");
        }

        // 4. Sections
        for (section_idx, section) in self.sections.iter().enumerate() {
            // Empty line before section (except first)
            if section_idx > 0 {
                output.push('\n');
            }

            // Section header
            let section_name = self.format_section_name(&section.section);
            output.push_str(&section_name);
            output.push('\n');

            // Process measures
            let mut measure_idx = 0;
            while measure_idx < section.measures.len() {
                let measure = &section.measures[measure_idx];
                
                // Check for repeats
                let repeat_count = measure.repeat_count;

                // Text cues before this measure
                for cue in &measure.text_cues {
                    output.push_str(&format!("{}\n", cue));
                }

                // Check for key change at this position
                let position = self.calculate_position(section_idx, measure_idx);
                for key_change in &self.key_changes {
                    if key_change.position.total_duration.measures == position.total_duration.measures
                        && key_change.section_index == section_idx {
                        let key_str = self.format_key_for_syntax(&key_change.to_key);
                        output.push_str(&key_str);
                        output.push(' ');
                    }
                }

                // Check for time signature change at this position
                for ts_change in &self.time_signature_changes {
                    if ts_change.position.total_duration.measures == position.total_duration.measures
                        && ts_change.section_index == section_idx {
                        output.push_str(&format!("{}/{} ", ts_change.time_signature.numerator, ts_change.time_signature.denominator));
                    }
                }

                // Chords in this measure
                let mut chord_parts = Vec::new();
                for chord in &measure.chords {
                    let chord_str = self.format_chord_for_syntax(chord, &measure.time_signature);
                    if !chord_str.is_empty() {
                        chord_parts.push(chord_str);
                    }
                }

                if !chord_parts.is_empty() {
                    output.push_str(&chord_parts.join(" "));
                    
                    // Add measure separator at end of measure (except last in section)
                    // Only add if there are multiple measures or if explicitly needed
                    if measure_idx < section.measures.len() - 1 {
                        output.push_str(" |");
                    }
                    
                    // Add repeat count if > 1
                    if repeat_count > 1 {
                        // Check if this is a smart repeat (would need to check section length)
                        // For now, just output xN
                        output.push_str(&format!(" x{}", repeat_count));
                    }
                } else {
                    // Empty measure - just add separator if not last
                    if measure_idx < section.measures.len() - 1 {
                        output.push_str("|");
                    }
                }

                output.push('\n');
                measure_idx += 1;
            }

        }

        output
    }

    /// Format a key for syntax output (#G, bBb, or #C)
    /// Always use # prefix for major keys to match parser expectations
    fn format_key_for_syntax(&self, key: &Key) -> String {
        let root = key.root();
        let note_name = root.name();
        
        // Remove any existing # or b from note name
        let clean_note = note_name.trim_start_matches('#').trim_start_matches('b');
        
        // For major keys (Ionian), use # prefix
        // For minor keys, we'd use b prefix, but for now all keys are major
        if note_name.contains('b') {
            format!("b{}", clean_note)
        } else {
            // Use # prefix for all major keys (matches parser expectation)
            format!("#{}", clean_note)
        }
    }

    /// Format a section name for syntax output
    fn format_section_name(&self, section: &Section) -> String {
        let mut name = String::new();
        
        // Subsection prefix
        if section.is_subsection {
            name.push('^');
        }
        
        // Section type
        match &section.section_type {
            crate::sections::SectionType::Custom(custom_name) => {
                name.push('[');
                name.push_str(custom_name);
                name.push(']');
            }
            _ => {
                name.push_str(&section.section_type.abbreviation());
            }
        }
        
        // Number (only if section type should be numbered)
        if section.section_type.should_number() {
            if let Some(num) = section.number {
                name.push(' ');
                name.push_str(&num.to_string());
            }
        }
        
        // Split letter
        if let Some(letter) = section.split_letter {
            name.push(letter);
        }
        
        // Measure count
        if let Some(count) = section.measure_count {
            name.push(' ');
            name.push_str(&count.to_string());
        }
        
        name
    }

    /// Format a chord for syntax output
    fn format_chord_for_syntax(&self, chord: &ChordInstance, _time_sig: &(u8, u8)) -> String {
        let mut output = String::new();
        
        // Push notation (leading apostrophes)
        if let Some((is_push, amount)) = &chord.push_pull {
            if *is_push {
                let apostrophes = match amount {
                    crate::chord::PushPullAmount::Eighth => "'",
                    crate::chord::PushPullAmount::Sixteenth => "''",
                    crate::chord::PushPullAmount::ThirtySecond => "'''",
                };
                output.push_str(apostrophes);
            }
        }
        
        // Chord symbol (use full_symbol which preserves the original format)
        output.push_str(&chord.full_symbol);
        
        // Rhythm notation
        output.push_str(&self.format_rhythm_for_syntax(&chord.rhythm));
        
        // Pull notation (trailing apostrophes)
        if let Some((is_push, amount)) = &chord.push_pull {
            if !is_push {
                let apostrophes = match amount {
                    crate::chord::PushPullAmount::Eighth => "'",
                    crate::chord::PushPullAmount::Sixteenth => "''",
                    crate::chord::PushPullAmount::ThirtySecond => "'''",
                };
                output.push_str(apostrophes);
            }
        }
        
        // Commands
        for command in &chord.commands {
            match command {
                super::commands::Command::Fermata => {
                    output.push_str(" /fermata");
                }
                super::commands::Command::Accent => {
                    output.push_str("->");
                }
            }
        }
        
        output
    }

    /// Format rhythm notation for syntax output
    fn format_rhythm_for_syntax(&self, rhythm: &crate::chord::ChordRhythm) -> String {
        use crate::chord::ChordRhythm;
        match rhythm {
            ChordRhythm::Default => String::new(),
            ChordRhythm::Slashes(count) => {
                "/".repeat(*count as usize)
            }
            ChordRhythm::Lily { duration, dotted, multiplier, tied } => {
                let mut output = format!("_{}", duration.value());
                if *dotted {
                    output.push('.');
                }
                if let Some(mult) = multiplier {
                    output.push_str(&format!("*{}", mult));
                }
                if *tied {
                    output.push('~');
                }
                output
            }
            ChordRhythm::Rest { duration, dotted, multiplier } => {
                let mut output = format!("r{}", duration.value());
                if *dotted {
                    output.push('.');
                }
                if let Some(mult) = multiplier {
                    output.push_str(&format!("*{}", mult));
                }
                output
            }
            ChordRhythm::Space { duration, dotted, multiplier } => {
                let mut output = format!("s{}", duration.value());
                if *dotted {
                    output.push('.');
                }
                if let Some(mult) = multiplier {
                    output.push_str(&format!("*{}", mult));
                }
                output
            }
            ChordRhythm::Push(_) | ChordRhythm::Pull(_) => {
                // Push/pull is handled separately in format_chord_for_syntax
                String::new()
            }
        }
    }
}

impl Default for Chart {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::super::types::{ChartSection, Measure};
    use super::*;
    use crate::primitives::MusicalNote;
    use crate::sections::{Section, SectionType};

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
        chart
            .sections
            .push(ChartSection::new(intro).with_measures(intro_measures));

        // Section 1: Verse - 4 measures, key changes to G major after 2 measures
        let verse = Section::new(SectionType::Verse).with_measure_count(4);
        let verse_measures = vec![
            Measure::new().with_time_signature((4, 4)), // Measure 0 in verse
            Measure::new().with_time_signature((4, 4)), // Measure 1 in verse
            Measure::new().with_time_signature((4, 4)), // Measure 2 (key change to G here)
            Measure::new().with_time_signature((4, 4)), // Measure 3
        ];
        chart
            .sections
            .push(ChartSection::new(verse).with_measures(verse_measures));

        // Add key change at measure 4 (start of measure 2 in verse = section 1, measure 2)
        let g_major = Key::major(MusicalNote::g());
        let key_change_position = AbsolutePosition::new(
            MusicalDuration::new(4, 0, 0), // After 4 measures total
            1,                             // Section 1 (verse)
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
        chart
            .sections
            .push(ChartSection::new(chorus).with_measures(chorus_measures));

        // Add time signature change at measure 6 (start of chorus = section 2, measure 0)
        let time_6_8 = TimeSignature::new(6, 8);
        let ts_change_position = AbsolutePosition::new(
            MusicalDuration::new(6, 0, 0), // After 6 measures total
            2,                             // Section 2 (chorus)
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

        let key_before_change =
            chart.key_at_position(&AbsolutePosition::new(MusicalDuration::new(3, 0, 0), 1));
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
        chart
            .sections
            .push(ChartSection::new(section1).with_measures(measures1));

        let section2 = Section::new(SectionType::Chorus);
        let measures2 = vec![Measure::new(), Measure::new()];
        chart
            .sections
            .push(ChartSection::new(section2).with_measures(measures2));

        let section3 = Section::new(SectionType::Bridge);
        let measures3 = vec![Measure::new(), Measure::new()];
        chart
            .sections
            .push(ChartSection::new(section3).with_measures(measures3));

        // Key change at measure 2 (start of section 1)
        chart.add_key_change(
            g_major.clone(),
            AbsolutePosition::new(MusicalDuration::new(2, 0, 0), 1),
            1,
        );

        // Key change at measure 4 (start of section 2)
        chart.add_key_change(
            d_major.clone(),
            AbsolutePosition::new(MusicalDuration::new(4, 0, 0), 2),
            2,
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
        chart
            .sections
            .push(ChartSection::new(section).with_measures(vec![Measure::new()]));

        // Add key change
        chart.add_key_change(
            f_major.clone(),
            AbsolutePosition::new(MusicalDuration::new(1, 0, 0), 0),
            0,
        );

        // Verify current_key was updated
        assert_eq!(chart.current_key, Some(f_major));
    }

    #[test]
    fn test_round_trip_serialization() {
        let input = r#"
My Song - Test Artist
120bpm 4/4 #C

Intro 4
C G Am F

VS 8
C G Am F x2

CH 8
F C G Am
"#;

        // Parse the chart
        let chart1 = Chart::parse(input).expect("Should parse successfully");
        
        // Serialize it
        let output = chart1.to_syntax();
        println!("Serialized output:\n{}", output);
        
        // Parse it again
        let chart2 = Chart::parse(&output).expect("Should parse serialized output");
        
        // Verify they have the same structure
        assert_eq!(chart1.metadata.title, chart2.metadata.title);
        assert_eq!(chart1.metadata.artist, chart2.metadata.artist);
        assert_eq!(chart1.tempo, chart2.tempo);
        assert_eq!(chart1.initial_key, chart2.initial_key);
        assert_eq!(chart1.sections.len(), chart2.sections.len());
        
        // Verify sections have same measure counts
        for (s1, s2) in chart1.sections.iter().zip(chart2.sections.iter()) {
            assert_eq!(s1.measures.len(), s2.measures.len());
        }
    }
}
