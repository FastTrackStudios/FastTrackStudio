use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Represents different types of song sections with their full names and abbreviations
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum SectionType {
    Breakdown,
    Bridge,
    Chorus,
    Ending,
    Instrumental,
    Interlude,
    Intro,
    Outro,
    Post(Box<SectionType>),  // Post-Chorus, Post-Verse, etc.
    Pre(Box<SectionType>),   // Pre-Chorus, Pre-Verse, etc.
    PreChorus,
    Refrain,
    Solo,
    Tag,
    Turnaround,
    Vamp,
    Verse,
}

impl SectionType {
    /// Get the full name of the section (dynamic for Post)
    pub fn full_name(&self) -> String {
        match self {
            SectionType::Breakdown => "Breakdown".to_string(),
            SectionType::Bridge => "Bridge".to_string(),
            SectionType::Chorus => "Chorus".to_string(),
            SectionType::Ending => "Ending".to_string(),
            SectionType::Instrumental => "Instrumental".to_string(),
            SectionType::Interlude => "Interlude".to_string(),
            SectionType::Intro => "Intro".to_string(),
            SectionType::Outro => "Outro".to_string(),
            SectionType::Post(inner) => format!("Post-{}", inner.full_name()),
            SectionType::Pre(inner) => format!("Pre-{}", inner.full_name()),
            SectionType::PreChorus => "Pre-Chorus".to_string(),
            SectionType::Refrain => "Refrain".to_string(),
            SectionType::Solo => "Solo".to_string(),
            SectionType::Tag => "Tag".to_string(),
            SectionType::Turnaround => "Turnaround".to_string(),
            SectionType::Vamp => "Vamp".to_string(),
            SectionType::Verse => "Verse".to_string(),
        }
    }

    /// Get the abbreviated name of the section (dynamic for Post)
    pub fn abbreviation(&self) -> String {
        match self {
            SectionType::Breakdown => "BD".to_string(),
            SectionType::Bridge => "BR".to_string(),
            SectionType::Chorus => "CH".to_string(),
            SectionType::Ending => "END".to_string(),
            SectionType::Instrumental => "INST".to_string(),
            SectionType::Interlude => "INT".to_string(),
            SectionType::Intro => "IN".to_string(),
            SectionType::Outro => "OUT".to_string(),
            SectionType::Post(inner) => format!("POST-{}", inner.abbreviation()),
            SectionType::Pre(inner) => format!("PRE-{}", inner.abbreviation()),
            SectionType::PreChorus => "PRE".to_string(),
            SectionType::Refrain => "REF".to_string(),
            SectionType::Solo => "SOLO".to_string(),
            SectionType::Tag => "TAG".to_string(),
            SectionType::Turnaround => "TA".to_string(),
            SectionType::Vamp => "VAMP".to_string(),
            SectionType::Verse => "VS".to_string(),
        }
    }

    /// Get the LilyPond markup for the section with auto-numbering
    pub fn to_lilypond_markup(&self, number: u32) -> String {
        format!(r#"\mark "{} {}""#, self.abbreviation(), number)
    }

    /// Get the LilyPond markup for the section without numbering
    pub fn to_lilypond_markup_no_number(&self) -> String {
        format!(r#"\mark "{}""#, self.abbreviation())
    }
    
    /// Check if this section type should be numbered in charts
    pub fn should_number(&self) -> bool {
        match self {
            SectionType::Intro | SectionType::Outro | SectionType::Ending | 
            SectionType::Solo | SectionType::Tag | SectionType::Turnaround | 
            SectionType::Vamp | SectionType::PreChorus | SectionType::Instrumental => false,
            SectionType::Post(_) => false, // Post sections are typically not numbered
            SectionType::Pre(_) => false,   // Pre sections are typically not numbered
            _ => true,
        }
    }
}

/// Represents a section with its type and optional number
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Section {
    pub section_type: SectionType,
    pub number: Option<u32>,
    pub split_letter: Option<char>,  // 'a', 'b', 'c' for consecutive sections
    pub measures: u32,
}

impl Section {
    /// Create a new section with a number
    pub fn new(section_type: SectionType, number: u32, measures: u32) -> Self {
        Self {
            section_type,
            number: Some(number),
            split_letter: None,
            measures,
        }
    }

    /// Create a new section without a number
    pub fn new_no_number(section_type: SectionType, measures: u32) -> Self {
        Self {
            section_type,
            number: None,
            split_letter: None,
            measures,
        }
    }
    
    /// Create a new section with optional measures (for auto-numbering)
    pub fn new_with_optional_measures(section_type: SectionType, measures: Option<u32>) -> Self {
        Self {
            section_type,
            number: None, // Will be set by auto-numbering
            split_letter: None,
            measures: measures.unwrap_or(0), // Default to 0 if not specified
        }
    }
    
    /// Generate the chart name for display
    /// 
    /// Rules:
    /// - Intro, Outro, Ending, Solo, Tag, Turnaround, Vamp: Use full names
    /// - Everything else: Use abbreviations ("VS 1", "CH 2", "BR")
    /// - Include number and split letter if present
    pub fn generate_chart_name(&self) -> String {
        let base_name = match &self.section_type {
            SectionType::Intro | SectionType::Outro | SectionType::Ending | 
            SectionType::Solo | SectionType::Tag | SectionType::Turnaround | 
            SectionType::Vamp => self.section_type.full_name(),
            _ => self.section_type.abbreviation(),
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

    /// Get the LilyPond markup for this section
    pub fn to_lilypond_markup(&self) -> String {
        match self.number {
            Some(num) => self.section_type.to_lilypond_markup(num),
            None => self.section_type.to_lilypond_markup_no_number(),
        }
    }

    /// Get the LilyPond skip music for the measures
    pub fn to_lilypond_skip(&self) -> String {
        format!("s1*{}", self.measures)
    }

    /// Get the complete LilyPond section (mark + skip)
    pub fn to_lilypond_section(&self) -> String {
        format!("{}\n{}", self.to_lilypond_markup(), self.to_lilypond_skip())
    }
}

/// Helper function to create a verse section
pub fn verse(number: u32, measures: u32) -> Section {
    Section::new(SectionType::Verse, number, measures)
}

/// Helper function to create a chorus section
pub fn chorus(number: u32, measures: u32) -> Section {
    Section::new(SectionType::Chorus, number, measures)
}

/// Helper function to create a bridge section
pub fn bridge(number: u32, measures: u32) -> Section {
    Section::new(SectionType::Bridge, number, measures)
}

/// Helper function to create an intro section
pub fn intro(measures: u32) -> Section {
    Section::new_no_number(SectionType::Intro, measures)
}

/// Helper function to create an outro section
pub fn outro(measures: u32) -> Section {
    Section::new_no_number(SectionType::Outro, measures)
}

/// Helper function to create a breakdown section
pub fn breakdown(number: u32, measures: u32) -> Section {
    Section::new(SectionType::Breakdown, number, measures)
}

/// Helper function to create an ending section
pub fn ending(measures: u32) -> Section {
    Section::new_no_number(SectionType::Ending, measures)
}

/// Helper function to create an interlude section
pub fn interlude(number: u32, measures: u32) -> Section {
    Section::new(SectionType::Interlude, number, measures)
}

/// Helper function to create a refrain section
pub fn refrain(number: u32, measures: u32) -> Section {
    Section::new(SectionType::Refrain, number, measures)
}

/// Helper function to create a solo section
pub fn solo(measures: u32) -> Section {
    Section::new_no_number(SectionType::Solo, measures)
}

/// Helper function to create a tag section
pub fn tag(measures: u32) -> Section {
    Section::new_no_number(SectionType::Tag, measures)
}

/// Helper function to create a turnaround section
pub fn turnaround(measures: u32) -> Section {
    Section::new_no_number(SectionType::Turnaround, measures)
}

/// Helper function to create a vamp section
pub fn vamp(measures: u32) -> Section {
    Section::new_no_number(SectionType::Vamp, measures)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_section_type_names() {
        assert_eq!(SectionType::Verse.full_name(), "Verse");
        assert_eq!(SectionType::Verse.abbreviation(), "VS");
        assert_eq!(SectionType::Chorus.full_name(), "Chorus");
        assert_eq!(SectionType::Chorus.abbreviation(), "CH");
        assert_eq!(SectionType::Breakdown.full_name(), "Breakdown");
        assert_eq!(SectionType::Breakdown.abbreviation(), "BD");
        assert_eq!(SectionType::Solo.full_name(), "Solo");
        assert_eq!(SectionType::Solo.abbreviation(), "SOLO");
        assert_eq!(SectionType::Vamp.full_name(), "Vamp");
        assert_eq!(SectionType::Vamp.abbreviation(), "VAMP");
    }

    #[test]
    fn test_lilypond_markup() {
        assert_eq!(SectionType::Verse.to_lilypond_markup(1), r#"\mark "VS 1""#);
        assert_eq!(SectionType::Chorus.to_lilypond_markup(2), r#"\mark "CH 2""#);
        assert_eq!(SectionType::Intro.to_lilypond_markup_no_number(), r#"\mark "IN""#);
    }

    #[test]
    fn test_section_creation() {
        let verse1 = verse(1, 8);
        assert_eq!(verse1.section_type, SectionType::Verse);
        assert_eq!(verse1.number, Some(1));
        assert_eq!(verse1.measures, 8);

        let intro_section = intro(4);
        assert_eq!(intro_section.section_type, SectionType::Intro);
        assert_eq!(intro_section.number, None);
        assert_eq!(intro_section.measures, 4);
    }

    #[test]
    fn test_section_lilypond_output() {
        let verse1 = verse(1, 8);
        let expected = r#"\mark "VS 1"
s1*8"#;
        assert_eq!(verse1.to_lilypond_section(), expected);

        let intro_section = intro(4);
        let expected_intro = r#"\mark "IN"
s1*4"#;
        assert_eq!(intro_section.to_lilypond_section(), expected_intro);
    }

    #[test]
    fn test_new_section_types() {
        let breakdown1 = breakdown(1, 8);
        assert_eq!(breakdown1.section_type, SectionType::Breakdown);
        assert_eq!(breakdown1.number, Some(1));
        assert_eq!(breakdown1.measures, 8);

        let solo_section = solo(12);
        assert_eq!(solo_section.section_type, SectionType::Solo);
        assert_eq!(solo_section.number, None);
        assert_eq!(solo_section.measures, 12);

        let vamp_section = vamp(4);
        assert_eq!(vamp_section.section_type, SectionType::Vamp);
        assert_eq!(vamp_section.number, None);
        assert_eq!(vamp_section.measures, 4);
    }

    #[test]
    fn test_chart_name_generation() {
        // Test sections that use full names
        let solo_section = solo(8);
        assert_eq!(solo_section.generate_chart_name(), "Solo");

        let vamp_section = vamp(4);
        assert_eq!(vamp_section.generate_chart_name(), "Vamp");

        // Test sections that use abbreviations
        let breakdown1 = breakdown(1, 8);
        assert_eq!(breakdown1.generate_chart_name(), "BD 1");

        let chorus2 = chorus(2, 16);
        assert_eq!(chorus2.generate_chart_name(), "CH 2");
    }

    #[test]
    fn test_post_sections() {
        // Test Post-Chorus
        let post_chorus = SectionType::Post(Box::new(SectionType::Chorus));
        assert_eq!(post_chorus.full_name(), "Post-Chorus");
        assert_eq!(post_chorus.abbreviation(), "POST-CH");

        // Test Post-Verse
        let post_verse = SectionType::Post(Box::new(SectionType::Verse));
        assert_eq!(post_verse.full_name(), "Post-Verse");
        assert_eq!(post_verse.abbreviation(), "POST-VS");

        // Test Post-Bridge
        let post_bridge = SectionType::Post(Box::new(SectionType::Bridge));
        assert_eq!(post_bridge.full_name(), "Post-Bridge");
        assert_eq!(post_bridge.abbreviation(), "POST-BR");
    }

    #[test]
    fn test_split_letters() {
        let mut verse1a = verse(1, 8);
        verse1a.split_letter = Some('a');
        assert_eq!(verse1a.generate_chart_name(), "VS 1a");

        let mut chorus2b = chorus(2, 16);
        chorus2b.split_letter = Some('b');
        assert_eq!(chorus2b.generate_chart_name(), "CH 2b");

        let mut solo_section = solo(12);
        solo_section.split_letter = Some('c');
        assert_eq!(solo_section.generate_chart_name(), "Soloc");
    }

    #[test]
    fn test_lilypond_output_with_post() {
        let post_chorus_type = SectionType::Post(Box::new(SectionType::Chorus));
        assert_eq!(post_chorus_type.to_lilypond_markup(1), r#"\mark "POST-CH 1""#);
        assert_eq!(post_chorus_type.to_lilypond_markup_no_number(), r#"\mark "POST-CH""#);
    }

    #[test]
    fn test_all_section_abbreviations() {
        // Test all abbreviations are unique and reasonable length
        let abbreviations = vec![
            (SectionType::Breakdown, "BD"),
            (SectionType::Bridge, "BR"),
            (SectionType::Chorus, "CH"),
            (SectionType::Ending, "END"),
            (SectionType::Instrumental, "INST"),
            (SectionType::Interlude, "INT"),
            (SectionType::Intro, "IN"),
            (SectionType::Outro, "OUT"),
            (SectionType::Refrain, "REF"),
            (SectionType::Solo, "SOLO"),
            (SectionType::Tag, "TAG"),
            (SectionType::Turnaround, "TA"),
            (SectionType::Vamp, "VAMP"),
            (SectionType::Verse, "VS"),
        ];

        for (section_type, expected_abbrev) in abbreviations {
            assert_eq!(section_type.abbreviation(), expected_abbrev);
        }
    }

    #[test]
    fn test_all_section_full_names() {
        // Test all full names are properly capitalized
        let full_names = vec![
            (SectionType::Breakdown, "Breakdown"),
            (SectionType::Bridge, "Bridge"),
            (SectionType::Chorus, "Chorus"),
            (SectionType::Ending, "Ending"),
            (SectionType::Instrumental, "Instrumental"),
            (SectionType::Interlude, "Interlude"),
            (SectionType::Intro, "Intro"),
            (SectionType::Outro, "Outro"),
            (SectionType::Refrain, "Refrain"),
            (SectionType::Solo, "Solo"),
            (SectionType::Tag, "Tag"),
            (SectionType::Turnaround, "Turnaround"),
            (SectionType::Vamp, "Vamp"),
            (SectionType::Verse, "Verse"),
        ];

        for (section_type, expected_name) in full_names {
            assert_eq!(section_type.full_name(), expected_name);
        }
    }

    #[test]
    fn test_section_creation_methods() {
        // Test numbered sections
        let verse1 = Section::new(SectionType::Verse, 1, 8);
        assert_eq!(verse1.section_type, SectionType::Verse);
        assert_eq!(verse1.number, Some(1));
        assert_eq!(verse1.measures, 8);
        assert_eq!(verse1.split_letter, None);

        // Test unnumbered sections
        let intro = Section::new_no_number(SectionType::Intro, 4);
        assert_eq!(intro.section_type, SectionType::Intro);
        assert_eq!(intro.number, None);
        assert_eq!(intro.measures, 4);
        assert_eq!(intro.split_letter, None);
    }

    #[test]
    fn test_complex_chart_names() {
        // Test numbered sections with full names
        let ending1 = Section::new(SectionType::Ending, 1, 4);
        assert_eq!(ending1.generate_chart_name(), "Ending 1");

        let tag2 = Section::new(SectionType::Tag, 2, 2);
        assert_eq!(tag2.generate_chart_name(), "Tag 2");

        // Test unnumbered sections with abbreviations
        let instrumental = Section::new_no_number(SectionType::Instrumental, 16);
        assert_eq!(instrumental.generate_chart_name(), "INST");

        let interlude = Section::new_no_number(SectionType::Interlude, 8);
        assert_eq!(interlude.generate_chart_name(), "INT");
    }

    #[test]
    fn test_lilypond_section_generation() {
        // Test complete LilyPond section output
        let verse1 = verse(1, 8);
        let expected = r#"\mark "VS 1"
s1*8"#;
        assert_eq!(verse1.to_lilypond_section(), expected);

        let intro = intro(4);
        let expected_intro = r#"\mark "IN"
s1*4"#;
        assert_eq!(intro.to_lilypond_section(), expected_intro);

        // Test with post-sections
        let post_chorus_type = SectionType::Post(Box::new(SectionType::Chorus));
        let post_chorus = Section::new(post_chorus_type, 1, 16);
        let expected_post = r#"\mark "POST-CH 1"
s1*16"#;
        assert_eq!(post_chorus.to_lilypond_section(), expected_post);
    }

    #[test]
    fn test_helper_function_consistency() {
        // Test that all helper functions create sections with correct properties
        let sections = vec![
            (verse(1, 8), SectionType::Verse, Some(1)),
            (chorus(2, 16), SectionType::Chorus, Some(2)),
            (bridge(1, 8), SectionType::Bridge, Some(1)),
            (breakdown(1, 8), SectionType::Breakdown, Some(1)),
            (interlude(1, 8), SectionType::Interlude, Some(1)),
            (refrain(1, 8), SectionType::Refrain, Some(1)),
            (intro(4), SectionType::Intro, None),
            (outro(4), SectionType::Outro, None),
            (ending(4), SectionType::Ending, None),
            (solo(12), SectionType::Solo, None),
            (tag(2), SectionType::Tag, None),
            (turnaround(4), SectionType::Turnaround, None),
            (vamp(8), SectionType::Vamp, None),
        ];

        for (section, expected_type, expected_number) in sections {
            assert_eq!(section.section_type, expected_type);
            assert_eq!(section.number, expected_number);
            assert_eq!(section.split_letter, None);
        }
    }
}

/// Auto-numbering system for sections
#[derive(Debug, Clone)]
pub struct SectionNumberer {
    counters: HashMap<SectionType, u32>,
    last_section: Option<SectionType>,
    consecutive_count: u32, // Count of consecutive sections of the same type
}

impl SectionNumberer {
    pub fn new() -> Self {
        Self {
            counters: HashMap::new(),
            last_section: None,
            consecutive_count: 0,
        }
    }
    
    /// Process a section and assign auto-numbering
    pub fn process_section(&mut self, section_type: SectionType, measures: Option<u32>) -> Section {
        let mut section = Section::new_with_optional_measures(section_type.clone(), measures);
        
        // Only number sections that should be numbered
        if section_type.should_number() {
            // Check if this is the same section type as the last one (for split letters)
            if let Some(last) = &self.last_section {
                if *last == section_type {
                    // Same section type - add split letter
                    self.consecutive_count += 1;
                    let split_letter = char::from_u32(('a' as u32) + (self.consecutive_count - 2)).unwrap_or('a');
                    section.split_letter = Some(split_letter);
                    // Keep the same number as the previous section
                    let current_count = self.counters.get(&section_type).unwrap_or(&0);
                    section.number = Some(*current_count);
                } else {
                    // Different section type - increment counter and reset consecutive count
                    let count = self.counters.entry(section_type.clone()).or_insert(0);
                    *count += 1;
                    section.number = Some(*count);
                    self.consecutive_count = 1;
                }
            } else {
                // First section of this type
                let count = self.counters.entry(section_type.clone()).or_insert(0);
                *count += 1;
                section.number = Some(*count);
                self.consecutive_count = 1;
            }
        }
        
        self.last_section = Some(section_type);
        section
    }
}

impl Default for SectionNumberer {
    fn default() -> Self {
        Self::new()
    }
}
