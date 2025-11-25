//! Section Types
//!
//! Defines different types of song sections

/// Represents different types of song sections
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SectionType {
    Intro,
    Verse,
    Chorus,
    Bridge,
    Outro,
    Instrumental,
    Pre(Box<SectionType>),  // Pre-Chorus, Pre-Verse, etc.
    Post(Box<SectionType>), // Post-Chorus, Post-Verse, etc.
    Custom(String),         // Custom section types like "Hits", "SOLO Keys", etc.
}

impl SectionType {
    /// Get the full name of the section
    pub fn full_name(&self) -> String {
        match self {
            SectionType::Intro => "Intro".to_string(),
            SectionType::Verse => "Verse".to_string(),
            SectionType::Chorus => "Chorus".to_string(),
            SectionType::Bridge => "Bridge".to_string(),
            SectionType::Outro => "Outro".to_string(),
            SectionType::Instrumental => "Instrumental".to_string(),
            SectionType::Pre(inner) => format!("Pre-{}", inner.full_name()),
            SectionType::Post(inner) => format!("Post-{}", inner.full_name()),
            SectionType::Custom(name) => name.clone(),
        }
    }

    /// Get the abbreviated name of the section
    pub fn abbreviation(&self) -> String {
        match self {
            SectionType::Intro => "IN".to_string(),
            SectionType::Verse => "VS".to_string(),
            SectionType::Chorus => "CH".to_string(),
            SectionType::Bridge => "BR".to_string(),
            SectionType::Outro => "OUT".to_string(),
            SectionType::Instrumental => "INST".to_string(),
            SectionType::Pre(inner) => format!("PRE-{}", inner.abbreviation()),
            SectionType::Post(inner) => format!("POST-{}", inner.abbreviation()),
            SectionType::Custom(name) => name.clone(), // Custom sections use their full name
        }
    }

    /// Check if this section type should be numbered in charts
    pub fn should_number(&self) -> bool {
        match self {
            SectionType::Intro | SectionType::Outro | SectionType::Instrumental => false,
            SectionType::Pre(_) | SectionType::Post(_) => false,
            SectionType::Custom(_) => false, // Custom sections don't get numbered
            _ => true,
        }
    }

    /// Parse a section marker from input
    /// 
    /// Supports:
    /// - Standard sections: "VS 16", "Intro 4", etc.
    /// - Custom sections with brackets: "[Hits]", "[SOLO Keys] 8", etc.
    pub fn parse(input: &str) -> Option<(Self, Option<usize>)> {
        let input = input.trim();
        
        // Check for custom section with brackets: [Hits] or [SOLO Keys] 8
        if input.starts_with('[') && input.contains(']') {
            // Find the closing bracket
            if let Some(close_bracket_idx) = input[1..].find(']') {
                let name = &input[1..close_bracket_idx + 1]; // Extract name between brackets
                let remaining = input[close_bracket_idx + 2..].trim();
                
                // Parse measure count if present
                let measure_count = if remaining.is_empty() {
                    None
                } else {
                    remaining.parse::<usize>().ok()
                };
                
                return Some((SectionType::Custom(name.to_string()), measure_count));
            }
        }
        
        // Parse standard sections (case-insensitive)
        let input_lower = input.to_lowercase();
        let parts: Vec<&str> = input_lower.split_whitespace().collect();

        if parts.is_empty() {
            return None;
        }

        let section_str = parts[0];

        // Section markers should be alone or followed by only a measure count (number)
        // This prevents "c d g" from being parsed as a section marker
        if parts.len() > 2 {
            return None; // Too many tokens, not a section marker
        }

        let measure_count = if parts.len() > 1 {
            // If there's a second token, it must be a number (measure count)
            // Otherwise, this isn't a valid section marker
            match parts[1].parse::<usize>() {
                Ok(count) => Some(count),
                Err(_) => return None, // Second token is not a number, so not a valid section marker
            }
        } else {
            None
        };

        let section_type = match section_str {
            "intro" | "in" => Some(SectionType::Intro),
            "verse" | "vs" | "v" => Some(SectionType::Verse),
            "chorus" | "ch" | "c" => Some(SectionType::Chorus),
            "bridge" | "br" | "b" => Some(SectionType::Bridge),
            "outro" | "out" | "o" => Some(SectionType::Outro),
            "instrumental" | "inst" | "i" => Some(SectionType::Instrumental),
            _ => None,
        };

        section_type.map(|st| (st, measure_count))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_section_type_names() {
        assert_eq!(SectionType::Verse.full_name(), "Verse");
        assert_eq!(SectionType::Chorus.full_name(), "Chorus");
        assert_eq!(SectionType::Bridge.full_name(), "Bridge");
        assert_eq!(SectionType::Intro.full_name(), "Intro");
        assert_eq!(SectionType::Outro.full_name(), "Outro");
    }

    #[test]
    fn test_section_type_abbreviations() {
        assert_eq!(SectionType::Verse.abbreviation(), "VS");
        assert_eq!(SectionType::Chorus.abbreviation(), "CH");
        assert_eq!(SectionType::Bridge.abbreviation(), "BR");
        assert_eq!(SectionType::Intro.abbreviation(), "IN");
        assert_eq!(SectionType::Outro.abbreviation(), "OUT");
        assert_eq!(SectionType::Instrumental.abbreviation(), "INST");
    }

    #[test]
    fn test_pre_post_sections() {
        let pre_chorus = SectionType::Pre(Box::new(SectionType::Chorus));
        assert_eq!(pre_chorus.full_name(), "Pre-Chorus");
        assert_eq!(pre_chorus.abbreviation(), "PRE-CH");

        let post_chorus = SectionType::Post(Box::new(SectionType::Chorus));
        assert_eq!(post_chorus.full_name(), "Post-Chorus");
        assert_eq!(post_chorus.abbreviation(), "POST-CH");
    }

    #[test]
    fn test_should_number() {
        assert!(SectionType::Verse.should_number());
        assert!(SectionType::Chorus.should_number());
        assert!(SectionType::Bridge.should_number());

        assert!(!SectionType::Intro.should_number());
        assert!(!SectionType::Outro.should_number());
        assert!(!SectionType::Instrumental.should_number());
        assert!(!SectionType::Pre(Box::new(SectionType::Chorus)).should_number());
        assert!(!SectionType::Post(Box::new(SectionType::Chorus)).should_number());
    }

    #[test]
    fn test_parse_section_markers() {
        assert_eq!(
            SectionType::parse("vs 4"),
            Some((SectionType::Verse, Some(4)))
        );
        assert_eq!(
            SectionType::parse("ch 8"),
            Some((SectionType::Chorus, Some(8)))
        );
        assert_eq!(
            SectionType::parse("intro 2"),
            Some((SectionType::Intro, Some(2)))
        );
        assert_eq!(SectionType::parse("br"), Some((SectionType::Bridge, None)));
    }

    #[test]
    fn test_parse_invalid() {
        assert_eq!(SectionType::parse("invalid"), None);
        assert_eq!(SectionType::parse(""), None);
    }

    #[test]
    fn test_parse_custom_sections() {
        // Custom section with brackets
        assert_eq!(
            SectionType::parse("[Hits]"),
            Some((SectionType::Custom("Hits".to_string()), None))
        );
        
        // Custom section with brackets and measure count
        assert_eq!(
            SectionType::parse("[SOLO Keys] 8"),
            Some((SectionType::Custom("SOLO Keys".to_string()), Some(8)))
        );
        
        // Custom section with brackets, no measure count
        assert_eq!(
            SectionType::parse("[Bridge Out]"),
            Some((SectionType::Custom("Bridge Out".to_string()), None))
        );
    }

    #[test]
    fn test_custom_section_names() {
        let hits = SectionType::Custom("Hits".to_string());
        assert_eq!(hits.full_name(), "Hits");
        assert_eq!(hits.abbreviation(), "Hits");
        assert!(!hits.should_number());
    }
}
