//! Section Types
//!
//! Defines different types of song sections

use super::measure_expr::MeasureExpression;
use facet::Facet;
use serde::{Deserialize, Serialize};

/// Represents different types of song sections
#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Facet)]
pub enum SectionType {
    Intro,
    Verse,
    Chorus,
    Bridge,
    Outro,
    Instrumental,
    CountIn,                // Count-in measures (rendered small, with whole rests)
    End,                    // End section (from SONGEND to =END, for ring-out/fade)
    Hits,                   // Hits section (rhythmic accents)
    Interlude,              // Interlude section
    Breakdown,              // Breakdown section
    Pre(Box<SectionType>),  // Pre-Chorus, Pre-Verse, etc.
    Post(Box<SectionType>), // Post-Chorus, Post-Verse, etc.
    Custom(String),         // Custom section types like "SOLO Keys", etc.
}

/// Result of parsing a section marker line.
///
/// Contains the section type, optional measure expression, and optional comment/annotation.
#[derive(Debug, Clone, PartialEq)]
pub struct ParsedSection {
    /// The section type (Verse, Chorus, etc.)
    pub section_type: SectionType,
    /// Optional measure expression (e.g., "8", "+1", "4x4")
    pub measure_expr: Option<MeasureExpression>,
    /// Optional comment/annotation (e.g., "Down", "Build", "Horns", "Half-time")
    pub comment: Option<String>,
}

impl ParsedSection {
    /// Create a new parsed section with just a section type.
    pub fn new(section_type: SectionType) -> Self {
        Self {
            section_type,
            measure_expr: None,
            comment: None,
        }
    }

    /// Create a parsed section with section type and measure expression.
    pub fn with_measures(
        section_type: SectionType,
        measure_expr: Option<MeasureExpression>,
    ) -> Self {
        Self {
            section_type,
            measure_expr,
            comment: None,
        }
    }

    /// Create a full parsed section with all fields.
    pub fn full(
        section_type: SectionType,
        measure_expr: Option<MeasureExpression>,
        comment: Option<String>,
    ) -> Self {
        Self {
            section_type,
            measure_expr,
            comment,
        }
    }
}

/// Preset comment modifiers that can appear before section types.
/// e.g., "Down CH 4" -> Chorus with comment "Down"
const COMMENT_PRESETS: &[(&str, &str)] = &[
    ("down", "Down"),
    ("build", "Build"),
    ("half-time", "Half-time"),
    ("halftime", "Half-time"),
    ("double-time", "Double-time"),
    ("doubletime", "Double-time"),
    ("soft", "Soft"),
    ("loud", "Loud"),
    ("quiet", "Quiet"),
    ("big", "Big"),
    ("small", "Small"),
    ("sparse", "Sparse"),
    ("full", "Full"),
    ("stripped", "Stripped"),
    ("breakdown", "Breakdown"), // Can be both a section type and a modifier
];

impl SectionType {
    /// Get a lowercase key for this section type.
    ///
    /// Useful for caching, config lookup, or CSS class names.
    /// Returns lowercase strings like "intro", "verse", "pre_chorus".
    pub fn key(&self) -> String {
        match self {
            SectionType::Intro => "intro".to_string(),
            SectionType::Verse => "verse".to_string(),
            SectionType::Chorus => "chorus".to_string(),
            SectionType::Bridge => "bridge".to_string(),
            SectionType::Outro => "outro".to_string(),
            SectionType::Instrumental => "instrumental".to_string(),
            SectionType::CountIn => "count_in".to_string(),
            SectionType::End => "end".to_string(),
            SectionType::Hits => "hits".to_string(),
            SectionType::Interlude => "interlude".to_string(),
            SectionType::Breakdown => "breakdown".to_string(),
            SectionType::Pre(inner) => format!("pre_{}", inner.key()),
            SectionType::Post(inner) => format!("post_{}", inner.key()),
            SectionType::Custom(name) => {
                // Convert custom name to lowercase with underscores
                name.to_lowercase().replace(' ', "_")
            }
        }
    }

    /// Check if this section type should be rendered in charts.
    ///
    /// Returns false for End sections, which are typically silent/fade-out
    /// and don't need visual representation in the chart.
    pub fn should_render(&self) -> bool {
        !matches!(self, SectionType::End)
    }

    /// Get the full name of the section
    pub fn full_name(&self) -> String {
        match self {
            SectionType::Intro => "Intro".to_string(),
            SectionType::Verse => "Verse".to_string(),
            SectionType::Chorus => "Chorus".to_string(),
            SectionType::Bridge => "Bridge".to_string(),
            SectionType::Outro => "Outro".to_string(),
            SectionType::Instrumental => "Instrumental".to_string(),
            SectionType::CountIn => "Count-In".to_string(),
            SectionType::End => "End".to_string(),
            SectionType::Hits => "Hits".to_string(),
            SectionType::Interlude => "Interlude".to_string(),
            SectionType::Breakdown => "Breakdown".to_string(),
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
            SectionType::CountIn => "COUNT".to_string(),
            SectionType::End => "END".to_string(),
            SectionType::Hits => "HITS".to_string(),
            SectionType::Interlude => "INT".to_string(),
            SectionType::Breakdown => "BD".to_string(),
            SectionType::Pre(inner) => format!("PRE-{}", inner.abbreviation()),
            SectionType::Post(inner) => format!("POST-{}", inner.abbreviation()),
            SectionType::Custom(name) => name.clone(), // Custom sections use their full name
        }
    }

    /// Check if this section type should be numbered in charts
    pub fn should_number(&self) -> bool {
        match self {
            SectionType::Intro
            | SectionType::Outro
            | SectionType::Instrumental
            | SectionType::CountIn
            | SectionType::End => false,
            SectionType::Hits | SectionType::Interlude | SectionType::Breakdown => false,
            SectionType::Pre(_) | SectionType::Post(_) => false,
            SectionType::Custom(_) => false, // Custom sections don't get numbered
            _ => true,
        }
    }

    /// Check if this section type should show a section header/label in charts
    /// CountIn and End are hidden from charts but visible in progress bars
    pub fn should_show_header(&self) -> bool {
        !matches!(self, SectionType::CountIn | SectionType::End)
    }

    /// Check if this section should use compact/small measure rendering
    pub fn is_compact(&self) -> bool {
        matches!(self, SectionType::CountIn)
    }

    /// Parse a section type from a string (name or abbreviation)
    ///
    /// Handles case-insensitive matching and common typos/variations:
    /// - "verse", "Verse", "VERSE", "vs", "VS", "vErSe", "vrse" -> Verse
    /// - "chorus", "Chorus", "CHORUS", "ch", "CH", "chorous", "corus" -> Chorus
    /// - etc.
    pub fn parse(s: &str) -> Result<Self, String> {
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
            "count" | "countin" | "count-in" => return Ok(SectionType::CountIn),
            "hits" | "hit" => return Ok(SectionType::Hits),
            "interlude" | "inter" | "int" => return Ok(SectionType::Interlude),
            "breakdown" | "bd" => return Ok(SectionType::Breakdown),
            _ => {}
        }

        // Try fuzzy matching for common typos and variations
        // Verse variations
        if Self::fuzzy_match(s_lower, "verse", &["vrse", "verce", "vers", "versa"]) {
            return Ok(SectionType::Verse);
        }

        // Chorus variations
        if Self::fuzzy_match(
            s_lower,
            "chorus",
            &["chorous", "corus", "chrous", "chors", "chor"],
        ) {
            return Ok(SectionType::Chorus);
        }

        // Bridge variations
        if Self::fuzzy_match(s_lower, "bridge", &["bridg", "brige", "brid"]) {
            return Ok(SectionType::Bridge);
        }

        // Intro variations - handle "introduction", "intro", etc.
        // Note: "int" is NOT an intro variant - it maps to Interlude (see exact matches above)
        if Self::fuzzy_match(s_lower, "intro", &["intr", "introo", "introduction"]) {
            return Ok(SectionType::Intro);
        }
        // Also check if it starts with "introduction"
        if s_lower.starts_with("introduction") {
            return Ok(SectionType::Intro);
        }

        // Outro variations - handle "outroduction", "outro", etc.
        if Self::fuzzy_match(s_lower, "outro", &["outr", "out", "outroo", "outroduction"]) {
            return Ok(SectionType::Outro);
        }
        // Also check if it starts with "outroduction"
        if s_lower.starts_with("outroduction") {
            return Ok(SectionType::Outro);
        }

        // Instrumental variations
        if Self::fuzzy_match(
            s_lower,
            "instrumental",
            &["instumental", "instrumantal", "instrument"],
        ) {
            return Ok(SectionType::Instrumental);
        }

        // Count-in variations
        if Self::fuzzy_match(
            s_lower,
            "count",
            &["countin", "count-in", "countinn", "cnt"],
        ) {
            return Ok(SectionType::CountIn);
        }

        // Hits variations
        if Self::fuzzy_match(s_lower, "hits", &["hit", "hts"]) {
            return Ok(SectionType::Hits);
        }

        // Interlude variations
        if Self::fuzzy_match(s_lower, "interlude", &["inter", "interlud", "intrlude"]) {
            return Ok(SectionType::Interlude);
        }

        // Breakdown variations
        if Self::fuzzy_match(s_lower, "breakdown", &["brkdown", "breakdwn", "bdown"]) {
            return Ok(SectionType::Breakdown);
        }

        // Try to parse Pre/Post
        if let Some(rest) = s_lower.strip_prefix("pre-") {
            if let Ok(inner) = Self::parse(rest) {
                return Ok(SectionType::Pre(Box::new(inner)));
            }
        }
        if let Some(rest) = s_lower.strip_prefix("post-") {
            if let Ok(inner) = Self::parse(rest) {
                return Ok(SectionType::Post(Box::new(inner)));
            }
        }

        Err(format!(
            "Unknown section type: '{}' - supported types: verse, chorus, bridge, intro, outro, instrumental, count, hits, interlude, breakdown, pre-*, post-*",
            s
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

    /// Parse a section marker from input (for chart parsing)
    ///
    /// Supports:
    /// - Standard sections: "VS 16", "Intro 4", etc.
    /// - Custom sections with brackets: "[Hits]", "[SOLO Keys] 8", etc.
    /// - Expressions: "VS 8+1", "VS 4x4", "VS +1", "VS -1"
    /// - Quoted comments: `CH 4 "Down"`, `VS 8 "Build"`, `Interlude "Horns"`
    /// - Preset modifiers: `Down CH 4`, `Build VS 8`
    ///
    /// Returns a `ParsedSection` containing the section type, measure expression, and optional comment.
    pub fn parse_with_measure_count(input: &str) -> Option<ParsedSection> {
        let input = input.trim();

        // First, extract any quoted comment at the end: CH 4 "Down"
        let (input_without_quote, quoted_comment) = extract_quoted_comment(input);
        let input = input_without_quote.trim();

        // Check for preset modifier at the start: "Down CH 4"
        let (preset_comment, remaining_input) = extract_preset_modifier(input);

        // Use the quoted comment if present, otherwise use preset comment
        let comment = quoted_comment.or(preset_comment);
        let input = remaining_input;

        // Check for custom section with brackets: [Hits] or [SOLO Keys] 8
        if input.starts_with('[') && input.contains(']') {
            // Find the closing bracket
            if let Some(close_bracket_idx) = input[1..].find(']') {
                let name = &input[1..close_bracket_idx + 1]; // Extract name between brackets

                // Exclude track markers - these are not sections
                let name_lower = name.to_lowercase();
                let first_word = name_lower.split_whitespace().next().unwrap_or("");
                if ["chords", "melody", "rhythm", "lyrics"].contains(&first_word) {
                    return None; // This is a track marker, not a section
                }

                let remaining = input[close_bracket_idx + 2..].trim();

                // Parse measure expression if present
                let measure_expr = if remaining.is_empty() {
                    None
                } else {
                    // Must be a valid expression, otherwise it's not a section marker
                    Some(MeasureExpression::parse(remaining)?)
                };

                return Some(ParsedSection::full(
                    SectionType::Custom(name.to_string()),
                    measure_expr,
                    comment,
                ));
            }
        }

        // Parse standard sections (case-insensitive)
        let input_lower = input.to_lowercase();
        let parts: Vec<&str> = input_lower.split_whitespace().collect();

        if parts.is_empty() {
            return None;
        }

        let section_str = parts[0];

        // Section markers should be alone or followed by only a measure count/expression
        // This prevents "c d g" from being parsed as a section marker
        if parts.len() > 2 {
            return None; // Too many tokens, not a section marker
        }

        let measure_expr = if parts.len() > 1 {
            // If there's a second token, it must be a valid expression
            // Otherwise, this isn't a valid section marker
            Some(MeasureExpression::parse(parts[1])?)
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
            "count" | "countin" | "count-in" => Some(SectionType::CountIn),
            "hits" | "hit" => Some(SectionType::Hits),
            "interlude" | "inter" | "int" => Some(SectionType::Interlude),
            "breakdown" | "bd" => Some(SectionType::Breakdown),
            _ => None,
        };

        section_type.map(|st| ParsedSection::full(st, measure_expr, comment))
    }
}

/// Extract a quoted comment from the end of the input.
/// Returns (input without quote, optional comment)
/// Example: `CH 4 "Down"` -> (`CH 4`, Some("Down"))
/// Example: `Interlude "Horns"` -> (`Interlude`, Some("Horns"))
fn extract_quoted_comment(input: &str) -> (&str, Option<String>) {
    // Look for a quoted string at the end
    if let Some(last_quote) = input.rfind('"') {
        // Find the opening quote
        let before_last = &input[..last_quote];
        if let Some(open_quote) = before_last.rfind('"') {
            let comment = input[open_quote + 1..last_quote].trim().to_string();
            let remaining = input[..open_quote].trim();
            if !comment.is_empty() {
                return (remaining, Some(comment));
            }
        }
    }
    (input, None)
}

/// Extract a preset modifier from the start of the input.
/// Returns (optional comment, remaining input)
/// Example: `Down CH 4` -> (Some("Down"), `CH 4`)
fn extract_preset_modifier(input: &str) -> (Option<String>, &str) {
    let input_lower = input.to_lowercase();

    for (preset_lower, preset_display) in COMMENT_PRESETS {
        // Check if input starts with this preset followed by a space
        if input_lower.starts_with(preset_lower) {
            let after_preset = &input[preset_lower.len()..];
            if after_preset.starts_with(' ') || after_preset.starts_with('\t') {
                let remaining = after_preset.trim_start();
                // Make sure the remaining part could be a section
                // (has at least one more token that looks like a section type)
                let first_word = remaining.split_whitespace().next().unwrap_or("");
                let first_word_lower = first_word.to_lowercase();
                let could_be_section = matches!(
                    first_word_lower.as_str(),
                    "intro"
                        | "in"
                        | "verse"
                        | "vs"
                        | "v"
                        | "chorus"
                        | "ch"
                        | "c"
                        | "bridge"
                        | "br"
                        | "b"
                        | "outro"
                        | "out"
                        | "o"
                        | "instrumental"
                        | "inst"
                        | "i"
                        | "count"
                        | "countin"
                        | "count-in"
                        | "hits"
                        | "hit"
                        | "interlude"
                        | "inter"
                        | "int"
                        | "breakdown"
                        | "bd"
                ) || first_word.starts_with('[');

                if could_be_section {
                    return (Some((*preset_display).to_string()), remaining);
                }
            }
        }
    }
    (None, input)
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
            SectionType::parse_with_measure_count("vs 4"),
            Some(ParsedSection::with_measures(
                SectionType::Verse,
                Some(MeasureExpression::Absolute(4))
            ))
        );
        assert_eq!(
            SectionType::parse_with_measure_count("ch 8"),
            Some(ParsedSection::with_measures(
                SectionType::Chorus,
                Some(MeasureExpression::Absolute(8))
            ))
        );
        assert_eq!(
            SectionType::parse_with_measure_count("intro 2"),
            Some(ParsedSection::with_measures(
                SectionType::Intro,
                Some(MeasureExpression::Absolute(2))
            ))
        );
        assert_eq!(
            SectionType::parse_with_measure_count("br"),
            Some(ParsedSection::with_measures(SectionType::Bridge, None))
        );
    }

    #[test]
    fn test_parse_expressions() {
        // Addition expression
        assert_eq!(
            SectionType::parse_with_measure_count("vs 8+1"),
            Some(ParsedSection::with_measures(
                SectionType::Verse,
                Some(MeasureExpression::Absolute(9))
            ))
        );

        // Subtraction expression
        assert_eq!(
            SectionType::parse_with_measure_count("vs 8-1"),
            Some(ParsedSection::with_measures(
                SectionType::Verse,
                Some(MeasureExpression::Absolute(7))
            ))
        );

        // Multiplication expression
        assert_eq!(
            SectionType::parse_with_measure_count("vs 4x4"),
            Some(ParsedSection::with_measures(
                SectionType::Verse,
                Some(MeasureExpression::Absolute(16))
            ))
        );

        // Relative add
        assert_eq!(
            SectionType::parse_with_measure_count("vs +1"),
            Some(ParsedSection::with_measures(
                SectionType::Verse,
                Some(MeasureExpression::Add(1))
            ))
        );

        // Relative subtract
        assert_eq!(
            SectionType::parse_with_measure_count("vs -1"),
            Some(ParsedSection::with_measures(
                SectionType::Verse,
                Some(MeasureExpression::Subtract(1))
            ))
        );
    }

    #[test]
    fn test_parse_invalid() {
        assert_eq!(SectionType::parse_with_measure_count("invalid"), None);
        assert_eq!(SectionType::parse_with_measure_count(""), None);
        // Invalid expression should cause parse to fail
        assert_eq!(SectionType::parse_with_measure_count("vs abc"), None);
    }

    #[test]
    fn test_parse_custom_sections() {
        // Custom section with brackets
        assert_eq!(
            SectionType::parse_with_measure_count("[Hits]"),
            Some(ParsedSection::with_measures(
                SectionType::Custom("Hits".to_string()),
                None
            ))
        );

        // Custom section with brackets and measure count
        assert_eq!(
            SectionType::parse_with_measure_count("[SOLO Keys] 8"),
            Some(ParsedSection::with_measures(
                SectionType::Custom("SOLO Keys".to_string()),
                Some(MeasureExpression::Absolute(8))
            ))
        );

        // Custom section with brackets, no measure count
        assert_eq!(
            SectionType::parse_with_measure_count("[Bridge Out]"),
            Some(ParsedSection::with_measures(
                SectionType::Custom("Bridge Out".to_string()),
                None
            ))
        );

        // Custom section with expression
        assert_eq!(
            SectionType::parse_with_measure_count("[SOLO Keys] 4x2"),
            Some(ParsedSection::with_measures(
                SectionType::Custom("SOLO Keys".to_string()),
                Some(MeasureExpression::Absolute(8))
            ))
        );
    }

    #[test]
    fn test_parse_quoted_comments() {
        // Section with quoted comment
        let parsed = SectionType::parse_with_measure_count(r#"ch 4 "Down""#).unwrap();
        assert_eq!(parsed.section_type, SectionType::Chorus);
        assert_eq!(parsed.measure_expr, Some(MeasureExpression::Absolute(4)));
        assert_eq!(parsed.comment, Some("Down".to_string()));

        // Section with quoted comment and no measure count
        let parsed = SectionType::parse_with_measure_count(r#"vs "Build""#).unwrap();
        assert_eq!(parsed.section_type, SectionType::Verse);
        assert_eq!(parsed.measure_expr, None);
        assert_eq!(parsed.comment, Some("Build".to_string()));

        // Interlude with quoted comment (like from REAPER region)
        let parsed = SectionType::parse_with_measure_count(r#"interlude "Horns""#).unwrap();
        assert_eq!(parsed.section_type, SectionType::Interlude);
        assert_eq!(parsed.measure_expr, None);
        assert_eq!(parsed.comment, Some("Horns".to_string()));
    }

    #[test]
    fn test_parse_preset_modifiers() {
        // Down chorus
        let parsed = SectionType::parse_with_measure_count("Down ch 4").unwrap();
        assert_eq!(parsed.section_type, SectionType::Chorus);
        assert_eq!(parsed.measure_expr, Some(MeasureExpression::Absolute(4)));
        assert_eq!(parsed.comment, Some("Down".to_string()));

        // Build verse
        let parsed = SectionType::parse_with_measure_count("Build vs 8").unwrap();
        assert_eq!(parsed.section_type, SectionType::Verse);
        assert_eq!(parsed.measure_expr, Some(MeasureExpression::Absolute(8)));
        assert_eq!(parsed.comment, Some("Build".to_string()));

        // Half-time bridge
        let parsed = SectionType::parse_with_measure_count("Half-time br 4").unwrap();
        assert_eq!(parsed.section_type, SectionType::Bridge);
        assert_eq!(parsed.measure_expr, Some(MeasureExpression::Absolute(4)));
        assert_eq!(parsed.comment, Some("Half-time".to_string()));
    }

    #[test]
    fn test_custom_section_names() {
        let hits = SectionType::Custom("Hits".to_string());
        assert_eq!(hits.full_name(), "Hits");
        assert_eq!(hits.abbreviation(), "Hits");
        assert!(!hits.should_number());
    }

    #[test]
    fn test_section_type_keys() {
        assert_eq!(SectionType::Intro.key(), "intro");
        assert_eq!(SectionType::Verse.key(), "verse");
        assert_eq!(SectionType::Chorus.key(), "chorus");
        assert_eq!(SectionType::Bridge.key(), "bridge");
        assert_eq!(SectionType::Outro.key(), "outro");
        assert_eq!(SectionType::Instrumental.key(), "instrumental");
        assert_eq!(SectionType::CountIn.key(), "count_in");
        assert_eq!(SectionType::End.key(), "end");

        // Pre/Post sections
        let pre_chorus = SectionType::Pre(Box::new(SectionType::Chorus));
        assert_eq!(pre_chorus.key(), "pre_chorus");

        let post_verse = SectionType::Post(Box::new(SectionType::Verse));
        assert_eq!(post_verse.key(), "post_verse");

        // Custom sections
        let custom = SectionType::Custom("SOLO Keys".to_string());
        assert_eq!(custom.key(), "solo_keys");
    }

    #[test]
    fn test_should_render() {
        assert!(SectionType::Verse.should_render());
        assert!(SectionType::Chorus.should_render());
        assert!(SectionType::CountIn.should_render()); // CountIn is rendered (as compact)
        assert!(!SectionType::End.should_render()); // End is not rendered
    }
}
