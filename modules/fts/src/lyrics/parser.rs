//! Parser for lyrics text format

use super::core::{LyricLine, LyricSection, Lyrics};
use thiserror::Error;

/// Parser configuration options
#[derive(Debug, Clone, PartialEq)]
pub struct ParserConfig {
    /// Whether to split lyrics into separate lead and background vocal tracks
    pub split_lead_background: bool,
    /// Pattern to identify background vocals (default: parenthetical text like "(Hey)", "(Oh)")
    pub background_vocal_pattern: BackgroundVocalPattern,
}

impl Default for ParserConfig {
    fn default() -> Self {
        Self {
            split_lead_background: false,
            background_vocal_pattern: BackgroundVocalPattern::Parenthetical,
        }
    }
}

/// Pattern used to identify background vocals
#[derive(Debug, Clone, PartialEq)]
pub enum BackgroundVocalPattern {
    /// Background vocals are marked with parentheses: (Hey), (Oh), etc.
    Parenthetical,
    /// Custom pattern (for future use)
    Custom(String),
}

/// Errors that can occur during parsing
#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Invalid section marker: {0}")]
    InvalidSectionMarker(String),
    #[error("Unexpected end of input")]
    UnexpectedEnd,
    #[error("Parse error: {0}")]
    Other(String),
}

/// Parse lyrics from text format
///
/// Expected format:
/// ```text
/// [Intro]
/// (Woo)
/// (Are you ready?)
/// Well!
/// Well!
/// [Verse 1]
/// Drowning, fishing, dropping, screaming under the lights
/// ...
/// ```
pub fn parse_lyrics(text: &str, song_name: String) -> Result<Lyrics, ParseError> {
    parse_lyrics_with_config(text, song_name, ParserConfig::default())
}

/// Parse lyrics from text format with configuration
pub fn parse_lyrics_with_config(
    text: &str,
    song_name: String,
    config: ParserConfig,
) -> Result<Lyrics, ParseError> {
    let mut lyrics = Lyrics::new(song_name);
    let lines: Vec<&str> = text.lines().collect();
    let mut current_section: Option<LyricSection> = None;
    let mut i = 0;

    while i < lines.len() {
        let line = lines[i].trim();

        // Skip empty lines
        if line.is_empty() {
            i += 1;
            continue;
        }

        // Check if this is a section marker
        if let Some(section_name) = parse_section_marker(line) {
            // Save previous section if it exists
            if let Some(section) = current_section.take() {
                lyrics.add_section(section);
            }

            // Create new section
            current_section = Some(LyricSection::new(section_name));
        } else {
            // This is a lyric line
            if let Some(ref mut section) = current_section {
                let lyric_line = LyricLine::new(line.to_string());
                section.add_line(lyric_line);
            } else {
                // No section yet - create a default one
                current_section = Some(LyricSection::new("Default".to_string()));
                if let Some(ref mut section) = current_section {
                    let lyric_line = LyricLine::new(line.to_string());
                    section.add_line(lyric_line);
                }
            }
        }

        i += 1;
    }

    // Add the last section if it exists
    if let Some(section) = current_section {
        lyrics.add_section(section);
    }

    // If splitting lead/background, process the lyrics
    if config.split_lead_background {
        lyrics = split_lead_background(lyrics, &config)?;
    }

    Ok(lyrics)
}

/// Split lyrics into lead and background vocal tracks
fn split_lead_background(
    lyrics: Lyrics,
    _config: &ParserConfig,
) -> Result<Lyrics, ParseError> {
    // For now, we'll mark parenthetical parts as background vocals
    // In the future, we could create separate Lyrics objects or add metadata
    let mut processed_lyrics = lyrics;
    
    // Add metadata to indicate this has been split
    processed_lyrics.set_metadata("split_lead_background", "true");
    
    // Process each section
    for section in &mut processed_lyrics.sections {
        for line in &mut section.lines {
            // Mark parenthetical parts as background vocals
            for part in &mut line.parts {
                if let crate::lyrics::core::LinePart::Parenthetical(_) = part {
                    // We could add metadata here or create a new LinePart variant
                    // For now, parenthetical parts are already identified
                }
            }
        }
    }
    
    Ok(processed_lyrics)
}

/// Parse a section marker from a line
/// Returns Some(section_name) if the line is a section marker, None otherwise
fn parse_section_marker(line: &str) -> Option<String> {
    let trimmed = line.trim();
    if trimmed.starts_with('[') && trimmed.ends_with(']') {
        let name = trimmed[1..trimmed.len() - 1].trim();
        if !name.is_empty() {
            return Some(name.to_string());
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_section_marker() {
        assert_eq!(parse_section_marker("[Intro]"), Some("Intro".to_string()));
        assert_eq!(parse_section_marker("[Verse 1]"), Some("Verse 1".to_string()));
        assert_eq!(parse_section_marker("[Chorus]"), Some("Chorus".to_string()));
        assert_eq!(parse_section_marker("Not a section"), None);
        assert_eq!(parse_section_marker("[Invalid"), None);
    }

    #[test]
    fn test_parse_lyrics() {
        let text = r#"[Intro]
(Woo)
(Are you ready?)
Well!
Well!

[Verse 1]
Drowning, fishing, dropping, screaming under the lights
I'm feeling everything crashing, burning, I lost track of time
Into you

[Chorus]
I'm breathing, I'm breathing, I think I'm reading you well
I'm breathing, I'm breathing, I think I'm reading you well"#;

        let lyrics = parse_lyrics(text, "Test Song".to_string()).unwrap();
        assert_eq!(lyrics.sections.len(), 3);
        assert_eq!(lyrics.sections[0].name, "Intro");
        assert_eq!(lyrics.sections[0].lines.len(), 4);
        assert_eq!(lyrics.sections[1].name, "Verse 1");
        assert_eq!(lyrics.sections[1].lines.len(), 3);
        assert_eq!(lyrics.sections[2].name, "Chorus");
        assert_eq!(lyrics.sections[2].lines.len(), 2);
    }

    #[test]
    fn test_parse_parenthetical() {
        let line = LyricLine::new("(Woo)".to_string());
        assert_eq!(line.parts.len(), 1);
        match &line.parts[0] {
            crate::lyrics::core::LinePart::Parenthetical(text) => assert_eq!(text, "Woo"),
            _ => panic!("Expected parenthetical part"),
        }

        let line2 = LyricLine::new("Well! (Are you ready?) Well!".to_string());
        assert_eq!(line2.parts.len(), 3);
    }

    #[test]
    fn test_parser_config_default() {
        let config = ParserConfig::default();
        assert!(!config.split_lead_background);
        assert_eq!(config.background_vocal_pattern, BackgroundVocalPattern::Parenthetical);
    }

    #[test]
    fn test_parse_with_config() {
        let text = r#"[Verse 1]
Main lyrics (Hey) more lyrics (Oh)"#;

        let config = ParserConfig {
            split_lead_background: true,
            background_vocal_pattern: BackgroundVocalPattern::Parenthetical,
        };

        let lyrics = parse_lyrics_with_config(text, "Test Song".to_string(), config).unwrap();
        assert_eq!(lyrics.get_metadata("split_lead_background"), Some(&"true".to_string()));
    }
}
