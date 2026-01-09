//! Text cues for instrument-specific directions
//!
//! Provides a system for adding notes and directions to specific instrument groups
//! using @ notation (e.g., @keys "synth here", @drums "crash on 3")

use serde::{Deserialize, Serialize};
use std::fmt;

/// Represents an instrument group that can be targeted by cues
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum InstrumentGroup {
    All,
    Keys,
    Drums,
    Bass,
    Guitar,
    Vocals,
    Custom(String),
}

impl InstrumentGroup {
    /// Parse an instrument group from a string
    pub fn parse(s: &str) -> Self {
        match s.to_lowercase().as_str() {
            "all" => InstrumentGroup::All,
            "keys" | "key" | "keyboard" | "piano" => InstrumentGroup::Keys,
            "drums" | "drum" | "percussion" | "perc" => InstrumentGroup::Drums,
            "bass" => InstrumentGroup::Bass,
            "guitar" | "git" | "gtr" => InstrumentGroup::Guitar,
            "vocals" | "vox" | "voice" => InstrumentGroup::Vocals,
            _ => InstrumentGroup::Custom(s.to_string()),
        }
    }
}

impl fmt::Display for InstrumentGroup {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InstrumentGroup::All => write!(f, "all"),
            InstrumentGroup::Keys => write!(f, "keys"),
            InstrumentGroup::Drums => write!(f, "drums"),
            InstrumentGroup::Bass => write!(f, "bass"),
            InstrumentGroup::Guitar => write!(f, "guitar"),
            InstrumentGroup::Vocals => write!(f, "vocals"),
            InstrumentGroup::Custom(name) => write!(f, "{}", name),
        }
    }
}

/// Represents a text cue for a specific instrument group
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TextCue {
    pub group: InstrumentGroup,
    pub text: String,
}

impl TextCue {
    pub fn new(group: InstrumentGroup, text: String) -> Self {
        Self { group, text }
    }

    /// Parse a text cue from a line starting with @
    ///
    /// Format: @<group> "<text>"
    /// Example: @keys "synth here"
    pub fn parse(line: &str) -> Result<Self, String> {
        let line = line.trim();

        if !line.starts_with('@') {
            return Err("Text cue must start with @".to_string());
        }

        // Remove @ symbol
        let content = line[1..].trim();

        // Find the start of the quoted text
        if let Some(quote_start) = content.find('"') {
            let group_str = content[..quote_start].trim();
            let rest = &content[quote_start + 1..];

            // Find the closing quote
            if let Some(quote_end) = rest.find('"') {
                let text = rest[..quote_end].to_string();
                let group = InstrumentGroup::parse(group_str);

                Ok(TextCue::new(group, text))
            } else {
                Err("Missing closing quote for text cue".to_string())
            }
        } else {
            Err("Text cue must have quoted text".to_string())
        }
    }
}

impl fmt::Display for TextCue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@{} \"{}\"", self.group, self.text)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_instrument_group_parsing() {
        assert_eq!(InstrumentGroup::parse("keys"), InstrumentGroup::Keys);
        assert_eq!(InstrumentGroup::parse("DRUMS"), InstrumentGroup::Drums);
        assert_eq!(InstrumentGroup::parse("bass"), InstrumentGroup::Bass);
        assert_eq!(InstrumentGroup::parse("guitar"), InstrumentGroup::Guitar);
        assert_eq!(InstrumentGroup::parse("vocals"), InstrumentGroup::Vocals);
        assert_eq!(InstrumentGroup::parse("all"), InstrumentGroup::All);

        match InstrumentGroup::parse("synth") {
            InstrumentGroup::Custom(name) => assert_eq!(name, "synth"),
            _ => panic!("Expected Custom variant"),
        }
    }

    #[test]
    fn test_text_cue_parsing() {
        let cue = TextCue::parse("@keys \"synth here\"").unwrap();
        assert_eq!(cue.group, InstrumentGroup::Keys);
        assert_eq!(cue.text, "synth here");

        let cue2 = TextCue::parse("@drums \"crash on 3\"").unwrap();
        assert_eq!(cue2.group, InstrumentGroup::Drums);
        assert_eq!(cue2.text, "crash on 3");

        let cue3 = TextCue::parse("  @guitar \"let ring\"  ").unwrap();
        assert_eq!(cue3.group, InstrumentGroup::Guitar);
        assert_eq!(cue3.text, "let ring");
    }

    #[test]
    fn test_text_cue_errors() {
        assert!(TextCue::parse("keys \"synth here\"").is_err());
        assert!(TextCue::parse("@keys synth here").is_err());
        assert!(TextCue::parse("@keys \"missing quote").is_err());
    }

    #[test]
    fn test_text_cue_display() {
        let cue = TextCue::new(InstrumentGroup::Keys, "synth here".to_string());
        assert_eq!(format!("{}", cue), "@keys \"synth here\"");
    }
}
