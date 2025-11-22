//! Chart Commands
//!
//! Special commands that can be applied to chords, melodies, or rhythms
//! Commands can be specified with slash syntax (/fermata) or shorthand (->)

use std::fmt;

/// Commands that can be applied to musical elements
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Command {
    /// Fermata - hold the note/chord longer
    Fermata,

    /// Accent - emphasize the note/chord
    Accent,
}

impl Command {
    /// Parse a command from a slash notation (e.g., "/fermata", "/accent")
    pub fn parse_slash(text: &str) -> Option<Self> {
        let text = text.trim().trim_start_matches('/').trim().to_lowercase();

        match text.as_str() {
            "fermata" => Some(Command::Fermata),
            "accent" => Some(Command::Accent),
            _ => None,
        }
    }

    /// Get the symbol representation of this command
    pub fn symbol(&self) -> &'static str {
        match self {
            Command::Fermata => "𝄐", // Unicode fermata symbol
            Command::Accent => ">",  // Accent symbol
        }
    }

    /// Get the display name of this command
    pub fn name(&self) -> &'static str {
        match self {
            Command::Fermata => "fermata",
            Command::Accent => "accent",
        }
    }
}

impl fmt::Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.symbol())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_fermata() {
        assert_eq!(Command::parse_slash("/fermata"), Some(Command::Fermata));
        assert_eq!(Command::parse_slash("/FERMATA"), Some(Command::Fermata));
        assert_eq!(Command::parse_slash("  /fermata  "), Some(Command::Fermata));
    }

    #[test]
    fn test_parse_accent() {
        assert_eq!(Command::parse_slash("/accent"), Some(Command::Accent));
        assert_eq!(Command::parse_slash("/ACCENT"), Some(Command::Accent));
    }

    #[test]
    fn test_parse_invalid() {
        assert_eq!(Command::parse_slash("/unknown"), None);
        assert_eq!(Command::parse_slash("not_a_command"), None);
    }

    #[test]
    fn test_symbols() {
        assert_eq!(Command::Fermata.symbol(), "𝄐");
        assert_eq!(Command::Accent.symbol(), ">");
    }

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", Command::Fermata), "𝄐");
        assert_eq!(format!("{}", Command::Accent), ">");
    }
}
