//! Domain-specific token types for musical primitives
//!
//! These tokens represent musical concepts that are recognized and validated
//! by specialized parsers/lexers in different musical contexts.

use std::fmt::Display;
use super::note::Modifier;

/// Musical token types - these are semantic tokens identified by context-aware parsers
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MusicalToken {
    /// A validated note name: C, C#, Db, etc.
    NoteName(String),
    
    /// A validated scale/mode name: major, minor, dorian, etc.
    ScaleName(String),
    
    /// A scale degree (1-7)
    ScaleDegree(u8),
    
    /// An extension number (9, 11, 13, etc.)
    Extension(String),
    
    /// An accidental modifier: Sharp, Flat, DoubleSharp, DoubleFlat
    Accidental(Modifier),
}

impl Display for MusicalToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MusicalToken::NoteName(name) => f.write_str(name),
            MusicalToken::ScaleName(name) => f.write_str(name),
            MusicalToken::ScaleDegree(deg) => write!(f, "{}", deg),
            MusicalToken::Extension(ext) => f.write_str(ext),
            MusicalToken::Accidental(modifier) => write!(f, "{}", modifier),
        }
    }
}

impl MusicalToken {
    /// Check if a string is a valid note name
    pub fn is_valid_note_name(s: &str) -> bool {
        matches!(
            s.to_uppercase().as_str(),
            "A" | "B" | "C" | "D" | "E" | "F" | "G" |
            "A#" | "B#" | "C#" | "D#" | "E#" | "F#" | "G#" |
            "AB" | "BB" | "CB" | "DB" | "EB" | "FB" | "GB" |
            "A♯" | "B♯" | "C♯" | "D♯" | "E♯" | "F♯" | "G♯" |
            "A♭" | "B♭" | "C♭" | "D♭" | "E♭" | "F♭" | "G♭"
        )
    }
    
    /// Normalize note name (convert 'b' to flat, handle case)
    pub fn normalize_note_name(s: &str) -> String {
        let upper = s.to_uppercase();
        // Handle 'Ab' vs 'AB' (note B)
        if upper.len() == 2 && upper.ends_with('B') && upper.chars().nth(0).unwrap() != 'B' {
            // This is a flat note like Ab, Db, etc.
            format!("{}b", upper.chars().nth(0).unwrap())
        } else {
            upper.replace('♯', "#").replace('♭', "b")
        }
    }
    
    /// Parse a note name from string if valid
    pub fn parse_note_name(s: &str) -> Option<MusicalToken> {
        let normalized = Self::normalize_note_name(s);
        if Self::is_valid_note_name(&normalized) {
            Some(MusicalToken::NoteName(normalized))
        } else {
            None
        }
    }
    
    /// Check if a string is a valid scale name
    pub fn is_valid_scale_name(s: &str) -> bool {
        matches!(
            s.to_lowercase().as_str(),
            // Diatonic modes
            "major" | "ionian" | "minor" | "aeolian" | "dorian" | 
            "phrygian" | "lydian" | "mixolydian" | "locrian" |
            // Melodic minor modes
            "melodicminor" | "melodic_minor" | "dorianb2" | "dorian_b2" |
            "lydianaugmented" | "lydian_augmented" | "lydiandominant" | "lydian_dominant" |
            "mixolydianb6" | "mixolydian_b6" | "aeolianb5" | "aeolian_b5" |
            "superlocrian" | "super_locrian" | "altered" |
            // Harmonic minor modes
            "harmonicminor" | "harmonic_minor" | "locriannat6" | "locrian_nat_6" |
            "ionianaugmented" | "ionian_augmented" | "romaniandorian" | "romanian_dorian" |
            "phrygiandominant" | "phrygian_dominant" | "lydian#9" | "lydian_sharp_9" |
            "ultralocrian" | "ultra_locrian"
        )
    }
    
    /// Parse a scale name from string if valid
    pub fn parse_scale_name(s: &str) -> Option<MusicalToken> {
        if Self::is_valid_scale_name(s) {
            Some(MusicalToken::ScaleName(s.to_lowercase()))
        } else {
            None
        }
    }
    
    /// Parse an accidental from a character
    pub fn parse_accidental(c: char) -> Option<MusicalToken> {
        match c {
            '#' | '♯' => Some(MusicalToken::Accidental(Modifier::Sharp)),
            'b' | '♭' => Some(MusicalToken::Accidental(Modifier::Flat)),
            _ => None,
        }
    }
    
    /// Parse an accidental from a string (handles double sharps/flats)
    pub fn parse_accidental_str(s: &str) -> Option<MusicalToken> {
        match s {
            "#" | "♯" => Some(MusicalToken::Accidental(Modifier::Sharp)),
            "b" | "♭" => Some(MusicalToken::Accidental(Modifier::Flat)),
            "##" | "𝄪" | "x" => Some(MusicalToken::Accidental(Modifier::DSharp)),
            "bb" | "𝄫" => Some(MusicalToken::Accidental(Modifier::DFlat)),
            _ => None,
        }
    }
    
    /// Check if a character is an accidental
    pub fn is_accidental(c: char) -> bool {
        matches!(c, '#' | 'b' | '♯' | '♭' | '𝄪' | '𝄫' | 'x')
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_note_name_validation() {
        assert!(MusicalToken::is_valid_note_name("C"));
        assert!(MusicalToken::is_valid_note_name("C#"));
        assert!(MusicalToken::is_valid_note_name("Db"));
        assert!(MusicalToken::is_valid_note_name("AB")); // A-flat
        assert!(!MusicalToken::is_valid_note_name("H"));
        assert!(!MusicalToken::is_valid_note_name("X#"));
    }
    
    #[test]
    fn test_note_name_normalization() {
        assert_eq!(MusicalToken::normalize_note_name("Ab"), "Ab");
        assert_eq!(MusicalToken::normalize_note_name("AB"), "Ab");
        assert_eq!(MusicalToken::normalize_note_name("c#"), "C#");
        assert_eq!(MusicalToken::normalize_note_name("B"), "B");
        assert_eq!(MusicalToken::normalize_note_name("BB"), "Bb");
    }
    
    #[test]
    fn test_scale_name_validation() {
        assert!(MusicalToken::is_valid_scale_name("major"));
        assert!(MusicalToken::is_valid_scale_name("Major"));
        assert!(MusicalToken::is_valid_scale_name("DORIAN"));
        assert!(MusicalToken::is_valid_scale_name("melodic_minor"));
        assert!(!MusicalToken::is_valid_scale_name("invalid"));
    }
    
    #[test]
    fn test_accidental_parsing() {
        // Single accidentals
        assert_eq!(
            MusicalToken::parse_accidental('#'),
            Some(MusicalToken::Accidental(Modifier::Sharp))
        );
        assert_eq!(
            MusicalToken::parse_accidental('b'),
            Some(MusicalToken::Accidental(Modifier::Flat))
        );
        assert_eq!(
            MusicalToken::parse_accidental('♯'),
            Some(MusicalToken::Accidental(Modifier::Sharp))
        );
        assert_eq!(
            MusicalToken::parse_accidental('♭'),
            Some(MusicalToken::Accidental(Modifier::Flat))
        );
        
        // Double accidentals (from string)
        assert_eq!(
            MusicalToken::parse_accidental_str("##"),
            Some(MusicalToken::Accidental(Modifier::DSharp))
        );
        assert_eq!(
            MusicalToken::parse_accidental_str("bb"),
            Some(MusicalToken::Accidental(Modifier::DFlat))
        );
        assert_eq!(
            MusicalToken::parse_accidental_str("x"),
            Some(MusicalToken::Accidental(Modifier::DSharp))
        );
    }
    
    #[test]
    fn test_is_accidental() {
        assert!(MusicalToken::is_accidental('#'));
        assert!(MusicalToken::is_accidental('b'));
        assert!(MusicalToken::is_accidental('♯'));
        assert!(MusicalToken::is_accidental('♭'));
        assert!(MusicalToken::is_accidental('x'));
        assert!(!MusicalToken::is_accidental('C'));
        assert!(!MusicalToken::is_accidental('5'));
    }
}

