//! Orchestral String Instruments
//!
//! Traditional bowed string instruments used in orchestral settings

use std::fmt;
use serde::{Deserialize, Serialize};

/// Orchestral string instruments (bowed strings)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize )]
pub enum StringInstrument {
    /// First violin section
    Violin,
    /// Viola section
    Viola,
    /// Cello section
    Cello,
    /// Double bass section (also called contrabass or upright bass)
    DoubleBass,
}

impl fmt::Display for StringInstrument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StringInstrument::Violin => write!(f, "Violin"),
            StringInstrument::Viola => write!(f, "Viola"),
            StringInstrument::Cello => write!(f, "Cello"),
            StringInstrument::DoubleBass => write!(f, "Double Bass"),
        }
    }
}

impl StringInstrument {
    /// Get all orchestral string instruments
    pub fn all() -> Vec<Self> {
        vec![
            StringInstrument::Violin,
            StringInstrument::Viola,
            StringInstrument::Cello,
            StringInstrument::DoubleBass,
        ]
    }

    /// Get the orchestral ordering index (0-based)
    /// Order: Violin (0), Viola (1), Cello (2), DoubleBass (3)
    pub fn orchestral_index(&self) -> usize {
        *self as usize
    }

    /// Get the typical range in semitones above C0
    pub fn typical_range(&self) -> (u8, u8) {
        match self {
            StringInstrument::Violin => (55, 103), // G3 to G7
            StringInstrument::Viola => (48, 91),   // C3 to G6
            StringInstrument::Cello => (36, 84),   // C2 to C6
            StringInstrument::DoubleBass => (16, 67), // E0 to G4 (sounds octave lower)
        }
    }

    /// Get the clef typically used for this instrument
    pub fn typical_clef(&self) -> &'static str {
        match self {
            StringInstrument::Violin => "treble",
            StringInstrument::Viola => "alto",
            StringInstrument::Cello => "bass", // also tenor for higher passages
            StringInstrument::DoubleBass => "bass",
        }
    }

    /// Parse string instrument from text
    pub fn from_string(s: &str) -> Option<Self> {
        let s_lower = s.to_lowercase();
        let s_trimmed = s_lower.trim();

        if s_trimmed.contains("violin") || s_trimmed.contains("violine") {
            Some(StringInstrument::Violin)
        } else if s_trimmed.contains("viola") {
            Some(StringInstrument::Viola)
        } else if s_trimmed.contains("cello") || s_trimmed.contains("violoncello") {
            Some(StringInstrument::Cello)
        } else if s_trimmed.contains("double bass") || s_trimmed.contains("doublebass") ||
                  s_trimmed.contains("kontrabass") || s_trimmed.contains("upright bass") ||
                  s_trimmed.contains("contrabass") {
            Some(StringInstrument::DoubleBass)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_instrument_parsing() {
        assert_eq!(StringInstrument::from_string("Violin"), Some(StringInstrument::Violin));
        assert_eq!(StringInstrument::from_string("Violine"), Some(StringInstrument::Violin));
        assert_eq!(StringInstrument::from_string("Viola"), Some(StringInstrument::Viola));
        assert_eq!(StringInstrument::from_string("Cello"), Some(StringInstrument::Cello));
        assert_eq!(StringInstrument::from_string("Violoncello"), Some(StringInstrument::Cello));
        assert_eq!(StringInstrument::from_string("Double Bass"), Some(StringInstrument::DoubleBass));
        assert_eq!(StringInstrument::from_string("Kontrabass"), Some(StringInstrument::DoubleBass));
        assert_eq!(StringInstrument::from_string("Piano"), None);
    }

    #[test]
    fn test_string_instrument_display() {
        assert_eq!(format!("{}", StringInstrument::Violin), "Violin");
        assert_eq!(format!("{}", StringInstrument::Viola), "Viola");
        assert_eq!(format!("{}", StringInstrument::Cello), "Cello");
        assert_eq!(format!("{}", StringInstrument::DoubleBass), "Double Bass");
    }

    #[test]
    fn test_typical_clefs() {
        assert_eq!(StringInstrument::Violin.typical_clef(), "treble");
        assert_eq!(StringInstrument::Viola.typical_clef(), "alto");
        assert_eq!(StringInstrument::Cello.typical_clef(), "bass");
        assert_eq!(StringInstrument::DoubleBass.typical_clef(), "bass");
    }

    #[test]
    fn test_all_strings() {
        let all = StringInstrument::all();
        assert_eq!(all.len(), 4);
        assert!(all.contains(&StringInstrument::Violin));
        assert!(all.contains(&StringInstrument::Viola));
        assert!(all.contains(&StringInstrument::Cello));
        assert!(all.contains(&StringInstrument::DoubleBass));
    }

    #[test]
    fn test_typical_ranges() {
        let (low, high) = StringInstrument::Violin.typical_range();
        assert!(high > low);

        // Violin should have higher range than double bass
        let (bass_low, bass_high) = StringInstrument::DoubleBass.typical_range();
        assert!(low > bass_low);
        assert!(high > bass_high);
    }
}
