//! Orchestral Brass Instruments
//!
//! Traditional brass instruments used in orchestral settings with transposition support

use std::fmt;
use serde::{Deserialize, Serialize};
use ts_rs::TS;
use crate::Transposition;

/// Orchestral brass instruments
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, TS)]
#[ts(export)]
pub enum BrassInstrument {
    /// French horn (typically in F)
    Horn,
    /// Trumpet (typically in Bb, sometimes C)
    Trumpet,
    /// Trombone (concert pitch)
    Trombone,
    /// Bass trombone (concert pitch)
    BassTrombone,
    /// Tuba (concert pitch)
    Tuba,
}

impl BrassInstrument {
    /// Get the default transposition for this brass instrument
    pub fn default_transposition(&self) -> Transposition {
        match self {
            BrassInstrument::Horn => Transposition::F,
            BrassInstrument::Trumpet => Transposition::Bb,
            _ => Transposition::C, // Concert pitch
        }
    }

    /// Get all orchestral brass instruments
    pub fn all() -> Vec<Self> {
        vec![
            BrassInstrument::Horn,
            BrassInstrument::Trumpet,
            BrassInstrument::Trombone,
            BrassInstrument::BassTrombone,
            BrassInstrument::Tuba,
        ]
    }

    /// Get the orchestral ordering index (0-based)
    /// Order: Horn (0), Trumpet (1), Trombone (2), Bass Trombone (3), Tuba (4)
    pub fn orchestral_index(&self) -> usize {
        *self as usize
    }

    /// Get the typical range in semitones above C0
    pub fn typical_range(&self) -> (u8, u8) {
        match self {
            BrassInstrument::Horn => (34, 77),        // Bb1 to F5
            BrassInstrument::Trumpet => (52, 82),     // E3 to Bb5
            BrassInstrument::Trombone => (40, 72),    // E2 to C5
            BrassInstrument::BassTrombone => (28, 65), // E1 to F4
            BrassInstrument::Tuba => (16, 58),        // E0 to Bb3
        }
    }

    /// Get the clef typically used for this instrument
    pub fn typical_clef(&self) -> &'static str {
        match self {
            BrassInstrument::Horn => "treble", // Sometimes bass clef for low parts
            BrassInstrument::Trumpet => "treble",
            BrassInstrument::Trombone => "bass", // Sometimes tenor clef
            BrassInstrument::BassTrombone => "bass",
            BrassInstrument::Tuba => "bass",
        }
    }

    /// Parse brass instrument from text
    pub fn from_string(s: &str) -> Option<Self> {
        let s_lower = s.to_lowercase();
        let s_trimmed = s_lower.trim();

        if s_trimmed.contains("french horn") || s_trimmed.contains("horn") && !s_trimmed.contains("english") && !s_trimmed.contains("flugelhorn") {
            Some(BrassInstrument::Horn)
        } else if s_trimmed.contains("bass trombone") || s_trimmed.contains("bassposaune") {
            Some(BrassInstrument::BassTrombone)
        } else if s_trimmed.contains("trombone") || s_trimmed.contains("posaune") {
            Some(BrassInstrument::Trombone)
        } else if s_trimmed.contains("trumpet") || s_trimmed.contains("trompete") {
            Some(BrassInstrument::Trumpet)
        } else if s_trimmed.contains("tuba") {
            Some(BrassInstrument::Tuba)
        } else {
            None
        }
    }

    /// Check if this is a high brass instrument
    pub fn is_high_brass(&self) -> bool {
        matches!(
            self,
            BrassInstrument::Horn |
            BrassInstrument::Trumpet
        )
    }

    /// Check if this is a low brass instrument
    pub fn is_low_brass(&self) -> bool {
        matches!(
            self,
            BrassInstrument::Trombone |
            BrassInstrument::BassTrombone |
            BrassInstrument::Tuba
        )
    }
}

impl fmt::Display for BrassInstrument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BrassInstrument::Horn => write!(f, "Horn"),
            BrassInstrument::Trumpet => write!(f, "Trumpet"),
            BrassInstrument::Trombone => write!(f, "Trombone"),
            BrassInstrument::BassTrombone => write!(f, "Bass Trombone"),
            BrassInstrument::Tuba => write!(f, "Tuba"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_brass_instrument_parsing() {
        assert_eq!(BrassInstrument::from_string("Horn"), Some(BrassInstrument::Horn));
        assert_eq!(BrassInstrument::from_string("French Horn"), Some(BrassInstrument::Horn));
        assert_eq!(BrassInstrument::from_string("Trumpet"), Some(BrassInstrument::Trumpet));
        assert_eq!(BrassInstrument::from_string("Trombone"), Some(BrassInstrument::Trombone));
        assert_eq!(BrassInstrument::from_string("Bass Trombone"), Some(BrassInstrument::BassTrombone));
        assert_eq!(BrassInstrument::from_string("Tuba"), Some(BrassInstrument::Tuba));
        assert_eq!(BrassInstrument::from_string("Piano"), None);
    }

    #[test]
    fn test_brass_instrument_display() {
        assert_eq!(format!("{}", BrassInstrument::Horn), "Horn");
        assert_eq!(format!("{}", BrassInstrument::Trumpet), "Trumpet");
        assert_eq!(format!("{}", BrassInstrument::Trombone), "Trombone");
        assert_eq!(format!("{}", BrassInstrument::BassTrombone), "Bass Trombone");
        assert_eq!(format!("{}", BrassInstrument::Tuba), "Tuba");
    }

    #[test]
    fn test_default_transpositions() {
        assert_eq!(BrassInstrument::Horn.default_transposition(), Transposition::F);
        assert_eq!(BrassInstrument::Trumpet.default_transposition(), Transposition::Bb);
        assert_eq!(BrassInstrument::Trombone.default_transposition(), Transposition::C);
        assert_eq!(BrassInstrument::Tuba.default_transposition(), Transposition::C);
    }

    #[test]
    fn test_high_low_brass_classification() {
        // High brass
        assert!(BrassInstrument::Horn.is_high_brass());
        assert!(BrassInstrument::Trumpet.is_high_brass());

        // Low brass
        assert!(BrassInstrument::Trombone.is_low_brass());
        assert!(BrassInstrument::BassTrombone.is_low_brass());
        assert!(BrassInstrument::Tuba.is_low_brass());

        // Should not be both
        assert!(!BrassInstrument::Horn.is_low_brass());
        assert!(!BrassInstrument::Tuba.is_high_brass());
    }

    #[test]
    fn test_all_brass() {
        let all = BrassInstrument::all();
        assert_eq!(all.len(), 5);
        assert!(all.contains(&BrassInstrument::Horn));
        assert!(all.contains(&BrassInstrument::Trumpet));
        assert!(all.contains(&BrassInstrument::Trombone));
        assert!(all.contains(&BrassInstrument::BassTrombone));
        assert!(all.contains(&BrassInstrument::Tuba));
    }

    #[test]
    fn test_typical_ranges() {
        let (low, high) = BrassInstrument::Trumpet.typical_range();
        assert!(high > low);

        // Trumpet should have higher range than tuba
        let (tuba_low, tuba_high) = BrassInstrument::Tuba.typical_range();
        assert!(low > tuba_low);
        assert!(high > tuba_high);
    }

    #[test]
    fn test_typical_clefs() {
        assert_eq!(BrassInstrument::Trumpet.typical_clef(), "treble");
        assert_eq!(BrassInstrument::Horn.typical_clef(), "treble");
        assert_eq!(BrassInstrument::Trombone.typical_clef(), "bass");
        assert_eq!(BrassInstrument::Tuba.typical_clef(), "bass");
    }
}
