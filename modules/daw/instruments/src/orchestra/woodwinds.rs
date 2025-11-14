//! Orchestral Woodwind Instruments
//!
//! Traditional woodwind instruments used in orchestral settings with transposition support

use std::fmt;
use serde::{Deserialize, Serialize};
use ts_rs::TS;
use crate::Transposition;

/// Orchestral woodwind instruments with transposition support
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, TS)]
#[ts(export)]
pub enum WoodwindInstrument {
    /// Piccolo (concert pitch)
    Piccolo,
    /// Flute (concert pitch)
    Flute,
    /// Oboe (concert pitch)
    Oboe,
    /// English horn (sounds perfect fifth lower)
    EnglishHorn,
    /// Clarinet (typically in Bb, sometimes A)
    Clarinet,
    /// Bass clarinet (typically in Bb)
    BassClarinet,
    /// Bassoon (concert pitch)
    Bassoon,
    /// Contrabassoon (sounds octave lower)
    Contrabassoon,
}

impl WoodwindInstrument {
    /// Get the default transposition for this woodwind
    pub fn default_transposition(&self) -> Transposition {
        match self {
            WoodwindInstrument::Clarinet => Transposition::Bb,
            _ => Transposition::C, // Concert pitch
        }
    }

    /// Get all orchestral woodwind instruments
    pub fn all() -> Vec<Self> {
        vec![
            WoodwindInstrument::Piccolo,
            WoodwindInstrument::Flute,
            WoodwindInstrument::Oboe,
            WoodwindInstrument::EnglishHorn,
            WoodwindInstrument::Clarinet,
            WoodwindInstrument::BassClarinet,
            WoodwindInstrument::Bassoon,
            WoodwindInstrument::Contrabassoon,
        ]
    }

    /// Get the orchestral ordering index (0-based)
    /// Order: Piccolo (0), Flute (1), Oboe (2), English Horn (3), Clarinet (4), Bass Clarinet (5), Bassoon (6), Contrabassoon (7)
    pub fn orchestral_index(&self) -> usize {
        *self as usize
    }

    /// Get the typical range in semitones above C0
    pub fn typical_range(&self) -> (u8, u8) {
        match self {
            WoodwindInstrument::Piccolo => (74, 108),     // D5 to C8
            WoodwindInstrument::Flute => (60, 96),        // C4 to C7
            WoodwindInstrument::Oboe => (58, 91),         // Bb3 to G6
            WoodwindInstrument::EnglishHorn => (52, 84),  // E3 to C6 (sounds F below)
            WoodwindInstrument::Clarinet => (50, 91),     // D3 to G6
            WoodwindInstrument::BassClarinet => (26, 72), // D1 to C5 (sounds major 9th below for Bb)
            WoodwindInstrument::Bassoon => (34, 75),      // Bb1 to Eb5
            WoodwindInstrument::Contrabassoon => (22, 67), // Bb0 to G4 (sounds octave lower)
        }
    }

    /// Get the clef typically used for this instrument
    pub fn typical_clef(&self) -> &'static str {
        match self {
            WoodwindInstrument::Piccolo => "treble",
            WoodwindInstrument::Flute => "treble",
            WoodwindInstrument::Oboe => "treble",
            WoodwindInstrument::EnglishHorn => "treble", // sounds F below
            WoodwindInstrument::Clarinet => "treble",
            WoodwindInstrument::BassClarinet => "treble", // sounds major 9th below for Bb
            WoodwindInstrument::Bassoon => "bass",
            WoodwindInstrument::Contrabassoon => "bass", // sounds octave lower
        }
    }

    /// Parse woodwind instrument from text
    pub fn from_string(s: &str) -> Option<Self> {
        let s_lower = s.to_lowercase();
        let s_trimmed = s_lower.trim();

        if s_trimmed.contains("piccolo") {
            Some(WoodwindInstrument::Piccolo)
        } else if s_trimmed.contains("flute") || s_trimmed.contains("flöte") {
            Some(WoodwindInstrument::Flute)
        } else if s_trimmed.contains("english horn") || s_trimmed.contains("englischhorn") {
            Some(WoodwindInstrument::EnglishHorn)
        } else if s_trimmed.contains("oboe") {
            Some(WoodwindInstrument::Oboe)
        } else if s_trimmed.contains("bass clarinet") || s_trimmed.contains("bassklarinette") {
            Some(WoodwindInstrument::BassClarinet)
        } else if s_trimmed.contains("clarinet") || s_trimmed.contains("klarinette") {
            Some(WoodwindInstrument::Clarinet)
        } else if s_trimmed.contains("contrabassoon") || s_trimmed.contains("kontrafagott") {
            Some(WoodwindInstrument::Contrabassoon)
        } else if s_trimmed.contains("bassoon") || s_trimmed.contains("fagott") {
            Some(WoodwindInstrument::Bassoon)
        } else {
            None
        }
    }
}

impl fmt::Display for WoodwindInstrument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WoodwindInstrument::Piccolo => write!(f, "Piccolo"),
            WoodwindInstrument::Flute => write!(f, "Flute"),
            WoodwindInstrument::Oboe => write!(f, "Oboe"),
            WoodwindInstrument::EnglishHorn => write!(f, "English Horn"),
            WoodwindInstrument::Clarinet => write!(f, "Clarinet"),
            WoodwindInstrument::BassClarinet => write!(f, "Bass Clarinet"),
            WoodwindInstrument::Bassoon => write!(f, "Bassoon"),
            WoodwindInstrument::Contrabassoon => write!(f, "Contrabassoon"),
        }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_woodwind_instrument_parsing() {
        assert_eq!(
            WoodwindInstrument::from_string("Piccolo"),
            Some(WoodwindInstrument::Piccolo)
        );
        assert_eq!(
            WoodwindInstrument::from_string("Flute"),
            Some(WoodwindInstrument::Flute)
        );
        assert_eq!(
            WoodwindInstrument::from_string("Oboe"),
            Some(WoodwindInstrument::Oboe)
        );
        assert_eq!(
            WoodwindInstrument::from_string("Clarinet"),
            Some(WoodwindInstrument::Clarinet)
        );
        assert_eq!(
            WoodwindInstrument::from_string("Bass Clarinet"),
            Some(WoodwindInstrument::BassClarinet)
        );
        assert_eq!(
            WoodwindInstrument::from_string("Bassoon"),
            Some(WoodwindInstrument::Bassoon)
        );
        assert_eq!(
            WoodwindInstrument::from_string("Contrabassoon"),
            Some(WoodwindInstrument::Contrabassoon)
        );

        assert_eq!(WoodwindInstrument::from_string("Piano"), None);
    }

    #[test]
    fn test_woodwind_instrument_display() {
        assert_eq!(format!("{}", WoodwindInstrument::Piccolo), "Piccolo");
        assert_eq!(format!("{}", WoodwindInstrument::Flute), "Flute");
        assert_eq!(format!("{}", WoodwindInstrument::Clarinet), "Clarinet");
        assert_eq!(format!("{}", WoodwindInstrument::Bassoon), "Bassoon");
    }

    #[test]
    fn test_default_transpositions() {
        assert_eq!(WoodwindInstrument::Clarinet.default_transposition(), Transposition::Bb);
        assert_eq!(WoodwindInstrument::Flute.default_transposition(), Transposition::C);
        assert_eq!(WoodwindInstrument::Oboe.default_transposition(), Transposition::C);
    }

    #[test]
    fn test_all_woodwinds() {
        let all = WoodwindInstrument::all();
        assert_eq!(all.len(), 8);

        // Check we have the main orchestral woodwinds
        assert!(all.contains(&WoodwindInstrument::Piccolo));
        assert!(all.contains(&WoodwindInstrument::Flute));
        assert!(all.contains(&WoodwindInstrument::Oboe));
        assert!(all.contains(&WoodwindInstrument::EnglishHorn));
        assert!(all.contains(&WoodwindInstrument::Clarinet));
        assert!(all.contains(&WoodwindInstrument::BassClarinet));
        assert!(all.contains(&WoodwindInstrument::Bassoon));
        assert!(all.contains(&WoodwindInstrument::Contrabassoon));
    }

    #[test]
    fn test_typical_ranges() {
        let (low, high) = WoodwindInstrument::Piccolo.typical_range();
        assert!(high > low);

        // Piccolo should have higher range than contrabassoon
        let (contra_low, contra_high) = WoodwindInstrument::Contrabassoon.typical_range();
        assert!(low > contra_low);
        assert!(high > contra_high);
    }

    #[test]
    fn test_typical_clefs() {
        assert_eq!(WoodwindInstrument::Flute.typical_clef(), "treble");
        assert_eq!(WoodwindInstrument::Clarinet.typical_clef(), "treble");
        assert_eq!(WoodwindInstrument::Bassoon.typical_clef(), "bass");
        assert_eq!(WoodwindInstrument::Contrabassoon.typical_clef(), "bass");
    }
}
