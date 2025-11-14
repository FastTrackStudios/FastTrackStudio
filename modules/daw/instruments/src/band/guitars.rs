//! Guitar Family Instruments
//!
//! String instruments played by plucking or strumming, commonly used in bands

use std::fmt;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

/// Guitar family instruments
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, TS)]
#[ts(export)]
pub enum GuitarInstrument {
    /// Acoustic guitar with steel strings
    AcousticGuitar,
    /// Electric guitar requiring amplification
    ElectricGuitar,
    /// Classical guitar with nylon strings
    ClassicalGuitar,
    /// Bass guitar (4-6 strings)
    BassGuitar,
    /// Electric bass guitar
    ElectricBass,
    /// Banjo with 4-5 strings
    Banjo,
    /// Mandolin with 8 strings in 4 pairs
    Mandolin,
    /// Ukulele with 4 strings
    Ukulele,
    /// 12-string guitar
    TwelveString,
    /// Steel guitar (lap steel)
    SteelGuitar,
}

impl fmt::Display for GuitarInstrument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GuitarInstrument::AcousticGuitar => write!(f, "Acoustic Guitar"),
            GuitarInstrument::ElectricGuitar => write!(f, "Electric Guitar"),
            GuitarInstrument::ClassicalGuitar => write!(f, "Classical Guitar"),
            GuitarInstrument::BassGuitar => write!(f, "Bass Guitar"),
            GuitarInstrument::ElectricBass => write!(f, "Electric Bass"),
            GuitarInstrument::Banjo => write!(f, "Banjo"),
            GuitarInstrument::Mandolin => write!(f, "Mandolin"),
            GuitarInstrument::Ukulele => write!(f, "Ukulele"),
            GuitarInstrument::TwelveString => write!(f, "12-String Guitar"),
            GuitarInstrument::SteelGuitar => write!(f, "Steel Guitar"),
        }
    }
}

impl GuitarInstrument {
    /// Get the orchestral ordering index (0-based)
    pub fn orchestral_index(&self) -> usize {
        *self as usize
    }

    /// Get all guitar instruments
    pub fn all() -> Vec<Self> {
        vec![
            GuitarInstrument::AcousticGuitar,
            GuitarInstrument::ElectricGuitar,
            GuitarInstrument::ClassicalGuitar,
            GuitarInstrument::BassGuitar,
            GuitarInstrument::ElectricBass,
            GuitarInstrument::Banjo,
            GuitarInstrument::Mandolin,
            GuitarInstrument::Ukulele,
            GuitarInstrument::TwelveString,
            GuitarInstrument::SteelGuitar,
        ]
    }

    /// Get the typical number of strings
    pub fn string_count(&self) -> u8 {
        match self {
            GuitarInstrument::AcousticGuitar => 6,
            GuitarInstrument::ElectricGuitar => 6,
            GuitarInstrument::ClassicalGuitar => 6,
            GuitarInstrument::BassGuitar => 4,
            GuitarInstrument::ElectricBass => 4,
            GuitarInstrument::Banjo => 5,
            GuitarInstrument::Mandolin => 8,
            GuitarInstrument::Ukulele => 4,
            GuitarInstrument::TwelveString => 12,
            GuitarInstrument::SteelGuitar => 6,
        }
    }

    /// Check if this is a bass instrument
    pub fn is_bass(&self) -> bool {
        matches!(self, GuitarInstrument::BassGuitar | GuitarInstrument::ElectricBass)
    }

    /// Check if this requires amplification
    pub fn requires_amplification(&self) -> bool {
        matches!(
            self,
            GuitarInstrument::ElectricGuitar | GuitarInstrument::ElectricBass
        )
    }

    /// Get the typical tuning (in semitones from C0)
    pub fn standard_tuning(&self) -> Vec<u8> {
        match self {
            GuitarInstrument::AcousticGuitar |
            GuitarInstrument::ElectricGuitar |
            GuitarInstrument::ClassicalGuitar => {
                vec![40, 45, 50, 55, 59, 64] // E2, A2, D3, G3, B3, E4
            },
            GuitarInstrument::BassGuitar | GuitarInstrument::ElectricBass => {
                vec![28, 33, 38, 43] // E1, A1, D2, G2
            },
            GuitarInstrument::Banjo => {
                vec![50, 55, 59, 62, 67] // D3, G3, B3, D4, G4 (5-string)
            },
            GuitarInstrument::Mandolin => {
                vec![55, 55, 62, 62, 69, 69, 76, 76] // G3, G3, D4, D4, A4, A4, E5, E5
            },
            GuitarInstrument::Ukulele => {
                vec![67, 60, 64, 69] // G4, C4, E4, A4
            },
            GuitarInstrument::TwelveString => {
                vec![40, 52, 45, 57, 50, 62, 55, 67, 59, 71, 64, 76] // E2, E3, A2, A3, etc.
            },
            GuitarInstrument::SteelGuitar => {
                vec![40, 45, 50, 55, 59, 64] // E2, A2, D3, G3, B3, E4 (varies by tuning)
            },
        }
    }

    /// Get the typical clef used for notation
    pub fn typical_clef(&self) -> &'static str {
        match self {
            GuitarInstrument::BassGuitar | GuitarInstrument::ElectricBass => "bass",
            _ => "treble",
        }
    }

    /// Parse guitar instrument from text
    pub fn from_string(s: &str) -> Option<Self> {
        let s_lower = s.to_lowercase();
        let s_trimmed = s_lower.trim();

        if s_trimmed.contains("acoustic guitar") || s_trimmed.contains("steel string") {
            Some(GuitarInstrument::AcousticGuitar)
        } else if s_trimmed.contains("electric guitar") || s_trimmed == "electric" {
            Some(GuitarInstrument::ElectricGuitar)
        } else if s_trimmed.contains("classical guitar") || s_trimmed.contains("nylon string") {
            Some(GuitarInstrument::ClassicalGuitar)
        } else if s_trimmed.contains("bass guitar") || s_trimmed == "bass" {
            Some(GuitarInstrument::BassGuitar)
        } else if s_trimmed.contains("electric bass") || s_trimmed == "e-bass" {
            Some(GuitarInstrument::ElectricBass)
        } else if s_trimmed.contains("banjo") {
            Some(GuitarInstrument::Banjo)
        } else if s_trimmed.contains("mandolin") {
            Some(GuitarInstrument::Mandolin)
        } else if s_trimmed.contains("ukulele") || s_trimmed.contains("uke") {
            Some(GuitarInstrument::Ukulele)
        } else if s_trimmed.contains("12") && s_trimmed.contains("string") {
            Some(GuitarInstrument::TwelveString)
        } else if s_trimmed.contains("steel guitar") || s_trimmed.contains("lap steel") {
            Some(GuitarInstrument::SteelGuitar)
        } else if s_trimmed == "guitar" || s_trimmed == "gtr" {
            // Default to acoustic guitar for generic "guitar"
            Some(GuitarInstrument::AcousticGuitar)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_guitar_parsing() {
        assert_eq!(GuitarInstrument::from_string("Acoustic Guitar"), Some(GuitarInstrument::AcousticGuitar));
        assert_eq!(GuitarInstrument::from_string("Electric Guitar"), Some(GuitarInstrument::ElectricGuitar));
        assert_eq!(GuitarInstrument::from_string("Classical Guitar"), Some(GuitarInstrument::ClassicalGuitar));
        assert_eq!(GuitarInstrument::from_string("Bass Guitar"), Some(GuitarInstrument::BassGuitar));
        assert_eq!(GuitarInstrument::from_string("Electric Bass"), Some(GuitarInstrument::ElectricBass));
        assert_eq!(GuitarInstrument::from_string("Banjo"), Some(GuitarInstrument::Banjo));
        assert_eq!(GuitarInstrument::from_string("Mandolin"), Some(GuitarInstrument::Mandolin));
        assert_eq!(GuitarInstrument::from_string("Ukulele"), Some(GuitarInstrument::Ukulele));
        assert_eq!(GuitarInstrument::from_string("12-String Guitar"), Some(GuitarInstrument::TwelveString));
        assert_eq!(GuitarInstrument::from_string("Steel Guitar"), Some(GuitarInstrument::SteelGuitar));
        assert_eq!(GuitarInstrument::from_string("Guitar"), Some(GuitarInstrument::AcousticGuitar));
        assert_eq!(GuitarInstrument::from_string("Piano"), None);
    }

    #[test]
    fn test_guitar_display() {
        assert_eq!(format!("{}", GuitarInstrument::AcousticGuitar), "Acoustic Guitar");
        assert_eq!(format!("{}", GuitarInstrument::ElectricGuitar), "Electric Guitar");
        assert_eq!(format!("{}", GuitarInstrument::BassGuitar), "Bass Guitar");
        assert_eq!(format!("{}", GuitarInstrument::Ukulele), "Ukulele");
    }

    #[test]
    fn test_string_counts() {
        assert_eq!(GuitarInstrument::AcousticGuitar.string_count(), 6);
        assert_eq!(GuitarInstrument::BassGuitar.string_count(), 4);
        assert_eq!(GuitarInstrument::Mandolin.string_count(), 8);
        assert_eq!(GuitarInstrument::Ukulele.string_count(), 4);
        assert_eq!(GuitarInstrument::TwelveString.string_count(), 12);
    }

    #[test]
    fn test_bass_classification() {
        assert!(GuitarInstrument::BassGuitar.is_bass());
        assert!(GuitarInstrument::ElectricBass.is_bass());
        assert!(!GuitarInstrument::AcousticGuitar.is_bass());
        assert!(!GuitarInstrument::Mandolin.is_bass());
    }

    #[test]
    fn test_amplification_requirements() {
        assert!(GuitarInstrument::ElectricGuitar.requires_amplification());
        assert!(GuitarInstrument::ElectricBass.requires_amplification());
        assert!(!GuitarInstrument::AcousticGuitar.requires_amplification());
        assert!(!GuitarInstrument::ClassicalGuitar.requires_amplification());
    }

    #[test]
    fn test_typical_clefs() {
        assert_eq!(GuitarInstrument::AcousticGuitar.typical_clef(), "treble");
        assert_eq!(GuitarInstrument::BassGuitar.typical_clef(), "bass");
        assert_eq!(GuitarInstrument::ElectricBass.typical_clef(), "bass");
        assert_eq!(GuitarInstrument::Mandolin.typical_clef(), "treble");
    }

    #[test]
    fn test_all_guitars() {
        let all = GuitarInstrument::all();
        assert_eq!(all.len(), 10);
        assert!(all.contains(&GuitarInstrument::AcousticGuitar));
        assert!(all.contains(&GuitarInstrument::ElectricGuitar));
        assert!(all.contains(&GuitarInstrument::BassGuitar));
        assert!(all.contains(&GuitarInstrument::Ukulele));
    }

    #[test]
    fn test_standard_tuning() {
        let guitar_tuning = GuitarInstrument::AcousticGuitar.standard_tuning();
        assert_eq!(guitar_tuning.len(), 6);

        let bass_tuning = GuitarInstrument::BassGuitar.standard_tuning();
        assert_eq!(bass_tuning.len(), 4);

        let mandolin_tuning = GuitarInstrument::Mandolin.standard_tuning();
        assert_eq!(mandolin_tuning.len(), 8);
    }
}
