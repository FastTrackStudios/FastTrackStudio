//! Harp Instrument
//!
//! The harp is a unique orchestral instrument that stands apart from other string instruments
//! due to its plucked strings, pedal mechanism, and unique playing techniques.

use std::fmt;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

/// Harp instrument (separate from bowed strings due to unique characteristics)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, TS)]
#[ts(export)]
pub enum HarpInstrument {
    /// Concert harp (pedal harp)
    Harp,
    /// Celtic harp (lever harp)
    CelticHarp,
    /// Electric harp
    ElectricHarp,
}

impl fmt::Display for HarpInstrument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HarpInstrument::Harp => write!(f, "Harp"),
            HarpInstrument::CelticHarp => write!(f, "Celtic Harp"),
            HarpInstrument::ElectricHarp => write!(f, "Electric Harp"),
        }
    }
}

impl HarpInstrument {
    /// Get all harp instruments
    pub fn all() -> Vec<Self> {
        vec![
            HarpInstrument::Harp,
            HarpInstrument::CelticHarp,
            HarpInstrument::ElectricHarp,
        ]
    }

    /// Get the orchestral ordering index (0-based)
    /// Order: Harp (0), CelticHarp (1), ElectricHarp (2)
    pub fn orchestral_index(&self) -> usize {
        *self as usize
    }

    /// Get the typical range in semitones above C0
    pub fn typical_range(&self) -> (u8, u8) {
        match self {
            HarpInstrument::Harp => (23, 103),        // B0 to G7 (47 strings)
            HarpInstrument::CelticHarp => (36, 84),   // C2 to C6 (varies by size)
            HarpInstrument::ElectricHarp => (23, 103), // Similar to concert harp
        }
    }

    /// Get the clef typically used for this instrument
    pub fn typical_clef(&self) -> &'static str {
        match self {
            HarpInstrument::Harp => "grand_staff", // Uses both treble and bass clef
            HarpInstrument::CelticHarp => "treble", // Usually treble clef
            HarpInstrument::ElectricHarp => "grand_staff",
        }
    }

    /// Get the number of pedals (for pedal harps)
    pub fn pedal_count(&self) -> Option<u8> {
        match self {
            HarpInstrument::Harp => Some(7), // 7 pedals for 7 note names
            HarpInstrument::CelticHarp => None, // Uses levers instead
            HarpInstrument::ElectricHarp => Some(7),
        }
    }

    /// Get typical string count
    pub fn string_count(&self) -> (u8, u8) {
        match self {
            HarpInstrument::Harp => (46, 47),         // Concert harp: 46-47 strings
            HarpInstrument::CelticHarp => (22, 38),   // Celtic harp: varies widely
            HarpInstrument::ElectricHarp => (46, 47), // Similar to concert harp
        }
    }

    /// Parse harp instrument from text
    pub fn from_string(s: &str) -> Option<Self> {
        let s_lower = s.to_lowercase();
        let s_trimmed = s_lower.trim();

        if s_trimmed.contains("celtic") || s_trimmed.contains("irish") || s_trimmed.contains("lever") {
            Some(HarpInstrument::CelticHarp)
        } else if s_trimmed.contains("electric") || s_trimmed.contains("amplified") {
            Some(HarpInstrument::ElectricHarp)
        } else if s_trimmed.contains("harp") || s_trimmed.contains("harfe") {
            Some(HarpInstrument::Harp) // Default to concert harp
        } else {
            None
        }
    }

    /// Check if this harp type uses pedals
    pub fn uses_pedals(&self) -> bool {
        matches!(self, HarpInstrument::Harp | HarpInstrument::ElectricHarp)
    }

    /// Check if this harp type uses levers
    pub fn uses_levers(&self) -> bool {
        matches!(self, HarpInstrument::CelticHarp)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_harp_instrument_parsing() {
        assert_eq!(HarpInstrument::from_string("Harp"), Some(HarpInstrument::Harp));
        assert_eq!(HarpInstrument::from_string("Harfe"), Some(HarpInstrument::Harp));
        assert_eq!(
            HarpInstrument::from_string("Celtic Harp"),
            Some(HarpInstrument::CelticHarp)
        );
        assert_eq!(
            HarpInstrument::from_string("Irish Harp"),
            Some(HarpInstrument::CelticHarp)
        );
        assert_eq!(
            HarpInstrument::from_string("Electric Harp"),
            Some(HarpInstrument::ElectricHarp)
        );
        assert_eq!(HarpInstrument::from_string("Piano"), None);
    }

    #[test]
    fn test_harp_instrument_display() {
        assert_eq!(format!("{}", HarpInstrument::Harp), "Harp");
        assert_eq!(format!("{}", HarpInstrument::CelticHarp), "Celtic Harp");
        assert_eq!(format!("{}", HarpInstrument::ElectricHarp), "Electric Harp");
    }

    #[test]
    fn test_pedal_count() {
        assert_eq!(HarpInstrument::Harp.pedal_count(), Some(7));
        assert_eq!(HarpInstrument::CelticHarp.pedal_count(), None);
        assert_eq!(HarpInstrument::ElectricHarp.pedal_count(), Some(7));
    }

    #[test]
    fn test_string_count() {
        let (min, max) = HarpInstrument::Harp.string_count();
        assert!(max >= min);
        assert!(min >= 40); // Concert harps have at least 40+ strings

        let (celtic_min, celtic_max) = HarpInstrument::CelticHarp.string_count();
        assert!(celtic_max >= celtic_min);
        assert!(celtic_min >= 20); // Celtic harps vary but at least 20+
    }

    #[test]
    fn test_pedals_vs_levers() {
        assert!(HarpInstrument::Harp.uses_pedals());
        assert!(!HarpInstrument::Harp.uses_levers());

        assert!(!HarpInstrument::CelticHarp.uses_pedals());
        assert!(HarpInstrument::CelticHarp.uses_levers());

        assert!(HarpInstrument::ElectricHarp.uses_pedals());
        assert!(!HarpInstrument::ElectricHarp.uses_levers());
    }

    #[test]
    fn test_all_harps() {
        let all = HarpInstrument::all();
        assert_eq!(all.len(), 3);
        assert!(all.contains(&HarpInstrument::Harp));
        assert!(all.contains(&HarpInstrument::CelticHarp));
        assert!(all.contains(&HarpInstrument::ElectricHarp));
    }

    #[test]
    fn test_typical_ranges() {
        let (low, high) = HarpInstrument::Harp.typical_range();
        assert!(high > low);
        assert!(high - low > 70); // Concert harp has wide range (6+ octaves)

        let (celtic_low, celtic_high) = HarpInstrument::CelticHarp.typical_range();
        assert!(celtic_high > celtic_low);
        // Celtic harp typically has smaller range than concert harp
        assert!((celtic_high - celtic_low) <= (high - low));
    }
}
