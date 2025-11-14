//! Vocal Parts and Voice Types
//!
//! Voice classifications for solo and choral singing

use std::fmt;
use serde::{Deserialize, Serialize};
use ts_rs::TS;

/// Vocal parts and voice types
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, TS)]
#[ts(export)]
pub enum Vocal {
    /// Soprano voice (highest female voice)
    Soprano,
    /// Mezzo-soprano voice (middle female voice)
    MezzoSoprano,
    /// Alto voice (lowest female voice)
    Alto,
    /// Countertenor voice (highest male voice, falsetto)
    Countertenor,
    /// Tenor voice (highest natural male voice)
    Tenor,
    /// Baritone voice (middle male voice)
    Baritone,
    /// Bass voice (lowest male voice)
    Bass,
    /// Mixed choir (SATB)
    Choir,
    /// Children's choir
    ChildrensChoir,
    /// Women's choir (SSA or similar)
    WomensChoir,
    /// Men's choir (TTBB or similar)
    MensChoir,
    /// Vocal soloist (unspecified voice type)
    Soloist,
}

impl fmt::Display for Vocal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Vocal::Soprano => write!(f, "Soprano"),
            Vocal::MezzoSoprano => write!(f, "Mezzo-Soprano"),
            Vocal::Alto => write!(f, "Alto"),
            Vocal::Countertenor => write!(f, "Countertenor"),
            Vocal::Tenor => write!(f, "Tenor"),
            Vocal::Baritone => write!(f, "Baritone"),
            Vocal::Bass => write!(f, "Bass"),
            Vocal::Choir => write!(f, "Choir"),
            Vocal::ChildrensChoir => write!(f, "Children's Choir"),
            Vocal::WomensChoir => write!(f, "Women's Choir"),
            Vocal::MensChoir => write!(f, "Men's Choir"),
            Vocal::Soloist => write!(f, "Vocal Soloist"),
        }
    }
}

impl Vocal {
    /// Get all vocal types
    pub fn all() -> Vec<Self> {
        vec![
            Vocal::Soprano,
            Vocal::MezzoSoprano,
            Vocal::Alto,
            Vocal::Countertenor,
            Vocal::Tenor,
            Vocal::Baritone,
            Vocal::Bass,
            Vocal::Choir,
            Vocal::ChildrensChoir,
            Vocal::WomensChoir,
            Vocal::MensChoir,
            Vocal::Soloist,
        ]
    }

    /// Get the orchestral ordering index (0-based)
    /// Order: Soprano (0), Mezzo-Soprano (1), Alto (2), Countertenor (3), Tenor (4), Baritone (5), Bass (6), etc.
    pub fn orchestral_index(&self) -> usize {
        *self as usize
    }

    /// Check if this is a solo voice type
    pub fn is_solo(&self) -> bool {
        matches!(
            self,
            Vocal::Soprano |
            Vocal::MezzoSoprano |
            Vocal::Alto |
            Vocal::Countertenor |
            Vocal::Tenor |
            Vocal::Baritone |
            Vocal::Bass |
            Vocal::Soloist
        )
    }

    /// Check if this is a choir/ensemble
    pub fn is_ensemble(&self) -> bool {
        matches!(
            self,
            Vocal::Choir |
            Vocal::ChildrensChoir |
            Vocal::WomensChoir |
            Vocal::MensChoir
        )
    }

    /// Get the typical range in semitones above C0
    pub fn typical_range(&self) -> (u8, u8) {
        match self {
            Vocal::Soprano => (60, 81),        // C4 to A5
            Vocal::MezzoSoprano => (57, 77),   // A3 to F5
            Vocal::Alto => (53, 72),           // F3 to C5
            Vocal::Countertenor => (55, 76),   // G3 to E5
            Vocal::Tenor => (48, 69),          // C3 to A4
            Vocal::Baritone => (45, 65),       // A2 to F4
            Vocal::Bass => (40, 60),           // E2 to C4
            Vocal::Choir => (40, 81),          // Full SATB range
            Vocal::ChildrensChoir => (60, 84), // C4 to C6
            Vocal::WomensChoir => (53, 81),    // F3 to A5
            Vocal::MensChoir => (40, 69),      // E2 to A4
            Vocal::Soloist => (40, 84),        // Wide range for unspecified voice
        }
    }

    /// Get the typical clef used for notation
    pub fn typical_clef(&self) -> &'static str {
        match self {
            Vocal::Soprano | Vocal::MezzoSoprano | Vocal::ChildrensChoir => "treble",
            Vocal::Alto => "treble", // Sometimes alto clef
            Vocal::Countertenor | Vocal::Tenor => "treble",
            Vocal::Baritone | Vocal::Bass => "bass",
            Vocal::Choir | Vocal::WomensChoir | Vocal::MensChoir | Vocal::Soloist => "treble",
        }
    }

    /// Parse vocal type from text
    pub fn from_string(s: &str) -> Option<Self> {
        let s_lower = s.to_lowercase();
        let s_trimmed = s_lower.trim();

        if s_trimmed.contains("soprano") && !s_trimmed.contains("mezzo") {
            Some(Vocal::Soprano)
        } else if s_trimmed.contains("mezzo") || s_trimmed.contains("mezzo-soprano") {
            Some(Vocal::MezzoSoprano)
        } else if s_trimmed.contains("alto") {
            Some(Vocal::Alto)
        } else if s_trimmed.contains("countertenor") || s_trimmed.contains("counter-tenor") {
            Some(Vocal::Countertenor)
        } else if s_trimmed.contains("tenor") && !s_trimmed.contains("counter") {
            Some(Vocal::Tenor)
        } else if s_trimmed.contains("baritone") {
            Some(Vocal::Baritone)
        } else if s_trimmed.contains("bass") && !s_trimmed.contains("double") {
            Some(Vocal::Bass)
        } else if s_trimmed.contains("children") && s_trimmed.contains("choir") {
            Some(Vocal::ChildrensChoir)
        } else if s_trimmed.contains("women") && s_trimmed.contains("choir") {
            Some(Vocal::WomensChoir)
        } else if s_trimmed.contains("men") && s_trimmed.contains("choir") {
            Some(Vocal::MensChoir)
        } else if s_trimmed.contains("choir") || s_trimmed.contains("chorus") {
            Some(Vocal::Choir)
        } else if s_trimmed.contains("soloist") || s_trimmed.contains("solo") {
            Some(Vocal::Soloist)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vocal_parsing() {
        assert_eq!(Vocal::from_string("Soprano"), Some(Vocal::Soprano));
        assert_eq!(Vocal::from_string("Mezzo-Soprano"), Some(Vocal::MezzoSoprano));
        assert_eq!(Vocal::from_string("Alto"), Some(Vocal::Alto));
        assert_eq!(Vocal::from_string("Countertenor"), Some(Vocal::Countertenor));
        assert_eq!(Vocal::from_string("Tenor"), Some(Vocal::Tenor));
        assert_eq!(Vocal::from_string("Baritone"), Some(Vocal::Baritone));
        assert_eq!(Vocal::from_string("Bass"), Some(Vocal::Bass));
        assert_eq!(Vocal::from_string("Choir"), Some(Vocal::Choir));
        assert_eq!(Vocal::from_string("Children's Choir"), Some(Vocal::ChildrensChoir));
        assert_eq!(Vocal::from_string("Soloist"), Some(Vocal::Soloist));
        assert_eq!(Vocal::from_string("Piano"), None);
    }

    #[test]
    fn test_vocal_display() {
        assert_eq!(format!("{}", Vocal::Soprano), "Soprano");
        assert_eq!(format!("{}", Vocal::MezzoSoprano), "Mezzo-Soprano");
        assert_eq!(format!("{}", Vocal::Bass), "Bass");
        assert_eq!(format!("{}", Vocal::ChildrensChoir), "Children's Choir");
    }

    #[test]
    fn test_solo_classification() {
        assert!(Vocal::Soprano.is_solo());
        assert!(Vocal::Tenor.is_solo());
        assert!(Vocal::Bass.is_solo());
        assert!(Vocal::Soloist.is_solo());
        assert!(!Vocal::Choir.is_solo());
        assert!(!Vocal::ChildrensChoir.is_solo());
    }

    #[test]
    fn test_ensemble_classification() {
        assert!(Vocal::Choir.is_ensemble());
        assert!(Vocal::ChildrensChoir.is_ensemble());
        assert!(Vocal::WomensChoir.is_ensemble());
        assert!(Vocal::MensChoir.is_ensemble());
        assert!(!Vocal::Soprano.is_ensemble());
        assert!(!Vocal::Soloist.is_ensemble());
    }

    #[test]
    fn test_typical_clefs() {
        assert_eq!(Vocal::Soprano.typical_clef(), "treble");
        assert_eq!(Vocal::Alto.typical_clef(), "treble");
        assert_eq!(Vocal::Tenor.typical_clef(), "treble");
        assert_eq!(Vocal::Bass.typical_clef(), "bass");
        assert_eq!(Vocal::Baritone.typical_clef(), "bass");
    }

    #[test]
    fn test_all_vocals() {
        let all = Vocal::all();
        assert_eq!(all.len(), 12);
        assert!(all.contains(&Vocal::Soprano));
        assert!(all.contains(&Vocal::Alto));
        assert!(all.contains(&Vocal::Tenor));
        assert!(all.contains(&Vocal::Bass));
        assert!(all.contains(&Vocal::Choir));
    }

    #[test]
    fn test_typical_ranges() {
        let (low, high) = Vocal::Soprano.typical_range();
        assert!(high > low);

        // Soprano should have higher range than bass
        let (bass_low, bass_high) = Vocal::Bass.typical_range();
        assert!(low > bass_low);
        assert!(high > bass_high);

        // Choir should have widest range
        let (choir_low, choir_high) = Vocal::Choir.typical_range();
        assert!(choir_low <= bass_low);
        assert!(choir_high >= high);
    }
}
