//! Keyboard Instruments
//!
//! Keyboard instruments commonly used in bands and popular music

use std::fmt;
use serde::{Deserialize, Serialize};

/// Keyboard instruments
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum KeyboardInstrument {
    /// Acoustic piano
    Piano,
    /// Electric piano (Rhodes, Wurlitzer, etc.)
    ElectricPiano,
    /// Hammond organ or similar
    Organ,
    /// Church or pipe organ
    PipeOrgan,
    /// Electronic synthesizer
    Synthesizer,
    /// Harpsichord
    Harpsichord,
    /// Clavichord
    Clavichord,
    /// Celesta (orchestral keyboard)
    Celesta,
    /// Electronic keyboard (generic)
    Keyboard,
    /// Digital workstation
    Workstation,
    /// Accordion
    Accordion,
    /// Harmonium
    Harmonium,
}

impl fmt::Display for KeyboardInstrument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            KeyboardInstrument::Piano => write!(f, "Piano"),
            KeyboardInstrument::ElectricPiano => write!(f, "Electric Piano"),
            KeyboardInstrument::Organ => write!(f, "Organ"),
            KeyboardInstrument::PipeOrgan => write!(f, "Pipe Organ"),
            KeyboardInstrument::Synthesizer => write!(f, "Synthesizer"),
            KeyboardInstrument::Harpsichord => write!(f, "Harpsichord"),
            KeyboardInstrument::Clavichord => write!(f, "Clavichord"),
            KeyboardInstrument::Celesta => write!(f, "Celesta"),
            KeyboardInstrument::Keyboard => write!(f, "Keyboard"),
            KeyboardInstrument::Workstation => write!(f, "Workstation"),
            KeyboardInstrument::Accordion => write!(f, "Accordion"),
            KeyboardInstrument::Harmonium => write!(f, "Harmonium"),
        }
    }
}

impl KeyboardInstrument {
    /// Get the orchestral ordering index (0-based)
    pub fn orchestral_index(&self) -> usize {
        *self as usize
    }

    /// Get all keyboard instruments
    pub fn all() -> Vec<Self> {
        vec![
            KeyboardInstrument::Piano,
            KeyboardInstrument::ElectricPiano,
            KeyboardInstrument::Organ,
            KeyboardInstrument::PipeOrgan,
            KeyboardInstrument::Synthesizer,
            KeyboardInstrument::Harpsichord,
            KeyboardInstrument::Clavichord,
            KeyboardInstrument::Celesta,
            KeyboardInstrument::Keyboard,
            KeyboardInstrument::Workstation,
            KeyboardInstrument::Accordion,
            KeyboardInstrument::Harmonium,
        ]
    }

    /// Check if this is an electronic instrument
    pub fn is_electronic(&self) -> bool {
        matches!(
            self,
            KeyboardInstrument::ElectricPiano |
            KeyboardInstrument::Synthesizer |
            KeyboardInstrument::Keyboard |
            KeyboardInstrument::Workstation
        )
    }

    /// Check if this is an acoustic instrument
    pub fn is_acoustic(&self) -> bool {
        matches!(
            self,
            KeyboardInstrument::Piano |
            KeyboardInstrument::PipeOrgan |
            KeyboardInstrument::Harpsichord |
            KeyboardInstrument::Clavichord |
            KeyboardInstrument::Celesta |
            KeyboardInstrument::Accordion |
            KeyboardInstrument::Harmonium
        )
    }

    /// Check if this requires amplification
    pub fn requires_amplification(&self) -> bool {
        matches!(
            self,
            KeyboardInstrument::ElectricPiano |
            KeyboardInstrument::Organ |
            KeyboardInstrument::Synthesizer |
            KeyboardInstrument::Keyboard |
            KeyboardInstrument::Workstation
        )
    }

    /// Get the typical range in semitones above C0
    pub fn typical_range(&self) -> (u8, u8) {
        match self {
            KeyboardInstrument::Piano => (21, 108), // A0 to C8
            KeyboardInstrument::ElectricPiano => (21, 108), // A0 to C8
            KeyboardInstrument::Organ => (24, 108), // C1 to C8
            KeyboardInstrument::PipeOrgan => (12, 127), // C0 to G10 (can go very high/low)
            KeyboardInstrument::Synthesizer => (0, 127), // Full MIDI range
            KeyboardInstrument::Harpsichord => (29, 89), // F1 to F6
            KeyboardInstrument::Clavichord => (36, 84), // C2 to C6
            KeyboardInstrument::Celesta => (60, 108), // C4 to C8 (sounds 8va higher)
            KeyboardInstrument::Keyboard => (21, 108), // A0 to C8
            KeyboardInstrument::Workstation => (0, 127), // Full MIDI range
            KeyboardInstrument::Accordion => (48, 96), // C3 to C7
            KeyboardInstrument::Harmonium => (48, 84), // C3 to C6
        }
    }

    /// Get the typical clef used for notation
    pub fn typical_clef(&self) -> &'static str {
        match self {
            // Most keyboards use grand staff (treble and bass)
            _ => "treble", // Simplified - most use both treble and bass clefs
        }
    }

    /// Get the typical number of keys
    pub fn typical_key_count(&self) -> u8 {
        match self {
            KeyboardInstrument::Piano => 88,
            KeyboardInstrument::ElectricPiano => 88,
            KeyboardInstrument::Organ => 61, // Manual keyboards typically 61 keys
            KeyboardInstrument::PipeOrgan => 61, // Per manual
            KeyboardInstrument::Synthesizer => 61,
            KeyboardInstrument::Harpsichord => 61,
            KeyboardInstrument::Clavichord => 50,
            KeyboardInstrument::Celesta => 49,
            KeyboardInstrument::Keyboard => 61,
            KeyboardInstrument::Workstation => 88,
            KeyboardInstrument::Accordion => 41, // Piano keys (treble side)
            KeyboardInstrument::Harmonium => 61,
        }
    }

    /// Parse keyboard instrument from text
    pub fn from_string(s: &str) -> Option<Self> {
        let s_lower = s.to_lowercase();
        let s_trimmed = s_lower.trim();

        if s_trimmed.contains("piano") && !s_trimmed.contains("electric") {
            Some(KeyboardInstrument::Piano)
        } else if s_trimmed.contains("electric piano") || s_trimmed.contains("e-piano") ||
                  s_trimmed.contains("rhodes") || s_trimmed.contains("wurlitzer") {
            Some(KeyboardInstrument::ElectricPiano)
        } else if s_trimmed.contains("pipe organ") || s_trimmed.contains("church organ") {
            Some(KeyboardInstrument::PipeOrgan)
        } else if s_trimmed.contains("organ") && !s_trimmed.contains("pipe") {
            Some(KeyboardInstrument::Organ)
        } else if s_trimmed.contains("synthesizer") || s_trimmed.contains("synth") {
            Some(KeyboardInstrument::Synthesizer)
        } else if s_trimmed.contains("harpsichord") {
            Some(KeyboardInstrument::Harpsichord)
        } else if s_trimmed.contains("clavichord") {
            Some(KeyboardInstrument::Clavichord)
        } else if s_trimmed.contains("celesta") {
            Some(KeyboardInstrument::Celesta)
        } else if s_trimmed.contains("workstation") {
            Some(KeyboardInstrument::Workstation)
        } else if s_trimmed.contains("accordion") {
            Some(KeyboardInstrument::Accordion)
        } else if s_trimmed.contains("harmonium") {
            Some(KeyboardInstrument::Harmonium)
        } else if s_trimmed == "keyboard" || s_trimmed == "keys" || s_trimmed == "kbd" {
            Some(KeyboardInstrument::Keyboard)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keyboard_parsing() {
        assert_eq!(KeyboardInstrument::from_string("Piano"), Some(KeyboardInstrument::Piano));
        assert_eq!(KeyboardInstrument::from_string("Electric Piano"), Some(KeyboardInstrument::ElectricPiano));
        assert_eq!(KeyboardInstrument::from_string("Rhodes"), Some(KeyboardInstrument::ElectricPiano));
        assert_eq!(KeyboardInstrument::from_string("Organ"), Some(KeyboardInstrument::Organ));
        assert_eq!(KeyboardInstrument::from_string("Pipe Organ"), Some(KeyboardInstrument::PipeOrgan));
        assert_eq!(KeyboardInstrument::from_string("Synthesizer"), Some(KeyboardInstrument::Synthesizer));
        assert_eq!(KeyboardInstrument::from_string("Synth"), Some(KeyboardInstrument::Synthesizer));
        assert_eq!(KeyboardInstrument::from_string("Harpsichord"), Some(KeyboardInstrument::Harpsichord));
        assert_eq!(KeyboardInstrument::from_string("Keyboard"), Some(KeyboardInstrument::Keyboard));
        assert_eq!(KeyboardInstrument::from_string("Keys"), Some(KeyboardInstrument::Keyboard));
        assert_eq!(KeyboardInstrument::from_string("Guitar"), None);
    }

    #[test]
    fn test_keyboard_display() {
        assert_eq!(format!("{}", KeyboardInstrument::Piano), "Piano");
        assert_eq!(format!("{}", KeyboardInstrument::ElectricPiano), "Electric Piano");
        assert_eq!(format!("{}", KeyboardInstrument::Synthesizer), "Synthesizer");
        assert_eq!(format!("{}", KeyboardInstrument::Harpsichord), "Harpsichord");
    }

    #[test]
    fn test_electronic_classification() {
        assert!(KeyboardInstrument::ElectricPiano.is_electronic());
        assert!(KeyboardInstrument::Synthesizer.is_electronic());
        assert!(KeyboardInstrument::Keyboard.is_electronic());
        assert!(!KeyboardInstrument::Piano.is_electronic());
        assert!(!KeyboardInstrument::Harpsichord.is_electronic());
    }

    #[test]
    fn test_acoustic_classification() {
        assert!(KeyboardInstrument::Piano.is_acoustic());
        assert!(KeyboardInstrument::Harpsichord.is_acoustic());
        assert!(KeyboardInstrument::PipeOrgan.is_acoustic());
        assert!(!KeyboardInstrument::Synthesizer.is_acoustic());
        assert!(!KeyboardInstrument::Keyboard.is_acoustic());
    }

    #[test]
    fn test_amplification_requirements() {
        assert!(KeyboardInstrument::ElectricPiano.requires_amplification());
        assert!(KeyboardInstrument::Synthesizer.requires_amplification());
        assert!(KeyboardInstrument::Organ.requires_amplification());
        assert!(!KeyboardInstrument::Piano.requires_amplification());
        assert!(!KeyboardInstrument::Harpsichord.requires_amplification());
    }

    #[test]
    fn test_key_counts() {
        assert_eq!(KeyboardInstrument::Piano.typical_key_count(), 88);
        assert_eq!(KeyboardInstrument::Synthesizer.typical_key_count(), 61);
        assert_eq!(KeyboardInstrument::Celesta.typical_key_count(), 49);
        assert_eq!(KeyboardInstrument::Workstation.typical_key_count(), 88);
    }

    #[test]
    fn test_all_keyboards() {
        let all = KeyboardInstrument::all();
        assert_eq!(all.len(), 12);
        assert!(all.contains(&KeyboardInstrument::Piano));
        assert!(all.contains(&KeyboardInstrument::ElectricPiano));
        assert!(all.contains(&KeyboardInstrument::Synthesizer));
        assert!(all.contains(&KeyboardInstrument::Organ));
    }

    #[test]
    fn test_typical_ranges() {
        let (low, high) = KeyboardInstrument::Piano.typical_range();
        assert!(high > low);
        assert_eq!(low, 21); // A0
        assert_eq!(high, 108); // C8

        let (synth_low, synth_high) = KeyboardInstrument::Synthesizer.typical_range();
        assert_eq!(synth_low, 0); // Full MIDI range
        assert_eq!(synth_high, 127);
    }
}
