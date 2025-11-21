//! Band Percussion Instruments
//!
//! Percussion instruments commonly used in band, jazz, and popular music settings

use std::fmt;
use serde::{Deserialize, Serialize};

/// Band/popular percussion instruments
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize )]
#[ts(export)]
pub enum PercussionInstrument {
    // Drum kit components
    /// Complete drum set/kit
    DrumSet,
    /// Snare drum
    SnareDrum,
    /// Bass drum (kick drum)
    BassDrum,
    /// Tom-toms (rack and floor toms)
    TomToms,
    /// Hi-hat cymbals
    HiHat,
    /// Ride cymbal
    Ride,
    /// Crash cymbal
    Crash,
    /// Splash cymbal
    Splash,
    /// China cymbal
    China,

    // Orchestral percussion (also used in bands)
    /// Timpani (kettle drums)
    Timpani,
    /// Concert bass drum
    ConcertBassDrum,
    /// Concert snare drum
    ConcertSnareDrum,

    // Mallet percussion
    /// Glockenspiel
    Glockenspiel,
    /// Vibraphone
    Vibraphone,
    /// Marimba
    Marimba,
    /// Xylophone
    Xylophone,
    /// Tubular bells (chimes)
    TubularBells,
    /// Crotales (antique cymbals)
    Crotales,

    // Latin/world percussion
    /// Congas
    Congas,
    /// Bongos
    Bongos,
    /// Timbales
    Timbales,
    /// Cowbell
    Cowbell,
    /// Claves
    Claves,
    /// Maracas
    Maracas,
    /// Guiro
    Guiro,
    /// Cabasa
    Cabasa,

    // Asian percussion
    /// Taiko drums
    TaikoDrums,
    /// Gong
    Gong,
    /// Tam-tam
    TamTam,

    // Auxiliary percussion
    /// Tambourine
    Tambourine,
    /// Triangle
    Triangle,
    /// Wood block
    Woodblock,
    /// Temple blocks
    TempleBlocks,
    /// Sleigh bells
    SleighBells,
    /// Wind chimes
    WindChimes,
    /// Finger cymbals
    FingerCymbals,
    /// Castanets
    Castanets,
    /// Whip (slapstick)
    Whip,
    /// Ratchet
    Ratchet,
    /// Thundersheet
    Thundersheet,

    // Electronic
    /// Electronic drum pad
    ElectronicPad,
    /// Drum machine
    DrumMachine,

    // Generic
    /// Generic percussion (unspecified)
    Percussion,

    // Custom
    /// Custom percussion instrument
    Other(String),
}

impl PercussionInstrument {
    /// Get the orchestral ordering index (0-based)
    pub fn orchestral_index(&self) -> usize {
        match self {
            // Drum kit components (0-6)
            PercussionInstrument::DrumSet => 0,
            PercussionInstrument::SnareDrum => 1,
            PercussionInstrument::BassDrum => 2,
            PercussionInstrument::TomToms => 3,
            PercussionInstrument::HiHat => 4,
            PercussionInstrument::Ride => 5,
            PercussionInstrument::Crash => 6,

            // Orchestral percussion (10-19)
            PercussionInstrument::Timpani => 10,
            PercussionInstrument::ConcertBassDrum => 11,
            PercussionInstrument::ConcertSnareDrum => 12,
            PercussionInstrument::Splash => 13,
            PercussionInstrument::China => 14,

            // Mallet percussion (20-29)
            PercussionInstrument::Glockenspiel => 20,
            PercussionInstrument::Vibraphone => 21,
            PercussionInstrument::Marimba => 22,
            PercussionInstrument::Xylophone => 23,
            PercussionInstrument::TubularBells => 24,
            PercussionInstrument::Crotales => 25,

            // Latin/world percussion (30-49)
            PercussionInstrument::Congas => 30,
            PercussionInstrument::Bongos => 31,
            PercussionInstrument::Timbales => 32,
            PercussionInstrument::Cowbell => 33,
            PercussionInstrument::Claves => 34,
            PercussionInstrument::Maracas => 35,
            PercussionInstrument::Guiro => 36,
            PercussionInstrument::Cabasa => 37,

            // Asian percussion (50-59)
            PercussionInstrument::TaikoDrums => 50,
            PercussionInstrument::Gong => 51,
            PercussionInstrument::TamTam => 52,

            // Auxiliary percussion (60-79)
            PercussionInstrument::Tambourine => 60,
            PercussionInstrument::Triangle => 61,
            PercussionInstrument::Woodblock => 62,
            PercussionInstrument::TempleBlocks => 63,
            PercussionInstrument::SleighBells => 64,
            PercussionInstrument::WindChimes => 65,
            PercussionInstrument::FingerCymbals => 66,
            PercussionInstrument::Castanets => 67,
            PercussionInstrument::Whip => 68,
            PercussionInstrument::Ratchet => 69,
            PercussionInstrument::Thundersheet => 70,

            // Electronic (80-89)
            PercussionInstrument::ElectronicPad => 80,
            PercussionInstrument::DrumMachine => 81,

            // Generic (90-98)
            PercussionInstrument::Percussion => 90,

            // Custom (999)
            PercussionInstrument::Other(_) => 999,
        }
    }

    /// Get all common percussion instruments
    pub fn all() -> Vec<Self> {
        vec![
            // Drum kit
            PercussionInstrument::DrumSet,
            PercussionInstrument::SnareDrum,
            PercussionInstrument::BassDrum,
            PercussionInstrument::TomToms,
            PercussionInstrument::HiHat,
            PercussionInstrument::Ride,
            PercussionInstrument::Crash,

            // Orchestral
            PercussionInstrument::Timpani,
            PercussionInstrument::ConcertBassDrum,
            PercussionInstrument::ConcertSnareDrum,

            // Mallet percussion
            PercussionInstrument::Glockenspiel,
            PercussionInstrument::Vibraphone,
            PercussionInstrument::Marimba,
            PercussionInstrument::Xylophone,
            PercussionInstrument::TubularBells,

            // Latin percussion
            PercussionInstrument::Congas,
            PercussionInstrument::Bongos,
            PercussionInstrument::Timbales,
            PercussionInstrument::Cowbell,

            // Auxiliary
            PercussionInstrument::Tambourine,
            PercussionInstrument::Triangle,
            PercussionInstrument::Woodblock,

            // Asian
            PercussionInstrument::TaikoDrums,
            PercussionInstrument::Gong,
            PercussionInstrument::TamTam,
        ]
    }

    /// Check if this is a pitched percussion instrument
    pub fn is_pitched(&self) -> bool {
        matches!(self,
            PercussionInstrument::Timpani |
            PercussionInstrument::Glockenspiel |
            PercussionInstrument::Vibraphone |
            PercussionInstrument::Marimba |
            PercussionInstrument::Xylophone |
            PercussionInstrument::TubularBells |
            PercussionInstrument::Crotales |
            PercussionInstrument::TomToms // Toms can be tuned to specific pitches
        )
    }

    /// Check if this is typically part of a drum kit
    pub fn is_drum_kit_component(&self) -> bool {
        matches!(self,
            PercussionInstrument::SnareDrum |
            PercussionInstrument::BassDrum |
            PercussionInstrument::TomToms |
            PercussionInstrument::HiHat |
            PercussionInstrument::Ride |
            PercussionInstrument::Crash |
            PercussionInstrument::Splash |
            PercussionInstrument::China
        )
    }

    /// Check if this is a mallet instrument
    pub fn is_mallet_instrument(&self) -> bool {
        matches!(self,
            PercussionInstrument::Glockenspiel |
            PercussionInstrument::Vibraphone |
            PercussionInstrument::Marimba |
            PercussionInstrument::Xylophone |
            PercussionInstrument::Crotales
        )
    }

    /// Get the typical clef used for this instrument
    pub fn typical_clef(&self) -> &'static str {
        match self {
            // Pitched percussion uses standard clefs
            PercussionInstrument::Timpani => "bass",
            PercussionInstrument::Glockenspiel => "treble",
            PercussionInstrument::Vibraphone => "treble",
            PercussionInstrument::Marimba => "treble", // sometimes both
            PercussionInstrument::Xylophone => "treble",
            PercussionInstrument::TubularBells => "treble",
            PercussionInstrument::Crotales => "treble",

            // Most other percussion uses percussion clef
            _ => "percussion",
        }
    }

    /// Parse percussion instrument from text
    pub fn from_string(s: &str) -> Option<Self> {
        let s_lower = s.to_lowercase();
        let s_trimmed = s_lower.trim();

        // Drum kit components
        if s_trimmed.contains("drum set") || s_trimmed.contains("drum kit") ||
           s_trimmed.contains("drumset") || s_trimmed.contains("drums") {
            Some(PercussionInstrument::DrumSet)
        } else if s_trimmed.contains("snare") && !s_trimmed.contains("concert") {
            Some(PercussionInstrument::SnareDrum)
        } else if s_trimmed.contains("kick") || (s_trimmed.contains("bass") && s_trimmed.contains("drum")) {
            if s_trimmed.contains("concert") {
                Some(PercussionInstrument::ConcertBassDrum)
            } else {
                Some(PercussionInstrument::BassDrum)
            }
        } else if s_trimmed.contains("tom") || s_trimmed.contains("toms") {
            Some(PercussionInstrument::TomToms)
        } else if s_trimmed.contains("hi-hat") || s_trimmed.contains("hihat") || s_trimmed.contains("hi hat") {
            Some(PercussionInstrument::HiHat)
        } else if s_trimmed.contains("ride") {
            Some(PercussionInstrument::Ride)
        } else if s_trimmed.contains("crash") {
            Some(PercussionInstrument::Crash)
        } else if s_trimmed.contains("splash") {
            Some(PercussionInstrument::Splash)
        } else if s_trimmed.contains("china") {
            Some(PercussionInstrument::China)

        // Orchestral percussion
        } else if s_trimmed.contains("timpani") || s_trimmed.contains("pauken") {
            Some(PercussionInstrument::Timpani)
        } else if s_trimmed.contains("concert snare") {
            Some(PercussionInstrument::ConcertSnareDrum)

        // Mallet percussion
        } else if s_trimmed.contains("glockenspiel") || s_trimmed.contains("bells") {
            Some(PercussionInstrument::Glockenspiel)
        } else if s_trimmed.contains("vibraphone") || s_trimmed.contains("vibes") {
            Some(PercussionInstrument::Vibraphone)
        } else if s_trimmed.contains("marimba") {
            Some(PercussionInstrument::Marimba)
        } else if s_trimmed.contains("xylophone") {
            Some(PercussionInstrument::Xylophone)
        } else if s_trimmed.contains("tubular bells") || s_trimmed.contains("chimes") {
            Some(PercussionInstrument::TubularBells)
        } else if s_trimmed.contains("crotales") || s_trimmed.contains("antique cymbals") {
            Some(PercussionInstrument::Crotales)

        // Latin percussion
        } else if s_trimmed.contains("congas") {
            Some(PercussionInstrument::Congas)
        } else if s_trimmed.contains("bongos") {
            Some(PercussionInstrument::Bongos)
        } else if s_trimmed.contains("timbales") {
            Some(PercussionInstrument::Timbales)
        } else if s_trimmed.contains("cowbell") {
            Some(PercussionInstrument::Cowbell)
        } else if s_trimmed.contains("claves") {
            Some(PercussionInstrument::Claves)
        } else if s_trimmed.contains("maracas") {
            Some(PercussionInstrument::Maracas)
        } else if s_trimmed.contains("guiro") || s_trimmed.contains("güiro") {
            Some(PercussionInstrument::Guiro)
        } else if s_trimmed.contains("cabasa") {
            Some(PercussionInstrument::Cabasa)

        // Asian percussion
        } else if s_trimmed.contains("taiko") {
            Some(PercussionInstrument::TaikoDrums)
        } else if s_trimmed.contains("gong") {
            Some(PercussionInstrument::Gong)
        } else if s_trimmed.contains("tam tam") || s_trimmed.contains("tamtam") || s_trimmed.contains("tam-tam") {
            Some(PercussionInstrument::TamTam)

        // Auxiliary percussion
        } else if s_trimmed.contains("tambourine") {
            Some(PercussionInstrument::Tambourine)
        } else if s_trimmed.contains("triangle") {
            Some(PercussionInstrument::Triangle)
        } else if s_trimmed.contains("wood block") || s_trimmed.contains("woodblock") {
            Some(PercussionInstrument::Woodblock)
        } else if s_trimmed.contains("temple blocks") {
            Some(PercussionInstrument::TempleBlocks)
        } else if s_trimmed.contains("sleigh bells") {
            Some(PercussionInstrument::SleighBells)
        } else if s_trimmed.contains("wind chimes") || s_trimmed.contains("windchimes") {
            Some(PercussionInstrument::WindChimes)
        } else if s_trimmed.contains("finger cymbals") {
            Some(PercussionInstrument::FingerCymbals)
        } else if s_trimmed.contains("castanets") {
            Some(PercussionInstrument::Castanets)
        } else if s_trimmed.contains("whip") || s_trimmed.contains("slapstick") {
            Some(PercussionInstrument::Whip)
        } else if s_trimmed.contains("ratchet") {
            Some(PercussionInstrument::Ratchet)
        } else if s_trimmed.contains("thundersheet") || s_trimmed.contains("thunder sheet") {
            Some(PercussionInstrument::Thundersheet)

        // Electronic
        } else if s_trimmed.contains("electronic") && s_trimmed.contains("pad") {
            Some(PercussionInstrument::ElectronicPad)
        } else if s_trimmed.contains("drum machine") {
            Some(PercussionInstrument::DrumMachine)

        // Generic
        } else if s_trimmed.contains("percussion") || s_trimmed.contains("perc") ||
                 s_trimmed.contains("schlagzeug") || s_trimmed.contains("perkussion") {
            Some(PercussionInstrument::Percussion)
        } else {
            None
        }
    }
}

impl fmt::Display for PercussionInstrument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // Drum kit
            PercussionInstrument::DrumSet => write!(f, "Drum Set"),
            PercussionInstrument::SnareDrum => write!(f, "Snare Drum"),
            PercussionInstrument::BassDrum => write!(f, "Bass Drum"),
            PercussionInstrument::TomToms => write!(f, "Tom-Toms"),
            PercussionInstrument::HiHat => write!(f, "Hi-Hat"),
            PercussionInstrument::Ride => write!(f, "Ride Cymbal"),
            PercussionInstrument::Crash => write!(f, "Crash Cymbal"),
            PercussionInstrument::Splash => write!(f, "Splash Cymbal"),
            PercussionInstrument::China => write!(f, "China Cymbal"),

            // Orchestral
            PercussionInstrument::Timpani => write!(f, "Timpani"),
            PercussionInstrument::ConcertBassDrum => write!(f, "Concert Bass Drum"),
            PercussionInstrument::ConcertSnareDrum => write!(f, "Concert Snare Drum"),

            // Mallet percussion
            PercussionInstrument::Glockenspiel => write!(f, "Glockenspiel"),
            PercussionInstrument::Vibraphone => write!(f, "Vibraphone"),
            PercussionInstrument::Marimba => write!(f, "Marimba"),
            PercussionInstrument::Xylophone => write!(f, "Xylophone"),
            PercussionInstrument::TubularBells => write!(f, "Tubular Bells"),
            PercussionInstrument::Crotales => write!(f, "Crotales"),

            // Latin percussion
            PercussionInstrument::Congas => write!(f, "Congas"),
            PercussionInstrument::Bongos => write!(f, "Bongos"),
            PercussionInstrument::Timbales => write!(f, "Timbales"),
            PercussionInstrument::Cowbell => write!(f, "Cowbell"),
            PercussionInstrument::Claves => write!(f, "Claves"),
            PercussionInstrument::Maracas => write!(f, "Maracas"),
            PercussionInstrument::Guiro => write!(f, "Güiro"),
            PercussionInstrument::Cabasa => write!(f, "Cabasa"),

            // Asian percussion
            PercussionInstrument::TaikoDrums => write!(f, "Taiko Drums"),
            PercussionInstrument::Gong => write!(f, "Gong"),
            PercussionInstrument::TamTam => write!(f, "Tam-Tam"),

            // Auxiliary
            PercussionInstrument::Tambourine => write!(f, "Tambourine"),
            PercussionInstrument::Triangle => write!(f, "Triangle"),
            PercussionInstrument::Woodblock => write!(f, "Woodblock"),
            PercussionInstrument::TempleBlocks => write!(f, "Temple Blocks"),
            PercussionInstrument::SleighBells => write!(f, "Sleigh Bells"),
            PercussionInstrument::WindChimes => write!(f, "Wind Chimes"),
            PercussionInstrument::FingerCymbals => write!(f, "Finger Cymbals"),
            PercussionInstrument::Castanets => write!(f, "Castanets"),
            PercussionInstrument::Whip => write!(f, "Whip"),
            PercussionInstrument::Ratchet => write!(f, "Ratchet"),
            PercussionInstrument::Thundersheet => write!(f, "Thundersheet"),

            // Electronic
            PercussionInstrument::ElectronicPad => write!(f, "Electronic Pad"),
            PercussionInstrument::DrumMachine => write!(f, "Drum Machine"),

            // Generic
            PercussionInstrument::Percussion => write!(f, "Percussion"),

            // Custom
            PercussionInstrument::Other(name) => write!(f, "{}", name),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_percussion_parsing() {
        assert_eq!(
            PercussionInstrument::from_string("Drum Set"),
            Some(PercussionInstrument::DrumSet)
        );
        assert_eq!(
            PercussionInstrument::from_string("Snare Drum"),
            Some(PercussionInstrument::SnareDrum)
        );
        assert_eq!(
            PercussionInstrument::from_string("Hi-Hat"),
            Some(PercussionInstrument::HiHat)
        );
        assert_eq!(
            PercussionInstrument::from_string("Timpani"),
            Some(PercussionInstrument::Timpani)
        );
        assert_eq!(
            PercussionInstrument::from_string("Vibraphone"),
            Some(PercussionInstrument::Vibraphone)
        );
        assert_eq!(
            PercussionInstrument::from_string("Congas"),
            Some(PercussionInstrument::Congas)
        );
        assert_eq!(PercussionInstrument::from_string("Piano"), None);
    }

    #[test]
    fn test_percussion_display() {
        assert_eq!(format!("{}", PercussionInstrument::DrumSet), "Drum Set");
        assert_eq!(format!("{}", PercussionInstrument::SnareDrum), "Snare Drum");
        assert_eq!(format!("{}", PercussionInstrument::Vibraphone), "Vibraphone");
        assert_eq!(format!("{}", PercussionInstrument::TaikoDrums), "Taiko Drums");
    }

    #[test]
    fn test_pitched_percussion() {
        assert!(PercussionInstrument::Timpani.is_pitched());
        assert!(PercussionInstrument::Vibraphone.is_pitched());
        assert!(PercussionInstrument::Marimba.is_pitched());
        assert!(!PercussionInstrument::SnareDrum.is_pitched());
        assert!(!PercussionInstrument::Triangle.is_pitched());
    }

    #[test]
    fn test_drum_kit_components() {
        assert!(PercussionInstrument::SnareDrum.is_drum_kit_component());
        assert!(PercussionInstrument::BassDrum.is_drum_kit_component());
        assert!(PercussionInstrument::HiHat.is_drum_kit_component());
        assert!(!PercussionInstrument::Timpani.is_drum_kit_component());
        assert!(!PercussionInstrument::Triangle.is_drum_kit_component());
    }

    #[test]
    fn test_mallet_instruments() {
        assert!(PercussionInstrument::Vibraphone.is_mallet_instrument());
        assert!(PercussionInstrument::Marimba.is_mallet_instrument());
        assert!(PercussionInstrument::Xylophone.is_mallet_instrument());
        assert!(!PercussionInstrument::SnareDrum.is_mallet_instrument());
        assert!(!PercussionInstrument::Triangle.is_mallet_instrument());
    }

    #[test]
    fn test_typical_clefs() {
        assert_eq!(PercussionInstrument::Timpani.typical_clef(), "bass");
        assert_eq!(PercussionInstrument::Glockenspiel.typical_clef(), "treble");
        assert_eq!(PercussionInstrument::SnareDrum.typical_clef(), "percussion");
        assert_eq!(PercussionInstrument::Triangle.typical_clef(), "percussion");
    }

    #[test]
    fn test_all_percussion() {
        let all = PercussionInstrument::all();
        assert!(!all.is_empty());
        assert!(all.len() > 20);

        // Check we have representatives from different categories
        let has_drum_kit = all.iter().any(|i| i.is_drum_kit_component());
        let has_mallet = all.iter().any(|i| i.is_mallet_instrument());
        let has_pitched = all.iter().any(|i| i.is_pitched());

        assert!(has_drum_kit);
        assert!(has_mallet);
        assert!(has_pitched);
    }
}
