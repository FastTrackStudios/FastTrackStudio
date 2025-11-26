//! World and Ethnic Instruments
//!
//! Traditional instruments from various cultures and regions around the world

use std::fmt;
use serde::{Deserialize, Serialize};

/// World and ethnic instruments from various cultures
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum WorldInstrument {
    // Asian Instruments
    /// Chinese two-stringed bowed instrument
    Erhu,
    /// Chinese plucked zither
    Guzheng,
    /// Japanese bamboo flute
    Shakuhachi,
    /// Japanese three-stringed plucked instrument
    Shamisen,
    /// Japanese thirteen-stringed zither
    Koto,
    /// Japanese taiko drum
    TaikoDrum,
    /// Indian sitar
    Sitar,
    /// Indian tabla drums
    Tabla,
    /// Indian sarod
    Sarod,
    /// Indian tanpura (drone instrument)
    Tanpura,
    /// Indian bansuri flute
    Bansuri,

    // Middle Eastern Instruments
    /// Middle Eastern oud
    Oud,
    /// Armenian duduk
    Duduk,
    /// Middle Eastern ney flute
    Ney,
    /// Middle Eastern darbuka drum
    Darbuka,
    /// Middle Eastern qanun zither
    Qanun,
    /// Middle Eastern frame drum
    FrameDrum,

    // Celtic Instruments
    /// Irish/Scottish fiddle
    CelticFiddle,
    /// Bagpipes
    Bagpipes,
    /// Irish tin whistle
    TinWhistle,
    /// Irish bodhran drum
    Bodhran,
    /// Celtic harp
    CelticHarp,
    /// Irish flute
    IrishFlute,
    /// Uilleann pipes
    UilleanPipes,

    // African Instruments
    /// West African djembe drum
    Djembe,
    /// African thumb piano
    Kalimba,
    /// African talking drum
    TalkingDrum,
    /// African balafon (xylophone)
    Balafon,
    /// African kora (harp-lute)
    Kora,

    // Latin American Instruments
    /// Peruvian cajón
    Cajon,
    /// Maracas shakers
    Maracas,
    /// Cuban claves
    Claves,
    /// Andean panpipes
    Panpipes,
    /// Charango (small guitar-like instrument)
    Charango,
    /// Quena flute
    Quena,
    /// Cajón peruano
    CajonPeruano,

    // European Folk Instruments
    /// Accordion
    Accordion,
    /// Concertina
    Concertina,
    /// Dulcimer
    Dulcimer,
    /// Hurdy-gurdy
    HurdyGurdy,
    /// Alpine zither
    AlpineZither,

    // Other/Generic
    /// Generic world percussion
    WorldPercussion,
    /// Other world instrument not specified
    Other,
}

impl fmt::Display for WorldInstrument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // Asian
            WorldInstrument::Erhu => write!(f, "Erhu"),
            WorldInstrument::Guzheng => write!(f, "Guzheng"),
            WorldInstrument::Shakuhachi => write!(f, "Shakuhachi"),
            WorldInstrument::Shamisen => write!(f, "Shamisen"),
            WorldInstrument::Koto => write!(f, "Koto"),
            WorldInstrument::TaikoDrum => write!(f, "Taiko Drum"),
            WorldInstrument::Sitar => write!(f, "Sitar"),
            WorldInstrument::Tabla => write!(f, "Tabla"),
            WorldInstrument::Sarod => write!(f, "Sarod"),
            WorldInstrument::Tanpura => write!(f, "Tanpura"),
            WorldInstrument::Bansuri => write!(f, "Bansuri"),

            // Middle Eastern
            WorldInstrument::Oud => write!(f, "Oud"),
            WorldInstrument::Duduk => write!(f, "Duduk"),
            WorldInstrument::Ney => write!(f, "Ney"),
            WorldInstrument::Darbuka => write!(f, "Darbuka"),
            WorldInstrument::Qanun => write!(f, "Qanun"),
            WorldInstrument::FrameDrum => write!(f, "Frame Drum"),

            // Celtic
            WorldInstrument::CelticFiddle => write!(f, "Celtic Fiddle"),
            WorldInstrument::Bagpipes => write!(f, "Bagpipes"),
            WorldInstrument::TinWhistle => write!(f, "Tin Whistle"),
            WorldInstrument::Bodhran => write!(f, "Bodhrán"),
            WorldInstrument::CelticHarp => write!(f, "Celtic Harp"),
            WorldInstrument::IrishFlute => write!(f, "Irish Flute"),
            WorldInstrument::UilleanPipes => write!(f, "Uilleann Pipes"),

            // African
            WorldInstrument::Djembe => write!(f, "Djembe"),
            WorldInstrument::Kalimba => write!(f, "Kalimba"),
            WorldInstrument::TalkingDrum => write!(f, "Talking Drum"),
            WorldInstrument::Balafon => write!(f, "Balafon"),
            WorldInstrument::Kora => write!(f, "Kora"),

            // Latin American
            WorldInstrument::Cajon => write!(f, "Cajón"),
            WorldInstrument::Maracas => write!(f, "Maracas"),
            WorldInstrument::Claves => write!(f, "Claves"),
            WorldInstrument::Panpipes => write!(f, "Panpipes"),
            WorldInstrument::Charango => write!(f, "Charango"),
            WorldInstrument::Quena => write!(f, "Quena"),
            WorldInstrument::CajonPeruano => write!(f, "Cajón Peruano"),

            // European Folk
            WorldInstrument::Accordion => write!(f, "Accordion"),
            WorldInstrument::Concertina => write!(f, "Concertina"),
            WorldInstrument::Dulcimer => write!(f, "Dulcimer"),
            WorldInstrument::HurdyGurdy => write!(f, "Hurdy-Gurdy"),
            WorldInstrument::AlpineZither => write!(f, "Alpine Zither"),

            // Other
            WorldInstrument::WorldPercussion => write!(f, "World Percussion"),
            WorldInstrument::Other => write!(f, "World Instrument"),
        }
    }
}

impl WorldInstrument {
    /// Get the orchestral ordering index (0-based)
    pub fn orchestral_index(&self) -> usize {
        *self as usize
    }

    /// Get all world instruments
    pub fn all() -> Vec<Self> {
        vec![
            // Asian
            WorldInstrument::Erhu,
            WorldInstrument::Guzheng,
            WorldInstrument::Shakuhachi,
            WorldInstrument::Shamisen,
            WorldInstrument::Koto,
            WorldInstrument::TaikoDrum,
            WorldInstrument::Sitar,
            WorldInstrument::Tabla,
            WorldInstrument::Sarod,
            WorldInstrument::Tanpura,
            WorldInstrument::Bansuri,

            // Middle Eastern
            WorldInstrument::Oud,
            WorldInstrument::Duduk,
            WorldInstrument::Ney,
            WorldInstrument::Darbuka,
            WorldInstrument::Qanun,
            WorldInstrument::FrameDrum,

            // Celtic
            WorldInstrument::CelticFiddle,
            WorldInstrument::Bagpipes,
            WorldInstrument::TinWhistle,
            WorldInstrument::Bodhran,
            WorldInstrument::CelticHarp,
            WorldInstrument::IrishFlute,
            WorldInstrument::UilleanPipes,

            // African
            WorldInstrument::Djembe,
            WorldInstrument::Kalimba,
            WorldInstrument::TalkingDrum,
            WorldInstrument::Balafon,
            WorldInstrument::Kora,

            // Latin American
            WorldInstrument::Cajon,
            WorldInstrument::Maracas,
            WorldInstrument::Claves,
            WorldInstrument::Panpipes,
            WorldInstrument::Charango,
            WorldInstrument::Quena,
            WorldInstrument::CajonPeruano,

            // European Folk
            WorldInstrument::Accordion,
            WorldInstrument::Concertina,
            WorldInstrument::Dulcimer,
            WorldInstrument::HurdyGurdy,
            WorldInstrument::AlpineZither,

            // Other
            WorldInstrument::WorldPercussion,
            WorldInstrument::Other,
        ]
    }

    /// Get the cultural region/origin
    pub fn cultural_region(&self) -> &'static str {
        match self {
            WorldInstrument::Erhu | WorldInstrument::Guzheng | WorldInstrument::Shakuhachi |
            WorldInstrument::Shamisen | WorldInstrument::Koto | WorldInstrument::TaikoDrum |
            WorldInstrument::Sitar | WorldInstrument::Tabla | WorldInstrument::Sarod |
            WorldInstrument::Tanpura | WorldInstrument::Bansuri => "Asian",

            WorldInstrument::Oud | WorldInstrument::Duduk | WorldInstrument::Ney |
            WorldInstrument::Darbuka | WorldInstrument::Qanun | WorldInstrument::FrameDrum => "Middle Eastern",

            WorldInstrument::CelticFiddle | WorldInstrument::Bagpipes | WorldInstrument::TinWhistle |
            WorldInstrument::Bodhran | WorldInstrument::CelticHarp | WorldInstrument::IrishFlute |
            WorldInstrument::UilleanPipes => "Celtic",

            WorldInstrument::Djembe | WorldInstrument::Kalimba | WorldInstrument::TalkingDrum |
            WorldInstrument::Balafon | WorldInstrument::Kora => "African",

            WorldInstrument::Cajon | WorldInstrument::Maracas | WorldInstrument::Claves |
            WorldInstrument::Panpipes | WorldInstrument::Charango | WorldInstrument::Quena |
            WorldInstrument::CajonPeruano => "Latin American",

            WorldInstrument::Accordion | WorldInstrument::Concertina | WorldInstrument::Dulcimer |
            WorldInstrument::HurdyGurdy | WorldInstrument::AlpineZither => "European Folk",

            WorldInstrument::WorldPercussion | WorldInstrument::Other => "Various",
        }
    }

    /// Check if this is a percussion instrument
    pub fn is_percussion(&self) -> bool {
        matches!(
            self,
            WorldInstrument::TaikoDrum | WorldInstrument::Tabla | WorldInstrument::Darbuka |
            WorldInstrument::FrameDrum | WorldInstrument::Bodhran | WorldInstrument::Djembe |
            WorldInstrument::TalkingDrum | WorldInstrument::Balafon | WorldInstrument::Cajon |
            WorldInstrument::Maracas | WorldInstrument::Claves | WorldInstrument::CajonPeruano |
            WorldInstrument::WorldPercussion
        )
    }

    /// Check if this is a string instrument
    pub fn is_string(&self) -> bool {
        matches!(
            self,
            WorldInstrument::Erhu | WorldInstrument::Guzheng | WorldInstrument::Shamisen |
            WorldInstrument::Koto | WorldInstrument::Sitar | WorldInstrument::Sarod |
            WorldInstrument::Tanpura | WorldInstrument::Oud | WorldInstrument::Qanun |
            WorldInstrument::CelticFiddle | WorldInstrument::CelticHarp | WorldInstrument::Kalimba |
            WorldInstrument::Kora | WorldInstrument::Charango | WorldInstrument::Dulcimer |
            WorldInstrument::HurdyGurdy | WorldInstrument::AlpineZither
        )
    }

    /// Check if this is a wind instrument
    pub fn is_wind(&self) -> bool {
        matches!(
            self,
            WorldInstrument::Shakuhachi | WorldInstrument::Bansuri | WorldInstrument::Duduk |
            WorldInstrument::Ney | WorldInstrument::Bagpipes | WorldInstrument::TinWhistle |
            WorldInstrument::IrishFlute | WorldInstrument::UilleanPipes | WorldInstrument::Panpipes |
            WorldInstrument::Quena | WorldInstrument::Accordion | WorldInstrument::Concertina
        )
    }

    /// Parse world instrument from text
    pub fn from_string(s: &str) -> Option<Self> {
        let s_lower = s.to_lowercase();
        let s_trimmed = s_lower.trim();

        // Asian instruments
        if s_trimmed.contains("erhu") {
            Some(WorldInstrument::Erhu)
        } else if s_trimmed.contains("guzheng") {
            Some(WorldInstrument::Guzheng)
        } else if s_trimmed.contains("shakuhachi") {
            Some(WorldInstrument::Shakuhachi)
        } else if s_trimmed.contains("shamisen") {
            Some(WorldInstrument::Shamisen)
        } else if s_trimmed.contains("koto") {
            Some(WorldInstrument::Koto)
        } else if s_trimmed.contains("taiko") {
            Some(WorldInstrument::TaikoDrum)
        } else if s_trimmed.contains("sitar") {
            Some(WorldInstrument::Sitar)
        } else if s_trimmed.contains("tabla") {
            Some(WorldInstrument::Tabla)
        } else if s_trimmed.contains("sarod") {
            Some(WorldInstrument::Sarod)
        } else if s_trimmed.contains("tanpura") {
            Some(WorldInstrument::Tanpura)
        } else if s_trimmed.contains("bansuri") {
            Some(WorldInstrument::Bansuri)

        // Middle Eastern instruments
        } else if s_trimmed.contains("oud") {
            Some(WorldInstrument::Oud)
        } else if s_trimmed.contains("duduk") {
            Some(WorldInstrument::Duduk)
        } else if s_trimmed.contains("ney") {
            Some(WorldInstrument::Ney)
        } else if s_trimmed.contains("darbuka") {
            Some(WorldInstrument::Darbuka)
        } else if s_trimmed.contains("qanun") {
            Some(WorldInstrument::Qanun)
        } else if s_trimmed.contains("frame drum") {
            Some(WorldInstrument::FrameDrum)

        // Celtic instruments
        } else if s_trimmed.contains("celtic fiddle") || s_trimmed.contains("irish fiddle") {
            Some(WorldInstrument::CelticFiddle)
        } else if s_trimmed.contains("bagpipes") || s_trimmed.contains("pipes") {
            Some(WorldInstrument::Bagpipes)
        } else if s_trimmed.contains("tin whistle") || s_trimmed.contains("penny whistle") {
            Some(WorldInstrument::TinWhistle)
        } else if s_trimmed.contains("bodhran") || s_trimmed.contains("bodhrán") {
            Some(WorldInstrument::Bodhran)
        } else if s_trimmed.contains("celtic harp") || s_trimmed.contains("irish harp") {
            Some(WorldInstrument::CelticHarp)
        } else if s_trimmed.contains("irish flute") {
            Some(WorldInstrument::IrishFlute)
        } else if s_trimmed.contains("uilleann") {
            Some(WorldInstrument::UilleanPipes)

        // African instruments
        } else if s_trimmed.contains("djembe") {
            Some(WorldInstrument::Djembe)
        } else if s_trimmed.contains("kalimba") || s_trimmed.contains("thumb piano") {
            Some(WorldInstrument::Kalimba)
        } else if s_trimmed.contains("talking drum") {
            Some(WorldInstrument::TalkingDrum)
        } else if s_trimmed.contains("balafon") {
            Some(WorldInstrument::Balafon)
        } else if s_trimmed.contains("kora") {
            Some(WorldInstrument::Kora)

        // Latin American instruments
        } else if s_trimmed.contains("cajón") || s_trimmed.contains("cajon") {
            if s_trimmed.contains("peruano") {
                Some(WorldInstrument::CajonPeruano)
            } else {
                Some(WorldInstrument::Cajon)
            }
        } else if s_trimmed.contains("maracas") {
            Some(WorldInstrument::Maracas)
        } else if s_trimmed.contains("claves") {
            Some(WorldInstrument::Claves)
        } else if s_trimmed.contains("panpipes") || s_trimmed.contains("pan flute") {
            Some(WorldInstrument::Panpipes)
        } else if s_trimmed.contains("charango") {
            Some(WorldInstrument::Charango)
        } else if s_trimmed.contains("quena") {
            Some(WorldInstrument::Quena)

        // European Folk instruments
        } else if s_trimmed.contains("accordion") {
            Some(WorldInstrument::Accordion)
        } else if s_trimmed.contains("concertina") {
            Some(WorldInstrument::Concertina)
        } else if s_trimmed.contains("dulcimer") {
            Some(WorldInstrument::Dulcimer)
        } else if s_trimmed.contains("hurdy") || s_trimmed.contains("hurdy-gurdy") {
            Some(WorldInstrument::HurdyGurdy)
        } else if s_trimmed.contains("alpine zither") {
            Some(WorldInstrument::AlpineZither)

        // Generic
        } else if s_trimmed.contains("world percussion") {
            Some(WorldInstrument::WorldPercussion)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_world_instrument_parsing() {
        // Asian
        assert_eq!(WorldInstrument::from_string("Erhu"), Some(WorldInstrument::Erhu));
        assert_eq!(WorldInstrument::from_string("Sitar"), Some(WorldInstrument::Sitar));
        assert_eq!(WorldInstrument::from_string("Tabla"), Some(WorldInstrument::Tabla));

        // Celtic
        assert_eq!(WorldInstrument::from_string("Bagpipes"), Some(WorldInstrument::Bagpipes));
        assert_eq!(WorldInstrument::from_string("Tin Whistle"), Some(WorldInstrument::TinWhistle));
        assert_eq!(WorldInstrument::from_string("Bodhrán"), Some(WorldInstrument::Bodhran));

        // African
        assert_eq!(WorldInstrument::from_string("Djembe"), Some(WorldInstrument::Djembe));
        assert_eq!(WorldInstrument::from_string("Kalimba"), Some(WorldInstrument::Kalimba));

        // Latin American
        assert_eq!(WorldInstrument::from_string("Cajón"), Some(WorldInstrument::Cajon));
        assert_eq!(WorldInstrument::from_string("Maracas"), Some(WorldInstrument::Maracas));

        // Should not parse non-world instruments
        assert_eq!(WorldInstrument::from_string("Piano"), None);
        assert_eq!(WorldInstrument::from_string("Violin"), None);
    }

    #[test]
    fn test_world_instrument_display() {
        assert_eq!(format!("{}", WorldInstrument::Erhu), "Erhu");
        assert_eq!(format!("{}", WorldInstrument::Bagpipes), "Bagpipes");
        assert_eq!(format!("{}", WorldInstrument::TaikoDrum), "Taiko Drum");
        assert_eq!(format!("{}", WorldInstrument::CelticHarp), "Celtic Harp");
    }

    #[test]
    fn test_cultural_regions() {
        assert_eq!(WorldInstrument::Sitar.cultural_region(), "Asian");
        assert_eq!(WorldInstrument::Oud.cultural_region(), "Middle Eastern");
        assert_eq!(WorldInstrument::Bagpipes.cultural_region(), "Celtic");
        assert_eq!(WorldInstrument::Djembe.cultural_region(), "African");
        assert_eq!(WorldInstrument::Cajon.cultural_region(), "Latin American");
        assert_eq!(WorldInstrument::Accordion.cultural_region(), "European Folk");
    }

    #[test]
    fn test_percussion_classification() {
        assert!(WorldInstrument::Tabla.is_percussion());
        assert!(WorldInstrument::Djembe.is_percussion());
        assert!(WorldInstrument::TaikoDrum.is_percussion());
        assert!(WorldInstrument::Cajon.is_percussion());
        assert!(!WorldInstrument::Sitar.is_percussion());
        assert!(!WorldInstrument::Bagpipes.is_percussion());
    }

    #[test]
    fn test_string_classification() {
        assert!(WorldInstrument::Sitar.is_string());
        assert!(WorldInstrument::Erhu.is_string());
        assert!(WorldInstrument::Oud.is_string());
        assert!(WorldInstrument::CelticHarp.is_string());
        assert!(!WorldInstrument::Tabla.is_string());
        assert!(!WorldInstrument::Bagpipes.is_string());
    }

    #[test]
    fn test_wind_classification() {
        assert!(WorldInstrument::Shakuhachi.is_wind());
        assert!(WorldInstrument::Bagpipes.is_wind());
        assert!(WorldInstrument::TinWhistle.is_wind());
        assert!(WorldInstrument::Panpipes.is_wind());
        assert!(!WorldInstrument::Sitar.is_wind());
        assert!(!WorldInstrument::Tabla.is_wind());
    }

    #[test]
    fn test_all_world_instruments() {
        let all = WorldInstrument::all();
        assert!(all.len() > 30); // Should have many instruments
        assert!(all.contains(&WorldInstrument::Sitar));
        assert!(all.contains(&WorldInstrument::Bagpipes));
        assert!(all.contains(&WorldInstrument::Djembe));
        assert!(all.contains(&WorldInstrument::Cajon));
    }

    #[test]
    fn test_cajon_variants() {
        assert_eq!(WorldInstrument::from_string("Cajón"), Some(WorldInstrument::Cajon));
        assert_eq!(WorldInstrument::from_string("Cajón Peruano"), Some(WorldInstrument::CajonPeruano));
        assert_eq!(WorldInstrument::from_string("Cajon"), Some(WorldInstrument::Cajon));
    }
}
