//! Musical Instruments Library
//!
//! This crate provides comprehensive musical instrument definitions and categorization
//! for DAW applications. It includes support for orchestral, popular, world, and
//! electronic instruments with proper tuning and transposition information.
//!
//! ## Features
//!
//! - **Comprehensive Instrument Database**: Classical orchestral, popular, world, and electronic instruments
//! - **Smart String Parsing**: Parse instrument names from strings with fuzzy matching
//! - **Tuning Support**: Instruments can specify their tuning (Bb, A, F, etc.)
//! - **Category System**: Organized by instrument families with proper categorization
//! - **Display Formatting**: Rich display with color coding for different instrument families
//! - **TypeScript Integration**: Full ts-rs support for frontend integration
//!
//! ## Usage Examples
//!
//! ### Basic Usage
//! ```rust
//! use instruments::{Instrument, InstrumentCategory, Transposition, StringInstrument, BrassInstrument};
//!
//! // Create instruments
//! let violin = Instrument::String(StringInstrument::Violin);
//! let trumpet = Instrument::Brass(BrassInstrument::Trumpet, Transposition::Bb);
//!
//! // Get categories
//! assert_eq!(violin.category(), InstrumentCategory::Strings);
//! assert_eq!(trumpet.category(), InstrumentCategory::Brass);
//! ```
//!
//! ### String Parsing
//! ```rust
//! use instruments::Part;
//!
//! let part = Part::from_string("Violin");
//! let clarinet_part = Part::from_string("Clarinet in Bb");
//! let trumpet_part = Part::from_string("Trumpet in Bb");
//! let solo_cello = Part::from_string("Solo Cello");
//! ```

use std::fmt;
use serde::{Deserialize, Serialize};
use ts_rs::TS;
use regex::Regex;
use enum_iterator::{Sequence, all};

// Module declarations
pub mod orchestra;
pub mod band;
pub mod vocals;
pub mod world;

// Re-exports from submodules
pub use orchestra::{StringInstrument, BrassInstrument, WoodwindInstrument, HarpInstrument};
pub use band::{PercussionInstrument, GuitarInstrument, KeyboardInstrument};
pub use vocals::Vocal;
pub use world::WorldInstrument;

/// Common transpositions for transposing instruments
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, TS)]
#[ts(export)]
pub enum Transposition {
    /// Concert pitch (no transposition)
    C,
    /// Bb transposition (sounds major 2nd lower)
    Bb,
    /// A transposition (sounds minor 3rd lower)
    A,
    /// F transposition (sounds perfect 5th lower)
    F,
    /// Eb transposition (sounds major 6th lower)
    Eb,
}

impl fmt::Display for Transposition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Transposition::C => write!(f, "in C"),
            Transposition::Bb => write!(f, "in Bb"),
            Transposition::A => write!(f, "in A"),
            Transposition::F => write!(f, "in F"),
            Transposition::Eb => write!(f, "in Eb"),
        }
    }
}

/// Instrument categories for organization and color coding
/// Order matches standard orchestral score layout
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize, TS, Sequence)]
#[ts(export)]
pub enum InstrumentCategory {
    Woodwinds,    // 0 - First in orchestral scores
    Brass,        // 1 - Second in orchestral scores
    Percussion,   // 2 - Percussion section
    Keyboard,     // 3 - Piano/keyboard parts
    Harp,         // 4 - Harp section
    Strings,      // 5 - String section
    Choir,        // 6 - Vocal parts
    Solo,         // 7 - Solo instruments
    RhythmSection,// 8 - Modern band instruments
    World,        // 9 - World/ethnic instruments
    Other,        // 10 - Miscellaneous
}

impl fmt::Display for InstrumentCategory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InstrumentCategory::Strings => write!(f, "Strings"),
            InstrumentCategory::Woodwinds => write!(f, "Woodwinds"),
            InstrumentCategory::Brass => write!(f, "Brass"),
            InstrumentCategory::Percussion => write!(f, "Percussion"),
            InstrumentCategory::Keyboard => write!(f, "Keyboard"),
            InstrumentCategory::Harp => write!(f, "Harp"),
            InstrumentCategory::RhythmSection => write!(f, "Rhythm Section"),
            InstrumentCategory::Choir => write!(f, "Choir"),
            InstrumentCategory::Solo => write!(f, "Solo"),
            InstrumentCategory::World => write!(f, "World"),
            InstrumentCategory::Other => write!(f, "Other"),
        }
    }
}

/// Main instrument enum encompassing all instrument types
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, TS)]
#[ts(export)]
pub enum Instrument {
    String(StringInstrument),
    Woodwind(WoodwindInstrument, Transposition),
    Brass(BrassInstrument, Transposition),
    Percussion(PercussionInstrument),
    Keyboard(KeyboardInstrument),
    Harp(HarpInstrument),
    Guitar(GuitarInstrument),
    World(WorldInstrument),
    Other(String),
}

impl Instrument {
    /// Get the category of this instrument
    pub fn category(&self) -> InstrumentCategory {
        match self {
            Instrument::String(_) => InstrumentCategory::Strings,
            Instrument::Woodwind(_, _) => InstrumentCategory::Woodwinds,
            Instrument::Brass(_, _) => InstrumentCategory::Brass,
            Instrument::Percussion(_) => InstrumentCategory::Percussion,
            Instrument::Keyboard(_) => InstrumentCategory::Keyboard,
            Instrument::Harp(_) => InstrumentCategory::Harp,
            Instrument::Guitar(_) => InstrumentCategory::RhythmSection,
            Instrument::World(_) => InstrumentCategory::World,
            Instrument::Other(_) => InstrumentCategory::Other,
        }
    }

    /// Get the orchestral ordering index for this instrument within its category
    pub fn orchestral_index(&self) -> usize {
        match self {
            Instrument::String(inst) => inst.orchestral_index(),
            Instrument::Woodwind(inst, _) => inst.orchestral_index(),
            Instrument::Brass(inst, _) => inst.orchestral_index(),
            Instrument::Percussion(inst) => inst.orchestral_index(),
            Instrument::Keyboard(inst) => inst.orchestral_index(),
            Instrument::Harp(inst) => inst.orchestral_index(),
            Instrument::Guitar(inst) => inst.orchestral_index(),
            Instrument::World(inst) => inst.orchestral_index(),
            Instrument::Other(_) => 0, // Default for other instruments
        }
    }

    /// Parse instrument from string with optional transposition
    pub fn from_string(s: &str) -> Option<Self> {
        let s_lower = s.to_lowercase();

        // Extract transposition if present
        let transposition = if s_lower.contains("in bb") || s_lower.contains("in b♭") {
            Transposition::Bb
        } else if s_lower.contains("in a") && !s_lower.contains("in ab") {
            Transposition::A
        } else if s_lower.contains("in f") {
            Transposition::F
        } else if s_lower.contains("in eb") || s_lower.contains("in e♭") {
            Transposition::Eb
        } else {
            Transposition::C
        };

        // Try parsing each instrument type
        if let Some(inst) = StringInstrument::from_string(s) {
            Some(Instrument::String(inst))
        } else if let Some(inst) = WoodwindInstrument::from_string(s) {
            Some(Instrument::Woodwind(inst, transposition))
        } else if let Some(inst) = BrassInstrument::from_string(s) {
            Some(Instrument::Brass(inst, transposition))
        } else if let Some(inst) = PercussionInstrument::from_string(s) {
            Some(Instrument::Percussion(inst))
        } else if let Some(inst) = KeyboardInstrument::from_string(s) {
            Some(Instrument::Keyboard(inst))
        } else if let Some(inst) = HarpInstrument::from_string(s) {
            Some(Instrument::Harp(inst))
        } else if let Some(inst) = GuitarInstrument::from_string(s) {
            Some(Instrument::Guitar(inst))
        } else if let Some(inst) = WorldInstrument::from_string(s) {
            Some(Instrument::World(inst))
        } else {
            None
        }
    }

    /// Get all known instruments
    pub fn all_known() -> Vec<Self> {
        let mut instruments = Vec::new();

        // Add string instruments
        for inst in StringInstrument::all() {
            instruments.push(Instrument::String(inst));
        }

        // Add woodwind instruments with common transpositions
        for inst in WoodwindInstrument::all() {
            let default_trans = inst.default_transposition();
            instruments.push(Instrument::Woodwind(inst.clone(), default_trans));
            // Add alternate transpositions for some instruments
            match &inst {
                WoodwindInstrument::Clarinet => {
                    instruments.push(Instrument::Woodwind(inst.clone(), Transposition::A));
                },
                _ => {}
            }
        }

        // Add brass instruments with common transpositions
        for inst in BrassInstrument::all() {
            let default_trans = inst.default_transposition();
            instruments.push(Instrument::Brass(inst.clone(), default_trans));
            // Add alternate transpositions
            match &inst {
                BrassInstrument::Trumpet => {
                    instruments.push(Instrument::Brass(inst.clone(), Transposition::C));
                },
                _ => {}
            }
        }

        // Add other instruments
        for inst in PercussionInstrument::all() {
            instruments.push(Instrument::Percussion(inst));
        }
        for inst in KeyboardInstrument::all() {
            instruments.push(Instrument::Keyboard(inst));
        }
        for inst in HarpInstrument::all() {
            instruments.push(Instrument::Harp(inst));
        }
        for inst in GuitarInstrument::all() {
            instruments.push(Instrument::Guitar(inst));
        }
        for inst in WorldInstrument::all() {
            instruments.push(Instrument::World(inst));
        }

        instruments
    }
}

impl fmt::Display for Instrument {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instrument::String(inst) => write!(f, "{}", inst),
            Instrument::Woodwind(inst, Transposition::C) => write!(f, "{}", inst),
            Instrument::Woodwind(inst, trans) => write!(f, "{} {}", inst, trans),
            Instrument::Brass(inst, Transposition::C) => write!(f, "{}", inst),
            Instrument::Brass(inst, trans) => write!(f, "{} {}", inst, trans),
            Instrument::Percussion(inst) => write!(f, "{}", inst),
            Instrument::Keyboard(inst) => write!(f, "{}", inst),
            Instrument::Harp(inst) => write!(f, "{}", inst),
            Instrument::Guitar(inst) => write!(f, "{}", inst),
            Instrument::World(inst) => write!(f, "{}", inst),
            Instrument::Other(name) => write!(f, "{}", name),
        }
    }
}

/// Musical part representation
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, TS)]
#[ts(export)]
pub enum Part {
    /// Regular instrument part
    Instrument(Instrument, Option<u8>), // instrument and optional part number
    /// Solo instrument part
    Solo(Instrument),
    /// Vocal part
    Vocal(Vocal),
    /// Full orchestral/band score
    FullScore,
    /// Score and parts combined
    ScoreAndParts,
    /// Other unspecified part
    Other(String),
}

impl InstrumentCategory {
    /// Get all categories in orchestral order
    pub fn orchestral_order() -> Vec<Self> {
        all::<Self>().collect()
    }

    /// Get the orchestral ordering index (0-based)
    pub fn orchestral_index(&self) -> usize {
        *self as usize
    }
}

/// Trait for instruments that can be ordered in orchestral/performance context
pub trait OrchestralOrder {
    /// Get the orchestral ordering index within the category
    fn orchestral_index(&self) -> usize;

    /// Get all instruments of this type in orchestral order
    fn orchestral_order() -> Vec<Self> where Self: Sized;
}

impl Part {
    /// Extract number from a string like "Violin 1" -> Some(1)
    fn extract_number(s: &str) -> Option<u8> {
        // Match patterns like "Violin 1", "1st Violin", "Clarinet I", "Horn 2"
        let re = Regex::new(r"(?:\s+(\d+)|(\d+)(?:st|nd|rd|th)\s+|[^\w\s]+([IVX]+))").ok()?;

        if let Some(caps) = re.captures(s) {
            // Try numeric capture groups
            if let Some(num_str) = caps.get(1).or_else(|| caps.get(2)) {
                return num_str.as_str().parse::<u8>().ok();
            }
            // Try Roman numerals
            if let Some(roman) = caps.get(3) {
                return match roman.as_str() {
                    "I" => Some(1),
                    "II" => Some(2),
                    "III" => Some(3),
                    "IV" => Some(4),
                    "V" => Some(5),
                    "VI" => Some(6),
                    _ => None,
                };
            }
        }
        None
    }

    /// Parse part from string
    pub fn from_string(s: &str) -> Option<Self> {
        let s_trimmed = s.trim();

        // Check for special cases first
        if s_trimmed.to_lowercase().contains("full score") {
            return Some(Part::FullScore);
        }
        if s_trimmed.to_lowercase().contains("score and parts") {
            return Some(Part::ScoreAndParts);
        }

        // Check for solo parts
        if s_trimmed.to_lowercase().starts_with("solo ") {
            let instrument_part = &s_trimmed[5..]; // Remove "solo "
            if let Some(instrument) = Instrument::from_string(instrument_part) {
                return Some(Part::Solo(instrument));
            }
        }

        // Check for vocal parts
        if let Some(vocal) = Vocal::from_string(s_trimmed) {
            return Some(Part::Vocal(vocal));
        }

        // Check for numbered instrument parts
        let part_number = Self::extract_number(s_trimmed);

        // Try to parse as instrument
        if let Some(instrument) = Instrument::from_string(s_trimmed) {
            Some(Part::Instrument(instrument, part_number))
        } else {
            Some(Part::Other(s_trimmed.to_string()))
        }
    }

    /// Get the category of this part
    pub fn category(&self) -> InstrumentCategory {
        match self {
            Part::Instrument(instrument, _) => instrument.category(),
            Part::Solo(instrument) => InstrumentCategory::Solo,
            Part::Vocal(_) => InstrumentCategory::Choir,
            Part::FullScore | Part::ScoreAndParts => InstrumentCategory::Other,
            Part::Other(_) => InstrumentCategory::Other,
        }
    }
}

impl fmt::Display for Part {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Part::Instrument(instrument, Some(num)) => write!(f, "{} {}", instrument, num),
            Part::Instrument(instrument, None) => write!(f, "{}", instrument),
            Part::Solo(instrument) => write!(f, "Solo {}", instrument),
            Part::Vocal(vocal) => write!(f, "{}", vocal),
            Part::FullScore => write!(f, "Full Score"),
            Part::ScoreAndParts => write!(f, "Score and Parts"),
            Part::Other(name) => write!(f, "{}", name),
        }
    }
}

impl Part {
    /// Sort parts in orchestral/score order
    pub fn orchestral_sort(parts: &mut [Part]) {
        parts.sort_by(|a, b| {
            // First sort by category (orchestral order)
            let cat_cmp = a.category().cmp(&b.category());
            if cat_cmp != std::cmp::Ordering::Equal {
                return cat_cmp;
            }

            // Within category, sort by instrument type and part number
            match (a, b) {
                (Part::Instrument(inst_a, num_a), Part::Instrument(inst_b, num_b)) => {
                    // Compare instruments by their orchestral index, then part numbers
                    let idx_a = inst_a.orchestral_index();
                    let idx_b = inst_b.orchestral_index();
                    let inst_cmp = idx_a.cmp(&idx_b);
                    if inst_cmp != std::cmp::Ordering::Equal {
                        return inst_cmp;
                    }
                    num_a.cmp(num_b)
                },
                (Part::Vocal(vocal_a), Part::Vocal(vocal_b)) => {
                    // Sort vocals in SATB order using orchestral index
                    vocal_a.orchestral_index().cmp(&vocal_b.orchestral_index())
                },
                _ => std::cmp::Ordering::Equal,
            }
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_transposition_display() {
        assert_eq!(format!("{}", Transposition::Bb), "in Bb");
        assert_eq!(format!("{}", Transposition::A), "in A");
        assert_eq!(format!("{}", Transposition::F), "in F");
    }

    #[test]
    fn test_instrument_categories() {
        let violin = Instrument::String(StringInstrument::Violin);
        let clarinet = Instrument::Woodwind(WoodwindInstrument::Clarinet, Transposition::Bb);
        let trumpet = Instrument::Brass(BrassInstrument::Trumpet, Transposition::Bb);
        let piano = Instrument::Keyboard(KeyboardInstrument::Piano);

        assert_eq!(violin.category(), InstrumentCategory::Strings);
        assert_eq!(clarinet.category(), InstrumentCategory::Woodwinds);
        assert_eq!(trumpet.category(), InstrumentCategory::Brass);
        assert_eq!(piano.category(), InstrumentCategory::Keyboard);
    }

    #[test]
    fn test_instrument_parsing() {
        assert!(matches!(
            Instrument::from_string("Violin"),
            Some(Instrument::String(StringInstrument::Violin))
        ));

        assert!(matches!(
            Instrument::from_string("Clarinet in Bb"),
            Some(Instrument::Woodwind(WoodwindInstrument::Clarinet, Transposition::Bb))
        ));

        assert!(matches!(
            Instrument::from_string("Trumpet in C"),
            Some(Instrument::Brass(BrassInstrument::Trumpet, Transposition::C))
        ));

        assert!(matches!(
            Instrument::from_string("Piano"),
            Some(Instrument::Keyboard(KeyboardInstrument::Piano))
        ));
    }

    #[test]
    fn test_part_parsing() {
        let violin_part = Part::from_string("Violin 1");
        assert!(matches!(violin_part, Some(Part::Instrument(Instrument::String(StringInstrument::Violin), Some(1)))));

        let solo_cello = Part::from_string("Solo Cello");
        assert!(matches!(solo_cello, Some(Part::Solo(Instrument::String(StringInstrument::Cello)))));

        let soprano = Part::from_string("Soprano");
        assert!(matches!(soprano, Some(Part::Vocal(Vocal::Soprano))));

        let full_score = Part::from_string("Full Score");
        assert!(matches!(full_score, Some(Part::FullScore)));
    }

    #[test]
    fn test_instrument_display() {
        let clarinet_bb = Instrument::Woodwind(WoodwindInstrument::Clarinet, Transposition::Bb);
        let clarinet_a = Instrument::Woodwind(WoodwindInstrument::Clarinet, Transposition::A);

        assert_eq!(format!("{}", clarinet_bb), "Clarinet in Bb");
        assert_eq!(format!("{}", clarinet_a), "Clarinet in A");
    }

    #[test]
    fn test_part_categories() {
        let violin_part = Part::Instrument(Instrument::String(StringInstrument::Violin), Some(1));
        let soprano_part = Part::Vocal(Vocal::Soprano);

        assert_eq!(violin_part.category(), InstrumentCategory::Strings);
        assert_eq!(soprano_part.category(), InstrumentCategory::Choir);
    }

    #[test]
    fn test_all_known_instruments() {
        let instruments = Instrument::all_known();
        assert!(!instruments.is_empty());

        // Should contain various instrument types
        let has_violin = instruments.iter().any(|i| matches!(i, Instrument::String(StringInstrument::Violin)));
        let has_clarinet = instruments.iter().any(|i| matches!(i, Instrument::Woodwind(WoodwindInstrument::Clarinet, _)));
        let has_trumpet = instruments.iter().any(|i| matches!(i, Instrument::Brass(BrassInstrument::Trumpet, _)));

        assert!(has_violin);
        assert!(has_clarinet);
        assert!(has_trumpet);
    }
}
