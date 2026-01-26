//! Rig - Complete instrument setup
//!
//! A Rig represents the complete instrument setup like "Guitar Rig" or "Keys Rig".
//! It defines the available sections and global blocks.

use facet::Facet;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use super::{GlobalBlock, Section};

/// The complete instrument setup.
///
/// A Rig defines the structure of an instrument with its sections and global
/// effects. Different profiles can use the same rig but with different presets.
///
/// # Examples
///
/// - `Guitar Rig` - Sections: Amp, Effects; Global: Reverb, Delay
/// - `Keys Rig` - Sections: Piano, Organ, Synth; Global: Reverb, Chorus
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct Rig {
    /// Unique identifier
    pub id: Uuid,
    /// Rig name (e.g., "Guitar Rig", "Keys Rig")
    pub name: String,
    /// Type of instrument this rig is for
    pub instrument_type: InstrumentType,
    /// Available sections in this rig
    pub sections: Vec<Section>,
    /// Shared global effects blocks
    pub global_blocks: Vec<GlobalBlock>,
}

/// Type of instrument a rig is designed for.
#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Facet)]
pub enum InstrumentType {
    Guitar,
    Keys,
    Bass,
    Vocals,
    Drums,
    Synth,
    Custom(String),
}

impl Rig {
    /// Create a new rig
    pub fn new(name: impl Into<String>, instrument_type: InstrumentType) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            instrument_type,
            sections: Vec::new(),
            global_blocks: Vec::new(),
        }
    }

    /// Add a section to the rig
    pub fn add_section(&mut self, section: Section) {
        self.sections.push(section);
    }

    /// Add a global block to the rig
    pub fn add_global_block(&mut self, block: GlobalBlock) {
        self.global_blocks.push(block);
    }

    /// Get a section by ID
    pub fn get_section(&self, id: Uuid) -> Option<&Section> {
        self.sections.iter().find(|s| s.id == id)
    }

    /// Get a mutable section by ID
    pub fn get_section_mut(&mut self, id: Uuid) -> Option<&mut Section> {
        self.sections.iter_mut().find(|s| s.id == id)
    }

    /// Get a global block by ID
    pub fn get_global_block(&self, id: Uuid) -> Option<&GlobalBlock> {
        self.global_blocks.iter().find(|b| b.block.id == id)
    }
}

impl std::fmt::Display for InstrumentType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InstrumentType::Guitar => write!(f, "Guitar"),
            InstrumentType::Keys => write!(f, "Keys"),
            InstrumentType::Bass => write!(f, "Bass"),
            InstrumentType::Vocals => write!(f, "Vocals"),
            InstrumentType::Drums => write!(f, "Drums"),
            InstrumentType::Synth => write!(f, "Synth"),
            InstrumentType::Custom(name) => write!(f, "{name}"),
        }
    }
}
