//! Rig — a complete instrument setup.
//!
//! A [`Rig`] ties together sections (with layers, blocks, and sources) and
//! global blocks into a full signal chain for one instrument type.

use std::fmt;

use facet::Facet;

use crate::block::GlobalBlock;
use crate::id::{BlockId, ModulePresetId, RigId, SectionId};
use crate::module::ModuleType;
use crate::module_preset::{GlobalModuleOverride, ModulePreset};
use crate::section::Section;

// ─────────────────────────────────────────────────────────────────────────────
// InstrumentType
// ─────────────────────────────────────────────────────────────────────────────

/// The instrument category for a rig.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Facet)]
#[repr(u8)]
pub enum InstrumentType {
    Guitar = 0,
    Keys = 1,
    Bass = 2,
    Vocals = 3,
    Drums = 4,
    Synth = 5,
    Custom(String) = 6,
}

impl fmt::Display for InstrumentType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Guitar => f.write_str("Guitar"),
            Self::Keys => f.write_str("Keys"),
            Self::Bass => f.write_str("Bass"),
            Self::Vocals => f.write_str("Vocals"),
            Self::Drums => f.write_str("Drums"),
            Self::Synth => f.write_str("Synth"),
            Self::Custom(name) => f.write_str(name),
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Rig
// ─────────────────────────────────────────────────────────────────────────────

/// A complete instrument setup with sections and global blocks.
///
/// Each rig represents a full signal chain for one instrument type.
/// Sections contain layers with local blocks, while global blocks are
/// shared across the entire rig.
#[derive(Debug, Clone, Facet)]
pub struct Rig {
    pub id: RigId,
    pub name: String,
    pub instrument_type: InstrumentType,
    pub sections: Vec<Section>,
    pub global_blocks: Vec<GlobalBlock>,
    /// All available module presets in this rig.
    pub module_presets: Vec<ModulePreset>,
    /// Globally locked module overrides (persist across all preset/scene/song changes).
    pub global_module_overrides: Vec<GlobalModuleOverride>,
}

impl Rig {
    /// Create a new empty rig for the given instrument type.
    pub fn new(name: impl Into<String>, instrument_type: InstrumentType) -> Self {
        Self {
            id: RigId::new(),
            name: name.into(),
            instrument_type,
            sections: Vec::new(),
            global_blocks: Vec::new(),
            module_presets: Vec::new(),
            global_module_overrides: Vec::new(),
        }
    }

    /// Add a section to the rig.
    pub fn add_section(&mut self, section: Section) {
        self.sections.push(section);
    }

    /// Add a global block to the rig.
    pub fn add_global_block(&mut self, global_block: GlobalBlock) {
        self.global_blocks.push(global_block);
    }

    /// Get a section by ID.
    pub fn get_section(&self, id: SectionId) -> Option<&Section> {
        self.sections.iter().find(|s| s.id == id)
    }

    /// Get a mutable section by ID.
    pub fn get_section_mut(&mut self, id: SectionId) -> Option<&mut Section> {
        self.sections.iter_mut().find(|s| s.id == id)
    }

    /// Get a global block by its underlying `BlockId`.
    pub fn get_global_block(&self, block_id: BlockId) -> Option<&GlobalBlock> {
        self.global_blocks.iter().find(|gb| gb.block.id == block_id)
    }

    /// Add a module preset to the rig.
    pub fn add_module_preset(&mut self, preset: ModulePreset) {
        self.module_presets.push(preset);
    }

    /// Get a module preset by ID.
    pub fn get_module_preset(&self, id: ModulePresetId) -> Option<&ModulePreset> {
        self.module_presets.iter().find(|p| p.id == id)
    }

    /// Add a global module override.
    pub fn add_global_override(&mut self, global_override: GlobalModuleOverride) {
        self.global_module_overrides.push(global_override);
    }

    /// Check whether a module type is locked by a global override.
    pub fn is_module_locked(&self, module_type: &ModuleType) -> bool {
        self.global_module_overrides
            .iter()
            .any(|o| o.module_type == *module_type && o.locked)
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::{Block, PluginId};
    use crate::layer::Layer;
    use crate::normalized::Order;

    fn test_section() -> Section {
        Section::new("Amp Section", Layer::new("Main", 0))
    }

    fn test_global_block() -> GlobalBlock {
        let block = Block::new("Room Reverb", PluginId::vst3("com.test.reverb", "Reverb"));
        GlobalBlock {
            block,
            order: Order::new(0),
        }
    }

    #[test]
    fn rig_creation() {
        let rig = Rig::new("Main Guitar", InstrumentType::Guitar);
        assert_eq!(rig.name, "Main Guitar");
        assert_eq!(rig.instrument_type, InstrumentType::Guitar);
        assert!(rig.sections.is_empty());
        assert!(rig.global_blocks.is_empty());
    }

    #[test]
    fn section_management() {
        let mut rig = Rig::new("Keys Rig", InstrumentType::Keys);
        let section = test_section();
        let section_id = section.id;

        rig.add_section(section);
        assert_eq!(rig.sections.len(), 1);

        // Find by ID
        assert!(rig.get_section(section_id).is_some());
        assert!(rig.get_section(SectionId::new()).is_none());

        // Mutable access
        let s = rig.get_section_mut(section_id).unwrap();
        s.enabled = false;
        assert!(!rig.get_section(section_id).unwrap().enabled);
    }

    #[test]
    fn global_blocks() {
        let mut rig = Rig::new("Bass Rig", InstrumentType::Bass);
        let gb = test_global_block();
        let block_id = gb.block.id;

        rig.add_global_block(gb);
        assert_eq!(rig.global_blocks.len(), 1);

        assert!(rig.get_global_block(block_id).is_some());
        assert!(rig.get_global_block(BlockId::new()).is_none());
    }

    #[test]
    fn instrument_type_display() {
        assert_eq!(format!("{}", InstrumentType::Guitar), "Guitar");
        assert_eq!(format!("{}", InstrumentType::Keys), "Keys");
        assert_eq!(format!("{}", InstrumentType::Bass), "Bass");
        assert_eq!(format!("{}", InstrumentType::Vocals), "Vocals");
        assert_eq!(format!("{}", InstrumentType::Drums), "Drums");
        assert_eq!(format!("{}", InstrumentType::Synth), "Synth");
        assert_eq!(
            format!("{}", InstrumentType::Custom("Theremin".into())),
            "Theremin"
        );
    }

}
