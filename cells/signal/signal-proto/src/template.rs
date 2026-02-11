//! Rig Templates — structural blueprints for presets.
//!
//! A [`RigTemplate`] defines a rig's structure (which modules, which block
//! slots) without binding to actual plugins. When instantiated it produces a
//! real [`Preset`](crate::preset::Preset) where every block starts as a
//! placeholder awaiting plugin assignment.
//!
//! Hierarchy: `RigTemplate` → `ModuleTemplate` → `BlockPlaceholder`

use facet::Facet;

use crate::block::{Block, BlockType, PluginId};
use crate::category::{BaseTone, PresetCategory};
use crate::id::RigTemplateId;
use crate::module::{ModuleBlock, ModuleType};
use crate::module_preset::{ModuleAssignment, ModulePreset};
use crate::normalized::Order;
use crate::preset::Preset;

// ─────────────────────────────────────────────────────────────────────────────
// BlockPlaceholder
// ─────────────────────────────────────────────────────────────────────────────

/// A placeholder for a block slot in a template.
///
/// Contains the structural information (type, alias, description) but no
/// plugin binding. When instantiated, becomes a real [`Block`](crate::block::Block)
/// with [`PluginId::unassigned()`](crate::block::PluginId::unassigned).
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct BlockPlaceholder {
    /// The DSP category this slot expects (EQ, Compressor, Drive, etc.).
    pub block_type: BlockType,
    /// Display name for the block (e.g. "Main Drive", "Room Reverb").
    pub name: String,
    /// Optional short alias shown in the node graph (e.g. "Rescue-EQ").
    pub alias: Option<String>,
    /// Optional description of this block's purpose in the template.
    pub description: Option<String>,
    /// Position within the module's internal grid (column). `None` means
    /// auto-layout (linear chain). Used for 2D module layouts like parallel
    /// amp pairs or multi-lane time sections.
    pub local_col: Option<usize>,
    /// Position within the module's internal grid (row).
    pub local_row: Option<usize>,
}

impl BlockPlaceholder {
    /// Create a new block placeholder.
    pub fn new(name: impl Into<String>, block_type: BlockType) -> Self {
        Self {
            block_type,
            name: name.into(),
            alias: None,
            description: None,
            local_col: None,
            local_row: None,
        }
    }

    /// Set a short alias (builder pattern).
    #[must_use]
    pub fn with_alias(mut self, alias: impl Into<String>) -> Self {
        self.alias = Some(alias.into());
        self
    }

    /// Set a description (builder pattern).
    #[must_use]
    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = Some(description.into());
        self
    }

    /// Set the block's position within the module's internal 2D grid.
    ///
    /// Used for modules with non-linear layouts (e.g. parallel amps,
    /// multi-lane time sections). Blocks without `at()` are laid out
    /// linearly in chain order.
    #[must_use]
    pub fn at(mut self, col: usize, row: usize) -> Self {
        self.local_col = Some(col);
        self.local_row = Some(row);
        self
    }

    /// Convert this placeholder into a real [`Block`] with [`PluginId::unassigned()`].
    pub fn to_block(&self) -> Block {
        let mut block =
            Block::new(&self.name, PluginId::unassigned()).with_block_type(self.block_type);
        if let Some(alias) = &self.alias {
            block.alias = Some(alias.clone());
        }
        if let Some(desc) = &self.description {
            block.description = Some(desc.clone());
        }
        block
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// ModuleTemplate
// ─────────────────────────────────────────────────────────────────────────────

/// Template for a processing module — defines which block slots exist.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct ModuleTemplate {
    /// The module type this template defines (Drive, Amp, Time, etc.).
    pub module_type: ModuleType,
    /// Display name for the module (e.g. "Drive", "Amp/Cab").
    pub name: String,
    /// Optional description of this module's role in the rig.
    pub description: Option<String>,
    /// Block slots in signal-chain order.
    pub blocks: Vec<BlockPlaceholder>,
    /// Internal grid width (columns) for 2D layouts. `None` = auto (block count).
    pub grid_width: Option<usize>,
    /// Internal grid height (rows) for 2D layouts. `None` = auto (1 row).
    pub grid_height: Option<usize>,
}

impl ModuleTemplate {
    /// Create a new module template with no blocks.
    pub fn new(name: impl Into<String>, module_type: ModuleType) -> Self {
        Self {
            module_type,
            name: name.into(),
            description: None,
            blocks: Vec::new(),
            grid_width: None,
            grid_height: None,
        }
    }

    /// Set a description (builder pattern).
    #[must_use]
    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = Some(description.into());
        self
    }

    /// Set the module's internal grid dimensions for 2D layouts.
    ///
    /// Modules with a grid size will lay out their blocks using
    /// `BlockPlaceholder::local_col` / `local_row` positions within
    /// this grid. Unpositioned blocks fall back to linear auto-layout.
    #[must_use]
    pub fn with_grid_size(mut self, width: usize, height: usize) -> Self {
        self.grid_width = Some(width);
        self.grid_height = Some(height);
        self
    }

    /// Add a block placeholder (builder pattern).
    #[must_use]
    pub fn with_block(mut self, block: BlockPlaceholder) -> Self {
        self.blocks.push(block);
        self
    }

    /// Convert this module template into a [`ModulePreset`] with placeholder blocks.
    pub fn to_module_preset(&self) -> ModulePreset {
        let mut mp = ModulePreset::new(&self.name, self.module_type);
        if let Some(desc) = &self.description {
            mp.description = Some(desc.clone());
        }
        mp.grid_width = self.grid_width;
        mp.grid_height = self.grid_height;
        for (i, bp) in self.blocks.iter().enumerate() {
            let mut mb = ModuleBlock::new(bp.to_block(), Order::new(i as u8));
            mb.local_col = bp.local_col;
            mb.local_row = bp.local_row;
            mp.add_block(mb);
        }
        mp
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// RigTemplate
// ─────────────────────────────────────────────────────────────────────────────

/// A structural blueprint for a complete rig preset.
///
/// Contains module templates in signal-chain order. When instantiated,
/// produces a real [`Preset`](crate::preset::Preset) with placeholder blocks.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct RigTemplate {
    pub id: RigTemplateId,
    /// Display name (e.g. "Guitar Rig Template", "Vocal Rig Template").
    pub name: String,
    /// Optional description of this template's intended use.
    pub description: Option<String>,
    /// Module templates in signal-chain order.
    pub modules: Vec<ModuleTemplate>,
}

impl RigTemplate {
    /// Create a new empty rig template.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            id: RigTemplateId::new(),
            name: name.into(),
            description: None,
            modules: Vec::new(),
        }
    }

    /// Set a description (builder pattern).
    #[must_use]
    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = Some(description.into());
        self
    }

    /// Add a module template (builder pattern).
    #[must_use]
    pub fn with_module(mut self, module: ModuleTemplate) -> Self {
        self.modules.push(module);
        self
    }

    /// Find a module template by type.
    pub fn module(&self, module_type: ModuleType) -> Option<&ModuleTemplate> {
        self.modules.iter().find(|m| m.module_type == module_type)
    }

    /// Instantiate this template into a real [`Preset`] with placeholder blocks.
    ///
    /// Returns `(preset, module_presets)` — the preset with module assignments
    /// pointing to the newly-created module presets.
    pub fn instantiate(&self) -> (Preset, Vec<ModulePreset>) {
        let mut preset = Preset::new(
            &self.name,
            PresetCategory::Generic {
                base_tone: BaseTone::Clean,
            },
        );
        preset.description = self.description.clone();

        let mut module_presets = Vec::with_capacity(self.modules.len());

        for (module_idx, module_template) in self.modules.iter().enumerate() {
            let module_preset = module_template.to_module_preset();
            let assignment = ModuleAssignment::new(
                module_template.module_type,
                module_preset.id,
                Order::new(module_idx as u8),
            );
            preset.module_assignments.push(assignment);
            module_presets.push(module_preset);
        }

        (preset, module_presets)
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn block_placeholder_creation() {
        let bp = BlockPlaceholder::new("Main Drive", BlockType::Drive)
            .with_alias("OD1")
            .with_description("Primary overdrive pedal");

        assert_eq!(bp.name, "Main Drive");
        assert_eq!(bp.block_type, BlockType::Drive);
        assert_eq!(bp.alias.as_deref(), Some("OD1"));
        assert_eq!(bp.description.as_deref(), Some("Primary overdrive pedal"));
    }

    #[test]
    fn module_template_creation() {
        let mt = ModuleTemplate::new("Drive", ModuleType::Drive)
            .with_description("Overdrive and distortion stage")
            .with_block(BlockPlaceholder::new("Boost", BlockType::Boost))
            .with_block(BlockPlaceholder::new("Drive 1", BlockType::Drive))
            .with_block(BlockPlaceholder::new("Drive 2", BlockType::Drive));

        assert_eq!(mt.name, "Drive");
        assert_eq!(mt.module_type, ModuleType::Drive);
        assert_eq!(mt.blocks.len(), 3);
        assert_eq!(mt.blocks[0].name, "Boost");
        assert_eq!(mt.blocks[1].block_type, BlockType::Drive);
    }

    #[test]
    fn rig_template_creation() {
        let rt = RigTemplate::new("Guitar Rig Template")
            .with_description("Standard guitar signal chain")
            .with_module(
                ModuleTemplate::new("Source", ModuleType::Source)
                    .with_block(BlockPlaceholder::new("Input Gate", BlockType::Gate)),
            )
            .with_module(
                ModuleTemplate::new("Drive", ModuleType::Drive)
                    .with_block(BlockPlaceholder::new("Drive 1", BlockType::Drive)),
            );

        assert_eq!(rt.name, "Guitar Rig Template");
        assert_eq!(rt.modules.len(), 2);
        assert!(rt.module(ModuleType::Source).is_some());
        assert!(rt.module(ModuleType::Drive).is_some());
        assert!(rt.module(ModuleType::Amp).is_none());
    }

    #[test]
    fn rig_template_find_module() {
        let rt = RigTemplate::new("Test").with_module(ModuleTemplate::new("Amp", ModuleType::Amp));

        let amp = rt.module(ModuleType::Amp).unwrap();
        assert_eq!(amp.name, "Amp");
    }

    #[test]
    fn block_placeholder_to_block() {
        let bp = BlockPlaceholder::new("Rescue-EQ", BlockType::Eq)
            .with_alias("R-EQ")
            .with_description("First EQ for fixing problem frequencies");

        let block = bp.to_block();
        assert_eq!(block.name, "Rescue-EQ");
        assert_eq!(block.display_name(), "R-EQ");
        assert_eq!(block.block_type, BlockType::Eq);
        assert!(block.is_placeholder());
        assert_eq!(
            block.description.as_deref(),
            Some("First EQ for fixing problem frequencies")
        );
    }

    #[test]
    fn module_template_to_module_preset() {
        let mt = ModuleTemplate::new("Drive", ModuleType::Drive)
            .with_description("Overdrive stage")
            .with_block(BlockPlaceholder::new("Boost", BlockType::Boost))
            .with_block(BlockPlaceholder::new("Drive 1", BlockType::Drive));

        let mp = mt.to_module_preset();
        assert_eq!(mp.name, "Drive");
        assert_eq!(mp.module_type, ModuleType::Drive);
        assert_eq!(mp.description.as_deref(), Some("Overdrive stage"));
        assert_eq!(mp.blocks.len(), 2);
        assert!(mp.blocks[0].block.is_placeholder());
        assert_eq!(mp.blocks[0].block.name, "Boost");
        assert_eq!(mp.blocks[1].block.name, "Drive 1");
    }

    #[test]
    fn rig_template_instantiate() {
        let rt = RigTemplate::new("Guitar Template")
            .with_description("Standard guitar chain")
            .with_module(
                ModuleTemplate::new("Source", ModuleType::Source)
                    .with_block(BlockPlaceholder::new("Input Gate", BlockType::Gate))
                    .with_block(BlockPlaceholder::new("Input Volume", BlockType::Volume)),
            )
            .with_module(
                ModuleTemplate::new("Drive", ModuleType::Drive)
                    .with_block(BlockPlaceholder::new("Boost", BlockType::Boost))
                    .with_block(BlockPlaceholder::new("Drive 1", BlockType::Drive))
                    .with_block(BlockPlaceholder::new("Drive 2", BlockType::Drive)),
            );

        let (preset, module_presets) = rt.instantiate();

        // Preset
        assert_eq!(preset.name, "Guitar Template");
        assert_eq!(preset.description.as_deref(), Some("Standard guitar chain"));
        assert_eq!(preset.module_assignments.len(), 2);

        // Module presets
        assert_eq!(module_presets.len(), 2);
        assert_eq!(module_presets[0].module_type, ModuleType::Source);
        assert_eq!(module_presets[0].blocks.len(), 2);
        assert_eq!(module_presets[1].module_type, ModuleType::Drive);
        assert_eq!(module_presets[1].blocks.len(), 3);

        // Assignments link correctly
        assert_eq!(
            preset.module_assignments[0].module_preset_id,
            module_presets[0].id
        );
        assert_eq!(
            preset.module_assignments[1].module_preset_id,
            module_presets[1].id
        );

        // All blocks are placeholders
        for mp in &module_presets {
            for mb in &mp.blocks {
                assert!(
                    mb.block.is_placeholder(),
                    "block '{}' should be a placeholder",
                    mb.block.name
                );
            }
        }
    }
}
