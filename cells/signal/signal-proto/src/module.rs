//! Module types — processing stages in the signal chain.
//!
//! A [`Module`] is a named processing stage (e.g. EQ, Dynamics, Drive) that
//! contains one or more [`ModuleBlock`]s and optional [`ModuleMacro`] knobs
//! for quick parameter access. Modules are the building blocks of a rig's
//! signal flow.

use std::fmt;

use facet::Facet;
use uuid::Uuid;

use crate::block::Block;
use crate::id::{BlockId, ModuleId};
use crate::normalized::{MidiChannel, NormalizedF64, Order};
use crate::tags::{Taggable, Tags};

// ─────────────────────────────────────────────────────────────────────────────
// ModuleType
// ─────────────────────────────────────────────────────────────────────────────

/// Functional category of a processing module.
///
/// Determines where the module fits in the signal chain and how the UI
/// groups and labels it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Facet)]
#[repr(u8)]
pub enum ModuleType {
    // ── Vocal chain ──────────────────────────────────────────────────────
    Rescue = 0,
    Correction = 1,
    Tonal = 2,
    VocalModulation = 3,
    Sends = 4,

    // ── Instrument chain ─────────────────────────────────────────────────
    Source = 10,
    Eq = 11,
    Dynamics = 12,
    Special = 13,
    Drive = 14,
    PreFx = 15,
    Volume = 16,
    Amp = 17,
    PostEq = 19,
    Modulation = 20,
    Time = 21,
    Motion = 22,
    Master = 23,
}

impl ModuleType {
    /// Human-readable display name for this module type.
    pub fn display_name(self) -> &'static str {
        match self {
            Self::Rescue => "Rescue",
            Self::Correction => "Correction",
            Self::Tonal => "Tonal",
            Self::VocalModulation => "Vocal Modulation",
            Self::Sends => "Sends",
            Self::Source => "Source",
            Self::Eq => "EQ",
            Self::Dynamics => "Dynamics",
            Self::Special => "Special",
            Self::Drive => "Drive",
            Self::PreFx => "Pre FX",
            Self::Volume => "Volume",
            Self::Amp => "Amp",
            Self::PostEq => "Post EQ",
            Self::Modulation => "Modulation",
            Self::Time => "Time",
            Self::Motion => "Motion",
            Self::Master => "Master",
        }
    }
}

impl fmt::Display for ModuleType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.display_name())
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// SendMode
// ─────────────────────────────────────────────────────────────────────────────

/// How a module routes its output to the send bus.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Facet, Default)]
#[repr(u8)]
pub enum SendMode {
    /// Module is an insert (no send routing).
    #[default]
    Insert = 0,
    /// One send per layer.
    PerLayer = 1,
    /// One send per module.
    PerModule = 2,
    /// Single global send.
    Global = 3,
}

// ─────────────────────────────────────────────────────────────────────────────
// MidiControl / TriggerMode / MidiTriggerConfig
// ─────────────────────────────────────────────────────────────────────────────

/// A MIDI control source for triggering a block.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Facet)]
#[repr(u8)]
pub enum MidiControl {
    /// Continuous Controller number (0–127).
    Cc(u8),
    /// Note number (0–127).
    Note(u8),
}

/// How a MIDI trigger behaves.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Facet)]
#[repr(u8)]
pub enum TriggerMode {
    /// Active only while held.
    Momentary,
    /// Alternates on each press.
    Toggle,
    /// Maps CC value continuously to a parameter.
    Continuous,
}

/// Configuration for MIDI-triggered block activation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Facet)]
pub struct MidiTriggerConfig {
    pub channel: MidiChannel,
    pub control: MidiControl,
    pub mode: TriggerMode,
}

// ─────────────────────────────────────────────────────────────────────────────
// ModuleBlock
// ─────────────────────────────────────────────────────────────────────────────

/// A block positioned within a module, with optional MIDI trigger.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct ModuleBlock {
    pub id: Uuid,
    pub block: Block,
    pub order: Order,
    pub midi_trigger: Option<MidiTriggerConfig>,
    pub tags: Tags,
}

impl ModuleBlock {
    /// Create a new module block at the given order position.
    pub fn new(block: Block, order: Order) -> Self {
        Self {
            id: Uuid::new_v4(),
            block,
            order,
            midi_trigger: None,
            tags: Tags::new(),
        }
    }

    /// Attach a MIDI trigger configuration.
    #[must_use]
    pub fn with_midi_trigger(mut self, config: MidiTriggerConfig) -> Self {
        self.midi_trigger = Some(config);
        self
    }
}

impl Taggable for ModuleBlock {
    fn tags(&self) -> &Tags {
        &self.tags
    }

    fn tags_mut(&mut self) -> &mut Tags {
        &mut self.tags
    }

    fn name(&self) -> &str {
        &self.block.name
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// ModuleMacro
// ─────────────────────────────────────────────────────────────────────────────

/// A macro knob that exposes a single block parameter for quick access.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct ModuleMacro {
    pub id: Uuid,
    pub name: String,
    pub block_id: BlockId,
    pub param_index: u32,
    pub display_value: String,
    pub normalized: NormalizedF64,
}

// ─────────────────────────────────────────────────────────────────────────────
// Module
// ─────────────────────────────────────────────────────────────────────────────

/// A processing stage containing blocks and macro knobs.
///
/// Modules represent logical groups in the signal chain (e.g. "Drive",
/// "Amp", "EQ"). Each module holds one or more [`ModuleBlock`]s
/// and optional [`ModuleMacro`] knobs for quick parameter access.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct Module {
    pub id: ModuleId,
    pub name: String,
    pub module_type: ModuleType,
    pub blocks: Vec<ModuleBlock>,
    pub macros: Vec<ModuleMacro>,
    pub enabled: bool,
    pub level: NormalizedF64,
    pub send_mode: SendMode,
    pub split_sync: bool,
    pub tags: Tags,
}

impl Module {
    /// Create a new enabled module with default level and no blocks.
    pub fn new(name: impl Into<String>, module_type: ModuleType) -> Self {
        Self {
            id: ModuleId::new(),
            name: name.into(),
            module_type,
            blocks: Vec::new(),
            macros: Vec::new(),
            enabled: true,
            level: NormalizedF64::ONE,
            send_mode: SendMode::default(),
            split_sync: false,
            tags: Tags::new(),
        }
    }

    /// Add a block to this module.
    pub fn add_block(&mut self, module_block: ModuleBlock) {
        self.blocks.push(module_block);
    }

    /// Add a macro knob to this module.
    pub fn add_macro(&mut self, module_macro: ModuleMacro) {
        self.macros.push(module_macro);
    }

    /// Get a block by its underlying `BlockId`.
    pub fn get_block(&self, block_id: BlockId) -> Option<&ModuleBlock> {
        self.blocks.iter().find(|mb| mb.block.id == block_id)
    }

    /// Get a mutable block by its underlying `BlockId`.
    pub fn get_block_mut(&mut self, block_id: BlockId) -> Option<&mut ModuleBlock> {
        self.blocks.iter_mut().find(|mb| mb.block.id == block_id)
    }

    /// Toggle the enabled state, returning the new state.
    pub fn toggle_bypass(&mut self) -> bool {
        self.enabled = !self.enabled;
        self.enabled
    }
}

impl Taggable for Module {
    fn tags(&self) -> &Tags {
        &self.tags
    }

    fn tags_mut(&mut self) -> &mut Tags {
        &mut self.tags
    }

    fn name(&self) -> &str {
        &self.name
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::PluginId;

    fn test_block() -> Block {
        Block::new("Test Block", PluginId::vst3("com.test.plugin", "Test Plugin"))
    }

    #[test]
    fn module_creation() {
        let module = Module::new("My Drive", ModuleType::Drive);
        assert_eq!(module.name, "My Drive");
        assert_eq!(module.module_type, ModuleType::Drive);
        assert!(module.enabled);
        assert_eq!(module.level.get(), 1.0);
        assert_eq!(module.send_mode, SendMode::Insert);
        assert!(!module.split_sync);
        assert!(module.blocks.is_empty());
        assert!(module.macros.is_empty());
    }

    #[test]
    fn block_management() {
        let mut module = Module::new("EQ", ModuleType::Eq);
        let block = test_block();
        let block_id = block.id;

        module.add_block(ModuleBlock::new(block, Order::new(0)));
        assert_eq!(module.blocks.len(), 1);

        // Find by block ID
        assert!(module.get_block(block_id).is_some());
        assert!(module.get_block(BlockId::new()).is_none());

        // Mutable access
        let mb = module.get_block_mut(block_id).unwrap();
        mb.block.bypassed = true;
        assert!(module.get_block(block_id).unwrap().block.bypassed);
    }

    #[test]
    fn toggle_bypass() {
        let mut module = Module::new("Amp", ModuleType::Amp);
        assert!(module.enabled);

        let new_state = module.toggle_bypass();
        assert!(!new_state);
        assert!(!module.enabled);

        let new_state = module.toggle_bypass();
        assert!(new_state);
        assert!(module.enabled);
    }

    #[test]
    fn display_names() {
        assert_eq!(ModuleType::Rescue.display_name(), "Rescue");
        assert_eq!(ModuleType::VocalModulation.display_name(), "Vocal Modulation");
        assert_eq!(ModuleType::Eq.display_name(), "EQ");
        assert_eq!(ModuleType::PreFx.display_name(), "Pre FX");
        assert_eq!(ModuleType::PostEq.display_name(), "Post EQ");
        assert_eq!(ModuleType::Time.display_name(), "Time");
        assert_eq!(ModuleType::Motion.display_name(), "Motion");
        assert_eq!(ModuleType::Master.display_name(), "Master");

        // Display trait matches display_name
        assert_eq!(format!("{}", ModuleType::Drive), "Drive");
        assert_eq!(format!("{}", ModuleType::Time), "Time");
    }

    #[test]
    fn send_mode_default_is_insert() {
        assert_eq!(SendMode::default(), SendMode::Insert);
    }

    #[test]
    fn module_block_with_midi_trigger() {
        let block = test_block();
        let config = MidiTriggerConfig {
            channel: MidiChannel::new(1),
            control: MidiControl::Cc(64),
            mode: TriggerMode::Toggle,
        };

        let mb = ModuleBlock::new(block, Order::new(0)).with_midi_trigger(config);
        assert!(mb.midi_trigger.is_some());

        let trigger = mb.midi_trigger.unwrap();
        assert_eq!(trigger.channel.get(), 1);
        assert_eq!(trigger.control, MidiControl::Cc(64));
        assert_eq!(trigger.mode, TriggerMode::Toggle);
    }

}
