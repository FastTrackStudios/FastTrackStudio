//! Module types — processing stages in the signal chain.
//!
//! A [`Module`] is a named processing stage (e.g. EQ, Dynamics, Drive) that
//! contains one or more [`ModuleBlock`]s and optional [`ModuleMacro`] knobs
//! for quick parameter access. Modules are the building blocks of a rig's
//! signal flow.

use std::fmt;

use crate::block::Block;
use crate::id::{BlockId, ModuleBlockId, ModuleId, ModuleMacroId};
use crate::normalized::{MidiChannel, NormalizedF64, Order};
use crate::tags::{Taggable, Tags};
use daw_proto::FxNodeId;

// ─────────────────────────────────────────────────────────────────────────────
// ModuleType
// ─────────────────────────────────────────────────────────────────────────────

/// Functional category of a processing module.
///
/// Determines where the module fits in the signal chain and how the UI
/// groups and labels it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, ::facet::Facet)]
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
    Custom = 99,
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
            Self::Custom => "Custom",
        }
    }

    /// The variant name as a string (e.g. `"Eq"`, `"VocalModulation"`).
    ///
    /// This is the canonical serialisation form for storage — it matches the
    /// Rust variant name exactly and round-trips through [`ModuleType::from_variant_name`].
    pub fn variant_name(self) -> &'static str {
        match self {
            Self::Rescue => "Rescue",
            Self::Correction => "Correction",
            Self::Tonal => "Tonal",
            Self::VocalModulation => "VocalModulation",
            Self::Sends => "Sends",
            Self::Source => "Source",
            Self::Eq => "Eq",
            Self::Dynamics => "Dynamics",
            Self::Special => "Special",
            Self::Drive => "Drive",
            Self::PreFx => "PreFx",
            Self::Volume => "Volume",
            Self::Amp => "Amp",
            Self::PostEq => "PostEq",
            Self::Modulation => "Modulation",
            Self::Time => "Time",
            Self::Motion => "Motion",
            Self::Master => "Master",
            Self::Custom => "Custom",
        }
    }

    /// Parse a `ModuleType` from its variant name string (e.g. `"Eq"`, `"VocalModulation"`).
    ///
    /// This is the inverse of [`ModuleType::variant_name`]. Returns `None` for
    /// unrecognised strings. For fuzzy matching from DAW container names, use
    /// [`ModuleType::from_container_name`] instead.
    pub fn from_variant_name(s: &str) -> Option<Self> {
        Some(match s {
            "Rescue" => Self::Rescue,
            "Correction" => Self::Correction,
            "Tonal" => Self::Tonal,
            "VocalModulation" => Self::VocalModulation,
            "Sends" => Self::Sends,
            "Source" => Self::Source,
            "Eq" => Self::Eq,
            "Dynamics" => Self::Dynamics,
            "Special" => Self::Special,
            "Drive" => Self::Drive,
            "PreFx" => Self::PreFx,
            "Volume" => Self::Volume,
            "Amp" => Self::Amp,
            "PostEq" => Self::PostEq,
            "Modulation" => Self::Modulation,
            "Time" => Self::Time,
            "Motion" => Self::Motion,
            "Master" => Self::Master,
            "Custom" => Self::Custom,
            _ => return None,
        })
    }

    /// Map a REAPER container name to a `ModuleType`.
    ///
    /// Case-insensitive matching with common aliases for each module type.
    /// Returns `None` for unrecognized names.
    pub fn from_container_name(name: &str) -> Option<Self> {
        match name.to_ascii_uppercase().trim() {
            // Vocal chain
            "RESCUE" => Some(Self::Rescue),
            "CORRECTION" | "PITCH" | "TUNE" => Some(Self::Correction),
            "TONAL" => Some(Self::Tonal),
            "VOCAL MODULATION" | "VOCAL MOD" | "VOX MOD" => Some(Self::VocalModulation),
            "SENDS" | "SEND" => Some(Self::Sends),

            // Instrument chain
            "SOURCE" | "INPUT" | "SRC" => Some(Self::Source),
            "EQ" | "EQUALIZER" => Some(Self::Eq),
            "DYNAMICS" | "DYN" | "COMP" | "COMPRESSOR" => Some(Self::Dynamics),
            "SPECIAL" | "UTILITY" => Some(Self::Special),
            "DRIVE" | "OD" | "DISTORTION" | "DIST" | "OVERDRIVE" => Some(Self::Drive),
            "PRE-FX" | "PREFX" | "PRE FX" | "PRE" => Some(Self::PreFx),
            "VOLUME" | "VOL" => Some(Self::Volume),
            "AMP" | "AMPLIFIER" => Some(Self::Amp),
            "POST-EQ" | "POSTEQ" | "POST EQ" => Some(Self::PostEq),
            "MODULATION" | "MOD" => Some(Self::Modulation),
            "TIME" | "DELAY" | "REVERB" | "ECHO" => Some(Self::Time),
            "MOTION" | "TREMOLO" | "TREM" => Some(Self::Motion),
            "MASTER" | "OUTPUT" | "OUT" => Some(Self::Master),
            _ => None,
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, ::facet::Facet, Default)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, ::facet::Facet)]
#[repr(u8)]
pub enum MidiControl {
    /// Continuous Controller number (0–127).
    Cc(u8),
    /// Note number (0–127).
    Note(u8),
}

/// How a MIDI trigger behaves.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, ::facet::Facet)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, ::facet::Facet)]
pub struct MidiTriggerConfig {
    pub channel: MidiChannel,
    pub control: MidiControl,
    pub mode: TriggerMode,
}

// ─────────────────────────────────────────────────────────────────────────────
// ModuleBlock
// ─────────────────────────────────────────────────────────────────────────────

/// A block positioned within a module, with optional MIDI trigger.
#[derive(Debug, Clone, PartialEq, ::facet::Facet)]
pub struct ModuleBlock {
    pub id: ModuleBlockId,
    pub block: Block,
    pub order: Order,
    pub midi_trigger: Option<MidiTriggerConfig>,
    pub tags: Tags,
    /// When bound to a DAW FX chain, the FxNodeId of the plugin or
    /// nested container this block maps to.
    pub fx_node_id: Option<FxNodeId>,
    /// Position within the module's internal 2D grid (column).
    /// `None` = auto-layout (linear chain order).
    pub local_col: Option<usize>,
    /// Position within the module's internal 2D grid (row).
    pub local_row: Option<usize>,
}

impl ModuleBlock {
    /// Create a new module block at the given order position.
    pub fn new(block: Block, order: Order) -> Self {
        Self {
            id: ModuleBlockId::new(),
            block,
            order,
            midi_trigger: None,
            tags: Tags::new(),
            fx_node_id: None,
            local_col: None,
            local_row: None,
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
#[derive(Debug, Clone, PartialEq, ::facet::Facet)]
pub struct ModuleMacro {
    pub id: ModuleMacroId,
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
#[derive(Debug, Clone, PartialEq, ::facet::Facet)]
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
    /// When bound to a DAW FX chain, the FxNodeId of the container
    /// this module maps to.
    pub fx_node_id: Option<FxNodeId>,
    /// Internal grid width for 2D block layouts. `None` = linear chain.
    pub grid_width: Option<usize>,
    /// Internal grid height for 2D block layouts. `None` = 1 row.
    pub grid_height: Option<usize>,
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
            fx_node_id: None,
            grid_width: None,
            grid_height: None,
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
        Block::new(
            "Test Block",
            PluginId::vst3("com.test.plugin", "Test Plugin"),
        )
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
        assert_eq!(
            ModuleType::VocalModulation.display_name(),
            "Vocal Modulation"
        );
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

    #[test]
    fn from_container_name_instrument_chain() {
        assert_eq!(
            ModuleType::from_container_name("DRIVE"),
            Some(ModuleType::Drive)
        );
        assert_eq!(
            ModuleType::from_container_name("AMP"),
            Some(ModuleType::Amp)
        );
        assert_eq!(
            ModuleType::from_container_name("TIME"),
            Some(ModuleType::Time)
        );
        assert_eq!(ModuleType::from_container_name("EQ"), Some(ModuleType::Eq));
        assert_eq!(
            ModuleType::from_container_name("DYNAMICS"),
            Some(ModuleType::Dynamics)
        );
        assert_eq!(
            ModuleType::from_container_name("MODULATION"),
            Some(ModuleType::Modulation)
        );
        assert_eq!(
            ModuleType::from_container_name("MASTER"),
            Some(ModuleType::Master)
        );
        assert_eq!(
            ModuleType::from_container_name("SOURCE"),
            Some(ModuleType::Source)
        );
        assert_eq!(
            ModuleType::from_container_name("PRE-FX"),
            Some(ModuleType::PreFx)
        );
        assert_eq!(
            ModuleType::from_container_name("MOTION"),
            Some(ModuleType::Motion)
        );
    }

    #[test]
    fn from_container_name_case_insensitive() {
        assert_eq!(
            ModuleType::from_container_name("drive"),
            Some(ModuleType::Drive)
        );
        assert_eq!(
            ModuleType::from_container_name("Drive"),
            Some(ModuleType::Drive)
        );
        assert_eq!(
            ModuleType::from_container_name("dRiVe"),
            Some(ModuleType::Drive)
        );
        assert_eq!(
            ModuleType::from_container_name("amp"),
            Some(ModuleType::Amp)
        );
    }

    #[test]
    fn from_container_name_aliases() {
        // Drive aliases
        assert_eq!(
            ModuleType::from_container_name("OD"),
            Some(ModuleType::Drive)
        );
        assert_eq!(
            ModuleType::from_container_name("DISTORTION"),
            Some(ModuleType::Drive)
        );
        assert_eq!(
            ModuleType::from_container_name("OVERDRIVE"),
            Some(ModuleType::Drive)
        );

        // Amp aliases
        assert_eq!(
            ModuleType::from_container_name("AMPLIFIER"),
            Some(ModuleType::Amp)
        );

        // Time aliases
        assert_eq!(
            ModuleType::from_container_name("DELAY"),
            Some(ModuleType::Time)
        );
        assert_eq!(
            ModuleType::from_container_name("REVERB"),
            Some(ModuleType::Time)
        );

        // Dynamics aliases
        assert_eq!(
            ModuleType::from_container_name("DYN"),
            Some(ModuleType::Dynamics)
        );
        assert_eq!(
            ModuleType::from_container_name("COMP"),
            Some(ModuleType::Dynamics)
        );

        // Source aliases
        assert_eq!(
            ModuleType::from_container_name("INPUT"),
            Some(ModuleType::Source)
        );

        // Master aliases
        assert_eq!(
            ModuleType::from_container_name("OUTPUT"),
            Some(ModuleType::Master)
        );

        // Modulation aliases
        assert_eq!(
            ModuleType::from_container_name("MOD"),
            Some(ModuleType::Modulation)
        );

        // Motion aliases
        assert_eq!(
            ModuleType::from_container_name("TREMOLO"),
            Some(ModuleType::Motion)
        );
    }

    #[test]
    fn from_container_name_unrecognized() {
        assert_eq!(ModuleType::from_container_name("FOOBAR"), None);
        assert_eq!(ModuleType::from_container_name(""), None);
        assert_eq!(ModuleType::from_container_name("My Custom Container"), None);
    }

    // ModuleType::from_variant_name / variant_name round-trip

    #[test]
    fn variant_name_round_trips_all_module_types() {
        let all = [
            ModuleType::Rescue,
            ModuleType::Correction,
            ModuleType::Tonal,
            ModuleType::VocalModulation,
            ModuleType::Sends,
            ModuleType::Source,
            ModuleType::Eq,
            ModuleType::Dynamics,
            ModuleType::Special,
            ModuleType::Drive,
            ModuleType::PreFx,
            ModuleType::Volume,
            ModuleType::Amp,
            ModuleType::PostEq,
            ModuleType::Modulation,
            ModuleType::Time,
            ModuleType::Motion,
            ModuleType::Master,
            ModuleType::Custom,
        ];
        for mt in &all {
            let name = mt.variant_name();
            let parsed = ModuleType::from_variant_name(name)
                .unwrap_or_else(|| panic!("from_variant_name({name:?}) returned None"));
            assert_eq!(*mt, parsed, "round-trip failed for {name}");
        }
    }

    #[test]
    fn from_variant_name_returns_none_for_unknown() {
        assert_eq!(ModuleType::from_variant_name("Nonexistent"), None);
        assert_eq!(ModuleType::from_variant_name(""), None);
        // Display names are NOT variant names
        assert_eq!(ModuleType::from_variant_name("Vocal Modulation"), None);
        assert_eq!(ModuleType::from_variant_name("Pre FX"), None);
    }
}
