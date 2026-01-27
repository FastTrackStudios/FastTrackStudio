//! Module - Processing stage within a track
//!
//! Modules represent intent-driven collections of blocks in a signal chain.
//! They can operate at different routing levels (Insert, PerLayer, PerModule, Global)
//! and support split-sync mode for preserving multi-channel outputs.

use facet::Facet;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use super::Block;

/// A processing module within a track.
///
/// Each module contains a set of blocks that work together for a specific
/// purpose in the signal chain. Modules can be bypassed as a unit and have
/// exposed macros for quick parameter access.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct Module {
    /// Unique identifier
    pub id: Uuid,
    /// Display name
    pub name: String,
    /// Type of module (determines its role in the signal chain)
    pub module_type: ModuleType,
    /// Blocks within this module
    pub blocks: Vec<ModuleBlock>,
    /// Exposed macro parameters for quick access (2-4 key params)
    pub macros: Vec<ModuleMacro>,
    /// Whether this module is enabled (not bypassed)
    pub enabled: bool,
    /// Output level (0.0 - 1.0)
    pub level: f64,
    /// How this module routes its processing
    pub send_mode: SendMode,
    /// Same processing per channel, preserving outputs
    pub split_sync: bool,
}

/// Type of module in the signal chain.
///
/// Different instruments use different module chains:
/// - Vocal: Rescue -> Correction -> Tonal -> Modulation
/// - Guitar: Amp -> Cabinet -> Modulation
/// - Bass: Amp -> Cabinet -> Compression
/// - Keyboard: Tonal -> Modulation
/// - Drums: Transient -> Tonal
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Facet)]
pub enum ModuleType {
    // === Vocal modules ===
    /// Fix problems: De-Esser, Gate, Rescue-EQ, Compressor (Control)
    Rescue = 0,
    /// Pitch correction: Tuner
    Correction = 1,
    /// Shape tone: Compressor (Style), Tonal-EQ, Saturator
    Tonal = 2,
    /// Add character: Chorus, Flanger, etc.
    VocalModulation = 3,
    /// Spatial effects (shared): Verb, Delay, Special
    Sends = 4,

    // === Guitar/Bass modules ===
    /// Source: Input Gate, Input Volume
    Source = 10,
    /// EQ: Tonal shaping before processing
    Eq = 11,
    /// Dynamics: Compressor, Gate, Limiter
    Dynamics = 12,
    /// Special: Envelope Filter, Wah, Pitch Octave, Doubler
    Special = 13,
    /// Drive: Boost, Overdrive, Distortion (stackable)
    Drive = 14,
    /// Pre-FX: Delay/Reverb before amp
    PreFx = 15,
    /// Volume: Expression pedal, volume control
    Volume = 16,
    /// Amp simulation: Drive, EQ, Presence
    Amp = 17,
    /// Cabinet/IR simulation
    Cabinet = 18,
    /// Post-EQ: Final tonal shaping
    PostEq = 19,
    /// Modulation: Chorus, Flanger, Phaser, etc.
    Modulation = 20,
    /// Post-FX: Delay/Reverb after amp
    PostFx = 21,
    /// Legacy compression/limiting (use Dynamics instead)
    #[deprecated(note = "Use Dynamics module type instead")]
    Compression = 22,

    // === Drum modules ===
    /// Transient shaping: Attack, Sustain
    Transient = 30,
}

/// How a Module routes its processing.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Facet)]
pub enum SendMode {
    /// Insert mode: inline in the signal chain (default)
    Insert = 0,
    /// Per-Layer: each layer gets isolated processing
    PerLayer = 1,
    /// Per-Module: isolated to this module's output
    PerModule = 2,
    /// Global: shared across all sources (like a send bus)
    Global = 3,
}

/// A block within a module with ordering and MIDI trigger config.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct ModuleBlock {
    /// Unique identifier for this module block entry
    pub id: Uuid,
    /// The underlying block
    pub block: Block,
    /// Position in the module's signal chain (lower = earlier)
    pub order: u8,
    /// Optional MIDI trigger configuration (e.g., for Chorus toggle)
    pub midi_trigger: Option<MidiTriggerConfig>,
}

/// A macro parameter exposed for quick access on the module card.
///
/// Macros provide shortcuts to key parameters without opening the full editor.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct ModuleMacro {
    /// Unique identifier
    pub id: Uuid,
    /// Display name (e.g., "Gate", "Drive", "Verb")
    pub name: String,
    /// Which block this macro controls
    pub block_id: Uuid,
    /// Which parameter within the block (index)
    pub param_index: u32,
    /// Formatted display value (e.g., "0.3", "On", "35%")
    pub display_value: String,
    /// Normalized value (0.0 - 1.0) for sliders/knobs
    pub normalized: f64,
}

/// MIDI trigger configuration for a block.
///
/// Allows a block to be toggled or controlled via MIDI messages.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct MidiTriggerConfig {
    /// MIDI channel (1-16)
    pub channel: u8,
    /// MIDI CC number or note number
    pub control: MidiControl,
    /// Whether the control is momentary or toggle
    pub mode: TriggerMode,
}

/// Type of MIDI control.
#[repr(u8)]
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub enum MidiControl {
    /// Control Change message
    Cc(u8) = 0,
    /// Note On/Off message
    Note(u8) = 1,
}

/// How the MIDI trigger behaves.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Facet)]
pub enum TriggerMode {
    /// Active only while held
    Momentary,
    /// Toggles on each press
    Toggle,
    /// Value follows CC value
    Continuous,
}

impl Module {
    /// Create a new module
    pub fn new(name: impl Into<String>, module_type: ModuleType) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            module_type,
            blocks: Vec::new(),
            macros: Vec::new(),
            enabled: true,
            level: 1.0,
            send_mode: SendMode::Insert,
            split_sync: false,
        }
    }

    /// Add a block to this module
    pub fn add_block(&mut self, block: Block, order: u8) -> Uuid {
        let id = Uuid::new_v4();
        self.blocks.push(ModuleBlock {
            id,
            block,
            order,
            midi_trigger: None,
        });
        id
    }

    /// Add a macro parameter
    pub fn add_macro(
        &mut self,
        name: impl Into<String>,
        block_id: Uuid,
        param_index: u32,
        display_value: impl Into<String>,
        normalized: f64,
    ) {
        self.macros.push(ModuleMacro {
            id: Uuid::new_v4(),
            name: name.into(),
            block_id,
            param_index,
            display_value: display_value.into(),
            normalized,
        });
    }

    /// Get a block by ID
    pub fn get_block(&self, id: Uuid) -> Option<&ModuleBlock> {
        self.blocks.iter().find(|b| b.id == id)
    }

    /// Get a mutable block by ID
    pub fn get_block_mut(&mut self, id: Uuid) -> Option<&mut ModuleBlock> {
        self.blocks.iter_mut().find(|b| b.id == id)
    }

    /// Toggle module bypass
    pub fn toggle_bypass(&mut self) {
        self.enabled = !self.enabled;
    }
}

impl ModuleBlock {
    /// Create a new module block
    pub fn new(block: Block, order: u8) -> Self {
        Self {
            id: Uuid::new_v4(),
            block,
            order,
            midi_trigger: None,
        }
    }

    /// Set MIDI trigger configuration
    pub fn with_midi_trigger(mut self, config: MidiTriggerConfig) -> Self {
        self.midi_trigger = Some(config);
        self
    }
}

impl ModuleType {
    /// Get a short display name for this module type
    pub fn display_name(&self) -> &'static str {
        match self {
            ModuleType::Rescue => "Rescue",
            ModuleType::Correction => "Correction",
            ModuleType::Tonal => "Tonal",
            ModuleType::VocalModulation => "Modulation",
            ModuleType::Sends => "Sends",
            ModuleType::Source => "Source",
            ModuleType::Eq => "EQ",
            ModuleType::Dynamics => "Dynamics",
            ModuleType::Special => "Special",
            ModuleType::Drive => "Drive",
            ModuleType::PreFx => "Pre-FX",
            ModuleType::Volume => "Volume",
            ModuleType::Amp => "Amp",
            ModuleType::Cabinet => "Cabinet",
            ModuleType::PostEq => "Post-EQ",
            ModuleType::Modulation => "Modulation",
            ModuleType::PostFx => "Post-FX",
            #[allow(deprecated)]
            ModuleType::Compression => "Compression",
            ModuleType::Transient => "Transient",
        }
    }
}

impl std::fmt::Display for ModuleType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.display_name())
    }
}

impl Default for SendMode {
    fn default() -> Self {
        Self::Insert
    }
}
