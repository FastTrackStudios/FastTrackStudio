//! Block types — DSP processing units in the signal chain.
//!
//! A [`Block`] is a single processing node (amp, compressor, EQ, etc.) backed
//! by a plugin. Blocks hold their plugin identity, bypass state, and parameter
//! values. [`GlobalBlock`] wraps a block with a signal-chain [`Order`] for
//! positioning in a rig's global block pool.

use facet::Facet;

use crate::id::BlockId;
use crate::normalized::Order;
use crate::parameter::ParameterValue;
use crate::tags::{Taggable, Tags};

// ─────────────────────────────────────────────────────────────────────────────
// PluginFormat
// ─────────────────────────────────────────────────────────────────────────────

/// Audio plugin format.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Facet)]
#[repr(C)]
pub enum PluginFormat {
    Vst3,
    Clap,
    Au,
    Js,
}

// ─────────────────────────────────────────────────────────────────────────────
// PluginId
// ─────────────────────────────────────────────────────────────────────────────

/// Identifies a specific plugin by format, unique ID, and display name.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Facet)]
pub struct PluginId {
    /// Plugin format (VST3, CLAP, AU, JS).
    pub format: PluginFormat,
    /// Format-specific unique identifier (e.g. VST3 class ID, CLAP plugin ID).
    pub uid: String,
    /// Human-readable plugin name.
    pub display_name: String,
}

impl PluginId {
    /// Create a new plugin identifier.
    pub fn new(
        format: PluginFormat,
        uid: impl Into<String>,
        display_name: impl Into<String>,
    ) -> Self {
        Self {
            format,
            uid: uid.into(),
            display_name: display_name.into(),
        }
    }

    /// Create a VST3 plugin identifier.
    pub fn vst3(uid: impl Into<String>, display_name: impl Into<String>) -> Self {
        Self::new(PluginFormat::Vst3, uid, display_name)
    }

    /// Create a CLAP plugin identifier.
    pub fn clap(uid: impl Into<String>, display_name: impl Into<String>) -> Self {
        Self::new(PluginFormat::Clap, uid, display_name)
    }

    /// Create a JS (REAPER JSFX) plugin identifier.
    pub fn js(uid: impl Into<String>, display_name: impl Into<String>) -> Self {
        Self::new(PluginFormat::Js, uid, display_name)
    }

    /// Sentinel value for blocks that have no plugin assigned yet
    /// (template placeholders awaiting user assignment).
    pub fn unassigned() -> Self {
        Self {
            format: PluginFormat::Vst3,
            uid: String::new(),
            display_name: "Unassigned".into(),
        }
    }

    /// Whether this is the unassigned sentinel.
    pub fn is_unassigned(&self) -> bool {
        self.uid.is_empty()
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// BlockType
// ─────────────────────────────────────────────────────────────────────────────

/// Functional category of a DSP block.
///
/// Used for UI grouping, icon selection, and signal-chain validation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Facet, Default)]
#[repr(C)]
pub enum BlockType {
    Input,
    Compressor,
    #[default]
    Drive,
    Amp,
    Cabinet,
    Eq,
    Modulation,
    Delay,
    Reverb,
    Gate,
    Volume,
    Pitch,
    Tremolo,
    Limiter,
    Send,
    Special,
    Freeze,
    Custom,
    // Vocal-specific
    DeEsser,
    Saturator,
    Tuner,
    // Fine-grained modulation
    Chorus,
    Flanger,
    Phaser,
    RingModulator,
    // Fine-grained special
    Wah,
    Filter,
    Doubler,
    // Motion
    Panner,
    Vibrato,
    Rotary,
    // Utility
    Crossover,
    // Drive subcategory
    Boost,
}

impl BlockType {
    /// UI category grouping for block type selectors and filters.
    pub fn category(&self) -> &'static str {
        match self {
            Self::Chorus
            | Self::Flanger
            | Self::Phaser
            | Self::RingModulator
            | Self::Modulation => "Modulation",
            Self::Tremolo | Self::Panner | Self::Vibrato | Self::Rotary => "Motion",
            Self::Delay | Self::Reverb | Self::Freeze => "Time",
            Self::Wah | Self::Filter | Self::Pitch | Self::Doubler | Self::Special => "Special",
            Self::Drive | Self::Boost | Self::Saturator => "Drive",
            Self::Amp | Self::Cabinet => "Amp",
            Self::Eq | Self::Crossover => "EQ",
            Self::Compressor | Self::Gate | Self::Limiter | Self::DeEsser => "Dynamics",
            Self::Volume | Self::Send | Self::Input | Self::Tuner => "Utility",
            Self::Custom => "Other",
        }
    }

    /// Human-readable display name for UI labels.
    pub fn display_name(&self) -> &'static str {
        match self {
            Self::Input => "Input",
            Self::Compressor => "Compressor",
            Self::Drive => "Drive",
            Self::Amp => "Amp",
            Self::Cabinet => "Cabinet",
            Self::Eq => "EQ",
            Self::Modulation => "Modulation",
            Self::Delay => "Delay",
            Self::Reverb => "Reverb",
            Self::Gate => "Gate",
            Self::Volume => "Volume",
            Self::Pitch => "Pitch",
            Self::Tremolo => "Tremolo",
            Self::Limiter => "Limiter",
            Self::Send => "Send",
            Self::Special => "Special",
            Self::Freeze => "Freeze",
            Self::Custom => "Custom",
            Self::DeEsser => "De-Esser",
            Self::Saturator => "Saturator",
            Self::Tuner => "Tuner",
            Self::Chorus => "Chorus",
            Self::Flanger => "Flanger",
            Self::Phaser => "Phaser",
            Self::RingModulator => "Ring Modulator",
            Self::Wah => "Wah",
            Self::Filter => "Filter",
            Self::Doubler => "Doubler",
            Self::Panner => "Panner",
            Self::Vibrato => "Vibrato",
            Self::Rotary => "Rotary",
            Self::Crossover => "Crossover",
            Self::Boost => "Boost",
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Block
// ─────────────────────────────────────────────────────────────────────────────

/// A single DSP processing unit in the signal chain.
///
/// Each block is backed by a plugin, has a bypass toggle, and holds a list of
/// parameter values. Parameters are stored by index into the plugin's parameter
/// spec list.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct Block {
    /// Unique identifier for this block instance.
    pub id: BlockId,
    /// User-facing name (e.g. "Main Drive", "Room Reverb").
    pub name: String,
    /// Optional short alias shown in the node graph instead of `name`
    /// (e.g. "Rescue-EQ", "Verb").
    pub alias: Option<String>,
    /// Optional human-readable description of the block's purpose.
    pub description: Option<String>,
    /// Plugin backing this block.
    pub plugin_id: PluginId,
    /// Optional preset name loaded in the plugin.
    pub plugin_preset: Option<String>,
    /// Whether the block is bypassed (signal passes through unprocessed).
    pub bypassed: bool,
    /// The processing category this block belongs to (Drive, Amp, Delay, etc.).
    pub block_type: BlockType,
    /// Current parameter values (sparse — only non-default values need storing).
    pub parameters: Vec<ParameterValue>,
    /// Tags for organizing and filtering.
    pub tags: Tags,
}

impl Block {
    /// Create a new block with the given name and plugin.
    ///
    /// Starts un-bypassed with no preset and no parameter overrides.
    pub fn new(name: impl Into<String>, plugin_id: PluginId) -> Self {
        Self {
            id: BlockId::new(),
            name: name.into(),
            alias: None,
            description: None,
            plugin_id,
            plugin_preset: None,
            bypassed: false,
            block_type: BlockType::Custom,
            parameters: Vec::new(),
            tags: Tags::new(),
        }
    }

    /// Set the block type category (builder pattern).
    #[must_use]
    pub fn with_block_type(mut self, block_type: BlockType) -> Self {
        self.block_type = block_type;
        self
    }

    /// Set a short alias for display in the node graph (builder pattern).
    #[must_use]
    pub fn with_alias(mut self, alias: impl Into<String>) -> Self {
        self.alias = Some(alias.into());
        self
    }

    /// Set a description of the block's purpose (builder pattern).
    #[must_use]
    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = Some(description.into());
        self
    }

    /// Returns the alias if set, otherwise the name.
    ///
    /// Use this for node graph labels where space is limited and the user
    /// may have given a shorter alias.
    pub fn display_name(&self) -> &str {
        self.alias.as_deref().unwrap_or(&self.name)
    }

    /// Whether this block is a placeholder (no real plugin assigned yet).
    pub fn is_placeholder(&self) -> bool {
        self.plugin_id.is_unassigned()
    }

    /// Set a parameter value by index.
    ///
    /// If a parameter with the given index already exists, its value is updated.
    /// Otherwise a new entry is added.
    pub fn set_parameter(&mut self, index: u32, value: f64) {
        if let Some(param) = self.parameters.iter_mut().find(|p| p.index == index) {
            param.value = crate::normalized::NormalizedF64::new(value);
        } else {
            self.parameters.push(ParameterValue::new(index, value));
        }
    }

    /// Get the current value of a parameter by index.
    ///
    /// Returns `None` if no value has been set for this index.
    pub fn get_parameter(&self, index: u32) -> Option<f64> {
        self.parameters
            .iter()
            .find(|p| p.index == index)
            .map(|p| p.value.get())
    }

    /// Toggle the bypass state, returning the new state.
    pub fn toggle_bypass(&mut self) -> bool {
        self.bypassed = !self.bypassed;
        self.bypassed
    }
}

impl Taggable for Block {
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
// GlobalBlock
// ─────────────────────────────────────────────────────────────────────────────

/// A block with a signal-chain ordering for the global block pool.
///
/// Global blocks are shared across presets and positioned in a fixed order.
#[derive(Debug, Clone, Facet)]
pub struct GlobalBlock {
    /// The underlying block.
    pub block: Block,
    /// Position in the global signal chain.
    pub order: Order,
    /// Tags for organizing and filtering.
    pub tags: Tags,
}

impl Taggable for GlobalBlock {
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
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    fn test_plugin() -> PluginId {
        PluginId::vst3("com.example.drive", "Example Drive")
    }

    // Block::set_parameter / get_parameter

    #[test]
    fn set_and_get_parameter() {
        let mut block = Block::new("Test", test_plugin());
        assert_eq!(block.get_parameter(0), None);

        block.set_parameter(0, 0.75);
        assert_eq!(block.get_parameter(0), Some(0.75));
    }

    #[test]
    fn set_parameter_updates_existing() {
        let mut block = Block::new("Test", test_plugin());
        block.set_parameter(0, 0.25);
        block.set_parameter(0, 0.9);

        assert_eq!(block.get_parameter(0), Some(0.9));
        assert_eq!(
            block.parameters.len(),
            1,
            "should update in place, not append"
        );
    }

    #[test]
    fn set_multiple_parameters() {
        let mut block = Block::new("Test", test_plugin());
        block.set_parameter(0, 0.1);
        block.set_parameter(5, 0.5);
        block.set_parameter(10, 0.9);

        assert_eq!(block.get_parameter(0), Some(0.1));
        assert_eq!(block.get_parameter(5), Some(0.5));
        assert_eq!(block.get_parameter(10), Some(0.9));
        assert_eq!(block.get_parameter(1), None);
        assert_eq!(block.parameters.len(), 3);
    }

    #[test]
    fn set_parameter_clamps_value() {
        let mut block = Block::new("Test", test_plugin());
        block.set_parameter(0, 1.5);
        assert_eq!(block.get_parameter(0), Some(1.0));

        block.set_parameter(1, -0.5);
        assert_eq!(block.get_parameter(1), Some(0.0));
    }

    // Block::toggle_bypass

    #[test]
    fn toggle_bypass() {
        let mut block = Block::new("Test", test_plugin());
        assert!(!block.bypassed);

        let new_state = block.toggle_bypass();
        assert!(new_state);
        assert!(block.bypassed);

        let new_state = block.toggle_bypass();
        assert!(!new_state);
        assert!(!block.bypassed);
    }

    // Block::new defaults

    #[test]
    fn new_block_defaults() {
        let block = Block::new("My Drive", test_plugin());
        assert_eq!(block.name, "My Drive");
        assert_eq!(block.plugin_id.format, PluginFormat::Vst3);
        assert_eq!(block.plugin_id.uid, "com.example.drive");
        assert_eq!(block.plugin_id.display_name, "Example Drive");
        assert!(block.plugin_preset.is_none());
        assert!(!block.bypassed);
        assert!(block.parameters.is_empty());
    }

    // PluginId constructors

    #[test]
    fn plugin_id_constructors() {
        let vst3 = PluginId::vst3("uid1", "VST3 Plugin");
        assert_eq!(vst3.format, PluginFormat::Vst3);

        let clap = PluginId::clap("uid2", "CLAP Plugin");
        assert_eq!(clap.format, PluginFormat::Clap);

        let js = PluginId::js("uid3", "JS Plugin");
        assert_eq!(js.format, PluginFormat::Js);
    }

    // BlockType default

    #[test]
    fn block_type_default_is_drive() {
        assert_eq!(BlockType::default(), BlockType::Drive);
    }

    // GlobalBlock

    #[test]
    fn global_block_holds_order() {
        let block = Block::new("Global EQ", PluginId::clap("eq", "EQ"));
        let global = GlobalBlock {
            block,
            order: Order::new(3),
            tags: Tags::new(),
        };
        assert_eq!(global.order.get(), 3);
        assert_eq!(global.block.name, "Global EQ");
    }
}
