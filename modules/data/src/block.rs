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

// ─────────────────────────────────────────────────────────────────────────────
// PluginFormat
// ─────────────────────────────────────────────────────────────────────────────

/// Audio plugin format.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Facet)]
#[repr(u8)]
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
    pub fn new(format: PluginFormat, uid: impl Into<String>, display_name: impl Into<String>) -> Self {
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
}

// ─────────────────────────────────────────────────────────────────────────────
// BlockType
// ─────────────────────────────────────────────────────────────────────────────

/// Functional category of a DSP block.
///
/// Used for UI grouping, icon selection, and signal-chain validation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Facet, Default)]
#[repr(u8)]
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
    Custom,
}

// ─────────────────────────────────────────────────────────────────────────────
// Block
// ─────────────────────────────────────────────────────────────────────────────

/// A single DSP processing unit in the signal chain.
///
/// Each block is backed by a plugin, has a bypass toggle, and holds a list of
/// parameter values. Parameters are stored by index into the plugin's parameter
/// spec list.
#[derive(Debug, Clone, Facet)]
pub struct Block {
    /// Unique identifier for this block instance.
    pub id: BlockId,
    /// User-facing name (e.g. "Main Drive", "Room Reverb").
    pub name: String,
    /// Plugin backing this block.
    pub plugin_id: PluginId,
    /// Optional preset name loaded in the plugin.
    pub plugin_preset: Option<String>,
    /// Whether the block is bypassed (signal passes through unprocessed).
    pub bypassed: bool,
    /// Current parameter values (sparse — only non-default values need storing).
    pub parameters: Vec<ParameterValue>,
}

impl Block {
    /// Create a new block with the given name and plugin.
    ///
    /// Starts un-bypassed with no preset and no parameter overrides.
    pub fn new(name: impl Into<String>, plugin_id: PluginId) -> Self {
        Self {
            id: BlockId::new(),
            name: name.into(),
            plugin_id,
            plugin_preset: None,
            bypassed: false,
            parameters: Vec::new(),
        }
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
        assert_eq!(block.parameters.len(), 1, "should update in place, not append");
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
        let global = GlobalBlock { block, order: Order::new(3) };
        assert_eq!(global.order.get(), 3);
        assert_eq!(global.block.name, "Global EQ");
    }

}
