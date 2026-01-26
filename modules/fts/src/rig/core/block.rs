//! Block - Single DSP processing unit
//!
//! A Block represents a single plugin instance, either local to a layer
//! or global (shared by multiple layers via sends).

use facet::Facet;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// A single DSP processing unit (plugin instance).
///
/// Blocks are the fundamental building blocks of the signal chain.
/// They can be arranged in series/parallel within layers or used as
/// global shared effects.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct Block {
    /// Unique identifier
    pub id: Uuid,
    /// Block name (e.g., "Main Reverb", "Crunch Amp")
    pub name: String,
    /// Plugin identification
    pub plugin_id: PluginId,
    /// Plugin's internal preset name (if using one)
    pub plugin_preset: Option<String>,
    /// Whether this block is bypassed
    pub bypassed: bool,
    /// Current parameter values (indexed by parameter ID)
    pub parameters: Vec<ParameterValue>,
}

/// A global block that can receive sends from multiple layers.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct GlobalBlock {
    /// The underlying block
    pub block: Block,
    /// Position in the global effects chain (lower = earlier)
    pub order: u8,
}

/// Platform-agnostic plugin identifier.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Facet)]
pub struct PluginId {
    /// Plugin format
    pub format: PluginFormat,
    /// Unique identifier (VST3 uid, CLAP id, etc.)
    pub uid: String,
    /// Human-readable display name
    pub display_name: String,
}

/// Plugin format types.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Facet)]
pub enum PluginFormat {
    /// VST3 plugin
    Vst3,
    /// CLAP plugin
    Clap,
    /// Audio Unit (macOS)
    Au,
    /// REAPER JS effect
    Js,
}

/// Stored parameter value.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct ParameterValue {
    /// Parameter index within the plugin
    pub index: u32,
    /// Normalized value (0.0 - 1.0)
    pub value: f64,
}

impl Block {
    /// Create a new block
    pub fn new(name: impl Into<String>, plugin_id: PluginId) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            plugin_id,
            plugin_preset: None,
            bypassed: false,
            parameters: Vec::new(),
        }
    }

    /// Set a parameter value
    pub fn set_parameter(&mut self, index: u32, value: f64) {
        let value = value.clamp(0.0, 1.0);
        if let Some(param) = self.parameters.iter_mut().find(|p| p.index == index) {
            param.value = value;
        } else {
            self.parameters.push(ParameterValue { index, value });
        }
    }

    /// Get a parameter value
    pub fn get_parameter(&self, index: u32) -> Option<f64> {
        self.parameters.iter().find(|p| p.index == index).map(|p| p.value)
    }

    /// Toggle bypass state
    pub fn toggle_bypass(&mut self) {
        self.bypassed = !self.bypassed;
    }
}

impl GlobalBlock {
    /// Create a new global block
    pub fn new(block: Block, order: u8) -> Self {
        Self { block, order }
    }
}

impl PluginId {
    /// Create a new plugin ID
    pub fn new(format: PluginFormat, uid: impl Into<String>, display_name: impl Into<String>) -> Self {
        Self {
            format,
            uid: uid.into(),
            display_name: display_name.into(),
        }
    }

    /// Create a VST3 plugin ID
    pub fn vst3(uid: impl Into<String>, display_name: impl Into<String>) -> Self {
        Self::new(PluginFormat::Vst3, uid, display_name)
    }

    /// Create a CLAP plugin ID
    pub fn clap(uid: impl Into<String>, display_name: impl Into<String>) -> Self {
        Self::new(PluginFormat::Clap, uid, display_name)
    }

    /// Create a REAPER JS plugin ID
    pub fn js(path: impl Into<String>, display_name: impl Into<String>) -> Self {
        Self::new(PluginFormat::Js, path, display_name)
    }
}

impl std::fmt::Display for PluginFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PluginFormat::Vst3 => write!(f, "VST3"),
            PluginFormat::Clap => write!(f, "CLAP"),
            PluginFormat::Au => write!(f, "AU"),
            PluginFormat::Js => write!(f, "JS"),
        }
    }
}

impl ParameterValue {
    /// Create a new parameter value
    pub fn new(index: u32, value: f64) -> Self {
        Self {
            index,
            value: value.clamp(0.0, 1.0),
        }
    }
}
