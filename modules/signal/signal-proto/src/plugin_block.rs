//! Plugin block definitions — maps a single DAW plugin's parameters
//! into virtual modules and blocks for UI organization.
//!
//! A [`PluginBlockDef`] is a self-contained JSON document that describes:
//! - The plugin identity (name, vendor, parameter count)
//! - Virtual modules grouping parameter subsets
//! - Virtual blocks within each module
//! - Parameter mappings from virtual block params to plugin param indices
//!
//! Plugin block defs are NOT stored in the database. They are embedded
//! inline in [`LayerSnapshot`](crate::layer::LayerSnapshot) as JSON, or
//! saved/loaded as standalone `.json` files.

use facet::Facet;
use serde::{Deserialize, Serialize};

use crate::{Block, BlockParameter, BlockType, ModuleBlock, ModuleBlockSource, ModuleType};
use crate::{SignalChain, SignalNode};

// ─── ID ─────────────────────────────────────────────────────────

crate::typed_uuid_id!(
    /// Identifies a plugin block definition.
    PluginBlockDefId
);

// ─── ParamMapping ───────────────────────────────────────────────

/// Maps a virtual block parameter to a real plugin parameter.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct ParamMapping {
    /// Human-readable parameter name for UI display.
    pub name: String,
    /// Index into the real plugin's parameter array.
    pub plugin_param_index: u32,
    /// Default normalized value (0.0..1.0).
    pub default_value: f32,
}

impl ParamMapping {
    pub fn new(name: impl Into<String>, plugin_param_index: u32, default_value: f32) -> Self {
        Self {
            name: name.into(),
            plugin_param_index,
            default_value: default_value.clamp(0.0, 1.0),
        }
    }
}

// ─── VirtualBlock ───────────────────────────────────────────────

/// A virtual block within a virtual module — controls a subset of plugin params.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct VirtualBlock {
    /// Unique ID within this plugin block def (e.g., "justa-boost").
    pub id: String,
    /// Display label (e.g., "Justa Boost").
    pub label: String,
    /// Block type for color/category in the grid.
    pub block_type: BlockType,
    /// Parameter mappings to the real plugin.
    pub params: Vec<ParamMapping>,
    /// Whether this block is currently enabled/active.
    #[serde(default = "default_true")]
    pub enabled: bool,
}

fn default_true() -> bool {
    true
}

impl VirtualBlock {
    pub fn new(id: impl Into<String>, label: impl Into<String>, block_type: BlockType) -> Self {
        Self {
            id: id.into(),
            label: label.into(),
            block_type,
            params: Vec::new(),
            enabled: true,
        }
    }

    #[must_use]
    pub fn with_param(mut self, mapping: ParamMapping) -> Self {
        self.params.push(mapping);
        self
    }

    #[must_use]
    pub fn with_params(mut self, mappings: Vec<ParamMapping>) -> Self {
        self.params.extend(mappings);
        self
    }
}

// ─── VirtualModule ──────────────────────────────────────────────

/// A virtual module grouping virtual blocks.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct VirtualModule {
    /// Unique ID within this plugin block def (e.g., "pedals").
    pub id: String,
    /// Display label (e.g., "Pedals").
    pub label: String,
    /// Module type for color/grouping in the grid.
    pub module_type: ModuleType,
    /// Ordered list of virtual blocks in this module.
    pub blocks: Vec<VirtualBlock>,
}

impl VirtualModule {
    pub fn new(id: impl Into<String>, label: impl Into<String>, module_type: ModuleType) -> Self {
        Self {
            id: id.into(),
            label: label.into(),
            module_type,
            blocks: Vec::new(),
        }
    }

    #[must_use]
    pub fn with_block(mut self, block: VirtualBlock) -> Self {
        self.blocks.push(block);
        self
    }
}

// ─── PluginBlockDef ─────────────────────────────────────────────

/// Complete definition of how a single plugin maps to virtual modules/blocks.
///
/// This is the top-level JSON-serializable document. It is NOT stored in the
/// database — it lives as a JSON file or is embedded inline in a LayerSnapshot.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct PluginBlockDef {
    /// Unique identifier for this definition.
    pub id: PluginBlockDefId,
    /// The real plugin name as reported by the DAW.
    pub plugin_name: String,
    /// Plugin vendor (e.g., "Neural DSP").
    pub vendor: Option<String>,
    /// Total parameter count of the real plugin (for validation).
    pub param_count: u32,
    /// Virtual modules organizing this plugin's parameters.
    pub modules: Vec<VirtualModule>,
    /// Schema version for forward compatibility.
    #[serde(default = "default_version")]
    pub version: u32,
}

fn default_version() -> u32 {
    1
}

impl PluginBlockDef {
    pub fn new(plugin_name: impl Into<String>, param_count: u32) -> Self {
        Self {
            id: PluginBlockDefId::new(),
            plugin_name: plugin_name.into(),
            vendor: None,
            param_count,
            modules: Vec::new(),
            version: 1,
        }
    }

    #[must_use]
    pub fn with_vendor(mut self, vendor: impl Into<String>) -> Self {
        self.vendor = Some(vendor.into());
        self
    }

    #[must_use]
    pub fn with_module(mut self, module: VirtualModule) -> Self {
        self.modules.push(module);
        self
    }

    /// All virtual blocks across all modules, in order.
    pub fn all_blocks(&self) -> Vec<&VirtualBlock> {
        self.modules.iter().flat_map(|m| &m.blocks).collect()
    }

    /// Validate that no parameter index exceeds `param_count` and no index is mapped twice.
    pub fn validate(&self) -> Result<(), PluginBlockDefError> {
        let mut seen = std::collections::HashSet::new();
        for block in self.all_blocks() {
            for param in &block.params {
                if param.plugin_param_index >= self.param_count {
                    return Err(PluginBlockDefError::IndexOutOfRange {
                        block_id: block.id.clone(),
                        param_name: param.name.clone(),
                        index: param.plugin_param_index,
                        max: self.param_count,
                    });
                }
                if !seen.insert(param.plugin_param_index) {
                    return Err(PluginBlockDefError::DuplicateIndex {
                        index: param.plugin_param_index,
                        block_id: block.id.clone(),
                    });
                }
            }
        }
        Ok(())
    }

    /// Convert this definition into `(label, module_type, SignalChain)` tuples
    /// suitable for the grid rendering pipeline.
    ///
    /// Each `VirtualModule` becomes a tuple. Each `VirtualBlock` becomes a
    /// `SignalNode::Block(ModuleBlock)` with `ModuleBlockSource::Inline`,
    /// carrying the virtual block's parameters as `BlockParameter`s.
    pub fn to_module_chains(&self) -> Vec<(String, ModuleType, SignalChain)> {
        self.modules
            .iter()
            .map(|vm| {
                let blocks: Vec<ModuleBlock> = vm
                    .blocks
                    .iter()
                    .map(|vb| {
                        let block = Block::from_parameters(
                            vb.params
                                .iter()
                                .map(|p| {
                                    BlockParameter::new(
                                        format!("p{}", p.plugin_param_index),
                                        &p.name,
                                        p.default_value,
                                    )
                                })
                                .collect(),
                        );
                        ModuleBlock::new(
                            &vb.id,
                            &vb.label,
                            vb.block_type,
                            ModuleBlockSource::Inline { block },
                        )
                    })
                    .collect();

                let nodes: Vec<SignalNode> = blocks.into_iter().map(SignalNode::Block).collect();
                let chain = SignalChain::new(nodes);
                (vm.label.clone(), vm.module_type, chain)
            })
            .collect()
    }
}

// ─── Errors ─────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
pub enum PluginBlockDefError {
    IndexOutOfRange {
        block_id: String,
        param_name: String,
        index: u32,
        max: u32,
    },
    DuplicateIndex {
        index: u32,
        block_id: String,
    },
}

impl std::fmt::Display for PluginBlockDefError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IndexOutOfRange {
                block_id,
                param_name,
                index,
                max,
            } => {
                write!(
                    f,
                    "param '{param_name}' in block '{block_id}' has index {index} >= max {max}"
                )
            }
            Self::DuplicateIndex { index, block_id } => {
                write!(
                    f,
                    "param index {index} mapped multiple times (found in block '{block_id}')"
                )
            }
        }
    }
}

impl std::error::Error for PluginBlockDefError {}
