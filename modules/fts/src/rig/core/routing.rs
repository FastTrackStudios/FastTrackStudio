//! Routing - Signal flow configuration
//!
//! Defines how layers are connected within a section (serial, parallel, custom).

use facet::Facet;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// How layers are routed within a section.
#[repr(u8)]
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub enum SectionRouting {
    /// Layer 1 → Layer 2 → Layer 3 (series)
    Serial,
    /// All layers mixed together (parallel)
    Parallel,
    /// Custom routing graph
    Custom(Vec<RoutingNode>),
}

/// A node in a custom routing graph.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct RoutingNode {
    /// Source of the signal
    pub source: BlockRef,
    /// Destination for the signal
    pub destination: BlockRef,
    /// Send level (0.0 - 1.0)
    pub level: f64,
}

/// Reference to a point in the signal chain.
#[repr(u8)]
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub enum BlockRef {
    /// Input to the section
    SectionInput,
    /// Output from the section
    SectionOutput,
    /// Output of a specific layer
    LayerOutput { layer_index: u8 },
    /// Input to a specific layer
    LayerInput { layer_index: u8 },
    /// Output of a specific block within a layer
    BlockOutput { layer_index: u8, block_id: Uuid },
    /// Input to a specific block within a layer
    BlockInput { layer_index: u8, block_id: Uuid },
    /// A global block output
    GlobalBlockOutput { block_id: Uuid },
    /// A global block input
    GlobalBlockInput { block_id: Uuid },
}

impl Default for SectionRouting {
    fn default() -> Self {
        Self::Parallel
    }
}

impl RoutingNode {
    /// Create a new routing node
    pub fn new(source: BlockRef, destination: BlockRef, level: f64) -> Self {
        Self {
            source,
            destination,
            level: level.clamp(0.0, 1.0),
        }
    }

    /// Create a routing node with unity gain
    pub fn unity(source: BlockRef, destination: BlockRef) -> Self {
        Self::new(source, destination, 1.0)
    }
}

impl SectionRouting {
    /// Check if this is serial routing
    pub fn is_serial(&self) -> bool {
        matches!(self, SectionRouting::Serial)
    }

    /// Check if this is parallel routing
    pub fn is_parallel(&self) -> bool {
        matches!(self, SectionRouting::Parallel)
    }

    /// Check if this is custom routing
    pub fn is_custom(&self) -> bool {
        matches!(self, SectionRouting::Custom(_))
    }
}
