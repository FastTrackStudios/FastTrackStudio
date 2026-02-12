//! Routing — signal flow within a section.
//!
//! Defines how audio flows between layers, blocks, and global effects.


use crate::id::BlockId;

// ─────────────────────────────────────────────────────────────────────────────
// SectionRouting
// ─────────────────────────────────────────────────────────────────────────────

/// How audio flows through a section's layers.
#[derive(Debug, Clone, ::facet::Facet, Default)]
#[repr(u8)]
pub enum SectionRouting {
    /// Layers run in series: Layer 1 → Layer 2 → ... → Output
    #[default]
    Serial,
    /// Layers run in parallel and are summed at the output
    Parallel,
    /// Custom routing graph
    Custom(Vec<RoutingNode>),
}

impl SectionRouting {
    pub fn is_serial(&self) -> bool {
        matches!(self, Self::Serial)
    }

    pub fn is_parallel(&self) -> bool {
        matches!(self, Self::Parallel)
    }

    pub fn is_custom(&self) -> bool {
        matches!(self, Self::Custom(_))
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// RoutingNode
// ─────────────────────────────────────────────────────────────────────────────

/// A single connection in a custom routing graph.
#[derive(Debug, Clone, ::facet::Facet)]
pub struct RoutingNode {
    pub source: BlockRef,
    pub destination: BlockRef,
    pub level: f64,
}

impl RoutingNode {
    pub fn new(source: BlockRef, destination: BlockRef, level: f64) -> Self {
        Self {
            source,
            destination,
            level: level.clamp(0.0, 1.0),
        }
    }

    /// Unity-gain connection.
    pub fn unity(source: BlockRef, destination: BlockRef) -> Self {
        Self::new(source, destination, 1.0)
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// BlockRef — what a routing node connects to
// ─────────────────────────────────────────────────────────────────────────────

/// A reference to a point in the signal chain.
#[derive(Debug, Clone, ::facet::Facet)]
#[repr(u8)]
pub enum BlockRef {
    /// The section's audio input
    SectionInput,
    /// The section's audio output
    SectionOutput,
    /// Output of a specific layer
    LayerOutput { layer_index: u8 },
    /// Input of a specific layer
    LayerInput { layer_index: u8 },
    /// Output of a block within a layer
    BlockOutput { layer_index: u8, block_id: BlockId },
    /// Input of a block within a layer
    BlockInput { layer_index: u8, block_id: BlockId },
    /// Output of a global block
    GlobalBlockOutput { block_id: BlockId },
    /// Input of a global block
    GlobalBlockInput { block_id: BlockId },
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_routing_is_serial() {
        assert!(SectionRouting::default().is_serial());
    }

    #[test]
    fn routing_type_checks() {
        assert!(SectionRouting::Serial.is_serial());
        assert!(!SectionRouting::Serial.is_parallel());
        assert!(SectionRouting::Parallel.is_parallel());
        assert!(SectionRouting::Custom(vec![]).is_custom());
    }

    #[test]
    fn routing_node_clamps_level() {
        let node = RoutingNode::new(BlockRef::SectionInput, BlockRef::SectionOutput, 2.0);
        assert_eq!(node.level, 1.0);

        let node = RoutingNode::new(BlockRef::SectionInput, BlockRef::SectionOutput, -1.0);
        assert_eq!(node.level, 0.0);
    }

    #[test]
    fn unity_gain() {
        let node = RoutingNode::unity(BlockRef::SectionInput, BlockRef::SectionOutput);
        assert_eq!(node.level, 1.0);
    }
}
