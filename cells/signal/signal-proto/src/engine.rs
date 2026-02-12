//! Engine — a pure container for layers.
//!
//! Like a Nord synth engine with Layer A, B, C. An engine groups layers
//! by instrument type and always has at least one layer (enforced by
//! [`NonEmptyVec`]).

use crate::id::EngineId;
use crate::layer::Layer;
use crate::non_empty::NonEmptyVec;
use crate::rig::InstrumentType;
use crate::tags::{Taggable, Tags};
use crate::version::LayerIndex;

/// Alias for clarity — engine type is the instrument category.
pub type EngineType = InstrumentType;

// ─────────────────────────────────────────────────────────────────────────────
// Engine
// ─────────────────────────────────────────────────────────────────────────────

/// A container for layers, grouped by instrument type.
///
/// Engines always have at least one layer. The `NonEmptyVec` guarantees
/// `.first()` never fails.
#[derive(..Domain)]
pub struct Engine {
    pub id: EngineId,
    pub name: String,
    pub engine_type: EngineType,
    pub layers: NonEmptyVec<Layer>,
    pub tags: Tags,
}

impl Engine {
    /// Create an engine with its first (required) layer.
    pub fn new(name: impl Into<String>, engine_type: EngineType, first_layer: Layer) -> Self {
        Self {
            id: EngineId::new(),
            name: name.into(),
            engine_type,
            layers: NonEmptyVec::new(first_layer),
            tags: Tags::new(),
        }
    }

    /// Add a layer to this engine.
    pub fn add_layer(&mut self, layer: Layer) {
        self.layers.push(layer);
    }

    /// Get a layer by its 1-based index.
    pub fn layer(&self, index: LayerIndex) -> Option<&Layer> {
        self.layers.get(index.to_zero_based())
    }

    /// Get a layer by its 1-based index (mutable).
    pub fn layer_mut(&mut self, index: LayerIndex) -> Option<&mut Layer> {
        self.layers.get_mut(index.to_zero_based())
    }

    /// Number of layers in this engine.
    pub fn layer_count(&self) -> usize {
        self.layers.len()
    }
}

impl Taggable for Engine {
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
    use crate::rig::InstrumentType;

    #[test]
    fn engine_always_has_at_least_one_layer() {
        let layer = Layer::new("Main", LayerIndex::new(1));
        let engine = Engine::new("Guitar", InstrumentType::Guitar, layer);
        assert_eq!(engine.layers.len(), 1);
        assert_eq!(engine.layers.first().name, "Main");
    }

    #[test]
    fn engine_add_layers() {
        let layer1 = Layer::new("Main", LayerIndex::new(1));
        let mut engine = Engine::new("Guitar", InstrumentType::Guitar, layer1);
        engine.add_layer(Layer::new("Harmony", LayerIndex::new(2)));
        engine.add_layer(Layer::new("Octave", LayerIndex::new(3)));

        assert_eq!(engine.layer_count(), 3);
    }

    #[test]
    fn engine_layer_by_index() {
        let layer1 = Layer::new("Main", LayerIndex::new(1));
        let mut engine = Engine::new("Guitar", InstrumentType::Guitar, layer1);
        engine.add_layer(Layer::new("Harmony", LayerIndex::new(2)));

        // 1-based access
        assert_eq!(engine.layer(LayerIndex::new(1)).unwrap().name, "Main");
        assert_eq!(engine.layer(LayerIndex::new(2)).unwrap().name, "Harmony");
        assert!(engine.layer(LayerIndex::new(3)).is_none());
    }

    #[test]
    fn engine_layer_mut() {
        let layer1 = Layer::new("Main", LayerIndex::new(1));
        let mut engine = Engine::new("Guitar", InstrumentType::Guitar, layer1);

        engine.layer_mut(LayerIndex::new(1)).unwrap().enabled = false;
        assert!(!engine.layer(LayerIndex::new(1)).unwrap().enabled);
    }

    #[test]
    fn engine_type_is_instrument_type() {
        let layer = Layer::new("Main", LayerIndex::new(1));
        let engine = Engine::new("Synth Pad", InstrumentType::Synth, layer);
        assert_eq!(engine.engine_type, InstrumentType::Synth);
    }
}
