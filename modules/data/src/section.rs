//! Section — a group of layers with routing.
//!
//! A section must have at least one layer, enforced by `NonEmptyVec<Layer>`.

use facet::Facet;

use crate::id::{LayerId, SectionId};
use crate::layer::Layer;
use crate::non_empty::NonEmptyVec;
use crate::normalized::NormalizedF64;
use crate::routing::SectionRouting;

/// A group of layers with shared routing.
///
/// Invariant: always has at least one layer (`NonEmptyVec`).
#[derive(Debug, Clone, Facet)]
pub struct Section {
    pub id: SectionId,
    pub name: String,
    /// At least one layer. Enforced by `NonEmptyVec`.
    pub layers: NonEmptyVec<Layer>,
    pub routing: SectionRouting,
    pub enabled: bool,
    pub volume: NormalizedF64,
}

impl Section {
    /// Create a section with one initial layer.
    pub fn new(name: impl Into<String>, first_layer: Layer) -> Self {
        Self {
            id: SectionId::new(),
            name: name.into(),
            layers: NonEmptyVec::new(first_layer),
            routing: SectionRouting::default(),
            enabled: true,
            volume: NormalizedF64::ONE,
        }
    }

    /// Create a section with multiple layers. Returns `None` if `layers` is empty.
    pub fn with_layers(name: impl Into<String>, layers: Vec<Layer>) -> Option<Self> {
        let layers = NonEmptyVec::from_vec(layers)?;
        Some(Self {
            id: SectionId::new(),
            name: name.into(),
            layers,
            routing: SectionRouting::default(),
            enabled: true,
            volume: NormalizedF64::ONE,
        })
    }

    /// Add a layer to the section.
    pub fn add_layer(&mut self, layer: Layer) {
        self.layers.push(layer);
    }

    /// Get a layer by ID.
    pub fn get_layer(&self, id: LayerId) -> Option<&Layer> {
        self.layers.iter().find(|l| l.id == id)
    }

    /// Get a mutable layer by ID.
    pub fn get_layer_mut(&mut self, id: LayerId) -> Option<&mut Layer> {
        self.layers.iter_mut().find(|l| l.id == id)
    }

    /// Number of layers (always >= 1).
    pub fn layer_count(&self) -> usize {
        self.layers.len()
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn section_has_at_least_one_layer() {
        let layer = Layer::new("Layer 1", 0);
        let section = Section::new("Amp Section", layer);
        assert_eq!(section.layer_count(), 1);
    }

    #[test]
    fn with_layers_empty_returns_none() {
        let result = Section::with_layers("Empty", vec![]);
        assert!(result.is_none());
    }

    #[test]
    fn with_layers_nonempty() {
        let layers = vec![Layer::new("L1", 0), Layer::new("L2", 1)];
        let section = Section::with_layers("Dual", layers).unwrap();
        assert_eq!(section.layer_count(), 2);
    }

    #[test]
    fn add_layer() {
        let mut section = Section::new("Test", Layer::new("L1", 0));
        section.add_layer(Layer::new("L2", 1));
        assert_eq!(section.layer_count(), 2);
    }

    #[test]
    fn get_layer_by_id() {
        let layer = Layer::new("Main", 0);
        let layer_id = layer.id;
        let section = Section::new("Test", layer);
        assert!(section.get_layer(layer_id).is_some());
        assert!(section.get_layer(LayerId::new()).is_none());
    }

    #[test]
    fn defaults() {
        let section = Section::new("Test", Layer::new("L1", 0));
        assert!(section.enabled);
        assert_eq!(section.volume.get(), 1.0);
        assert!(section.routing.is_serial());
    }
}
