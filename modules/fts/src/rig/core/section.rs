//! Section - A major part of the rig
//!
//! Sections represent distinct parts of the rig like "Amp", "Effects",
//! "Piano", "Organ", etc. Each section can have multiple layers.

use facet::Facet;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use super::{Layer, SectionRouting};

/// A major part of the rig that can contain multiple layers.
///
/// Sections can be enabled/disabled independently and have their own
/// volume control. Layers within a section can be routed in series,
/// parallel, or with custom routing.
///
/// # Examples
///
/// - `Amp` section with 1 layer (guitar amp + cab)
/// - `Effects` section with 2 layers (parallel effects chains)
/// - `Piano` section with 2 layers (acoustic + electric pianos)
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct Section {
    /// Unique identifier
    pub id: Uuid,
    /// Section name (e.g., "Amp", "Piano")
    pub name: String,
    /// Layers in this section
    pub layers: Vec<Layer>,
    /// How layers are routed within this section
    pub routing: SectionRouting,
    /// Whether this section is enabled
    pub enabled: bool,
    /// Section volume (0.0 - 1.0)
    pub volume: f64,
}

impl Section {
    /// Create a new section
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            layers: Vec::new(),
            routing: SectionRouting::Parallel,
            enabled: true,
            volume: 1.0,
        }
    }

    /// Create a section with a specific number of layers
    pub fn with_layers(name: impl Into<String>, layer_count: u8) -> Self {
        let name = name.into();
        let mut section = Self::new(name.clone());
        for i in 0..layer_count {
            section.add_layer(Layer::new(format!("{name} Layer {}", i + 1), i));
        }
        section
    }

    /// Add a layer to this section
    pub fn add_layer(&mut self, layer: Layer) {
        self.layers.push(layer);
    }

    /// Get a layer by index
    pub fn get_layer(&self, index: u8) -> Option<&Layer> {
        self.layers.iter().find(|l| l.index == index)
    }

    /// Get a mutable layer by index
    pub fn get_layer_mut(&mut self, index: u8) -> Option<&mut Layer> {
        self.layers.iter_mut().find(|l| l.index == index)
    }

    /// Get the number of layers
    pub fn layer_count(&self) -> usize {
        self.layers.len()
    }
}
