//! Engine domain — containers for layers with scene-style variants.
//!
//! An [`Engine`] groups one or more Layers. [`EngineScene`] selects which
//! layer variant to use for each layer, forming a "scene" of the engine.

use facet::Facet;
use serde::{Deserialize, Serialize};

use crate::layer::{LayerId, LayerSnapshotId};
use crate::metadata::Metadata;
use crate::overrides::Override;

// ─── IDs ────────────────────────────────────────────────────────

crate::typed_string_id!(
    /// Identifies an Engine collection.
    EngineId
);
crate::typed_string_id!(
    /// Identifies a specific Engine variant (scene).
    EngineSceneId
);

// ─── Layer selection ────────────────────────────────────────────

/// Which variant to use for a specific layer within an engine scene.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct LayerSelection {
    pub layer_id: LayerId,
    pub variant_id: LayerSnapshotId,
}

impl LayerSelection {
    pub fn new(layer_id: impl Into<LayerId>, variant_id: impl Into<LayerSnapshotId>) -> Self {
        Self {
            layer_id: layer_id.into(),
            variant_id: variant_id.into(),
        }
    }
}

// ─── EngineScene ──────────────────────────────────────────────

/// A scene-style variant for an Engine — selects layer variants and overrides.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct EngineScene {
    pub id: EngineSceneId,
    pub name: String,
    pub layer_selections: Vec<LayerSelection>,
    pub overrides: Vec<Override>,
    pub metadata: Metadata,
}

impl EngineScene {
    pub fn new(id: impl Into<EngineSceneId>, name: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            name: name.into(),
            layer_selections: Vec::new(),
            overrides: Vec::new(),
            metadata: Metadata::new(),
        }
    }

    #[must_use]
    pub fn with_layer(mut self, selection: LayerSelection) -> Self {
        self.layer_selections.push(selection);
        self
    }

    #[must_use]
    pub fn with_override(mut self, ov: Override) -> Self {
        self.overrides.push(ov);
        self
    }

    #[must_use]
    pub fn with_metadata(mut self, metadata: Metadata) -> Self {
        self.metadata = metadata;
        self
    }
}

// ─── Engine ─────────────────────────────────────────────────────

/// An Engine collection — groups layers and provides scene-style variants.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct Engine {
    pub id: EngineId,
    pub name: String,
    pub layer_ids: Vec<LayerId>,
    pub default_variant_id: EngineSceneId,
    pub variants: Vec<EngineScene>,
    pub metadata: Metadata,
}

impl Engine {
    pub fn new(
        id: impl Into<EngineId>,
        name: impl Into<String>,
        layer_ids: Vec<LayerId>,
        default_variant: EngineScene,
    ) -> Self {
        let default_variant_id = default_variant.id.clone();
        Self {
            id: id.into(),
            name: name.into(),
            layer_ids,
            default_variant_id,
            variants: vec![default_variant],
            metadata: Metadata::new(),
        }
    }

    pub fn add_variant(&mut self, variant: EngineScene) {
        self.variants.push(variant);
    }

    pub fn default_variant(&self) -> Option<&EngineScene> {
        self.variants
            .iter()
            .find(|v| v.id == self.default_variant_id)
    }

    pub fn variant(&self, id: &EngineSceneId) -> Option<&EngineScene> {
        self.variants.iter().find(|v| &v.id == id)
    }

    #[must_use]
    pub fn with_metadata(mut self, metadata: Metadata) -> Self {
        self.metadata = metadata;
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_engine_creation() {
        let variant = EngineScene::new("ev1", "Default Scene")
            .with_layer(LayerSelection::new("layer-1", "v1"));

        let engine = Engine::new(
            "engine-1",
            "Guitar Engine",
            vec![LayerId::new("layer-1")],
            variant,
        );

        assert_eq!(engine.name, "Guitar Engine");
        assert_eq!(engine.layer_ids.len(), 1);
        assert!(engine.default_variant().is_some());
    }
}
