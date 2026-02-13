//! Layer domain — processing lanes that combine modules.
//!
//! A [`Layer`] groups module references and standalone block references
//! into a processing lane. Layers live inside Engines.
//!
//! [`LayerSnapshot`] captures a specific configuration of a Layer,
//! selecting which module/block variants to use plus optional overrides.

use facet::Facet;
use serde::{Deserialize, Serialize};

use crate::metadata::Metadata;
use crate::overrides::Override;
use crate::{ModulePresetId, ModuleSnapshotId, PresetId, SnapshotId};

// ─── IDs ────────────────────────────────────────────────────────

crate::typed_string_id!(
    /// Identifies a Layer collection.
    LayerId
);
crate::typed_string_id!(
    /// Identifies a specific Layer variant.
    LayerSnapshotId
);

// ─── Module reference ───────────────────────────────────────────

/// A reference to a specific module variant within a layer.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct ModuleRef {
    /// Which module collection to pull from.
    pub collection_id: ModulePresetId,
    /// Which variant within that collection. `None` = default variant.
    pub variant_id: Option<ModuleSnapshotId>,
}

impl ModuleRef {
    pub fn new(collection_id: impl Into<ModulePresetId>) -> Self {
        Self {
            collection_id: collection_id.into(),
            variant_id: None,
        }
    }

    #[must_use]
    pub fn with_variant(mut self, variant_id: impl Into<ModuleSnapshotId>) -> Self {
        self.variant_id = Some(variant_id.into());
        self
    }
}

// ─── Block reference ────────────────────────────────────────────

/// A reference to a specific standalone block variant within a layer.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct BlockRef {
    /// Which block collection to pull from.
    pub collection_id: PresetId,
    /// Which variant within that collection. `None` = default variant.
    pub variant_id: Option<SnapshotId>,
}

impl BlockRef {
    pub fn new(collection_id: impl Into<PresetId>) -> Self {
        Self {
            collection_id: collection_id.into(),
            variant_id: None,
        }
    }

    #[must_use]
    pub fn with_variant(mut self, variant_id: impl Into<SnapshotId>) -> Self {
        self.variant_id = Some(variant_id.into());
        self
    }
}

// ─── LayerSnapshot ───────────────────────────────────────────────

/// A specific configuration of a Layer — which modules and blocks to use.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct LayerSnapshot {
    pub id: LayerSnapshotId,
    pub name: String,
    pub module_refs: Vec<ModuleRef>,
    pub block_refs: Vec<BlockRef>,
    pub overrides: Vec<Override>,
    pub enabled: bool,
    pub metadata: Metadata,
}

impl LayerSnapshot {
    pub fn new(id: impl Into<LayerSnapshotId>, name: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            name: name.into(),
            module_refs: Vec::new(),
            block_refs: Vec::new(),
            overrides: Vec::new(),
            enabled: true,
            metadata: Metadata::new(),
        }
    }

    #[must_use]
    pub fn with_module(mut self, module_ref: ModuleRef) -> Self {
        self.module_refs.push(module_ref);
        self
    }

    #[must_use]
    pub fn with_block(mut self, block_ref: BlockRef) -> Self {
        self.block_refs.push(block_ref);
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

// ─── Layer ──────────────────────────────────────────────────────

/// A Layer collection — groups variants of a processing lane.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct Layer {
    pub id: LayerId,
    pub name: String,
    pub default_variant_id: LayerSnapshotId,
    pub variants: Vec<LayerSnapshot>,
    pub metadata: Metadata,
}

impl Layer {
    pub fn new(
        id: impl Into<LayerId>,
        name: impl Into<String>,
        default_variant: LayerSnapshot,
    ) -> Self {
        let default_variant_id = default_variant.id.clone();
        Self {
            id: id.into(),
            name: name.into(),
            default_variant_id,
            variants: vec![default_variant],
            metadata: Metadata::new(),
        }
    }

    pub fn add_variant(&mut self, variant: LayerSnapshot) {
        self.variants.push(variant);
    }

    pub fn default_variant(&self) -> Option<&LayerSnapshot> {
        self.variants
            .iter()
            .find(|v| v.id == self.default_variant_id)
    }

    pub fn variant(&self, id: &LayerSnapshotId) -> Option<&LayerSnapshot> {
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
    fn test_layer_creation() {
        let variant = LayerSnapshot::new("v1", "Default").with_module(ModuleRef::new("mod-drive"));

        let layer = Layer::new("layer-1", "Main Layer", variant);
        assert_eq!(layer.name, "Main Layer");
        assert_eq!(layer.variants.len(), 1);
        assert!(layer.default_variant().is_some());
    }

    #[test]
    fn test_layer_multiple_variants() {
        let v1 = LayerSnapshot::new("v1", "Clean");
        let v2 = LayerSnapshot::new("v2", "Heavy");

        let mut layer = Layer::new("layer-1", "Guitar", v1);
        layer.add_variant(v2);

        assert_eq!(layer.variants.len(), 2);
        assert!(layer.variant(&LayerSnapshotId::new("v2")).is_some());
    }
}
