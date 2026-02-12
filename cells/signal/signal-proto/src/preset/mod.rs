//! Preset system — level-specific presets with shared metadata.
//!
//! Each level of the hierarchy has its own preset type. A preset is a named,
//! versioned wrapper around a snapshot (for blocks/modules) or scene (for
//! layers/engines/rigs/racks).
//!
//! All presets share [`PresetMetadata`] for name, category, tags, rating, etc.

pub mod builder;


use crate::category::PresetCategory;
use crate::id::*;
use crate::module::ModuleType;
use crate::normalized::Rating;
use crate::scene::{EngineScene, LayerScene, RackScene, RigScene};
use crate::snapshot::{BlockSnapshot, ModuleSnapshot};
use crate::tags::Tags;

// ─── PresetMetadata ──────────────────────────────────────────────

/// Shared metadata for all preset types.
#[derive(Debug, Clone, ::facet::Facet)]
pub struct PresetMetadata {
    pub name: String,
    pub category: PresetCategory,
    pub author: Option<String>,
    pub description: Option<String>,
    pub rating: Rating,
    pub tags: Tags,
}

impl PresetMetadata {
    pub fn new(name: impl Into<String>, category: PresetCategory) -> Self {
        Self {
            name: name.into(),
            category,
            author: None,
            description: None,
            rating: Rating::default(),
            tags: Tags::new(),
        }
    }

    #[must_use]
    pub fn with_author(mut self, author: impl Into<String>) -> Self {
        self.author = Some(author.into());
        self
    }

    #[must_use]
    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = Some(description.into());
        self
    }

    #[must_use]
    pub fn with_rating(mut self, rating: Rating) -> Self {
        self.rating = rating;
        self
    }
}

// ─── Block Preset ────────────────────────────────────────────────

/// A named, versioned block snapshot.
#[derive(Debug, Clone, ::facet::Facet)]
pub struct BlockPreset {
    pub id: BlockPresetId,
    pub metadata: PresetMetadata,
    pub snapshot: BlockSnapshot,
    pub version: u32,
}

impl BlockPreset {
    pub fn new(metadata: PresetMetadata, snapshot: BlockSnapshot) -> Self {
        Self {
            id: BlockPresetId::new(),
            metadata,
            snapshot,
            version: 1,
        }
    }
}

// ─── Module Preset ───────────────────────────────────────────────

/// A named, versioned module snapshot.
#[derive(Debug, Clone, ::facet::Facet)]
pub struct ModulePreset {
    pub id: ModulePresetId,
    pub metadata: PresetMetadata,
    pub module_type: ModuleType,
    pub snapshot: ModuleSnapshot,
    pub version: u32,
}

impl ModulePreset {
    pub fn new(
        metadata: PresetMetadata,
        module_type: ModuleType,
        snapshot: ModuleSnapshot,
    ) -> Self {
        Self {
            id: ModulePresetId::new(),
            metadata,
            module_type,
            snapshot,
            version: 1,
        }
    }
}

// ─── Layer Preset ────────────────────────────────────────────────

/// A named, versioned layer scene.
#[derive(Debug, Clone, ::facet::Facet)]
pub struct LayerPreset {
    pub id: LayerPresetId,
    pub metadata: PresetMetadata,
    pub scene: LayerScene,
    pub version: u32,
}

impl LayerPreset {
    pub fn new(metadata: PresetMetadata, scene: LayerScene) -> Self {
        Self {
            id: LayerPresetId::new(),
            metadata,
            scene,
            version: 1,
        }
    }
}

// ─── Engine Preset ───────────────────────────────────────────────

/// A named, versioned engine scene.
#[derive(Debug, Clone, ::facet::Facet)]
pub struct EnginePreset {
    pub id: EnginePresetId,
    pub metadata: PresetMetadata,
    pub scene: EngineScene,
    pub version: u32,
}

impl EnginePreset {
    pub fn new(metadata: PresetMetadata, scene: EngineScene) -> Self {
        Self {
            id: EnginePresetId::new(),
            metadata,
            scene,
            version: 1,
        }
    }
}

// ─── Rig Preset ──────────────────────────────────────────────────

/// A named, versioned rig scene.
#[derive(Debug, Clone, ::facet::Facet)]
pub struct RigPreset {
    pub id: RigPresetId,
    pub metadata: PresetMetadata,
    pub scene: RigScene,
    pub version: u32,
}

impl RigPreset {
    pub fn new(metadata: PresetMetadata, scene: RigScene) -> Self {
        Self {
            id: RigPresetId::new(),
            metadata,
            scene,
            version: 1,
        }
    }
}

// ─── Rack Preset ─────────────────────────────────────────────────

/// A named, versioned rack scene.
#[derive(Debug, Clone, ::facet::Facet)]
pub struct RackPreset {
    pub id: RackPresetId,
    pub metadata: PresetMetadata,
    pub scene: RackScene,
    pub version: u32,
}

impl RackPreset {
    pub fn new(metadata: PresetMetadata, scene: RackScene) -> Self {
        Self {
            id: RackPresetId::new(),
            metadata,
            scene,
            version: 1,
        }
    }
}

// ─── Tests ───────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::PluginId;
    use crate::category::BaseTone;
    use crate::scene::LayerSceneBuilder;

    #[test]
    fn block_preset_creation() {
        let snap = BlockSnapshot::new(
            "Clean Tone",
            crate::id::BlockId::new(),
            PluginId::vst3("x", "X"),
        );
        let meta = PresetMetadata::new(
            "My Clean",
            PresetCategory::Generic {
                base_tone: BaseTone::Clean,
            },
        );
        let preset = BlockPreset::new(meta, snap);
        assert_eq!(preset.metadata.name, "My Clean");
        assert_eq!(preset.version, 1);
    }

    #[test]
    fn layer_preset_creation() {
        let scene = LayerSceneBuilder::new("Test")
            .modules(vec![])
            .no_standalone_blocks()
            .build();
        let meta = PresetMetadata::new(
            "Clean Layer",
            PresetCategory::Generic {
                base_tone: BaseTone::Clean,
            },
        )
        .with_author("Test Author")
        .with_description("A clean layer preset");

        let preset = LayerPreset::new(meta, scene);
        assert_eq!(preset.metadata.name, "Clean Layer");
        assert_eq!(preset.metadata.author.as_deref(), Some("Test Author"));
    }

    #[test]
    fn metadata_defaults() {
        let meta = PresetMetadata::new(
            "Test",
            PresetCategory::Generic {
                base_tone: BaseTone::Clean,
            },
        );
        assert!(meta.author.is_none());
        assert!(meta.description.is_none());
    }
}
