//! Rig domain — complete instrument setups with scene-style variants.
//!
//! A [`Rig`] groups one or more Engines. [`RigScene`] selects which
//! engine variant to use for each engine, forming a top-level "scene".

use facet::Facet;
use serde::{Deserialize, Serialize};

use crate::engine::{EngineId, EngineSceneId};
use crate::metadata::Metadata;
use crate::overrides::Override;

// ─── IDs ────────────────────────────────────────────────────────

crate::typed_string_id!(
    /// Identifies a Rig collection.
    RigId
);
crate::typed_string_id!(
    /// Identifies a specific Rig variant (scene).
    RigSceneId
);
crate::typed_string_id!(
    /// Categorizes a Rig by instrument type (e.g. "guitar", "bass", "keys").
    RigTypeId
);

// ─── Engine selection ───────────────────────────────────────────

/// Which variant to use for a specific engine within a rig scene.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct EngineSelection {
    pub engine_id: EngineId,
    pub variant_id: EngineSceneId,
}

impl EngineSelection {
    pub fn new(engine_id: impl Into<EngineId>, variant_id: impl Into<EngineSceneId>) -> Self {
        Self {
            engine_id: engine_id.into(),
            variant_id: variant_id.into(),
        }
    }
}

// ─── RigScene ─────────────────────────────────────────────────

/// A scene-style variant for a Rig — selects engine variants and overrides.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct RigScene {
    pub id: RigSceneId,
    pub name: String,
    pub engine_selections: Vec<EngineSelection>,
    pub overrides: Vec<Override>,
    pub metadata: Metadata,
}

impl RigScene {
    pub fn new(id: impl Into<RigSceneId>, name: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            name: name.into(),
            engine_selections: Vec::new(),
            overrides: Vec::new(),
            metadata: Metadata::new(),
        }
    }

    #[must_use]
    pub fn with_engine(mut self, selection: EngineSelection) -> Self {
        self.engine_selections.push(selection);
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

// ─── Rig ────────────────────────────────────────────────────────

/// A Rig collection — a complete instrument with engines and scene variants.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct Rig {
    pub id: RigId,
    pub name: String,
    pub rig_type_id: Option<RigTypeId>,
    pub engine_ids: Vec<EngineId>,
    pub default_variant_id: RigSceneId,
    pub variants: Vec<RigScene>,
    pub metadata: Metadata,
}

impl Rig {
    pub fn new(
        id: impl Into<RigId>,
        name: impl Into<String>,
        engine_ids: Vec<EngineId>,
        default_variant: RigScene,
    ) -> Self {
        let default_variant_id = default_variant.id.clone();
        Self {
            id: id.into(),
            name: name.into(),
            rig_type_id: None,
            engine_ids,
            default_variant_id,
            variants: vec![default_variant],
            metadata: Metadata::new(),
        }
    }

    pub fn add_variant(&mut self, variant: RigScene) {
        self.variants.push(variant);
    }

    pub fn default_variant(&self) -> Option<&RigScene> {
        self.variants
            .iter()
            .find(|v| v.id == self.default_variant_id)
    }

    pub fn variant(&self, id: &RigSceneId) -> Option<&RigScene> {
        self.variants.iter().find(|v| &v.id == id)
    }

    #[must_use]
    pub fn with_rig_type(mut self, rig_type_id: impl Into<RigTypeId>) -> Self {
        self.rig_type_id = Some(rig_type_id.into());
        self
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
    fn test_rig_creation() {
        let variant = RigScene::new("rv1", "Default Scene")
            .with_engine(EngineSelection::new("engine-1", "ev1"));

        let rig = Rig::new(
            "rig-1",
            "Guitar Rig",
            vec![EngineId::new("engine-1")],
            variant,
        )
        .with_rig_type("guitar");

        assert_eq!(rig.name, "Guitar Rig");
        assert_eq!(rig.rig_type_id.as_ref().unwrap().as_str(), "guitar");
        assert!(rig.default_variant().is_some());
    }
}
