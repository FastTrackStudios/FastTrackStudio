//! Deterministic domain resolver/compiler output types.
//!
//! Resolver turns selected variants (rig scene / profile patch / song section)
//! into an executable graph with effective override stack.

use facet::Facet;
use serde::{Deserialize, Serialize};

use crate::engine::{EngineId, EngineSceneId};
use crate::layer::{LayerId, LayerSnapshotId};
use crate::overrides::Override;
use crate::profile::{PatchId, ProfileId};
use crate::rig::{RigId, RigSceneId};
use crate::song::{SectionId, SongId};
use crate::{Block, BlockType, ModulePresetId, ModuleSnapshotId, PresetId, SnapshotId};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Facet)]
#[repr(C)]
pub enum ResolveError {
    NotFound(String),
    InvalidReference(String),
    CycleDetected(String),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
#[repr(C)]
pub enum ResolveTarget {
    RigScene {
        rig_id: RigId,
        scene_id: RigSceneId,
    },
    ProfilePatch {
        profile_id: ProfileId,
        patch_id: PatchId,
    },
    SongSection {
        song_id: SongId,
        section_id: SectionId,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
#[repr(C)]
pub enum LayerSource {
    LayerPreset {
        layer_id: LayerId,
        variant_id: LayerSnapshotId,
    },
    InlinedInParent,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct ResolvedBlock {
    pub node_id: String,
    pub label: String,
    pub block_type: BlockType,
    pub source_preset_id: Option<PresetId>,
    pub source_variant_id: Option<SnapshotId>,
    pub block: Block,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct ResolvedModule {
    pub source_preset_id: ModulePresetId,
    pub source_variant_id: ModuleSnapshotId,
    pub blocks: Vec<ResolvedBlock>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct ResolvedLayer {
    pub layer_id: LayerId,
    pub layer_variant_id: LayerSnapshotId,
    pub source: LayerSource,
    pub modules: Vec<ResolvedModule>,
    pub standalone_blocks: Vec<ResolvedBlock>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct ResolvedEngine {
    pub engine_id: EngineId,
    pub engine_scene_id: EngineSceneId,
    pub layers: Vec<ResolvedLayer>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct ResolvedGraph {
    pub target: ResolveTarget,
    pub rig_id: RigId,
    pub rig_scene_id: RigSceneId,
    pub engines: Vec<ResolvedEngine>,
    pub effective_overrides: Vec<Override>,
}
