//! Scene types — higher-level compositions that select children's state.
//!
//! Scenes exist at the Layer, Engine, Rig, and Rack levels. Each scene
//! references the state of its children (snapshots for blocks/modules,
//! sub-scenes for layers/engines/rigs).
//!
//! **Typestate builders** enforce that all required children are specified
//! before a scene can be built. You cannot call `.build()` until all
//! required steps have been completed.
//!
//! # Typestate Proof: Cannot build LayerScene without setting modules
//!
//! ```compile_fail
//! use signal_proto::scene::LayerSceneBuilder;
//!
//! let scene = LayerSceneBuilder::new("Test")
//!     .no_standalone_blocks() // ERROR: no method on NeedsModules state
//!     .build();
//! ```
//!
//! # Typestate Proof: Cannot build LayerScene without setting standalone blocks
//!
//! ```compile_fail
//! use signal_proto::scene::LayerSceneBuilder;
//!
//! let scene = LayerSceneBuilder::new("Test")
//!     .modules(vec![])
//!     .build(); // ERROR: no method `build` on NeedsBlocks state
//! ```
//!
//! # Typestate Proof: Cannot build EngineScene without setting layers
//!
//! ```compile_fail
//! use signal_proto::scene::EngineSceneBuilder;
//!
//! let scene = EngineSceneBuilder::new("Test")
//!     .build(); // ERROR: no method `build` on NeedsLayers state
//! ```

use std::marker::PhantomData;

use crate::id::*;
use crate::normalized::NormalizedF64;
use crate::tags::Tags;
use crate::version::{LayerIndex, VersionedRef};

// ─── LayerScene ──────────────────────────────────────────────────

/// A scene at the layer level — selects module snapshots and standalone
/// block snapshots for a specific layer configuration.
#[derive(..Domain)]
pub struct LayerScene {
    pub id: LayerSceneId,
    pub name: String,
    pub module_snapshots: Vec<VersionedRef<ModuleSnapshotId>>,
    pub block_snapshots: Vec<VersionedRef<BlockSnapshotId>>,
    pub enabled: bool,
    pub volume: NormalizedF64,
    pub tags: Tags,
}

// ─── LayerScene Builder (typestate) ──────────────────────────────

/// Builder state: modules have not been specified yet.
#[derive(..Marker)]
pub struct NeedsModules;
/// Builder state: modules have been specified.
#[derive(..Marker)]
pub struct ModulesSet;
/// Builder state: standalone blocks have not been specified yet.
#[derive(..Marker)]
pub struct NeedsBlocks;
/// Builder state: standalone blocks have been specified (or explicitly skipped).
#[derive(..Marker)]
pub struct BlocksSet;

/// Typestate builder for [`LayerScene`].
///
/// Enforces that modules are set before blocks, and both are set before build.
/// The two type parameters track independent progress:
/// - `HasModules`: `NeedsModules` → `ModulesSet`
/// - `HasBlocks`: `NeedsBlocks` → `BlocksSet`
pub struct LayerSceneBuilder<HasModules, HasBlocks> {
    name: String,
    module_snapshots: Vec<VersionedRef<ModuleSnapshotId>>,
    block_snapshots: Vec<VersionedRef<BlockSnapshotId>>,
    enabled: bool,
    volume: NormalizedF64,
    _modules: PhantomData<HasModules>,
    _blocks: PhantomData<HasBlocks>,
}

impl LayerSceneBuilder<NeedsModules, NeedsBlocks> {
    /// Start building a layer scene.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            module_snapshots: Vec::new(),
            block_snapshots: Vec::new(),
            enabled: true,
            volume: NormalizedF64::ONE,
            _modules: PhantomData,
            _blocks: PhantomData,
        }
    }

    /// Set the module snapshot references for this scene.
    pub fn modules(
        self,
        refs: Vec<VersionedRef<ModuleSnapshotId>>,
    ) -> LayerSceneBuilder<ModulesSet, NeedsBlocks> {
        LayerSceneBuilder {
            name: self.name,
            module_snapshots: refs,
            block_snapshots: self.block_snapshots,
            enabled: self.enabled,
            volume: self.volume,
            _modules: PhantomData,
            _blocks: PhantomData,
        }
    }
}

impl LayerSceneBuilder<ModulesSet, NeedsBlocks> {
    /// Set the standalone block snapshot references.
    pub fn standalone_blocks(
        self,
        refs: Vec<VersionedRef<BlockSnapshotId>>,
    ) -> LayerSceneBuilder<ModulesSet, BlocksSet> {
        LayerSceneBuilder {
            name: self.name,
            module_snapshots: self.module_snapshots,
            block_snapshots: refs,
            enabled: self.enabled,
            volume: self.volume,
            _modules: PhantomData,
            _blocks: PhantomData,
        }
    }

    /// Skip standalone blocks — this scene has none.
    pub fn no_standalone_blocks(self) -> LayerSceneBuilder<ModulesSet, BlocksSet> {
        LayerSceneBuilder {
            name: self.name,
            module_snapshots: self.module_snapshots,
            block_snapshots: Vec::new(),
            enabled: self.enabled,
            volume: self.volume,
            _modules: PhantomData,
            _blocks: PhantomData,
        }
    }
}

impl LayerSceneBuilder<ModulesSet, BlocksSet> {
    /// Set enabled state (default: true).
    #[must_use]
    pub fn enabled(mut self, enabled: bool) -> Self {
        self.enabled = enabled;
        self
    }

    /// Set volume level (default: 1.0).
    #[must_use]
    pub fn volume(mut self, vol: NormalizedF64) -> Self {
        self.volume = vol;
        self
    }

    /// Build the layer scene. Only callable when modules and blocks are set.
    pub fn build(self) -> LayerScene {
        LayerScene {
            id: LayerSceneId::new(),
            name: self.name,
            module_snapshots: self.module_snapshots,
            block_snapshots: self.block_snapshots,
            enabled: self.enabled,
            volume: self.volume,
            tags: Tags::new(),
        }
    }
}

// ─── EngineScene ─────────────────────────────────────────────────

/// An entry mapping a layer index to a layer scene reference.
#[derive(..Domain)]
pub struct LayerSceneEntry {
    pub layer_index: LayerIndex,
    pub scene_ref: VersionedRef<LayerSceneId>,
}

/// A scene at the engine level — selects layer scenes for each layer.
#[derive(..Domain)]
pub struct EngineScene {
    pub id: EngineSceneId,
    pub name: String,
    pub layer_scenes: Vec<LayerSceneEntry>,
    pub tags: Tags,
}

// ─── EngineScene Builder (typestate) ─────────────────────────────

/// Builder state: layers have not been specified yet.
#[derive(..Marker)]
pub struct NeedsLayers;
/// Builder state: layers have been specified.
#[derive(..Marker)]
pub struct LayersSet;

/// Typestate builder for [`EngineScene`].
pub struct EngineSceneBuilder<HasLayers> {
    name: String,
    layer_scenes: Vec<LayerSceneEntry>,
    _layers: PhantomData<HasLayers>,
}

impl EngineSceneBuilder<NeedsLayers> {
    /// Start building an engine scene.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            layer_scenes: Vec::new(),
            _layers: PhantomData,
        }
    }

    /// Set the layer scene entries.
    pub fn layers(self, entries: Vec<LayerSceneEntry>) -> EngineSceneBuilder<LayersSet> {
        EngineSceneBuilder {
            name: self.name,
            layer_scenes: entries,
            _layers: PhantomData,
        }
    }
}

impl EngineSceneBuilder<LayersSet> {
    /// Build the engine scene. Only callable when layers are set.
    pub fn build(self) -> EngineScene {
        EngineScene {
            id: EngineSceneId::new(),
            name: self.name,
            layer_scenes: self.layer_scenes,
            tags: Tags::new(),
        }
    }
}

// ─── RigScene ────────────────────────────────────────────────────

/// An entry mapping an engine ID to an engine scene reference.
#[derive(..Domain)]
pub struct EngineSceneEntry {
    pub engine_id: EngineId,
    pub scene_ref: VersionedRef<EngineSceneId>,
}

/// A scene at the rig level — selects engine scenes for each engine.
#[derive(..Domain)]
pub struct RigScene {
    pub id: RigSceneId,
    pub name: String,
    pub engine_scenes: Vec<EngineSceneEntry>,
    pub tags: Tags,
}

// ─── RigScene Builder (typestate) ────────────────────────────────

/// Builder state: engines have not been specified yet.
#[derive(..Marker)]
pub struct NeedsEngines;
/// Builder state: engines have been specified.
#[derive(..Marker)]
pub struct EnginesSet;

/// Typestate builder for [`RigScene`].
pub struct RigSceneBuilder<HasEngines> {
    name: String,
    engine_scenes: Vec<EngineSceneEntry>,
    _engines: PhantomData<HasEngines>,
}

impl RigSceneBuilder<NeedsEngines> {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            engine_scenes: Vec::new(),
            _engines: PhantomData,
        }
    }

    pub fn engines(self, entries: Vec<EngineSceneEntry>) -> RigSceneBuilder<EnginesSet> {
        RigSceneBuilder {
            name: self.name,
            engine_scenes: entries,
            _engines: PhantomData,
        }
    }
}

impl RigSceneBuilder<EnginesSet> {
    pub fn build(self) -> RigScene {
        RigScene {
            id: RigSceneId::new(),
            name: self.name,
            engine_scenes: self.engine_scenes,
            tags: Tags::new(),
        }
    }
}

// ─── RackScene ───────────────────────────────────────────────────

/// An entry mapping a rig ID to a rig scene reference.
#[derive(..Domain)]
pub struct RigSceneEntry {
    pub rig_id: RigId,
    pub scene_ref: VersionedRef<RigSceneId>,
}

/// A scene at the rack level — selects rig scenes for each rig.
#[derive(..Domain)]
pub struct RackScene {
    pub id: RackSceneId,
    pub name: String,
    pub rig_scenes: Vec<RigSceneEntry>,
    pub tags: Tags,
}

// ─── RackScene Builder (typestate) ───────────────────────────────

/// Builder state: rigs have not been specified yet.
#[derive(..Marker)]
pub struct NeedsRigs;
/// Builder state: rigs have been specified.
#[derive(..Marker)]
pub struct RigsSet;

/// Typestate builder for [`RackScene`].
pub struct RackSceneBuilder<HasRigs> {
    name: String,
    rig_scenes: Vec<RigSceneEntry>,
    _rigs: PhantomData<HasRigs>,
}

impl RackSceneBuilder<NeedsRigs> {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            rig_scenes: Vec::new(),
            _rigs: PhantomData,
        }
    }

    pub fn rigs(self, entries: Vec<RigSceneEntry>) -> RackSceneBuilder<RigsSet> {
        RackSceneBuilder {
            name: self.name,
            rig_scenes: entries,
            _rigs: PhantomData,
        }
    }
}

impl RackSceneBuilder<RigsSet> {
    pub fn build(self) -> RackScene {
        RackScene {
            id: RackSceneId::new(),
            name: self.name,
            rig_scenes: self.rig_scenes,
            tags: Tags::new(),
        }
    }
}

// ─── ScopedSceneRef ──────────────────────────────────────────────

/// A reference to a scene at any level of the hierarchy.
///
/// Used by `SongSection` and `Patch` to point to whichever level of
/// scene they operate on.
#[derive(..Domain)]
#[repr(u8)]
pub enum ScopedSceneRef {
    Layer(VersionedRef<LayerSceneId>),
    Engine(VersionedRef<EngineSceneId>),
    Rig(VersionedRef<RigSceneId>),
    Rack(VersionedRef<RackSceneId>),
}

// ─── SceneTransition ─────────────────────────────────────────────

/// How to transition between scenes.
#[derive(..Marker)]
#[repr(u8)]
pub enum SceneTransition {
    /// Instant switch.
    Immediate,
    /// Crossfade over a duration in milliseconds.
    Crossfade(u32),
    /// Spillover: let existing tails ring out.
    Spillover,
}

impl Default for SceneTransition {
    fn default() -> Self {
        Self::Immediate
    }
}

// ─── Tests ───────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn layer_scene_builder_full_path() {
        let module_ref = VersionedRef::new(ModuleSnapshotId::new(), 1);
        let block_ref = VersionedRef::new(BlockSnapshotId::new(), 1);

        let scene = LayerSceneBuilder::new("Clean Verse")
            .modules(vec![module_ref])
            .standalone_blocks(vec![block_ref])
            .build();

        assert_eq!(scene.name, "Clean Verse");
        assert_eq!(scene.module_snapshots.len(), 1);
        assert_eq!(scene.block_snapshots.len(), 1);
        assert!(scene.enabled);
    }

    #[test]
    fn layer_scene_builder_no_blocks() {
        let scene = LayerSceneBuilder::new("Simple")
            .modules(vec![])
            .no_standalone_blocks()
            .enabled(false)
            .volume(NormalizedF64::new(0.8))
            .build();

        assert_eq!(scene.name, "Simple");
        assert!(!scene.enabled);
        assert!((scene.volume.get() - 0.8).abs() < f64::EPSILON);
        assert!(scene.block_snapshots.is_empty());
    }

    #[test]
    fn engine_scene_builder() {
        let entry = LayerSceneEntry {
            layer_index: LayerIndex::new(1),
            scene_ref: VersionedRef::new(LayerSceneId::new(), 1),
        };

        let scene = EngineSceneBuilder::new("Main Guitar")
            .layers(vec![entry])
            .build();

        assert_eq!(scene.name, "Main Guitar");
        assert_eq!(scene.layer_scenes.len(), 1);
    }

    #[test]
    fn rig_scene_builder() {
        let entry = EngineSceneEntry {
            engine_id: EngineId::new(),
            scene_ref: VersionedRef::new(EngineSceneId::new(), 1),
        };

        let scene = RigSceneBuilder::new("Full Rig")
            .engines(vec![entry])
            .build();

        assert_eq!(scene.name, "Full Rig");
        assert_eq!(scene.engine_scenes.len(), 1);
    }

    #[test]
    fn rack_scene_builder() {
        let entry = RigSceneEntry {
            rig_id: RigId::new(),
            scene_ref: VersionedRef::new(RigSceneId::new(), 1),
        };

        let scene = RackSceneBuilder::new("All Vocals")
            .rigs(vec![entry])
            .build();

        assert_eq!(scene.name, "All Vocals");
        assert_eq!(scene.rig_scenes.len(), 1);
    }

    #[test]
    fn scoped_scene_ref_variants() {
        let layer_ref = ScopedSceneRef::Layer(VersionedRef::new(LayerSceneId::new(), 1));
        let engine_ref = ScopedSceneRef::Engine(VersionedRef::new(EngineSceneId::new(), 1));
        let rig_ref = ScopedSceneRef::Rig(VersionedRef::new(RigSceneId::new(), 1));
        let rack_ref = ScopedSceneRef::Rack(VersionedRef::new(RackSceneId::new(), 1));

        // Just verify they can be constructed
        assert!(matches!(layer_ref, ScopedSceneRef::Layer(_)));
        assert!(matches!(engine_ref, ScopedSceneRef::Engine(_)));
        assert!(matches!(rig_ref, ScopedSceneRef::Rig(_)));
        assert!(matches!(rack_ref, ScopedSceneRef::Rack(_)));
    }

    #[test]
    fn scene_transition_default() {
        assert_eq!(SceneTransition::default(), SceneTransition::Immediate);
    }
}
