//! Active hierarchy — runtime resolution chain with typestate enforcement.
//!
//! Each level wraps its domain type with an `Unresolved` / `Resolved` state.
//! Unresolved: the structural entity is selected but its state (snapshot/scene)
//! is not loaded. Resolved: state is loaded and all children are resolved.
//!
//! **Key guarantee**: You cannot access `.snapshot()` or `.scene()` on an
//! unresolved wrapper — it's a compile error, not a runtime check.
//!
//! # Typestate Proof: Cannot access snapshot on unresolved block
//!
//! ```compile_fail
//! use signal_proto::active::ActiveBlock;
//! use signal_proto::block::{Block, PluginId};
//!
//! let block = Block::new("Test", PluginId::vst3("x", "X"));
//! let active = ActiveBlock::new(block);
//! let _ = active.snapshot(); // ERROR: no method `snapshot` on `ActiveBlock<Unresolved>`
//! ```
//!
//! # Typestate Proof: Cannot access active_blocks on unresolved module
//!
//! ```compile_fail
//! use signal_proto::active::ActiveModule;
//! use signal_proto::module::{Module, ModuleType};
//!
//! let module = Module::new("Drive", ModuleType::Drive);
//! let active = ActiveModule::new(module);
//! let _ = active.active_blocks(); // ERROR: no method on `ActiveModule<Unresolved>`
//! ```
//!
//! # Typestate Proof: Cannot access scene on unresolved layer
//!
//! ```compile_fail
//! use signal_proto::active::ActiveLayer;
//! use signal_proto::layer::Layer;
//! use signal_proto::version::LayerIndex;
//!
//! let layer = Layer::new("Main", LayerIndex::new(1));
//! let active = ActiveLayer::new(layer);
//! let _ = active.scene(); // ERROR: no method `scene` on `ActiveLayer<Unresolved>`
//! ```

use std::marker::PhantomData;

use crate::block::Block;
use crate::engine::Engine;
use crate::id::{BlockId, ModuleId};
use crate::layer::Layer;
use crate::module::{Module, ModuleType};
use crate::parameter::ParameterValue;
use crate::rack::Rack;
use crate::rig::Rig;
use crate::scene::{EngineScene, LayerScene, RackScene, RigScene};
use crate::snapshot::{BlockSnapshot, ModuleSnapshot};
use crate::version::LayerIndex;

// ─── State markers ───────────────────────────────────────────────

/// State not yet loaded — snapshot/scene is unavailable.
#[derive(..Marker)]
pub struct Unresolved;

/// State is loaded — all children are resolved.
#[derive(..Marker)]
pub struct Resolved;

// ─── ActiveBlock ─────────────────────────────────────────────────

/// A block with optional resolved snapshot state.
pub struct ActiveBlock<State = Unresolved> {
    block: Block,
    snapshot: Option<BlockSnapshot>,
    _state: PhantomData<State>,
}

impl ActiveBlock<Unresolved> {
    /// Wrap a block without loading its snapshot.
    pub fn new(block: Block) -> Self {
        Self {
            block,
            snapshot: None,
            _state: PhantomData,
        }
    }

    /// Resolve by loading a snapshot. Returns the `Resolved` wrapper.
    pub fn resolve(self, snapshot: BlockSnapshot) -> ActiveBlock<Resolved> {
        ActiveBlock {
            block: self.block,
            snapshot: Some(snapshot),
            _state: PhantomData,
        }
    }

    /// Access the block (always available).
    pub fn block(&self) -> &Block {
        &self.block
    }
}

impl ActiveBlock<Resolved> {
    /// The block.
    pub fn block(&self) -> &Block {
        &self.block
    }

    /// The loaded snapshot. Only available when Resolved.
    pub fn snapshot(&self) -> &BlockSnapshot {
        self.snapshot
            .as_ref()
            .expect("resolved block always has snapshot")
    }

    /// Look up a parameter value by index.
    pub fn parameter(&self, index: u32) -> Option<&ParameterValue> {
        self.snapshot().parameter(index)
    }

    /// Drop the snapshot and return to Unresolved.
    pub fn unresolve(self) -> ActiveBlock<Unresolved> {
        ActiveBlock {
            block: self.block,
            snapshot: None,
            _state: PhantomData,
        }
    }
}

// ─── ActiveModule ────────────────────────────────────────────────

/// A module with optional resolved snapshot and child blocks.
pub struct ActiveModule<State = Unresolved> {
    module: Module,
    snapshot: Option<ModuleSnapshot>,
    active_blocks: Vec<ActiveBlock<Resolved>>,
    _state: PhantomData<State>,
}

impl ActiveModule<Unresolved> {
    pub fn new(module: Module) -> Self {
        Self {
            module,
            snapshot: None,
            active_blocks: Vec::new(),
            _state: PhantomData,
        }
    }

    /// Resolve with a snapshot and pre-resolved blocks.
    pub fn resolve(
        self,
        snapshot: ModuleSnapshot,
        blocks: Vec<ActiveBlock<Resolved>>,
    ) -> ActiveModule<Resolved> {
        ActiveModule {
            module: self.module,
            snapshot: Some(snapshot),
            active_blocks: blocks,
            _state: PhantomData,
        }
    }

    pub fn module(&self) -> &Module {
        &self.module
    }
}

impl ActiveModule<Resolved> {
    pub fn module(&self) -> &Module {
        &self.module
    }

    pub fn snapshot(&self) -> &ModuleSnapshot {
        self.snapshot
            .as_ref()
            .expect("resolved module always has snapshot")
    }

    pub fn active_blocks(&self) -> &[ActiveBlock<Resolved>] {
        &self.active_blocks
    }

    pub fn block(&self, id: BlockId) -> Option<&ActiveBlock<Resolved>> {
        self.active_blocks.iter().find(|b| b.block().id == id)
    }

    pub fn unresolve(self) -> ActiveModule<Unresolved> {
        ActiveModule {
            module: self.module,
            snapshot: None,
            active_blocks: Vec::new(),
            _state: PhantomData,
        }
    }
}

// ─── ActiveLayer ─────────────────────────────────────────────────

/// A layer with optional resolved scene, child modules, and standalone blocks.
pub struct ActiveLayer<State = Unresolved> {
    layer: Layer,
    scene: Option<LayerScene>,
    active_modules: Vec<ActiveModule<Resolved>>,
    active_standalone_blocks: Vec<ActiveBlock<Resolved>>,
    _state: PhantomData<State>,
}

impl ActiveLayer<Unresolved> {
    pub fn new(layer: Layer) -> Self {
        Self {
            layer,
            scene: None,
            active_modules: Vec::new(),
            active_standalone_blocks: Vec::new(),
            _state: PhantomData,
        }
    }

    pub fn resolve(
        self,
        scene: LayerScene,
        modules: Vec<ActiveModule<Resolved>>,
        standalone_blocks: Vec<ActiveBlock<Resolved>>,
    ) -> ActiveLayer<Resolved> {
        ActiveLayer {
            layer: self.layer,
            scene: Some(scene),
            active_modules: modules,
            active_standalone_blocks: standalone_blocks,
            _state: PhantomData,
        }
    }

    pub fn layer(&self) -> &Layer {
        &self.layer
    }
}

impl ActiveLayer<Resolved> {
    pub fn layer(&self) -> &Layer {
        &self.layer
    }

    pub fn scene(&self) -> &LayerScene {
        self.scene
            .as_ref()
            .expect("resolved layer always has scene")
    }

    pub fn active_modules(&self) -> &[ActiveModule<Resolved>] {
        &self.active_modules
    }

    pub fn active_standalone_blocks(&self) -> &[ActiveBlock<Resolved>] {
        &self.active_standalone_blocks
    }

    pub fn module_by_type(&self, mt: ModuleType) -> Option<&ActiveModule<Resolved>> {
        self.active_modules
            .iter()
            .find(|m| m.module().module_type == mt)
    }

    pub fn module_by_id(&self, id: ModuleId) -> Option<&ActiveModule<Resolved>> {
        self.active_modules.iter().find(|m| m.module().id == id)
    }

    pub fn unresolve(self) -> ActiveLayer<Unresolved> {
        ActiveLayer {
            layer: self.layer,
            scene: None,
            active_modules: Vec::new(),
            active_standalone_blocks: Vec::new(),
            _state: PhantomData,
        }
    }
}

// ─── ActiveEngine ────────────────────────────────────────────────

/// An engine with optional resolved scene and child layers.
pub struct ActiveEngine<State = Unresolved> {
    engine: Engine,
    scene: Option<EngineScene>,
    active_layers: Vec<ActiveLayer<Resolved>>,
    _state: PhantomData<State>,
}

impl ActiveEngine<Unresolved> {
    pub fn new(engine: Engine) -> Self {
        Self {
            engine,
            scene: None,
            active_layers: Vec::new(),
            _state: PhantomData,
        }
    }

    pub fn resolve(
        self,
        scene: EngineScene,
        layers: Vec<ActiveLayer<Resolved>>,
    ) -> ActiveEngine<Resolved> {
        ActiveEngine {
            engine: self.engine,
            scene: Some(scene),
            active_layers: layers,
            _state: PhantomData,
        }
    }

    pub fn engine(&self) -> &Engine {
        &self.engine
    }
}

impl ActiveEngine<Resolved> {
    pub fn engine(&self) -> &Engine {
        &self.engine
    }

    pub fn scene(&self) -> &EngineScene {
        self.scene
            .as_ref()
            .expect("resolved engine always has scene")
    }

    pub fn active_layers(&self) -> &[ActiveLayer<Resolved>] {
        &self.active_layers
    }

    /// Get a resolved layer by 1-based index.
    pub fn layer(&self, index: LayerIndex) -> Option<&ActiveLayer<Resolved>> {
        self.active_layers.get(index.to_zero_based())
    }

    pub fn unresolve(self) -> ActiveEngine<Unresolved> {
        ActiveEngine {
            engine: self.engine,
            scene: None,
            active_layers: Vec::new(),
            _state: PhantomData,
        }
    }
}

// ─── ActiveRig ───────────────────────────────────────────────────

/// A rig with optional resolved scene and child engines.
pub struct ActiveRig<State = Unresolved> {
    rig: Rig,
    scene: Option<RigScene>,
    active_engines: Vec<ActiveEngine<Resolved>>,
    _state: PhantomData<State>,
}

impl ActiveRig<Unresolved> {
    pub fn new(rig: Rig) -> Self {
        Self {
            rig,
            scene: None,
            active_engines: Vec::new(),
            _state: PhantomData,
        }
    }

    pub fn resolve(
        self,
        scene: RigScene,
        engines: Vec<ActiveEngine<Resolved>>,
    ) -> ActiveRig<Resolved> {
        ActiveRig {
            rig: self.rig,
            scene: Some(scene),
            active_engines: engines,
            _state: PhantomData,
        }
    }

    pub fn rig(&self) -> &Rig {
        &self.rig
    }
}

impl ActiveRig<Resolved> {
    pub fn rig(&self) -> &Rig {
        &self.rig
    }

    pub fn scene(&self) -> &RigScene {
        self.scene.as_ref().expect("resolved rig always has scene")
    }

    pub fn active_engines(&self) -> &[ActiveEngine<Resolved>] {
        &self.active_engines
    }

    pub fn unresolve(self) -> ActiveRig<Unresolved> {
        ActiveRig {
            rig: self.rig,
            scene: None,
            active_engines: Vec::new(),
            _state: PhantomData,
        }
    }
}

// ─── ActiveRack ──────────────────────────────────────────────────

/// A rack with optional resolved scene and child rigs.
pub struct ActiveRack<State = Unresolved> {
    rack: Rack,
    scene: Option<RackScene>,
    active_rigs: Vec<ActiveRig<Resolved>>,
    _state: PhantomData<State>,
}

impl ActiveRack<Unresolved> {
    pub fn new(rack: Rack) -> Self {
        Self {
            rack,
            scene: None,
            active_rigs: Vec::new(),
            _state: PhantomData,
        }
    }

    pub fn resolve(self, scene: RackScene, rigs: Vec<ActiveRig<Resolved>>) -> ActiveRack<Resolved> {
        ActiveRack {
            rack: self.rack,
            scene: Some(scene),
            active_rigs: rigs,
            _state: PhantomData,
        }
    }

    pub fn rack(&self) -> &Rack {
        &self.rack
    }
}

impl ActiveRack<Resolved> {
    pub fn rack(&self) -> &Rack {
        &self.rack
    }

    pub fn scene(&self) -> &RackScene {
        self.scene.as_ref().expect("resolved rack always has scene")
    }

    pub fn active_rigs(&self) -> &[ActiveRig<Resolved>] {
        &self.active_rigs
    }

    pub fn unresolve(self) -> ActiveRack<Unresolved> {
        ActiveRack {
            rack: self.rack,
            scene: None,
            active_rigs: Vec::new(),
            _state: PhantomData,
        }
    }
}

// ─── Tests ───────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block::PluginId;
    use crate::module::Module;
    use crate::parameter::ParameterValue;
    use crate::rig::InstrumentType;
    use crate::scene::*;
    use crate::snapshot::{BlockSnapshot, ModuleSnapshot};
    use crate::version::VersionedRef;

    #[test]
    fn active_block_resolve_unresolve() {
        let block = Block::new("Klon", PluginId::vst3("klon", "Klon Centaur"));
        let block_id = block.id;
        let snap = BlockSnapshot::new("Light OD", block_id, PluginId::vst3("klon", "Klon Centaur"))
            .with_parameters(vec![ParameterValue::new(0, 0.7)]);

        let active = ActiveBlock::new(block);
        assert_eq!(active.block().name, "Klon");

        let resolved = active.resolve(snap);
        assert_eq!(resolved.snapshot().name(), "Light OD");
        assert_eq!(resolved.parameter(0).unwrap().value.get(), 0.7);

        let unresolved = resolved.unresolve();
        assert_eq!(unresolved.block().name, "Klon");
    }

    #[test]
    fn active_module_resolve() {
        let module = Module::new("Drive", ModuleType::Drive);
        let block = Block::new("Klon", PluginId::vst3("klon", "Klon"));
        let block_id = block.id;
        let snap = BlockSnapshot::new("OD", block_id, PluginId::vst3("klon", "Klon"));
        let active_block = ActiveBlock::new(block).resolve(snap);

        let module_snap = ModuleSnapshot::new("Blues Stack", vec![]);
        let active_module = ActiveModule::new(module).resolve(module_snap, vec![active_block]);

        assert_eq!(active_module.snapshot().name, "Blues Stack");
        assert_eq!(active_module.active_blocks().len(), 1);
    }

    #[test]
    fn active_layer_full_resolution() {
        // Build blocks
        let block = Block::new("Drive", PluginId::vst3("d", "Drive"));
        let block_id = block.id;
        let snap = BlockSnapshot::new("Clean", block_id, PluginId::vst3("d", "Drive"));
        let active_block = ActiveBlock::new(block).resolve(snap);

        // Build module
        let module = Module::new("Drive", ModuleType::Drive);
        let module_snap = ModuleSnapshot::new("Stack", vec![]);
        let active_module = ActiveModule::new(module).resolve(module_snap, vec![active_block]);

        // Build layer
        let layer = Layer::new("Main", LayerIndex::new(1));
        let layer_scene = LayerSceneBuilder::new("Verse")
            .modules(vec![])
            .no_standalone_blocks()
            .build();
        let active_layer =
            ActiveLayer::new(layer).resolve(layer_scene, vec![active_module], vec![]);

        assert_eq!(active_layer.scene().name, "Verse");
        assert_eq!(active_layer.active_modules().len(), 1);
        assert!(active_layer.module_by_type(ModuleType::Drive).is_some());
        assert!(active_layer.module_by_type(ModuleType::Amp).is_none());
    }

    #[test]
    fn active_engine_with_layers() {
        let layer = Layer::new("Main", LayerIndex::new(1));
        let layer_scene = LayerSceneBuilder::new("Clean")
            .modules(vec![])
            .no_standalone_blocks()
            .build();
        let active_layer = ActiveLayer::new(layer).resolve(layer_scene, vec![], vec![]);

        let engine = Engine::new(
            "Guitar",
            InstrumentType::Guitar,
            Layer::new("Placeholder", LayerIndex::new(1)),
        );
        let engine_scene = EngineSceneBuilder::new("Main Guitar")
            .layers(vec![LayerSceneEntry {
                layer_index: LayerIndex::new(1),
                scene_ref: VersionedRef::new(crate::id::LayerSceneId::new(), 1),
            }])
            .build();
        let active_engine = ActiveEngine::new(engine).resolve(engine_scene, vec![active_layer]);

        assert_eq!(active_engine.scene().name, "Main Guitar");
        assert_eq!(active_engine.active_layers().len(), 1);
        assert!(active_engine.layer(LayerIndex::new(1)).is_some());
        assert!(active_engine.layer(LayerIndex::new(2)).is_none());
    }
}
