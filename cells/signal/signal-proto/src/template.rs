//! Template system — structural blueprints at every level.
//!
//! Templates capture the shape of a thing (modules, block slots, layers) without
//! binding to specific plugins or parameter state. Bidirectional:
//!
//! - **Instance → Template**: `thing.to_template()` strips plugins, keeps structure.
//! - **Template → Instance**: `T::from_template(&template)` creates placeholders.
//!
//! The [`Templatable`] trait is implemented for Block, Module, Layer, Engine, Rig, Rack.

use crate::block::{Block, BlockType, PluginId};
use crate::engine::Engine;
use crate::id::{EngineTemplateId, RackTemplateId, RigTemplateId};
use crate::layer::Layer;
use crate::module::{Module, ModuleBlock, ModuleType};
use crate::non_empty::NonEmptyVec;
use crate::normalized::Order;
use crate::rack::Rack;
use crate::rig::{InstrumentType, Rig};
use crate::version::LayerIndex;

// ─── Templatable trait ───────────────────────────────────────────

/// Types that can produce and consume structural templates.
pub trait Templatable: Sized {
    type Template;

    /// Extract a structural template — strips plugin bindings and parameter state.
    fn to_template(&self) -> Self::Template;

    /// Instantiate from a template — creates real instances with placeholder plugins.
    fn from_template(template: &Self::Template) -> Self;
}

// ─── BlockTemplate ───────────────────────────────────────────────

/// A block slot — knows what type of DSP it needs but not which plugin.
#[derive(Debug, Clone, PartialEq, ::facet::Facet)]
pub struct BlockTemplate {
    pub block_type: BlockType,
    pub name: String,
    pub alias: Option<String>,
    pub description: Option<String>,
    pub local_col: Option<usize>,
    pub local_row: Option<usize>,
}

impl BlockTemplate {
    /// Create a block template with the given name and type.
    pub fn new(name: impl Into<String>, block_type: BlockType) -> Self {
        Self {
            block_type,
            name: name.into(),
            alias: None,
            description: None,
            local_col: None,
            local_row: None,
        }
    }

    #[must_use]
    pub fn with_alias(mut self, alias: impl Into<String>) -> Self {
        self.alias = Some(alias.into());
        self
    }

    #[must_use]
    pub fn with_description(mut self, desc: impl Into<String>) -> Self {
        self.description = Some(desc.into());
        self
    }

    /// Set 2D grid position (column, row).
    #[must_use]
    pub fn at(mut self, col: usize, row: usize) -> Self {
        self.local_col = Some(col);
        self.local_row = Some(row);
        self
    }
}

impl Templatable for Block {
    type Template = BlockTemplate;

    fn to_template(&self) -> BlockTemplate {
        BlockTemplate {
            block_type: self.block_type,
            name: self.name.clone(),
            alias: self.alias.clone(),
            description: self.description.clone(),
            local_col: None,
            local_row: None,
        }
    }

    fn from_template(t: &BlockTemplate) -> Block {
        let mut block = Block::new(&t.name, PluginId::unassigned()).with_block_type(t.block_type);
        if let Some(alias) = &t.alias {
            block = block.with_alias(alias);
        }
        if let Some(desc) = &t.description {
            block = block.with_description(desc);
        }
        block
    }
}

// ─── ModuleTemplate ──────────────────────────────────────────────

/// Structural blueprint for a processing module.
#[derive(Debug, Clone, PartialEq, ::facet::Facet)]
pub struct ModuleTemplate {
    pub module_type: ModuleType,
    pub name: String,
    pub description: Option<String>,
    pub blocks: Vec<BlockTemplate>,
    pub grid_width: Option<usize>,
    pub grid_height: Option<usize>,
}

impl ModuleTemplate {
    /// Create a module template with the given name and type.
    pub fn new(name: impl Into<String>, module_type: ModuleType) -> Self {
        Self {
            module_type,
            name: name.into(),
            description: None,
            blocks: Vec::new(),
            grid_width: None,
            grid_height: None,
        }
    }

    #[must_use]
    pub fn with_description(mut self, desc: impl Into<String>) -> Self {
        self.description = Some(desc.into());
        self
    }

    #[must_use]
    pub fn with_block(mut self, block: BlockTemplate) -> Self {
        self.blocks.push(block);
        self
    }

    #[must_use]
    pub fn with_grid_size(mut self, width: usize, height: usize) -> Self {
        self.grid_width = Some(width);
        self.grid_height = Some(height);
        self
    }
}

impl Templatable for Module {
    type Template = ModuleTemplate;

    fn to_template(&self) -> ModuleTemplate {
        ModuleTemplate {
            module_type: self.module_type,
            name: self.name.clone(),
            description: None,
            blocks: self
                .blocks
                .iter()
                .map(|mb| mb.block.to_template())
                .collect(),
            grid_width: self.grid_width,
            grid_height: self.grid_height,
        }
    }

    fn from_template(t: &ModuleTemplate) -> Module {
        let mut module = Module::new(&t.name, t.module_type);
        module.grid_width = t.grid_width;
        module.grid_height = t.grid_height;
        for (i, bt) in t.blocks.iter().enumerate() {
            let block = Block::from_template(bt);
            let mut mb = ModuleBlock::new(block, Order::new(i as u8));
            mb.local_col = bt.local_col;
            mb.local_row = bt.local_row;
            module.add_block(mb);
        }
        module
    }
}

// ─── LayerTemplate ───────────────────────────────────────────────

/// Blueprint for a layer — which modules and standalone blocks it has.
#[derive(Debug, Clone, ::facet::Facet)]
pub struct LayerTemplate {
    pub name: String,
    pub index: LayerIndex,
    pub modules: Vec<ModuleTemplate>,
    pub standalone_blocks: Vec<BlockTemplate>,
    pub description: Option<String>,
}

impl LayerTemplate {
    /// Create a layer template with the given name and index.
    pub fn new(name: impl Into<String>, index: LayerIndex) -> Self {
        Self {
            name: name.into(),
            index,
            modules: Vec::new(),
            standalone_blocks: Vec::new(),
            description: None,
        }
    }

    #[must_use]
    pub fn with_module(mut self, module: ModuleTemplate) -> Self {
        self.modules.push(module);
        self
    }

    #[must_use]
    pub fn with_description(mut self, desc: impl Into<String>) -> Self {
        self.description = Some(desc.into());
        self
    }
}

impl Templatable for Layer {
    type Template = LayerTemplate;

    fn to_template(&self) -> LayerTemplate {
        LayerTemplate {
            name: self.name.clone(),
            index: self.index,
            modules: self.modules.iter().map(|m| m.to_template()).collect(),
            standalone_blocks: self
                .standalone_blocks
                .iter()
                .map(|b| b.to_template())
                .collect(),
            description: None,
        }
    }

    fn from_template(t: &LayerTemplate) -> Layer {
        let mut layer = Layer::new(&t.name, t.index);
        for mt in &t.modules {
            layer.add_module(Module::from_template(mt));
        }
        for bt in &t.standalone_blocks {
            layer.add_standalone_block(Block::from_template(bt));
        }
        layer
    }
}

// ─── EngineTemplate ──────────────────────────────────────────────

/// Blueprint for an engine — which layers and their structure.
#[derive(Debug, Clone, ::facet::Facet)]
pub struct EngineTemplate {
    pub id: EngineTemplateId,
    pub name: String,
    pub engine_type: InstrumentType,
    pub layers: NonEmptyVec<LayerTemplate>,
    pub description: Option<String>,
}

impl EngineTemplate {
    /// Create an engine template with one initial layer.
    pub fn new(name: impl Into<String>, engine_type: InstrumentType, layer: LayerTemplate) -> Self {
        Self {
            id: EngineTemplateId::new(),
            name: name.into(),
            engine_type,
            layers: NonEmptyVec::new(layer),
            description: None,
        }
    }

    #[must_use]
    pub fn with_description(mut self, desc: impl Into<String>) -> Self {
        self.description = Some(desc.into());
        self
    }
}

impl Templatable for Engine {
    type Template = EngineTemplate;

    fn to_template(&self) -> EngineTemplate {
        let layer_templates: Vec<LayerTemplate> =
            self.layers.iter().map(|l| l.to_template()).collect();
        EngineTemplate {
            id: EngineTemplateId::new(),
            name: self.name.clone(),
            engine_type: self.engine_type.clone(),
            layers: NonEmptyVec::from_vec(layer_templates)
                .expect("engine always has at least one layer"),
            description: None,
        }
    }

    fn from_template(t: &EngineTemplate) -> Engine {
        let first_layer = Layer::from_template(t.layers.first());
        let mut engine = Engine::new(&t.name, t.engine_type.clone(), first_layer);
        for layer_template in t.layers.iter().skip(1) {
            engine.add_layer(Layer::from_template(layer_template));
        }
        engine
    }
}

// ─── RigTemplate ─────────────────────────────────────────────────

/// Blueprint for a rig — which engines it contains.
#[derive(Debug, Clone, ::facet::Facet)]
pub struct RigTemplate {
    pub id: RigTemplateId,
    pub name: String,
    pub instrument_type: InstrumentType,
    pub engines: NonEmptyVec<EngineTemplate>,
    pub description: Option<String>,
}

impl RigTemplate {
    /// Create a rig template with one initial engine.
    pub fn new(
        name: impl Into<String>,
        instrument_type: InstrumentType,
        engine: EngineTemplate,
    ) -> Self {
        Self {
            id: RigTemplateId::new(),
            name: name.into(),
            instrument_type,
            engines: NonEmptyVec::new(engine),
            description: None,
        }
    }

    #[must_use]
    pub fn with_description(mut self, desc: impl Into<String>) -> Self {
        self.description = Some(desc.into());
        self
    }

    /// Convenience: access modules from the first engine's first layer.
    /// Useful for single-engine/single-layer templates (the common case).
    pub fn modules(&self) -> &[ModuleTemplate] {
        &self.engines.first().layers.first().modules
    }
}

impl Templatable for Rig {
    type Template = RigTemplate;

    fn to_template(&self) -> RigTemplate {
        let engine_templates: Vec<EngineTemplate> =
            self.engines.iter().map(|e| e.to_template()).collect();
        RigTemplate {
            id: RigTemplateId::new(),
            name: self.name.clone(),
            instrument_type: self.instrument_type.clone(),
            engines: NonEmptyVec::from_vec(engine_templates)
                .expect("rig always has at least one engine"),
            description: None,
        }
    }

    fn from_template(t: &RigTemplate) -> Rig {
        let first_engine = Engine::from_template(t.engines.first());
        let mut rig = Rig::new(&t.name, t.instrument_type.clone(), first_engine);
        for engine_template in t.engines.iter().skip(1) {
            rig.add_engine(Engine::from_template(engine_template));
        }
        rig
    }
}

// ─── RackTemplate ────────────────────────────────────────────────

/// Blueprint for a rack — which rigs it contains.
#[derive(Debug, Clone, ::facet::Facet)]
pub struct RackTemplate {
    pub id: RackTemplateId,
    pub name: String,
    pub rigs: Vec<RigTemplate>,
    pub description: Option<String>,
}

impl Templatable for Rack {
    type Template = RackTemplate;

    fn to_template(&self) -> RackTemplate {
        RackTemplate {
            id: RackTemplateId::new(),
            name: self.name.clone(),
            rigs: self.rigs.iter().map(|r| r.to_template()).collect(),
            description: None,
        }
    }

    fn from_template(t: &RackTemplate) -> Rack {
        let mut rack = Rack::new(&t.name);
        for rt in &t.rigs {
            rack.add_rig(Rig::from_template(rt));
        }
        rack
    }
}

// ─── Scene Templates ─────────────────────────────────────────────

/// Captures the structure of a layer scene without snapshot IDs.
#[derive(Debug, Clone, ::facet::Facet)]
pub struct LayerSceneTemplate {
    pub name: String,
    pub module_types: Vec<ModuleType>,
    pub has_standalone_blocks: bool,
    pub enabled: bool,
}

/// Captures the structure of an engine scene.
#[derive(Debug, Clone, ::facet::Facet)]
pub struct EngineSceneTemplate {
    pub name: String,
    pub layer_count: u8,
}

// ─── Tests ───────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn block_round_trip() {
        let block = Block::new("Klon", PluginId::vst3("klon", "Klon Centaur"))
            .with_block_type(BlockType::Drive)
            .with_alias("KTR");

        let template = block.to_template();
        assert_eq!(template.name, "Klon");
        assert_eq!(template.block_type, BlockType::Drive);
        assert_eq!(template.alias.as_deref(), Some("KTR"));

        let instantiated = Block::from_template(&template);
        assert_eq!(instantiated.name, "Klon");
        assert_eq!(instantiated.block_type, BlockType::Drive);
        assert!(instantiated.is_placeholder());
    }

    #[test]
    fn module_round_trip() {
        let mut module = Module::new("Drive", ModuleType::Drive);
        module.grid_width = Some(2);
        let block =
            Block::new("OD", PluginId::vst3("od", "Overdrive")).with_block_type(BlockType::Drive);
        module.add_block(ModuleBlock::new(block, Order::new(0)));

        let template = module.to_template();
        assert_eq!(template.module_type, ModuleType::Drive);
        assert_eq!(template.blocks.len(), 1);
        assert_eq!(template.grid_width, Some(2));

        let instantiated = Module::from_template(&template);
        assert_eq!(instantiated.module_type, ModuleType::Drive);
        assert_eq!(instantiated.blocks.len(), 1);
        assert!(instantiated.blocks[0].block.is_placeholder());
    }

    #[test]
    fn engine_round_trip() {
        let mut engine = Engine::new(
            "Guitar",
            InstrumentType::Guitar,
            Layer::new("Main", LayerIndex::new(1)),
        );
        engine.add_layer(Layer::new("Harmony", LayerIndex::new(2)));

        let template = engine.to_template();
        assert_eq!(template.layers.len(), 2);
        assert_eq!(template.layers.first().name, "Main");

        let instantiated = Engine::from_template(&template);
        assert_eq!(instantiated.layers.len(), 2);
        assert_eq!(instantiated.layers.first().name, "Main");
    }

    #[test]
    fn rig_round_trip() {
        let engine = Engine::new(
            "Main",
            InstrumentType::Guitar,
            Layer::new("Layer 1", LayerIndex::new(1)),
        );
        let rig = Rig::new("Guitar Rig", InstrumentType::Guitar, engine);

        let template = rig.to_template();
        assert_eq!(template.name, "Guitar Rig");
        assert_eq!(template.engines.len(), 1);

        let instantiated = Rig::from_template(&template);
        assert_eq!(instantiated.name, "Guitar Rig");
        assert_eq!(instantiated.engines.len(), 1);
    }

    #[test]
    fn rack_round_trip() {
        let engine = Engine::new(
            "Main",
            InstrumentType::Guitar,
            Layer::new("Layer 1", LayerIndex::new(1)),
        );
        let rig = Rig::new("Rig", InstrumentType::Guitar, engine);
        let mut rack = Rack::new("Guitar Pool");
        rack.add_rig(rig);

        let template = rack.to_template();
        assert_eq!(template.rigs.len(), 1);

        let instantiated = Rack::from_template(&template);
        assert_eq!(instantiated.rigs.len(), 1);
    }

    #[test]
    fn template_strips_plugins() {
        let mut module = Module::new("Drive", ModuleType::Drive);
        let block = Block::new("Real Plugin", PluginId::vst3("com.real", "Real"))
            .with_block_type(BlockType::Drive);
        module.add_block(ModuleBlock::new(block, Order::new(0)));

        let layer = Layer::new("Main", LayerIndex::new(1));
        let mut layer_with_module = layer;
        layer_with_module.add_module(module);

        let engine = Engine::new("Guitar", InstrumentType::Guitar, layer_with_module);
        let rig = Rig::new("Test Rig", InstrumentType::Guitar, engine);

        let template = rig.to_template();
        let new_rig = Rig::from_template(&template);

        // Verify all blocks are placeholders
        for engine in new_rig.engines.iter() {
            for layer in engine.layers.iter() {
                for module in &layer.modules {
                    for mb in &module.blocks {
                        assert!(
                            mb.block.is_placeholder(),
                            "Expected placeholder, got: {:?}",
                            mb.block.plugin_id
                        );
                    }
                }
            }
        }
    }
}
