//! Layer — a processing lane within an engine.
//!
//! Layers hold modules (Drive, Amp, EQ, etc.) and optional standalone blocks.
//! Each layer has a 1-based [`LayerIndex`] enforced by the type system,
//! volume/pan/key-range controls, and MIDI channel routing.

use crate::block::Block;
use crate::id::{LayerId, ModuleId};
use crate::module::{Module, ModuleType};
use crate::normalized::{MidiChannel, MidiNote, NormalizedF64, Pan};
use crate::tags::{Taggable, Tags};
use crate::version::LayerIndex;

// ─────────────────────────────────────────────────────────────────────────────
// KeyRange — enforces low ≤ high
// ─────────────────────────────────────────────────────────────────────────────

/// MIDI key range with the invariant that `low <= high`.
///
/// Construction returns `None` if the range is inverted.
#[derive(Debug, Clone, Copy, PartialEq, Eq, ::facet::Facet)]
pub struct KeyRange {
    low: MidiNote,
    high: MidiNote,
}

impl KeyRange {
    /// Create a key range. Returns `None` if `low > high`.
    pub fn new(low: MidiNote, high: MidiNote) -> Option<Self> {
        if low.get() <= high.get() {
            Some(Self { low, high })
        } else {
            None
        }
    }

    /// Full keyboard range: MIDI notes 0–127.
    pub fn full() -> Self {
        Self {
            low: MidiNote::MIN,
            high: MidiNote::MAX,
        }
    }

    pub fn low(&self) -> MidiNote {
        self.low
    }

    pub fn high(&self) -> MidiNote {
        self.high
    }

    /// Check if a MIDI note falls within this range (inclusive).
    pub fn contains(&self, note: MidiNote) -> bool {
        note.get() >= self.low.get() && note.get() <= self.high.get()
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Layer
// ─────────────────────────────────────────────────────────────────────────────

/// A processing lane within an engine.
///
/// Holds modules (named processing stages like Drive, Amp, EQ) and
/// optional standalone blocks that don't belong to any module.
#[derive(Debug, Clone, PartialEq, ::facet::Facet)]
pub struct Layer {
    pub id: LayerId,
    /// 1-based index within the engine. Layer 1, Layer 2, Layer 3, etc.
    pub index: LayerIndex,
    pub name: String,
    /// Processing modules in signal-chain order.
    pub modules: Vec<Module>,
    /// Blocks that don't belong to any module (e.g. utility, routing).
    pub standalone_blocks: Vec<Block>,
    pub enabled: bool,
    pub volume: NormalizedF64,
    pub pan: Pan,
    /// Optional key range for keyboard splits.
    pub key_range: Option<KeyRange>,
    /// Optional MIDI channel filter (None = omni).
    pub midi_channel: Option<MidiChannel>,
    pub tags: Tags,
}

impl Layer {
    /// Create a new enabled layer at the given 1-based index.
    pub fn new(name: impl Into<String>, index: LayerIndex) -> Self {
        Self {
            id: LayerId::new(),
            index,
            name: name.into(),
            modules: Vec::new(),
            standalone_blocks: Vec::new(),
            enabled: true,
            volume: NormalizedF64::ONE,
            pan: Pan::CENTER,
            key_range: None,
            midi_channel: None,
            tags: Tags::new(),
        }
    }

    /// Add a module to this layer.
    pub fn add_module(&mut self, module: Module) {
        self.modules.push(module);
    }

    /// Add a standalone block to this layer.
    pub fn add_standalone_block(&mut self, block: Block) {
        self.standalone_blocks.push(block);
    }

    /// Find a module by type.
    pub fn module_by_type(&self, module_type: ModuleType) -> Option<&Module> {
        self.modules.iter().find(|m| m.module_type == module_type)
    }

    /// Find a module by type (mutable).
    pub fn module_by_type_mut(&mut self, module_type: ModuleType) -> Option<&mut Module> {
        self.modules
            .iter_mut()
            .find(|m| m.module_type == module_type)
    }

    /// Find a module by ID.
    pub fn module_by_id(&self, id: ModuleId) -> Option<&Module> {
        self.modules.iter().find(|m| m.id == id)
    }

    /// Find a module by ID (mutable).
    pub fn module_by_id_mut(&mut self, id: ModuleId) -> Option<&mut Module> {
        self.modules.iter_mut().find(|m| m.id == id)
    }

    /// Set the key range for keyboard splits.
    pub fn set_key_range(&mut self, range: KeyRange) {
        self.key_range = Some(range);
    }
}

impl Taggable for Layer {
    fn tags(&self) -> &Tags {
        &self.tags
    }

    fn tags_mut(&mut self) -> &mut Tags {
        &mut self.tags
    }

    fn name(&self) -> &str {
        &self.name
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::module::Module;

    #[test]
    fn key_range_valid() {
        let range = KeyRange::new(MidiNote::new(36), MidiNote::new(84));
        assert!(range.is_some());
        let range = range.unwrap();
        assert_eq!(range.low().get(), 36);
        assert_eq!(range.high().get(), 84);
    }

    #[test]
    fn key_range_equal_bounds_valid() {
        let range = KeyRange::new(MidiNote::new(60), MidiNote::new(60));
        assert!(range.is_some());
    }

    #[test]
    fn key_range_inverted_returns_none() {
        let range = KeyRange::new(MidiNote::new(100), MidiNote::new(50));
        assert!(range.is_none());
    }

    #[test]
    fn key_range_contains() {
        let range = KeyRange::new(MidiNote::new(36), MidiNote::new(84)).unwrap();
        assert!(range.contains(MidiNote::new(36)));
        assert!(range.contains(MidiNote::new(60)));
        assert!(range.contains(MidiNote::new(84)));
        assert!(!range.contains(MidiNote::new(35)));
        assert!(!range.contains(MidiNote::new(85)));
    }

    #[test]
    fn key_range_full() {
        let range = KeyRange::full();
        assert_eq!(range.low().get(), 0);
        assert_eq!(range.high().get(), 127);
    }

    #[test]
    fn layer_defaults() {
        let layer = Layer::new("Main", LayerIndex::new(1));
        assert!(layer.enabled);
        assert_eq!(layer.volume.get(), 1.0);
        assert_eq!(layer.pan.get(), 0.0);
        assert_eq!(layer.index.get(), 1);
        assert!(layer.key_range.is_none());
        assert!(layer.modules.is_empty());
        assert!(layer.standalone_blocks.is_empty());
    }

    #[test]
    fn layer_with_modules() {
        let mut layer = Layer::new("Main", LayerIndex::new(1));
        layer.add_module(Module::new("Drive", ModuleType::Drive));
        layer.add_module(Module::new("Amp", ModuleType::Amp));

        assert_eq!(layer.modules.len(), 2);
        assert!(layer.module_by_type(ModuleType::Drive).is_some());
        assert!(layer.module_by_type(ModuleType::Amp).is_some());
        assert!(layer.module_by_type(ModuleType::Eq).is_none());
    }

    #[test]
    fn layer_module_by_id() {
        let mut layer = Layer::new("Main", LayerIndex::new(1));
        let module = Module::new("Drive", ModuleType::Drive);
        let module_id = module.id;
        layer.add_module(module);

        assert!(layer.module_by_id(module_id).is_some());
        assert!(layer.module_by_id(ModuleId::new()).is_none());
    }

    #[test]
    fn layer_standalone_blocks() {
        let mut layer = Layer::new("Main", LayerIndex::new(1));
        let block =
            crate::block::Block::new("Utility", crate::block::PluginId::vst3("util", "Utility"));
        layer.add_standalone_block(block);
        assert_eq!(layer.standalone_blocks.len(), 1);
    }

    #[test]
    fn layer_uses_one_based_index() {
        let layer = Layer::new("Layer A", LayerIndex::new(1));
        assert_eq!(layer.index.get(), 1);
        assert_eq!(layer.index.to_zero_based(), 0);

        let layer3 = Layer::new("Layer C", LayerIndex::new(3));
        assert_eq!(layer3.index.get(), 3);
        assert_eq!(layer3.index.to_zero_based(), 2);
    }
}
