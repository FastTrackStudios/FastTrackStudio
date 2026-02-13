//! Service-driven controller for a single block with three normalized parameters.
//!
//! The controller exposes **collection / variant** terminology:
//!
//! - A **collection** is a named group of block-state variants (backed by `Preset`).
//! - A **variant** is a specific block-state snapshot within a collection (backed by `Snapshot`).
//!
//! Module-level equivalents (`ModulePreset` / `ModuleSnapshot`) follow the same
//! naming convention via `list_module_collections` / `load_module_variant`.

use signal_live::SignalLive;
use signal_proto::{
    Block, BlockService, BlockType, ModulePreset, ModulePresetId, ModuleSnapshot, ModuleSnapshotId,
    Preset, PresetId, SnapshotId,
};
use std::sync::Arc;

pub trait ContextFactory: Send + Sync {
    fn make_context(&self) -> roam::Context;
}

pub type SharedContextFactory = Arc<dyn ContextFactory>;

#[derive(Default)]
pub struct DefaultContextFactory;

impl ContextFactory for DefaultContextFactory {
    fn make_context(&self) -> roam::Context {
        roam::Context::new(
            Default::default(),
            Default::default(),
            Default::default(),
            Default::default(),
            vec![],
        )
    }
}

pub struct SignalController<S = SignalLive>
where
    S: BlockService,
{
    service: Arc<S>,
    context_factory: SharedContextFactory,
}

impl<S> Clone for SignalController<S>
where
    S: BlockService,
{
    fn clone(&self) -> Self {
        Self {
            service: self.service.clone(),
            context_factory: self.context_factory.clone(),
        }
    }
}

impl<S> PartialEq for SignalController<S>
where
    S: BlockService,
{
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.service, &other.service)
    }
}

impl<S> Eq for SignalController<S> where S: BlockService {}

impl<S> SignalController<S>
where
    S: BlockService,
{
    pub fn new(service: Arc<S>) -> Self {
        Self::new_with_context(service, Arc::new(DefaultContextFactory))
    }

    pub fn new_with_context(service: Arc<S>, context_factory: SharedContextFactory) -> Self {
        Self {
            service,
            context_factory,
        }
    }

    // region: --- Block operations

    pub async fn get_block(&self, block_type: BlockType) -> Block {
        let cx = self.context_factory.make_context();
        self.service.get_block(&cx, block_type).await
    }

    pub async fn set_block(&self, block_type: BlockType, block: Block) -> Block {
        let cx = self.context_factory.make_context();
        self.service.set_block(&cx, block_type, block).await
    }

    pub async fn get_value(&self, block_type: BlockType) -> f32 {
        self.get_block(block_type).await.first_value().unwrap_or(0.0)
    }

    pub async fn set_value(&self, block_type: BlockType, value: f32) -> Block {
        let mut block = self.get_block(block_type).await;
        block.set_first_value(value);
        self.set_block(block_type, block).await
    }

    // endregion: --- Block operations

    // region: --- Collection + variant operations (block-level)

    /// List all collections for a given block type.
    pub async fn list_collections(&self, block_type: BlockType) -> Vec<Preset> {
        let cx = self.context_factory.make_context();
        self.service.list_presets(&cx, block_type).await
    }

    /// Load the default variant from a collection, returning its block state.
    pub async fn load_collection_default(
        &self,
        block_type: BlockType,
        collection_id: impl Into<PresetId>,
    ) -> Option<Block> {
        let cx = self.context_factory.make_context();
        self.service
            .load_preset(&cx, block_type, collection_id.into())
            .await
            .map(|snapshot| snapshot.block())
    }

    /// Load a specific variant from a collection, returning its block state.
    pub async fn load_variant(
        &self,
        block_type: BlockType,
        collection_id: impl Into<PresetId>,
        variant_id: impl Into<SnapshotId>,
    ) -> Option<Block> {
        let cx = self.context_factory.make_context();
        self.service
            .load_preset_snapshot(&cx, block_type, collection_id.into(), variant_id.into())
            .await
            .map(|snapshot| snapshot.block())
    }

    // endregion: --- Collection + variant operations (block-level)

    // region: --- Collection + variant operations (module-level)

    /// List all module-level collections.
    pub async fn list_module_collections(&self) -> Vec<ModulePreset> {
        let cx = self.context_factory.make_context();
        self.service.list_module_presets(&cx).await
    }

    /// Load the default variant from a module collection.
    pub async fn load_module_collection_default(
        &self,
        collection_id: impl Into<ModulePresetId>,
    ) -> Option<ModuleSnapshot> {
        let cx = self.context_factory.make_context();
        self.service
            .load_module_preset(&cx, collection_id.into())
            .await
    }

    /// Load a specific variant from a module collection.
    pub async fn load_module_variant(
        &self,
        collection_id: impl Into<ModulePresetId>,
        variant_id: impl Into<ModuleSnapshotId>,
    ) -> Option<ModuleSnapshot> {
        let cx = self.context_factory.make_context();
        self.service
            .load_module_preset_snapshot(&cx, collection_id.into(), variant_id.into())
            .await
    }

    // endregion: --- Collection + variant operations (module-level)

    // region: --- Deprecated shims

    /// Deprecated: use [`list_collections`] instead.
    #[deprecated(note = "use list_collections")]
    pub async fn list_presets(&self, block_type: BlockType) -> Vec<Preset> {
        self.list_collections(block_type).await
    }

    /// Deprecated: use [`load_collection_default`] instead.
    #[deprecated(note = "use load_collection_default")]
    pub async fn load_preset(
        &self,
        block_type: BlockType,
        preset_id: impl Into<PresetId>,
    ) -> Option<Block> {
        self.load_collection_default(block_type, preset_id).await
    }

    /// Deprecated: use [`load_variant`] instead.
    #[deprecated(note = "use load_variant")]
    pub async fn load_preset_snapshot(
        &self,
        block_type: BlockType,
        preset_id: impl Into<PresetId>,
        snapshot_id: impl Into<SnapshotId>,
    ) -> Option<Block> {
        self.load_variant(block_type, preset_id, snapshot_id).await
    }

    /// Deprecated: use [`list_module_collections`] instead.
    #[deprecated(note = "use list_module_collections")]
    pub async fn list_module_presets(&self) -> Vec<ModulePreset> {
        self.list_module_collections().await
    }

    /// Deprecated: use [`load_module_collection_default`] instead.
    #[deprecated(note = "use load_module_collection_default")]
    pub async fn load_module_preset(
        &self,
        preset_id: impl Into<ModulePresetId>,
    ) -> Option<ModuleSnapshot> {
        self.load_module_collection_default(preset_id).await
    }

    /// Deprecated: use [`load_module_variant`] instead.
    #[deprecated(note = "use load_module_variant")]
    pub async fn load_module_preset_snapshot(
        &self,
        preset_id: impl Into<ModulePresetId>,
        snapshot_id: impl Into<ModuleSnapshotId>,
    ) -> Option<ModuleSnapshot> {
        self.load_module_variant(preset_id, snapshot_id).await
    }

    // endregion: --- Deprecated shims
}
