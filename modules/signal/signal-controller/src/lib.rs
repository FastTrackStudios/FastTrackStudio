//! Service-driven controller for a single block with three normalized parameters.

use signal_live::SignalLive;
use signal_proto::{
    Block, BlockService, BlockType, ModulePreset, ModulePresetId, ModuleSnapshot, ModuleSnapshotId,
    PresetId, SnapshotId,
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

    pub async fn load_preset(
        &self,
        block_type: BlockType,
        preset_id: impl Into<PresetId>,
    ) -> Option<Block> {
        let cx = self.context_factory.make_context();
        self.service
            .load_preset(&cx, block_type, preset_id.into())
            .await
            .map(|snapshot| snapshot.block())
    }

    pub async fn load_preset_snapshot(
        &self,
        block_type: BlockType,
        preset_id: impl Into<PresetId>,
        snapshot_id: impl Into<SnapshotId>,
    ) -> Option<Block> {
        let cx = self.context_factory.make_context();
        self.service
            .load_preset_snapshot(&cx, block_type, preset_id.into(), snapshot_id.into())
            .await
            .map(|snapshot| snapshot.block())
    }

    pub async fn list_presets(&self, block_type: BlockType) -> Vec<signal_proto::Preset> {
        let cx = self.context_factory.make_context();
        self.service.list_presets(&cx, block_type).await
    }

    pub async fn list_module_presets(&self) -> Vec<ModulePreset> {
        let cx = self.context_factory.make_context();
        self.service.list_module_presets(&cx).await
    }

    pub async fn load_module_preset(
        &self,
        preset_id: impl Into<ModulePresetId>,
    ) -> Option<ModuleSnapshot> {
        let cx = self.context_factory.make_context();
        self.service.load_module_preset(&cx, preset_id.into()).await
    }

    pub async fn load_module_preset_snapshot(
        &self,
        preset_id: impl Into<ModulePresetId>,
        snapshot_id: impl Into<ModuleSnapshotId>,
    ) -> Option<ModuleSnapshot> {
        let cx = self.context_factory.make_context();
        self.service
            .load_module_preset_snapshot(&cx, preset_id.into(), snapshot_id.into())
            .await
    }
}
