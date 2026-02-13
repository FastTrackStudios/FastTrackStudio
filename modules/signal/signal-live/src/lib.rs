//! Live service implementation for signal2.

use roam::Context;
use signal_proto::{
    Block, BlockService, BlockType, ModulePreset, ModulePresetId, ModuleSnapshot, ModuleSnapshotId,
    Preset, PresetId, Snapshot, SnapshotId,
};
use signal_storage::{BlockRepo, BlockRepoLive, DatabaseConnection};
use std::sync::Arc;

pub struct SignalLive<R = BlockRepoLive>
where
    R: BlockRepo,
{
    repo: Arc<R>,
}

impl<R> SignalLive<R>
where
    R: BlockRepo,
{
    pub fn new(repo: Arc<R>) -> Self {
        Self { repo }
    }
}

impl SignalLive<BlockRepoLive> {
    pub fn from_db(db: DatabaseConnection) -> Self {
        Self::new(Arc::new(BlockRepoLive::new(db)))
    }
}

impl<R> BlockService for SignalLive<R>
where
    R: BlockRepo,
{
    async fn get_block(&self, _cx: &Context, block_type: BlockType) -> Block {
        self.repo
            .load_block(block_type)
            .await
            .ok()
            .flatten()
            .unwrap_or_default()
    }

    async fn set_block(&self, _cx: &Context, block_type: BlockType, block: Block) -> Block {
        let _ = self.repo.save_block(block_type, block.clone()).await;
        block
    }

    async fn list_presets(&self, _cx: &Context, block_type: BlockType) -> Vec<Preset> {
        self.repo.list_presets(block_type).await.unwrap_or_default()
    }

    async fn load_preset(
        &self,
        _cx: &Context,
        block_type: BlockType,
        preset_id: PresetId,
    ) -> Option<Snapshot> {
        let snapshot = self
            .repo
            .load_preset(block_type, &preset_id)
            .await
            .ok()
            .flatten();
        if let Some(snapshot) = snapshot.as_ref() {
            let _ = self.repo.save_block(block_type, snapshot.block()).await;
        }
        snapshot
    }

    async fn load_preset_snapshot(
        &self,
        _cx: &Context,
        block_type: BlockType,
        preset_id: PresetId,
        snapshot_id: SnapshotId,
    ) -> Option<Snapshot> {
        let snapshot = self
            .repo
            .load_preset_snapshot(block_type, &preset_id, &snapshot_id)
            .await
            .ok()
            .flatten();
        if let Some(snapshot) = snapshot.as_ref() {
            let _ = self.repo.save_block(block_type, snapshot.block()).await;
        }
        snapshot
    }

    async fn list_module_presets(&self, _cx: &Context) -> Vec<ModulePreset> {
        self.repo.list_module_presets().await.unwrap_or_default()
    }

    async fn load_module_preset(
        &self,
        _cx: &Context,
        preset_id: ModulePresetId,
    ) -> Option<ModuleSnapshot> {
        self.repo
            .load_module_preset(&preset_id)
            .await
            .ok()
            .flatten()
    }

    async fn load_module_preset_snapshot(
        &self,
        _cx: &Context,
        preset_id: ModulePresetId,
        snapshot_id: ModuleSnapshotId,
    ) -> Option<ModuleSnapshot> {
        self.repo
            .load_module_preset_snapshot(&preset_id, &snapshot_id)
            .await
            .ok()
            .flatten()
    }
}
