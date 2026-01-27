//! Mock implementation of BlockService for testing and standalone apps

use std::sync::Arc;
use uuid::Uuid;

use super::block_service::{BlockCommand, BlockPresetInfo, BlockService};
use super::core::{BlockPreset, BlockType};
use super::mock::MockRig;

/// Mock implementation of BlockService
#[derive(Clone)]
pub struct MockBlockService {
    mock_rig: Arc<MockRig>,
}

impl MockBlockService {
    /// Create a new mock block service
    pub fn new(mock_rig: Arc<MockRig>) -> Self {
        Self { mock_rig }
    }

    /// Get block presets from mock data
    fn get_presets(&self) -> Vec<BlockPreset> {
        self.mock_rig.block_presets()
    }
}

impl BlockService for MockBlockService {
    async fn get_all_block_presets(&self, _cx: &roam::Context) -> Vec<BlockPresetInfo> {
        self.get_presets()
            .iter()
            .filter(|p| !p.hidden)
            .map(|p| p.into())
            .collect()
    }

    async fn get_block_presets_by_type(&self, _cx: &roam::Context, block_type: BlockType) -> Vec<BlockPresetInfo> {
        self.get_presets()
            .iter()
            .filter(|p| !p.hidden && p.block.block_type == block_type)
            .map(|p| p.into())
            .collect()
    }

    async fn get_block_presets_by_tag(&self, _cx: &roam::Context, tag: String) -> Vec<BlockPresetInfo> {
        let tag_uuid = Uuid::parse_str(&tag).ok();
        self.get_presets()
            .iter()
            .filter(|p| !p.hidden && tag_uuid.map_or(false, |id| p.tags.has(id)))
            .map(|p| p.into())
            .collect()
    }

    async fn search_block_presets(&self, _cx: &roam::Context, query: String) -> Vec<BlockPresetInfo> {
        let query_lower = query.to_lowercase();
        self.get_presets()
            .iter()
            .filter(|p| {
                !p.hidden && p.name.to_lowercase().contains(&query_lower)
            })
            .map(|p| p.into())
            .collect()
    }

    async fn get_block_preset(&self, _cx: &roam::Context, preset_id: Uuid) -> Option<BlockPresetInfo> {
        self.get_presets()
            .iter()
            .find(|p| p.id == preset_id)
            .map(|p| p.into())
    }

    async fn execute(&self, _cx: &roam::Context, command: BlockCommand) -> Result<(), String> {
        match command {
            BlockCommand::LoadBlockPreset { preset_id } => {
                // In a real implementation, this would load the preset into the current rig
                tracing::info!("Loading block preset: {}", preset_id);
                Ok(())
            }
            BlockCommand::SaveBlockPreset { name, block_type, tags } => {
                tracing::info!("Saving block preset: {}", name);
                Ok(())
            }
            BlockCommand::UpdateBlockPreset { preset_id, .. } => {
                tracing::info!("Updating block preset: {}", preset_id);
                Ok(())
            }
            BlockCommand::DeleteBlockPreset { preset_id } => {
                tracing::info!("Deleting block preset: {}", preset_id);
                Ok(())
            }
            BlockCommand::SetBlockPresetHidden { preset_id, hidden } => {
                tracing::info!("Setting block preset {} hidden: {}", preset_id, hidden);
                Ok(())
            }
        }
    }
}
