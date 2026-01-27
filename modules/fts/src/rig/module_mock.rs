//! Mock implementation of ModuleService for testing and standalone apps

use std::sync::Arc;
use uuid::Uuid;

use super::module_service::{ModuleCommand, ModulePresetInfo, ModuleService};
use super::core::{ModulePreset, ModuleType};
use super::mock::MockRig;

/// Mock implementation of ModuleService
#[derive(Clone)]
pub struct MockModuleService {
    mock_rig: Arc<MockRig>,
}

impl MockModuleService {
    /// Create a new mock module service
    pub fn new(mock_rig: Arc<MockRig>) -> Self {
        Self { mock_rig }
    }

    /// Get module presets from mock data
    fn get_presets(&self) -> Vec<ModulePreset> {
        self.mock_rig.module_presets()
    }
}

impl ModuleService for MockModuleService {
    async fn get_all_module_presets(&self, _cx: &roam::Context) -> Vec<ModulePresetInfo> {
        self.get_presets()
            .iter()
            .filter(|p| !p.hidden)
            .map(|p| p.into())
            .collect()
    }

    async fn get_module_presets_by_type(&self, _cx: &roam::Context, module_type: ModuleType) -> Vec<ModulePresetInfo> {
        self.get_presets()
            .iter()
            .filter(|p| !p.hidden && p.module.module_type == module_type)
            .map(|p| p.into())
            .collect()
    }

    async fn get_module_presets_by_tag(&self, _cx: &roam::Context, tag: String) -> Vec<ModulePresetInfo> {
        let tag_uuid = Uuid::parse_str(&tag).ok();
        self.get_presets()
            .iter()
            .filter(|p| !p.hidden && tag_uuid.map_or(false, |id| p.tags.has(id)))
            .map(|p| p.into())
            .collect()
    }

    async fn search_module_presets(&self, _cx: &roam::Context, query: String) -> Vec<ModulePresetInfo> {
        let query_lower = query.to_lowercase();
        self.get_presets()
            .iter()
            .filter(|p| {
                !p.hidden && p.name.to_lowercase().contains(&query_lower)
            })
            .map(|p| p.into())
            .collect()
    }

    async fn get_module_preset(&self, _cx: &roam::Context, preset_id: Uuid) -> Option<ModulePresetInfo> {
        self.get_presets()
            .iter()
            .find(|p| p.id == preset_id)
            .map(|p| p.into())
    }

    async fn execute(&self, _cx: &roam::Context, command: ModuleCommand) -> Result<(), String> {
        match command {
            ModuleCommand::LoadModulePreset { preset_id } => {
                // In a real implementation, this would load the preset into the current rig
                tracing::info!("Loading module preset: {}", preset_id);
                Ok(())
            }
            ModuleCommand::SaveModulePreset { name, module_type, tags } => {
                tracing::info!("Saving module preset: {}", name);
                Ok(())
            }
            ModuleCommand::UpdateModulePreset { preset_id, .. } => {
                tracing::info!("Updating module preset: {}", preset_id);
                Ok(())
            }
            ModuleCommand::DeleteModulePreset { preset_id } => {
                tracing::info!("Deleting module preset: {}", preset_id);
                Ok(())
            }
            ModuleCommand::SetModulePresetHidden { preset_id, hidden } => {
                tracing::info!("Setting module preset {} hidden: {}", preset_id, hidden);
                Ok(())
            }
        }
    }
}
