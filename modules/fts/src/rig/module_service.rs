//! ModuleService - ROAM service for module preset operations
//!
//! This service provides RPC access to module presets for browsing and loading.

use facet::Facet;
use std::sync::Arc;
use uuid::Uuid;

use super::core::{ModulePreset, ModuleType};

// region:    --- RPC Types

/// Simplified module preset information for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct ModulePresetInfo {
    /// Preset ID
    pub id: Uuid,
    /// Preset name
    pub name: String,
    /// Module type
    pub module_type: ModuleType,
    /// Number of blocks in this module
    pub block_count: usize,
    /// Block names for display
    pub block_names: Vec<String>,
    /// Tag IDs for categorization
    pub tag_ids: Vec<Uuid>,
    /// User rating (0-5)
    pub rating: u8,
    /// Optional notes
    pub notes: Option<String>,
    /// Whether hidden from browser
    pub hidden: bool,
}

// endregion: --- RPC Types

// region:    --- Commands

/// Commands that can be executed on the module service.
#[repr(u8)]
#[derive(Debug, Clone, Facet)]
pub enum ModuleCommand {
    /// Load a module preset by ID
    LoadModulePreset { preset_id: Uuid },
    /// Create a new module preset from current settings
    SaveModulePreset {
        name: String,
        module_type: ModuleType,
        tags: Vec<String>,
    },
    /// Update a module preset
    UpdateModulePreset {
        preset_id: Uuid,
        name: Option<String>,
        tags: Option<Vec<String>>,
        rating: Option<u8>,
        notes: Option<String>,
    },
    /// Delete a module preset
    DeleteModulePreset { preset_id: Uuid },
    /// Hide/show a module preset
    SetModulePresetHidden { preset_id: Uuid, hidden: bool },
}

// endregion: --- Commands

// region:    --- Events

/// Events emitted by the module service.
#[repr(u8)]
#[derive(Debug, Clone, Facet)]
pub enum ModuleEvent {
    /// A module preset was loaded
    ModulePresetLoaded { preset: ModulePresetInfo },
    /// A module preset was saved
    ModulePresetSaved { preset: ModulePresetInfo },
    /// A module preset was updated
    ModulePresetUpdated { preset_id: Uuid },
    /// A module preset was deleted
    ModulePresetDeleted { preset_id: Uuid },
}

// endregion: --- Events

// region:    --- Service Trait

/// ModuleService provides RPC access to module preset operations.
///
/// This trait defines the contract for module preset management.
/// Implementations can be for REAPER, mock testing, or standalone apps.
#[roam::service]
pub trait ModuleService {
    /// Get all available module presets
    async fn get_all_module_presets(&self) -> Vec<ModulePresetInfo>;

    /// Get module presets filtered by type
    async fn get_module_presets_by_type(&self, module_type: ModuleType) -> Vec<ModulePresetInfo>;

    /// Get module presets filtered by tag
    async fn get_module_presets_by_tag(&self, tag: String) -> Vec<ModulePresetInfo>;

    /// Search module presets by name
    async fn search_module_presets(&self, query: String) -> Vec<ModulePresetInfo>;

    /// Get a specific module preset by ID
    async fn get_module_preset(&self, preset_id: Uuid) -> Option<ModulePresetInfo>;

    /// Execute a command
    async fn execute(&self, command: ModuleCommand) -> Result<(), String>;
}

// endregion: --- Service Trait

// region:    --- Conversion Helpers

impl From<&ModulePreset> for ModulePresetInfo {
    fn from(preset: &ModulePreset) -> Self {
        Self {
            id: preset.id,
            name: preset.name.clone(),
            module_type: preset.module.module_type,
            block_count: preset.module.blocks.len(),
            block_names: preset.module.blocks.iter().map(|b| b.name.clone()).collect(),
            tag_ids: preset.tags.all().into_iter().collect(),
            rating: preset.rating,
            notes: preset.notes.clone(),
            hidden: preset.hidden,
        }
    }
}

// endregion: --- Conversion Helpers

// region:    --- Local Client

crate::define_local_client! {
    /// Local client for in-process ModuleService calls
    ///
    /// Wraps any ModuleService implementation and provides direct async method calls.
    client: LocalModuleClient,
    service: ModuleService,
    methods: {
        /// Get all available module presets
        async fn get_all_module_presets() -> Vec<ModulePresetInfo>;

        /// Get module presets filtered by type
        async fn get_module_presets_by_type(module_type: ModuleType) -> Vec<ModulePresetInfo>;

        /// Get module presets filtered by tag
        async fn get_module_presets_by_tag(tag: String) -> Vec<ModulePresetInfo>;

        /// Search module presets by name
        async fn search_module_presets(query: String) -> Vec<ModulePresetInfo>;

        /// Get a specific module preset by ID
        async fn get_module_preset(preset_id: Uuid) -> Option<ModulePresetInfo>;

        /// Execute a command
        async fn execute(command: ModuleCommand) -> Result<(), String>;
    }
}

// endregion: --- Local Client
