//! BlockService - ROAM service for block preset operations
//!
//! This service provides RPC access to block presets for browsing and loading.

use facet::Facet;
use std::sync::Arc;
use uuid::Uuid;

use super::core::{BlockPreset, BlockType};

// region:    --- RPC Types

/// Simplified block preset information for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct BlockPresetInfo {
    /// Preset ID
    pub id: Uuid,
    /// Preset name
    pub name: String,
    /// Block type
    pub block_type: BlockType,
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

/// Commands that can be executed on the block service.
#[repr(u8)]
#[derive(Debug, Clone, Facet)]
pub enum BlockCommand {
    /// Load a block preset by ID
    LoadBlockPreset { preset_id: Uuid },
    /// Create a new block preset from current settings
    SaveBlockPreset {
        name: String,
        block_type: BlockType,
        tags: Vec<String>,
    },
    /// Update a block preset
    UpdateBlockPreset {
        preset_id: Uuid,
        name: Option<String>,
        tags: Option<Vec<String>>,
        rating: Option<u8>,
        notes: Option<String>,
    },
    /// Delete a block preset
    DeleteBlockPreset { preset_id: Uuid },
    /// Hide/show a block preset
    SetBlockPresetHidden { preset_id: Uuid, hidden: bool },
}

// endregion: --- Commands

// region:    --- Events

/// Events emitted by the block service.
#[repr(u8)]
#[derive(Debug, Clone, Facet)]
pub enum BlockEvent {
    /// A block preset was loaded
    BlockPresetLoaded { preset: BlockPresetInfo },
    /// A block preset was saved
    BlockPresetSaved { preset: BlockPresetInfo },
    /// A block preset was updated
    BlockPresetUpdated { preset_id: Uuid },
    /// A block preset was deleted
    BlockPresetDeleted { preset_id: Uuid },
}

// endregion: --- Events

// region:    --- Service Trait

/// BlockService provides RPC access to block preset operations.
///
/// This trait defines the contract for block preset management.
/// Implementations can be for REAPER, mock testing, or standalone apps.
#[roam::service]
pub trait BlockService {
    /// Get all available block presets
    async fn get_all_block_presets(&self) -> Vec<BlockPresetInfo>;

    /// Get block presets filtered by type
    async fn get_block_presets_by_type(&self, block_type: BlockType) -> Vec<BlockPresetInfo>;

    /// Get block presets filtered by tag
    async fn get_block_presets_by_tag(&self, tag: String) -> Vec<BlockPresetInfo>;

    /// Search block presets by name
    async fn search_block_presets(&self, query: String) -> Vec<BlockPresetInfo>;

    /// Get a specific block preset by ID
    async fn get_block_preset(&self, preset_id: Uuid) -> Option<BlockPresetInfo>;

    /// Execute a command
    async fn execute(&self, command: BlockCommand) -> Result<(), String>;
}

// endregion: --- Service Trait

// region:    --- Conversion Helpers

impl From<&BlockPreset> for BlockPresetInfo {
    fn from(preset: &BlockPreset) -> Self {
        Self {
            id: preset.id,
            name: preset.name.clone(),
            block_type: preset.block.block_type,
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
    /// Local client for in-process BlockService calls
    ///
    /// Wraps any BlockService implementation and provides direct async method calls.
    client: LocalBlockClient,
    service: BlockService,
    methods: {
        /// Get all available block presets
        async fn get_all_block_presets() -> Vec<BlockPresetInfo>;

        /// Get block presets filtered by type
        async fn get_block_presets_by_type(block_type: BlockType) -> Vec<BlockPresetInfo>;

        /// Get block presets filtered by tag
        async fn get_block_presets_by_tag(tag: String) -> Vec<BlockPresetInfo>;

        /// Search block presets by name
        async fn search_block_presets(query: String) -> Vec<BlockPresetInfo>;

        /// Get a specific block preset by ID
        async fn get_block_preset(preset_id: Uuid) -> Option<BlockPresetInfo>;

        /// Execute a command
        async fn execute(command: BlockCommand) -> Result<(), String>;
    }
}

// endregion: --- Local Client
