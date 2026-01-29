//! RigService - ROAM service for rig operations
//!
//! This service provides RPC access to rig state and preset management.
//! Commands are executed via the service implementation (Mock or REAPER).
//!
//! Note: This module uses simplified RPC types that can be easily serialized.
//! Service implementations convert between these and the full domain types.

use facet::Facet;
use roam::{Context, Tx};
use std::sync::Arc;
use uuid::Uuid;

use super::core::{
    GlobalBlock, InstrumentType, PatchCategory, PerformanceSetlist, PerformanceSong, Preset,
    PresetCategory, Profile, Rig, Scene, SectionRouting,
};

// region:    --- RPC Types

/// Simplified profile information for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct ProfileInfo {
    /// Profile ID
    pub id: Uuid,
    /// Profile name
    pub name: String,
    /// Associated rig ID
    pub rig_id: Uuid,
    /// Number of presets in this profile
    pub preset_count: usize,
    /// Number of scenes in this profile
    #[facet(default)]
    pub scene_count: usize,
    /// Scene names for quick display
    #[facet(default)]
    pub scene_names: Vec<String>,
    /// Detailed scene info
    #[facet(default)]
    pub scenes: Vec<SceneInfo>,
}

/// Simplified rig information for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct RigInfo {
    /// Rig ID
    pub id: Uuid,
    /// Rig name
    pub name: String,
    /// Instrument type
    pub instrument_type: String,
    /// Number of sections
    pub section_count: usize,
    /// Number of global blocks
    pub global_block_count: usize,
}

/// Simplified section information for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct SectionInfo {
    /// Section ID
    pub id: Uuid,
    /// Section name
    pub name: String,
    /// Number of layers
    pub layer_count: usize,
    /// Whether section is enabled
    pub enabled: bool,
    /// Section volume
    pub volume: f64,
    /// Routing type
    pub routing: String,
}

/// Simplified global block information for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct GlobalBlockInfo {
    /// Block ID
    pub id: Uuid,
    /// Block name
    pub name: String,
    /// Plugin display name
    pub plugin_name: String,
    /// Order in chain
    pub order: u8,
    /// Whether bypassed
    pub bypassed: bool,
    /// Whether enabled (inverse of bypassed, for convenience)
    pub enabled: bool,
}

/// Simplified preset information for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct PresetInfo {
    /// Preset ID
    pub id: Uuid,
    /// Preset name
    pub name: String,
    /// Category display string (e.g., "Clean/Blues")
    pub category: String,
    /// Tag IDs applied to this preset (manual only, for display)
    #[facet(default)]
    pub tag_ids: Vec<Uuid>,
    /// Star rating (0-5, 0 = unrated)
    #[facet(default)]
    pub rating: u8,
    /// Number of snapshots
    pub snapshot_count: usize,
    /// Snapshot names for quick display
    pub snapshot_names: Vec<String>,
    /// Detailed snapshot info (with tags)
    #[facet(default)]
    pub snapshots: Vec<SnapshotInfo>,
    /// Preset-specific blocks (each preset has its own chain)
    #[facet(default)]
    pub blocks: Vec<PresetBlockInfo>,
}

/// Simplified snapshot information for preset display.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct SnapshotInfo {
    /// Snapshot ID
    pub id: Uuid,
    /// Snapshot name
    pub name: String,
    /// Tag IDs applied to this snapshot (manual only, for display)
    /// Auto-derived tags are NOT included since they're redundant with the name
    #[facet(default)]
    pub tag_ids: Vec<Uuid>,
    /// Block parameter overrides for this snapshot
    #[facet(default)]
    pub block_overrides: Vec<BlockOverrideInfo>,
}

/// Block information within a preset
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct PresetBlockInfo {
    /// Block ID
    pub id: Uuid,
    /// Block name (e.g., "Drive", "Amp", "Delay")
    pub name: String,
    /// Block type for UI rendering
    pub block_type: String,
    /// Order in signal chain
    pub order: u8,
    /// Whether bypassed (default state)
    pub bypassed: bool,
    /// Parameters with their default values
    pub parameters: Vec<BlockParameterInfo>,
}

/// Parameter information for a block
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct BlockParameterInfo {
    /// Parameter ID
    pub id: String,
    /// Human-readable name
    pub name: String,
    /// Normalized value (0.0 - 1.0)
    pub value: f64,
    /// Min display value
    pub min_display: f64,
    /// Max display value
    pub max_display: f64,
    /// Unit suffix
    pub unit: String,
}

/// Block override info for a snapshot
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct BlockOverrideInfo {
    /// Block ID this applies to
    pub block_id: Uuid,
    /// Bypass override (None = use preset default)
    pub bypassed: Option<bool>,
    /// Parameter overrides
    pub parameters: Vec<ParameterOverrideInfo>,
}

/// Single parameter override
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct ParameterOverrideInfo {
    /// Parameter ID
    pub param_id: String,
    /// Overridden value
    pub value: f64,
}

/// Simplified patch information for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct PatchInfo {
    /// Patch ID
    pub id: Uuid,
    /// Patch name
    pub name: String,
    /// Category base
    pub category_base: String,
    /// Category style (if any)
    pub category_style: Option<String>,
    /// Number of variations
    pub variation_count: usize,
}

/// Simplified song information for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct SongInfo {
    /// Song index
    pub index: usize,
    /// Song name
    pub name: String,
    /// Number of scenes
    pub scene_count: usize,
    /// Current scene index
    pub current_scene_index: usize,
    /// Scene names for display in list
    pub scene_names: Vec<String>,
}

/// Simplified scene information for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct SceneInfo {
    /// Scene index
    pub index: usize,
    /// Scene name
    pub name: String,
    /// Preset ID
    pub preset_id: Uuid,
    /// Preset name
    pub preset_name: String,
    /// Snapshot ID (if any)
    pub snapshot_id: Option<Uuid>,
    /// Snapshot name (if any)
    pub snapshot_name: Option<String>,
}

/// Simplified setlist information for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct SetlistInfo {
    /// Setlist ID
    pub id: Uuid,
    /// Setlist name
    pub name: String,
    /// Number of songs
    pub song_count: usize,
}

// endregion: --- RPC Types

// region:    --- Commands

/// Commands that can be executed on the rig service.
#[repr(u8)]
#[derive(Debug, Clone, Facet)]
pub enum RigCommand {
    // Profile/Rig
    /// Load a profile by ID
    LoadProfile { profile_id: Uuid },
    /// Load a rig by ID
    LoadRig { rig_id: Uuid },

    // Preset loading
    /// Load a preset by ID
    LoadPreset { preset_id: Uuid },
    /// Load a preset with a specific snapshot
    LoadPresetWithSnapshot { preset_id: Uuid, snapshot_id: Uuid },
    /// Load a preset by category (with fallback)
    LoadPresetByCategory { category: PresetCategory },

    // Section control
    /// Enable/disable a section
    SetSectionEnabled { section_id: Uuid, enabled: bool },
    /// Set section volume
    SetSectionVolume { section_id: Uuid, volume: f64 },
    /// Set section routing
    SetSectionRouting { section_id: Uuid, routing: SectionRouting },

    // Layer control
    /// Set the patch for a layer
    SetLayerPatch {
        section_id: Uuid,
        layer_index: u8,
        patch_id: Uuid,
    },
    /// Set the patch variation for a layer
    SetLayerPatchVariation {
        section_id: Uuid,
        layer_index: u8,
        variation_id: Uuid,
    },
    /// Enable/disable a layer
    SetLayerEnabled {
        section_id: Uuid,
        layer_index: u8,
        enabled: bool,
    },
    /// Set layer volume
    SetLayerVolume {
        section_id: Uuid,
        layer_index: u8,
        volume: f64,
    },
    /// Set layer send to a global block
    SetLayerGlobalSend {
        section_id: Uuid,
        layer_index: u8,
        global_block_id: Uuid,
        level: f64,
    },

    // Block control
    /// Bypass/enable a block
    SetBlockBypassed { block_id: Uuid, bypassed: bool },
    /// Set the order of global blocks
    SetGlobalBlockOrder { block_ids: Vec<Uuid> },

    // Parameters
    /// Set a parameter value
    SetParameter { block_id: Uuid, param_index: u32, value: f64 },

    // Scene navigation
    /// Go to a specific scene
    GoToScene { scene_index: usize },
    /// Go to the next scene
    NextScene,
    /// Go to the previous scene
    PreviousScene,

    // Song navigation
    /// Go to a specific song
    GoToSong { song_index: usize },
    /// Go to the next song
    NextSong,
    /// Go to the previous song
    PreviousSong,

    // Setlist
    /// Load a setlist by ID
    LoadSetlist { setlist_id: Uuid },

    // Preloading
    /// Preload a preset
    PreloadPreset { preset_id: Uuid },
    /// Preload all presets for a song
    PreloadSong { song_index: usize },
}

// endregion: --- Commands

// region:    --- Events

/// Events emitted by the rig service.
#[repr(u8)]
#[derive(Debug, Clone, Facet)]
pub enum RigEvent {
    /// A profile was loaded
    ProfileLoaded { profile: ProfileInfo },
    /// A rig was loaded
    RigLoaded { rig: RigInfo },
    /// A preset was loaded
    PresetLoaded { preset: PresetInfo },
    /// A snapshot was activated
    SnapshotActivated { preset_id: Uuid, snapshot_id: Uuid },
    /// A section changed (enabled, volume, routing)
    SectionChanged { section_id: Uuid },
    /// A layer changed (patch, volume, etc.)
    LayerChanged { section_id: Uuid, layer_index: u8 },
    /// A block changed (bypass, parameters)
    BlockChanged { block_id: Uuid },
    /// A parameter changed
    ParameterChanged {
        block_id: Uuid,
        param_index: u32,
        value: f64,
    },
    /// The current scene changed
    SceneChanged { song_index: usize, scene_index: usize },
    /// The current song changed
    SongChanged { song_index: usize },
    /// A setlist was loaded
    SetlistLoaded { setlist: SetlistInfo },
    /// A transition started
    TransitionStarted { from_preset: Uuid, to_preset: Uuid },
    /// A transition completed
    TransitionCompleted,
    /// A preset was preloaded
    PreloadCompleted { preset_id: Uuid },
}

// endregion: --- Events

// region:    --- Service Trait

/// RigService provides RPC access to rig operations.
///
/// This trait defines the contract for rig management services.
/// Implementations can be for REAPER, mock testing, or standalone apps.
#[roam::service]
pub trait RigService {
    // Profile/Rig queries
    /// Get all available profiles
    async fn get_available_profiles(&self) -> Vec<ProfileInfo>;
    /// Get the currently loaded profile
    async fn get_current_profile(&self) -> Option<ProfileInfo>;
    /// Get the currently loaded rig
    async fn get_current_rig(&self) -> Option<RigInfo>;
    /// Get all sections in the current rig
    async fn get_sections(&self) -> Vec<SectionInfo>;
    /// Get all global blocks in the current rig
    async fn get_global_blocks(&self) -> Vec<GlobalBlockInfo>;

    // Preset queries
    /// Get all available presets in the current profile
    async fn get_available_presets(&self) -> Vec<PresetInfo>;
    /// Get the currently loaded preset
    async fn get_current_preset(&self) -> Option<PresetInfo>;
    /// Get presets by category (uses fallback resolution)
    async fn get_presets_by_category(&self, category: PresetCategory) -> Vec<PresetInfo>;
    /// Get patches available for a section
    async fn get_patches(&self, section_id: Uuid) -> Vec<PatchInfo>;

    // Setlist queries
    /// Get all available setlists
    async fn get_available_setlists(&self) -> Vec<SetlistInfo>;
    /// Get the current setlist
    async fn get_current_setlist(&self) -> Option<SetlistInfo>;

    // Scene/Song queries
    /// Get all songs in the current setlist
    async fn get_setlist_songs(&self) -> Vec<SongInfo>;
    /// Get the current song
    async fn get_current_song(&self) -> Option<SongInfo>;
    /// Get the current scene
    async fn get_current_scene(&self) -> Option<SceneInfo>;

    // Commands
    /// Execute a rig command
    async fn execute(&self, cmd: RigCommand);

    // Subscriptions
    /// Subscribe to all rig events
    async fn subscribe(&self, events: Tx<RigEvent>);
    /// Subscribe to parameter changes for specific blocks
    async fn subscribe_parameters(
        &self,
        block_ids: Vec<Uuid>,
        values: Tx<(Uuid, u32, f64)>,
    );
}

// endregion: --- Service Trait

// region:    --- Type Conversions

impl ProfileInfo {
    /// Convert from domain Profile to RPC ProfileInfo
    pub fn from_profile(p: &Profile) -> Self {
        let scene_count = p.scene_templates.len();
        let scene_names: Vec<String> = p.scene_templates.iter()
            .map(|st| st.name.clone())
            .collect();

        let scenes: Vec<SceneInfo> = p.scene_templates.iter()
            .enumerate()
            .map(|(index, st)| {
                // Extract preset ID from the preset reference
                let preset_id = match &st.preset_reference {
                    super::core::PresetReference::Direct { preset_id } => *preset_id,
                    _ => Uuid::nil(), // For other reference types, use placeholder
                };

                SceneInfo {
                    index,
                    name: st.name.clone(),
                    preset_id,
                    preset_name: String::new(), // Will be resolved from preset registry
                    snapshot_id: st.default_snapshot_id,
                    snapshot_name: None, // Will be resolved from preset
                }
            })
            .collect();

        Self {
            id: p.id,
            name: p.name.clone(),
            rig_id: p.rig_id,
            preset_count: p.presets.len(),
            scene_count,
            scene_names,
            scenes,
        }
    }
}

impl RigInfo {
    /// Convert from domain Rig to RPC RigInfo
    pub fn from_rig(r: &Rig) -> Self {
        Self {
            id: r.id,
            name: r.name.clone(),
            instrument_type: r.instrument_type.to_string(),
            section_count: r.sections.len(),
            global_block_count: r.global_blocks.len(),
        }
    }
}

impl SectionInfo {
    /// Convert from domain Section to RPC SectionInfo
    pub fn from_section(s: &super::core::Section) -> Self {
        let routing = match &s.routing {
            SectionRouting::Serial => "Serial",
            SectionRouting::Parallel => "Parallel",
            SectionRouting::Custom(_) => "Custom",
        };
        Self {
            id: s.id,
            name: s.name.clone(),
            layer_count: s.layers.len(),
            enabled: s.enabled,
            volume: s.volume,
            routing: routing.to_string(),
        }
    }
}

impl GlobalBlockInfo {
    /// Convert from domain GlobalBlock to RPC GlobalBlockInfo
    pub fn from_global_block(gb: &GlobalBlock) -> Self {
        Self {
            id: gb.block.id,
            name: gb.block.name.clone(),
            plugin_name: gb.block.plugin_id.display_name.clone(),
            order: gb.order,
            bypassed: gb.block.bypassed,
            enabled: !gb.block.bypassed,
        }
    }
}

impl PresetInfo {
    /// Convert from domain Preset to RPC PresetInfo
    pub fn from_preset(p: &Preset) -> Self {
        Self {
            id: p.id,
            name: p.name.clone(),
            category: p.category.display_name(),
            // Only include manual tags (display_tags), not auto-derived
            tag_ids: p.tags.display_tags().copied().collect(),
            rating: p.rating,
            snapshot_count: p.snapshots.len(),
            snapshot_names: p.snapshots.iter().map(|s| s.name.clone()).collect(),
            snapshots: p.snapshots.iter().map(SnapshotInfo::from_snapshot).collect(),
            blocks: p.blocks.iter().map(PresetBlockInfo::from_preset_block).collect(),
        }
    }
}

impl SnapshotInfo {
    /// Convert from domain Snapshot to RPC SnapshotInfo
    pub fn from_snapshot(s: &super::core::Snapshot) -> Self {
        Self {
            id: s.id,
            name: s.name.clone(),
            // Only include manual tags (display_tags), not auto-derived
            tag_ids: s.tags.display_tags().copied().collect(),
            block_overrides: s.block_overrides.iter().map(BlockOverrideInfo::from_block_override).collect(),
        }
    }
}

impl PresetBlockInfo {
    /// Convert from domain PresetBlock to RPC PresetBlockInfo
    pub fn from_preset_block(b: &super::core::PresetBlock) -> Self {
        Self {
            id: b.id,
            name: b.name.clone(),
            block_type: b.block_type.to_string(),
            order: b.order,
            bypassed: b.bypassed,
            parameters: b.default_parameters.iter().map(BlockParameterInfo::from_block_parameter).collect(),
        }
    }
}

impl BlockParameterInfo {
    /// Convert from domain BlockParameter to RPC BlockParameterInfo
    pub fn from_block_parameter(p: &super::core::BlockParameter) -> Self {
        Self {
            id: p.id.clone(),
            name: p.name.clone(),
            value: p.value,
            min_display: p.min_display,
            max_display: p.max_display,
            unit: p.unit.clone(),
        }
    }
}

impl BlockOverrideInfo {
    /// Convert from domain BlockOverride to RPC BlockOverrideInfo
    pub fn from_block_override(o: &super::core::BlockOverride) -> Self {
        Self {
            block_id: o.block_id,
            bypassed: o.bypassed,
            parameters: o.parameters.iter().map(|p| ParameterOverrideInfo {
                param_id: p.param_id.clone(),
                value: p.value,
            }).collect(),
        }
    }
}

impl PatchInfo {
    /// Convert from domain Patch to RPC PatchInfo
    pub fn from_patch(p: &super::core::Patch) -> Self {
        Self {
            id: p.id,
            name: p.name.clone(),
            category_base: p.category.base.clone(),
            category_style: p.category.style.clone(),
            variation_count: p.variations.len(),
        }
    }
}

impl SongInfo {
    /// Convert from domain PerformanceSong to RPC SongInfo
    pub fn from_song(index: usize, s: &PerformanceSong, current_scene_index: usize) -> Self {
        Self {
            index,
            name: s.name.clone(),
            scene_count: s.scenes.len(),
            current_scene_index,
            scene_names: s.scenes.iter().map(|scene| scene.name.clone()).collect(),
        }
    }
}

impl SceneInfo {
    /// Convert from domain Scene to RPC SceneInfo
    pub fn from_scene(index: usize, s: &Scene, preset_name: String, snapshot_name: Option<String>) -> Self {
        Self {
            index,
            name: s.name.clone(),
            preset_id: s.preset_id,
            preset_name,
            snapshot_id: s.snapshot_id,
            snapshot_name,
        }
    }
}

impl SetlistInfo {
    /// Convert from domain PerformanceSetlist to RPC SetlistInfo
    pub fn from_setlist(s: &PerformanceSetlist) -> Self {
        Self {
            id: s.id,
            name: s.name.clone(),
            song_count: s.songs.len(),
        }
    }
}

// endregion: --- Type Conversions

// region:    --- Local Client

crate::define_local_client! {
    /// A local client for in-process RigService calls.
    ///
    /// This wraps any `RigService` implementation and provides a convenient
    /// interface for calling service methods without needing a transport layer.
    client: LocalRigClient,
    service: RigService,
    methods: {
        /// Get all available profiles
        async fn get_available_profiles() -> Vec<ProfileInfo>;

        /// Get the currently loaded profile
        async fn get_current_profile() -> Option<ProfileInfo>;

        /// Get the currently loaded rig
        async fn get_current_rig() -> Option<RigInfo>;

        /// Get all sections in the current rig
        async fn get_sections() -> Vec<SectionInfo>;

        /// Get all global blocks in the current rig
        async fn get_global_blocks() -> Vec<GlobalBlockInfo>;

        /// Get all available presets in the current profile
        async fn get_available_presets() -> Vec<PresetInfo>;

        /// Get the currently loaded preset
        async fn get_current_preset() -> Option<PresetInfo>;

        /// Get presets by category (uses fallback resolution)
        async fn get_presets_by_category(category: PresetCategory) -> Vec<PresetInfo>;

        /// Get patches available for a section
        async fn get_patches(section_id: Uuid) -> Vec<PatchInfo>;

        /// Get all available setlists
        async fn get_available_setlists() -> Vec<SetlistInfo>;

        /// Get the current setlist
        async fn get_current_setlist() -> Option<SetlistInfo>;

        /// Get all songs in the current setlist
        async fn get_setlist_songs() -> Vec<SongInfo>;

        /// Get the current song
        async fn get_current_song() -> Option<SongInfo>;

        /// Get the current scene
        async fn get_current_scene() -> Option<SceneInfo>;

        /// Execute a rig command
        async fn execute(cmd: RigCommand) -> ();

        /// Subscribe to all rig events
        async fn subscribe(events: Tx<RigEvent>) -> ();

        /// Subscribe to parameter changes for specific blocks
        async fn subscribe_parameters(block_ids: Vec<Uuid>, values: Tx<(Uuid, u32, f64)>) -> ();
    }
}

// endregion: --- Local Client
