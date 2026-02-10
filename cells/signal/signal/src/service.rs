//! RigControlService — ROAM service for the gapless module engine.
//!
//! This service provides RPC access to engine state, module slot management,
//! preset/scene transitions, and preloading. Commands are executed via the
//! `execute` method; events are streamed via `subscribe` / `subscribe_slots`.
//!
//! Note: This module uses simplified RPC types that can be easily serialized.
//! Service implementations convert between these and the full domain types.

use facet::Facet;
use roam::{Context, Tx};
use uuid::Uuid;

use crate::engine::{
    EngineError, InstanceHandle, InstanceState, PreloadPriority, PresetLoadHandle, PresetReadiness,
    SwitchOutcome,
};
use crate::module::ModuleType;

// region:    --- RPC Types

/// Engine-wide state information for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct EngineStateInfo {
    /// Whether the engine has been initialized.
    pub initialized: bool,
    /// All slot states, one per active module type.
    #[facet(default)]
    pub slots: Vec<SlotStateInfo>,
}

/// Per-slot state information for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct SlotStateInfo {
    /// Which processing stage this slot represents.
    pub module_type: ModuleType,
    /// Whether this slot is disabled (muted).
    pub disabled: bool,
    /// The currently active instance handle (if any).
    #[facet(default)]
    pub active_instance: Option<u64>,
    /// All loaded instances and their states.
    #[facet(default)]
    pub instances: Vec<InstanceInfo>,
}

/// Per-instance state information for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct InstanceInfo {
    /// Instance handle (raw u64).
    pub handle: u64,
    /// Current lifecycle state.
    pub state: InstanceState,
}

/// Result of a preset/scene transition for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct TransitionResultInfo {
    /// Overall outcome.
    pub outcome: SwitchOutcomeInfo,
    /// Per-slot errors (non-fatal). Empty if clean.
    #[facet(default)]
    pub slot_errors: Vec<SlotErrorInfo>,
}

/// Switch outcome for RPC communication.
#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Facet)]
pub enum SwitchOutcomeInfo {
    /// Switch completed immediately.
    Completed = 0,
    /// Switch is deferred — some slots need to load first.
    Pending {
        handle: u64,
        loaded: u16,
        total: u16,
    } = 1,
    /// Switch failed.
    Failed { reason: String } = 2,
}

/// Per-slot error during a transition.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct SlotErrorInfo {
    /// Module type that had the error.
    pub module_type: ModuleType,
    /// Error description.
    pub error: String,
}

/// Readiness status for a preload operation.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct PreloadStatusInfo {
    /// The preload handle (raw u64).
    pub handle: u64,
    /// Current readiness.
    pub readiness: PresetReadiness,
}

/// Simplified profile information for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct ProfileInfo {
    /// Profile ID
    pub id: Uuid,
    /// Profile name
    pub name: String,
    /// Associated rig ID
    pub rig_id: Uuid,
    /// Number of scene templates in this profile
    pub scene_count: usize,
    /// Scene names for quick display
    #[facet(default)]
    pub scene_names: Vec<String>,
    /// Detailed scene info
    #[facet(default)]
    pub scenes: Vec<ProfileSceneInfo>,
    /// Optional description
    #[facet(default)]
    pub description: Option<String>,
}

/// Simplified profile scene information for RPC communication.
///
/// A ProfileScene is a template/scene within a Profile that references
/// a specific Preset and optionally a specific PresetSnapshot (variation).
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct ProfileSceneInfo {
    /// Scene index within profile/song
    pub index: usize,
    /// Scene name
    pub name: String,
    /// Preset ID this scene references
    pub preset_id: Uuid,
    /// Preset name (resolved)
    pub preset_name: String,
    /// Preset scene ID (which variation of the preset to use, if any)
    pub preset_snapshot_id: Option<Uuid>,
    /// Preset scene name (if any)
    pub preset_snapshot_name: Option<String>,
}

/// Simplified preset information for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct PresetInfo {
    /// Preset ID
    pub id: Uuid,
    /// Preset name
    pub name: String,
    /// Category
    pub category: String,
    /// Star rating (0-5)
    pub rating: u8,
    /// Optional description
    #[facet(default)]
    pub description: Option<String>,
    /// Number of scenes (preset-level variations)
    pub scene_count: usize,
    /// Scene names for quick display
    pub scene_names: Vec<String>,
    /// Scene details (preset-level variations that can load different modules)
    #[facet(default)]
    pub scenes: Vec<PresetSnapshotInfo>,
    // TEMP: Commented out to test serialization
    // /// Default scene index to load when preset is activated (None means scene 0)
    // #[facet(default)]
    // pub default_scene_index: Option<usize>,
}

/// Simplified preset scene information for RPC communication.
///
/// Preset scenes allow switching between different module configurations
/// within a preset. The routing/order is locked, but the content of modules
/// can change between scenes.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct PresetSnapshotInfo {
    /// Scene ID (internally this is a SnapshotId in the domain model)
    pub id: Uuid,
    /// Scene name (e.g., "Verse", "Chorus", "Solo")
    pub name: String,
}

/// Simplified song information for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct SongInfo {
    /// Song index in setlist
    pub index: usize,
    /// Song name
    pub name: String,
    /// Artist name
    pub artist: Option<String>,
    /// Number of scenes
    pub scene_count: usize,
    /// Scene names for quick display
    pub scene_names: Vec<String>,
    /// Current scene index (if this is the active song)
    #[facet(default)]
    pub current_scene_index: Option<usize>,
}

/// Simplified setlist information for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct SetlistInfo {
    /// Setlist name
    pub name: String,
    /// Number of songs
    pub song_count: usize,
    /// Song names
    pub song_names: Vec<String>,
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
}

// endregion: --- RPC Types

// region:    --- Commands

/// Commands that can be executed on the rig control engine.
#[repr(u8)]
#[derive(Debug, Clone, Facet)]
pub enum RigControlCommand {
    // ── Lifecycle ────────────────────────────────────────────────────────
    /// Initialize the engine with a rig's module types.
    Initialize { rig_id: Uuid },
    /// Shut down the engine, releasing all resources.
    Shutdown,

    // ── Transitions ─────────────────────────────────────────────────────
    /// Load a preset with a specific scene (primary entry point).
    LoadPresetWithScene { preset_id: Uuid, scene_index: usize },
    /// Load a song scene by song and scene index.
    ///
    /// Resolves the song's scene (including song-level module overrides)
    /// and loads the resulting preset+scene into the engine.
    LoadSongScene {
        song_index: usize,
        scene_index: usize,
    },
    /// Switch scene within the current preset.
    SwitchScene { scene_index: usize },
    /// Apply a module snapshot (parameter changes only).
    ApplySnapshot {
        module_type: ModuleType,
        snapshot_id: Uuid,
    },

    // ── Preloading ──────────────────────────────────────────────────────
    /// Preload a preset+scene in the background.
    Preload {
        preset_id: Uuid,
        scene_index: usize,
        priority: PreloadPriority,
    },

    // ── Slot Control ────────────────────────────────────────────────────
    /// Disable a specific module slot (mute all output).
    DisableSlot { module_type: ModuleType },
    /// Re-enable a previously disabled module slot.
    EnableSlot { module_type: ModuleType },

    // ── Profile/Preset Management ───────────────────────────────────────
    /// Load a specific profile
    LoadProfile { profile_id: Uuid },
    /// Load a specific preset (uses scene index 0)
    LoadPreset { preset_id: Uuid },
    /// Go to next song in setlist
    NextSong,
    /// Go to previous song in setlist
    PreviousSong,
    /// Go to next scene in current song
    NextScene,
    /// Go to previous scene in current song
    PreviousScene,

    // ── Tick ─────────────────────────────────────────────────────────────
    /// Process one engine tick (preload queue, tail cleanup).
    Tick,
}

// endregion: --- Commands

// region:    --- Events

/// Events emitted by the rig control engine.
#[repr(u8)]
#[derive(Debug, Clone, Facet)]
pub enum RigControlEvent {
    // ── Lifecycle ────────────────────────────────────────────────────────
    /// Engine was initialized with a rig.
    EngineInitialized { rig_id: Uuid },
    /// Engine was shut down.
    EngineShutdown,

    // ── Transitions ─────────────────────────────────────────────────────
    /// A preset/scene transition started.
    TransitionStarted { preset_id: Uuid, scene_index: usize },
    /// A transition completed (all slots activated).
    TransitionCompleted { result: TransitionResultInfo },
    /// A transition failed.
    TransitionFailed { reason: String },

    // ── Slot State ──────────────────────────────────────────────────────
    /// A module slot's state changed (instance loaded, activated, etc.).
    SlotStateChanged {
        module_type: ModuleType,
        slot_state: SlotStateInfo,
    },
    /// A module slot was disabled.
    SlotDisabled { module_type: ModuleType },
    /// A module slot was re-enabled.
    SlotEnabled { module_type: ModuleType },

    // ── Preloading ──────────────────────────────────────────────────────
    /// A preload operation started.
    PreloadStarted {
        handle: u64,
        preset_id: Uuid,
        scene_index: usize,
    },
    /// Preload progress changed.
    PreloadProgress {
        handle: u64,
        loaded: u16,
        total: u16,
    },
    /// A preload operation completed (all slots ready).
    PreloadCompleted { handle: u64 },
    /// A preload operation failed.
    PreloadFailed { handle: u64, reason: String },

    // ── Profile/Preset/Song Events ──────────────────────────────────────
    /// A profile was loaded
    ProfileLoaded { profile: ProfileInfo },
    /// A preset was loaded
    PresetLoaded {
        preset: PresetInfo,
        scene_index: usize,
    },
    /// A song changed
    SongChanged { song_index: usize },
    /// Current scene index changed
    SceneIndexChanged { scene_index: usize },

    // ── Tail Cleanup ────────────────────────────────────────────────────
    /// A tailing instance was cleaned up.
    TailCleanedUp { module_type: ModuleType },
}

// endregion: --- Events

// region:    --- Service Trait

/// RigControlService provides RPC access to the gapless module engine.
///
/// This trait defines the contract for engine management.
/// Implementations can be for REAPER, mock testing, or standalone apps.
///
/// # Observability
///
/// Use `RigControlDispatcher::with_middleware(TelemetryMiddleware::new(exporter))`
/// for automatic OpenTelemetry span creation on every call. Client-side tracing
/// is available via `TracingCaller::new(handle, exporter)`.
///
/// # Middleware
///
/// ```ignore
/// let dispatcher = RigControlDispatcher::new(handler)
///     .with_middleware(AuthMiddleware::new())
///     .with_middleware(TelemetryMiddleware::new(exporter));
/// ```
#[roam::service]
pub trait RigControlService {
    // ── Engine State Queries ────────────────────────────────────────────

    /// Get the full engine state (initialized?, all slot states).
    async fn get_engine_state(&self) -> EngineStateInfo;

    /// Get a single slot's state.
    async fn get_slot_state(&self, module_type: ModuleType) -> Option<SlotStateInfo>;

    /// Get all slot states.
    async fn get_all_slot_states(&self) -> Vec<SlotStateInfo>;

    /// Check readiness of a preload operation.
    async fn check_preload_status(&self, handle: u64) -> Option<PreloadStatusInfo>;

    // ── Profile/Preset/Song Queries ─────────────────────────────────────

    /// Get all available profiles
    async fn get_available_profiles(&self) -> Vec<ProfileInfo>;
    /// Get the currently loaded profile
    async fn get_current_profile(&self) -> Option<ProfileInfo>;
    /// Get all available presets
    async fn get_available_presets(&self) -> Vec<PresetInfo>;
    /// Get the currently loaded preset
    async fn get_current_preset(&self) -> Option<PresetInfo>;
    /// Get the current rig info
    async fn get_current_rig(&self) -> Option<RigInfo>;
    /// Get all available setlists
    async fn get_available_setlists(&self) -> Vec<SetlistInfo>;
    /// Get the current setlist
    async fn get_current_setlist(&self) -> Option<SetlistInfo>;
    /// Get all songs in the current setlist
    async fn get_setlist_songs(&self) -> Vec<SongInfo>;
    /// Get the current song
    async fn get_current_song(&self) -> Option<SongInfo>;
    /// Get the current scene (in performance/song mode)
    async fn get_current_scene(&self) -> Option<ProfileSceneInfo>;

    // ── Commands ────────────────────────────────────────────────────────

    /// Execute an engine command.
    async fn execute(&self, cmd: RigControlCommand);

    // ── Subscriptions ───────────────────────────────────────────────────

    /// Subscribe to all engine events.
    async fn subscribe(&self, events: Tx<RigControlEvent>);

    /// Subscribe to state changes for specific module slots.
    async fn subscribe_slots(&self, module_types: Vec<ModuleType>, states: Tx<SlotStateInfo>);
}

// endregion: --- Service Trait

// region:    --- Type Conversions

impl EngineStateInfo {
    /// Create an uninitialized engine state.
    pub fn uninitialized() -> Self {
        Self {
            initialized: false,
            slots: Vec::new(),
        }
    }

    /// Create an initialized engine state with slot info.
    pub fn initialized(slots: Vec<SlotStateInfo>) -> Self {
        Self {
            initialized: true,
            slots,
        }
    }
}

impl SlotStateInfo {
    /// Build slot state info from engine domain types.
    pub fn new(
        module_type: ModuleType,
        disabled: bool,
        active_instance: Option<InstanceHandle>,
        instances: Vec<(InstanceHandle, InstanceState)>,
    ) -> Self {
        Self {
            module_type,
            disabled,
            active_instance: active_instance.map(|h| h.raw()),
            instances: instances
                .into_iter()
                .map(|(handle, state)| InstanceInfo {
                    handle: handle.raw(),
                    state,
                })
                .collect(),
        }
    }
}

impl TransitionResultInfo {
    /// Convert from a domain `TransitionResult`.
    pub fn from_transition(result: &crate::rig_engine::TransitionResult) -> Self {
        Self {
            outcome: SwitchOutcomeInfo::from_outcome(&result.outcome),
            slot_errors: result
                .slot_errors
                .iter()
                .map(|(mt, err)| SlotErrorInfo {
                    module_type: *mt,
                    error: err.to_string(),
                })
                .collect(),
        }
    }

    /// Create a successful, clean result.
    pub fn completed() -> Self {
        Self {
            outcome: SwitchOutcomeInfo::Completed,
            slot_errors: Vec::new(),
        }
    }
}

impl SwitchOutcomeInfo {
    /// Convert from a domain `SwitchOutcome`.
    pub fn from_outcome(outcome: &SwitchOutcome) -> Self {
        match outcome {
            SwitchOutcome::Completed => Self::Completed,
            SwitchOutcome::Pending { handle, readiness } => {
                let (loaded, total) = match readiness {
                    PresetReadiness::Loading { loaded, total } => (*loaded, *total),
                    PresetReadiness::Ready => (1, 1),
                    PresetReadiness::Failed => (0, 0),
                };
                Self::Pending {
                    handle: handle.raw(),
                    loaded,
                    total,
                }
            }
            SwitchOutcome::Failed { reason } => Self::Failed {
                reason: reason.clone(),
            },
        }
    }
}

impl SlotErrorInfo {
    /// Create from engine error.
    pub fn from_engine_error(module_type: ModuleType, error: &EngineError) -> Self {
        Self {
            module_type,
            error: error.to_string(),
        }
    }
}

impl PreloadStatusInfo {
    /// Create from domain types.
    pub fn new(handle: PresetLoadHandle, readiness: PresetReadiness) -> Self {
        Self {
            handle: handle.raw(),
            readiness,
        }
    }
}

impl ProfileInfo {
    /// Convert from domain Profile to RPC ProfileInfo
    pub fn from_profile(p: &crate::profile::Profile, presets: &[crate::preset::Preset]) -> Self {
        let scene_count = p.scene_templates.len();
        let scene_names: Vec<String> = p.scene_templates.iter().map(|st| st.name.clone()).collect();

        let scenes: Vec<ProfileSceneInfo> = p
            .scene_templates
            .iter()
            .enumerate()
            .map(|(index, st)| {
                // Extract preset ID from the preset reference
                let preset_id = match &st.preset_reference {
                    crate::profile::PresetReference::Direct { preset_id } => preset_id.as_uuid(),
                    _ => Uuid::nil(), // For other reference types, use placeholder
                };

                // Resolve preset name from presets list
                let preset_name = presets
                    .iter()
                    .find(|preset| preset.id.as_uuid() == preset_id)
                    .map(|p| p.name.clone())
                    .unwrap_or_default();

                ProfileSceneInfo {
                    index,
                    name: st.name.clone(),
                    preset_id,
                    preset_name,
                    preset_snapshot_id: st.default_snapshot_id.map(|id| id.as_uuid()),
                    preset_snapshot_name: None,
                }
            })
            .collect();

        Self {
            id: p.id.as_uuid(),
            name: p.name.clone(),
            rig_id: p.rig_id.as_uuid(),
            scene_count,
            scene_names,
            scenes,
            description: p.description.clone(),
        }
    }
}

impl PresetInfo {
    /// Convert from domain Preset to RPC PresetInfo
    pub fn from_preset(p: &crate::preset::Preset) -> Self {
        // Note: In the domain model, these are called "snapshots" but at the preset level
        // they represent "scenes" (different module configurations within the same routing)
        let scenes: Vec<PresetSnapshotInfo> = p
            .snapshots
            .iter()
            .map(|s| PresetSnapshotInfo {
                id: s.id.as_uuid(),
                name: s.name.clone(),
            })
            .collect();

        let scene_names: Vec<String> = scenes.iter().map(|s| s.name.clone()).collect();

        // TEMP: Find default scene index by looking up the default_snapshot_id
        // let default_scene_index = p.default_snapshot_id
        //     .and_then(|default_id| {
        //         p.snapshots.iter()
        //             .position(|s| s.id == default_id)
        //     });

        Self {
            id: p.id.as_uuid(),
            name: p.name.clone(),
            category: format!("{:?}", p.category),
            rating: p.rating.get(),
            description: p.description.clone(),
            scene_count: scenes.len(),
            scene_names,
            scenes,
            // TEMP: default_scene_index,
        }
    }
}

impl SongInfo {
    /// Convert from domain PerformanceSong to RPC SongInfo
    pub fn from_song(
        index: usize,
        song: &crate::performance::PerformanceSong,
        current_song_index: Option<usize>,
        current_scene_index: Option<usize>,
    ) -> Self {
        let scene_count = song.scenes.len();
        let scene_names: Vec<String> = song.scenes.iter().map(|s| s.name.clone()).collect();

        let current_scene = if current_song_index == Some(index) {
            current_scene_index
        } else {
            None
        };

        Self {
            index,
            name: song.name.clone(),
            artist: song.artist.clone(),
            scene_count,
            scene_names,
            current_scene_index: current_scene,
        }
    }
}

impl SetlistInfo {
    /// Convert from a list of songs to RPC SetlistInfo
    pub fn from_songs(
        name: impl Into<String>,
        songs: &[crate::performance::PerformanceSong],
    ) -> Self {
        let song_names: Vec<String> = songs.iter().map(|s| s.name.clone()).collect();

        Self {
            name: name.into(),
            song_count: songs.len(),
            song_names,
        }
    }
}

impl RigInfo {
    /// Convert from domain Rig to RPC RigInfo
    pub fn from_rig(r: &crate::rig::Rig) -> Self {
        Self {
            id: r.id.as_uuid(),
            name: r.name.clone(),
            instrument_type: format!("{:?}", r.instrument_type),
        }
    }
}

// endregion: --- Type Conversions

// region:    --- Mock Implementation

use std::sync::{Arc, RwLock};

use crate::mock::MockRigEngine;
use crate::performance::{PerformanceSong, Scene};
use crate::preset::Preset;
use crate::rig::Rig;
use crate::rig_engine::RigEngine;

/// Data context for a `MockRigControlService`.
///
/// Holds the rig, profiles, presets, and songs so the mock service can resolve
/// commands into the domain types the engine needs.
pub struct RigControlData {
    pub rig: Rig,
    pub profiles: Vec<crate::profile::Profile>,
    pub presets: Vec<Preset>,
    pub songs: Vec<PerformanceSong>,
}

impl RigControlData {
    /// Find a preset by its ID.
    pub fn find_preset(&self, id: Uuid) -> Option<&Preset> {
        self.presets.iter().find(|p| p.id.as_uuid() == id)
    }

    /// Find a song by index.
    pub fn song(&self, index: usize) -> Option<&PerformanceSong> {
        self.songs.get(index)
    }

    /// Find a song by name.
    pub fn song_by_name(&self, name: &str) -> Option<(usize, &PerformanceSong)> {
        self.songs.iter().enumerate().find(|(_, s)| s.name == name)
    }
}

/// Mock implementation of [`RigControlService`] for testing.
///
/// Wraps a [`MockRigEngine`] and [`RigControlData`] to provide a
/// fully functional service that resolves commands against the guitar
/// rig data model.
pub struct MockRigControlService {
    engine: MockRigEngine,
    data: RigControlData,
    initialized: RwLock<bool>,
    current_profile_index: RwLock<usize>,
    current_preset_index: RwLock<usize>,
    current_song_index: RwLock<usize>,
    current_scene_index: RwLock<usize>,
    event_subscriber: RwLock<Option<Arc<Tx<RigControlEvent>>>>,
}

impl MockRigControlService {
    /// Create a new mock service with the given data.
    pub fn new(data: RigControlData) -> Self {
        Self {
            engine: MockRigEngine::new(),
            data,
            initialized: RwLock::new(false),
            current_profile_index: RwLock::new(0),
            current_preset_index: RwLock::new(0),
            current_song_index: RwLock::new(0),
            current_scene_index: RwLock::new(0),
            event_subscriber: RwLock::new(None),
        }
    }

    /// Create a new mock service with default guitar rig data.
    pub fn with_guitar_defaults() -> Self {
        let defaults = crate::defaults::guitar::build_guitar_rig();
        let data = RigControlData {
            rig: defaults.rig,
            profiles: defaults.profiles,
            presets: defaults.presets,
            songs: defaults.songs,
        };
        Self::new(data)
    }

    /// Get a reference to the underlying rig data.
    pub fn data(&self) -> &RigControlData {
        &self.data
    }

    /// Build the current preset's modules for UI display.
    ///
    /// Iterates the current preset's `module_assignments`, looks up each
    /// `ModulePreset` in the rig, and constructs `Module` instances with
    /// blocks and macros copied from the preset.
    pub fn build_current_modules(&self) -> Vec<crate::module::Module> {
        let preset_index = *self.current_preset_index.read().unwrap();
        let Some(preset) = self.data.presets.get(preset_index) else {
            return Vec::new();
        };
        let rig = &self.data.rig;

        let mut modules = Vec::new();
        for assignment in &preset.module_assignments {
            if !assignment.enabled {
                continue;
            }

            if let Some(mp) = rig.get_module_preset(assignment.module_preset_id) {
                let mut module = crate::module::Module::new(&mp.name, mp.module_type);
                for block in &mp.blocks {
                    module.add_block(block.clone());
                }
                for m in &mp.macros {
                    module.add_macro(m.clone());
                }
                modules.push(module);
            }
        }

        modules
    }

    /// Build slot state info from the engine's internal state for a module type.
    fn build_slot_state(&self, module_type: ModuleType) -> Option<SlotStateInfo> {
        let slot = self.engine.slot(module_type)?;
        Some(SlotStateInfo::new(
            module_type,
            slot.is_disabled(),
            slot.active_instance(),
            slot.loaded_instances(),
        ))
    }

    /// Broadcast an event to the subscriber (non-blocking)
    fn broadcast_event(&self, event: RigControlEvent) {
        // Clone the Arc<Tx> if present
        let tx_opt = self.event_subscriber.read().unwrap().clone();
        if let Some(tx) = tx_opt {
            // Send asynchronously in a background task
            #[cfg(not(target_arch = "wasm32"))]
            {
                tokio::spawn(async move {
                    let _ = tx.send(&event).await;
                });
            }
            #[cfg(target_arch = "wasm32")]
            {
                wasm_bindgen_futures::spawn_local(async move {
                    let _ = tx.send(&event).await;
                });
            }
        }
    }

    /// Resolve a scene, applying song-level overrides when relevant.
    fn resolve_scene_for_load(
        &self,
        _preset: &Preset,
        scene: &Scene,
        song_overrides: &[crate::module_preset::ModuleOverride],
    ) -> Scene {
        // Merge song-level module overrides into the scene's overrides
        if song_overrides.is_empty() {
            return scene.clone();
        }

        let mut merged_scene = scene.clone();
        for song_override in song_overrides {
            // Only add song override if the scene doesn't already override this module type
            let already_overridden = merged_scene
                .module_overrides
                .iter()
                .any(|o| o.module_type == song_override.module_type);
            if !already_overridden {
                merged_scene.module_overrides.push(song_override.clone());
            }
        }
        merged_scene
    }
}

impl RigControlService for MockRigControlService {
    async fn get_engine_state(&self, _cx: &Context) -> EngineStateInfo {
        let initialized = *self.initialized.read().unwrap();
        if !initialized {
            return EngineStateInfo::uninitialized();
        }

        let slots = self.get_all_slot_states(_cx).await;
        EngineStateInfo::initialized(slots)
    }

    async fn get_slot_state(
        &self,
        _cx: &Context,
        module_type: ModuleType,
    ) -> Option<SlotStateInfo> {
        // MockRigEngine::slot() currently returns None — we need to read
        // the internal state directly. Use engine's slot states helper.
        let states = self.engine.current_slot_states_public();
        states.into_iter().find_map(|ss| {
            if ss.module_type == module_type {
                Some(SlotStateInfo {
                    module_type: ss.module_type,
                    disabled: ss.is_disabled,
                    active_instance: ss.active_handle.map(|h| h.raw()),
                    instances: Vec::new(), // Simplified for mock
                })
            } else {
                None
            }
        })
    }

    async fn get_all_slot_states(&self, _cx: &Context) -> Vec<SlotStateInfo> {
        let states = self.engine.current_slot_states_public();
        states
            .into_iter()
            .map(|ss| SlotStateInfo {
                module_type: ss.module_type,
                disabled: ss.is_disabled,
                active_instance: ss.active_handle.map(|h| h.raw()),
                instances: Vec::new(), // Simplified for mock
            })
            .collect()
    }

    async fn check_preload_status(&self, _cx: &Context, handle: u64) -> Option<PreloadStatusInfo> {
        let plh = PresetLoadHandle::from_raw(handle);
        let readiness = self.engine.check_readiness(plh);
        Some(PreloadStatusInfo::new(plh, readiness))
    }

    async fn get_available_profiles(&self, _cx: &Context) -> Vec<ProfileInfo> {
        self.data
            .profiles
            .iter()
            .map(|p| ProfileInfo::from_profile(p, &self.data.presets))
            .collect()
    }

    async fn get_current_profile(&self, _cx: &Context) -> Option<ProfileInfo> {
        let index = *self.current_profile_index.read().unwrap();
        self.data
            .profiles
            .get(index)
            .map(|p| ProfileInfo::from_profile(p, &self.data.presets))
    }

    async fn get_available_presets(&self, _cx: &Context) -> Vec<PresetInfo> {
        self.data
            .presets
            .iter()
            .map(PresetInfo::from_preset)
            .collect()
    }

    async fn get_current_preset(&self, _cx: &Context) -> Option<PresetInfo> {
        let index = *self.current_preset_index.read().unwrap();
        self.data.presets.get(index).map(PresetInfo::from_preset)
    }

    async fn get_current_rig(&self, _cx: &Context) -> Option<RigInfo> {
        Some(RigInfo::from_rig(&self.data.rig))
    }

    async fn get_available_setlists(&self, _cx: &Context) -> Vec<SetlistInfo> {
        // For now, just return one setlist with all songs
        vec![SetlistInfo::from_songs("Main Setlist", &self.data.songs)]
    }

    async fn get_current_setlist(&self, _cx: &Context) -> Option<SetlistInfo> {
        Some(SetlistInfo::from_songs("Main Setlist", &self.data.songs))
    }

    async fn get_setlist_songs(&self, _cx: &Context) -> Vec<SongInfo> {
        let current_song = *self.current_song_index.read().unwrap();
        let current_scene = *self.current_scene_index.read().unwrap();

        self.data
            .songs
            .iter()
            .enumerate()
            .map(|(i, song)| SongInfo::from_song(i, song, Some(current_song), Some(current_scene)))
            .collect()
    }

    async fn get_current_song(&self, _cx: &Context) -> Option<SongInfo> {
        let current_song = *self.current_song_index.read().unwrap();
        let current_scene = *self.current_scene_index.read().unwrap();

        self.data.songs.get(current_song).map(|song| {
            SongInfo::from_song(current_song, song, Some(current_song), Some(current_scene))
        })
    }

    async fn get_current_scene(&self, _cx: &Context) -> Option<ProfileSceneInfo> {
        let song_index = *self.current_song_index.read().unwrap();
        let scene_index = *self.current_scene_index.read().unwrap();

        let song = self.data.songs.get(song_index)?;
        let scene = song.scenes.get(scene_index)?;

        let preset = self.data.presets.iter().find(|p| p.id == scene.preset_id)?;

        Some(ProfileSceneInfo {
            index: scene_index,
            name: scene.name.clone(),
            preset_id: preset.id.as_uuid(),
            preset_name: preset.name.clone(),
            preset_snapshot_id: None,
            preset_snapshot_name: None,
        })
    }

    async fn execute(&self, _cx: &Context, cmd: RigControlCommand) {
        match cmd {
            RigControlCommand::Initialize { rig_id: _ } => {
                let _ = self.engine.initialize(&self.data.rig).await;
                *self.initialized.write().unwrap() = true;
            }
            RigControlCommand::Shutdown => {
                self.engine.shutdown().await;
                *self.initialized.write().unwrap() = false;
            }
            RigControlCommand::LoadPresetWithScene {
                preset_id,
                scene_index,
            } => {
                // Look up preset
                let Some(preset) = self.data.find_preset(preset_id) else {
                    return;
                };

                // Find the scene from any song that references this preset at this index
                // or construct a default scene
                let (scene, song_overrides) = self.find_scene_for_preset(preset_id, scene_index);

                let merged = self.resolve_scene_for_load(preset, &scene, &song_overrides);
                let _ = self
                    .engine
                    .load_preset_with_scene(preset, &merged, &self.data.rig)
                    .await;

                // Update current preset index
                if let Some(index) = self
                    .data
                    .presets
                    .iter()
                    .position(|p| p.id.as_uuid() == preset_id)
                {
                    *self.current_preset_index.write().unwrap() = index;
                }

                // Broadcast preset loaded event
                let preset_info = PresetInfo::from_preset(preset);
                self.broadcast_event(RigControlEvent::PresetLoaded {
                    preset: preset_info,
                    scene_index,
                });
            }
            RigControlCommand::LoadSongScene {
                song_index,
                scene_index,
            } => {
                let Some(song) = self.data.songs.get(song_index) else {
                    return;
                };
                let Some(scene) = song.scenes.get(scene_index) else {
                    return;
                };
                let Some(preset) = self.data.find_preset(scene.preset_id.as_uuid()) else {
                    return;
                };

                let merged = self.resolve_scene_for_load(preset, scene, &song.module_overrides);
                let _ = self
                    .engine
                    .load_preset_with_scene(preset, &merged, &self.data.rig)
                    .await;

                // Update current indices
                *self.current_song_index.write().unwrap() = song_index;
                *self.current_scene_index.write().unwrap() = scene_index;

                // Broadcast events
                self.broadcast_event(RigControlEvent::SongChanged { song_index });
                self.broadcast_event(RigControlEvent::SceneIndexChanged { scene_index });

                // Also broadcast preset loaded (scenes in songs use scene 0 as the base)
                let preset_info = PresetInfo::from_preset(preset);
                self.broadcast_event(RigControlEvent::PresetLoaded {
                    preset: preset_info,
                    scene_index: 0,
                });
            }
            RigControlCommand::SwitchScene { scene_index: _ } => {
                // Need to know current preset — simplified: search resolved state
                // In a real impl this would track current state
            }
            RigControlCommand::ApplySnapshot {
                module_type,
                snapshot_id,
            } => {
                // Find the snapshot across all module presets
                let snap = self
                    .data
                    .rig
                    .module_presets
                    .iter()
                    .flat_map(|p| p.snapshots.iter())
                    .find(|s| s.id.as_uuid() == snapshot_id);
                if let Some(snapshot) = snap {
                    let _ = self.engine.apply_snapshot(module_type, snapshot).await;
                }
            }
            RigControlCommand::Preload {
                preset_id,
                scene_index: _,
                priority: _,
            } => {
                if let Some(preset) = self.data.find_preset(preset_id) {
                    let scene = Scene::new("Preload", preset.id);
                    let _ = self.engine.preload(preset, &scene, &self.data.rig).await;
                }
            }
            RigControlCommand::DisableSlot { module_type } => {
                self.engine.disable_slot(module_type).await;
            }
            RigControlCommand::EnableSlot { module_type } => {
                self.engine.enable_slot(module_type).await;
            }
            RigControlCommand::LoadProfile { profile_id } => {
                if let Some(index) = self
                    .data
                    .profiles
                    .iter()
                    .position(|p| p.id.as_uuid() == profile_id)
                {
                    *self.current_profile_index.write().unwrap() = index;

                    // Get profile and broadcast event
                    if let Some(profile) = self.data.profiles.get(index) {
                        let profile_info = ProfileInfo::from_profile(profile, &self.data.presets);
                        self.broadcast_event(RigControlEvent::ProfileLoaded {
                            profile: profile_info,
                        });
                    }
                }
            }
            RigControlCommand::LoadPreset { preset_id } => {
                if let Some(index) = self
                    .data
                    .presets
                    .iter()
                    .position(|p| p.id.as_uuid() == preset_id)
                {
                    *self.current_preset_index.write().unwrap() = index;
                    // Note: In rig-control, presets don't have scenes - they're loaded via songs
                    // This command just tracks which preset is "current" for UI purposes
                }
            }
            RigControlCommand::NextSong => {
                let mut song_index = self.current_song_index.write().unwrap();
                *song_index = (*song_index + 1).min(self.data.songs.len().saturating_sub(1));
                *self.current_scene_index.write().unwrap() = 0;
                let index = *song_index;
                drop(song_index);

                // Broadcast events
                self.broadcast_event(RigControlEvent::SongChanged { song_index: index });
                self.broadcast_event(RigControlEvent::SceneIndexChanged { scene_index: 0 });
            }
            RigControlCommand::PreviousSong => {
                let mut song_index = self.current_song_index.write().unwrap();
                *song_index = song_index.saturating_sub(1);
                *self.current_scene_index.write().unwrap() = 0;
                let index = *song_index;
                drop(song_index);

                // Broadcast events
                self.broadcast_event(RigControlEvent::SongChanged { song_index: index });
                self.broadcast_event(RigControlEvent::SceneIndexChanged { scene_index: 0 });
            }
            RigControlCommand::NextScene => {
                let mut scene_index = self.current_scene_index.write().unwrap();
                let song_index = *self.current_song_index.read().unwrap();
                if let Some(song) = self.data.songs.get(song_index) {
                    *scene_index = (*scene_index + 1).min(song.scenes.len().saturating_sub(1));
                }
                let index = *scene_index;
                drop(scene_index);

                // Broadcast event
                self.broadcast_event(RigControlEvent::SceneIndexChanged { scene_index: index });
            }
            RigControlCommand::PreviousScene => {
                let mut scene_index = self.current_scene_index.write().unwrap();
                *scene_index = scene_index.saturating_sub(1);
                let index = *scene_index;
                drop(scene_index);

                // Broadcast event
                self.broadcast_event(RigControlEvent::SceneIndexChanged { scene_index: index });
            }
            RigControlCommand::Tick => {
                self.engine.tick().await;
            }
        }
    }

    async fn subscribe(&self, _cx: &Context, events: Tx<RigControlEvent>) {
        // Store the event subscriber (replace any previous subscriber)
        *self.event_subscriber.write().unwrap() = Some(Arc::new(events));
    }

    async fn subscribe_slots(
        &self,
        _cx: &Context,
        _module_types: Vec<ModuleType>,
        _states: Tx<SlotStateInfo>,
    ) {
        // Mock: no slot event broadcasting yet
    }
}

impl MockRigControlService {
    /// Find the scene and song-level overrides for a given preset_id + scene_index.
    ///
    /// Searches all songs for scenes that reference this preset. The `scene_index`
    /// is interpreted as the index among scenes in the first matching song that
    /// uses this preset, or across all songs if needed.
    fn find_scene_for_preset(
        &self,
        preset_id: Uuid,
        scene_index: usize,
    ) -> (Scene, Vec<crate::module_preset::ModuleOverride>) {
        // Find scenes across all songs that reference this preset
        for song in &self.data.songs {
            if let Some(scene) = song.scenes.get(scene_index) {
                if scene.preset_id.as_uuid() == preset_id {
                    return (scene.clone(), song.module_overrides.clone());
                }
            }
        }

        // Fallback: construct a default scene for the preset
        (
            Scene::new("Default", crate::id::PresetId::from_uuid(preset_id)),
            Vec::new(),
        )
    }

    /// Load a specific song scene by song index and scene index.
    ///
    /// This is a convenience for tests that think in terms of songs and
    /// scene positions rather than raw preset IDs.
    pub async fn load_song_scene(
        &self,
        song_index: usize,
        scene_index: usize,
    ) -> Option<TransitionResultInfo> {
        let song = self.data.songs.get(song_index)?;
        let scene = song.scenes.get(scene_index)?;
        let preset = self.data.find_preset(scene.preset_id.as_uuid())?;

        let merged = self.resolve_scene_for_load(preset, scene, &song.module_overrides);
        let result = self
            .engine
            .load_preset_with_scene(preset, &merged, &self.data.rig)
            .await;

        Some(TransitionResultInfo::from_transition(&result))
    }

    /// Get the current resolved module targets (for test assertions).
    pub fn current_resolved_modules(
        &self,
    ) -> Vec<(ModuleType, Option<crate::engine::ModuleTarget>)> {
        let states = self.engine.current_slot_states_public();
        states
            .into_iter()
            .map(|ss| {
                (
                    ss.module_type,
                    ss.current.and_then(|rs| rs.target().cloned()),
                )
            })
            .collect()
    }
}

// endregion: --- Mock Implementation
