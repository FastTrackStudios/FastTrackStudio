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
    EngineError, InstanceHandle, InstanceState, PreloadPriority, PresetLoadHandle,
    PresetReadiness, SwitchOutcome,
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
    LoadPresetWithScene {
        preset_id: Uuid,
        scene_index: usize,
    },
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
    TransitionStarted {
        preset_id: Uuid,
        scene_index: usize,
    },
    /// A transition completed (all slots activated).
    TransitionCompleted {
        result: TransitionResultInfo,
    },
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
    PreloadFailed {
        handle: u64,
        reason: String,
    },

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

    // ── Commands ────────────────────────────────────────────────────────

    /// Execute an engine command.
    async fn execute(&self, cmd: RigControlCommand);

    // ── Subscriptions ───────────────────────────────────────────────────

    /// Subscribe to all engine events.
    async fn subscribe(&self, events: Tx<RigControlEvent>);

    /// Subscribe to state changes for specific module slots.
    async fn subscribe_slots(
        &self,
        module_types: Vec<ModuleType>,
        states: Tx<SlotStateInfo>,
    );
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

// endregion: --- Type Conversions

// region:    --- Local Client

crate::define_local_client! {
    /// A local client for in-process RigControlService calls.
    ///
    /// For remote calls: `RigControlServiceClient::new(connection_handle)`
    /// For local calls: `LocalRigControlClient::new(Arc::new(mock_impl))`
    client: LocalRigControlClient,
    service: RigControlService,
    methods: {
        /// Get the full engine state
        async fn get_engine_state() -> EngineStateInfo;

        /// Get a single slot's state
        async fn get_slot_state(module_type: ModuleType) -> Option<SlotStateInfo>;

        /// Get all slot states
        async fn get_all_slot_states() -> Vec<SlotStateInfo>;

        /// Check readiness of a preload operation
        async fn check_preload_status(handle: u64) -> Option<PreloadStatusInfo>;

        /// Execute an engine command
        async fn execute(cmd: RigControlCommand) -> ();

        /// Subscribe to all engine events
        async fn subscribe(events: Tx<RigControlEvent>) -> ();

        /// Subscribe to state changes for specific module slots
        async fn subscribe_slots(module_types: Vec<ModuleType>, states: Tx<SlotStateInfo>) -> ();
    }
}

// endregion: --- Local Client

// region:    --- Mock Implementation

use std::sync::RwLock;

use crate::mock::MockRigEngine;
use crate::performance::{PerformanceSong, Scene};
use crate::preset::Preset;
use crate::rig::Rig;
use crate::rig_engine::RigEngine;

/// Data context for a `MockRigControlService`.
///
/// Holds the rig, presets, and songs so the mock service can resolve
/// commands into the domain types the engine needs.
pub struct RigControlData {
    pub rig: Rig,
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
        self.songs
            .iter()
            .enumerate()
            .find(|(_, s)| s.name == name)
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
}

impl MockRigControlService {
    /// Create a new mock service with the given data.
    pub fn new(data: RigControlData) -> Self {
        Self {
            engine: MockRigEngine::new(),
            data,
            initialized: RwLock::new(false),
        }
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

    /// Resolve a scene, applying song-level overrides when relevant.
    fn resolve_scene_for_load(
        &self,
        preset: &Preset,
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
                merged_scene
                    .module_overrides
                    .push(song_override.clone());
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

    async fn get_slot_state(&self, _cx: &Context, module_type: ModuleType) -> Option<SlotStateInfo> {
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
                    let _ = self
                        .engine
                        .apply_snapshot(module_type, snapshot)
                        .await;
                }
            }
            RigControlCommand::Preload {
                preset_id,
                scene_index: _,
                priority: _,
            } => {
                if let Some(preset) = self.data.find_preset(preset_id) {
                    let scene = Scene::new("Preload", preset.id);
                    let _ = self
                        .engine
                        .preload(preset, &scene, &self.data.rig)
                        .await;
                }
            }
            RigControlCommand::DisableSlot { module_type } => {
                self.engine.disable_slot(module_type).await;
            }
            RigControlCommand::EnableSlot { module_type } => {
                self.engine.enable_slot(module_type).await;
            }
            RigControlCommand::Tick => {
                self.engine.tick().await;
            }
        }
    }

    async fn subscribe(&self, _cx: &Context, _events: Tx<RigControlEvent>) {
        // Mock: no event broadcasting yet
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
            Scene::new(
                "Default",
                crate::id::PresetId::from_uuid(preset_id),
            ),
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
            .map(|ss| (ss.module_type, ss.current.and_then(|rs| rs.target().cloned())))
            .collect()
    }
}

// endregion: --- Mock Implementation
