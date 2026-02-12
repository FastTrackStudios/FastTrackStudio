//! RigControlService — ROAM service for the gapless module engine.
//!
//! This service provides RPC access to engine state, module slot management,
//! scene transitions, and preloading. Commands are executed via the
//! `execute` method; events are streamed via `subscribe` / `subscribe_slots`.
//!
//! Note: This module uses simplified RPC types that can be easily serialized.
//! Service implementations convert between these and the full domain types.

use facet::Facet;
use roam::{Context, Tx};

use crate::category::PresetCategory;
use crate::engine::{
    EngineError, InstanceHandle, InstanceState, PreloadPriority, PresetLoadHandle, PresetReadiness,
    SwitchOutcome,
};
use crate::id::{ModuleSnapshotId, PatchId, ProfileId, RigId, RigPresetId, SongId};
use crate::module::ModuleType;
use crate::normalized::Rating;
use crate::rig::InstrumentType;

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
    pub active_instance: Option<InstanceHandle>,
    /// All loaded instances and their states.
    #[facet(default)]
    pub instances: Vec<InstanceInfo>,
}

/// Per-instance state information for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct InstanceInfo {
    /// Instance handle.
    pub handle: InstanceHandle,
    /// Current lifecycle state.
    pub state: InstanceState,
}

/// Result of a scene transition for RPC communication.
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
        handle: PresetLoadHandle,
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
    /// The preload handle.
    pub handle: PresetLoadHandle,
    /// Current readiness.
    pub readiness: PresetReadiness,
}

/// Simplified profile information for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct ProfileInfo {
    /// Profile ID
    pub id: ProfileId,
    /// Profile name
    pub name: String,
    /// Number of patches in this profile
    pub patch_count: usize,
    /// Patch details
    #[facet(default)]
    pub patches: Vec<PatchInfo>,
}

/// Simplified patch information for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct PatchInfo {
    /// Patch ID
    pub id: PatchId,
    /// Patch name
    pub name: String,
    /// Index within the profile
    pub index: usize,
}

/// Simplified rig preset information for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct RigPresetInfo {
    /// Preset ID
    pub id: RigPresetId,
    /// Preset name
    pub name: String,
    /// Category
    pub category: PresetCategory,
    /// Star rating
    pub rating: Rating,
}

/// Simplified song information for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct SongInfo {
    /// Song ID
    pub id: SongId,
    /// Song index in setlist
    pub index: usize,
    /// Song name
    pub name: String,
    /// Artist name
    pub artist: Option<String>,
    /// Number of sections
    pub section_count: usize,
    /// Section names for quick display
    pub section_names: Vec<String>,
    /// Current section index (if this is the active song)
    #[facet(default)]
    pub current_section_index: Option<usize>,
}

/// Simplified section information for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct SectionInfo {
    /// Section index
    pub index: usize,
    /// Section name
    pub name: String,
    /// Whether this section has overrides
    pub has_overrides: bool,
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
    pub id: RigId,
    /// Rig name
    pub name: String,
    /// Instrument type
    pub instrument_type: InstrumentType,
    /// Number of engines
    pub engine_count: usize,
}

// endregion: --- RPC Types

// region:    --- Commands

/// Commands that can be executed on the rig control engine.
#[repr(u8)]
#[derive(Debug, Clone, Facet)]
pub enum RigControlCommand {
    // ── Lifecycle ────────────────────────────────────────────────────────
    /// Initialize the engine with a rig's module types.
    Initialize { rig_id: RigId },
    /// Shut down the engine, releasing all resources.
    Shutdown,

    // ── Transitions ─────────────────────────────────────────────────────
    /// Load a patch from a profile.
    LoadPatch {
        profile_id: ProfileId,
        patch_index: usize,
    },
    /// Load a song section by song and section index.
    LoadSongSection {
        song_index: usize,
        section_index: usize,
    },
    /// Apply a module snapshot (parameter changes only).
    ApplySnapshot {
        module_type: ModuleType,
        snapshot_id: ModuleSnapshotId,
    },

    // ── Preloading ──────────────────────────────────────────────────────
    /// Preload a song section in the background.
    PreloadSection {
        song_index: usize,
        section_index: usize,
        priority: PreloadPriority,
    },

    // ── Slot Control ────────────────────────────────────────────────────
    /// Disable a specific module slot (mute all output).
    DisableSlot { module_type: ModuleType },
    /// Re-enable a previously disabled module slot.
    EnableSlot { module_type: ModuleType },

    // ── Profile/Song Management ─────────────────────────────────────────
    /// Load a specific profile
    LoadProfile { profile_id: ProfileId },
    /// Go to next song in setlist
    NextSong,
    /// Go to previous song in setlist
    PreviousSong,
    /// Go to next section in current song
    NextSection,
    /// Go to previous section in current song
    PreviousSection,

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
    EngineInitialized { rig_id: RigId },
    /// Engine was shut down.
    EngineShutdown,

    // ── Transitions ─────────────────────────────────────────────────────
    /// A scene transition started.
    TransitionStarted {
        song_index: Option<usize>,
        section_index: usize,
    },
    /// A transition completed (all slots activated).
    TransitionCompleted { result: TransitionResultInfo },
    /// A transition failed.
    TransitionFailed { reason: String },

    // ── Slot State ──────────────────────────────────────────────────────
    /// A module slot's state changed.
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
    PreloadStarted { handle: u64 },
    /// A preload operation completed.
    PreloadCompleted { handle: u64 },

    // ── Profile/Song Events ─────────────────────────────────────────────
    /// A profile was loaded
    ProfileLoaded { profile: ProfileInfo },
    /// A song changed
    SongChanged { song_index: usize },
    /// Current section index changed
    SectionChanged { section_index: usize },
    /// A patch was loaded
    PatchLoaded { patch: PatchInfo },

    // ── Tail Cleanup ────────────────────────────────────────────────────
    /// A tailing instance was cleaned up.
    TailCleanedUp { module_type: ModuleType },
}

// endregion: --- Events

// region:    --- Service Trait

/// RigControlService provides RPC access to the gapless module engine.
#[roam::service]
pub trait RigControlService {
    // ── Engine State Queries ────────────────────────────────────────────

    /// Get the full engine state.
    async fn get_engine_state(&self) -> EngineStateInfo;

    /// Get a single slot's state.
    async fn get_slot_state(&self, module_type: ModuleType) -> Option<SlotStateInfo>;

    /// Get all slot states.
    async fn get_all_slot_states(&self) -> Vec<SlotStateInfo>;

    /// Check readiness of a preload operation.
    async fn check_preload_status(&self, handle: u64) -> Option<PreloadStatusInfo>;

    // ── Profile/Song Queries ────────────────────────────────────────────

    /// Get all available profiles
    async fn get_available_profiles(&self) -> Vec<ProfileInfo>;
    /// Get the currently loaded profile
    async fn get_current_profile(&self) -> Option<ProfileInfo>;
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
    /// Get the current section
    async fn get_current_section(&self) -> Option<SectionInfo>;

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
    pub fn uninitialized() -> Self {
        Self {
            initialized: false,
            slots: Vec::new(),
        }
    }

    pub fn initialized(slots: Vec<SlotStateInfo>) -> Self {
        Self {
            initialized: true,
            slots,
        }
    }
}

impl SlotStateInfo {
    pub fn new(
        module_type: ModuleType,
        disabled: bool,
        active_instance: Option<InstanceHandle>,
        instances: Vec<(InstanceHandle, InstanceState)>,
    ) -> Self {
        Self {
            module_type,
            disabled,
            active_instance,
            instances: instances
                .into_iter()
                .map(|(handle, state)| InstanceInfo { handle, state })
                .collect(),
        }
    }
}

impl TransitionResultInfo {
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

    pub fn completed() -> Self {
        Self {
            outcome: SwitchOutcomeInfo::Completed,
            slot_errors: Vec::new(),
        }
    }
}

impl SwitchOutcomeInfo {
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
                    handle: *handle,
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
    pub fn from_engine_error(module_type: ModuleType, error: &EngineError) -> Self {
        Self {
            module_type,
            error: error.to_string(),
        }
    }
}

impl PreloadStatusInfo {
    pub fn new(handle: PresetLoadHandle, readiness: PresetReadiness) -> Self {
        Self { handle, readiness }
    }
}

impl ProfileInfo {
    pub fn from_profile(p: &crate::profile::Profile) -> Self {
        let patches: Vec<PatchInfo> = p
            .patches
            .iter()
            .enumerate()
            .map(|(i, patch)| PatchInfo {
                id: patch.id,
                name: patch.name.clone(),
                index: i,
            })
            .collect();

        Self {
            id: p.id,
            name: p.name.clone(),
            patch_count: patches.len(),
            patches,
        }
    }
}

impl SongInfo {
    pub fn from_song(
        index: usize,
        song: &crate::song::Song,
        current_song_index: Option<usize>,
        current_section_index: Option<usize>,
    ) -> Self {
        let section_count = song.sections.len();
        let section_names: Vec<String> = song.sections.iter().map(|s| s.name.clone()).collect();

        let current_section = if current_song_index == Some(index) {
            current_section_index
        } else {
            None
        };

        Self {
            id: song.id,
            index,
            name: song.name.clone(),
            artist: song.artist.clone(),
            section_count,
            section_names,
            current_section_index: current_section,
        }
    }
}

impl SetlistInfo {
    pub fn from_songs(name: impl Into<String>, songs: &[crate::song::Song]) -> Self {
        let song_names: Vec<String> = songs.iter().map(|s| s.name.clone()).collect();
        Self {
            name: name.into(),
            song_count: songs.len(),
            song_names,
        }
    }
}

impl RigInfo {
    pub fn from_rig(r: &crate::rig::Rig) -> Self {
        Self {
            id: r.id,
            name: r.name.clone(),
            instrument_type: r.instrument_type.clone(),
            engine_count: r.engine_count(),
        }
    }
}

// endregion: --- Type Conversions

// region:    --- Mock Implementation

use std::sync::{Arc, RwLock};

use crate::mock::MockRigEngine;
use crate::profile::Profile;
use crate::rig::Rig;
use crate::rig_engine::RigEngine;
use crate::song::Song;
use crate::stores::{InMemorySceneStore, SceneStore};

/// Data context for a `MockRigControlService`.
///
/// Holds the rig, scene store, profiles, and songs.
pub struct RigControlData {
    pub rig: Rig,
    pub store: InMemorySceneStore,
    pub profiles: Vec<Profile>,
    pub songs: Vec<Song>,
}

impl RigControlData {
    /// Find a song by index.
    pub fn song(&self, index: usize) -> Option<&Song> {
        self.songs.get(index)
    }
}

/// Mock implementation of [`RigControlService`] for testing.
///
/// Wraps a [`MockRigEngine`] and [`RigControlData`] to provide a
/// fully functional service that resolves commands against the
/// rig data model.
pub struct MockRigControlService {
    engine: MockRigEngine,
    data: RigControlData,
    initialized: RwLock<bool>,
    current_profile_index: RwLock<usize>,
    current_song_index: RwLock<usize>,
    current_section_index: RwLock<usize>,
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
            current_song_index: RwLock::new(0),
            current_section_index: RwLock::new(0),
            event_subscriber: RwLock::new(None),
        }
    }

    /// Create a new mock service with minimal inline test data.
    ///
    /// Builds a simple guitar rig with Drive + Amp modules, a single
    /// profile with one patch, and one song with two sections.
    /// The full defaults module is deferred to Phase 12.
    pub fn with_guitar_defaults() -> Self {
        use crate::category::PresetCategory;
        use crate::module::{Module, ModuleType};
        use crate::preset::{ModulePreset, PresetMetadata};
        use crate::scene::{
            EngineSceneBuilder, EngineSceneEntry, LayerSceneBuilder, LayerSceneEntry,
            RigSceneBuilder, ScopedSceneRef,
        };
        use crate::snapshot::ModuleSnapshot;
        use crate::version::{LayerIndex, VersionedRef};
        use signal_proto::engine::Engine as ProtoEngine;
        use signal_proto::layer::Layer;
        use signal_proto::rig::InstrumentType;

        let mut store = InMemorySceneStore::new();

        // 1. Register module presets
        let drive_snapshot = ModuleSnapshot::new("Blues Stack", vec![]);
        let drive_snap_id = drive_snapshot.id;
        let drive_preset = ModulePreset::new(
            PresetMetadata::new("Klon", PresetCategory::default()),
            ModuleType::Drive,
            drive_snapshot,
        );
        store.register_module_preset(drive_preset);

        let amp_snapshot = ModuleSnapshot::new("Clean Deluxe", vec![]);
        let amp_snap_id = amp_snapshot.id;
        let amp_preset = ModulePreset::new(
            PresetMetadata::new("Deluxe Reverb", PresetCategory::default()),
            ModuleType::Amp,
            amp_snapshot,
        );
        store.register_module_preset(amp_preset);

        // 2. Build physical rig
        let mut layer = Layer::new("Main", LayerIndex::new(1));
        layer.add_module(Module::new("Drive", ModuleType::Drive));
        layer.add_module(Module::new("Amp", ModuleType::Amp));
        let engine = ProtoEngine::new("Guitar", InstrumentType::Guitar, layer);
        let engine_id = engine.id;
        let rig = Rig::new("Guitar Rig", InstrumentType::Guitar, engine);

        // 3. Build scenes (layer → engine → rig)
        let layer_scene_verse = LayerSceneBuilder::new("Clean Verse")
            .modules(vec![
                VersionedRef::new(drive_snap_id, 1),
                VersionedRef::new(amp_snap_id, 1),
            ])
            .no_standalone_blocks()
            .build();
        let layer_scene_verse_id = layer_scene_verse.id;
        store.register_layer_scene(layer_scene_verse);

        let layer_scene_chorus = LayerSceneBuilder::new("Drive Chorus")
            .modules(vec![
                VersionedRef::new(drive_snap_id, 1),
                VersionedRef::new(amp_snap_id, 1),
            ])
            .no_standalone_blocks()
            .build();
        let layer_scene_chorus_id = layer_scene_chorus.id;
        store.register_layer_scene(layer_scene_chorus);

        let engine_scene_verse = EngineSceneBuilder::new("Verse Engine")
            .layers(vec![LayerSceneEntry {
                layer_index: LayerIndex::new(1),
                scene_ref: VersionedRef::new(layer_scene_verse_id, 1),
            }])
            .build();
        let engine_scene_verse_id = engine_scene_verse.id;
        store.register_engine_scene(engine_scene_verse);

        let engine_scene_chorus = EngineSceneBuilder::new("Chorus Engine")
            .layers(vec![LayerSceneEntry {
                layer_index: LayerIndex::new(1),
                scene_ref: VersionedRef::new(layer_scene_chorus_id, 1),
            }])
            .build();
        let engine_scene_chorus_id = engine_scene_chorus.id;
        store.register_engine_scene(engine_scene_chorus);

        let rig_scene_verse = RigSceneBuilder::new("Verse Rig")
            .engines(vec![EngineSceneEntry {
                engine_id,
                scene_ref: VersionedRef::new(engine_scene_verse_id, 1),
            }])
            .build();
        let rig_scene_verse_id = rig_scene_verse.id;
        store.register_rig_scene(rig_scene_verse);

        let rig_scene_chorus = RigSceneBuilder::new("Chorus Rig")
            .engines(vec![EngineSceneEntry {
                engine_id,
                scene_ref: VersionedRef::new(engine_scene_chorus_id, 1),
            }])
            .build();
        let rig_scene_chorus_id = rig_scene_chorus.id;
        store.register_rig_scene(rig_scene_chorus);

        // 4. Build profile with patches
        let mut profile = Profile::new("Live Show");
        profile.add_patch(crate::profile::Patch::new(
            "Clean Verse",
            ScopedSceneRef::Rig(VersionedRef::new(rig_scene_verse_id, 1)),
        ));
        profile.add_patch(crate::profile::Patch::new(
            "Drive Chorus",
            ScopedSceneRef::Rig(VersionedRef::new(rig_scene_chorus_id, 1)),
        ));

        // 5. Build song with sections
        let mut song = Song::new("Amazing Grace").with_artist("Traditional");
        song.add_section(crate::song::SongSection::new(
            "Verse",
            ScopedSceneRef::Rig(VersionedRef::new(rig_scene_verse_id, 1)),
        ));
        song.add_section(crate::song::SongSection::new(
            "Chorus",
            ScopedSceneRef::Rig(VersionedRef::new(rig_scene_chorus_id, 1)),
        ));

        let data = RigControlData {
            rig,
            store,
            profiles: vec![profile],
            songs: vec![song],
        };

        Self::new(data)
    }

    /// Get a reference to the underlying rig data.
    pub fn data(&self) -> &RigControlData {
        &self.data
    }

    /// Build the current rig's modules for UI display.
    ///
    /// Walks the physical hierarchy: rig → first engine → first layer → modules.
    pub fn build_current_modules(&self) -> Vec<crate::module::Module> {
        let rig = &self.data.rig;
        let engine = rig.engines.first();
        let layer = engine.layers.first();
        layer.modules.clone()
    }

    /// Broadcast an event to the subscriber (non-blocking)
    fn broadcast_event(&self, event: RigControlEvent) {
        let tx_opt = self.event_subscriber.read().unwrap().clone();
        if let Some(tx) = tx_opt {
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
        let states = self.engine.current_slot_states_public();
        states.into_iter().find_map(|ss| {
            if ss.module_type == module_type {
                Some(SlotStateInfo {
                    module_type: ss.module_type,
                    disabled: ss.is_disabled,
                    active_instance: ss.active_handle,
                    instances: Vec::new(),
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
                active_instance: ss.active_handle,
                instances: Vec::new(),
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
            .map(ProfileInfo::from_profile)
            .collect()
    }

    async fn get_current_profile(&self, _cx: &Context) -> Option<ProfileInfo> {
        let index = *self.current_profile_index.read().unwrap();
        self.data.profiles.get(index).map(ProfileInfo::from_profile)
    }

    async fn get_current_rig(&self, _cx: &Context) -> Option<RigInfo> {
        Some(RigInfo::from_rig(&self.data.rig))
    }

    async fn get_available_setlists(&self, _cx: &Context) -> Vec<SetlistInfo> {
        vec![SetlistInfo::from_songs("Main Setlist", &self.data.songs)]
    }

    async fn get_current_setlist(&self, _cx: &Context) -> Option<SetlistInfo> {
        Some(SetlistInfo::from_songs("Main Setlist", &self.data.songs))
    }

    async fn get_setlist_songs(&self, _cx: &Context) -> Vec<SongInfo> {
        let current_song = *self.current_song_index.read().unwrap();
        let current_section = *self.current_section_index.read().unwrap();

        self.data
            .songs
            .iter()
            .enumerate()
            .map(|(i, song)| {
                SongInfo::from_song(i, song, Some(current_song), Some(current_section))
            })
            .collect()
    }

    async fn get_current_song(&self, _cx: &Context) -> Option<SongInfo> {
        let current_song = *self.current_song_index.read().unwrap();
        let current_section = *self.current_section_index.read().unwrap();

        self.data.songs.get(current_song).map(|song| {
            SongInfo::from_song(
                current_song,
                song,
                Some(current_song),
                Some(current_section),
            )
        })
    }

    async fn get_current_section(&self, _cx: &Context) -> Option<SectionInfo> {
        let song_index = *self.current_song_index.read().unwrap();
        let section_index = *self.current_section_index.read().unwrap();

        let song = self.data.songs.get(song_index)?;
        let section = song.sections.get(section_index)?;

        Some(SectionInfo {
            index: section_index,
            name: section.name.clone(),
            has_overrides: !section.overrides.is_empty(),
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
            RigControlCommand::LoadPatch {
                profile_id,
                patch_index,
            } => {
                let profile = self.data.profiles.iter().find(|p| p.id == profile_id);
                let Some(profile) = profile else { return };
                let Some(patch) = profile.patches.get(patch_index) else {
                    return;
                };

                let _ = self
                    .engine
                    .load_scene(&patch.scene_ref, &self.data.rig, &self.data.store, &[])
                    .await;

                self.broadcast_event(RigControlEvent::PatchLoaded {
                    patch: PatchInfo {
                        id: patch.id,
                        name: patch.name.clone(),
                        index: patch_index,
                    },
                });
            }
            RigControlCommand::LoadSongSection {
                song_index,
                section_index,
            } => {
                let Some(song) = self.data.songs.get(song_index) else {
                    return;
                };
                let Some(section) = song.sections.get(section_index) else {
                    return;
                };

                let _ = self
                    .engine
                    .load_scene(
                        &section.scene_ref,
                        &self.data.rig,
                        &self.data.store,
                        &section.overrides,
                    )
                    .await;

                *self.current_song_index.write().unwrap() = song_index;
                *self.current_section_index.write().unwrap() = section_index;

                self.broadcast_event(RigControlEvent::SongChanged { song_index });
                self.broadcast_event(RigControlEvent::SectionChanged { section_index });
            }
            RigControlCommand::ApplySnapshot {
                module_type,
                snapshot_id,
            } => {
                let snap = self.data.store.module_snapshot(&snapshot_id);
                if let Some(snapshot) = snap {
                    let _ = self.engine.apply_snapshot(module_type, snapshot).await;
                }
            }
            RigControlCommand::PreloadSection {
                song_index,
                section_index,
                priority: _,
            } => {
                if let Some(song) = self.data.songs.get(song_index) {
                    if let Some(section) = song.sections.get(section_index) {
                        let _ = self
                            .engine
                            .preload(&section.scene_ref, &self.data.rig, &self.data.store)
                            .await;
                    }
                }
            }
            RigControlCommand::DisableSlot { module_type } => {
                self.engine.disable_slot(module_type).await;
            }
            RigControlCommand::EnableSlot { module_type } => {
                self.engine.enable_slot(module_type).await;
            }
            RigControlCommand::LoadProfile { profile_id } => {
                if let Some(index) = self.data.profiles.iter().position(|p| p.id == profile_id) {
                    *self.current_profile_index.write().unwrap() = index;

                    if let Some(profile) = self.data.profiles.get(index) {
                        let profile_info = ProfileInfo::from_profile(profile);
                        self.broadcast_event(RigControlEvent::ProfileLoaded {
                            profile: profile_info,
                        });
                    }
                }
            }
            RigControlCommand::NextSong => {
                let mut song_index = self.current_song_index.write().unwrap();
                *song_index = (*song_index + 1).min(self.data.songs.len().saturating_sub(1));
                *self.current_section_index.write().unwrap() = 0;
                let index = *song_index;
                drop(song_index);

                self.broadcast_event(RigControlEvent::SongChanged { song_index: index });
                self.broadcast_event(RigControlEvent::SectionChanged { section_index: 0 });
            }
            RigControlCommand::PreviousSong => {
                let mut song_index = self.current_song_index.write().unwrap();
                *song_index = song_index.saturating_sub(1);
                *self.current_section_index.write().unwrap() = 0;
                let index = *song_index;
                drop(song_index);

                self.broadcast_event(RigControlEvent::SongChanged { song_index: index });
                self.broadcast_event(RigControlEvent::SectionChanged { section_index: 0 });
            }
            RigControlCommand::NextSection => {
                let mut section_index = self.current_section_index.write().unwrap();
                let song_index = *self.current_song_index.read().unwrap();
                if let Some(song) = self.data.songs.get(song_index) {
                    *section_index =
                        (*section_index + 1).min(song.sections.len().saturating_sub(1));
                }
                let index = *section_index;
                drop(section_index);

                self.broadcast_event(RigControlEvent::SectionChanged {
                    section_index: index,
                });
            }
            RigControlCommand::PreviousSection => {
                let mut section_index = self.current_section_index.write().unwrap();
                *section_index = section_index.saturating_sub(1);
                let index = *section_index;
                drop(section_index);

                self.broadcast_event(RigControlEvent::SectionChanged {
                    section_index: index,
                });
            }
            RigControlCommand::Tick => {
                self.engine.tick().await;
            }
        }
    }

    async fn subscribe(&self, _cx: &Context, events: Tx<RigControlEvent>) {
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
    /// Load a song section directly (convenience for tests).
    pub async fn load_song_section(
        &self,
        song_index: usize,
        section_index: usize,
    ) -> Option<TransitionResultInfo> {
        let song = self.data.songs.get(song_index)?;
        let section = song.sections.get(section_index)?;

        let result = self
            .engine
            .load_scene(
                &section.scene_ref,
                &self.data.rig,
                &self.data.store,
                &section.overrides,
            )
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
