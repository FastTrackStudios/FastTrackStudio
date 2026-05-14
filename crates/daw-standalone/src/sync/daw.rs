//! `Standalone` root handle and shared state types for the sync backend.

use std::collections::{BTreeMap, HashMap};
use std::sync::{Arc, Mutex};

use daw_proto::{
    AudioEngineState, AudioInputInfo, AudioLatency, DawError, DawResult, Fx, FxChainContext, Item,
    LastTouchedFx, LoadedPluginInfo, Marker, ProjectInfo, Region, ScreensetRect, Take, TempoPoint,
    Track, TrackRoute, Transport as TransportState, sync::Daw,
};

use super::action_registry::{ActionEntry, StandaloneActionRegistry};
use super::audio_engine::StandaloneAudioEngine;
use super::plugin_loader::StandalonePluginLoader;
use super::project::StandaloneProject;
use super::toolbar::{StandaloneToolbar, ToolbarEntry, ToolbarTargetKey};
use super::window_geometry::{StandaloneWindowGeometry, TargetKey as WindowTargetKey, WindowMap};

/// Hashable key derived from `FxChainContext` for use as a HashMap key.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum FxChainKey {
    Track(String),
    Input(String),
    Monitoring,
}

impl From<&FxChainContext> for FxChainKey {
    fn from(ctx: &FxChainContext) -> Self {
        match ctx {
            FxChainContext::Track(g) => FxChainKey::Track(g.clone()),
            FxChainContext::Input(g) => FxChainKey::Input(g.clone()),
            FxChainContext::Monitoring => FxChainKey::Monitoring,
        }
    }
}

/// Track-side entry for an item. Stores the item plus its current track guid
/// (so cross-track lookups by item guid can find the owning track).
#[derive(Clone, Debug)]
pub struct ItemEntry {
    pub item: Item,
}

/// All takes for an item, plus the active take's index.
#[derive(Clone, Debug, Default)]
pub struct TakeList {
    pub active_idx: u32,
    pub takes: Vec<Take>,
}

/// Per-FX state stored in the chain. Keeps the public `Fx` plus an internal
/// state-chunk blob and parameter map (param idx -> normalized value).
#[derive(Clone, Debug)]
pub struct FxEntry {
    pub fx: Fx,
    pub state_chunk: String,
    pub params: HashMap<u32, f64>,
}

/// Per-project in-memory state.
pub struct ProjectState {
    pub info: ProjectInfo,
    pub transport: TransportState,
    pub regions: BTreeMap<u32, Region>,
    pub markers: BTreeMap<u32, Marker>,
    pub tempo_points: Vec<TempoPoint>,
    pub tracks: Vec<Track>,
    pub next_region_id: u32,
    pub next_marker_id: u32,
    /// Per-track ext state keyed by `(track_guid, section, key)`.
    pub track_ext_state: HashMap<(String, String, String), String>,

    // Phase 2 storage ─────────────────────────────────────────────────────
    /// FX chains keyed by their context.
    pub fx_chains: HashMap<FxChainKey, Vec<FxEntry>>,
    /// Items keyed by item guid.
    pub items: HashMap<String, ItemEntry>,
    /// Per-track item ordering. `track_guid -> [item_guid, ...]`.
    pub items_by_track: HashMap<String, Vec<String>>,
    /// Takes keyed by item guid.
    pub takes: HashMap<String, TakeList>,
    /// Sends keyed by source track guid.
    pub sends: HashMap<String, Vec<TrackRoute>>,
    /// Receives keyed by destination track guid.
    pub receives: HashMap<String, Vec<TrackRoute>>,
    /// Hardware outputs keyed by source track guid.
    pub hw_outputs: HashMap<String, Vec<TrackRoute>>,
    /// Counter for synthesizing item GUIDs.
    pub next_item_counter: u64,
    /// Counter for synthesizing FX GUIDs.
    pub next_fx_counter: u64,
    /// Counter for synthesizing take GUIDs.
    pub next_take_counter: u64,
}

impl ProjectState {
    pub fn new(info: ProjectInfo) -> Self {
        Self {
            info,
            transport: TransportState::new(),
            regions: BTreeMap::new(),
            markers: BTreeMap::new(),
            tempo_points: Vec::new(),
            tracks: Vec::new(),
            next_region_id: 0,
            next_marker_id: 0,
            track_ext_state: HashMap::new(),
            fx_chains: HashMap::new(),
            items: HashMap::new(),
            items_by_track: HashMap::new(),
            takes: HashMap::new(),
            sends: HashMap::new(),
            receives: HashMap::new(),
            hw_outputs: HashMap::new(),
            next_item_counter: 0,
            next_fx_counter: 0,
            next_take_counter: 0,
        }
    }
}

/// Aggregate state for the standalone sync backend.
pub struct StandaloneState {
    pub projects: HashMap<String, ProjectState>,
    pub current_project_guid: Option<String>,
    /// Global ext state keyed by `(section, key)`.
    pub global_ext_state: HashMap<(String, String), String>,
    /// Last-touched FX, if any.
    pub last_touched_fx: Option<LastTouchedFx>,
    /// Buffered console output (for tests).
    pub console_log: Vec<String>,

    // Phase 3 storage ─────────────────────────────────────────────────────
    /// Registered actions keyed by command name.
    pub actions: HashMap<String, ActionEntry>,
    /// Counter for assigning fresh action ids.
    pub next_action_id: u32,
    /// Cached audio engine state.
    pub audio_engine: AudioEngineState,
    /// Cached audio latency snapshot.
    pub audio_latency: AudioLatency,
    /// Cached audio inputs (one entry per device).
    pub audio_inputs: Vec<AudioInputInfo>,
    /// Loaded plugins keyed by path.
    pub plugins: HashMap<String, LoadedPluginInfo>,
    /// Toolbar buttons keyed by `(target, command_name)`.
    pub toolbar_buttons: HashMap<(ToolbarTargetKey, String), ToolbarEntry>,
    /// Synthetic window geometry keyed by `WindowTarget`.
    pub windows: WindowMap,
}

impl StandaloneState {
    fn new() -> Self {
        Self {
            projects: HashMap::new(),
            current_project_guid: None,
            global_ext_state: HashMap::new(),
            last_touched_fx: None,
            console_log: Vec::new(),
            actions: HashMap::new(),
            next_action_id: 1,
            audio_engine: AudioEngineState::default(),
            audio_latency: AudioLatency::default(),
            audio_inputs: Vec::new(),
            plugins: HashMap::new(),
            toolbar_buttons: HashMap::new(),
            windows: HashMap::new(),
        }
    }
}

/// Root sync handle for the in-memory standalone backend.
///
/// Implements [`daw_proto::sync::Daw`]. Construct with [`Standalone::new`] and
/// optionally seed projects via [`Standalone::seed_project`].
#[derive(Clone)]
pub struct Standalone {
    pub(crate) state: Arc<Mutex<StandaloneState>>,
}

impl Default for Standalone {
    fn default() -> Self {
        Self::new()
    }
}

impl Standalone {
    /// Create a new empty standalone backend.
    pub fn new() -> Self {
        Self {
            state: Arc::new(Mutex::new(StandaloneState::new())),
        }
    }

    /// Seed an empty project into the state.
    ///
    /// If this is the first seeded project, it becomes the current project.
    /// Returns the GUID of the seeded project for convenience.
    pub fn seed_project(&self, info: ProjectInfo) -> String {
        let guid = info.guid.clone();
        let mut s = self.state.lock().expect("standalone state poisoned");
        if s.current_project_guid.is_none() {
            s.current_project_guid = Some(guid.clone());
        }
        s.projects.insert(guid.clone(), ProjectState::new(info));
        guid
    }

    /// Mark `guid` as the current project. No-op if the project is not present.
    pub fn set_current_project(&self, guid: &str) {
        let mut s = self.state.lock().expect("standalone state poisoned");
        if s.projects.contains_key(guid) {
            s.current_project_guid = Some(guid.to_string());
        }
    }

    /// Run a closure with mutable access to a project's state.
    pub(crate) fn with_project_mut<R>(
        &self,
        guid: &str,
        f: impl FnOnce(&mut ProjectState) -> R,
    ) -> DawResult<R> {
        let mut s = self.state.lock().expect("standalone state poisoned");
        let project = s
            .projects
            .get_mut(guid)
            .ok_or_else(|| DawError::not_found("Project", guid))?;
        Ok(f(project))
    }

    /// Run a closure with shared access to a project's state.
    pub(crate) fn with_project<R>(
        &self,
        guid: &str,
        f: impl FnOnce(&ProjectState) -> R,
    ) -> DawResult<R> {
        let s = self.state.lock().expect("standalone state poisoned");
        let project = s
            .projects
            .get(guid)
            .ok_or_else(|| DawError::not_found("Project", guid))?;
        Ok(f(project))
    }

    /// Captured console messages, useful in tests.
    pub fn console_log(&self) -> Vec<String> {
        let s = self.state.lock().expect("standalone state poisoned");
        s.console_log.clone()
    }

    /// Set the synthetic last-touched FX (for tests).
    pub fn set_last_touched_fx(&self, fx: Option<LastTouchedFx>) {
        let mut s = self.state.lock().expect("standalone state poisoned");
        s.last_touched_fx = fx;
    }

    /// Seed the cached audio engine state (for tests).
    pub fn seed_audio_engine_state(
        &self,
        engine: AudioEngineState,
        latency: AudioLatency,
        inputs: Vec<AudioInputInfo>,
    ) {
        let mut s = self.state.lock().expect("standalone state poisoned");
        s.audio_engine = engine;
        s.audio_latency = latency;
        s.audio_inputs = inputs;
    }

    /// Seed a synthetic window rect for a `WindowTarget` (for tests).
    pub fn seed_window_target(
        &self,
        target: daw_proto::WindowTarget,
        x: i32,
        y: i32,
        width: u32,
        height: u32,
    ) {
        let mut s = self.state.lock().expect("standalone state poisoned");
        s.windows.insert(
            WindowTargetKey::from(target),
            ScreensetRect {
                x,
                y,
                width,
                height,
            },
        );
    }
}

impl Daw for Standalone {
    type Project<'a> = StandaloneProject<'a>;
    type ActionRegistry<'a> = StandaloneActionRegistry<'a>;
    type AudioEngine<'a> = StandaloneAudioEngine<'a>;
    type PluginLoader<'a> = StandalonePluginLoader<'a>;
    type Toolbar<'a> = StandaloneToolbar<'a>;
    type WindowGeometry<'a> = StandaloneWindowGeometry<'a>;

    fn current_project(&self) -> DawResult<Self::Project<'_>> {
        let guid = {
            let s = self.state.lock().expect("standalone state poisoned");
            s.current_project_guid
                .clone()
                .ok_or_else(|| DawError::not_found("Project", "current"))?
        };
        // Verify the project is still resident.
        self.with_project(&guid, |_| ())?;
        Ok(StandaloneProject::new(self, guid))
    }

    fn project(&self, guid: &str) -> DawResult<Self::Project<'_>> {
        self.with_project(guid, |_| ())?;
        Ok(StandaloneProject::new(self, guid.to_string()))
    }

    fn projects(&self) -> Vec<ProjectInfo> {
        let s = self.state.lock().expect("standalone state poisoned");
        s.projects.values().map(|p| p.info.clone()).collect()
    }

    fn show_console_msg(&self, msg: &str) {
        let mut s = self.state.lock().expect("standalone state poisoned");
        s.console_log.push(msg.to_string());
    }

    fn last_touched_fx(&self) -> Option<LastTouchedFx> {
        let s = self.state.lock().expect("standalone state poisoned");
        s.last_touched_fx.clone()
    }

    fn action_registry(&self) -> Self::ActionRegistry<'_> {
        StandaloneActionRegistry::new(self)
    }

    fn audio_engine(&self) -> Self::AudioEngine<'_> {
        StandaloneAudioEngine::new(self)
    }

    fn plugin_loader(&self) -> Self::PluginLoader<'_> {
        StandalonePluginLoader::new(self)
    }

    fn toolbar(&self) -> Self::Toolbar<'_> {
        StandaloneToolbar::new(self)
    }

    fn window_geometry(&self) -> Self::WindowGeometry<'_> {
        StandaloneWindowGeometry::new(self)
    }
}
