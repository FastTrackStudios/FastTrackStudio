//! `Standalone` root handle and shared state types for the sync backend.

use std::collections::{BTreeMap, HashMap};
use std::sync::{Arc, Mutex};

use daw_proto::automation::{Envelope, EnvelopePoint, EnvelopeRef, EnvelopeType};
use daw_proto::midi::MidiNote;
use daw_proto::primitives::AutomationMode;
use daw_proto::{
    Daw, DawError, DawResult, Fx, FxChainContext, Item, LastTouchedFx, Marker, ProjectInfo,
    RecordInput, Region, Take, TempoPoint, Track, TrackRoute, Transport as TransportState,
};

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

/// Per-track properties that aren't carried on the proto `Track` struct.
/// REAPER models these as `I_NCHAN`, `I_RECINPUT`, etc.; we store them
/// alongside the proto `Track` so routing/recording code can read them.
#[derive(Clone, Debug)]
pub struct TrackExt {
    /// Channel count. REAPER caps at 128. Standalone allows any
    /// `1..=128` (not constrained to stereo pairs). Default = 2.
    pub num_channels: u32,
    /// Record input source. `RecordInput::None` if none configured.
    pub record_input: RecordInput,
    /// Whether this track sends its output to the master/parent track
    /// (REAPER's `B_MAINSEND`). Default = true.
    pub parent_send_enabled: bool,
}

/// Stable hashable identity for an envelope on a track. Lifts
/// `EnvelopeRef`'s string-y / nested shape into something usable as a
/// `HashMap` key.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum EnvelopeKey {
    /// One of the predefined track envelopes (volume/pan/etc).
    Track(EnvelopeType),
    /// An FX parameter automation envelope.
    FxParam { fx_guid: String, param_index: u32 },
    /// Free-form by name (rare; ReaScript-style).
    Named(String),
}

impl EnvelopeKey {
    pub fn from_ref(env_ref: &EnvelopeRef) -> Self {
        match env_ref {
            EnvelopeRef::Type(t) => Self::Track(*t),
            EnvelopeRef::FxParam {
                fx_guid,
                param_index,
            } => Self::FxParam {
                fx_guid: fx_guid.clone(),
                param_index: *param_index,
            },
            EnvelopeRef::ByName(n) => Self::Named(n.clone()),
        }
    }
}

/// Per-envelope state. Points are kept sorted by time.
#[derive(Clone, Debug)]
pub struct EnvelopeData {
    pub visible: bool,
    pub armed: bool,
    pub automation_mode: AutomationMode,
    pub points: Vec<EnvelopePoint>,
}

impl EnvelopeData {
    pub fn new() -> Self {
        Self {
            visible: false,
            armed: false,
            automation_mode: AutomationMode::TrimRead,
            points: Vec::new(),
        }
    }

    /// Build a proto `Envelope` snapshot for this data + identity.
    pub fn to_proto(&self, track_guid: &str, key: &EnvelopeKey) -> Envelope {
        let (envelope_type, fx_guid, param_index, name) = match key {
            EnvelopeKey::Track(t) => (*t, None, None, envelope_default_name(*t).to_string()),
            EnvelopeKey::FxParam {
                fx_guid,
                param_index,
            } => (
                EnvelopeType::FxParam,
                Some(fx_guid.clone()),
                Some(*param_index),
                format!("FX {}: param {}", fx_guid, param_index),
            ),
            EnvelopeKey::Named(n) => (EnvelopeType::Volume, None, None, n.clone()),
        };
        Envelope {
            track_guid: track_guid.to_string(),
            envelope_type,
            name,
            fx_guid,
            param_index,
            visible: self.visible,
            armed: self.armed,
            automation_mode: self.automation_mode,
            point_count: self.points.len() as u32,
        }
    }
}

impl Default for EnvelopeData {
    fn default() -> Self {
        Self::new()
    }
}

fn envelope_default_name(t: EnvelopeType) -> &'static str {
    match t {
        EnvelopeType::Volume => "Volume",
        EnvelopeType::VolumePrefx => "Volume (Pre-FX)",
        EnvelopeType::Pan => "Pan",
        EnvelopeType::PanPrefx => "Pan (Pre-FX)",
        EnvelopeType::Width => "Width",
        EnvelopeType::WidthPrefx => "Width (Pre-FX)",
        EnvelopeType::Mute => "Mute",
        EnvelopeType::FxParam => "FX Param",
    }
}

impl Default for TrackExt {
    fn default() -> Self {
        Self {
            num_channels: 2,
            record_input: RecordInput::None,
            parent_send_enabled: true,
        }
    }
}

/// Per-project in-memory state.
pub struct ProjectState {
    pub info: ProjectInfo,
    pub transport: TransportState,
    pub regions: BTreeMap<u32, Region>,
    pub markers: BTreeMap<u32, Marker>,
    pub tempo_points: Vec<TempoPoint>,
    pub tracks: Vec<Track>,
    /// Per-track extended properties (channel count, record input) keyed
    /// by track GUID. Lazily populated on first mutation; missing keys
    /// resolve to `TrackExt::default()`.
    pub track_ext: HashMap<String, TrackExt>,
    /// MIDI note data keyed by take GUID. Only populated for MIDI takes.
    /// Notes are stored in insertion order; the `index` field on
    /// each `MidiNote` mirrors its position in the vec (rewritten on
    /// add/delete to keep references stable within a single read).
    pub midi_notes: HashMap<String, Vec<MidiNote>>,
    /// Automation envelopes keyed by `(track_guid, EnvelopeKey)`. Points
    /// are kept sorted by time within each envelope.
    pub envelopes: HashMap<(String, EnvelopeKey), EnvelopeData>,
    /// Global automation override (`None` = no override active).
    pub global_automation_override: Option<AutomationMode>,
    pub next_region_id: u32,
    pub next_marker_id: u32,
    /// Per-track ext state keyed by `(track_guid, section, key)`.
    pub track_ext_state: HashMap<(String, String, String), String>,
    /// Project-scoped ext state keyed by `(section, key)`. Mirrors
    /// REAPER's GetProjExtState/SetProjExtState semantics — these
    /// values live with the project, not the host.
    pub project_ext_state: HashMap<(String, String), String>,

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
            track_ext: HashMap::new(),
            midi_notes: HashMap::new(),
            envelopes: HashMap::new(),
            global_automation_override: None,
            next_region_id: 0,
            next_marker_id: 0,
            track_ext_state: HashMap::new(),
            project_ext_state: HashMap::new(),
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
}

impl StandaloneState {
    fn new() -> Self {
        Self {
            projects: HashMap::new(),
            current_project_guid: None,
            global_ext_state: HashMap::new(),
            last_touched_fx: None,
            console_log: Vec::new(),
        }
    }
}

/// Root sync handle for the in-memory standalone backend.
///
/// Implements [`daw_proto::Daw`]. Construct with [`Standalone::new`] and
/// optionally seed projects via [`Standalone::seed_project`].
#[derive(Clone)]
pub struct Standalone {
    pub(crate) state: Arc<Mutex<StandaloneState>>,
    /// Per-project transport engine bundles (sample clock + subscriber pumps).
    /// Lazily created on first access. WASM-compatible; lock-free on the
    /// inner [`TransportShared`] atomics.
    pub(crate) transport_engines: Arc<
        Mutex<std::collections::HashMap<String, Arc<crate::transport_engine::TransportBundle>>>,
    >,
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
            transport_engines: Arc::new(Mutex::new(std::collections::HashMap::new())),
        }
    }

    /// Get or lazily-create the transport engine bundle for `guid`.
    /// Seeds initial BPM from the project's `Transport.tempo`.
    pub fn transport_engine_for(
        &self,
        guid: &str,
    ) -> Arc<crate::transport_engine::TransportBundle> {
        // Fast path: already created.
        {
            let engines = self.transport_engines.lock().expect("engines poisoned");
            if let Some(b) = engines.get(guid) {
                return b.clone();
            }
        }
        // Slow path: seed from project tempo + insert.
        let initial_bpm = self
            .with_project(guid, |p| p.transport.tempo.bpm())
            .unwrap_or(120.0);
        let bundle = Arc::new(crate::transport_engine::TransportBundle::spawn(
            48_000,
            initial_bpm,
        ));
        let mut engines = self.transport_engines.lock().expect("engines poisoned");
        engines
            .entry(guid.to_string())
            .or_insert_with(|| bundle.clone())
            .clone()
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

    /// Attach a cpal-backed [`AudioEngine`](crate::audio_engine::AudioEngine)
    /// to project `guid`. Disables the project's soft clock so the
    /// audio callback becomes the sole driver of the playhead.
    /// Sample-accurate. Returns the engine; drop it to release the
    /// stream (and re-enable the soft clock manually if desired).
    #[cfg(feature = "audio")]
    pub fn attach_audio_engine(
        &self,
        guid: &str,
    ) -> Result<crate::audio_engine::AudioEngine, String> {
        let bundle = self.transport_engine_for(guid);
        bundle.disable_soft_clock();
        crate::audio_engine::AudioEngine::with_shared(bundle.shared.clone())
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
}

impl Daw for Standalone {
    type Project<'a> = super::project::StandaloneProject<'a>;

    fn current_project(&self) -> DawResult<Self::Project<'_>> {
        let guid = {
            let s = self.state.lock().expect("standalone state poisoned");
            s.current_project_guid
                .clone()
                .ok_or_else(|| DawError::not_found("Project", "current"))?
        };
        self.with_project(&guid, |_| ())?;
        Ok(super::project::StandaloneProject::new(self, guid))
    }

    fn project(&self, guid: &str) -> DawResult<Self::Project<'_>> {
        self.with_project(guid, |_| ())?;
        Ok(super::project::StandaloneProject::new(
            self,
            guid.to_string(),
        ))
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
}
