//! Live **sample-instrument** rig — the MIDI analog of [`GuitarRig`](crate::rig::GuitarRig),
//! running signal's [`SampleEngine`]s ON daw's realtime [`AudioEngine`].
//!
//! Where [`GuitarRig`] pass-through processes a live *audio* input, a
//! [`SamplerRig`] **generates** audio from MIDI: each loaded instrument is one
//! daw track whose FX chain holds a single [`SamplerInstrument`] (a
//! [`SampleEngine`] wrapped as a [`PluginInstance`]). The renderer runs every
//! track's chain each block — feeding it that block's live MIDI (pushed via
//! [`Standalone::push_note_on`] / `push_note_off` / `push_cc`) — and the
//! instrument writes its rendered voices into the track bus, which sums to
//! master.
//!
//! ## How a project is built
//!
//! [`SamplerRig::open`] mirrors [`GuitarRig::open`] exactly:
//! `Standalone::new` → `seed_project` → `set_current_project` → (per instrument)
//! `Tracks::add` + reserve one FX slot via `FxChains::add`/`get` →
//! `AudioEngine::with_project_prefs` → `set_meters` → `insert_plugin_instance`
//! → `set_play_state(Playing)`. The difference: there is **no** record input
//! (sample instruments are sources, not effects), and instruments are added at
//! runtime via [`add_instrument`](SamplerRig::add_instrument) rather than a
//! single fixed input track.
//!
//! Each instrument keeps its track guid + FX-slot guid + meter cell index in
//! the controller's [`InstrumentTrack`] routing table — this is what replaces
//! signal's `note_routing` / `runtime.rs` graph. `note_on(id, ..)` resolves the
//! instrument's track guid and calls `daw.push_note_on(track_guid, ..)`.
//!
//! ## Drum mic / bus routing (the mixer mapping)
//!
//! [`SamplerRig::from_mixer_layout`] maps a [`MixerLayout`] onto daw's native
//! send/bus primitives — **no new daw capability is needed**:
//!
//! - **Close mic / direct-to-master** → a plain instrument track. daw tracks
//!   parent-send to master by default, so nothing extra is wired.
//! - **Bus mic (overhead / room)** → the mic's instrument track gets a
//!   [`Routing::add_send`] to a shared **bus track** with
//!   [`SendMode::PostFx`], and its parent-send to master is disabled
//!   ([`Routing::set_parent_send_enabled`] `false`) so it reaches master ONLY
//!   through the bus. The bus track is a plain `Tracks::add` track that sums
//!   its receives and parent-sends to master, carrying the bus's FX chain.
//!
//! For v1 the model is "one instrument = one track = one mic": a multi-mic
//! piece is expressed as several [`MixerLayout`] strips (each its own engine
//! configured for that mic id), and the controller fans a note to every mic
//! track of one piece by pushing the same MIDI to each track's queue (see
//! [`note_on`](SamplerRig::note_on), which routes to all tracks of a piece id).

use std::collections::HashMap;
use std::sync::Arc;

use daw::service::{
    FxChainContext, FxChains, ProjectContext, ProjectInfo, RouteRef, Routing, SendMode, TrackRef,
    Tracks,
};
use daw::standalone::Standalone;
use daw::standalone::audio_engine::AudioEngine;
use daw::standalone::metering::{Meters, linear_to_db};
use daw::standalone::transport_engine::{PlayStateRepr, TransportShared};
use daw_audio_io::AudioIoPrefs;

use crate::engine::SampleEngine;
use crate::instrument::SamplerInstrument;
use crate::mixer::{FX_PREPARE_BLOCK, MixerLayout};

/// Stable identifier for a loaded instrument within the rig (e.g. a piece id
/// like `"kick"`, or `"kick:Overhead"` for a per-mic instrument).
pub type InstrumentId = String;

/// Block size sampler instruments are prepared for. The engine's render is
/// block-size agnostic, but the instrument sizes its de-interleave scratch on
/// `prepare`; preparing for [`FX_PREPARE_BLOCK`] avoids any per-block resize
/// against larger backend buffers.
const PREPARE_BLOCK: u32 = FX_PREPARE_BLOCK;

/// One instrument's place in the daw project: which track carries it, which FX
/// slot guid backs its [`SamplerInstrument`], and which [`Meters`] cell (=
/// project track index) holds its post-fader peak.
#[derive(Clone, Debug)]
pub struct InstrumentTrack {
    /// daw track guid — the target of `push_note_on` / `push_cc`.
    pub track_guid: String,
    /// FX-slot guid the instrument box was inserted under.
    pub fx_guid: String,
    /// Meter cell index (project track order) for this track's output peak.
    pub meter_index: usize,
    /// Optional grouping piece id (e.g. `"kick"`) when this instrument is one
    /// mic of a multi-mic piece. `None` = standalone instrument.
    pub piece: Option<String>,
}

/// One bus track (overhead / room) that sums its mic sends → master.
#[derive(Clone, Debug)]
pub struct BusTrack {
    /// The bus mic id this track collects (e.g. `"Overhead"`).
    pub id: String,
    pub track_guid: String,
    pub meter_index: usize,
}

/// A live sample-instrument rig: one daw track per instrument (each carrying a
/// [`SamplerInstrument`] in its FX chain), optionally fed through shared bus
/// tracks, running on daw's realtime [`AudioEngine`].
pub struct SamplerRig {
    daw: Standalone,
    // Engine owns the cpal output stream; drop = stop.
    _engine: AudioEngine,
    #[allow(dead_code)]
    shared: Arc<TransportShared>,
    meters: Arc<Meters>,
    #[allow(dead_code)]
    project_guid: String,

    /// Per-instrument routing table (the note-routing graph). Insertion order
    /// is the project track order, so a new track's meter cell index is the
    /// current `instruments.len() + buses.len()`.
    instruments: HashMap<InstrumentId, InstrumentTrack>,
    /// Insertion order of instrument ids (for stable iteration / next track idx).
    order: Vec<InstrumentId>,
    /// Shared bus tracks, keyed by bus mic id.
    buses: HashMap<String, BusTrack>,

    pub sample_rate: u32,
}

const SAMPLER_PROJECT_NAME: &str = "Signal Sampler Rig";

impl SamplerRig {
    /// Open an **empty** sampler project on daw's engine, transport rolling.
    /// Instruments are added afterwards via [`add_instrument`](Self::add_instrument).
    ///
    /// The engine is output-only (`want_input = false`): sample instruments
    /// generate audio from MIDI, so no live input stream is opened.
    pub fn open(prefs: &AudioIoPrefs) -> eyre::Result<Self> {
        // Low-latency quantum under JACK/PipeWire (same as GuitarRig::open).
        #[cfg(feature = "jack")]
        {
            if prefs.buffer_size != 0 && std::env::var_os("PIPEWIRE_LATENCY").is_none() {
                let rate = if prefs.sample_rate != 0 { prefs.sample_rate } else { 48_000 };
                let buf = prefs.buffer_size;
                unsafe { std::env::set_var("PIPEWIRE_LATENCY", format!("{buf}/{rate}")) };
                tracing::info!(quantum = %format!("{buf}/{rate}"), "sampler rig: requesting PipeWire low-latency quantum");
            }
        }

        let daw = Standalone::new();

        let project_guid = crate::rig::uuid_string();
        daw.seed_project(ProjectInfo {
            guid: project_guid.clone(),
            name: SAMPLER_PROJECT_NAME.to_string(),
            path: String::new(),
        });
        daw.set_current_project(&project_guid);

        // Output-only engine (no input stream needed for a sampler).
        let mut io = prefs.clone();
        io.want_input = false;
        let req_rate = if prefs.sample_rate != 0 { prefs.sample_rate } else { 48_000 };
        let shared = Arc::new(TransportShared::new(req_rate, 120.0));
        let engine =
            AudioEngine::with_project_prefs(daw.clone(), project_guid.clone(), shared.clone(), &io)
                .map_err(|e| eyre::eyre!("sampler rig: audio engine failed: {e}"))?;
        let sample_rate = engine.sample_rate();

        // Roll transport so the renderer runs every block.
        shared.set_play_state(PlayStateRepr::Playing);

        tracing::info!(
            sample_rate,
            project = %project_guid,
            "signal-sampler: sampler rig started on daw engine"
        );

        Ok(Self {
            daw,
            _engine: engine,
            shared,
            // Sized to 0 tracks initially; re-sized as tracks are added so each
            // instrument's cell index lines up with its project track order.
            meters: Meters::new(0),
            project_guid,
            instruments: HashMap::new(),
            order: Vec::new(),
            buses: HashMap::new(),
            sample_rate,
        })
    }

    /// Total tracks currently in the project (instruments + buses). Equals the
    /// next track's meter cell index.
    fn track_count(&self) -> usize {
        self.instruments.len() + self.buses.len()
    }

    /// Re-install the meter bank sized for the current track count, so every
    /// project track index (= insertion order) has a cell. daw's renderer reads
    /// `daw.meters()` fresh each block, so swapping the bank is safe between
    /// blocks; peaks reset to 0 and refill on the next block.
    fn resize_meters(&mut self) {
        let meters = Meters::new(self.track_count());
        self.daw.set_meters(meters.clone());
        self.meters = meters;
    }

    /// Add a daw track named `name` carrying `engine` as a [`SamplerInstrument`]
    /// in its FX chain. Returns the new track's guid + fx-slot guid + meter cell.
    /// `piece` groups multi-mic instruments under one note-routing key.
    fn add_instrument_track(
        &mut self,
        id: InstrumentId,
        name: &str,
        engine: SampleEngine,
        piece: Option<String>,
    ) -> eyre::Result<InstrumentTrack> {
        let meter_index = self.track_count();
        let track_guid = <Standalone as Tracks>::add(&self.daw, ProjectContext::Current, name, None)
            .map_err(|e| eyre::eyre!("sampler rig: add track {name:?} failed: {e}"))?;

        // Reserve one FX slot for the instrument and capture its assigned guid.
        let fx_ctx = FxChainContext::track(track_guid.clone());
        let idx = <Standalone as FxChains>::add(&self.daw, fx_ctx.clone(), &format!("inst-{id}"))
            .map_err(|e| eyre::eyre!("sampler rig: reserve fx slot for {id:?} failed: {e}"))?;
        let fx_guid = <Standalone as FxChains>::get(&self.daw, fx_ctx, idx)
            .map(|fx| fx.guid)
            .ok_or_else(|| eyre::eyre!("sampler rig: fx slot for {id:?} vanished after add"))?;

        // Grow the meter bank to include the new track, then drop the prepared
        // instrument box into its slot.
        self.resize_meters();
        let mut inst = SamplerInstrument::new(engine);
        let _ = daw::plugin::PluginInstance::prepare(
            &mut inst,
            self.sample_rate as f64,
            PREPARE_BLOCK,
        );
        self.daw.insert_plugin_instance(fx_guid.clone(), Box::new(inst));

        let track = InstrumentTrack { track_guid, fx_guid, meter_index, piece };
        self.instruments.insert(id.clone(), track.clone());
        self.order.push(id);
        Ok(track)
    }

    /// Add a standalone sample instrument: one daw track + a [`SamplerInstrument`]
    /// wrapping `engine` in its FX chain, summing to master. The engine must
    /// already be constructed at this rig's [`sample_rate`](Self::sample_rate).
    pub fn add_instrument(&mut self, id: InstrumentId, engine: SampleEngine) -> eyre::Result<()> {
        if self.instruments.contains_key(&id) {
            return Err(eyre::eyre!("sampler rig: instrument {id:?} already added"));
        }
        let name = id.clone();
        self.add_instrument_track(id, &name, engine, None)?;
        Ok(())
    }

    /// Add a plain **bus track** (no instrument) that sums its mic sends and
    /// parent-sends to master. Returns its guid + meter cell.
    fn add_bus_track(&mut self, bus_id: &str) -> eyre::Result<BusTrack> {
        let meter_index = self.track_count();
        let name = format!("{bus_id} (bus)");
        let track_guid =
            <Standalone as Tracks>::add(&self.daw, ProjectContext::Current, &name, None)
                .map_err(|e| eyre::eyre!("sampler rig: add bus track {bus_id:?} failed: {e}"))?;
        self.resize_meters();
        let bus = BusTrack { id: bus_id.to_string(), track_guid, meter_index };
        self.buses.insert(bus_id.to_string(), bus.clone());
        Ok(bus)
    }

    /// Wire a send from `source_track` into `bus_track` (post-FX), and disable
    /// the source's parent-send so it reaches master ONLY through the bus.
    ///
    /// Uses daw's [`Routing::add_send`] + [`Routing::set_send_mode`]
    /// ([`SendMode::PostFx`]) + [`Routing::set_parent_send_enabled`] — exactly
    /// the primitives `tests/render_routing.rs::send_routes_audio_into_destination_bus`
    /// exercises in daw.
    fn route_to_bus(&self, source_track: &str, bus_track: &str) -> eyre::Result<()> {
        let ctx = ProjectContext::Current;
        let idx = <Standalone as Routing>::add_send(
            &self.daw,
            ctx.clone(),
            TrackRef::guid(source_track),
            TrackRef::guid(bus_track),
        )
        .ok_or_else(|| eyre::eyre!("sampler rig: add_send {source_track} → {bus_track} failed"))?;
        <Standalone as Routing>::set_send_mode(
            &self.daw,
            ctx.clone(),
            TrackRef::guid(source_track),
            RouteRef::Index(idx),
            SendMode::PostFx,
        )
        .map_err(|e| eyre::eyre!("sampler rig: set_send_mode failed: {e}"))?;
        <Standalone as Routing>::set_parent_send_enabled(
            &self.daw,
            ctx,
            TrackRef::guid(source_track),
            false,
        )
        .map_err(|e| eyre::eyre!("sampler rig: disable parent send failed: {e}"))?;
        Ok(())
    }

    /// Build a sampler rig from a [`MixerLayout`] + a per-strip engine map.
    ///
    /// `engines` maps each `(engine_idx, mic_label)` channel/send strip key to
    /// the [`SampleEngine`] that should voice it. The mapping is:
    ///
    /// - Each **channel strip** (close mic) → one instrument track direct to
    ///   master, keyed `"{engine_label}:{mic_label}"`, grouped under piece
    ///   `engine_label`.
    /// - Each **send strip** (bus mic) → one instrument track that sends to the
    ///   strip's bus track and has its parent send disabled. A bus track is
    ///   created on first use of each distinct bus id, summing to master.
    ///
    /// Strips with no engine in `engines` are skipped (no track created).
    ///
    /// This is the testable mixer→daw mapping helper; the chosen daw calls are
    /// [`Tracks::add`], [`Routing::add_send`], [`Routing::set_send_mode`], and
    /// [`Routing::set_parent_send_enabled`].
    pub fn from_mixer_layout(
        prefs: &AudioIoPrefs,
        layout: &MixerLayout,
        mut engines: HashMap<(usize, String), SampleEngine>,
    ) -> eyre::Result<Self> {
        let mut rig = Self::open(prefs)?;

        for eng in &layout.engines {
            let piece = eng.label.clone();

            // Close mics → direct instrument tracks.
            for ch in &eng.channels {
                let key = (eng.engine_idx, ch.mic_label.clone());
                let Some(engine) = engines.remove(&key) else {
                    continue;
                };
                let id = format!("{}:{}", eng.label, ch.mic_label);
                let name = format!("{} {}", eng.label, ch.mic_label);
                rig.add_instrument_track(id, &name, engine, Some(piece.clone()))?;
            }

            // Bus mics → instrument tracks that send into a shared bus.
            for snd in &eng.sends {
                let key = (eng.engine_idx, snd.mic_label.clone());
                let Some(engine) = engines.remove(&key) else {
                    continue;
                };

                // Ensure the bus track exists (one per distinct bus id).
                let bus_id = if snd.bus_label.is_empty() {
                    snd.mic_label.clone()
                } else {
                    snd.bus_label.clone()
                };
                let bus_guid = match rig.buses.get(&bus_id) {
                    Some(b) => b.track_guid.clone(),
                    None => rig.add_bus_track(&bus_id)?.track_guid,
                };

                let id = format!("{}:{}", eng.label, snd.mic_label);
                let name = format!("{} {}", eng.label, snd.mic_label);
                let track = rig.add_instrument_track(id, &name, engine, Some(piece.clone()))?;
                rig.route_to_bus(&track.track_guid, &bus_guid)?;
            }
        }

        Ok(rig)
    }

    // ── Note driving ────────────────────────────────────────────────────────

    /// Resolve `id` to all daw track guids it should drive. An exact instrument
    /// id matches its own track; otherwise `id` is treated as a **piece** id and
    /// fans out to every mic track grouped under it.
    fn tracks_for(&self, id: &str) -> Vec<String> {
        if let Some(t) = self.instruments.get(id) {
            return vec![t.track_guid.clone()];
        }
        self.instruments
            .values()
            .filter(|t| t.piece.as_deref() == Some(id))
            .map(|t| t.track_guid.clone())
            .collect()
    }

    /// Note-on to instrument (or piece) `id`. Fans the same MIDI to every mic
    /// track of a multi-mic piece (the controller's routing table).
    pub fn note_on(&self, id: &str, note: u8, vel: u8) {
        for guid in self.tracks_for(id) {
            self.daw.push_note_on(&guid, note, vel);
        }
    }

    /// Note-off to instrument (or piece) `id`.
    pub fn note_off(&self, id: &str, note: u8) {
        for guid in self.tracks_for(id) {
            self.daw.push_note_off(&guid, note);
        }
    }

    /// Control-change to instrument (or piece) `id`.
    pub fn cc(&self, id: &str, controller: u8, value: u8) {
        for guid in self.tracks_for(id) {
            self.daw.push_cc(&guid, controller, value);
        }
    }

    // ── Introspection / metering ──────────────────────────────────────────────

    /// Resolved sample rate the rig opened at.
    pub fn sample_rate(&self) -> u32 {
        self.sample_rate
    }

    /// Number of instrument tracks.
    pub fn instrument_count(&self) -> usize {
        self.instruments.len()
    }

    /// The routing-table entry for an instrument id, if loaded.
    pub fn instrument(&self, id: &str) -> Option<&InstrumentTrack> {
        self.instruments.get(id)
    }

    /// Bus tracks, keyed by bus id.
    pub fn buses(&self) -> &HashMap<String, BusTrack> {
        &self.buses
    }

    /// Per-track output peak (linear) for instrument `id`.
    pub fn instrument_peak(&self, id: &str) -> f32 {
        self.instruments
            .get(id)
            .and_then(|t| self.meters.cell(t.meter_index))
            .map(|c| c.peak(0).max(c.peak(1)))
            .unwrap_or(0.0)
    }

    /// Per-track output peak (linear) for bus `id`.
    pub fn bus_peak(&self, id: &str) -> f32 {
        self.buses
            .get(id)
            .and_then(|b| self.meters.cell(b.meter_index))
            .map(|c| c.peak(0).max(c.peak(1)))
            .unwrap_or(0.0)
    }

    /// Master / overall output peak (linear) — the loudest track cell. daw's
    /// renderer meters per-track post-fader; a project-level master meter isn't
    /// surfaced, so this returns the max across all tracks.
    pub fn output_peak(&self) -> f32 {
        let mut pk = 0.0f32;
        for i in 0..self.meters.len() {
            if let Some(c) = self.meters.cell(i) {
                pk = pk.max(c.peak(0)).max(c.peak(1));
            }
        }
        pk
    }

    /// dB of the current output peak.
    pub fn output_peak_db(&self) -> f64 {
        linear_to_db(self.output_peak())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mixer::mic_is_bus;

    /// A 2-piece kit (kick, snare), each with a close mic + an Overhead bus mic.
    fn kit_layout() -> MixerLayout {
        // DrumMixer::build derives the same layout shape we map here.
        let mics = |v: &[&str]| v.iter().map(|s| s.to_string()).collect::<Vec<_>>();
        let engine_mics = vec![
            ("kick".to_string(), mics(&["In 1", "Overhead"])),
            ("snare".to_string(), mics(&["In 1", "Overhead"])),
        ];
        crate::mixer::DrumMixer::build(&engine_mics, 48_000).layout()
    }

    #[test]
    fn classifies_close_and_bus_mics() {
        // Sanity: the mapping relies on `mic_is_bus` to split direct vs send.
        assert!(!mic_is_bus("In 1"));
        assert!(mic_is_bus("Overhead"));
    }

    /// The mixer→daw mapping shape, derived purely from a `MixerLayout` without
    /// opening an audio device: 2 close mics + 2 bus-mic sends + 1 shared bus.
    /// (Counts mirror what `from_mixer_layout` would create.)
    #[test]
    fn layout_yields_expected_track_shape() {
        let layout = kit_layout();
        let total_channels: usize = layout.engines.iter().map(|e| e.channels.len()).sum();
        let total_sends: usize = layout.engines.iter().map(|e| e.sends.len()).sum();
        assert_eq!(total_channels, 2, "two close mics → two direct tracks");
        assert_eq!(total_sends, 2, "two overhead sends");
        assert_eq!(layout.buses.len(), 1, "one shared Overhead bus");
        // All overhead sends point at the same bus.
        let bus_labels: std::collections::HashSet<_> = layout
            .engines
            .iter()
            .flat_map(|e| e.sends.iter().map(|s| s.bus_label.clone()))
            .collect();
        assert_eq!(bus_labels.len(), 1);
        assert_eq!(bus_labels.into_iter().next().unwrap(), "Overhead");
    }

    // Device-backed tests (open / add_instrument / from_mixer_layout end to end)
    // require an audio backend; under `--features jack` they need a running
    // JACK/PipeWire server. Gated behind an env var so the default `cargo test`
    // stays headless-friendly, matching Phase 1's approach.
    fn audio_available() -> bool {
        std::env::var_os("SIGNAL_SAMPLER_RIG_AUDIO").is_some()
    }

    fn write_sine_wav(path: &std::path::Path, frames: usize) {
        let spec = hound::WavSpec {
            channels: 1,
            sample_rate: 48_000,
            bits_per_sample: 32,
            sample_format: hound::SampleFormat::Float,
        };
        let mut w = hound::WavWriter::create(path, spec).expect("create wav");
        for i in 0..frames {
            let t = i as f32 / 48_000.0;
            let s = (2.0 * std::f32::consts::PI * 220.0 * t).sin() * 0.8;
            w.write_sample(s).expect("write sample");
        }
        w.finalize().expect("finalize wav");
    }

    fn minimal_engine(dir: &std::path::Path) -> SampleEngine {
        let wav = dir.join("note.wav");
        write_sine_wav(&wav, 48_000);
        let styx = "\
name TestZoneLib
zones (
    { file note.wav, key_min 0, key_max 127, root_key 60, vel_min 0, vel_max 127 }
)
";
        let spec_path = dir.join("lib.styx");
        std::fs::write(&spec_path, styx).expect("write styx");
        let patch = crate::PlayerPatch::load(&spec_path, dir).expect("load patch");
        let mut engine = SampleEngine::new(patch, 48_000, "", "");
        engine.preload_samples();
        engine
    }

    #[test]
    fn open_add_instrument_and_note_on_reaches_track() {
        if !audio_available() {
            eprintln!("skip: set SIGNAL_SAMPLER_RIG_AUDIO=1 with an audio backend to run");
            return;
        }
        let dir = std::env::temp_dir().join(format!("signal-sampler-rig-{}", std::process::id()));
        std::fs::create_dir_all(&dir).expect("mkdir");

        let prefs = AudioIoPrefs {
            sample_rate: 48_000,
            buffer_size: 256,
            ..Default::default()
        };
        let mut rig = SamplerRig::open(&prefs).expect("open rig");
        rig.add_instrument("piano".into(), minimal_engine(&dir))
            .expect("add instrument");
        assert_eq!(rig.instrument_count(), 1);
        assert!(rig.instrument("piano").is_some());

        // note_on must enqueue to the instrument's track (push returns true when
        // the live-MIDI ring accepted it — proves the producer is wired).
        let guid = rig.instrument("piano").unwrap().track_guid.clone();
        assert!(rig.daw.push_note_on(&guid, 60, 100), "note must enqueue");

        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn from_mixer_layout_builds_tracks_buses_and_sends() {
        if !audio_available() {
            eprintln!("skip: set SIGNAL_SAMPLER_RIG_AUDIO=1 with an audio backend to run");
            return;
        }
        let dir = std::env::temp_dir().join(format!("signal-sampler-mix-{}", std::process::id()));
        std::fs::create_dir_all(&dir).expect("mkdir");

        let layout = kit_layout();
        let prefs = AudioIoPrefs { sample_rate: 48_000, buffer_size: 256, ..Default::default() };

        // One engine per channel/send strip key.
        let mut engines: HashMap<(usize, String), SampleEngine> = HashMap::new();
        for eng in &layout.engines {
            for ch in &eng.channels {
                engines.insert((eng.engine_idx, ch.mic_label.clone()), minimal_engine(&dir));
            }
            for snd in &eng.sends {
                engines.insert((eng.engine_idx, snd.mic_label.clone()), minimal_engine(&dir));
            }
        }

        let rig = SamplerRig::from_mixer_layout(&prefs, &layout, engines).expect("build rig");

        // 4 instruments (2 close + 2 overhead) and 1 bus.
        assert_eq!(rig.instrument_count(), 4);
        assert_eq!(rig.buses().len(), 1);
        assert!(rig.buses().contains_key("Overhead"));

        // Each overhead instrument should have a send to the Overhead bus and
        // its parent send disabled (verified via the Routing service).
        let bus_guid = rig.buses()["Overhead"].track_guid.clone();
        for (id, t) in &rig.instruments {
            if id.ends_with(":Overhead") {
                let sends = <Standalone as Routing>::sends(
                    &rig.daw,
                    ProjectContext::Current,
                    TrackRef::guid(&t.track_guid),
                );
                assert!(
                    sends.iter().any(|s| s.dest_track_guid.as_deref() == Some(bus_guid.as_str())),
                    "overhead mic {id} must send to the Overhead bus"
                );
                assert!(
                    !<Standalone as Routing>::parent_send_enabled(
                        &rig.daw,
                        ProjectContext::Current,
                        TrackRef::guid(&t.track_guid)
                    ),
                    "overhead mic {id} parent send must be disabled"
                );
            }
        }

        let _ = std::fs::remove_dir_all(&dir);
    }
}
