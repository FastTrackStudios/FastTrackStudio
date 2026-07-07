//! Signal Desktop — standalone Dioxus desktop app for signal chain management.
//!
//! Two rendering modes, selected at compile time:
//!
//! **Dev** (default): `dioxus::LaunchBuilder::desktop` (WebKit/WRY)
//! — enables `dx serve` hot-reload for rapid RSX/Tailwind prototyping.
//! `dev` is the default feature so plain `dx serve` works without flags.
//!
//! **Production** (`--no-default-features`): `nice_plug_dioxus::open_standalone_with_state`
//! (Blitz/Vello/wgpu renderer) — same pipeline as `fts-signal-plugin`
//! (VST3/CLAP). UI components are guaranteed to render identically in both
//! standalone and plugin contexts. Nix release builds pass `--no-default-features`
//! automatically.

use dioxus::prelude::*;

use std::sync::{Arc, Mutex};

use midicore::midir::MidiStream;
use signal::{Signal, connect_db_seeded};
use signal::BlockType;
use signal_sampler::rig_profile::RigStack;
use signal_sampler::{
    DeviceInfo, GuitarRig, ProfileRig, RigBlock, RigManager, RigPatch, RigProfile,
};
#[cfg(not(feature = "dev"))]
use signal_sampler::SamplerRig;
#[cfg(not(feature = "dev"))]
use signal_ui::ProcessingChain;
use signal_ui::{
    AudioDevice, AudioPrefs, AudioSettingsBridge, GuitarRigView, LiveBlock, PerfStack,
    PerformanceModel, RigAudioHandle,
};

/// Shared live rig (a [`ProfileRig`] wrapping the [`GuitarRig`]). Auto-opened at
/// launch and held for the session.
type SharedRig = Arc<Mutex<Option<ProfileRig>>>;

/// Live rig state, cloneable into UI callbacks.
#[derive(Clone)]
struct RigState {
    rig: SharedRig,
    /// Volume-boost engaged (adds output trim on top of the active patch).
    boost: Arc<Mutex<bool>>,
    /// The active patch's live FX chain, mirrored for the UI. Rebuilt whenever
    /// the active patch changes (the rig has no per-block bypass/param getters).
    blocks: Arc<Mutex<Vec<LiveBlock>>>,
}

impl RigState {
    fn new() -> Self {
        Self {
            rig: Arc::new(Mutex::new(None)),
            boost: Arc::new(Mutex::new(false)),
            blocks: Arc::new(Mutex::new(Vec::new())),
        }
    }
}

/// The primary dialable param for a block type: `(name, min, max, default)`.
fn primary_param(bt: BlockType) -> Option<(&'static str, f32, f32, f32)> {
    match bt {
        BlockType::Reverb | BlockType::Delay => Some(("mix", 0.0, 0.10, 0.08)),
        BlockType::Chorus | BlockType::Flanger | BlockType::Vibrato => Some(("mix", 0.0, 1.0, 0.4)),
        BlockType::Trem => Some(("depth", 0.0, 1.0, 0.5)),
        _ => None,
    }
}

/// Rebuild the UI's mirror of the active patch's FX chain and (re-)apply each
/// block's initial bypass to the engine (activation re-enables all slots, so the
/// off-by-default blocks must be re-bypassed here).
fn resync_blocks(state: &RigState) {
    let mut out = Vec::new();
    if let Ok(guard) = state.rig.lock() {
        if let Some(prig) = guard.as_ref() {
            if let Some(patch) = prig.active_patch() {
                let ids = prig.active_block_ids();
                let reals: Vec<&RigBlock> =
                    patch.chain.iter().filter(|b| b.has_backend()).collect();
                for (block, id) in reals.iter().zip(ids.iter()) {
                    if block.bypassed {
                        prig.rig().set_block_slot_bypass(id, true);
                    }
                    let (param_name, param_min, param_max, param_value) =
                        match primary_param(block.block_type) {
                            Some((n, mn, mx, dflt)) => {
                                (Some(n.to_string()), mn, mx, block.param_f32(n).unwrap_or(dflt))
                            }
                            None => (None, 0.0, 0.0, 0.0),
                        };
                    let name = if block.name.trim().is_empty() {
                        format!("{:?}", block.block_type)
                    } else {
                        block.name.clone()
                    };
                    out.push(LiveBlock {
                        id: id.clone(),
                        block_type: block.block_type,
                        name,
                        bypassed: block.bypassed,
                        param_name,
                        param_value,
                        param_min,
                        param_max,
                    });
                }
            }
        }
    }
    *state.blocks.lock().unwrap() = out;
}

/// Extra output gain (dB) applied when the volume boost is engaged.
const BOOST_DB: f32 = 6.0;

/// Re-apply the output trim = active patch base trim (+ boost if engaged). Call
/// after any patch activation, since `activate` resets the trim per patch.
fn apply_boost(prig: &ProfileRig, boost: bool) {
    let base = prig.active_patch().map(|p| p.output_trim_db).unwrap_or(0.0);
    prig.rig()
        .set_output_trim_db(base + if boost { BOOST_DB } else { 0.0 });
}

// ── Worship profile (hardcoded, one NAM per patch from ~/Downloads) ──────────

const FENDER_DIR: &str =
    "/home/cody/Downloads/Fender Deluxe Reverb '65 Reissue _ Clean _ SM57 + Royer R-121 + Room";
const CUSTOM_DIR: &str = "/home/cody/Downloads/Fender Style Custom Patches Made with Custom IR";
const AC30_MODEL: &str =
    "/home/cody/Downloads/1965 VOX AC30 Top Boost/'65 AC30_6 - The Iconic Cleanish.nam";

/// A bypassed (off-by-default) native FX block of the given type.
fn off(block_type: BlockType, name: &str) -> RigBlock {
    let mut b = RigBlock::of_type(block_type).named(name);
    b.bypassed = true;
    b
}

/// An active native FX block with build-time params (`(name, value)`).
fn on_fx(block_type: BlockType, name: &str, params: &[(&str, &str)]) -> RigBlock {
    let mut b = RigBlock::of_type(block_type).named(name);
    for (k, v) in params {
        b = b.with_param(*k, *v);
    }
    b
}

/// A bypassed native FX block carrying params (so it's ready when un-bypassed).
fn off_fx(block_type: BlockType, name: &str, params: &[(&str, &str)]) -> RigBlock {
    let mut b = on_fx(block_type, name, params);
    b.bypassed = true;
    b
}

/// The Worship guitar profile: 5 footswitch folders (stacks), each an ordered
/// rotation of patches, every patch a distinct NAM amp for now. Patch names are
/// globally unique (the backend addresses patches by name) — the UI strips the
/// folder prefix for display ("Clean Verb" → "Verb", the folder default → its
/// own name shows as "Default").
fn worship_profile() -> RigProfile {
    // Each patch is a full built-in-FX chain (all native, from features/fx/):
    //   Comp → NAM amp → [Modulation module: all off] → [Time module]
    // Time module: DLY 1 + Reverb 1 are subtle and ON; DLY 2 + Reverb 2 are
    // more extreme and BYPASSED (ready to kick in). The live rig chain is serial,
    // so this is the serial form of the template's parallel Time splits — with
    // only lane 1 active it's equivalent; true parallel routing is a later rig
    // feature. Delay/Reverb are Time-category, so the FX-Toggle footswitch
    // bypasses them.
    // Block names match the guitar-rig-template slot names so the UI grid
    // resolves each into its template slot (Dynamics→Compressor, Amp→Amp L,
    // Modulation→Chorus/Flanger/Phaser, Motion→Tremolo/Vibrato/Rotary,
    // Time→DLY 1/DLY 2/VERB 1/VERB 2).
    let amp = |name: &str, path: String| {
        RigPatch::new(name)
            .with_block(RigBlock::of_type(BlockType::Compressor).named("Compressor"))
            .with_block(RigBlock::nam(path).named("Amp L"))
            // Modulation + Motion modules — all off by default.
            .with_block(off(BlockType::Chorus, "Chorus"))
            .with_block(off(BlockType::Flanger, "Flanger"))
            .with_block(off(BlockType::Phaser, "Phaser"))
            .with_block(off(BlockType::Trem, "Tremolo"))
            .with_block(off(BlockType::Vibrato, "Vibrato"))
            .with_block(off(BlockType::Rotary, "Rotary"))
            // Time module — subtle pair on, extreme pair bypassed.
            .with_block(on_fx(BlockType::Delay, "DLY 1", &[("mix", "0.08"), ("time", "350"), ("feedback", "0.28")]))
            .with_block(off_fx(BlockType::Delay, "DLY 2", &[("mix", "0.10"), ("time", "600"), ("feedback", "0.62")]))
            .with_block(on_fx(BlockType::Reverb, "VERB 1", &[("mix", "0.08"), ("decay", "0.42"), ("size", "0.45")]))
            .with_block(off_fx(BlockType::Reverb, "VERB 2", &[("mix", "0.10"), ("decay", "0.85"), ("size", "0.92")]))
    };
    RigProfile::new("Worship")
        // Clean
        .with_patch(amp("Clean", format!("{FENDER_DIR}/Fender DRRI _ Clean _ SM57 + Royer R-121 + Room _ Full Rig.nam")))
        .with_patch(amp("Clean Dry", format!("{FENDER_DIR}/Fender DRRI _ Clean _ DI Capture (No Cab).nam")))
        .with_patch(amp("Clean Verb", format!("{FENDER_DIR}/Fender DRRI _ Clean _ Room Only _ Full Rig.nam")))
        // Crunch
        .with_patch(amp("Crunch", format!("{CUSTOM_DIR}/Vibrato Verb AA Crunch.nam")))
        .with_patch(amp("Crunch Edge", format!("{CUSTOM_DIR}/Vibrato Verb AA More.nam")))
        // Drive
        .with_patch(amp("Drive", format!("{CUSTOM_DIR}/Vibrato Verb AA Driven.nam")))
        .with_patch(amp("Drive Edge", format!("{CUSTOM_DIR}/Vibrato Verb AA Cranked.nam")))
        // Lead
        .with_patch(amp("Lead", format!("{CUSTOM_DIR}/Vib Arena Lead LT.nam")))
        .with_patch(amp("Lead POG", format!("{CUSTOM_DIR}/Vibrato Verb Octaver dist.nam")))
        // Ambient
        .with_patch(amp("Ambient", AC30_MODEL.to_string()))
        .with_patch(amp("Ambient Swells", format!("{CUSTOM_DIR}/Vibrato Verb SRV.nam")))
        .with_patch(amp("Ambient Delay Craze", format!("{CUSTOM_DIR}/Vibrato Spaghetti W.nam")))
        // Footswitch stacks (folders)
        .with_stack(RigStack::new("Clean", ["Clean", "Clean Dry", "Clean Verb"]))
        .with_stack(RigStack::new("Crunch", ["Crunch", "Crunch Edge"]))
        .with_stack(RigStack::new("Drive", ["Drive", "Drive Edge"]))
        .with_stack(RigStack::new("Lead", ["Lead", "Lead POG"]))
        .with_stack(RigStack::new("Ambient", ["Ambient", "Ambient Swells", "Ambient Delay Craze"]))
}

/// Folder-relative display label for a patch: the folder default shows as
/// "Default"; others drop the folder prefix ("Clean Verb" → "Verb").
fn patch_display(stack_name: &str, patch_name: &str) -> String {
    if patch_name.eq_ignore_ascii_case(stack_name) {
        "Default".to_string()
    } else {
        patch_name
            .strip_prefix(&format!("{stack_name} "))
            .unwrap_or(patch_name)
            .to_string()
    }
}

/// Snapshot the performance model (folders + live cursor/active state).
fn build_perf_model(prig: &ProfileRig) -> PerformanceModel {
    let active_stack = prig.active_stack();
    let patches = prig.patches();
    let stacks = prig
        .stacks()
        .iter()
        .enumerate()
        .map(|(si, st)| {
            let len = st.patches.len().max(1);
            let pos = prig.stack_position(si) % len;
            let cur = st.patches.get(pos).cloned().unwrap_or_default();
            let available = patches
                .iter()
                .position(|p| p.name.eq_ignore_ascii_case(&cur))
                .map(|i| prig.is_patch_available(i))
                .unwrap_or(false);
            PerfStack {
                name: st.name.clone(),
                current_patch: patch_display(&st.name, &cur),
                position: pos,
                patch_count: st.patches.len(),
                available,
                is_active: active_stack == Some(si),
            }
        })
        .collect();
    PerformanceModel {
        profile_name: prig.profile_name().unwrap_or_default().to_string(),
        stacks,
        fx_bypass: prig.fx_bypass(),
        boost: false, // filled in by the perf_model callback (state lives outside prig)
        tempo_bpm: 120,
    }
}

/// Open (or re-open) the live rig, then load the Worship profile (which
/// pre-installs every patch's chain for gapless footswitch switching).
///
/// Drops any existing rig *first* so the audio device is released before we
/// re-acquire it — otherwise the re-open races the teardown into "device busy".
fn open_rig_into(state: &RigState) {
    let rig = &state.rig;
    let had_previous = if let Ok(mut g) = rig.lock() {
        g.take().is_some()
    } else {
        false
    };
    if had_previous {
        std::thread::sleep(std::time::Duration::from_millis(250));
    }

    let mut mgr = RigManager::load(AUDIO_RIG_NAME);
    // Monitor + play through a single duplex interface.
    if mgr.audio.output_device.is_empty() && !mgr.audio.input_device.is_empty() {
        mgr.audio.output_device = mgr.audio.input_device.clone();
        let _ = mgr.save();
    }

    match GuitarRig::open(&mgr.audio) {
        Ok(g) => {
            tracing::info!(
                "rig live: in {} ch{} → out {}",
                if mgr.audio.input_device.is_empty() { "default" } else { &mgr.audio.input_device },
                mgr.audio.input_channel + 1,
                if mgr.audio.output_device.is_empty() { "default" } else { &mgr.audio.output_device },
            );
            let mut prig = ProfileRig::new(g);
            match prig.load_profile(worship_profile(), None) {
                Ok(()) => tracing::info!("worship profile loaded ({} patches)", prig.patches().len()),
                Err(e) => tracing::error!("profile load failed: {e}"),
            }
            if let Ok(mut slot) = rig.lock() {
                *slot = Some(prig);
            }
        }
        Err(e) => tracing::error!("rig open failed: {e:#}"),
    }
    // Mirror the (now active) patch's FX chain + apply bypass defaults.
    resync_blocks(state);
}

/// Build the [`RigAudioHandle`] the UI uses to read meters, drive footswitch
/// stacks, and re-open the rig.
fn make_rig_handle(state: RigState) -> RigAudioHandle {
    RigAudioHandle {
        start: Callback::new({
            let state = state.clone();
            move |_| {
                let state = state.clone();
                std::thread::spawn(move || open_rig_into(&state));
            }
        }),
        stop: Callback::new({
            let state = state.clone();
            move |_| {
                *state.rig.lock().unwrap() = None;
                tracing::info!("rig stopped");
            }
        }),
        is_running: Callback::new({
            let state = state.clone();
            move |_| state.rig.lock().map(|g| g.is_some()).unwrap_or(false)
        }),
        input_peak: Callback::new({
            let state = state.clone();
            move |_| {
                state.rig.lock()
                    .ok()
                    .and_then(|g| g.as_ref().map(|r| r.rig().input_peak()))
                    .unwrap_or(0.0)
            }
        }),
        output_peak: Callback::new({
            let state = state.clone();
            move |_| {
                state.rig.lock()
                    .ok()
                    .and_then(|g| g.as_ref().map(|r| r.rig().output_peak()))
                    .unwrap_or(0.0)
            }
        }),
        loaded_amp: Callback::new({
            let state = state.clone();
            move |_| {
                state.rig.lock().ok().and_then(|g| {
                    g.as_ref()
                        .and_then(|r| r.active_patch().map(|p| p.name.clone()))
                })
            }
        }),
        perf_model: Callback::new({
            let state = state.clone();
            move |_| {
                let mut m = state
                    .rig
                    .lock()
                    .ok()
                    .and_then(|g| g.as_ref().map(build_perf_model))
                    .unwrap_or_default();
                m.boost = *state.boost.lock().unwrap();
                m
            }
        }),
        press_stack: Callback::new({
            let state = state.clone();
            move |idx: usize| {
                let boost = *state.boost.lock().unwrap();
                if let Ok(mut guard) = state.rig.lock() {
                    if let Some(prig) = guard.as_mut() {
                        prig.activate_stack(idx);
                        apply_boost(prig, boost);
                    }
                }
                // Rebuild the block mirror + re-apply bypass defaults (drop the
                // rig lock first — resync_blocks re-locks it).
                resync_blocks(&state);
            }
        }),
        toggle_fx: Callback::new({
            let state = state.clone();
            move |_| {
                if let Ok(mut guard) = state.rig.lock() {
                    if let Some(prig) = guard.as_mut() {
                        let on = prig.toggle_fx_bypass();
                        tracing::info!("FX bypass: {}", if on { "ON" } else { "off" });
                    }
                }
            }
        }),
        toggle_boost: Callback::new({
            let state = state.clone();
            move |_| {
                let boost = {
                    let mut b = state.boost.lock().unwrap();
                    *b = !*b;
                    *b
                };
                if let Ok(guard) = state.rig.lock() {
                    if let Some(prig) = guard.as_ref() {
                        apply_boost(prig, boost);
                    }
                }
                tracing::info!("volume boost: {}", if boost { "ON" } else { "off" });
            }
        }),
        tap_tempo: Callback::new({
            move |_| tracing::info!("tap tempo (not yet wired to a delay block)")
        }),
        active_chain: Callback::new({
            let state = state.clone();
            move |_| state.blocks.lock().map(|b| b.clone()).unwrap_or_default()
        }),
        toggle_block_bypass: Callback::new({
            let state = state.clone();
            move |id: String| {
                let new_bypass = {
                    let mut blocks = state.blocks.lock().unwrap();
                    blocks.iter_mut().find(|b| b.id == id).map(|b| {
                        b.bypassed = !b.bypassed;
                        b.bypassed
                    })
                };
                if let Some(byp) = new_bypass {
                    if let Ok(guard) = state.rig.lock() {
                        if let Some(prig) = guard.as_ref() {
                            prig.rig().set_block_slot_bypass(&id, byp);
                        }
                    }
                }
            }
        }),
        set_block_param: Callback::new({
            let state = state;
            move |(id, name, value): (String, String, f32)| {
                if let Ok(mut blocks) = state.blocks.lock() {
                    if let Some(b) = blocks.iter_mut().find(|b| b.id == id) {
                        b.param_value = value;
                    }
                }
                if let Ok(guard) = state.rig.lock() {
                    if let Some(prig) = guard.as_ref() {
                        prig.rig().set_active_block_param(&id, &name, value);
                    }
                }
            }
        }),
    }
}

/// Rig whose audio prefs the settings page reads/writes (persisted to
/// `<config>/signal/rigs/guitar-rig.styx` by `RigManager`).
const AUDIO_RIG_NAME: &str = "Guitar Rig";

/// Enumerate audio devices (via daw/cpal) and load the persisted prefs.
fn audio_devices_and_prefs() -> (Vec<AudioDevice>, Vec<AudioDevice>, AudioPrefs) {
    fn map_device(d: DeviceInfo) -> AudioDevice {
        AudioDevice {
            name: d.name,
            channels: d.channels,
            default_sample_rate: d.default_sample_rate,
        }
    }
    let inputs = GuitarRig::input_devices().into_iter().map(map_device).collect();
    let outputs = GuitarRig::output_devices().into_iter().map(map_device).collect();
    let mgr = RigManager::load(AUDIO_RIG_NAME);
    let a = &mgr.audio;
    let prefs = AudioPrefs {
        input_device: a.input_device.clone(),
        input_channel: a.input_channel,
        output_device: a.output_device.clone(),
        sample_rate: a.sample_rate,
        buffer_size: a.buffer_size,
    };
    (inputs, outputs, prefs)
}

/// Persist edited audio prefs back to the rig's styx file (smart-saving, same
/// store the guitar TUI uses).
fn save_audio_prefs(p: AudioPrefs) {
    let mut mgr = RigManager::load(AUDIO_RIG_NAME);
    mgr.audio.input_device = p.input_device;
    mgr.audio.input_channel = p.input_channel;
    mgr.audio.output_device = p.output_device;
    mgr.audio.sample_rate = p.sample_rate;
    mgr.audio.buffer_size = p.buffer_size;
    if let Err(e) = mgr.save() {
        tracing::error!("failed to save audio prefs: {e}");
    }
}

/// Compiled Tailwind CSS for signal-ui components.
/// Embedded at compile time so Blitz can inject it into the document head
/// without an external file load (required for VST plugin compatibility).
const SIGNAL_CSS: &str = include_str!("../assets/tailwind.css");

/// Base document reset so the WebKit/WRY renderer fills the viewport.
/// Tailwind's preflight does not set `html, body { height: 100% }`, which
/// causes `h-full` on the root component div to have no effect (no parent
/// height to inherit from). This CSS runs before Tailwind to establish the
/// baseline. Blitz/wgpu doesn't use this — it manages layout directly.
#[cfg(feature = "dev")]
const BASE_CSS: &str = r#"
html, body {
    height: 100%;
    margin: 0;
    padding: 0;
    overflow: hidden;
    background: oklch(14.5% 0 0);
}
#main {
    height: 100%;
}
"#;

/// Default log filter: info-level globally, but suppress the Blitz/Dioxus
/// "Changing the props of Style {}" warning that fires on every re-render.
/// The CSS IS applied correctly — this warning is a Blitz limitation (style
/// elements can't be updated after mount), not a real error.
fn default_log_filter() -> tracing_subscriber::EnvFilter {
    tracing_subscriber::EnvFilter::try_from_default_env().unwrap_or_else(|_| {
        tracing_subscriber::EnvFilter::new("info,dioxus_document::elements=error")
    })
}

fn db_path() -> String {
    std::env::var("SIGNAL_DB").unwrap_or_else(|_| {
        let home = std::env::var("HOME").unwrap_or_else(|_| ".".into());
        let dir = std::path::PathBuf::from(home).join(".local/share/signal");
        std::fs::create_dir_all(&dir).ok();
        dir.join("signal.db").to_string_lossy().into_owned()
    })
}

// ── Production entry point (Blitz/wgpu — same renderer as VST plugin) ────────

/// Combined application state passed through the Blitz renderer's SharedState.
#[cfg(not(feature = "dev"))]
#[derive(Clone)]
struct AppState {
    signal: Signal,
    chain: ProcessingChain,
    sampler: SamplerRig,
}

#[cfg(not(feature = "dev"))]
fn main() {
    use nice_plug_dioxus::SharedState;
    use std::sync::Arc;

    tracing_subscriber::fmt()
        .with_env_filter(default_log_filter())
        .init();

    tracing::info!("Starting Signal Desktop (Blitz renderer)");

    // Initialise Signal before handing control to the Blitz event loop.
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .expect("tokio runtime");

    let controller: Signal = rt
        .block_on(connect_db_seeded(&db_path()))
        .expect("Failed to initialise Signal database");

    // Create the live audio processing chain (UI meter state).
    let chain = ProcessingChain::new(48000.0);

    // Create the sampler rig. It runs the SamplerBank on daw's AudioEngine; keep
    // the processing-chain engine disabled until both paths share one mixer.
    let sampler = SamplerRig::new().expect("Failed to initialise SamplerRig");

    let app_state = AppState {
        signal: controller,
        chain,
        sampler,
    };
    let shared = SharedState::new(Arc::new(app_state));

    // Blocks until the window is closed.
    nice_plug_dioxus::open_standalone_with_state(App, 1400, 900, Some(shared));
}

// ── Production root component ─────────────────────────────────────────────────

/// Root component for the production (Blitz) renderer.
///
/// Retrieves the [`Signal`] controller from Dioxus context (injected by
/// `open_standalone_with_state`) and delegates to [`SignalRoot`].
///
/// Intentionally prop-less so it can be used verbatim in
/// `create_dioxus_editor_with_state` when Signal runs as a VST3/CLAP plugin.
#[cfg(not(feature = "dev"))]
#[component]
fn App() -> Element {
    use nice_plug_dioxus::SharedState;

    let shared = use_context::<SharedState>();
    let state = shared
        .get::<AppState>()
        .expect("AppState not found in Dioxus context");

    let controller = state.signal.clone();
    let chain = state.chain.clone();
    let sampler = state.sampler.clone();

    // Provide shared state so child components can consume it via use_context().
    // `GuitarRigView` itself is self-contained, but we keep the controller +
    // audio contexts available for the live modules/plugins we build on top.
    provide_context(controller);
    use_context_provider(|| chain);
    use_context_provider(|| sampler);

    // Audio settings bridge: enumerate devices + load prefs once, wire persistence.
    let (inputs, outputs, prefs) = use_hook(audio_devices_and_prefs);
    let on_save = use_callback(move |p: AudioPrefs| save_audio_prefs(p));
    use_context_provider(|| AudioSettingsBridge {
        inputs: inputs.clone(),
        outputs: outputs.clone(),
        prefs: prefs.clone(),
        on_save,
    });

    // Live rig transport handle (meters + amp loading).
    let rig_state = use_hook(RigState::new);
    let rig_handle = use_hook(|| make_rig_handle(rig_state.clone()));
    use_context_provider(|| rig_handle.clone());

    // Auto-open the live rig once at launch (off the UI thread) so the engine is
    // always running — no Start click needed. Loads the AC30 amp on open.
    use_hook(|| {
        let state = rig_state.clone();
        std::thread::spawn(move || open_rig_into(&state));
    });

    rsx! {
        // AppStyles is prop-less/hook-less so Dioxus skips re-rendering it —
        // prevents the "Changing props of Style {}" warning at 60Hz.
        AppStyles {}
        // Static-RSX grounds: the default guitar rig template in the module/wire
        // grid. We build up from here toward a live rig.
        GuitarRigView {}
    }
}

// ── Dev entry point (WebKit/WRY — dx serve hot-reload) ───────────────────────

#[cfg(feature = "dev")]
fn main() {
    // WebKitGTK accelerated compositing breaks on Linux with NVIDIA/llvmpipe —
    // the GBM buffer allocation fails and the window stays white.
    // Disabling compositing mode forces software rendering.
    // See: https://github.com/NixOS/nixpkgs/issues/32580
    if std::env::var("WEBKIT_DISABLE_COMPOSITING_MODE").is_err() {
        // Safety: single-threaded at this point; no other threads have started.
        unsafe {
            std::env::set_var("WEBKIT_DISABLE_COMPOSITING_MODE", "1");
        }
    }

    tracing_subscriber::fmt()
        .with_env_filter(default_log_filter())
        .init();

    tracing::info!("Starting Signal Desktop (dev / WebKit renderer)");

    dioxus::LaunchBuilder::desktop()
        .with_cfg(
            dioxus::desktop::Config::default().with_window(
                dioxus::desktop::WindowBuilder::default()
                    .with_title("Signal (dev)")
                    .with_inner_size(dioxus::desktop::LogicalSize::new(1400.0, 900.0)),
            ),
        )
        .launch(DevApp);
}

/// Dev root: initialises Signal via an async resource so Tokio (managed by
/// WRY's event loop) handles the database I/O.
#[cfg(feature = "dev")]
#[component]
fn DevApp() -> Element {
    let controller = use_resource(|| async { connect_db_seeded(&db_path()).await });

    // Live rig transport handle — created unconditionally (stable across the
    // resource resolving) so the started rig survives re-renders.
    let rig_state = use_hook(RigState::new);

    // Auto-open the live rig once at launch (off the UI thread). Loads AC30.
    use_hook(|| {
        let state = rig_state.clone();
        std::thread::spawn(move || open_rig_into(&state));
    });

    // Clone owned values out of the reactive guard before building the Element
    // so the guard lifetime doesn't escape into the returned VNode.
    let state = controller
        .read()
        .as_ref()
        .map(|r| r.as_ref().map(Signal::clone).map_err(|e| e.to_string()));

    match state {
        None => rsx! {
            document::Style { {BASE_CSS} }
            div { style: "color: #e4e4e7; padding: 16px;", "Loading…" }
        },
        Some(Err(e)) => rsx! {
            document::Style { {BASE_CSS} }
            div { style: "color: #ef4444; padding: 16px;", "DB error: {e}" }
        },
        Some(Ok(ctrl)) => {
            // Open MIDI on first successful load (once per app lifetime).
            use_context_provider(|| {
                let midi = MidiStream::open_all();
                if midi.is_some() {
                    tracing::info!("MIDI: input active");
                } else {
                    tracing::info!("MIDI: no input ports found");
                }
                midi // provides Option<MidiInput>
            });
            provide_context(ctrl);
            // Audio settings bridge — same wiring as the production entry point.
            // `provide_context` / `Callback::new` are used (not the hook variants)
            // because this runs inside a conditional match arm.
            let (inputs, outputs, prefs) = audio_devices_and_prefs();
            provide_context(AudioSettingsBridge {
                inputs,
                outputs,
                prefs,
                on_save: Callback::new(save_audio_prefs),
            });
            provide_context(make_rig_handle(rig_state.clone()));
            rsx! {
                document::Style { {BASE_CSS} }
                AppStyles {}
                // Static-RSX grounds: the default guitar rig template rendered
                // in the module/wire grid. We build up from here.
                GuitarRigView {}
            }
        }
    }
}

// ── Shared helpers ────────────────────────────────────────────────────────────

/// Injects compiled Tailwind CSS into the document head.
///
/// Lives in its own component so it can be cheaply diffed. The Blitz renderer
/// does not support updating a `Style {}` element after mount — the warning
/// "Changing the props of Style {}" fires on every re-render of any ancestor
/// that contains this node. That warning is suppressed in the log filter
/// (see `default_log_filter`) because the CSS is applied correctly on the
/// first render and the subsequent update attempts are no-ops.
#[component]
fn AppStyles() -> Element {
    rsx! {
        document::Style { {SIGNAL_CSS} }
    }
}
