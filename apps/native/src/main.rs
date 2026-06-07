//! Signal Native — standalone Dioxus-native (Blitz) player harness.
//!
//! A focused app for developing the Dioxus UI and exercising the
//! [`SamplerPlayer`]: browse the on-disk library, load a
//! preset/pack/engine/block, and play it via the on-screen piano or a live
//! MIDI controller — with a live voice / pending-event readout.
//!
//! Renders with **dioxus-native** (Blitz/Vello/wgpu). Iterate with hot-reload:
//!
//! ```sh
//! cd apps/native && dx serve --platform native
//! # or, no hot-reload:
//! cargo run -p signal-native
//! ```
//!
//! Inline styles only (Blitz doesn't load external CSS reliably); the shared
//! components are inline-styled too.

use std::path::PathBuf;
use std::time::Duration;

use dioxus::prelude::*;
use signal_audio::MidiInput;
use signal_sampler::{PreloadProfile, SamplerPlayer};
use signal_ui::components::Piano;

mod mixer_view;
use mixer_view::MixerPanel;

/// Single instrument slot — whatever we load plays under this id, and the
/// piano / MIDI route note events to it.
const INSTRUMENT_ID: &str = "native";

/// Loadable sampler file extensions.
const LOADABLE: &[&str] = &["signalpreset", "signalpack", "signalengine", "signalblock"];

/// Base reset so the Blitz root fills the window.
const BASE_CSS: &str = "html,body{margin:0;padding:0;height:100%;background:#111;}";

/// Library root to browse. Override with `SIGNAL_LIBRARY`.
fn library_root() -> PathBuf {
    std::env::var("SIGNAL_LIBRARY")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("/run/media/AudioHaven/Signal/Libraries"))
}

/// Newtype so the library root is unambiguous in Dioxus context.
#[derive(Clone)]
struct LibraryRoot(PathBuf);

fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new("info")),
        )
        .init();

    tracing::info!("Starting Signal Native (dioxus-native / Blitz)");
    dioxus::launch(App);
}

#[component]
fn App() -> Element {
    // Initialise shared state once and publish it to children. The
    // SamplerPlayer owns the native cpal output stream; MIDI's connection
    // handle lives here for the app lifetime (cloning it clones only the
    // receiver).
    use_context_provider(|| {
        let player = SamplerPlayer::new().expect("init SamplerPlayer (cpal output)");
        // DrumKit: loads ALL samples (no cap — unlike FastAudition/Performance,
        // which truncate and leave velocity layers / RR / mics missing), ordered
        // kick/snare-first so the kit is playable quickly while the rest stream
        // in. Background-loaded, so the UI stays responsive throughout.
        player.set_preload_profile(PreloadProfile::DrumKit);
        player
    });
    use_context_provider(|| LibraryRoot(library_root()));
    use_context_provider(|| {
        let midi = MidiInput::open_all();
        match &midi {
            Some(_) => tracing::info!("MIDI: input active"),
            None => tracing::info!("MIDI: no input ports found"),
        }
        midi // Option<MidiInput>
    });

    rsx! {
        document::Style { {BASE_CSS} }
        PlayerPanel {}
    }
}

#[component]
fn PlayerPanel() -> Element {
    let sampler = use_context::<SamplerPlayer>();
    let midi_opt = use_context::<Option<MidiInput>>();
    let root = use_context::<LibraryRoot>().0;

    let mut current = use_signal(|| root.clone());
    let mut loaded = use_signal(|| Option::<String>::None);
    let mut status = use_signal(|| "Pick a preset / pack / engine to load".to_string());
    let mut active_notes: Signal<Vec<u8>> = use_signal(Vec::new);
    let mut voices = use_signal(|| 0usize);
    let mut pending = use_signal(|| 0usize);
    let mut loading = use_signal(|| false);
    let mut favorites = use_signal(default_favorites);
    // Bumped after each successful load so the mixer panel re-fetches its
    // layout; `show_mixer` toggles the stage between the mixer and the splash.
    let mut reload = use_signal(|| 0u64);
    let mut show_mixer = use_signal(|| true);
    // Background-load result slot: the worker thread deposits `(name, result)`
    // here; the poll future below picks it up and updates the UI. Keeps the
    // heavy load_preset (spec parse + engine build) OFF the dx event loop.
    let load_slot: LoadSlot = use_hook(|| std::sync::Arc::new(std::sync::Mutex::new(None)));

    // Poll the background-load slot.
    {
        let slot = load_slot.clone();
        use_future(move || {
            let slot = slot.clone();
            async move {
                loop {
                    futures_timer::Delay::new(Duration::from_millis(120)).await;
                    let done = slot.lock().ok().and_then(|mut g| g.take());
                    if let Some((name, res)) = done {
                        loading.set(false);
                        match res {
                            Ok(()) => {
                                loaded.set(Some(name.clone()));
                                status.set(format!("Loaded {name}"));
                                reload.set(reload() + 1);
                            }
                            Err(e) => status.set(format!("Load failed: {e}")),
                        }
                    }
                }
            }
        });
    }

    // Live readout.
    {
        let sampler = sampler.clone();
        use_future(move || {
            let sampler = sampler.clone();
            async move {
                loop {
                    futures_timer::Delay::new(Duration::from_millis(150)).await;
                    voices.set(sampler.active_voices(INSTRUMENT_ID));
                    pending.set(sampler.audio_stats().pending_events);
                }
            }
        });
    }

    // Live MIDI → player + piano highlight.
    {
        let sampler = sampler.clone();
        let midi = midi_opt.clone();
        use_future(move || {
            let sampler = sampler.clone();
            let midi = midi.clone();
            async move {
                loop {
                    futures_timer::Delay::new(Duration::from_millis(8)).await;
                    let Some(ref midi) = midi else { continue };
                    for ev in midi.drain() {
                        if ev.is_note_on() {
                            sampler.note_on(INSTRUMENT_ID, ev.note, ev.velocity);
                            let mut an = active_notes.write();
                            if !an.contains(&ev.note) {
                                an.push(ev.note);
                            }
                        } else if ev.is_note_off() {
                            sampler.note_off(INSTRUMENT_ID, ev.note);
                            active_notes.write().retain(|&n| n != ev.note);
                        } else if ev.is_cc() {
                            // Forward control changes (e.g. CC64 sustain pedal)
                            // so the engine's damper / pedal logic runs.
                            sampler.cc(INSTRUMENT_ID, ev.controller(), ev.cc_value());
                        }
                    }
                }
            }
        });
    }

    // Directory listing for the current folder.
    let cur = current.read().clone();
    let (mut dirs, mut files): (Vec<PathBuf>, Vec<PathBuf>) = (Vec::new(), Vec::new());
    if let Ok(rd) = std::fs::read_dir(&cur) {
        for entry in rd.flatten() {
            let p = entry.path();
            if p.is_dir() {
                dirs.push(p);
            } else if p
                .extension()
                .and_then(|x| x.to_str())
                .is_some_and(|x| LOADABLE.contains(&x))
            {
                files.push(p);
            }
        }
    }
    dirs.sort();
    files.sort();
    let at_root = cur == root;

    rsx! {
        div {
            style: "display:flex; flex-direction:column; width:100vw; height:100vh; \
                    background:#111; color:#d4d4d4; font-family:system-ui,sans-serif; font-size:13px;",

            // Header / status bar
            div {
                style: "display:flex; gap:16px; align-items:center; padding:8px 12px; \
                        background:#1c1c1e; border-bottom:1px solid #3a3a3c;",
                strong { style: "color:#e8813a;", "Signal Native — Player" }
                span { "{status}" }
                button {
                    style: if show_mixer() {
                        "margin-left:auto; padding:4px 12px; background:#e8813a; color:#111; \
                         border:none; border-radius:4px; cursor:pointer; font-weight:600;"
                    } else {
                        "margin-left:auto; padding:4px 12px; background:#2a2a2e; color:#ddd; \
                         border:1px solid #3a3a3c; border-radius:4px; cursor:pointer;"
                    },
                    onclick: move |_| { let v = !show_mixer(); show_mixer.set(v); },
                    "Mixer"
                }
                span { style: "margin-left:12px; color:#777;", "voices: {voices} · pending: {pending}" }
            }

            // Favorites — quick-load (background, off the UI thread).
            div {
                style: "display:flex; gap:8px; align-items:center; padding:6px 12px; \
                        background:#161618; border-bottom:1px solid #2a2a2c; overflow-x:auto;",
                span { style: "color:#777; font-size:11px;", "★ FAVORITES" }
                {favorites.read().clone().into_iter().map(|(label, path)| {
                    let sampler = sampler.clone();
                    let slot = load_slot.clone();
                    rsx! {
                        button {
                            key: "{path.display()}",
                            style: "padding:4px 10px; background:#2a2a2e; color:#e8d6c0; \
                                    border:1px solid #3a3a3c; border-radius:4px; cursor:pointer; white-space:nowrap;",
                            onclick: move |_| start_load(sampler.clone(), slot.clone(), loading, status, path.clone()),
                            "{label}"
                        }
                    }
                })}
                if loading() {
                    span { style: "color:#e8813a;", "loading…" }
                }
            }

            // Body: browser | stage
            div { style: "display:flex; flex:1; min-height:0;",

                // Library browser
                div {
                    style: "width:340px; overflow:auto; border-right:1px solid #3a3a3c; background:#161616;",
                    div {
                        style: "padding:6px 10px; color:#777; font-size:11px; \
                                border-bottom:1px solid #2a2a2c; word-break:break-all;",
                        "{cur.display()}"
                    }
                    if !at_root {
                        div {
                            style: "padding:5px 12px; cursor:pointer; color:#9aa3ad;",
                            onclick: move |_| {
                                let c = current.read().clone();
                                if let Some(p) = c.parent() { current.set(p.to_path_buf()); }
                            },
                            "⬑  .."
                        }
                    }
                    {dirs.into_iter().map(|d| {
                        let name = d.file_name().and_then(|n| n.to_str()).unwrap_or("?").to_string();
                        rsx! {
                            div {
                                key: "{d.display()}",
                                style: "padding:5px 12px; cursor:pointer;",
                                onclick: move |_| current.set(d.clone()),
                                "📁  {name}"
                            }
                        }
                    })}
                    {files.into_iter().map(|f| {
                        let name = f.file_name().and_then(|n| n.to_str()).unwrap_or("?").to_string();
                        let sampler = sampler.clone();
                        let slot = load_slot.clone();
                        rsx! {
                            div {
                                key: "{f.display()}",
                                style: "display:flex; align-items:center; gap:6px; padding:5px 12px;",
                                span {
                                    style: "flex:1; cursor:pointer; color:#cfe3f0;",
                                    onclick: {
                                        let sampler = sampler.clone();
                                        let slot = slot.clone();
                                        let p = f.clone();
                                        move |_| start_load(sampler.clone(), slot.clone(), loading, status, p.clone())
                                    },
                                    "♪  {name}"
                                }
                                button {
                                    style: "background:none; border:none; color:#6a6a72; cursor:pointer; font-size:14px;",
                                    title: "Add to favorites",
                                    onclick: {
                                        let p = f.clone();
                                        let label = name.clone();
                                        move |_| {
                                            let p = p.clone();
                                            let mut favs = favorites.write();
                                            if !favs.iter().any(|(_, fp)| *fp == p) {
                                                favs.push((label.clone(), p));
                                            }
                                        }
                                    },
                                    "★"
                                }
                            }
                        }
                    })}
                }

                // Stage: loaded instrument + piano
                div { style: "display:flex; flex:1; flex-direction:column; min-width:0;",
                    if show_mixer() {
                        div { style: "flex:1; min-height:0;",
                            MixerPanel { reload: reload() }
                        }
                    } else {
                        div {
                            style: "flex:1; display:flex; align-items:center; justify-content:center; color:#555;",
                            if let Some(n) = loaded.read().clone() {
                                div { style: "text-align:center;",
                                    div { style: "font-size:18px; color:#e8813a;", "{n}" }
                                    div { style: "margin-top:6px; color:#777;",
                                        "Play with the piano below or a MIDI controller" }
                                }
                            } else {
                                "No instrument loaded"
                            }
                        }
                    }
                    div {
                        style: "border-top:1px solid #3a3a3c; background:#1a1a1a; padding:8px;",
                        Piano {
                            start_note: 21,
                            end_note: 96,
                            active_notes: active_notes.read().clone(),
                            accent_color: "#e8813a".to_string(),
                            height: "200px".to_string(),
                            on_note_on: {
                                let sampler = sampler.clone();
                                move |n: u8| {
                                    sampler.note_on(INSTRUMENT_ID, n, 100);
                                    let mut an = active_notes.write();
                                    if !an.contains(&n) { an.push(n); }
                                }
                            },
                            on_note_off: {
                                let sampler = sampler.clone();
                                move |n: u8| {
                                    sampler.note_off(INSTRUMENT_ID, n);
                                    active_notes.write().retain(|&x| x != n);
                                }
                            },
                        }
                    }
                }
            }
        }
    }
}

// ── Background load + favorites ──────────────────────────────────────────────

/// Slot the background-load worker deposits its result into; the UI poll
/// future picks it up. `(display name, Ok / error string)`.
type LoadSlot = std::sync::Arc<std::sync::Mutex<Option<(String, Result<(), String>)>>>;

/// Seed favorites — a drum kit + the Keyscape Rhodes for quick testing.
/// Only entries whose file exists are kept. Override the root via SIGNAL_LIBRARY.
fn default_favorites() -> Vec<(String, PathBuf)> {
    let root = library_root();
    [
        (
            "🥁 MM2 Metal Monster",
            root.join("Drum Kits/GGD Modern and Massive 2/Presets/Metal Monster.signalpreset"),
        ),
        (
            "🎹 Keyscape Rhodes",
            root.join("Keys/Keyscape/Packs/Rhodes - Classic.signalpack"),
        ),
    ]
    .into_iter()
    .filter(|(_, p)| p.exists())
    .map(|(l, p)| (l.to_string(), p))
    .collect()
}

/// Kick off a load on a background thread so the heavy `load_preset` (spec
/// parse + engine build) never blocks the dx event loop. Sets the loading
/// state immediately; the worker deposits the result into `slot` for the poll
/// future to apply. `loading`/`status` are Copy signals (called from the UI).
fn start_load(
    sampler: SamplerPlayer,
    slot: LoadSlot,
    mut loading: Signal<bool>,
    mut status: Signal<String>,
    path: PathBuf,
) {
    let name = path
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("?")
        .to_string();
    loading.set(true);
    status.set(format!("Loading {name}…"));
    std::thread::Builder::new()
        .name("signal-native-load".into())
        .spawn(move || {
            sampler.unload_instrument(INSTRUMENT_ID);
            let ext = path.extension().and_then(|x| x.to_str()).unwrap_or("");
            let res = match ext {
                "signalpreset" => sampler.load_preset(INSTRUMENT_ID, &path).map(|_| ()),
                "signalpack" => sampler.load_pack(INSTRUMENT_ID, &path),
                "signalengine" => sampler.load_engine(INSTRUMENT_ID, &path),
                "signalblock" => sampler.load_block(INSTRUMENT_ID, &path),
                _ => Ok(()),
            }
            .map_err(|e| e.to_string());
            if let Ok(mut g) = slot.lock() {
                *g = Some((name, res));
            }
        })
        .ok();
}
