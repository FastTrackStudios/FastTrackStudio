//! Signal Amp — a standalone neural-amp modeler.
//!
//! Opens the live duplex audio (guitar in → out) via [`AmpEngine`], loads a
//! `.nam` model, and renders a minimal Blitz/Dioxus UI: the current amp, live
//! input/output meters, and a picker over the `.nam` models found in your
//! Downloads. Same renderer pipeline as the plugin/desktop app, so this UI
//! graduates to the iPhone (mobile) target unchanged.
//!
//! ```text
//! cargo run --release -p signal-amp --features signal-sampler/pipewire
//! ```

use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use dioxus::prelude::*;
use nice_plug_dioxus::SharedState;
use signal_sampler::{AmpEngine, RigAudioPrefs};

/// One selectable model in the picker.
#[derive(Clone, PartialEq)]
struct ModelEntry {
    path: String,
    name: String,
}

/// Shared app state handed to the Blitz renderer.
#[derive(Clone)]
struct AppState {
    amp: Arc<Mutex<AmpEngine>>,
    models: Vec<ModelEntry>,
    /// The model loaded at startup (so the UI can highlight it), if any.
    initial: Option<String>,
}

/// The VOX AC30 we default-load if present.
const DEFAULT_MODEL: &str =
    "/home/cody/Downloads/1965 VOX AC30 Top Boost/'65 AC30_6 - The Iconic Cleanish.nam";

/// Shallow-scan `~/Downloads` (depth ≤ 2) for `.nam` models.
fn scan_models() -> Vec<ModelEntry> {
    let home = std::env::var("HOME").unwrap_or_else(|_| ".".into());
    let root = PathBuf::from(home).join("Downloads");
    let mut out = Vec::new();
    let mut push = |p: &std::path::Path| {
        if p.extension().and_then(|s| s.to_str()) == Some("nam") {
            out.push(ModelEntry {
                path: p.to_string_lossy().into_owned(),
                name: p
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .unwrap_or("amp")
                    .to_string(),
            });
        }
    };
    if let Ok(top) = std::fs::read_dir(&root) {
        for e in top.flatten() {
            let p = e.path();
            if p.is_dir() {
                if let Ok(sub) = std::fs::read_dir(&p) {
                    for s in sub.flatten() {
                        push(&s.path());
                    }
                }
            } else {
                push(&p);
            }
        }
    }
    out.sort_by(|a, b| a.name.to_lowercase().cmp(&b.name.to_lowercase()));
    out.dedup_by(|a, b| a.path == b.path);
    out
}

fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new("info")),
        )
        .init();

    tracing::info!("Signal Amp starting — opening duplex audio…");

    let mut amp = match AmpEngine::open(&RigAudioPrefs::default()) {
        Ok(a) => a,
        Err(e) => {
            eprintln!("failed to open audio engine: {e}");
            std::process::exit(1);
        }
    };

    // Default-load the VOX AC30 if it's there.
    let mut initial = None;
    if std::path::Path::new(DEFAULT_MODEL).exists() {
        match amp.load_model(DEFAULT_MODEL) {
            Ok(()) => {
                tracing::info!("loaded default model: {}", amp.model_name());
                initial = Some(DEFAULT_MODEL.to_string());
            }
            Err(e) => tracing::warn!("could not load default model: {e}"),
        }
    }

    let state = AppState {
        amp: Arc::new(Mutex::new(amp)),
        models: scan_models(),
        initial,
    };

    let shared = SharedState::new(Arc::new(state));
    nice_plug_dioxus::open_standalone_with_state(App, 640, 480, Some(shared));
}

#[component]
fn App() -> Element {
    let shared = use_context::<SharedState>();
    let state = shared
        .get::<AppState>()
        .expect("AppState not in Dioxus context");
    let amp = state.amp.clone();
    let models = state.models.clone();

    // Reactive UI state.
    let model_name = use_signal(|| {
        amp.lock()
            .map(|a| a.model_name().to_string())
            .unwrap_or_else(|_| "—".into())
    });
    let selected = use_signal(|| state.initial.clone());
    let status = use_signal(String::new);
    let mut in_pk = use_signal(|| 0.0f32);
    let mut out_pk = use_signal(|| 0.0f32);
    let mut load_us = use_signal(|| 0u32);

    // Best-effort meter poll (~30 Hz). The audio runs independently of this;
    // if the timer doesn't tick in the renderer's executor the amp still works.
    {
        let amp = amp.clone();
        use_future(move || {
            let amp = amp.clone();
            async move {
                loop {
                    if let Ok(a) = amp.lock() {
                        in_pk.set(a.input_peak());
                        out_pk.set(a.output_peak());
                        load_us.set(a.dsp_load_us());
                    }
                    tokio::time::sleep(std::time::Duration::from_millis(33)).await;
                }
            }
        });
    }

    rsx! {
        div {
            style: "font-family: system-ui, sans-serif; background: #17171a; color: #e4e4e7; \
                    height: 100%; padding: 20px; display: flex; flex-direction: column; gap: 16px;",

            // Header
            div {
                div { style: "font-size: 13px; letter-spacing: 2px; color: #a1a1aa;", "SIGNAL AMP" }
                div { style: "font-size: 26px; font-weight: 700; color: #f4732a;", "{model_name}" }
            }

            // Meters
            Meter { label: "INPUT", value: in_pk() }
            Meter { label: "OUTPUT", value: out_pk() }
            div { style: "font-size: 12px; color: #71717a;", "DSP load: {load_us}µs / block" }

            if !status().is_empty() {
                div { style: "font-size: 12px; color: #eab308;", "{status}" }
            }

            // Model picker
            div { style: "font-size: 12px; color: #a1a1aa; margin-top: 4px;", "MODELS ({models.len()})" }
            div {
                style: "flex: 1; overflow-y: auto; display: flex; flex-direction: column; gap: 4px; \
                        border: 1px solid #27272a; border-radius: 6px; padding: 6px;",
                for entry in models.iter().cloned() {
                    {
                        let is_sel = selected().as_deref() == Some(entry.path.as_str());
                        let amp = amp.clone();
                        let path = entry.path.clone();
                        let name = entry.name.clone();
                        rsx! {
                            div {
                                style: format!(
                                    "padding: 6px 10px; border-radius: 4px; cursor: pointer; font-size: 13px; \
                                     background: {}; color: {};",
                                    if is_sel { "#f4732a" } else { "#1f1f23" },
                                    if is_sel { "#17171a" } else { "#d4d4d8" },
                                ),
                                onclick: move |_| {
                                    let amp = amp.clone();
                                    let path = path.clone();
                                    let name = name.clone();
                                    let mut model_name = model_name;
                                    let mut status = status;
                                    let mut selected = selected;
                                    spawn(async move {
                                        let res = amp.lock().map_err(|_| "engine busy".to_string())
                                            .and_then(|mut a| a.load_model(&path));
                                        match res {
                                            Ok(()) => {
                                                model_name.set(name.clone());
                                                selected.set(Some(path.clone()));
                                                status.set(String::new());
                                            }
                                            Err(e) => status.set(format!("load failed: {e}")),
                                        }
                                    });
                                },
                                "{entry.name}"
                            }
                        }
                    }
                }
            }
        }
    }
}

/// A simple horizontal level meter (linear peak 0..1, log-ish fill).
#[component]
fn Meter(label: String, value: f32) -> Element {
    let pct = (value.clamp(0.0, 1.0).sqrt() * 100.0) as u32; // sqrt = perceptual-ish
    let color = if value > 0.9 {
        "#ef4444"
    } else if value > 0.6 {
        "#eab308"
    } else {
        "#22c55e"
    };
    rsx! {
        div {
            style: "display: flex; align-items: center; gap: 10px;",
            div { style: "width: 60px; font-size: 11px; color: #a1a1aa;", "{label}" }
            div {
                style: "flex: 1; height: 14px; background: #0e0e10; border-radius: 3px; overflow: hidden;",
                div { style: format!("height: 100%; width: {pct}%; background: {color};") }
            }
        }
    }
}
