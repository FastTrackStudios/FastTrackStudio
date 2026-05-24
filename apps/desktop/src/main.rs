//! Signal Desktop — standalone Dioxus desktop app for signal chain management.
//!
//! Two rendering modes, selected at compile time:
//!
//! **Dev** (default): `dioxus::LaunchBuilder::desktop` (WebKit/WRY)
//! — enables `dx serve` hot-reload for rapid RSX/Tailwind prototyping.
//! `dev` is the default feature so plain `dx serve` works without flags.
//!
//! **Production** (`--no-default-features`): `nih_plug_dioxus::open_standalone_with_state`
//! (Blitz/Vello/wgpu renderer) — same pipeline as `fts-signal-plugin`
//! (VST3/CLAP). UI components are guaranteed to render identically in both
//! standalone and plugin contexts. Nix release builds pass `--no-default-features`
//! automatically.

use dioxus::prelude::*;

use signal::{Signal, connect_db_seeded};
use signal_audio::MidiInput;
#[cfg(not(feature = "dev"))]
use signal_audio::ProcessingChain;
#[cfg(not(feature = "dev"))]
use signal_sampler::SamplerPlayer;
use signal_ui::SignalRoot;

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
    sampler: SamplerPlayer,
}

#[cfg(not(feature = "dev"))]
fn main() {
    use nih_plug_dioxus::SharedState;
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

    // Create the live audio processing chain and start the engine.
    let chain = ProcessingChain::new(48000.0);
    let _engine = signal_audio::LiveAudioEngine::disabled();

    // Create the sampler player. It owns the native cpal output stream; keep
    // the processing-chain engine disabled until both paths share one mixer.
    let sampler = SamplerPlayer::new().expect("Failed to initialise SamplerPlayer");

    let app_state = AppState {
        signal: controller,
        chain,
        sampler,
    };
    let shared = SharedState::new(Arc::new(app_state));

    // Blocks until the window is closed.
    nih_plug_dioxus::open_standalone_with_state(App, 1400, 900, Some(shared));
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
    use nih_plug_dioxus::SharedState;

    let shared = use_context::<SharedState>();
    let state = shared
        .get::<AppState>()
        .expect("AppState not found in Dioxus context");

    let controller = state.signal.clone();
    let chain = state.chain.clone();
    let sampler = state.sampler.clone();

    // Provide shared state so child components can consume it via use_context().
    use_context_provider(|| chain);
    use_context_provider(|| sampler);

    rsx! {
        // AppStyles is prop-less/hook-less so Dioxus skips re-rendering it —
        // prevents the "Changing props of Style {}" warning at 60Hz.
        AppStyles {}
        SignalRoot { controller: controller }
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
                let midi = MidiInput::open_all();
                if midi.is_some() {
                    tracing::info!("MIDI: input active");
                } else {
                    tracing::info!("MIDI: no input ports found");
                }
                midi // provides Option<MidiInput>
            });
            rsx! {
                document::Style { {BASE_CSS} }
                AppStyles {}
                SignalRoot { controller: ctrl }
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
