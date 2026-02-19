//! FTS Control Desktop Application
//!
//! Central hub for FastTrackStudio that:
//! - Connects to REAPER(s) via Unix socket
//! - Runs session services in-process (no SHM complexity)
//! - Runs WebSocket gateway for browser access
//! - Renders the same UI as the web app
//!
//! # Renderer Selection
//!
//! This app supports two rendering backends:
//! - `desktop` (default): Uses Wry/WebView - fast, stable, browser-based rendering
//! - `native`: Uses Blitz/Vello - experimental GPU-accelerated native rendering
//!
//! To use native renderer: `cargo run --no-default-features --features native`
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────┐     Unix Socket      ┌──────────────────────────────────┐
//! │ REAPER Extension│◄────────────────────►│       fts-control Desktop        │
//! │  (daw-reaper)   │                      │  ┌─────────────────────────────┐ │
//! │                 │                      │  │ Same UI (session-ui crate)  │ │
//! └─────────────────┘                      │  │ PerformanceLayout, TopBar   │ │
//!                                          │  └─────────────────────────────┘ │
//!                                          │  ┌─────────────────────────────┐ │
//!                                          │  │ Session/Setlist Services    │ │
//!                                          │  │ (in-process, no SHM)        │ │
//!                                          │  └─────────────────────────────┘ │
//!                                          │  ┌─────────────────────────────┐ │
//!                                          │  │ WebSocket Gateway           │ │
//!                                          │  │ (Axum server for browsers)  │ │
//!                                          │  └─────────────────────────────┘ │
//!                                          └──────────────────────────────────┘
//!                                                           ▲
//!                                                      WebSocket
//!                                                     ┌────┴────┐
//!                                                     │ Browser │ (same UI via web)
//!                                                     └─────────┘
//! ```

mod actions;
mod command_palette;
mod gateway;
mod services;
mod signal_views;

// Conditionally import the right Dioxus prelude based on renderer feature
#[cfg(feature = "desktop")]
use dioxus::prelude::*;
#[cfg(feature = "native")]
use dioxus_native::prelude::*;

use std::rc::Rc;
use std::sync::Arc;

#[cfg(feature = "desktop")]
use dioxus::desktop::{tao::window::WindowBuilder, Config};
#[cfg(feature = "desktop")]
use keyflow_ui::ChartGraphics;

use session_ui::{
    ConnectionState, LatencyInfo, TopBar, ACTIVE_PLAYBACK_IS_PLAYING, ACTIVE_PLAYBACK_MUSICAL,
    AUDIO_LATENCY_SECONDS, LATENCY_INFO, SONG_CHARTS,
};

use daw_ui::FxBrowserDockPanel;
use keyflow_ui::{ChartPreviewPanel, ChartView, SESSION_CHART_SOURCE};

use signal::connect_db_seeded;

use dock_dioxus::{
    init_dock_presets, init_rig_dock, DockProvider, DockRoot, PanelRenderer, PanelRendererRegistry,
    PresetBar,
};
use dock_proto::PanelId;

use input::config::default_user_config_path;
use input_dioxus::{use_input_processor, ACTION_CONTEXT, TEXT_INPUT_FOCUS_COUNT};

use tokio;
use tracing::debug;

#[global_allocator]
static GLOBAL_ALLOCATOR: mimalloc::MiMalloc = mimalloc::MiMalloc;

/// Whether the dock layout system is active (vs. classic tab navigation).
static DOCK_MODE: GlobalSignal<bool> = Signal::global(|| true);

/// Top-level page: "main" (dock/classic tabs) or "rig" (rig dock view).
static TOP_PAGE: GlobalSignal<&'static str> = Signal::global(|| "rig");

static COMMAND_PALETTE_OPEN: GlobalSignal<bool> = Signal::global(|| false);
static COMMAND_PALETTE_QUERY: GlobalSignal<String> = Signal::global(String::new);

use actions::{dispatch_action, dispatch_input_commands, handle_dock_preset_shortcut};
use command_palette::{build_input_config, build_palette_entries};
use signal_views::{PerformanceWithChartToggle, SignalView};

const FAVICON: Asset = asset!("/assets/favicon.ico");
const MAIN_CSS: Asset = asset!("/assets/main.css");
const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");
const CHART_LOADING_PLACEHOLDER: &str = "Generating Chart\n\n[Loading]\n| C |";

fn refresh_session_chart_source() {
    use session_ui::{ACTIVE_INDICES, SETLIST_STRUCTURE, SONG_CHARTS};

    let active_song_index = ACTIVE_INDICES.peek().song_index;
    let setlist = SETLIST_STRUCTURE.peek();
    let charts = SONG_CHARTS.peek();

    let next_source = active_song_index
        .and_then(|song_index| setlist.songs.get(song_index))
        .map(|song| {
            charts
                .get(&song.project_guid)
                .map(|chart| chart.chart_text.clone())
                .unwrap_or_else(|| format!("{} (Generating)\n\n[Loading]\n| C |", song.name))
        })
        .or_else(|| Some(CHART_LOADING_PLACEHOLDER.to_string()));

    if *SESSION_CHART_SOURCE.peek() != next_source {
        *SESSION_CHART_SOURCE.write() = next_source;
    }
}

fn main() {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive("fts_control_desktop=info".parse().unwrap())
                .add_directive("session=info".parse().unwrap())
.add_directive("signal_ui=info".parse().unwrap())
                .add_directive("daw_control::fx=debug".parse().unwrap()),
        )
        .init();

    // Initialize peeps instrumentation for diagnostics
    peeps::init!();

    #[cfg(feature = "desktop")]
    debug!("Starting FTS Control Desktop (Wry/WebView + WGPU hybrid renderer)");
    #[cfg(feature = "native")]
    debug!("Starting FTS Control Desktop (Blitz/Native renderer)");

    // Start async runtime for services (before UI)
    std::thread::spawn(|| {
        tokio::runtime::Runtime::new()
            .expect("Failed to create Tokio runtime")
            .block_on(run_services());
    });

    // Launch Dioxus UI with the appropriate renderer
    #[cfg(feature = "desktop")]
    {
        use std::sync::Mutex;

        // Configure hybrid rendering: WGPU surface with Dioxus as transparent overlay
        let config = Config::new()
            .with_window(
                WindowBuilder::new()
                    .with_title("FTS Control")
                    .with_transparent(true),
            )
            // CRITICAL: Set WebView background to transparent (RGBA with alpha=0)
            // Without this, the WebView itself has an opaque background even if CSS is transparent
            .with_background_color((0, 0, 0, 0))
            .with_on_window(|window, dom| {
                // Get window size for initialization
                let size = window.inner_size();

                // Initialize anyrender-based graphics context
                let graphics = ChartGraphics::new(window, size.width, size.height);

                // Wrap in Arc<Mutex> for shared mutable access
                let graphics = Arc::new(Mutex::new(graphics));

                // Provide graphics context to all Dioxus components
                dom.provide_root_context(graphics);
            })
            .with_as_child_window();

        dioxus::LaunchBuilder::desktop()
            .with_cfg(config)
            .launch(App);
    }

    #[cfg(feature = "native")]
    dioxus_native::launch(App);
}

#[component]
fn App() -> Element {
    // Track active tab
    let mut active_tab = use_signal(|| "performance".to_string());

    // Build input config and processor once (persists across renders).
    let input_config = use_hook(build_input_config);
    let input_handle = use_input_processor(input_config.clone());
    let input_handle = Rc::new(input_handle);
    let palette_entries = use_hook(|| Rc::new(build_palette_entries(&input_config)));

    // Poll user keymap for changes and hot-reload bindings at runtime.
    let _input_reload_task = {
        let input_handle_reload = input_handle.clone();
        use_future(move || {
            let input_handle = input_handle_reload.clone();
            async move {
                let Some(user_path) = default_user_config_path() else {
                    return;
                };
                let mut last_modified = std::fs::metadata(&user_path)
                    .and_then(|m| m.modified())
                    .ok();

                loop {
                    tokio::time::sleep(tokio::time::Duration::from_millis(1000)).await;

                    let current_modified = std::fs::metadata(&user_path)
                        .and_then(|m| m.modified())
                        .ok();
                    if current_modified == last_modified {
                        continue;
                    }
                    last_modified = current_modified;

                    let next_config = build_input_config();
                    input_handle.reload_config(next_config);
                    tracing::info!(path = %user_path.display(), "Reloaded input keymap config");
                }
            }
        })
    };

    // Initialize action context with default tab on first render
    use_hook(|| {
        let mut ctx = ACTION_CONTEXT.write();
        ctx.set_tab("performance");
        ctx.set_mode("normal");
    });

    // Initialize dock layout presets (loads from disk or uses built-in defaults)
    use_hook(|| {
        init_dock_presets();
        init_rig_dock(dock_proto::rig_presets());
    });

    // Panel renderer — decentralized: each domain crate registers its own panels.
    let render_panel = use_hook(|| {
        let mut registry = PanelRendererRegistry::new();

        // Domain crates register their panels
        session_ui::register_panels(&mut registry);
        signal_ui::register_panels(&mut registry);
        registry.register(PanelId::FxBrowser, || {
            rsx! { FxBrowserDockPanel {} }
        });
        daw_ui::register_panels(&mut registry);

        // App-level panels (components defined in this binary)
        registry.register(PanelId::Performance, || {
            rsx! { PerformanceWithChartToggle {} }
        });
        registry.register(PanelId::ChartEditor, || {
            rsx! { ChartView {} }
        });
        registry.register(PanelId::ChartPreview, || {
            rsx! { ChartPreviewPanel {} }
        });
        registry.register(PanelId::Setlist, || {
            rsx! { SetlistView {} }
        });
        registry.register(PanelId::Settings, || {
            rsx! { SettingsView {} }
        });

        let registry = Rc::new(registry);
        PanelRenderer::new(move |panel_id| registry.render(panel_id))
    });

    // Signal disk-based storage signal
    let mut signal_store = use_signal(|| None::<signal::Signal>);
    use_effect(move || {
        spawn(async move {
            // Use the user's Music folder for the signal database
            let home = std::env::var("HOME").unwrap_or_else(|_| "/tmp".to_string());
            let db_path = format!("{}/Music/FastTrackStudio/Library/signal.db", home);

            // Create parent directory if it doesn't exist
            if let Some(parent) = std::path::Path::new(&db_path).parent() {
                let _ = std::fs::create_dir_all(parent);
            }

            match connect_db_seeded(&db_path).await {
                Ok(ctrl) => {
                    tracing::info!("Signal database connected at: {}", db_path);
                    provide_context(ctrl.clone());
                    signal_store.set(Some(ctrl));
                }
                Err(e) => {
                    tracing::error!("Failed to connect signal database at {}: {}", db_path, e)
                }
            }
        });
    });

    // Connection state - tracks DAW connection
    let mut connection_state = use_signal(|| ConnectionState::Connecting);

    // WGPU/Vello chart graphics context (desktop only)
    #[cfg(feature = "desktop")]
    let _graphics = consume_context::<Arc<std::sync::Mutex<ChartGraphics>>>();

    // Request initial redraw on mount (desktop only)
    #[cfg(feature = "desktop")]
    use_effect(|| {
        dioxus::desktop::window().window.request_redraw();
    });

    // Handle window resize events (desktop only)
    // We only request a redraw here — the actual WGPU surface resize is deferred
    // to the render effects which check window.inner_size() before each frame.
    // This avoids reconfiguring the GPU surface dozens of times during a macOS
    // animated resize (which causes a multi-second freeze).
    #[cfg(feature = "desktop")]
    {
        use dioxus::desktop::{tao::event::Event as WryEvent, use_wry_event_handler, window};

        use_wry_event_handler(move |event, _| {
            use dioxus::desktop::tao::event::WindowEvent;

            if let WryEvent::WindowEvent {
                event: WindowEvent::Resized(_),
                ..
            } = event
            {
                window().window.request_redraw();
            }
        });
    }

    // Latency polling task - runs within Dioxus context for proper signal updates
    let _latency_task = use_future(move || async move {
        // Wait for DAW to connect
        loop {
            if is_daw_connected() {
                break;
            }
            tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
        }

        debug!("Starting audio latency polling (Dioxus context)");

        loop {
            // Fetch full audio latency info from DAW
            match daw_control::Daw::get().audio_engine().get_state().await {
                Ok(state) => {
                    // Calculate latencies in milliseconds
                    let output_ms = state.latency.output_seconds * 1000.0;
                    let input_ms = if state.latency.sample_rate > 0 {
                        (state.latency.input_samples as f64 / state.latency.sample_rate as f64)
                            * 1000.0
                    } else {
                        0.0
                    };

                    // Update AUDIO_LATENCY_SECONDS for transport compensation
                    let current = *AUDIO_LATENCY_SECONDS.read();
                    if (state.latency.output_seconds - current).abs() > 0.0001 {
                        *AUDIO_LATENCY_SECONDS.write() = state.latency.output_seconds;
                    }

                    // Update full latency info for UI display
                    let new_info = LatencyInfo {
                        input_ms,
                        output_ms,
                        network_rtt_ms: 0.0, // Direct connection, no network latency
                        sample_rate: state.latency.sample_rate,
                        is_running: state.is_running,
                    };

                    let current_info = LATENCY_INFO.read().clone();
                    if new_info != current_info {
                        *LATENCY_INFO.write() = new_info;
                        debug!(
                            "Latency updated: input={:.1}ms, output={:.1}ms, rate={}Hz",
                            input_ms, output_ms, state.latency.sample_rate
                        );
                    }
                }
                Err(e) => {
                    tracing::warn!("Failed to get audio latency: {}", e);
                }
            }

            // Poll every 5 seconds
            tokio::time::sleep(tokio::time::Duration::from_secs(5)).await;
        }
    });

    // Main sync task - fetches setlist and subscribes to updates
    let _sync_task = use_future(move || async move {
        use session_ui::{
            Session, TransportState, ACTIVE_INDICES, PLAYBACK_STATE, SETLIST_STRUCTURE,
            SONG_TRANSPORT,
        };

        // Wait for DAW to connect
        loop {
            if is_daw_connected() {
                break;
            }
            tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
        }

        // Update connection state
        connection_state.set(ConnectionState::Connected);
        debug!("DAW connected, initializing...");

        let session = Session::get();
        let setlist_client = session.setlist();

        // Build setlist from open REAPER projects
        if let Err(e) = setlist_client.build_from_open_projects().await {
            tracing::warn!("Failed to build setlist: {}", e);
            return;
        }

        // Get initial setlist
        match setlist_client.get_setlist().await {
            Ok(Some(setlist)) => {
                debug!(
                    "Got setlist '{}' with {} songs",
                    setlist.name,
                    setlist.songs.len()
                );
                let songs = setlist.songs.clone();
                *SETLIST_STRUCTURE.write() = setlist;

                // Set initial active song
                match setlist_client.get_active_song().await {
                    Ok(Some(active_song)) => {
                        if let Some(idx) = songs
                            .iter()
                            .position(|s| s.project_guid == active_song.project_guid)
                        {
                            let mut indices = ACTIVE_INDICES.write();
                            indices.song_index = Some(idx);
                            indices.section_index = Some(0);
                            debug!("Active song: {} (index {})", active_song.name, idx);
                        }
                    }
                    Ok(None) if !songs.is_empty() => {
                        let mut indices = ACTIVE_INDICES.write();
                        indices.song_index = Some(0);
                        indices.section_index = Some(0);
                    }
                    _ => {}
                }
                refresh_session_chart_source();
            }
            Ok(None) => debug!("No setlist available"),
            Err(e) => tracing::warn!("Failed to get setlist: {}", e),
        }

        // Subscribe to setlist events (transport updates, active song changes, etc.)
        debug!("Subscribing to setlist events...");
        let (tx, mut rx) = roam::channel::<session_proto::SetlistEvent>();

        match setlist_client.subscribe(tx).await {
            Ok(()) => {
                debug!("Subscribed to SetlistService events (60Hz transport updates)");

                // Process events continuously
                while let Ok(Some(event)) = rx.recv().await {
                    match event {
                        session_proto::SetlistEvent::SetlistChanged(setlist) => {
                            debug!("Setlist changed: {} songs", setlist.songs.len());
                            let valid_guids: std::collections::HashSet<String> = setlist
                                .songs
                                .iter()
                                .map(|song| song.project_guid.clone())
                                .collect();
                            SONG_CHARTS
                                .write()
                                .retain(|guid, _| valid_guids.contains(guid));
                            *SETLIST_STRUCTURE.write() = setlist;
                            refresh_session_chart_source();
                        }
                        session_proto::SetlistEvent::SongHydrated { index, song } => {
                            let is_active_song = ACTIVE_INDICES.peek().song_index == Some(index);
                            let mut setlist = SETLIST_STRUCTURE.write();
                            if index < setlist.songs.len() {
                                setlist.songs[index] = song;
                            }
                            drop(setlist);
                            if is_active_song {
                                refresh_session_chart_source();
                            }
                        }

                        session_proto::SetlistEvent::ActiveIndicesChanged(indices) => {
                            let prev_song_index = ACTIVE_INDICES.peek().song_index;
                            *ACTIVE_INDICES.write() = indices.clone();
                            *PLAYBACK_STATE.write() = if indices.is_playing {
                                daw_proto::PlayState::Playing
                            } else {
                                daw_proto::PlayState::Stopped
                            };
                            if prev_song_index != indices.song_index {
                                refresh_session_chart_source();
                            }
                        }

                        session_proto::SetlistEvent::TransportUpdate(transports) => {
                            // PERFORMANCE: Only write to signals if values actually changed.
                            // Each .write() triggers re-renders for all subscribers.

                            let active_song_index = ACTIVE_INDICES.peek().song_index;
                            let audio_latency = *AUDIO_LATENCY_SECONDS.peek();
                            let mut transport_updates: Vec<(usize, TransportState)> =
                                Vec::with_capacity(transports.len());
                            let mut active_playback_update: Option<(
                                Option<daw_proto::MusicalPosition>,
                                bool,
                            )> = None;

                            {
                                let setlist = SETLIST_STRUCTURE.peek();
                                let existing_transports = SONG_TRANSPORT.peek();
                                for transport in transports {
                                    // Build the new transport state
                                    let compensated_position = if transport.is_playing
                                        && audio_latency > 0.0
                                    {
                                        let compensated_time = transport.position.time.map(|t| {
                                            daw_proto::TimePosition::from_seconds(
                                                t.as_seconds() + audio_latency,
                                            )
                                        });
                                        daw_proto::Position::new(
                                            transport.position.musical.clone(),
                                            compensated_time,
                                            transport.position.midi.clone(),
                                        )
                                    } else {
                                        transport.position.clone()
                                    };

                                    let loop_region_percent =
                                        transport.loop_region.as_ref().and_then(|region| {
                                            setlist.songs.get(transport.song_index).map(|song| {
                                                let song_duration = song.duration();
                                                if song_duration > 0.0 {
                                                    (
                                                        (region.start_seconds / song_duration)
                                                            .clamp(0.0, 1.0),
                                                        (region.end_seconds / song_duration)
                                                            .clamp(0.0, 1.0),
                                                    )
                                                } else {
                                                    (0.0, 1.0)
                                                }
                                            })
                                        });

                                    let new_transport = TransportState {
                                        position: compensated_position,
                                        bpm: transport.bpm,
                                        time_sig_num: transport.time_sig_num as i32,
                                        time_sig_denom: transport.time_sig_denom as i32,
                                        is_playing: transport.is_playing,
                                        is_looping: transport.is_looping,
                                        loop_region: loop_region_percent,
                                    };

                                    let needs_transport_update = existing_transports
                                        .get(&transport.song_index)
                                        .map(|existing| *existing != new_transport)
                                        .unwrap_or(true);

                                    if needs_transport_update {
                                        transport_updates
                                            .push((transport.song_index, new_transport));
                                    }

                                    if Some(transport.song_index) == active_song_index {
                                        active_playback_update = Some((
                                            transport.position.musical,
                                            transport.is_playing,
                                        ));
                                    }
                                }
                            }

                            if !transport_updates.is_empty() {
                                let mut song_transport = SONG_TRANSPORT.write();
                                for (song_index, transport_state) in transport_updates {
                                    song_transport.insert(song_index, transport_state);
                                }
                            }

                            if let Some((musical, is_playing)) = active_playback_update {
                                if *ACTIVE_PLAYBACK_MUSICAL.peek() != musical {
                                    *ACTIVE_PLAYBACK_MUSICAL.write() = musical;
                                }
                                if *ACTIVE_PLAYBACK_IS_PLAYING.peek() != is_playing {
                                    *ACTIVE_PLAYBACK_IS_PLAYING.write() = is_playing;
                                }

                                let new_state = if is_playing {
                                    daw_proto::PlayState::Playing
                                } else {
                                    daw_proto::PlayState::Stopped
                                };
                                if *PLAYBACK_STATE.peek() != new_state {
                                    *PLAYBACK_STATE.write() = new_state;
                                }
                            } else if active_song_index.is_none() {
                                if ACTIVE_PLAYBACK_MUSICAL.peek().is_some() {
                                    *ACTIVE_PLAYBACK_MUSICAL.write() = None;
                                }
                                if *ACTIVE_PLAYBACK_IS_PLAYING.peek() {
                                    *ACTIVE_PLAYBACK_IS_PLAYING.write() = false;
                                }
                            }
                        }

                        session_proto::SetlistEvent::SongEntered { index, song } => {
                            debug!("Entered song {}: {}", index, song.name);
                        }

                        session_proto::SetlistEvent::SongExited { index } => {
                            debug!("Exited song {}", index);
                        }

                        session_proto::SetlistEvent::SectionEntered {
                            song_index,
                            section_index,
                            section,
                        } => {
                            debug!(
                                "Entered section {}.{}: {}",
                                song_index, section_index, section.name
                            );
                        }

                        session_proto::SetlistEvent::SectionExited {
                            song_index,
                            section_index,
                        } => {
                            debug!("Exited section {}.{}", song_index, section_index);
                        }

                        session_proto::SetlistEvent::SongChartHydrated { index, chart } => {
                            SONG_CHARTS
                                .write()
                                .insert(chart.project_guid.clone(), chart);
                            let is_active_song = ACTIVE_INDICES.peek().song_index == Some(index);
                            if is_active_song {
                                refresh_session_chart_source();
                            }
                        }

                        session_proto::SetlistEvent::PositionChanged { indices, .. } => {
                            // High-frequency position update - update active indices
                            *ACTIVE_INDICES.write() = indices;
                        }
                    }
                }

                tracing::warn!("SetlistEvent stream ended");
            }
            Err(e) => {
                tracing::error!("Failed to subscribe to setlist events: {}", e);
            }
        }
    });

    rsx! {
        // Document head
        document::Link { rel: "icon", href: FAVICON }
        // Tailwind CSS first, then main.css to allow overrides for WGPU transparency
        document::Link { rel: "stylesheet", href: TAILWIND_CSS }
        document::Link { rel: "stylesheet", href: MAIN_CSS }

        // Main app layout with keyboard handler
        // Use transparent background when on chart tab OR when chart split is enabled
        {
            let dock_mode = *DOCK_MODE.read();

            // In dock mode, transparency depends on whether any WGPU-rendered panel is
            // the *active* tab (actually visible), not just present in the layout.
            // In classic mode, it depends on the active tab.
            let needs_transparency = if dock_mode {
                let layout = dock_dioxus::DOCK_LAYOUT.read();
                layout.panel_is_visible(PanelId::ChartEditor)
                    || layout.panel_is_visible(PanelId::ChartPreview)
            } else {
                active_tab() == "chart"
            };

            // Ensure transparent-mode CSS class on <html> matches the current state.
            #[cfg(feature = "desktop")]
            use_effect(move || {
                let dock = *DOCK_MODE.read();
                let want_transparent = if dock {
                    let layout = dock_dioxus::DOCK_LAYOUT.read();
                    layout.panel_is_visible(PanelId::ChartEditor)
                        || layout.panel_is_visible(PanelId::ChartPreview)
                } else {
                    active_tab() == "chart"
                };
                if want_transparent {
                    document::eval(r#"document.documentElement.classList.add('transparent-mode');"#);
                } else {
                    document::eval(r#"document.documentElement.classList.remove('transparent-mode');"#);
                }
            });

            let input_handle_key = input_handle.clone();
            let input_handle_wheel = input_handle.clone();

            rsx! {
                div {
                    class: if needs_transparency {
                        "h-screen flex flex-col text-foreground outline-none"
                    } else {
                        "h-screen flex flex-col bg-background text-foreground outline-none"
                    },
                    style: if needs_transparency { "background: transparent !important; background-color: transparent !important;" } else { "" },
                    tabindex: "0",
                    autofocus: true,
                    onkeydown: move |e: KeyboardEvent| {
                        // Skip action processing when a text input has focus
                        // or the command palette is open
                        if *TEXT_INPUT_FOCUS_COUNT.read() > 0 || *COMMAND_PALETTE_OPEN.read() {
                            return;
                        }

                        if handle_dock_preset_shortcut(&e) {
                            e.prevent_default();
                            return;
                        }

                        let commands = input_handle_key.handle_key(&e);
                        let handled = dispatch_input_commands(commands);
                        ACTION_CONTEXT
                            .write()
                            .set_mode(input_handle_key.current_mode().as_str());

                        if handled {
                            e.prevent_default();
                        }
                    },
                    onwheel: move |e: WheelEvent| {
                        if *COMMAND_PALETTE_OPEN.read() {
                            return;
                        }

                        let commands = input_handle_wheel.handle_wheel(&e);
                        if dispatch_input_commands(commands) {
                            e.prevent_default();
                        }
                    },

                    // Dock provider wraps the entire UI so all panels can access the renderer
                    DockProvider { render_panel: render_panel.clone(),

                        // ── Page switcher (Main / Rig) ──────────────────────
                        {
                            let current_page = *TOP_PAGE.read();
                            rsx! {
                                div { class: "flex items-center gap-0 bg-zinc-950 border-b border-zinc-800 px-2 py-0.5 flex-shrink-0",
                                    button {
                                        class: if current_page == "main" {
                                            "px-3 py-1 text-xs font-medium text-white bg-zinc-700 rounded-md transition-colors"
                                        } else {
                                            "px-3 py-1 text-xs font-medium text-zinc-400 hover:text-zinc-200 hover:bg-zinc-800 rounded-md transition-colors"
                                        },
                                        onclick: move |_| { *TOP_PAGE.write() = "main"; },
                                        "Session"
                                    }
                                    button {
                                        class: if current_page == "rig" {
                                            "px-3 py-1 text-xs font-medium text-white bg-zinc-700 rounded-md transition-colors"
                                        } else {
                                            "px-3 py-1 text-xs font-medium text-zinc-400 hover:text-zinc-200 hover:bg-zinc-800 rounded-md transition-colors"
                                        },
                                        onclick: move |_| { *TOP_PAGE.write() = "rig"; },
                                        "Signal"
                                    }
                                }
                            }
                        }

                        // ── Page content ─────────────────────────────────────
                        if *TOP_PAGE.read() == "rig" {
                            // Signal page: signal views
                            div {
                                class: "flex-1 overflow-hidden relative",
                                if signal_store().is_some() {
                                    SignalView {}
                                } else {
                                    div { class: "flex items-center justify-center h-full",
                                        p { class: "text-sm text-muted-foreground", "Bootstrapping signal storage..." }
                                    }
                                }
                            }
                        } else if *DOCK_MODE.read() {
                            // Main page, dock mode: preset bar + dock root
                            PresetBar {}

                            div {
                                class: "flex-1 overflow-hidden relative",
                                style: if needs_transparency { "background: transparent !important; background-color: transparent !important;" } else { "" },
                                DockRoot {}
                            }
                        } else {
                            // Main page, classic mode: top bar + tab content
                            TopBar {
                                connection_state: connection_state(),
                                active_tab: active_tab(),
                                on_tab_click: Some(Callback::new(move |tab: String| {
                                    ACTION_CONTEXT.write().set_tab(&tab);
                                    active_tab.set(tab);
                                })),
                            }

                            div {
                                class: "flex-1 overflow-hidden relative",
                                style: if needs_transparency { "background: transparent !important; background-color: transparent !important;" } else { "" },
                                match active_tab().as_str() {
                                    "performance" => rsx! { PerformanceWithChartToggle {} },
                                    "chart" => rsx! { ChartView {} },
                                    "setlist" => rsx! { SetlistView {} },
                                    "rig" => rsx! {
                                        if signal_store().is_some() {
                                            SignalView {}
                                        } else {
                                            div { class: "flex items-center justify-center h-full",
                                                p { class: "text-sm text-muted-foreground", "Bootstrapping signal storage..." }
                                            }
                                        }
                                    },
                                    "fx" => rsx! { FxBrowserDockPanel {} },
                                    "settings" => rsx! { SettingsView {} },
                                    _ => rsx! { PerformanceWithChartToggle {} },
                                }
                            }
                        }

                        // Input status overlay (mode / pending sequence / macro recording)
                        div {
                            class: "absolute bottom-2 right-2 z-20 rounded border border-zinc-700 bg-zinc-900/85 px-2 py-1 text-xs text-zinc-100 pointer-events-none",
                            "{input_handle.current_mode().as_str().to_uppercase()}"
                            if input_handle.is_recording() {
                                span { class: "ml-2 text-red-300", "REC" }
                            }
                            if let Some(pending) = input_handle.pending_display() {
                                span { class: "ml-2 text-amber-300", "{pending}" }
                            }
                        }

                        if *COMMAND_PALETTE_OPEN.read() {
                            {
                                let query = COMMAND_PALETTE_QUERY.read().clone();
                                let filtered: Vec<_> = palette_entries
                                    .iter()
                                    .filter(|entry| {
                                        if query.is_empty() {
                                            true
                                        } else {
                                            let q = query.to_lowercase();
                                            entry.name.to_lowercase().contains(&q)
                                                || entry.id.to_lowercase().contains(&q)
                                                || entry.description.to_lowercase().contains(&q)
                                                || entry
                                                    .mappings
                                                    .iter()
                                                    .any(|m| m.to_lowercase().contains(&q))
                                        }
                                    })
                                    .take(40)
                                    .collect();

                                rsx! {
                                    div {
                                        class: "absolute inset-0 z-30 flex items-start justify-center bg-black/50",
                                        onclick: move |_| {
                                            *COMMAND_PALETTE_OPEN.write() = false;
                                        },
                                        div {
                                            class: "mt-16 w-[min(900px,95vw)] max-h-[70vh] overflow-hidden rounded-lg border border-zinc-700 bg-zinc-900 shadow-xl",
                                            onclick: move |e| e.stop_propagation(),
                                            div {
                                                class: "border-b border-zinc-700 p-2",
                                                input {
                                                    class: "w-full rounded bg-zinc-800 px-3 py-2 text-sm text-zinc-100 outline-none",
                                                    r#type: "text",
                                                    value: "{query}",
                                                    placeholder: "Search actions, ids, or key mappings...",
                                                    autofocus: true,
                                                    oninput: move |e| {
                                                        *COMMAND_PALETTE_QUERY.write() = e.value();
                                                    },
                                                    onkeydown: move |e| {
                                                        if matches!(e.key(), Key::Escape) {
                                                            *COMMAND_PALETTE_OPEN.write() = false;
                                                            e.stop_propagation();
                                                            return;
                                                        }
                                                        if matches!(e.key(), Key::Enter) {
                                                            let query = COMMAND_PALETTE_QUERY.read().clone();
                                                            if let Some(first) = palette_entries
                                                                .iter()
                                                                .filter(|entry| {
                                                                    if query.is_empty() {
                                                                        true
                                                                    } else {
                                                                        let q = query.to_lowercase();
                                                                        entry.name.to_lowercase().contains(&q)
                                                                            || entry.id.to_lowercase().contains(&q)
                                                                            || entry.description.to_lowercase().contains(&q)
                                                                            || entry
                                                                                .mappings
                                                                                .iter()
                                                                                .any(|m| m.to_lowercase().contains(&q))
                                                                    }
                                                                })
                                                                .next()
                                                            {
                                                                dispatch_action(&first.id);
                                                            }
                                                            *COMMAND_PALETTE_OPEN.write() = false;
                                                            *COMMAND_PALETTE_QUERY.write() = String::new();
                                                            e.stop_propagation();
                                                        }
                                                    }
                                                }
                                            }
                                            div {
                                                class: "max-h-[60vh] overflow-y-auto p-2",
                                                for entry in filtered {
                                                    div {
                                                        class: "mb-2 rounded border border-zinc-800 bg-zinc-950/80 p-3 hover:border-zinc-600 cursor-pointer",
                                                        onclick: {
                                                            let id = entry.id.clone();
                                                            move |_| {
                                                                dispatch_action(&id);
                                                                *COMMAND_PALETTE_OPEN.write() = false;
                                                                *COMMAND_PALETTE_QUERY.write() = String::new();
                                                            }
                                                        },
                                                        div {
                                                            class: "flex items-center justify-between gap-3",
                                                            div { class: "text-sm font-medium text-zinc-100", "{entry.name}" }
                                                            if !entry.shortcut.is_empty() {
                                                                div { class: "text-xs text-emerald-300", "{entry.shortcut}" }
                                                            }
                                                        }
                                                        div { class: "mt-1 text-xs text-zinc-400", "{entry.id}" }
                                                        if !entry.description.is_empty() {
                                                            div { class: "mt-1 text-xs text-zinc-300", "{entry.description}" }
                                                        }
                                                        if !entry.when_clause.is_empty() {
                                                            div { class: "mt-1 text-xs text-amber-300", "when: {entry.when_clause}" }
                                                        }
                                                        if !entry.mappings.is_empty() {
                                                            div { class: "mt-2 text-xs text-cyan-300", "mapped:" }
                                                            for mapping in &entry.mappings {
                                                                div { class: "text-xs text-cyan-200", "{mapping}" }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Setlist management view (placeholder)
#[component]
fn SetlistView() -> Element {
    rsx! {
        div {
            class: "flex items-center justify-center h-full",
            div {
                class: "text-center",
                h2 {
                    class: "text-2xl font-bold text-foreground mb-4",
                    "Setlist Editor"
                }
                p {
                    class: "text-muted-foreground",
                    "Coming soon..."
                }
            }
        }
    }
}

/// Settings view (placeholder)
#[component]
fn SettingsView() -> Element {
    rsx! {
        div {
            class: "flex items-center justify-center h-full",
            div {
                class: "text-center",
                h2 {
                    class: "text-2xl font-bold text-foreground mb-4",
                    "Settings"
                }
                p {
                    class: "text-muted-foreground",
                    "Coming soon..."
                }
            }
        }
    }
}

/// Global flag to signal when DAW is connected (for UI to know when to fetch)
static DAW_CONNECTED: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// Check if DAW is connected
pub fn is_daw_connected() -> bool {
    DAW_CONNECTED.load(std::sync::atomic::Ordering::Relaxed)
}

// ============================================================================
// REAPER Connection (Unix Socket Client)
// ============================================================================

/// Default socket path, overridable via `FTS_SOCKET` env var.
const DEFAULT_SOCKET_PATH: &str = "/tmp/fts-control.sock";

/// Connector for the REAPER extension's Unix socket.
///
/// Implements `roam_stream::Connector` so that `roam_stream::connect()` can
/// establish (and automatically reconnect) a roam session over the well-known
/// socket at `/tmp/fts-control.sock`.
struct ReaperConnector {
    path: std::path::PathBuf,
}

impl roam_stream::Connector for ReaperConnector {
    type Transport = tokio::net::UnixStream;

    async fn connect(&self) -> std::io::Result<Self::Transport> {
        tokio::net::UnixStream::connect(&self.path).await
    }
}

/// Connect to the REAPER extension via Unix socket and initialize the DAW singleton.
///
/// Uses `roam_stream::connect()` which retries with backoff until the socket is
/// available, making it safe to start fts-control before REAPER is fully loaded.
async fn connect_to_reaper() {
    let socket_path = std::env::var("FTS_SOCKET")
        .map(std::path::PathBuf::from)
        .unwrap_or_else(|_| std::path::PathBuf::from(DEFAULT_SOCKET_PATH));
    debug!("Connecting to REAPER at {}", socket_path.display());

    let connector = ReaperConnector { path: socket_path };
    let config = roam_session::HandshakeConfig {
        max_payload_size: 1024 * 1024,            // 1 MiB
        initial_channel_credit: 16 * 1024 * 1024, // 16 MiB — matches server
        max_concurrent_requests: 64,
        ..Default::default()
    };

    let client = roam_stream::connect(connector, config, roam_session::NoDispatcher);

    match client.handle().await {
        Ok(handle) => {
            if let Err(e) = daw_control::Daw::init(handle) {
                tracing::error!("Failed to initialize DAW singleton: {}", e);
                return;
            }
            DAW_CONNECTED.store(true, std::sync::atomic::Ordering::Relaxed);
            debug!("Connected to REAPER via Unix socket — DAW singleton initialized");
        }
        Err(e) => {
            tracing::error!("Failed to connect to REAPER: {}", e);
        }
    }
}

/// Run background services (session services, gateway)
async fn run_services() {
    use gateway::{start_gateway, GatewayConfig};
    use services::LocalServices;
    use session_ui::Session;

    debug!("Initializing services...");

    // 1. Connect to REAPER via Unix socket (initializes Daw singleton)
    connect_to_reaper().await;

    // 2. Create local session services (these use Daw::get() internally)
    let services = LocalServices::new();
    debug!("Local services initialized");

    // 3. Initialize Session singleton for UI components
    match services.create_setlist_client().await {
        Ok(setlist_client) => {
            if let Err(e) = Session::init(setlist_client) {
                tracing::warn!("Failed to initialize Session singleton: {}", e);
            } else {
                debug!("Session singleton initialized");
            }
        }
        Err(e) => {
            tracing::error!("Failed to create setlist client: {}", e);
        }
    }

    // 4. Create dispatcher for gateway
    let dispatcher = services.create_dispatcher();

    // 5. Start WebSocket gateway for browser access
    let config = GatewayConfig::default();
    debug!("Starting gateway on {}", config.bind_addr);

    if let Err(e) = start_gateway(dispatcher, &config.bind_addr, config.static_dir.as_deref()).await
    {
        tracing::error!("Gateway error: {}", e);
    }
}
