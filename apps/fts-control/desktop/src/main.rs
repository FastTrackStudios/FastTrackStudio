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

mod daw_connection;
mod gateway;
mod services;

// Conditionally import the right Dioxus prelude based on renderer feature
#[cfg(feature = "desktop")]
use dioxus::prelude::*;
#[cfg(feature = "native")]
use dioxus_native::prelude::*;

use std::sync::Arc;
use std::{collections::HashMap, rc::Rc};

#[cfg(feature = "desktop")]
use dioxus::desktop::{tao::window::WindowBuilder, Config};
#[cfg(feature = "desktop")]
use keyflow_ui::ChartGraphics;

use session_ui::{
    ConnectionState, LatencyInfo, PerformanceLayout, Session, TopBar, ACTIVE_INDICES,
    ACTIVE_PLAYBACK_IS_PLAYING, ACTIVE_PLAYBACK_MUSICAL, AUDIO_LATENCY_SECONDS, LATENCY_INFO,
    SONG_CHARTS,
};

use daw_ui::FxBrowserDockPanel;
use keyflow_ui::{ChartPreviewPanel, ChartView, SESSION_CHART_SOURCE};

use lumen_blocks::components::dropdown::{
    Dropdown, DropdownContent, DropdownItem, DropdownTrigger,
};
use signal2::{bootstrap_in_memory_controller_async, SignalController};
use signal2_ui::views::CollectionBrowser;

use dock_dioxus::{
    init_dock_presets, init_rig_dock, DockProvider, DockRoot, PanelRenderer, PanelRendererRegistry,
    PresetBar,
};
use dock_proto::PanelId;

use actions_proto::ids::standalone as standalone_ids;
use actions_proto::ActionDefinition;
use input::{
    config::{default_user_config_path, load_default_config, load_user_config},
    InputCommand, KeymapConfig,
};
use input_dioxus::{use_input_processor, ACTION_CONTEXT, TEXT_INPUT_FOCUS_COUNT};
use session::session_actions;

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

#[derive(Clone)]
struct PaletteEntry {
    id: String,
    name: String,
    description: String,
    shortcut: String,
    when_clause: String,
    mappings: Vec<String>,
}

fn build_input_config() -> KeymapConfig {
    let mut merged = load_default_config().unwrap_or_default();

    let mut app_overlay = KeymapConfig::default();
    app_overlay.keymap.insert(
        "normal".to_string(),
        HashMap::from([
            (
                "Cmd+Comma".to_string(),
                standalone_ids::OPEN_SETTINGS.as_str().to_string(),
            ),
            (
                "Cmd+Shift+D".to_string(),
                standalone_ids::TOGGLE_DARK_MODE.as_str().to_string(),
            ),
            (
                "Cmd+Shift+P".to_string(),
                standalone_ids::COMMAND_PALETTE.as_str().to_string(),
            ),
        ]),
    );
    merged = KeymapConfig::merge(merged, app_overlay);

    if let Ok(Some(user)) = load_user_config() {
        merged = KeymapConfig::merge(merged, user);
    }

    merged
}

fn collect_mappings_by_action(config: &KeymapConfig) -> HashMap<String, Vec<String>> {
    let mut by_action: HashMap<String, Vec<String>> = HashMap::new();

    for (mode, bindings) in &config.keymap {
        for (keys, action) in bindings {
            by_action
                .entry(action.clone())
                .or_default()
                .push(format!("{mode}: {keys}"));
        }
    }

    for (mode, layers) in &config.keymap_context {
        for layer in layers {
            for (keys, action) in &layer.bindings {
                by_action
                    .entry(action.clone())
                    .or_default()
                    .push(format!("{mode} [{}]: {keys}", layer.when));
            }
        }
    }

    by_action
}

fn build_palette_entries(config: &KeymapConfig) -> Vec<PaletteEntry> {
    let mut actions: Vec<ActionDefinition> = session_actions::definitions();
    actions.extend(actions_standalone::common_action_definitions());

    let mappings = collect_mappings_by_action(config);
    let mut entries = Vec::with_capacity(actions.len());
    for action in actions {
        entries.push(PaletteEntry {
            id: action.id.as_str().to_string(),
            name: action.name,
            description: action.description,
            shortcut: action.shortcut_hint.unwrap_or_default(),
            when_clause: action.when.unwrap_or_default(),
            mappings: mappings
                .get(action.id.as_str())
                .cloned()
                .unwrap_or_default(),
        });
    }

    entries.sort_by(|a, b| a.name.cmp(&b.name));
    entries
}

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
                .add_directive("gateway_ws=info".parse().unwrap())
                .add_directive("signal2_ui=info".parse().unwrap())
                .add_directive("daw_control::fx=debug".parse().unwrap()),
        )
        .init();

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
        signal2_ui::register_panels(&mut registry);
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

    // Signal2 in-memory storage controller (same bootstrap as playground)
    let mut signal2_controller = use_signal(|| None::<SignalController>);
    use_effect(move || {
        spawn(async move {
            match bootstrap_in_memory_controller_async().await {
                Ok(ctrl) => {
                    provide_context(ctrl.clone());
                    signal2_controller.set(Some(ctrl));
                }
                Err(e) => tracing::error!("Failed to bootstrap signal2 storage: {e}"),
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
                            // Signal page: signal2 views
                            div {
                                class: "flex-1 overflow-hidden relative",
                                if let Some(ctrl) = signal2_controller() {
                                    Signal2View { controller: ctrl }
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
                                        if let Some(ctrl) = signal2_controller() {
                                            Signal2View { controller: ctrl }
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

fn handle_dock_preset_shortcut(e: &KeyboardEvent) -> bool {
    // Dock mode: F5-F9 switch screenset presets (no modifiers)
    if *DOCK_MODE.peek() {
        let mods = e.modifiers();
        if !mods.ctrl() && !mods.alt() && !mods.shift() && !mods.meta() {
            let preset_index = match e.key() {
                Key::F5 => Some(0),
                Key::F6 => Some(1),
                Key::F7 => Some(2),
                Key::F8 => Some(3),
                Key::F9 => Some(4),
                Key::F10 => Some(5),
                _ => None,
            };
            if let Some(idx) = preset_index {
                e.prevent_default();
                // Auto-save departing preset
                let current_layout = dock_dioxus::DOCK_LAYOUT.read().clone();
                let current_index = *dock_dioxus::DOCK_ACTIVE_PRESET_INDEX.read();
                {
                    let mut presets = dock_dioxus::DOCK_PRESETS.write();
                    if let Some(departing) = presets.presets.get_mut(current_index) {
                        departing.layout = current_layout;
                    }
                }
                // Load target preset
                let presets = dock_dioxus::DOCK_PRESETS.read();
                if let Some(preset) = presets.presets.get(idx) {
                    {
                        let mut workspace = dock_dioxus::DOCK_WORKSPACE.write();
                        let main_window = workspace.main_window;
                        if let Some(main) = workspace.windows.get_mut(&main_window) {
                            main.layout = preset.layout.clone();
                        }
                    }
                    *dock_dioxus::DOCK_LAYOUT.write() = preset.layout.clone();
                    *dock_dioxus::DOCK_ACTIVE_PRESET_INDEX.write() = idx;
                }
                return true;
            }
        }
    }
    false
}

fn dispatch_input_commands(commands: Vec<InputCommand>) -> bool {
    let mut handled = false;

    for command in commands {
        match command {
            InputCommand::Unhandled(_) => {}
            InputCommand::Action(action) => {
                dispatch_action(action.as_str());
                handled = true;
            }
            InputCommand::ActionWithArgs { action, .. } => {
                dispatch_action(action.as_str());
                handled = true;
            }
            InputCommand::SwitchMode(mode) | InputCommand::PushMode(mode) => {
                ACTION_CONTEXT.write().set_mode(mode.as_str());
                handled = true;
            }
            InputCommand::PopMode => {
                handled = true;
            }
            InputCommand::Pending { .. } => {
                handled = true;
            }
            InputCommand::InsertText(_) => {
                handled = true;
            }
        }
    }

    handled
}

fn dispatch_action(action_id: &str) {
    match action_id {
        id if id == session_actions::TOGGLE_PLAYBACK.as_str() => {
            spawn(async move {
                let _ = Session::get().setlist().toggle_playback().await;
            });
        }
        id if id == session_actions::TOGGLE_SONG_LOOP.as_str() => {
            spawn(async move {
                let _ = Session::get().setlist().toggle_song_loop().await;
            });
        }
        id if id == session_actions::SMART_NEXT.as_str() => {
            spawn(async move {
                let _ = Session::get().setlist().next_section().await;
            });
        }
        id if id == session_actions::SMART_PREVIOUS.as_str() => {
            spawn(async move {
                let _ = Session::get().setlist().previous_section().await;
            });
        }
        id if id == session_actions::NEXT_SONG.as_str() => {
            spawn(async move {
                let _ = Session::get().setlist().next_song().await;
            });
        }
        id if id == session_actions::PREVIOUS_SONG.as_str() => {
            spawn(async move {
                let _ = Session::get().setlist().previous_song().await;
            });
        }
        id if id == session_actions::NEXT_SECTION.as_str() => {
            spawn(async move {
                let _ = Session::get().setlist().next_section().await;
            });
        }
        id if id == session_actions::PREVIOUS_SECTION.as_str() => {
            spawn(async move {
                let _ = Session::get().setlist().previous_section().await;
            });
        }
        id if id == standalone_ids::COMMAND_PALETTE.as_str() => {
            *COMMAND_PALETTE_OPEN.write() = true;
            *COMMAND_PALETTE_QUERY.write() = String::new();
        }
        id if id == standalone_ids::TOGGLE_DARK_MODE.as_str() => {
            tracing::info!("Toggle dark mode triggered (not yet implemented)");
        }
        id if id == standalone_ids::OPEN_SETTINGS.as_str() => {
            tracing::info!("Open settings triggered (not yet implemented)");
        }
        _ => {
            tracing::debug!(action_id, "No handler registered for input action");
        }
    };
}

/// Performance view — renders the PerformanceLayout.
///
/// The chart preview is now a separate `ChartPreviewPanel` that can be placed
/// anywhere in the dock layout independently.
#[component]
fn PerformanceWithChartToggle() -> Element {
    rsx! {
        div {
            class: "relative h-full w-full bg-background",
            PerformanceLayout {}
        }
    }
}

// ---------------------------------------------------------------------------
// Signal2 view — Performance + Manage sub-tabs, browser dialog
// ---------------------------------------------------------------------------

/// Sub-tabs for the Signal page.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SignalTab {
    Performance,
    Manage,
    Editor,
}

/// Signal2 top-level view with Performance / Manage tabs and a Browser dialog.
#[component]
fn Signal2View(controller: SignalController) -> Element {
    let mut active_tab = use_signal(|| SignalTab::Manage);
    let mut browser_open = use_signal(|| false);

    rsx! {
        div { class: "h-full w-full flex flex-col bg-card overflow-hidden",
            // Toolbar: sub-tabs (left) + browser button (right)
            div { class: "flex items-center justify-between px-3 py-1.5 border-b border-border bg-zinc-900/60 flex-shrink-0",
                // Left: sub-tab pills
                div { class: "flex items-center gap-0.5 bg-zinc-800/80 rounded-lg p-0.5",
                    for (tab, label) in [(SignalTab::Performance, "Performance"), (SignalTab::Manage, "Manage"), (SignalTab::Editor, "Editor")] {
                        {
                            let is_active = active_tab() == tab;
                            rsx! {
                                button {
                                    class: if is_active {
                                        "px-3 py-1 text-xs font-medium text-white bg-zinc-700 rounded-md transition-colors"
                                    } else {
                                        "px-3 py-1 text-xs font-medium text-zinc-400 hover:text-zinc-200 hover:bg-zinc-800 rounded-md transition-colors"
                                    },
                                    onclick: move |_| active_tab.set(tab),
                                    "{label}"
                                }
                            }
                        }
                    }
                }

                // Right: browser button
                button {
                    class: "px-3 py-1 text-xs font-medium text-zinc-400 hover:text-zinc-200 hover:bg-zinc-800 rounded-md transition-colors",
                    onclick: move |_| browser_open.set(true),
                    "Browser"
                }
            }

            // Tab content
            div { class: "flex-1 min-h-0 overflow-hidden",
                match active_tab() {
                    SignalTab::Performance => rsx! {
                        Signal2PerformanceTab { controller: controller.clone() }
                    },
                    SignalTab::Manage => rsx! {
                        Signal2ManageTab { controller: controller.clone() }
                    },
                    SignalTab::Editor => rsx! {
                        Signal2EditorTab { controller: controller.clone() }
                    },
                }
            }

            // Browser dialog (near-full-screen)
            if browser_open() {
                Signal2BrowserDialog {
                    controller: controller.clone(),
                    on_close: move |_| browser_open.set(false),
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Performance tab
// ---------------------------------------------------------------------------

/// Performance tab — loads rig + song data and renders PerformanceView.
#[component]
fn Signal2PerformanceTab(controller: SignalController) -> Element {
    use signal2_ui::views::{
        PerfSceneTile, PerformanceView, RigStatus, SnapshotSlot, SongNavState,
    };

    let mut rig_data = use_signal(|| None::<signal2::rig::Rig>);
    let mut active_scene_id = use_signal(|| None::<String>);
    let mut morph_value = use_signal(|| 0.0_f64);
    let mut song_data = use_signal(|| None::<signal2::song::Song>);
    let mut section_index = use_signal(|| 0_usize);

    // Fetch first rig + first song
    {
        let controller = controller.clone();
        use_effect(move || {
            let controller = controller.clone();
            spawn(async move {
                let rigs = controller.list_rig_collections().await;
                if let Some(first) = rigs.first() {
                    if let Some(rig) = controller.load_rig_collection(first.id.to_string()).await {
                        if let Some(v) = rig.variants.first() {
                            active_scene_id.set(Some(v.id.to_string()));
                        }
                        rig_data.set(Some(rig));
                    }
                }
                let songs = controller.list_songs().await;
                if let Some(first) = songs.first() {
                    if let Some(song) = controller.load_song(first.id.to_string()).await {
                        song_data.set(Some(song));
                    }
                }
            });
        });
    }

    // Map rig → props
    let (status, scenes, snapshot_slots, morph_a, morph_b) = if let Some(rig) = rig_data() {
        let active_id = active_scene_id();
        let active_name = active_id
            .as_ref()
            .and_then(|id| rig.variants.iter().find(|v| v.id.to_string() == *id))
            .map(|v| v.name.clone())
            .unwrap_or_else(|| "None".to_string());

        let status = RigStatus {
            rig_name: rig.name.clone(),
            engine_count: rig.engine_ids.len(),
            layer_count: 0,
            active_scene_name: active_name,
        };

        let scenes: Vec<PerfSceneTile> = rig
            .variants
            .iter()
            .map(|v| PerfSceneTile {
                id: v.id.to_string(),
                name: v.name.clone(),
                is_active: active_id.as_deref() == Some(&v.id.to_string()),
                summary: format!("{} engines", v.engine_selections.len()),
            })
            .collect();

        let slots: Vec<SnapshotSlot> = (0..8)
            .map(|i| SnapshotSlot {
                index: i,
                name: None,
                is_a: i < 4,
                is_active: false,
            })
            .collect();

        let morph_a = scenes
            .first()
            .map(|s| s.name.clone())
            .unwrap_or_else(|| "A".to_string());
        let morph_b = scenes
            .get(1)
            .map(|s| s.name.clone())
            .unwrap_or_else(|| "B".to_string());

        (Some(status), scenes, slots, morph_a, morph_b)
    } else {
        (None, vec![], vec![], "A".to_string(), "B".to_string())
    };

    let song_nav = song_data().map(|song| {
        let idx = section_index();
        let section = song.sections.get(idx);
        SongNavState {
            song_name: song.name.clone(),
            section_name: section
                .map(|s| s.name.clone())
                .unwrap_or_else(|| "—".to_string()),
            section_index: idx,
            section_count: song.sections.len(),
            tempo: None,
            key_signature: None,
        }
    });

    rsx! {
        div { class: "h-full overflow-auto",
            if let Some(rig_status) = status {
                PerformanceView {
                    status: rig_status,
                    scenes,
                    morph_value: morph_value(),
                    morph_scene_a: morph_a,
                    morph_scene_b: morph_b,
                    snapshot_slots,
                    song_nav,
                    on_scene_select: move |id: String| { active_scene_id.set(Some(id)); },
                    on_morph_change: move |val: f64| { morph_value.set(val); },
                    on_prev_section: move |_| {
                        let idx = section_index();
                        if idx > 0 { section_index.set(idx - 1); }
                    },
                    on_next_section: move |_| {
                        let idx = section_index();
                        if let Some(song) = song_data() {
                            if idx + 1 < song.sections.len() { section_index.set(idx + 1); }
                        }
                    },
                }
            } else {
                div { class: "flex items-center justify-center h-full",
                    p { class: "text-sm text-muted-foreground", "Loading rig data..." }
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Manage tab — presets/profiles (left) | scene grid (center) | songs/sections (right)
// ---------------------------------------------------------------------------

/// Which editing mode the Manage tab is in.
#[derive(Clone, Copy, PartialEq, Eq)]
enum ManageMode {
    Preset,
    Profile,
    Song,
}

/// A preset item in the manage tab sidebar — either a Rig or Layer.
#[derive(Clone, PartialEq)]
struct ManagePresetItem {
    id: String,
    name: String,
    is_rig: bool,
    sub_items: Vec<(String, String)>, // (id, name) — scenes for rigs, variants for layers
}

/// A profile item in the manage tab sidebar — with expandable patches.
#[derive(Clone, PartialEq)]
struct ManageProfileItem {
    id: String,
    name: String,
    patches: Vec<(String, String)>, // (id, name)
}

#[component]
fn Signal2ManageTab(controller: SignalController) -> Element {
    use signal2::rig::RigType;
    use signal2::song::SectionSource;
    use signal2_ui::views::{
        engines_to_grid_slots, rig_type_to_engine_type, EngineFlowData, EngineParamLookup,
        RigGridPanel, SectionEntry, SongEditor, SongEntry,
    };

    // Rig type selector — filters presets, default Guitar (matches browser)
    let mut rig_type = use_signal(|| RigType::Guitar);
    // Manage mode — controls which panels are visible
    let mut manage_mode = use_signal(|| ManageMode::Song);

    let mut manage_profiles = use_signal(Vec::<ManageProfileItem>::new);
    let mut expanded_profile_ids = use_signal(std::collections::HashSet::<String>::new);
    let mut selected_profile = use_signal(|| None::<String>);
    let mut selected_patch = use_signal(|| None::<String>);

    // Combined preset list (rigs + layers), each with expandable sub-items
    let mut manage_presets = use_signal(Vec::<ManagePresetItem>::new);
    let mut expanded_ids = use_signal(std::collections::HashSet::<String>::new);
    // Currently selected parent preset (rig or layer ID)
    let mut selected_preset_id = use_signal(|| None::<String>);
    // Currently selected sub-item (scene or variant ID)
    let mut selected_sub_id = use_signal(|| None::<String>);

    // Setlist dropdown: list of (id, name) options. First entry is always "All Songs" (id="all").
    let mut setlist_options = use_signal(Vec::<(String, String)>::new);
    let mut selected_setlist_id = use_signal(|| "all".to_string());

    let mut songs = use_signal(Vec::<SongEntry>::new);
    let mut selected_song_id = use_signal(|| None::<String>);
    let mut song_sections = use_signal(Vec::<SectionEntry>::new);
    let mut selected_section_id = use_signal(|| None::<String>);
    let mut active_song_name = use_signal(|| "Songs".to_string());

    // Maps section_id → SectionSource for navigation and assignment
    let mut section_sources = use_signal(std::collections::HashMap::<String, SectionSource>::new);
    // Maps patch_id → (rig_id, scene_id) so section navigation via Patch doesn't need async
    let mut patch_rig_map = use_signal(std::collections::HashMap::<String, (String, String)>::new);
    // Maps patch_id → display label ("RigName / SceneName") for the profile browser
    let mut patch_display_labels = use_signal(std::collections::HashMap::<String, String>::new);

    // Track whether the active selection is a rig (true) or layer (false)
    let mut active_is_rig = use_signal(|| true);
    let mut rig_id = use_signal(|| None::<String>);
    let mut active_scene_id = use_signal(|| None::<String>);
    // Scenes for the currently selected rig preset (id, name) — for scene tabs
    let mut rig_scenes = use_signal(Vec::<(String, String)>::new);

    // Resolved engine flow data + param lookup for the grid panel (center)
    let mut canvas_engines = use_signal(Vec::<EngineFlowData>::new);
    let mut canvas_params = use_signal(EngineParamLookup::new);

    /// Build a SectionEntry with resolved source labels, and track its source.
    fn build_section_entry(
        sec: &signal2::song::Section,
        presets: &[ManagePresetItem],
        profiles: &[ManageProfileItem],
    ) -> (SectionEntry, SectionSource) {
        let source = sec.source.clone();
        let (rig_scene_name, profile_patch_name) = match &source {
            SectionSource::RigScene { rig_id, scene_id } => {
                let label = presets
                    .iter()
                    .find(|p| p.id == rig_id.to_string())
                    .and_then(|p| {
                        let scene_str = scene_id.to_string();
                        p.sub_items
                            .iter()
                            .find(|(sid, _)| *sid == scene_str)
                            .map(|(_, sname)| format!("{} / {}", p.name, sname))
                    });
                (label, None)
            }
            SectionSource::Patch { patch_id } => {
                let patch_str = patch_id.to_string();
                let label = profiles.iter().find_map(|prof| {
                    prof.patches
                        .iter()
                        .find(|(pid, _)| *pid == patch_str)
                        .map(|(_, pname)| format!("{} / {}", prof.name, pname))
                });
                (None, label)
            }
        };
        let entry = SectionEntry {
            id: sec.id.to_string(),
            name: sec.name.clone(),
            rig_scene_name,
            profile_patch_name,
            tempo: None,
            key_signature: None,
            notes: None,
        };
        (entry, source)
    }

    // Unified effect: re-runs when rig_type changes.
    // Loads rigs + layers as combined preset list, profiles, and songs.
    {
        let controller = controller.clone();
        use_effect(move || {
            let controller = controller.clone();
            let rt = rig_type();
            // Reset all state
            selected_preset_id.set(None);
            selected_sub_id.set(None);
            selected_profile.set(None);
            selected_patch.set(None);
            selected_song_id.set(None);
            selected_section_id.set(None);
            active_is_rig.set(true);
            rig_id.set(None);
            active_scene_id.set(None);
            rig_scenes.set(Vec::new());
            canvas_engines.set(Vec::new());
            canvas_params.set(EngineParamLookup::new());
            song_sections.set(Vec::new());
            active_song_name.set("Songs".to_string());
            expanded_ids.set(std::collections::HashSet::new());
            expanded_profile_ids.set(std::collections::HashSet::new());

            spawn(async move {
                // 1) Rigs filtered by type
                let rigs = controller.list_rig_collections().await;
                let filtered: Vec<_> = rigs
                    .into_iter()
                    .filter(|r| r.rig_type.map_or(false, |t| t == rt))
                    .collect();
                let rig_id_set: std::collections::HashSet<String> =
                    filtered.iter().map(|r| r.id.to_string()).collect();

                // 2) Layers filtered by engine type
                let et = rig_type_to_engine_type(rt);
                let all_layers = controller.list_layers().await;
                let matching_layers: Vec<_> = all_layers
                    .into_iter()
                    .filter(|l| l.engine_type == et)
                    .collect();

                // Build combined preset list: rigs first, then layers
                let mut items: Vec<ManagePresetItem> = Vec::new();
                for r in &filtered {
                    items.push(ManagePresetItem {
                        id: r.id.to_string(),
                        name: r.name.clone(),
                        is_rig: true,
                        sub_items: r
                            .variants
                            .iter()
                            .map(|v| (v.id.to_string(), v.name.clone()))
                            .collect(),
                    });
                }
                for l in &matching_layers {
                    items.push(ManagePresetItem {
                        id: l.id.to_string(),
                        name: l.name.clone(),
                        is_rig: false,
                        sub_items: l
                            .variants
                            .iter()
                            .map(|v| (v.id.to_string(), v.name.clone()))
                            .collect(),
                    });
                }
                manage_presets.set(items);

                // 3) Profiles — keep only those with patches targeting a rig of this type
                let all_profiles = controller.list_profiles().await;
                let matching_profiles: Vec<_> = all_profiles
                    .into_iter()
                    .filter(|p| {
                        p.patches
                            .iter()
                            .any(|patch| rig_id_set.contains(patch.rig_id.as_str()))
                    })
                    .collect();
                manage_profiles.set(
                    matching_profiles
                        .iter()
                        .map(|p| ManageProfileItem {
                            id: p.id.to_string(),
                            name: p.name.clone(),
                            patches: p
                                .patches
                                .iter()
                                .map(|patch| (patch.id.to_string(), patch.name.clone()))
                                .collect(),
                        })
                        .collect(),
                );
                // Cache patch → (rig_id, scene_id) for fast section navigation
                // Also build display labels: patch_id → "RigName / SceneName"
                let preset_items = manage_presets();
                let mut prm = std::collections::HashMap::new();
                let mut labels = std::collections::HashMap::new();
                for p in &matching_profiles {
                    for patch in &p.patches {
                        let rig_str = patch.rig_id.to_string();
                        let scene_str = patch.rig_variant_id.to_string();
                        prm.insert(patch.id.to_string(), (rig_str.clone(), scene_str.clone()));
                        let label = preset_items
                            .iter()
                            .find(|pi| pi.id == rig_str)
                            .and_then(|pi| {
                                pi.sub_items
                                    .iter()
                                    .find(|(sid, _)| *sid == scene_str)
                                    .map(|(_, sname)| format!("{} / {}", pi.name, sname))
                            })
                            .unwrap_or_else(|| "Unlinked".to_string());
                        labels.insert(patch.id.to_string(), label);
                    }
                }
                patch_rig_map.set(prm);
                patch_display_labels.set(labels);

                // 4) Setlists — build dropdown options + "All Songs" union
                let all_setlists = controller.list_setlists().await;
                let mut opts: Vec<(String, String)> =
                    vec![("all".to_string(), "All Songs".to_string())];
                for sl in &all_setlists {
                    opts.push((sl.id.to_string(), sl.name.clone()));
                }
                setlist_options.set(opts);
                selected_setlist_id.set("all".to_string());

                // "All Songs" = union of all songs from all setlists (deduped)
                let mut seen_song_ids = std::collections::HashSet::new();
                let mut all_union_songs = Vec::new();
                for sl in &all_setlists {
                    for entry in &sl.entries {
                        let sid = entry.song_id.to_string();
                        if seen_song_ids.insert(sid.clone()) {
                            if let Some(song) = controller.load_song(sid).await {
                                all_union_songs.push(song);
                            }
                        }
                    }
                }
                songs.set(
                    all_union_songs
                        .iter()
                        .map(|s| SongEntry {
                            id: s.id.to_string(),
                            name: s.name.clone(),
                            section_count: s.sections.len(),
                            duration_display: None,
                        })
                        .collect(),
                );

                // Auto-select first song
                let cur_presets = manage_presets();
                let cur_profiles = manage_profiles();
                if let Some(first_song) = all_union_songs.first() {
                    active_song_name.set(first_song.name.clone());
                    selected_song_id.set(Some(first_song.id.to_string()));
                    let mut entries = Vec::new();
                    let mut sources = std::collections::HashMap::new();
                    for sec in &first_song.sections {
                        let (entry, source) = build_section_entry(sec, &cur_presets, &cur_profiles);
                        sources.insert(entry.id.clone(), source);
                        entries.push(entry);
                    }
                    song_sections.set(entries);
                    section_sources.set(sources);
                }

                // 5) Auto-select first rig preset, expand it, resolve its first scene
                if let Some(first) = filtered.first() {
                    let first_id = first.id.to_string();
                    rig_id.set(Some(first_id.clone()));
                    selected_preset_id.set(Some(first_id.clone()));
                    active_is_rig.set(true);
                    expanded_ids.set([first_id.clone()].into_iter().collect());
                    rig_scenes.set(
                        first
                            .variants
                            .iter()
                            .map(|v| (v.id.to_string(), v.name.clone()))
                            .collect(),
                    );

                    if let Some(first_scene) = first.variants.first() {
                        let scene_id = first_scene.id.to_string();
                        active_scene_id.set(Some(scene_id.clone()));
                        selected_sub_id.set(Some(scene_id.clone()));
                        if let Some((engines, params)) = signal2_ui::views::resolve_scene_engines(
                            &controller,
                            &first_id,
                            &scene_id,
                        )
                        .await
                        {
                            canvas_engines.set(engines);
                            canvas_params.set(params);
                        }
                    }
                }
            });
        });
    }

    // Handle song selection — load that song's sections
    let load_song_sections = {
        let controller = controller.clone();
        move |song_id: String| {
            let controller = controller.clone();
            selected_song_id.set(Some(song_id.clone()));
            selected_section_id.set(None);
            spawn(async move {
                if let Some(song) = controller.load_song(song_id).await {
                    active_song_name.set(song.name.clone());
                    let cur_presets = manage_presets();
                    let cur_profiles = manage_profiles();
                    let mut entries = Vec::new();
                    let mut sources = std::collections::HashMap::new();
                    for sec in &song.sections {
                        let (entry, source) = build_section_entry(sec, &cur_presets, &cur_profiles);
                        sources.insert(entry.id.clone(), source);
                        entries.push(entry);
                    }
                    song_sections.set(entries);
                    section_sources.set(sources);
                }
            });
        }
    };

    // Handle setlist dropdown change — filter songs by setlist entries
    let change_setlist = {
        let controller = controller.clone();
        move |setlist_id: String| {
            let controller = controller.clone();
            selected_setlist_id.set(setlist_id.clone());
            selected_song_id.set(None);
            selected_section_id.set(None);
            song_sections.set(Vec::new());
            active_song_name.set("Songs".to_string());
            spawn(async move {
                if setlist_id == "all" {
                    // "All Songs" = union of all songs from all setlists (deduped)
                    let all_setlists = controller.list_setlists().await;
                    let mut seen_ids = std::collections::HashSet::new();
                    let mut all_union = Vec::new();
                    for sl in &all_setlists {
                        for entry in &sl.entries {
                            let sid = entry.song_id.to_string();
                            if seen_ids.insert(sid.clone()) {
                                if let Some(song) = controller.load_song(sid).await {
                                    all_union.push(song);
                                }
                            }
                        }
                    }
                    songs.set(
                        all_union
                            .iter()
                            .map(|s| SongEntry {
                                id: s.id.to_string(),
                                name: s.name.clone(),
                                section_count: s.sections.len(),
                                duration_display: None,
                            })
                            .collect(),
                    );
                    if let Some(first) = all_union.first() {
                        selected_song_id.set(Some(first.id.to_string()));
                        active_song_name.set(first.name.clone());
                        let cur_presets = manage_presets();
                        let cur_profiles = manage_profiles();
                        let mut entries = Vec::new();
                        let mut sources = std::collections::HashMap::new();
                        for sec in &first.sections {
                            let (entry, source) =
                                build_section_entry(sec, &cur_presets, &cur_profiles);
                            sources.insert(entry.id.clone(), source);
                            entries.push(entry);
                        }
                        song_sections.set(entries);
                        section_sources.set(sources);
                    }
                } else if let Some(setlist) = controller.load_setlist(setlist_id).await {
                    // Load songs from this specific setlist
                    let mut song_entries = Vec::new();
                    let mut first_song = None;
                    for entry in &setlist.entries {
                        if let Some(song) = controller.load_song(entry.song_id.clone()).await {
                            song_entries.push(SongEntry {
                                id: song.id.to_string(),
                                name: song.name.clone(),
                                section_count: song.sections.len(),
                                duration_display: None,
                            });
                            if first_song.is_none() {
                                first_song = Some(song);
                            }
                        }
                    }
                    songs.set(song_entries);
                    if let Some(song) = first_song {
                        selected_song_id.set(Some(song.id.to_string()));
                        active_song_name.set(song.name.clone());
                        let cur_presets = manage_presets();
                        let cur_profiles = manage_profiles();
                        let mut entries = Vec::new();
                        let mut sources = std::collections::HashMap::new();
                        for sec in &song.sections {
                            let (entry, source) =
                                build_section_entry(sec, &cur_presets, &cur_profiles);
                            sources.insert(entry.id.clone(), source);
                            entries.push(entry);
                        }
                        song_sections.set(entries);
                        section_sources.set(sources);
                    }
                }
            });
        }
    };

    // Handle preset parent click — toggle expand, auto-select first sub-item
    let select_preset = {
        let controller = controller.clone();
        move |item_id: String| {
            let controller = controller.clone();

            // Find the item in the current preset list
            let items = manage_presets();
            let Some(item) = items.iter().find(|i| i.id == item_id).cloned() else {
                return;
            };

            // Always expand (never collapse on click — collapse via a dedicated toggle)
            let mut exp = expanded_ids();
            exp.insert(item_id.clone());
            expanded_ids.set(exp);

            // Select this parent
            selected_preset_id.set(Some(item_id.clone()));
            active_is_rig.set(item.is_rig);

            if item.is_rig {
                rig_id.set(Some(item_id.clone()));
                rig_scenes.set(item.sub_items.clone());
            } else {
                rig_id.set(None);
                rig_scenes.set(Vec::new());
            }

            // Auto-select and load first sub-item
            if let Some((first_sub_id, _)) = item.sub_items.first() {
                let sub_id = first_sub_id.clone();
                selected_sub_id.set(Some(sub_id.clone()));
                if item.is_rig {
                    active_scene_id.set(Some(sub_id.clone()));
                } else {
                    active_scene_id.set(None);
                }

                // Clear stale data so the grid doesn't remount with old engines
                canvas_engines.set(Vec::new());
                canvas_params.set(EngineParamLookup::new());

                let is_rig = item.is_rig;
                spawn(async move {
                    let result = if is_rig {
                        signal2_ui::views::resolve_scene_engines(&controller, &item_id, &sub_id)
                            .await
                    } else {
                        signal2_ui::views::resolve_layer_engines(
                            &controller,
                            &item_id,
                            Some(&sub_id),
                        )
                        .await
                    };
                    if let Some((engines, params)) = result {
                        canvas_engines.set(engines);
                        canvas_params.set(params);
                    }
                });
            } else {
                selected_sub_id.set(None);
                active_scene_id.set(None);
                canvas_engines.set(Vec::new());
                canvas_params.set(EngineParamLookup::new());
            }
        }
    };

    // Handle sub-item click — resolve that scene/variant's engines for the canvas
    let select_sub_item = {
        let controller = controller.clone();
        move |parent_id: String, sub_id: String, is_rig: bool| {
            let controller = controller.clone();
            selected_sub_id.set(Some(sub_id.clone()));
            if is_rig {
                active_scene_id.set(Some(sub_id.clone()));
            }
            // Clear stale data so the grid doesn't remount with old engines
            canvas_engines.set(Vec::new());
            canvas_params.set(EngineParamLookup::new());
            spawn(async move {
                let result = if is_rig {
                    signal2_ui::views::resolve_scene_engines(&controller, &parent_id, &sub_id).await
                } else {
                    signal2_ui::views::resolve_layer_engines(&controller, &parent_id, Some(&sub_id))
                        .await
                };
                if let Some((engines, params)) = result {
                    canvas_engines.set(engines);
                    canvas_params.set(params);
                }
            });
        }
    };

    // Assign a rig scene to the active section (Song mode) or patch (Profile mode).
    let assign_current_section = {
        let controller = controller.clone();
        move |parent_id: String, sub_id: String, _is_rig: bool| {
            let mode = manage_mode();
            let controller = controller.clone();

            if mode == ManageMode::Song {
                if let Some(sec_id) = selected_section_id() {
                    let new_source = SectionSource::RigScene {
                        rig_id: parent_id.clone().into(),
                        scene_id: sub_id.clone().into(),
                    };
                    // Update local source map
                    let mut sources = section_sources();
                    sources.insert(sec_id.clone(), new_source.clone());
                    section_sources.set(sources);
                    // Update display label
                    let preset_label = manage_presets()
                        .iter()
                        .find(|p| p.id == parent_id)
                        .and_then(|p| {
                            p.sub_items
                                .iter()
                                .find(|(sid, _)| *sid == sub_id)
                                .map(|(_, sname)| format!("{} / {}", p.name, sname))
                        });
                    let mut sections = song_sections();
                    if let Some(entry) = sections.iter_mut().find(|e| e.id == sec_id) {
                        entry.rig_scene_name = preset_label;
                        entry.profile_patch_name = None;
                    }
                    song_sections.set(sections);
                    // Persist
                    if let Some(song_id) = selected_song_id() {
                        spawn(async move {
                            controller
                                .set_section_source(song_id, sec_id, new_source)
                                .await;
                        });
                    }
                }
            } else if mode == ManageMode::Profile {
                // Assign rig/scene to the currently selected profile patch
                if let (Some(prof_id), Some(patch_id_str)) = (selected_profile(), selected_patch())
                {
                    let mut prm = patch_rig_map();
                    prm.insert(patch_id_str.clone(), (parent_id.clone(), sub_id.clone()));
                    patch_rig_map.set(prm);
                    // Update display label for this patch
                    let new_label = manage_presets()
                        .iter()
                        .find(|p| p.id == parent_id)
                        .and_then(|p| {
                            p.sub_items
                                .iter()
                                .find(|(sid, _)| *sid == sub_id)
                                .map(|(_, sname)| format!("{} / {}", p.name, sname))
                        })
                        .unwrap_or_else(|| "Unlinked".to_string());
                    let mut lbls = patch_display_labels();
                    lbls.insert(patch_id_str.clone(), new_label);
                    patch_display_labels.set(lbls);
                    spawn(async move {
                        controller
                            .set_patch_preset(prof_id, patch_id_str, parent_id, sub_id)
                            .await;
                    });
                }
            }
            // Preset mode: no assignment
        }
    };

    // Navigate to a section's assigned preset/scene (without re-assigning).
    let navigate_to_section = {
        let controller = controller.clone();
        move |section_id: String| {
            let controller = controller.clone();
            selected_section_id.set(Some(section_id.clone()));
            let Some(source) = section_sources().get(&section_id).cloned() else {
                return;
            };
            // Resolve the rig_id + scene_id to navigate to
            let (rid_str, sid_str) = match &source {
                SectionSource::RigScene { rig_id, scene_id } => {
                    (rig_id.to_string(), scene_id.to_string())
                }
                SectionSource::Patch { patch_id } => {
                    let pid = patch_id.to_string();
                    // Select profile & patch in the sidebar
                    for prof in manage_profiles().iter() {
                        if prof.patches.iter().any(|(id, _)| *id == pid) {
                            let mut exp = expanded_profile_ids();
                            exp.insert(prof.id.clone());
                            expanded_profile_ids.set(exp);
                            selected_profile.set(Some(prof.id.clone()));
                            selected_patch.set(Some(pid.clone()));
                            break;
                        }
                    }
                    // Look up the patch's underlying rig/scene
                    match patch_rig_map().get(&pid).cloned() {
                        Some(pair) => pair,
                        None => return,
                    }
                }
            };
            // Set all signals synchronously so only one async resolve fires
            let is_rig = manage_presets()
                .iter()
                .find(|p| p.id == rid_str)
                .map_or(true, |p| p.is_rig);

            selected_preset_id.set(Some(rid_str.clone()));
            active_is_rig.set(is_rig);
            selected_sub_id.set(Some(sid_str.clone()));

            if is_rig {
                rig_id.set(Some(rid_str.clone()));
                active_scene_id.set(Some(sid_str.clone()));
                if let Some(item) = manage_presets().iter().find(|p| p.id == rid_str) {
                    rig_scenes.set(item.sub_items.clone());
                }
            } else {
                rig_id.set(None);
                active_scene_id.set(None);
                rig_scenes.set(Vec::new());
            }

            let mut exp = expanded_ids();
            exp.insert(rid_str.clone());
            expanded_ids.set(exp);
            canvas_engines.set(Vec::new());
            canvas_params.set(EngineParamLookup::new());
            spawn(async move {
                let result = if is_rig {
                    signal2_ui::views::resolve_scene_engines(&controller, &rid_str, &sid_str).await
                } else {
                    signal2_ui::views::resolve_layer_engines(&controller, &rid_str, Some(&sid_str))
                        .await
                };
                if let Some((engines, params)) = result {
                    canvas_engines.set(engines);
                    canvas_params.set(params);
                }
            });
        }
    };

    let current_preset = selected_preset_id();
    let current_sub = selected_sub_id();
    let current_scene = active_scene_id();
    let scenes = rig_scenes();
    let is_rig_active = active_is_rig();

    let mode = manage_mode();

    // ── Breadcrumb: resolve current context chain ──
    let bc_song = if mode == ManageMode::Song {
        selected_song_id()
            .and_then(|sid| songs().iter().find(|s| s.id == sid).map(|s| s.name.clone()))
    } else {
        None
    };
    let bc_section = if mode == ManageMode::Song {
        selected_section_id().and_then(|sid| {
            song_sections()
                .iter()
                .find(|s| s.id == sid)
                .map(|s| s.name.clone())
        })
    } else {
        None
    };
    // Profile/Patch: resolve from section source (if Patch), else from direct selection
    let (bc_profile, bc_patch) = {
        // First try: from current section's source
        let from_source = selected_section_id()
            .and_then(|sid| section_sources().get(&sid).cloned())
            .and_then(|source| match source {
                SectionSource::Patch { patch_id } => {
                    let patch_str = patch_id.to_string();
                    manage_profiles().iter().find_map(|prof| {
                        prof.patches
                            .iter()
                            .find(|(pid, _)| *pid == patch_str)
                            .map(|(_, pname)| (Some(prof.name.clone()), Some(pname.clone())))
                    })
                }
                _ => None,
            });
        // Fallback: from directly selected profile/patch signals
        from_source.unwrap_or_else(|| {
            let prof_name = selected_profile().and_then(|pid| {
                manage_profiles()
                    .iter()
                    .find(|p| p.id == pid)
                    .map(|p| p.name.clone())
            });
            let patch_name = selected_patch().and_then(|patch_id| {
                manage_profiles().iter().find_map(|prof| {
                    prof.patches
                        .iter()
                        .find(|(pid, _)| *pid == patch_id)
                        .map(|(_, pname)| pname.clone())
                })
            });
            (prof_name, patch_name)
        })
    };
    let bc_preset = current_preset.as_ref().and_then(|pid| {
        manage_presets()
            .iter()
            .find(|p| &p.id == pid)
            .map(|p| p.name.clone())
    });
    let bc_scene = current_preset.as_ref().and_then(|pid| {
        current_sub.as_ref().and_then(|sid| {
            manage_presets()
                .iter()
                .find(|p| &p.id == pid)
                .and_then(|p| {
                    p.sub_items
                        .iter()
                        .find(|(id, _)| id == sid)
                        .map(|(_, n)| n.clone())
                })
        })
    });

    rsx! {
        div { class: "flex flex-col h-full overflow-hidden",
            // ── Top bar: Mode tabs + Rig type selector + Scene tabs ──
            div { class: "flex items-center gap-2 px-3 py-1.5 border-b border-border bg-zinc-900/40 flex-shrink-0 overflow-x-auto",
                // Mode tabs
                for &(m, label) in &[(ManageMode::Song, "Song"), (ManageMode::Profile, "Profile"), (ManageMode::Preset, "Preset")] {
                    {
                        let is_active = mode == m;
                        rsx! {
                            button {
                                key: "{label}",
                                class: if is_active {
                                    "px-2.5 py-1 text-xs font-semibold rounded bg-zinc-600 text-zinc-100"
                                } else {
                                    "px-2.5 py-1 text-xs text-zinc-400 hover:text-zinc-200 hover:bg-zinc-800 rounded"
                                },
                                onclick: move |_| manage_mode.set(m),
                                "{label}"
                            }
                        }
                    }
                }

                // Divider
                div { class: "w-px h-4 bg-zinc-700 mx-1 flex-shrink-0" }

                // Rig type selector
                span { class: "text-[10px] text-zinc-500 mr-1 flex-shrink-0", "Rig:" }
                for &rt in &[RigType::Guitar, RigType::Bass, RigType::Keys, RigType::Vocals] {
                    {
                        let is_active = rig_type() == rt;
                        let label = match rt {
                            RigType::Guitar => "Guitar",
                            RigType::Bass => "Bass",
                            RigType::Keys => "Keys",
                            RigType::Vocals => "Vocals",
                            _ => "Other",
                        };
                        rsx! {
                            button {
                                key: "{label}",
                                class: if is_active {
                                    "px-2 py-0.5 text-[11px] font-medium rounded bg-zinc-600 text-zinc-100"
                                } else {
                                    "px-2 py-0.5 text-[11px] text-zinc-500 hover:text-zinc-300 hover:bg-zinc-800 rounded"
                                },
                                onclick: move |_| rig_type.set(rt),
                                "{label}"
                            }
                        }
                    }
                }

                // Scene tabs (only shown for rig presets)
                if is_rig_active && !scenes.is_empty() {
                    div { class: "w-px h-4 bg-zinc-700 mx-1 flex-shrink-0" }
                    span { class: "text-[10px] text-zinc-500 mr-1 flex-shrink-0", "Scenes" }
                    for (sid, sname) in scenes.iter() {
                        {
                            let is_active = current_scene.as_deref() == Some(sid.as_str());
                            let scene_id = sid.clone();
                            let rid = rig_id().unwrap_or_default();
                            let mut on_click = select_sub_item.clone();
                            let mut on_assign = assign_current_section.clone();
                            rsx! {
                                button {
                                    key: "{sid}",
                                    class: if is_active {
                                        "px-2.5 py-1 text-xs font-medium rounded bg-zinc-700 text-zinc-100"
                                    } else {
                                        "px-2.5 py-1 text-xs text-zinc-400 hover:text-zinc-200 hover:bg-zinc-800 rounded"
                                    },
                                    onclick: move |_| {
                                        on_click(rid.clone(), scene_id.clone(), true);
                                        on_assign(rid.clone(), scene_id.clone(), true);
                                    },
                                    "{sname}"
                                }
                            }
                        }
                    }
                }
            }

            // ── Breadcrumb context row ──
            if mode != ManageMode::Preset {
                div { class: "flex items-center gap-1 px-3 py-1 border-b border-border/50 bg-zinc-950/30 flex-shrink-0 text-[10px]",
                    if let Some(ref name) = bc_song {
                        span { class: "text-zinc-500", "Song:" }
                        span { class: "text-zinc-300 mr-2", "{name}" }
                    }
                    if let Some(ref name) = bc_section {
                        span { class: "text-zinc-600", "\u{203A}" }
                        span { class: "text-zinc-500 ml-1", "Section:" }
                        span { class: "text-zinc-300 mr-2", "{name}" }
                    }
                    if let Some(ref name) = bc_profile {
                        span { class: "text-zinc-600", "\u{203A}" }
                        span { class: "text-zinc-500 ml-1", "Profile:" }
                        span { class: "text-zinc-300 mr-2", "{name}" }
                    }
                    if let Some(ref name) = bc_patch {
                        span { class: "text-zinc-600", "\u{203A}" }
                        span { class: "text-zinc-500 ml-1", "Patch:" }
                        span { class: "text-zinc-300 mr-2", "{name}" }
                    }
                    if let Some(ref name) = bc_preset {
                        span { class: "text-zinc-600", "\u{203A}" }
                        span { class: "text-zinc-500 ml-1", "Preset:" }
                        span { class: "text-zinc-300 mr-2", "{name}" }
                    }
                    if let Some(ref name) = bc_scene {
                        span { class: "text-zinc-600", "\u{203A}" }
                        span { class: "text-zinc-500 ml-1", "Scene:" }
                        span { class: "text-zinc-300", "{name}" }
                    }
                    if bc_song.is_none() && bc_preset.is_none() {
                        span { class: "text-zinc-600 italic", "No selection" }
                    }
                }
            }

            // ── Body: mode-dependent panel layout ──
            div { class: "flex flex-1 min-h-0 overflow-hidden",
                // ── Left panel: Presets (always) + Profiles (Song mode only) ──
                div { class: "w-56 flex-shrink-0 border-r border-border flex flex-col min-h-0 bg-zinc-950/40",
                    // Preset list — fills all space in Preset/Profile mode, top ~60% in Song mode
                    div {
                        class: if mode == ManageMode::Song {
                            "flex-[3] min-h-0 flex flex-col border-b border-border"
                        } else {
                            "flex-1 min-h-0 flex flex-col"
                        },
                        div { class: "px-3 py-2 border-b border-border flex-shrink-0",
                            h3 { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider", "Presets" }
                        }
                        div { class: "flex-1 overflow-y-auto",
                            for item in manage_presets().iter().cloned() {
                                {
                                    let is_sel = current_preset.as_deref() == Some(item.id.as_str());
                                    let is_expanded = expanded_ids().contains(&item.id);
                                    let item_key = item.id.clone();
                                    let item_click_id = item.id.clone();
                                    let first_sub = item.sub_items.first().map(|(sid, _)| sid.clone());
                                    let is_rig = item.is_rig;
                                    let mut on_select = select_preset.clone();
                                    let mut on_assign = assign_current_section.clone();
                                    rsx! {
                                        div { key: "{item_key}",
                                            button {
                                                class: if is_sel {
                                                    "w-full text-left px-3 py-2 border-b border-zinc-800/50 bg-zinc-700/60"
                                                } else {
                                                    "w-full text-left px-3 py-2 border-b border-zinc-800/50 hover:bg-zinc-800/60"
                                                },
                                                onclick: move |_| {
                                                    on_select(item_click_id.clone());
                                                    // Auto-assign the default (first) scene/snapshot
                                                    if let Some(ref sub_id) = first_sub {
                                                        on_assign(item_click_id.clone(), sub_id.clone(), is_rig);
                                                    }
                                                },
                                                div { class: "flex items-center gap-1.5",
                                                    span { class: "text-[10px] text-zinc-500 w-3 flex-shrink-0",
                                                        if is_expanded { "\u{25BE}" } else { "\u{25B8}" }
                                                    }
                                                    span {
                                                        class: if item.is_rig {
                                                            "text-[9px] px-1 rounded bg-zinc-600 text-zinc-300 flex-shrink-0"
                                                        } else {
                                                            "text-[9px] px-1 rounded bg-zinc-700 text-zinc-400 flex-shrink-0"
                                                        },
                                                        if item.is_rig { "RIG" } else { "LYR" }
                                                    }
                                                    span { class: "text-sm text-zinc-200 truncate flex-1", "{item.name}" }
                                                    span { class: "text-[10px] text-zinc-500 flex-shrink-0",
                                                        "{item.sub_items.len()}"
                                                    }
                                                }
                                            }
                                            if is_expanded {
                                                for (sub_id, sub_name) in item.sub_items.iter() {
                                                    {
                                                        let is_sub_sel = current_sub.as_deref() == Some(sub_id.as_str());
                                                        let parent_id = item.id.clone();
                                                        let sub_id_click = sub_id.clone();
                                                        let is_rig = item.is_rig;
                                                        let mut on_sub = select_sub_item.clone();
                                                        let mut on_assign = assign_current_section.clone();
                                                        rsx! {
                                                            button {
                                                                key: "{sub_id}",
                                                                class: if is_sub_sel {
                                                                    "w-full text-left pl-8 pr-3 py-1.5 text-xs bg-zinc-700/40 text-zinc-200 border-b border-zinc-800/30"
                                                                } else {
                                                                    "w-full text-left pl-8 pr-3 py-1.5 text-xs text-zinc-400 hover:bg-zinc-800/40 hover:text-zinc-300 border-b border-zinc-800/30"
                                                                },
                                                                onclick: move |_| {
                                                                    on_sub(parent_id.clone(), sub_id_click.clone(), is_rig);
                                                                    on_assign(parent_id.clone(), sub_id_click.clone(), is_rig);
                                                                },
                                                                "{sub_name}"
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

                    // Profiles on the left — only in Song mode
                    if mode == ManageMode::Song {
                        {
                            let ctrl = controller.clone();
                            // Signal is Copy — rebind as mut inside Fn closure to call .set()
                            let song_patch_cb: Rc<dyn Fn(String, String)> = Rc::new(move |prof_id: String, patch_id: String| {
                                let mut section_sources = section_sources;
                                let mut song_sections = song_sections;
                                let mut selected_preset_id = selected_preset_id;
                                let mut active_is_rig = active_is_rig;
                                let mut rig_id = rig_id;
                                let mut selected_sub_id = selected_sub_id;
                                let mut active_scene_id = active_scene_id;
                                let mut expanded_ids = expanded_ids;
                                let mut rig_scenes = rig_scenes;
                                let mut canvas_engines = canvas_engines;
                                let mut canvas_params = canvas_params;
                                // 1) Assign section → Patch source
                                if let (Some(sec_id), Some(song_id)) = (selected_section_id(), selected_song_id()) {
                                    let new_source = SectionSource::Patch { patch_id: patch_id.clone().into() };
                                    let mut sources = section_sources();
                                    sources.insert(sec_id.clone(), new_source.clone());
                                    section_sources.set(sources);
                                    let label = manage_profiles().iter().find_map(|prof| {
                                        if prof.id == prof_id {
                                            prof.patches.iter()
                                                .find(|(pid, _)| *pid == patch_id)
                                                .map(|(_, pname)| format!("{} / {}", prof.name, pname))
                                        } else { None }
                                    });
                                    let mut sections = song_sections();
                                    if let Some(entry) = sections.iter_mut().find(|e| e.id == sec_id) {
                                        entry.profile_patch_name = label;
                                        entry.rig_scene_name = None;
                                    }
                                    song_sections.set(sections);
                                    let c = ctrl.clone();
                                    spawn(async move { c.set_section_source(song_id, sec_id, new_source).await; });
                                }
                                // 2) Navigate grid to the patch's rig/scene
                                if let Some((rid, sid)) = patch_rig_map().get(&patch_id).cloned() {
                                    let is_rig = manage_presets()
                                        .iter()
                                        .find(|p| p.id == rid)
                                        .map_or(true, |p| p.is_rig);

                                    selected_preset_id.set(Some(rid.clone()));
                                    active_is_rig.set(is_rig);
                                    selected_sub_id.set(Some(sid.clone()));

                                    if is_rig {
                                        rig_id.set(Some(rid.clone()));
                                        active_scene_id.set(Some(sid.clone()));
                                        if let Some(item) = manage_presets().iter().find(|p| p.id == rid) {
                                            rig_scenes.set(item.sub_items.clone());
                                        }
                                    } else {
                                        rig_id.set(None);
                                        active_scene_id.set(None);
                                        rig_scenes.set(Vec::new());
                                    }

                                    let mut exp = expanded_ids();
                                    exp.insert(rid.clone());
                                    expanded_ids.set(exp);
                                    canvas_engines.set(Vec::new());
                                    canvas_params.set(EngineParamLookup::new());
                                    let c = ctrl.clone();
                                    spawn(async move {
                                        let result = if is_rig {
                                            signal2_ui::views::resolve_scene_engines(&c, &rid, &sid).await
                                        } else {
                                            signal2_ui::views::resolve_layer_engines(&c, &rid, Some(&sid)).await
                                        };
                                        if let Some((engines, params)) = result {
                                            canvas_engines.set(engines);
                                            canvas_params.set(params);
                                        }
                                    });
                                }
                            });
                            rsx! {
                                div { class: "flex-[2] min-h-0 flex flex-col border-t border-border",
                                    div { class: "px-3 py-2 border-b border-border flex-shrink-0",
                                        h3 { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider", "Profiles" }
                                    }
                                    div { class: "flex-1 overflow-y-auto",
                                        {render_profile_list(manage_profiles, expanded_profile_ids, selected_profile, selected_patch, Some(song_patch_cb), patch_display_labels)}
                                    }
                                }
                            }
                        }
                    }
                }

                // ── Center: Rig preset canvas ──
                div { class: "flex-1 min-w-0 flex flex-col overflow-hidden",
                    div { class: "flex-1 min-h-0 overflow-hidden flex flex-col",
                        if !canvas_engines().is_empty() {
                            {
                                let grid_key = format!(
                                    "{}-{}",
                                    selected_preset_id().unwrap_or_default(),
                                    selected_sub_id().unwrap_or_default(),
                                );
                                let grid_slots = engines_to_grid_slots(&canvas_engines(), &canvas_params());
                                let save_controller = controller.clone();
                                rsx! {
                                    RigGridPanel {
                                        key: "{grid_key}",
                                        initial_slots: grid_slots,
                                        on_save: move |slot: signal2_ui::components::GridSlot| {
                                            let ctrl = save_controller.clone();
                                            let bt = slot.block_type;
                                            let pid = slot.preset_id.clone().unwrap_or_default();
                                            let sid = slot.snapshot_id.clone();
                                            let block = signal2::Block::from_parameters(
                                                slot.parameters.iter()
                                                    .map(|(name, val)| signal2::BlockParameter::new(
                                                        name.to_lowercase().replace(' ', "-"),
                                                        name.clone(),
                                                        *val,
                                                    ))
                                                    .collect()
                                            );
                                            spawn(async move {
                                                ctrl.update_snapshot_params(
                                                    bt,
                                                    pid,
                                                    sid.unwrap_or_default(),
                                                    block,
                                                ).await;
                                            });
                                        },
                                    }
                                }
                            }
                        } else if rig_id().is_some() {
                            div { class: "flex items-center justify-center h-full",
                                p { class: "text-sm text-muted-foreground", "Loading rig graph..." }
                            }
                        } else {
                            div { class: "flex items-center justify-center h-full",
                                p { class: "text-sm text-muted-foreground", "Select a preset" }
                            }
                        }
                    }
                }

                // ── Right panel: Profile mode → profiles; Song mode → sections/songs ──
                if mode == ManageMode::Profile {
                    {
                        let ctrl = controller.clone();
                        let profile_patch_cb: Rc<dyn Fn(String, String)> = Rc::new(move |_prof_id: String, patch_id: String| {
                            let mut selected_preset_id = selected_preset_id;
                            let mut active_is_rig = active_is_rig;
                            let mut rig_id = rig_id;
                            let mut selected_sub_id = selected_sub_id;
                            let mut active_scene_id = active_scene_id;
                            let mut expanded_ids = expanded_ids;
                            let mut rig_scenes = rig_scenes;
                            let mut canvas_engines = canvas_engines;
                            let mut canvas_params = canvas_params;
                            // Navigate grid to the patch's rig/scene
                            if let Some((rid, sid)) = patch_rig_map().get(&patch_id).cloned() {
                                let is_rig = manage_presets()
                                    .iter()
                                    .find(|p| p.id == rid)
                                    .map_or(true, |p| p.is_rig);

                                selected_preset_id.set(Some(rid.clone()));
                                active_is_rig.set(is_rig);
                                selected_sub_id.set(Some(sid.clone()));

                                if is_rig {
                                    rig_id.set(Some(rid.clone()));
                                    active_scene_id.set(Some(sid.clone()));
                                    if let Some(item) = manage_presets().iter().find(|p| p.id == rid) {
                                        rig_scenes.set(item.sub_items.clone());
                                    }
                                } else {
                                    rig_id.set(None);
                                    active_scene_id.set(None);
                                    rig_scenes.set(Vec::new());
                                }

                                let mut exp = expanded_ids();
                                exp.insert(rid.clone());
                                expanded_ids.set(exp);
                                canvas_engines.set(Vec::new());
                                canvas_params.set(EngineParamLookup::new());
                                let c = ctrl.clone();
                                spawn(async move {
                                    let result = if is_rig {
                                        signal2_ui::views::resolve_scene_engines(&c, &rid, &sid).await
                                    } else {
                                        signal2_ui::views::resolve_layer_engines(&c, &rid, Some(&sid)).await
                                    };
                                    if let Some((engines, params)) = result {
                                        canvas_engines.set(engines);
                                        canvas_params.set(params);
                                    }
                                });
                            }
                        });
                        rsx! {
                            div { class: "w-64 flex-shrink-0 border-l border-border flex flex-col min-h-0 bg-zinc-950/40",
                                div { class: "px-3 py-2 border-b border-border flex-shrink-0",
                                    h3 { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider", "Profiles" }
                                }
                                div { class: "flex-1 overflow-y-auto",
                                    {render_profile_list(manage_profiles, expanded_profile_ids, selected_profile, selected_patch, Some(profile_patch_cb), patch_display_labels)}
                                }
                            }
                        }
                    }
                }

                if mode == ManageMode::Song {
                    div { class: "w-72 flex-shrink-0 border-l border-border flex flex-col min-h-0 bg-zinc-950/40",
                        // Sections for selected song (top)
                        div { class: "flex-[3] min-h-0 flex flex-col border-b border-border",
                            div { class: "px-3 py-2 border-b border-border flex-shrink-0",
                                h3 { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider",
                                    "{active_song_name}"
                                }
                            }
                            div { class: "flex-1 overflow-y-auto",
                                {
                                    let mut nav = navigate_to_section.clone();
                                    rsx! {
                                        SongEditor {
                                            song_name: String::new(),
                                            sections: song_sections(),
                                            selected_section_id: selected_section_id(),
                                            on_select_section: move |id: String| { nav(id); },
                                        }
                                    }
                                }
                            }
                        }

                        // Songs in setlist (bottom) — setlist dropdown above song list
                        div { class: "flex-[2] min-h-0 flex flex-col",
                            // Setlist selector dropdown
                            div { class: "px-3 py-2 border-b border-border flex-shrink-0",
                                {
                                    let current_label = setlist_options()
                                        .iter()
                                        .find(|(id, _)| id == &selected_setlist_id())
                                        .map(|(_, name)| name.clone())
                                        .unwrap_or_else(|| "All Songs".to_string());
                                    rsx! {
                                        Dropdown {
                                            DropdownTrigger {
                                                button {
                                                    class: "w-full flex items-center justify-between bg-zinc-800 border border-zinc-700 rounded px-2 py-1 text-xs text-zinc-200 hover:bg-zinc-700 transition-colors",
                                                    span { "{current_label}" }
                                                    span { class: "text-zinc-500 text-[10px] ml-1", "\u{25BE}" }
                                                }
                                            }
                                            DropdownContent {
                                                width: "w-56".to_string(),
                                                for (idx, (sid, sname)) in setlist_options().iter().enumerate() {
                                                    {
                                                        let sid_val = sid.clone();
                                                        let sname_val = sname.clone();
                                                        let mut change = change_setlist.clone();
                                                        rsx! {
                                                            DropdownItem {
                                                                value: sid_val.clone(),
                                                                index: idx,
                                                                on_select: move |val: String| { change(val); },
                                                                "{sname_val}"
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            div { class: "flex-1 overflow-y-auto",
                                for song in songs().iter() {
                                    {
                                        let is_sel = selected_song_id().as_deref() == Some(song.id.as_str());
                                        let song_id = song.id.clone();
                                        let mut load = load_song_sections.clone();
                                        rsx! {
                                            button {
                                                key: "{song_id}",
                                                class: if is_sel {
                                                    "w-full text-left px-3 py-2 border-b border-zinc-800/50 bg-zinc-700/60"
                                                } else {
                                                    "w-full text-left px-3 py-2 border-b border-zinc-800/50 hover:bg-zinc-800/60"
                                                },
                                                onclick: move |_| { load(song_id.clone()); },
                                                div { class: "flex items-center gap-1.5",
                                                    span { class: "text-sm text-zinc-200 truncate flex-1", "{song.name}" }
                                                    span { class: "text-[10px] text-zinc-500 flex-shrink-0",
                                                        "{song.section_count}"
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

/// Renders the expandable profile list with patches.
/// Shared between Song mode (left panel) and Profile mode (right panel).
/// `on_patch_click` is called with (profile_id, patch_id) when a patch is clicked.
fn render_profile_list(
    manage_profiles: Signal<Vec<ManageProfileItem>>,
    mut expanded_profile_ids: Signal<std::collections::HashSet<String>>,
    mut selected_profile: Signal<Option<String>>,
    mut selected_patch: Signal<Option<String>>,
    on_patch_click: Option<Rc<dyn Fn(String, String)>>,
    patch_labels: Signal<std::collections::HashMap<String, String>>,
) -> Element {
    rsx! {
        for prof in manage_profiles().iter().cloned() {
            {
                let is_sel = selected_profile().as_deref() == Some(prof.id.as_str());
                let is_expanded = expanded_profile_ids().contains(&prof.id);
                let prof_key = prof.id.clone();
                let prof_click_id = prof.id.clone();
                rsx! {
                    div { key: "{prof_key}",
                        button {
                            class: if is_sel {
                                "w-full text-left px-3 py-2 border-b border-zinc-800/50 bg-zinc-700/60"
                            } else {
                                "w-full text-left px-3 py-2 border-b border-zinc-800/50 hover:bg-zinc-800/60"
                            },
                            onclick: move |_| {
                                let mut exp = expanded_profile_ids();
                                if exp.contains(&prof_click_id) {
                                    exp.remove(&prof_click_id);
                                } else {
                                    exp.insert(prof_click_id.clone());
                                }
                                expanded_profile_ids.set(exp);
                                selected_profile.set(Some(prof_click_id.clone()));
                            },
                            div { class: "flex items-center gap-1.5",
                                span { class: "text-[10px] text-zinc-500 w-3 flex-shrink-0",
                                    if is_expanded { "\u{25BE}" } else { "\u{25B8}" }
                                }
                                span { class: "text-sm text-zinc-200 truncate flex-1", "{prof.name}" }
                                span { class: "text-[10px] text-zinc-500 flex-shrink-0",
                                    "{prof.patches.len()}"
                                }
                            }
                        }
                        if is_expanded {
                            for (patch_id, patch_name) in prof.patches.iter() {
                                {
                                    let is_patch_sel = selected_patch().as_deref() == Some(patch_id.as_str());
                                    let pid = patch_id.clone();
                                    let prof_id_for_cb = prof.id.clone();
                                    let cb = on_patch_click.clone();
                                    let label = patch_labels().get(patch_id).cloned();
                                    rsx! {
                                        button {
                                            key: "{patch_id}",
                                            class: if is_patch_sel {
                                                "w-full text-left pl-8 pr-3 py-1.5 bg-zinc-700/40 text-zinc-200 border-b border-zinc-800/30"
                                            } else {
                                                "w-full text-left pl-8 pr-3 py-1.5 text-zinc-400 hover:bg-zinc-800/40 hover:text-zinc-300 border-b border-zinc-800/30"
                                            },
                                            onclick: move |_| {
                                                selected_patch.set(Some(pid.clone()));
                                                if let Some(ref cb) = cb {
                                                    cb(prof_id_for_cb.clone(), pid.clone());
                                                }
                                            },
                                            div { class: "flex flex-col gap-0.5",
                                                span { class: "text-xs truncate", "{patch_name}" }
                                                if let Some(lbl) = &label {
                                                    span { class: "text-[9px] text-zinc-500 truncate", "{lbl}" }
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

// ---------------------------------------------------------------------------
// Editor tab — split grid + gradient inspector
// ---------------------------------------------------------------------------

#[component]
fn Signal2EditorTab(controller: SignalController) -> Element {
    use signal2::rig::RigType;
    use signal2_ui::components::{GridSelection, GridSlot};
    use signal2_ui::views::{
        engines_to_grid_slots, EditorInspectorPanel, EngineFlowData, EngineParamLookup,
        RigGridPanel,
    };

    // Rig type selector — filters presets
    let mut rig_type = use_signal(|| RigType::Guitar);

    // Preset list (rigs only for simplicity)
    let mut presets = use_signal(Vec::<ManagePresetItem>::new);
    let mut expanded_ids = use_signal(std::collections::HashSet::<String>::new);
    let mut selected_preset_id = use_signal(|| None::<String>);
    let mut selected_sub_id = use_signal(|| None::<String>);
    let mut rig_scenes = use_signal(Vec::<(String, String)>::new);
    let mut active_scene_id = use_signal(|| None::<String>);

    // Resolved engine flow data + param lookup for the grid
    let mut canvas_engines = use_signal(Vec::<EngineFlowData>::new);
    let mut canvas_params = use_signal(EngineParamLookup::new);

    // Selection state lifted from RigGridPanel for the inspector
    let mut editor_selection = use_signal(|| None::<GridSelection>);
    let mut editor_chain = use_signal(Vec::<GridSlot>::new);

    // Load presets when rig_type changes
    {
        let controller = controller.clone();
        use_effect(move || {
            let controller = controller.clone();
            let rt = rig_type();
            selected_preset_id.set(None);
            selected_sub_id.set(None);
            active_scene_id.set(None);
            rig_scenes.set(Vec::new());
            canvas_engines.set(Vec::new());
            canvas_params.set(EngineParamLookup::new());
            editor_selection.set(None);
            editor_chain.set(Vec::new());

            spawn(async move {
                let rigs = controller.list_rig_collections().await;
                let filtered: Vec<_> = rigs
                    .into_iter()
                    .filter(|r| r.rig_type.map_or(false, |t| t == rt))
                    .collect();

                let items: Vec<ManagePresetItem> = filtered
                    .iter()
                    .map(|r| ManagePresetItem {
                        id: r.id.to_string(),
                        name: r.name.clone(),
                        is_rig: true,
                        sub_items: r
                            .variants
                            .iter()
                            .map(|v| (v.id.to_string(), v.name.clone()))
                            .collect(),
                    })
                    .collect();
                presets.set(items);

                // Auto-select first rig + first scene
                if let Some(first) = filtered.first() {
                    let first_id = first.id.to_string();
                    selected_preset_id.set(Some(first_id.clone()));
                    expanded_ids.set([first_id.clone()].into_iter().collect());
                    rig_scenes.set(
                        first
                            .variants
                            .iter()
                            .map(|v| (v.id.to_string(), v.name.clone()))
                            .collect(),
                    );

                    if let Some(first_scene) = first.variants.first() {
                        let scene_id = first_scene.id.to_string();
                        active_scene_id.set(Some(scene_id.clone()));
                        selected_sub_id.set(Some(scene_id.clone()));
                        if let Some((engines, params)) = signal2_ui::views::resolve_scene_engines(
                            &controller,
                            &first_id,
                            &scene_id,
                        )
                        .await
                        {
                            canvas_engines.set(engines);
                            canvas_params.set(params);
                        }
                    }
                }
            });
        });
    }

    // Handle preset click
    let select_preset = {
        let controller = controller.clone();
        move |item_id: String| {
            let controller = controller.clone();
            let items = presets();
            let Some(item) = items.iter().find(|i| i.id == item_id).cloned() else {
                return;
            };
            let mut exp = expanded_ids();
            exp.insert(item_id.clone());
            expanded_ids.set(exp);
            selected_preset_id.set(Some(item_id.clone()));
            rig_scenes.set(item.sub_items.clone());
            editor_selection.set(None);

            if let Some((first_sub_id, _)) = item.sub_items.first() {
                let sub_id = first_sub_id.clone();
                selected_sub_id.set(Some(sub_id.clone()));
                active_scene_id.set(Some(sub_id.clone()));
                canvas_engines.set(Vec::new());
                canvas_params.set(EngineParamLookup::new());
                spawn(async move {
                    if let Some((engines, params)) =
                        signal2_ui::views::resolve_scene_engines(&controller, &item_id, &sub_id)
                            .await
                    {
                        canvas_engines.set(engines);
                        canvas_params.set(params);
                    }
                });
            } else {
                selected_sub_id.set(None);
                active_scene_id.set(None);
                canvas_engines.set(Vec::new());
                canvas_params.set(EngineParamLookup::new());
            }
        }
    };

    // Handle scene tab click
    let select_scene = {
        let controller = controller.clone();
        move |parent_id: String, sub_id: String| {
            let controller = controller.clone();
            selected_sub_id.set(Some(sub_id.clone()));
            active_scene_id.set(Some(sub_id.clone()));
            editor_selection.set(None);
            canvas_engines.set(Vec::new());
            canvas_params.set(EngineParamLookup::new());
            spawn(async move {
                if let Some((engines, params)) =
                    signal2_ui::views::resolve_scene_engines(&controller, &parent_id, &sub_id).await
                {
                    canvas_engines.set(engines);
                    canvas_params.set(params);
                }
            });
        }
    };

    let current_preset = selected_preset_id();
    let current_sub = selected_sub_id();
    let current_scene = active_scene_id();
    let scenes = rig_scenes();

    rsx! {
        div { class: "flex flex-col h-full overflow-hidden",
            // ── Top bar: Rig type + Scene tabs ──
            div { class: "flex items-center gap-2 px-3 py-1.5 border-b border-border bg-zinc-900/40 flex-shrink-0 overflow-x-auto",
                // Rig type selector
                span { class: "text-[10px] text-zinc-500 mr-1 flex-shrink-0", "Rig:" }
                for &rt in &[RigType::Guitar, RigType::Bass, RigType::Keys, RigType::Vocals] {
                    {
                        let is_active = rig_type() == rt;
                        let label = match rt {
                            RigType::Guitar => "Guitar",
                            RigType::Bass => "Bass",
                            RigType::Keys => "Keys",
                            RigType::Vocals => "Vocals",
                            _ => "Other",
                        };
                        rsx! {
                            button {
                                key: "{label}",
                                class: if is_active {
                                    "px-2 py-0.5 text-[11px] font-medium rounded bg-zinc-600 text-zinc-100"
                                } else {
                                    "px-2 py-0.5 text-[11px] text-zinc-500 hover:text-zinc-300 hover:bg-zinc-800 rounded"
                                },
                                onclick: move |_| rig_type.set(rt),
                                "{label}"
                            }
                        }
                    }
                }

                // Scene tabs
                if !scenes.is_empty() {
                    div { class: "w-px h-4 bg-zinc-700 mx-1 flex-shrink-0" }
                    span { class: "text-[10px] text-zinc-500 mr-1 flex-shrink-0", "Scenes" }
                    for (sid, sname) in scenes.iter() {
                        {
                            let is_active = current_scene.as_deref() == Some(sid.as_str());
                            let scene_id = sid.clone();
                            let rid = selected_preset_id().unwrap_or_default();
                            let mut on_click = select_scene.clone();
                            rsx! {
                                button {
                                    key: "{sid}",
                                    class: if is_active {
                                        "px-2.5 py-1 text-xs font-medium rounded bg-zinc-700 text-zinc-100"
                                    } else {
                                        "px-2.5 py-1 text-xs text-zinc-400 hover:text-zinc-200 hover:bg-zinc-800 rounded"
                                    },
                                    onclick: move |_| {
                                        on_click(rid.clone(), scene_id.clone());
                                    },
                                    "{sname}"
                                }
                            }
                        }
                    }
                }
            }

            // ── Body: preset sidebar | grid | inspector ──
            div { class: "flex flex-1 min-h-0 overflow-hidden",
                // ── Left: Preset list ──
                div { class: "w-52 flex-shrink-0 border-r border-border flex flex-col min-h-0 bg-zinc-950/40",
                    div { class: "px-3 py-2 border-b border-border flex-shrink-0",
                        h3 { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider", "Presets" }
                    }
                    div { class: "flex-1 overflow-y-auto",
                        for item in presets().iter().cloned() {
                            {
                                let is_sel = current_preset.as_deref() == Some(item.id.as_str());
                                let is_expanded = expanded_ids().contains(&item.id);
                                let item_key = item.id.clone();
                                let item_click_id = item.id.clone();
                                let mut on_select = select_preset.clone();
                                rsx! {
                                    div { key: "{item_key}",
                                        button {
                                            class: if is_sel {
                                                "w-full text-left px-3 py-2 border-b border-zinc-800/50 bg-zinc-700/60"
                                            } else {
                                                "w-full text-left px-3 py-2 border-b border-zinc-800/50 hover:bg-zinc-800/60"
                                            },
                                            onclick: move |_| {
                                                on_select(item_click_id.clone());
                                            },
                                            div { class: "flex items-center gap-1.5",
                                                span { class: "text-[10px] text-zinc-500 w-3 flex-shrink-0",
                                                    if is_expanded { "\u{25BE}" } else { "\u{25B8}" }
                                                }
                                                span { class: "text-[9px] px-1 rounded bg-zinc-600 text-zinc-300 flex-shrink-0",
                                                    "RIG"
                                                }
                                                span { class: "text-sm text-zinc-200 truncate flex-1", "{item.name}" }
                                                span { class: "text-[10px] text-zinc-500 flex-shrink-0",
                                                    "{item.sub_items.len()}"
                                                }
                                            }
                                        }
                                        if is_expanded {
                                            for (sub_id, sub_name) in item.sub_items.iter() {
                                                {
                                                    let is_sub_sel = current_sub.as_deref() == Some(sub_id.as_str());
                                                    let parent_id = item.id.clone();
                                                    let sub_id_click = sub_id.clone();
                                                    let mut on_scene = select_scene.clone();
                                                    rsx! {
                                                        button {
                                                            key: "{sub_id}",
                                                            class: if is_sub_sel {
                                                                "w-full text-left pl-8 pr-3 py-1.5 text-xs bg-zinc-700/40 text-zinc-200 border-b border-zinc-800/30"
                                                            } else {
                                                                "w-full text-left pl-8 pr-3 py-1.5 text-xs text-zinc-400 hover:bg-zinc-800/40 hover:text-zinc-300 border-b border-zinc-800/30"
                                                            },
                                                            onclick: move |_| {
                                                                on_scene(parent_id.clone(), sub_id_click.clone());
                                                            },
                                                            "{sub_name}"
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

                // ── Center: Grid ──
                div { class: "flex-[3] min-w-0 flex flex-col overflow-hidden",
                    if !canvas_engines().is_empty() {
                        {
                            let grid_key = format!(
                                "editor-{}-{}",
                                selected_preset_id().unwrap_or_default(),
                                selected_sub_id().unwrap_or_default(),
                            );
                            let grid_slots = engines_to_grid_slots(&canvas_engines(), &canvas_params());
                            // Keep chain in sync for the inspector
                            let inspector_slots = grid_slots.clone();
                            editor_chain.set(inspector_slots);
                            let save_controller = controller.clone();
                            rsx! {
                                RigGridPanel {
                                    key: "{grid_key}",
                                    initial_slots: grid_slots,
                                    on_selection_change: move |sel: Option<GridSelection>| {
                                        editor_selection.set(sel);
                                    },
                                    on_param_change: move |(id, name, value)| {
                                        // Update inspector chain too
                                        let mut current = editor_chain();
                                        if let Some(slot) = current.iter_mut().find(|s| s.id == id) {
                                            if let Some(p) = slot.parameters.iter_mut().find(|(n, _)| *n == name) {
                                                p.1 = value;
                                            }
                                        }
                                        editor_chain.set(current);
                                    },
                                    on_save: move |slot: GridSlot| {
                                        let ctrl = save_controller.clone();
                                        let bt = slot.block_type;
                                        let pid = slot.preset_id.clone().unwrap_or_default();
                                        let sid = slot.snapshot_id.clone();
                                        let block = signal2::Block::from_parameters(
                                            slot.parameters.iter()
                                                .map(|(name, val)| signal2::BlockParameter::new(
                                                    name.to_lowercase().replace(' ', "-"),
                                                    name.clone(),
                                                    *val,
                                                ))
                                                .collect()
                                        );
                                        spawn(async move {
                                            ctrl.update_snapshot_params(
                                                bt,
                                                pid,
                                                sid.unwrap_or_default(),
                                                block,
                                            ).await;
                                        });
                                    },
                                }
                            }
                        }
                    } else if selected_preset_id().is_some() {
                        div { class: "flex items-center justify-center h-full",
                            p { class: "text-sm text-muted-foreground", "Loading rig graph..." }
                        }
                    } else {
                        div { class: "flex items-center justify-center h-full",
                            p { class: "text-sm text-muted-foreground", "Select a preset" }
                        }
                    }
                }

                // ── Right: Editor Inspector ──
                div { class: "w-80 flex-shrink-0 border-l border-border overflow-y-auto bg-zinc-950/30",
                    EditorInspectorPanel {
                        selection: editor_selection(),
                        chain: editor_chain(),
                    }
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Browser dialog (near-full-screen)
// ---------------------------------------------------------------------------

#[component]
fn Signal2BrowserDialog(controller: SignalController, on_close: Callback<()>) -> Element {
    rsx! {
        // Overlay
        div {
            class: "fixed inset-0 z-50 bg-black/80 animate-fade-in",
            onclick: move |_| on_close.call(()),
        }

        // Content — near-full-screen
        div {
            class: "fixed inset-4 z-50 flex flex-col border border-border bg-background rounded-lg shadow-2xl animate-scale-in overflow-hidden",
            style: "transform-origin: center center;",
            onclick: move |evt: MouseEvent| { evt.stop_propagation(); },

            // Header
            div { class: "flex items-center justify-between px-4 py-2 border-b border-border bg-muted/30 flex-shrink-0",
                h2 { class: "text-sm font-semibold", "Collection Browser" }
                button {
                    class: "px-2 py-1 text-xs rounded hover:bg-muted text-muted-foreground hover:text-foreground transition-colors",
                    onclick: move |_| on_close.call(()),
                    "\u{2715} Close"
                }
            }

            // Browser body
            div { class: "flex-1 min-h-0 overflow-hidden",
                CollectionBrowser { controller }
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

/// Run background services (DAW connection, session services, gateway)
async fn run_services() {
    use daw_connection::DawConnectionManager;
    use gateway::{start_gateway, GatewayConfig};
    use services::LocalServices;
    use session_ui::Session;

    debug!("Initializing services...");

    // 1. Create local session services
    let services = LocalServices::new();
    debug!("Local services initialized");

    // 2. Initialize Session singleton for UI components
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

    // 3. Create dispatcher for gateway
    let dispatcher = services.create_dispatcher();

    // 4. Try to connect to REAPER (non-blocking, will retry)
    let daw_manager = DawConnectionManager::new();
    tokio::spawn(async move {
        loop {
            match daw_manager.connect_default().await {
                Ok(conn) => {
                    debug!(
                        "Connected to DAW: {}",
                        conn.identity()
                            .map(|i| i.name.as_str())
                            .unwrap_or("unknown")
                    );

                    // Initialize daw-control singleton
                    if let Err(e) = daw_control::Daw::init(conn.handle().clone()) {
                        tracing::warn!("Failed to initialize Daw singleton: {}", e);
                    }

                    // Signal that DAW is connected - UI will fetch setlist
                    // Latency polling is handled by the Dioxus UI context (_latency_task)
                    DAW_CONNECTED.store(true, std::sync::atomic::Ordering::Relaxed);
                    debug!("DAW connection ready - UI can now fetch setlist");

                    break;
                }
                Err(e) => {
                    tracing::warn!("Failed to connect to DAW: {}. Retrying in 5s...", e);
                    tokio::time::sleep(tokio::time::Duration::from_secs(5)).await;
                }
            }
        }
    });

    // 5. Start WebSocket gateway for browser access
    let config = GatewayConfig::default();
    debug!("Starting gateway on {}", config.bind_addr);

    if let Err(e) = start_gateway(dispatcher, &config.bind_addr, config.static_dir.as_deref()).await
    {
        tracing::error!("Gateway error: {}", e);
    }
}
