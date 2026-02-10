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

mod chart_graphics;
mod daw_connection;
mod gateway;
mod services;

// Conditionally import the right Dioxus prelude based on renderer feature
#[cfg(feature = "desktop")]
use dioxus::prelude::*;
#[cfg(feature = "native")]
use dioxus_native::prelude::*;

use std::sync::Arc;
use std::time::{Duration, Instant};
use std::{collections::HashMap, rc::Rc};

#[cfg(feature = "desktop")]
use chart_graphics::ChartGraphics;
#[cfg(feature = "desktop")]
use dioxus::desktop::{tao::window::WindowBuilder, Config};

use session_ui::{
    ChartAreaBounds, ConnectionState, LatencyInfo, PerfChartViewport, PerformanceLayout,
    PerformanceSidebar, Session, TopBar, TransportPanel, ACTIVE_INDICES,
    ACTIVE_PLAYBACK_IS_PLAYING, ACTIVE_PLAYBACK_MUSICAL, AUDIO_LATENCY_SECONDS, CHART_AREA_BOUNDS,
    LATENCY_INFO, PERF_CHART_BASE_SCALE, PERF_CHART_CLICK, PERF_CHART_HOVER, PERF_CHART_VIEWPORT,
    SONG_CHARTS,
};

use daw_ui::{ArrangementView, FxBrowserDockPanel, FxChainTree, MixerPanel, TrackControlPanel};
use keyflow_ui::signals::{ChartEditorBounds, PreviewMode};
use keyflow_ui::{
    ChartEditorLayout, ChartLayoutManager, RenderStats, CHART_BASE_SCALE, CHART_CURSOR_POSITION,
    CHART_CURSOR_SCENE_CLICK, CHART_CURSOR_TICK, CHART_CURSOR_VISIBLE, CHART_EDITOR_BOUNDS,
    CHART_HOVER_SCENE_POINT, CHART_PAGE_INFO, CHART_PREVIEW_MODE, CHART_RENDER_STATS, CHART_SOURCE,
    CHART_VIEWPORT, SESSION_CHART_SOURCE,
};
use kurbo::Affine;
use signal_ui::{
    PresetBrowserPanel, ProfileBrowserPanel, RigEditorPanel, RigGridPanel, SceneGridDockPanel,
    SnapshotTestHarness, SongPartsPanel, SongSelectorPanel,
};

use dock_dioxus::{init_dock_presets, DockProvider, DockRoot, PanelRenderer, PresetBar};
use dock_proto::PanelId;

use actions_proto::ids::standalone as standalone_ids;
use actions_proto::ActionDefinition;
use input::{
    config::{default_user_config_path, load_default_config, load_user_config},
    InputCommand, KeymapConfig,
};
use input_dioxus::{use_input_processor, ACTION_CONTEXT};
use session::session_actions;

use tokio;
use tracing::{debug, info};

#[global_allocator]
static GLOBAL_ALLOCATOR: mimalloc::MiMalloc = mimalloc::MiMalloc;

/// Whether the dock layout system is active (vs. classic tab navigation).
static DOCK_MODE: GlobalSignal<bool> = Signal::global(|| true);
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
                // Snapshot/morph debugging — targeted debug for FX parameter pipeline
                .add_directive(
                    "signal_ui::components::snapshot_test_harness=debug"
                        .parse()
                        .unwrap(),
                )
                .add_directive("signal_control::daw_bridge=debug".parse().unwrap())
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
    });

    // Panel renderer — maps PanelId to existing UI components
    let render_panel = use_hook(|| {
        PanelRenderer::new(|panel_id| match panel_id {
            PanelId::Performance => rsx! { PerformanceWithChartToggle {} },
            PanelId::ChartEditor => rsx! { ChartView {} },
            PanelId::ChartPreview => rsx! { ChartPreviewPanel {} },
            PanelId::Navigator => rsx! { PerformanceSidebar {} },
            PanelId::Transport => rsx! { TransportPanel {} },
            PanelId::Setlist => rsx! { SetlistView {} },
            PanelId::RigGrid => rsx! { RigGridPanel {} },
            PanelId::PresetBrowser => rsx! { PresetBrowserPanel {} },
            PanelId::ProfileBrowser => rsx! { ProfileBrowserPanel {} },
            PanelId::SongParts => rsx! { SongPartsPanel {} },
            PanelId::SongSelector => rsx! { SongSelectorPanel {} },
            PanelId::SceneGrid => rsx! { SceneGridDockPanel {} },
            PanelId::FxBrowser => rsx! { FxBrowserDockPanel {} },
            PanelId::Mixer => rsx! { MixerPanel {} },
            PanelId::FxChainTree => rsx! { FxChainTree {} },
            PanelId::TrackControlPanel => rsx! { TrackControlPanel {} },
            PanelId::ArrangementView => rsx! { ArrangementView {} },
            PanelId::RigEditor => rsx! { RigEditorPanel {} },
            PanelId::SnapshotTest => rsx! { SnapshotTestHarness {} },
            PanelId::Settings => rsx! { SettingsView {} },
            _ => rsx! {
                div {
                    class: "flex items-center justify-center h-full text-zinc-500",
                    "{panel_id.display_name()} — Coming soon"
                }
            },
        })
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
                        info!(
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
                info!(
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
                        if *COMMAND_PALETTE_OPEN.read() {
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

                        // Top navigation bar (same as web) — shown in classic mode
                        if !*DOCK_MODE.read() {
                            TopBar {
                                connection_state: connection_state(),
                                active_tab: active_tab(),
                                on_tab_click: Some(Callback::new(move |tab: String| {
                                    ACTION_CONTEXT.write().set_tab(&tab);
                                    active_tab.set(tab);
                                })),
                            }
                        }

                        // Dock mode: preset bar + dock root
                        if *DOCK_MODE.read() {
                            // Preset bar (screenset selector)
                            PresetBar {}

                            // Dock layout fills remaining space
                            div {
                                class: "flex-1 overflow-hidden relative",
                                style: if needs_transparency { "background: transparent !important; background-color: transparent !important;" } else { "" },
                                DockRoot {}
                            }
                        } else {
                            // Classic tab mode (fallback)
                            div {
                                class: "flex-1 overflow-hidden relative",
                                style: if needs_transparency { "background: transparent !important; background-color: transparent !important;" } else { "" },
                                match active_tab().as_str() {
                                    "performance" => rsx! { PerformanceWithChartToggle {} },
                                    "chart" => rsx! { ChartView {} },
                                    "setlist" => rsx! { SetlistView {} },
                                    "rig" => rsx! { RigGridPanel {} },
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
                Key::F11 => Some(6),
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

// Used by ChartPreviewPanel (inside #[component] macro expansion)
#[allow(dead_code)]
#[derive(Clone, Copy)]
struct PerfStaticSceneKey {
    generation: u64,
    width: f64,
    height: f64,
    tx: f64,
    ty: f64,
    scale: f64,
}

#[allow(dead_code)]
impl PerfStaticSceneKey {
    fn approx_eq(self, other: Self) -> bool {
        const EPS: f64 = 0.001;
        self.generation == other.generation
            && (self.width - other.width).abs() <= EPS
            && (self.height - other.height).abs() <= EPS
            && (self.tx - other.tx).abs() <= EPS
            && (self.ty - other.ty).abs() <= EPS
            && (self.scale - other.scale).abs() <= EPS
    }
}

#[allow(dead_code)]
#[derive(Default)]
struct PerfCursorMotionState {
    last_sample_tick: Option<i64>,
    last_sample_time: Option<Instant>,
    velocity_ticks_per_sec: f64,
}

#[allow(dead_code)]
struct PerfRenderAccumulator {
    window_started: Instant,
    static_rebuilds: u64,
    static_build_ms: f64,
    overlay_ms: f64,
    frame_samples_ms: Vec<f64>,
}

#[allow(dead_code)]
impl PerfRenderAccumulator {
    fn new() -> Self {
        Self {
            window_started: Instant::now(),
            static_rebuilds: 0,
            static_build_ms: 0.0,
            overlay_ms: 0.0,
            frame_samples_ms: Vec::with_capacity(1024),
        }
    }

    fn record(&mut self, static_ms: f64, overlay_ms: f64, frame_ms: f64, static_rebuilt: bool) {
        if static_rebuilt {
            self.static_rebuilds += 1;
            self.static_build_ms += static_ms;
        }
        self.overlay_ms += overlay_ms;
        self.frame_samples_ms.push(frame_ms);
    }

    fn maybe_flush_log(&mut self) -> Option<(u64, f64, f64, f64, u64, f64)> {
        if self.window_started.elapsed() < Duration::from_secs(5)
            || self.frame_samples_ms.is_empty()
        {
            return None;
        }

        let frames = self.frame_samples_ms.len() as u64;
        let avg_frame_ms = self.frame_samples_ms.iter().sum::<f64>() / frames as f64;
        let mut sorted = self.frame_samples_ms.clone();
        sorted.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
        let p95_idx = ((sorted.len() as f64 * 0.95).floor() as usize).min(sorted.len() - 1);
        let p95_ms = sorted[p95_idx];
        let avg_overlay_ms = self.overlay_ms / frames as f64;
        let static_rebuilds = self.static_rebuilds;
        let avg_static_ms = if static_rebuilds > 0 {
            self.static_build_ms / static_rebuilds as f64
        } else {
            0.0
        };

        self.window_started = Instant::now();
        self.static_rebuilds = 0;
        self.static_build_ms = 0.0;
        self.overlay_ms = 0.0;
        self.frame_samples_ms.clear();

        Some((
            frames,
            avg_frame_ms,
            p95_ms,
            avg_overlay_ms,
            static_rebuilds,
            avg_static_ms,
        ))
    }
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

/// Chart Editor view — split editor with live WGPU chart preview.
///
/// Uses `ChartEditorLayout` from keyflow-ui for the Dioxus UI, and
/// renders the chart via `ChartLayoutManager` → `ChartGraphics` for WGPU.
#[component]
fn ChartView() -> Element {
    #[cfg(feature = "desktop")]
    {
        let graphics = consume_context::<Arc<std::sync::Mutex<ChartGraphics>>>();

        // Enable transparent mode on mount
        use_effect(move || {
            document::eval(r#"document.documentElement.classList.add('transparent-mode');"#);
        });

        // Cleanup: remove transparent mode when component unmounts (if no other chart visible)
        use_drop(move || {
            let layout = dock_dioxus::DOCK_LAYOUT.peek();
            if !layout.panel_is_visible(PanelId::ChartEditor)
                && !layout.panel_is_visible(PanelId::ChartPreview)
            {
                document::eval(r#"document.documentElement.classList.remove('transparent-mode');"#);
            }
        });

        // Layout effect: watches source + preview mode + bounds width.
        // Only re-layouts when chart content or layout dimensions change.
        // Stores the ChartLayoutManager in a signal so the render effect can reuse it.
        let layout_manager: Signal<Option<std::rc::Rc<std::cell::RefCell<ChartLayoutManager>>>> =
            use_signal(|| {
                ChartLayoutManager::new()
                    .ok()
                    .map(|m| std::rc::Rc::new(std::cell::RefCell::new(m)))
            });

        // Generation counter: bumped each time layout changes, so render effect re-fires.
        let mut layout_generation = use_signal(|| 0u64);

        // Layout effect: re-layout when source or preview mode changes.
        // Subscribes to CHART_EDITOR_BOUNDS so it fires when bounds first become valid,
        // but the hash check prevents re-layout when only x/y change.
        {
            use_effect(move || {
                let source = CHART_SOURCE.read().clone();
                let preview_mode = *CHART_PREVIEW_MODE.read();
                let bounds = *CHART_EDITOR_BOUNDS.read();

                if !bounds.is_valid() {
                    return;
                }

                // parse_and_layout handles all caching:
                // - Layout hash check (skip if source+mode unchanged)
                // - Parse cache (skip re-parse if only mode changed)
                // - Layout computation + scene cache invalidation
                let snippet_mode = preview_mode == PreviewMode::Snippet;
                if let Some(ref manager_rc) = *layout_manager.read() {
                    let mut manager = manager_rc.borrow_mut();
                    match manager.parse_and_layout(&source, bounds.width, snippet_mode) {
                        Ok(true) => {
                            layout_generation.set(layout_generation() + 1);
                            tracing::info!("Chart layout updated (gen {})", layout_generation());

                            // Eagerly write base_scale so navigation functions have it immediately
                            let base_scale = manager.fit_to_width_scale(bounds.width, bounds.dpr);
                            *CHART_BASE_SCALE.write() = base_scale;

                            // Populate page metadata for navigation UI
                            if let Some(metadata) = manager.page_metadata() {
                                let total = metadata.len() as u32;
                                let mut info = CHART_PAGE_INFO.write();
                                info.total_pages = total.max(1);
                                info.page_metadata = metadata;
                                // Clamp current page if layout now has fewer pages
                                if info.current_page > total {
                                    info.current_page = total.max(1);
                                }
                            }

                            // Apply FullPage zoom on initial layout (when zoom is still default)
                            if !snippet_mode {
                                let vp = *CHART_VIEWPORT.peek();
                                if vp.zoom_level == keyflow_ui::SemanticZoomLevel::FullPage
                                    && (vp.zoom - 1.0).abs() < 0.01
                                    && vp.scroll_y.abs() < 0.01
                                {
                                    // Compute FullPage zoom: fit entire page in viewport
                                    if let Some(page) =
                                        manager.layout_result().and_then(|r| r.pages.first())
                                    {
                                        if page.height > 0.0 && base_scale > 0.0 {
                                            let zoom = bounds.height / (page.height * base_scale);
                                            let mut vp = CHART_VIEWPORT.write();
                                            vp.zoom = zoom.clamp(0.1, 8.0);
                                            vp.zoom_level = keyflow_ui::SemanticZoomLevel::FullPage;
                                        }
                                    }
                                }
                            }
                        }
                        Ok(false) => {} // Nothing changed, caches hit
                        Err(e) => {
                            tracing::info!("Chart parse error: {}", e);
                        }
                    }
                }
            });
        }

        // FPS tracking: sliding window of frame times (same as vello stats.rs pattern).
        let fps_state = use_hook(|| std::rc::Rc::new(std::cell::RefCell::new(FpsTracker::new())));

        // Render effect: re-renders when layout, viewport, or bounds change.
        // This is cheap — just builds a Scene and paints it.
        {
            let graphics_clone = graphics.clone();
            let fps_tracker = fps_state.clone();
            use_effect(move || {
                let viewport = *CHART_VIEWPORT.read();
                let _gen = layout_generation(); // Subscribe to layout changes
                let active_song_index = ACTIVE_INDICES.peek().song_index;
                let playback_musical = *ACTIVE_PLAYBACK_MUSICAL.read();
                let playback_is_playing = *ACTIVE_PLAYBACK_IS_PLAYING.read();
                let current_cursor_tick = *CHART_CURSOR_TICK.read();

                // Peek bounds — don't subscribe. Bounds changes don't need re-render
                // unless they also change the layout (handled by layout effect).
                let bounds = *CHART_EDITOR_BOUNDS.peek();

                if !bounds.is_valid() {
                    return;
                }

                if let Some(ref manager_rc) = *layout_manager.read() {
                    let mut manager = manager_rc.borrow_mut();
                    if manager.layout_result().is_none() {
                        return;
                    }

                    let frame_start = std::time::Instant::now();

                    let base_scale = manager.fit_to_width_scale(bounds.width, bounds.dpr);
                    let pad = 20.0 * bounds.dpr; // 20 CSS px padding, scaled to physical
                    let transform = Affine::translate((
                        pad - viewport.scroll_x * bounds.dpr,
                        pad - viewport.scroll_y * bounds.dpr,
                    )) * Affine::scale(base_scale * viewport.zoom);

                    // Expose base_scale for UI navigation functions
                    if (*CHART_BASE_SCALE.peek() - base_scale).abs() > 0.001 {
                        *CHART_BASE_SCALE.write() = base_scale;
                    }

                    // Derive current page from scroll position (pages are horizontal)
                    let current_page = manager.current_page_for_scroll(
                        viewport.scroll_x,
                        base_scale,
                        viewport.zoom,
                        bounds.dpr,
                    );
                    {
                        let info = CHART_PAGE_INFO.peek();
                        if info.current_page != current_page
                            || info.zoom_level != viewport.zoom_level
                        {
                            drop(info);
                            let mut info = CHART_PAGE_INFO.write();
                            info.current_page = current_page;
                            info.zoom_level = viewport.zoom_level;
                        }
                    }

                    let mut cursor_tick = current_cursor_tick;

                    // Follow DAW transport while playback is running.
                    // DAW sends 1-indexed measure/beat; chart uses 0-indexed internally.
                    if playback_is_playing {
                        if let Some(musical) = playback_musical {
                            if let Some(playback_tick) = manager.tick_for_musical_position(
                                musical.measure - 1,
                                musical.beat - 1,
                                musical.subdivision,
                            ) {
                                cursor_tick = playback_tick;
                                if playback_tick != current_cursor_tick {
                                    *CHART_CURSOR_TICK.write() = playback_tick;
                                }
                            }
                        }
                    }

                    // Process click-to-position: convert scene point → tick
                    // Copy the value out before any writes to avoid borrow conflicts
                    let pending_click = *CHART_CURSOR_SCENE_CLICK.peek();
                    if let Some((scene_x, scene_y)) = pending_click {
                        // Clear first, then write tick — avoids holding peek borrow across writes
                        *CHART_CURSOR_SCENE_CLICK.write() = None;
                        if let Some(tick) = manager.tick_at_scene_point(scene_x, scene_y) {
                            tracing::info!(
                                "Click-to-position: scene=({:.1},{:.1}) → tick={}",
                                scene_x,
                                scene_y,
                                tick
                            );
                            cursor_tick = tick;
                            if tick != current_cursor_tick {
                                *CHART_CURSOR_TICK.write() = tick;
                            }

                            if let Some(song_index) = active_song_index {
                                if let Some((measure, beat, subdivision)) =
                                    manager.musical_position_at_tick(tick)
                                {
                                    // Chart returns 0-indexed measure/beat;
                                    // DAW expects 1-indexed.
                                    spawn(async move {
                                        let _ = Session::get()
                                            .setlist()
                                            .seek_to_musical_position(
                                                song_index,
                                                daw_proto::MusicalPosition::new(
                                                    measure + 1,
                                                    beat + 1,
                                                    subdivision,
                                                ),
                                            )
                                            .await;
                                    });
                                }
                            }
                        }
                    }

                    // Read cursor state for rendering (subscribe so effect re-fires on click)
                    let cursor_tick = if *CHART_CURSOR_VISIBLE.peek() {
                        Some(cursor_tick)
                    } else {
                        // Still subscribe even when hidden, so toggling visibility triggers re-render
                        let _ = *CHART_CURSOR_TICK.read();
                        None
                    };

                    // Read hover point for highlight rendering (subscribe so effect re-fires on hover)
                    let hover_point = *CHART_HOVER_SCENE_POINT.read();

                    // Update musical position display from cursor state
                    if let Some(tick) = cursor_tick {
                        let pos = manager.musical_position_for_tick(tick);
                        if *CHART_CURSOR_POSITION.peek() != pos {
                            *CHART_CURSOR_POSITION.write() = pos;
                        }
                    }

                    if let Ok(mut gfx) = graphics_clone.lock() {
                        // Ensure surface matches actual window size (initial size may be stale)
                        let win_size = dioxus::desktop::window().window.inner_size();
                        let (sw, sh) = gfx.size();
                        if sw != win_size.width || sh != win_size.height {
                            tracing::debug!(
                                "Surface resize: {}x{} -> {}x{}",
                                sw,
                                sh,
                                win_size.width,
                                win_size.height
                            );
                            gfx.resize(win_size.width, win_size.height);
                        }

                        let dock_offset = Affine::translate((bounds.x, bounds.y));
                        gfx.render_chart(|painter| {
                            manager.render_to_scene(
                                painter,
                                bounds.width,
                                bounds.height,
                                dock_offset,
                                transform,
                                cursor_tick,
                                hover_point,
                            );
                        });
                    }
                    dioxus::desktop::window().window.request_redraw();

                    // Record frame time (don't write signal here — avoid feedback loop)
                    let frame_time_us = frame_start.elapsed().as_micros() as u64;
                    fps_tracker.borrow_mut().add_sample(frame_time_us);
                }
            });
        }

        // FPS display update: periodic, decoupled from render to avoid feedback loops.
        {
            let fps_tracker_for_display = fps_state.clone();
            use_future(move || {
                let tracker = fps_tracker_for_display.clone();
                async move {
                    loop {
                        tokio::time::sleep(tokio::time::Duration::from_millis(500)).await;
                        let stats = tracker.borrow().snapshot();
                        *CHART_RENDER_STATS.write() = stats;
                    }
                }
            });
        }

        // Query chart preview area bounds — polls until valid, then watches for changes
        {
            use_future(move || {
                async move {
                    // Poll for bounds until we get valid ones, then keep updating
                    loop {
                        tokio::time::sleep(tokio::time::Duration::from_millis(200)).await;
                        tracing::trace!("Chart editor: polling for bounds...");

                        let result = document::eval(
                            r#"
                            const el = document.getElementById('chart-editor-preview');
                            if (el) {
                                const rect = el.getBoundingClientRect();
                                const dpr = window.devicePixelRatio || 1;
                                return JSON.stringify({
                                    x: rect.x * dpr,
                                    y: rect.y * dpr,
                                    width: rect.width * dpr,
                                    height: rect.height * dpr,
                                    dpr: dpr
                                });
                            }
                            return "null";
                        "#,
                        );

                        match result.await {
                            Ok(value) => {
                                // Try as string first, then try to_string for non-string JSON
                                let json_str = value
                                    .as_str()
                                    .map(|s| s.to_string())
                                    .unwrap_or_else(|| value.to_string());

                                tracing::trace!("Chart editor bounds eval result: {}", json_str);

                                if json_str != "null" && json_str != "\"null\"" {
                                    match serde_json::from_str::<serde_json::Value>(&json_str) {
                                        Ok(parsed) => {
                                            let x = parsed["x"].as_f64().unwrap_or(0.0);
                                            let y = parsed["y"].as_f64().unwrap_or(0.0);
                                            let width = parsed["width"].as_f64().unwrap_or(0.0);
                                            let height = parsed["height"].as_f64().unwrap_or(0.0);
                                            let dpr = parsed["dpr"].as_f64().unwrap_or(1.0);

                                            if width > 0.0 && height > 0.0 {
                                                let current = *CHART_EDITOR_BOUNDS.read();
                                                // Only update if bounds actually changed
                                                if (current.x - x).abs() > 1.0
                                                    || (current.y - y).abs() > 1.0
                                                    || (current.width - width).abs() > 1.0
                                                    || (current.height - height).abs() > 1.0
                                                {
                                                    tracing::info!(
                                                        "Chart editor bounds updated: ({:.0}, {:.0}, {:.0}x{:.0}), dpr={:.2}",
                                                        x, y, width, height, dpr
                                                    );
                                                    *CHART_EDITOR_BOUNDS.write() =
                                                        ChartEditorBounds::new(
                                                            x, y, width, height, dpr,
                                                        );
                                                }
                                            }
                                        }
                                        Err(e) => {
                                            tracing::warn!("Failed to parse bounds JSON: {}", e);
                                        }
                                    }
                                }
                            }
                            Err(e) => {
                                tracing::debug!("Chart editor bounds eval: {:?}", e);
                            }
                        }
                    }
                }
            });
        }
    }

    rsx! {
        div {
            class: "w-full h-full",
            style: "background: transparent !important;",
            ChartEditorLayout {}
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

/// Live chart preview panel — standalone WGPU-rendered chart with DAW cursor following.
///
/// This is the extracted chart preview that was previously embedded inside
/// PerformanceWithChartToggle. It can now be placed anywhere in the dock layout
/// independently. Renders the active song's chart with auto-follow, click-to-seek,
/// zoom/pan, and smooth cursor interpolation.
#[component]
fn ChartPreviewPanel() -> Element {
    #[cfg(feature = "desktop")]
    {
        let graphics = consume_context::<Arc<std::sync::Mutex<ChartGraphics>>>();

        // --- Chart layout manager (created once, persists across renders) ---
        let perf_layout_manager: Signal<
            Option<std::rc::Rc<std::cell::RefCell<ChartLayoutManager>>>,
        > = use_signal(|| match ChartLayoutManager::new() {
            Ok(m) => {
                tracing::debug!("ChartPreviewPanel: layout manager created");
                Some(std::rc::Rc::new(std::cell::RefCell::new(m)))
            }
            Err(e) => {
                tracing::error!("Failed to create ChartPreviewPanel layout manager: {}", e);
                None
            }
        });

        // Layout generation counter — bumped when layout changes, triggers re-render
        let mut perf_layout_gen = use_signal(|| 0u64);
        let perf_static_scene_cache =
            use_hook(|| std::rc::Rc::new(std::cell::RefCell::new(None::<PerfStaticSceneKey>)));
        let perf_cursor_frame_clock = use_signal(|| 0u64);
        let perf_cursor_motion = use_hook(|| {
            std::rc::Rc::new(std::cell::RefCell::new(PerfCursorMotionState::default()))
        });
        let perf_render_accumulator =
            use_hook(|| std::rc::Rc::new(std::cell::RefCell::new(PerfRenderAccumulator::new())));

        // Enable transparency on mount, disable on unmount
        use_effect(|| {
            document::eval(r#"document.documentElement.classList.add('transparent-mode');"#);
        });
        use_drop(|| {
            // Only remove if no other chart panels are actively visible
            let layout = dock_dioxus::DOCK_LAYOUT.peek();
            if !layout.panel_is_visible(PanelId::ChartEditor)
                && !layout.panel_is_visible(PanelId::ChartPreview)
            {
                document::eval(r#"document.documentElement.classList.remove('transparent-mode');"#);
            }
        });

        // --- Bounds polling: continuously query chart-preview-panel position ---
        {
            use_future(move || async move {
                loop {
                    tokio::time::sleep(tokio::time::Duration::from_millis(200)).await;

                    let result = document::eval(
                        r#"
                        const el = document.getElementById('chart-preview-panel');
                        if (el) {
                            const rect = el.getBoundingClientRect();
                            const dpr = window.devicePixelRatio || 1;
                            return JSON.stringify({
                                x: rect.x * dpr,
                                y: rect.y * dpr,
                                width: rect.width * dpr,
                                height: rect.height * dpr,
                                dpr: dpr
                            });
                        }
                        return "null";
                    "#,
                    );

                    match result.await {
                        Ok(value) => {
                            let json_str = value
                                .as_str()
                                .map(|s| s.to_string())
                                .unwrap_or_else(|| value.to_string());

                            if json_str != "null" && json_str != "\"null\"" {
                                if let Ok(parsed) =
                                    serde_json::from_str::<serde_json::Value>(&json_str)
                                {
                                    let x = parsed["x"].as_f64().unwrap_or(0.0);
                                    let y = parsed["y"].as_f64().unwrap_or(0.0);
                                    let width = parsed["width"].as_f64().unwrap_or(0.0);
                                    let height = parsed["height"].as_f64().unwrap_or(0.0);
                                    let dpr = parsed["dpr"].as_f64().unwrap_or(1.0);

                                    if width > 0.0 && height > 0.0 {
                                        let current = *CHART_AREA_BOUNDS.peek();
                                        if (current.x - x).abs() > 1.0
                                            || (current.y - y).abs() > 1.0
                                            || (current.width - width).abs() > 1.0
                                            || (current.height - height).abs() > 1.0
                                        {
                                            *CHART_AREA_BOUNDS.write() =
                                                ChartAreaBounds::new(x, y, width, height, dpr);
                                        }
                                    }
                                }
                            }
                        }
                        Err(_) => {}
                    }
                }
            });
        }

        // --- 120Hz local cursor ticker ---
        {
            let mut frame_clock = perf_cursor_frame_clock;
            use_future(move || async move {
                let mut interval = tokio::time::interval(Duration::from_millis(8));
                interval.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);
                loop {
                    interval.tick().await;
                    if *ACTIVE_PLAYBACK_IS_PLAYING.peek() {
                        frame_clock.set(frame_clock() + 1);
                    }
                }
            });
        }

        // --- Layout effect: parse + layout chart when source changes ---
        {
            use_effect(move || {
                let source = SESSION_CHART_SOURCE
                    .read()
                    .clone()
                    .unwrap_or_else(|| CHART_SOURCE.read().clone());
                let bounds = *CHART_AREA_BOUNDS.read();

                if !bounds.is_valid() {
                    return;
                }

                if let Some(ref manager_rc) = *perf_layout_manager.read() {
                    let mut manager = manager_rc.borrow_mut();
                    match manager.parse_and_layout(&source, bounds.width, false) {
                        Ok(true) => {
                            perf_layout_gen.set(perf_layout_gen() + 1);
                            tracing::debug!(
                                "ChartPreview layout done (gen {}), pages={}",
                                perf_layout_gen(),
                                manager.total_pages()
                            );
                        }
                        Ok(false) => {}
                        Err(e) => {
                            tracing::warn!("ChartPreview parse error: {}", e);
                        }
                    }
                }
            });
        }

        // --- Render effect: auto-follow cursor OR manual viewport, render scene ---
        {
            let graphics_clone = graphics.clone();
            let static_scene_cache = perf_static_scene_cache.clone();
            let cursor_motion = perf_cursor_motion.clone();
            let render_accumulator = perf_render_accumulator.clone();
            use_effect(move || {
                let frame_started = Instant::now();
                let current_cursor_tick = *CHART_CURSOR_TICK.read();
                let layout_generation = perf_layout_gen();
                let _frame_clock = perf_cursor_frame_clock();
                let bounds = *CHART_AREA_BOUNDS.read();
                let perf_vp = *PERF_CHART_VIEWPORT.read();
                let hover_point = *PERF_CHART_HOVER.read();
                let pending_click = *PERF_CHART_CLICK.read();
                let playback_musical = *ACTIVE_PLAYBACK_MUSICAL.read();
                let playback_is_playing = *ACTIVE_PLAYBACK_IS_PLAYING.read();
                let active_song_index = ACTIVE_INDICES.peek().song_index;

                if !bounds.is_valid() {
                    return;
                }

                if let Some(ref manager_rc) = *perf_layout_manager.read() {
                    let mut manager = manager_rc.borrow_mut();
                    if manager.layout_result().is_none() {
                        return;
                    }

                    let mut cursor_tick = current_cursor_tick;

                    // Follow live DAW playback
                    if playback_is_playing {
                        if let Some(musical) = playback_musical {
                            if let Some(playback_tick) = manager.tick_for_musical_position(
                                musical.measure - 1,
                                musical.beat - 1,
                                musical.subdivision,
                            ) {
                                let now = Instant::now();
                                {
                                    let mut motion = cursor_motion.borrow_mut();
                                    if let (Some(prev_tick), Some(prev_time)) =
                                        (motion.last_sample_tick, motion.last_sample_time)
                                    {
                                        let dt = now.duration_since(prev_time).as_secs_f64();
                                        if dt > 0.0 {
                                            motion.velocity_ticks_per_sec =
                                                (playback_tick - prev_tick) as f64 / dt;
                                        }
                                    }
                                    motion.last_sample_tick = Some(playback_tick);
                                    motion.last_sample_time = Some(now);
                                }
                            }
                        }

                        // Extrapolate cursor forward between transport packets
                        {
                            let motion = cursor_motion.borrow();
                            if let (Some(sample_tick), Some(sample_time)) =
                                (motion.last_sample_tick, motion.last_sample_time)
                            {
                                let elapsed =
                                    Instant::now().duration_since(sample_time).as_secs_f64();
                                let max_ahead = 480.0;
                                let ahead = (motion.velocity_ticks_per_sec * elapsed)
                                    .clamp(-max_ahead, max_ahead);
                                cursor_tick = (sample_tick as f64 + ahead).round() as i64;
                            }
                        }
                    } else {
                        let mut motion = cursor_motion.borrow_mut();
                        motion.last_sample_tick = None;
                        motion.last_sample_time = None;
                        motion.velocity_ticks_per_sec = 0.0;
                    }

                    // Click-to-seek
                    if let Some((scene_x, scene_y)) = pending_click {
                        *PERF_CHART_CLICK.write() = None;
                        if let Some(tick) = manager.tick_at_scene_point(scene_x, scene_y) {
                            cursor_tick = tick;
                            if tick != current_cursor_tick {
                                *CHART_CURSOR_TICK.write() = tick;
                            }

                            if let Some(song_index) = active_song_index {
                                if let Some((measure, beat, subdivision)) =
                                    manager.musical_position_at_tick(tick)
                                {
                                    spawn(async move {
                                        let _ = Session::get()
                                            .setlist()
                                            .seek_to_musical_position(
                                                song_index,
                                                daw_proto::MusicalPosition::new(
                                                    measure + 1,
                                                    beat + 1,
                                                    subdivision,
                                                ),
                                            )
                                            .await;
                                    });
                                }
                            }
                        }
                    }

                    // Compute viewport transform
                    let (page_num, sys_idx) =
                        manager.system_for_tick(cursor_tick).unwrap_or((1, 0));

                    let page_width = manager
                        .layout_result()
                        .and_then(|r| r.pages.iter().find(|p| p.number == page_num))
                        .map(|p| p.width)
                        .unwrap_or(595.0);

                    let pad_physical = 20.0 * bounds.dpr;
                    let available_width = bounds.width - pad_physical * 2.0;
                    let base_scale = if available_width > 0.0 && page_width > 0.0 {
                        available_width / page_width
                    } else {
                        bounds.dpr
                    };

                    if (*PERF_CHART_BASE_SCALE.peek() - base_scale).abs() > 0.001 {
                        *PERF_CHART_BASE_SCALE.write() = base_scale;
                    }

                    let scale = base_scale * perf_vp.zoom;

                    let (scroll_x, scroll_y) = if perf_vp.auto_follow {
                        let sy = manager
                            .scroll_y_for_system(page_num, sys_idx, scale, 1.0, bounds.dpr)
                            .unwrap_or(0.0);

                        let page_x_offset = manager
                            .layout_result()
                            .and_then(|r| {
                                r.pages
                                    .iter()
                                    .find(|p| p.number == page_num)
                                    .map(|p| p.x_offset)
                            })
                            .unwrap_or(0.0);
                        let sx = page_x_offset * scale / bounds.dpr;

                        (sx, sy)
                    } else {
                        (perf_vp.scroll_x, perf_vp.scroll_y)
                    };

                    let transform = Affine::translate((
                        pad_physical - scroll_x * bounds.dpr,
                        pad_physical - scroll_y * bounds.dpr,
                    )) * Affine::scale(scale);

                    let cursor = if *CHART_CURSOR_VISIBLE.peek() {
                        Some(cursor_tick)
                    } else {
                        None
                    };

                    let current_key = PerfStaticSceneKey {
                        generation: layout_generation,
                        width: bounds.width,
                        height: bounds.height,
                        tx: transform.translation().x,
                        ty: transform.translation().y,
                        scale,
                    };

                    let needs_static_rebuild = {
                        let cache = static_scene_cache.borrow();
                        match cache.as_ref() {
                            Some(cached_key) => !cached_key.approx_eq(current_key),
                            None => true,
                        }
                    };

                    if needs_static_rebuild {
                        *static_scene_cache.borrow_mut() = Some(current_key);
                    }

                    let render_start = Instant::now();
                    if let Ok(mut gfx) = graphics_clone.lock() {
                        let win_size = dioxus::desktop::window().window.inner_size();
                        let (sw, sh) = gfx.size();
                        if sw != win_size.width || sh != win_size.height {
                            gfx.resize(win_size.width, win_size.height);
                        }

                        let dock_offset = Affine::translate((bounds.x, bounds.y));
                        gfx.render_chart(|painter| {
                            manager.render_static_layer_to_scene(
                                painter,
                                bounds.width,
                                bounds.height,
                                dock_offset,
                                transform,
                            );
                            manager.render_overlay_layer_to_scene(
                                painter,
                                bounds.width,
                                bounds.height,
                                dock_offset,
                                transform,
                                cursor,
                                if playback_is_playing {
                                    None
                                } else {
                                    hover_point
                                },
                            );
                        });
                    }
                    let render_ms = render_start.elapsed().as_secs_f64() * 1000.0;
                    dioxus::desktop::window().window.request_redraw();

                    let frame_ms = frame_started.elapsed().as_secs_f64() * 1000.0;
                    let mut accum = render_accumulator.borrow_mut();
                    // static_build_ms is now combined in render_ms; pass 0 for overlay
                    // since layers are no longer timed separately
                    accum.record(render_ms, 0.0, frame_ms, needs_static_rebuild);
                    if let Some((
                        frames,
                        avg_frame_ms,
                        p95_ms,
                        avg_overlay_ms,
                        static_rebuilds,
                        avg_static_ms,
                    )) = accum.maybe_flush_log()
                    {
                        info!(
                            "ChartPreview renderer (5s): frames={}, avg={:.2}ms, p95={:.2}ms, overlay={:.2}ms, static_rebuilds={}, static={:.2}ms",
                            frames, avg_frame_ms, p95_ms, avg_overlay_ms, static_rebuilds, avg_static_ms,
                        );
                    }
                }
            });
        }
    }

    // Render the transparent div with mouse handlers
    let perf_vp = *PERF_CHART_VIEWPORT.read();

    // Local state for drag tracking
    let mut dragging = use_signal(|| false);
    let mut dragged = use_signal(|| false);
    let mut last_mouse = use_signal(|| (0.0f64, 0.0f64));

    rsx! {
        div {
            id: "chart-preview-panel",
            class: "h-full w-full relative cursor-grab",
            style: "background: transparent !important; background-color: transparent !important;",

            // Wheel → zoom (disables auto-follow)
            onwheel: move |evt| {
                let delta_y = evt.delta().strip_units().y;
                let mut vp = PERF_CHART_VIEWPORT.write();
                let zoom_factor = if delta_y < 0.0 { 1.05 } else { 0.95 };
                vp.zoom = (vp.zoom * zoom_factor).clamp(0.1, 8.0);
                vp.auto_follow = false;
            },

            // Mouse drag → pan (disables auto-follow)
            onmousedown: move |evt| {
                dragging.set(true);
                dragged.set(false);
                let coords = evt.client_coordinates();
                last_mouse.set((coords.x, coords.y));
            },

            onmousemove: move |evt| {
                let coords = evt.client_coordinates();

                if *dragging.read() {
                    let (lx, ly) = *last_mouse.read();
                    let dx = coords.x - lx;
                    let dy = coords.y - ly;

                    if dx.abs() > 1.0 || dy.abs() > 1.0 {
                        dragged.set(true);
                    }

                    let mut vp = PERF_CHART_VIEWPORT.write();
                    vp.scroll_x -= dx;
                    vp.scroll_y -= dy;
                    vp.auto_follow = false;

                    last_mouse.set((coords.x, coords.y));
                    *PERF_CHART_HOVER.write() = None;
                } else {
                    // Hover: convert CSS coords → scene coords
                    let bounds = *CHART_AREA_BOUNDS.peek();
                    let vp = *PERF_CHART_VIEWPORT.peek();
                    let base_scale = *PERF_CHART_BASE_SCALE.peek();

                    if base_scale > 0.0 && bounds.dpr > 0.0 {
                        let scale = base_scale * vp.zoom;
                        let pad = 20.0 * bounds.dpr;
                        let px_x = coords.x * bounds.dpr - bounds.x;
                        let px_y = coords.y * bounds.dpr - bounds.y;
                        let scene_x = (px_x - pad + vp.scroll_x * bounds.dpr) / scale;
                        let scene_y = (px_y - pad + vp.scroll_y * bounds.dpr) / scale;
                        *PERF_CHART_HOVER.write() = Some((scene_x, scene_y));
                    }
                }
            },

            onmouseup: move |evt| {
                if !*dragged.read() {
                    // Click-to-seek
                    let coords = evt.client_coordinates();
                    let bounds = *CHART_AREA_BOUNDS.peek();
                    let vp = *PERF_CHART_VIEWPORT.peek();
                    let base_scale = *PERF_CHART_BASE_SCALE.peek();

                    if base_scale > 0.0 && bounds.dpr > 0.0 {
                        let scale = base_scale * vp.zoom;
                        let pad = 20.0 * bounds.dpr;
                        let px_x = coords.x * bounds.dpr - bounds.x;
                        let px_y = coords.y * bounds.dpr - bounds.y;
                        let scene_x = (px_x - pad + vp.scroll_x * bounds.dpr) / scale;
                        let scene_y = (px_y - pad + vp.scroll_y * bounds.dpr) / scale;
                        *PERF_CHART_CLICK.write() = Some((scene_x, scene_y));
                    }
                }

                dragging.set(false);
                dragged.set(false);
                *PERF_CHART_HOVER.write() = None;
            },

            onmouseleave: move |_| {
                dragging.set(false);
                dragged.set(false);
                *PERF_CHART_HOVER.write() = None;
            },

            // Reset button — only visible when not in auto-follow mode
            if !perf_vp.auto_follow {
                div {
                    class: "absolute top-4 left-4 z-10",
                    button {
                        class: "bg-card/80 backdrop-blur-sm rounded-lg px-3 py-2 shadow-lg text-sm text-muted-foreground hover:text-foreground hover:bg-card/90 transition-colors flex items-center gap-2",
                        onclick: move |_| {
                            *PERF_CHART_VIEWPORT.write() = PerfChartViewport::default();
                        },
                        svg {
                            width: "14",
                            height: "14",
                            view_box: "0 0 24 24",
                            fill: "none",
                            stroke: "currentColor",
                            stroke_width: "2",
                            stroke_linecap: "round",
                            stroke_linejoin: "round",
                            path { d: "M3 12a9 9 0 1 0 9-9 9.75 9.75 0 0 0-6.74 2.74L3 8" }
                            path { d: "M3 3v5h5" }
                        }
                        "Reset View"
                    }
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

/// Sliding-window FPS tracker, based on vello's examples/with_winit/src/stats.rs.
/// Tracks frame render times and computes FPS / min / max over a 100-frame window.
struct FpsTracker {
    count: usize,
    sum: u64,
    min: u64,
    max: u64,
    samples: std::collections::VecDeque<u64>,
}

const FPS_WINDOW_SIZE: usize = 100;

impl FpsTracker {
    fn new() -> Self {
        Self {
            count: 0,
            sum: 0,
            min: u64::MAX,
            max: u64::MIN,
            samples: std::collections::VecDeque::with_capacity(FPS_WINDOW_SIZE),
        }
    }

    fn add_sample(&mut self, frame_time_us: u64) {
        let oldest = if self.count < FPS_WINDOW_SIZE {
            self.count += 1;
            None
        } else {
            self.samples.pop_front()
        };
        self.sum += frame_time_us;
        self.samples.push_back(frame_time_us);
        if let Some(oldest) = oldest {
            self.sum -= oldest;
        }
        self.min = self.min.min(frame_time_us);
        self.max = self.max.max(frame_time_us);
    }

    fn snapshot(&self) -> RenderStats {
        if self.count == 0 {
            return RenderStats::default();
        }
        let frame_time_ms = (self.sum as f64 / self.count as f64) * 0.001;
        let fps = 1000.0 / frame_time_ms;
        RenderStats {
            fps,
            frame_time_ms,
            frame_time_min_ms: self.min as f64 * 0.001,
            frame_time_max_ms: self.max as f64 * 0.001,
        }
    }
}
