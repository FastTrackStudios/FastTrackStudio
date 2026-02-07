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

#[cfg(feature = "desktop")]
use chart_graphics::ChartGraphics;
#[cfg(feature = "desktop")]
use dioxus::desktop::{tao::window::WindowBuilder, Config};

use session_ui::{
    ChartAreaBounds, ConnectionState, LatencyInfo, PerformanceLayout, Session, TopBar,
    ACTIVE_INDICES, AUDIO_LATENCY_SECONDS, CHART_AREA_BOUNDS, LATENCY_INFO,
    PERF_CHART_BASE_SCALE, PERF_CHART_CLICK, PERF_CHART_HOVER, PERF_CHART_VIEWPORT,
    SHOW_CHART_SPLIT, SONG_TRANSPORT,
};

use keyflow_ui::signals::{ChartEditorBounds, PreviewMode};
use keyflow_ui::{
    ChartEditorLayout, ChartLayoutManager, RenderStats, CHART_BASE_SCALE, CHART_CURSOR_POSITION,
    CHART_CURSOR_SCENE_CLICK, CHART_CURSOR_TICK, CHART_CURSOR_VISIBLE, CHART_EDITOR_BOUNDS,
    CHART_HOVER_SCENE_POINT, CHART_PAGE_INFO, CHART_PREVIEW_MODE, CHART_RENDER_STATS, CHART_SOURCE,
    CHART_VIEWPORT, SESSION_CHART_SOURCE,
};
use kurbo::Affine;
use signal_ui::RigLayout;

use tokio;
use tracing::info;

const FAVICON: Asset = asset!("/assets/favicon.ico");
const MAIN_CSS: Asset = asset!("/assets/main.css");
const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");
const CHART_LOADING_PLACEHOLDER: &str = "Generating Chart\n\n[Loading]\n| C |";

fn refresh_session_chart_source() {
    use session_ui::{ACTIVE_INDICES, SETLIST_STRUCTURE};

    let next_source = ACTIVE_INDICES
        .peek()
        .song_index
        .and_then(|song_index| SETLIST_STRUCTURE.peek().songs.get(song_index).cloned())
        .map(|song| {
            song.chart_text.unwrap_or_else(|| {
                format!("{} (Generating)\n\n[Loading]\n| C |", song.name)
            })
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
                .add_directive("fts_control_desktop=debug".parse().unwrap())
                .add_directive("session=debug".parse().unwrap())
                .add_directive("gateway_ws=debug".parse().unwrap()),
        )
        .init();

    #[cfg(feature = "desktop")]
    info!("Starting FTS Control Desktop (Wry/WebView + WGPU hybrid renderer)");
    #[cfg(feature = "native")]
    info!("Starting FTS Control Desktop (Blitz/Native renderer)");

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

    // Connection state - tracks DAW connection
    let mut connection_state = use_signal(|| ConnectionState::Connecting);

    // WGPU/Vello chart graphics context (desktop only)
    #[cfg(feature = "desktop")]
    let graphics = consume_context::<Arc<std::sync::Mutex<ChartGraphics>>>();

    // Request initial redraw on mount (desktop only)
    #[cfg(feature = "desktop")]
    use_effect(|| {
        dioxus::desktop::window().window.request_redraw();
    });

    // Handle window resize events to update WGPU surface (desktop only)
    #[cfg(feature = "desktop")]
    {
        use dioxus::desktop::{tao::event::Event as WryEvent, use_wry_event_handler, window};

        let graphics_clone = graphics.clone();
        use_wry_event_handler(move |event, _| {
            use dioxus::desktop::tao::event::WindowEvent;

            if let WryEvent::WindowEvent {
                event: WindowEvent::Resized(new_size),
                ..
            } = event
            {
                if let Ok(mut gfx) = graphics_clone.lock() {
                    tracing::info!(
                        "Window resized: {}x{} (was {}x{})",
                        new_size.width,
                        new_size.height,
                        gfx.size().0,
                        gfx.size().1
                    );
                    gfx.resize(new_size.width, new_size.height);
                }

                // Request a redraw
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

        info!("Starting audio latency polling (Dioxus context)");

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
        info!("DAW connected, initializing...");

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
                            info!("Active song: {} (index {})", active_song.name, idx);
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
            Ok(None) => info!("No setlist available"),
            Err(e) => tracing::warn!("Failed to get setlist: {}", e),
        }

        // Subscribe to setlist events (transport updates, active song changes, etc.)
        info!("Subscribing to setlist events...");
        let (tx, mut rx) = roam::channel::<session_proto::SetlistEvent>();

        match setlist_client.subscribe(tx).await {
            Ok(()) => {
                info!("Subscribed to SetlistService events (60Hz transport updates)");

                // Process events continuously
                while let Ok(Some(event)) = rx.recv().await {
                    match event {
                        session_proto::SetlistEvent::SetlistChanged(setlist) => {
                            info!("Setlist changed: {} songs", setlist.songs.len());
                            *SETLIST_STRUCTURE.write() = setlist;
                            refresh_session_chart_source();
                        }
                        session_proto::SetlistEvent::SongHydrated { index, song } => {
                            let mut setlist = SETLIST_STRUCTURE.write();
                            if index < setlist.songs.len() {
                                setlist.songs[index] = song;
                            }
                            drop(setlist);
                            refresh_session_chart_source();
                        }

                        session_proto::SetlistEvent::ActiveIndicesChanged(indices) => {
                            *ACTIVE_INDICES.write() = indices.clone();
                            *PLAYBACK_STATE.write() = if indices.is_playing {
                                daw_proto::PlayState::Playing
                            } else {
                                daw_proto::PlayState::Stopped
                            };
                            refresh_session_chart_source();
                        }

                        session_proto::SetlistEvent::TransportUpdate(transports) => {
                            // PERFORMANCE: Only write to signals if values actually changed.
                            // Each .write() triggers re-renders for all subscribers.

                            let active_song_index = ACTIVE_INDICES.peek().song_index;
                            let audio_latency = *AUDIO_LATENCY_SECONDS.peek();

                            for transport in transports {
                                // Build the new transport state
                                let compensated_position =
                                    if transport.is_playing && audio_latency > 0.0 {
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
                                        let setlist = SETLIST_STRUCTURE.peek();
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

                                // Only write SONG_TRANSPORT if this song's transport changed
                                // Must compute needs_update and drop peek() before calling write()
                                let needs_transport_update = SONG_TRANSPORT
                                    .peek()
                                    .get(&transport.song_index)
                                    .map(|existing| *existing != new_transport)
                                    .unwrap_or(true);

                                if needs_transport_update {
                                    SONG_TRANSPORT
                                        .write()
                                        .insert(transport.song_index, new_transport);
                                }

                                // Update ACTIVE_INDICES and PLAYBACK_STATE only for active song
                                if Some(transport.song_index) == active_song_index {
                                    // Check if playback state changed
                                    let new_state = if transport.is_playing {
                                        daw_proto::PlayState::Playing
                                    } else {
                                        daw_proto::PlayState::Stopped
                                    };

                                    let needs_playback_update = *PLAYBACK_STATE.peek() != new_state;
                                    if needs_playback_update {
                                        *PLAYBACK_STATE.write() = new_state;
                                    }

                                    // Check if indices actually changed before writing
                                    // Compute the check, dropping the peek() borrow before write()
                                    let indices_changed = {
                                        let current = ACTIVE_INDICES.peek();
                                        current.song_progress != Some(transport.progress)
                                            || current.section_progress
                                                != transport.section_progress
                                            || current.section_index != transport.section_index
                                            || current.is_playing != transport.is_playing
                                            || current.looping != transport.is_looping
                                    };

                                    if indices_changed {
                                        let mut indices = ACTIVE_INDICES.write();
                                        indices.song_progress = Some(transport.progress);
                                        indices.section_progress = transport.section_progress;
                                        indices.section_index = transport.section_index;
                                        indices.is_playing = transport.is_playing;
                                        indices.looping = transport.is_looping;
                                    }
                                }
                            }
                        }

                        session_proto::SetlistEvent::SongEntered { index, song } => {
                            info!("Entered song {}: {}", index, song.name);
                        }

                        session_proto::SetlistEvent::SongExited { index } => {
                            info!("Exited song {}", index);
                        }

                        session_proto::SetlistEvent::SectionEntered {
                            song_index,
                            section_index,
                            section,
                        } => {
                            info!(
                                "Entered section {}.{}: {}",
                                song_index, section_index, section.name
                            );
                        }

                        session_proto::SetlistEvent::SectionExited {
                            song_index,
                            section_index,
                        } => {
                            info!("Exited section {}.{}", song_index, section_index);
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
            let show_chart_split = *SHOW_CHART_SPLIT.read();
            let needs_transparency = active_tab() == "chart" || (active_tab() == "performance" && show_chart_split);

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
                        // Only handle keyboard shortcuts on performance tab
                        if active_tab() == "performance" {
                            handle_keyboard_shortcut(e);
                        }
                    },

                    // Top navigation bar (same as web)
                    TopBar {
                        connection_state: connection_state(),
                        active_tab: active_tab(),
                        on_tab_click: Some(Callback::new(move |tab: String| {
                            active_tab.set(tab);
                        })),
                    }

                    // Main content area
                    div {
                        class: "flex-1 overflow-hidden relative",
                        style: if needs_transparency { "background: transparent !important; background-color: transparent !important;" } else { "" },
                        match active_tab().as_str() {
                            "performance" => rsx! { PerformanceWithChartToggle {} },
                            "chart" => rsx! { ChartView {} },
                            "setlist" => rsx! { SetlistView {} },
                            "rig" => rsx! { RigLayout {} },
                            "settings" => rsx! { SettingsView {} },
                            _ => rsx! { PerformanceWithChartToggle {} },
                        }
                    }
                }
            }
        }
    }
}

/// Handle keyboard shortcuts for session actions
fn handle_keyboard_shortcut(e: KeyboardEvent) {
    // Don't handle if modifier keys are pressed (except for specific shortcuts)
    // This prevents interfering with browser/system shortcuts
    if e.modifiers().ctrl() || e.modifiers().alt() || e.modifiers().meta() {
        return;
    }

    match e.key() {
        // Space - Toggle playback
        Key::Character(c) if c == " " => {
            e.prevent_default();
            spawn(async move {
                tracing::debug!("Keyboard: Space -> toggle_playback");
                let _ = Session::get().setlist().toggle_playback().await;
            });
        }
        // L - Toggle loop
        Key::Character(c) if c.to_lowercase() == "l" => {
            e.prevent_default();
            spawn(async move {
                tracing::debug!("Keyboard: L -> toggle_song_loop");
                let _ = Session::get().setlist().toggle_song_loop().await;
            });
        }
        // Right arrow - Smart next (next section, then next song)
        Key::ArrowRight => {
            e.prevent_default();
            spawn(async move {
                tracing::debug!("Keyboard: Right -> next_section (smart next)");
                let _ = Session::get().setlist().next_section().await;
            });
        }
        // Left arrow - Smart previous (previous section, then previous song)
        Key::ArrowLeft => {
            e.prevent_default();
            spawn(async move {
                tracing::debug!("Keyboard: Left -> previous_section (smart previous)");
                let _ = Session::get().setlist().previous_section().await;
            });
        }
        // Down arrow - Next song
        Key::ArrowDown => {
            e.prevent_default();
            spawn(async move {
                tracing::debug!("Keyboard: Down -> next_song");
                let _ = Session::get().setlist().next_song().await;
            });
        }
        // Up arrow - Previous song
        Key::ArrowUp => {
            e.prevent_default();
            spawn(async move {
                tracing::debug!("Keyboard: Up -> previous_song");
                let _ = Session::get().setlist().previous_song().await;
            });
        }
        _ => {}
    }
}

/// Performance view with optional chart split
///
/// Wraps PerformanceLayout and adds a chart toggle button.
/// When the chart split is active, renders a 2-line zoomed chart that
/// auto-follows the cursor position, showing the current system plus
/// the next one.
#[component]
fn PerformanceWithChartToggle() -> Element {
    let show_chart = *SHOW_CHART_SPLIT.read();

    #[cfg(feature = "desktop")]
    {
        let graphics = consume_context::<Arc<std::sync::Mutex<ChartGraphics>>>();

        // --- Chart layout manager (created once, persists across renders) ---
        let perf_layout_manager: Signal<
            Option<std::rc::Rc<std::cell::RefCell<ChartLayoutManager>>>,
        > = use_signal(|| match ChartLayoutManager::new() {
            Ok(m) => {
                tracing::debug!("Perf ChartLayoutManager created");
                Some(std::rc::Rc::new(std::cell::RefCell::new(m)))
            }
            Err(e) => {
                tracing::error!("Failed to create perf ChartLayoutManager: {}", e);
                None
            }
        });

        // Layout generation counter — bumped when layout changes, triggers re-render
        let mut perf_layout_gen = use_signal(|| 0u64);

        // Transparency management — read signal inside effect for reactivity
        use_effect(move || {
            let chart_on = *SHOW_CHART_SPLIT.read();
            if chart_on {
                document::eval(r#"document.documentElement.classList.add('transparent-mode');"#);
            } else {
                document::eval(r#"document.documentElement.classList.remove('transparent-mode');"#);
                *CHART_AREA_BOUNDS.write() = ChartAreaBounds::default();
            }
        });

        // --- Bounds polling: continuously query chart-render-area position ---
        {
            use_future(move || async move {
                loop {
                    tokio::time::sleep(tokio::time::Duration::from_millis(200)).await;

                    if !*SHOW_CHART_SPLIT.peek() {
                        continue;
                    }

                    let result = document::eval(
                        r#"
                        const el = document.getElementById('chart-render-area');
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

        // --- Layout effect: parse + layout chart when source changes ---
        {
            use_effect(move || {
                let source = SESSION_CHART_SOURCE
                    .read()
                    .clone()
                    .unwrap_or_else(|| CHART_SOURCE.read().clone());
                let bounds = *CHART_AREA_BOUNDS.read();
                let chart_visible = *SHOW_CHART_SPLIT.read();

                tracing::debug!(
                    "Perf layout effect: visible={}, bounds=({:.0},{:.0} {:.0}x{:.0})",
                    chart_visible,
                    bounds.x,
                    bounds.y,
                    bounds.width,
                    bounds.height
                );

                if !chart_visible || !bounds.is_valid() {
                    return;
                }

                // Always use paginated mode (Page) for the performance view
                if let Some(ref manager_rc) = *perf_layout_manager.read() {
                    let mut manager = manager_rc.borrow_mut();
                    match manager.parse_and_layout(&source, bounds.width, false) {
                        Ok(true) => {
                            perf_layout_gen.set(perf_layout_gen() + 1);
                            tracing::debug!(
                                "Perf chart layout done (gen {}), pages={}",
                                perf_layout_gen(),
                                manager.total_pages()
                            );
                        }
                        Ok(false) => {}
                        Err(e) => {
                            tracing::warn!("Perf chart parse error: {}", e);
                        }
                    }
                } else {
                    tracing::error!("Perf layout: manager is None!");
                }
            });
        }

        // --- Render effect: auto-follow cursor OR manual viewport, render scene ---
        {
            let graphics_clone = graphics.clone();
            use_effect(move || {
                let current_cursor_tick = *CHART_CURSOR_TICK.read();
                let _gen = perf_layout_gen(); // Subscribe to layout changes
                let bounds = *CHART_AREA_BOUNDS.read();
                let chart_visible = *SHOW_CHART_SPLIT.read();
                let perf_vp = *PERF_CHART_VIEWPORT.read();
                let hover_point = *PERF_CHART_HOVER.read();
                let pending_click = *PERF_CHART_CLICK.read();
                let active_song_index = ACTIVE_INDICES.read().song_index;
                let active_transport =
                    active_song_index.and_then(|idx| SONG_TRANSPORT.read().get(&idx).cloned());
                let playback_musical = active_transport
                    .as_ref()
                    .and_then(|transport| transport.position.musical);
                let playback_is_playing = active_transport
                    .as_ref()
                    .map(|transport| transport.is_playing)
                    .unwrap_or(false);

                if !chart_visible || !bounds.is_valid() {
                    return;
                }

                if let Some(ref manager_rc) = *perf_layout_manager.read() {
                    let mut manager = manager_rc.borrow_mut();
                    if manager.layout_result().is_none() {
                        return;
                    }

                    let mut cursor_tick = current_cursor_tick;

                    // Follow live DAW playback by resolving current musical position into chart tick.
                    if playback_is_playing {
                        if let Some(musical) = playback_musical {
                            if let Some(playback_tick) = manager.tick_for_musical_position(
                                musical.measure,
                                musical.beat,
                                musical.subdivision,
                            ) {
                                cursor_tick = playback_tick;
                                if playback_tick != current_cursor_tick {
                                    *CHART_CURSOR_TICK.write() = playback_tick;
                                }
                            }
                        }
                    }

                    // Click-to-seek from performance chart area.
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
                                                    measure,
                                                    beat,
                                                    subdivision,
                                                ),
                                            )
                                            .await;
                                    });
                                }
                            }
                        }
                    }

                    // Find which page the cursor is on (needed for base_scale in both modes)
                    let (page_num, sys_idx) =
                        manager.system_for_tick(cursor_tick).unwrap_or((1, 0));

                    // Compute base_scale: fits page width into viewport at zoom=1.0
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

                    // Expose base_scale for mouse coordinate conversion in UI
                    if (*PERF_CHART_BASE_SCALE.peek() - base_scale).abs() > 0.001 {
                        *PERF_CHART_BASE_SCALE.write() = base_scale;
                    }

                    // Compute effective scale (base * user zoom)
                    let scale = base_scale * perf_vp.zoom;

                    let (scroll_x, scroll_y) = if perf_vp.auto_follow {
                        // Auto-follow: compute scroll from cursor position
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

                        // Keep scroll values in sync so switching to manual mode
                        // preserves the current view position instead of jumping to 0,0
                        {
                            let current = PERF_CHART_VIEWPORT.peek();
                            if (current.scroll_x - sx).abs() > 0.5
                                || (current.scroll_y - sy).abs() > 0.5
                            {
                                drop(current);
                                let mut vp = PERF_CHART_VIEWPORT.write();
                                vp.scroll_x = sx;
                                vp.scroll_y = sy;
                            }
                        }

                        (sx, sy)
                    } else {
                        // Manual mode: use user's scroll values directly
                        (perf_vp.scroll_x, perf_vp.scroll_y)
                    };

                    let transform = Affine::translate((
                        pad_physical - scroll_x * bounds.dpr,
                        pad_physical - scroll_y * bounds.dpr,
                    )) * Affine::scale(scale);

                    let mut scene = vello::Scene::new();

                    let cursor = if *CHART_CURSOR_VISIBLE.peek() {
                        Some(cursor_tick)
                    } else {
                        None
                    };

                    manager.render_to_scene(
                        &mut scene,
                        bounds.width,
                        bounds.height,
                        transform,
                        cursor,
                        hover_point,
                    );

                    if let Ok(mut gfx) = graphics_clone.lock() {
                        let win_size = dioxus::desktop::window().window.inner_size();
                        let (sw, sh) = gfx.size();
                        if sw != win_size.width || sh != win_size.height {
                            gfx.resize(win_size.width, win_size.height);
                        }

                        gfx.render_chart_scene(
                            &scene,
                            bounds.x,
                            bounds.y,
                            bounds.width,
                            bounds.height,
                        );
                    }
                    dioxus::desktop::window().window.request_redraw();
                }
            });
        }
    }

    rsx! {
        div {
            class: "relative h-full w-full",
            style: if show_chart { "background: transparent !important; background-color: transparent !important;" } else { "" },

            // The PerformanceLayout handles the split internally via SHOW_CHART_SPLIT
            PerformanceLayout {}

            // Chart toggle button (top-right of main content area)
            div {
                class: "absolute top-4 z-50",
                style: "right: 1rem;",
                button {
                    class: if show_chart {
                        "px-3 py-2 rounded-lg bg-primary text-primary-foreground text-sm font-medium transition-colors flex items-center gap-2 shadow-lg"
                    } else {
                        "px-3 py-2 rounded-lg bg-secondary hover:bg-secondary/80 text-secondary-foreground text-sm font-medium transition-colors flex items-center gap-2 shadow-lg"
                    },
                    onclick: move |_| {
                        let current = *SHOW_CHART_SPLIT.peek();
                        *SHOW_CHART_SPLIT.write() = !current;
                    },
                    // Music note / chart icon
                    svg {
                        width: "16",
                        height: "16",
                        view_box: "0 0 24 24",
                        fill: "none",
                        stroke: "currentColor",
                        stroke_width: "2",
                        stroke_linecap: "round",
                        stroke_linejoin: "round",
                        path { d: "M9 18V5l12-2v13" }
                        circle { cx: "6", cy: "18", r: "3" }
                        circle { cx: "18", cy: "16", r: "3" }
                    }
                    if show_chart {
                        "Hide Chart"
                    } else {
                        "Show Chart"
                    }
                }
            }
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

        // Cleanup: remove transparent mode when component unmounts
        use_drop(move || {
            document::eval(r#"document.documentElement.classList.remove('transparent-mode');"#);
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
                let active_song_index = ACTIVE_INDICES.read().song_index;
                let active_transport =
                    active_song_index.and_then(|idx| SONG_TRANSPORT.read().get(&idx).cloned());
                let playback_musical = active_transport
                    .as_ref()
                    .and_then(|transport| transport.position.musical);
                let playback_is_playing = active_transport
                    .as_ref()
                    .map(|transport| transport.is_playing)
                    .unwrap_or(false);
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

                    let mut scene = vello::Scene::new();
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
                    if playback_is_playing {
                        if let Some(musical) = playback_musical {
                            if let Some(playback_tick) = manager.tick_for_musical_position(
                                musical.measure,
                                musical.beat,
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
                                    spawn(async move {
                                        let _ = Session::get()
                                            .setlist()
                                            .seek_to_musical_position(
                                                song_index,
                                                daw_proto::MusicalPosition::new(
                                                    measure,
                                                    beat,
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

                    manager.render_to_scene(
                        &mut scene,
                        bounds.width,
                        bounds.height,
                        transform,
                        cursor_tick,
                        hover_point,
                    );

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
                            tracing::info!(
                                "Surface resize: {}x{} -> {}x{}",
                                sw,
                                sh,
                                win_size.width,
                                win_size.height
                            );
                            gfx.resize(win_size.width, win_size.height);
                        }

                        gfx.render_chart_scene(
                            &scene,
                            bounds.x,
                            bounds.y,
                            bounds.width,
                            bounds.height,
                        );
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

    info!("Initializing services...");

    // 1. Create local session services
    let services = LocalServices::new();
    info!("Local services initialized");

    // 2. Initialize Session singleton for UI components
    match services.create_setlist_client().await {
        Ok(setlist_client) => {
            if let Err(e) = Session::init(setlist_client) {
                tracing::warn!("Failed to initialize Session singleton: {}", e);
            } else {
                info!("Session singleton initialized");
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
                    info!(
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
                    info!("DAW connection ready - UI can now fetch setlist");

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
    info!("Starting gateway on {}", config.bind_addr);

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
