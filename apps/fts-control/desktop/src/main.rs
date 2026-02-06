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
    AUDIO_LATENCY_SECONDS, CHART_AREA_BOUNDS, LATENCY_INFO, SHOW_CHART_SPLIT,
};

use keyflow_ui::signals::{ChartEditorBounds, PreviewMode};
use keyflow_ui::{
    ChartEditorLayout, ChartLayoutManager, RenderStats, CHART_EDITOR_BOUNDS, CHART_PREVIEW_MODE,
    CHART_RENDER_STATS, CHART_SOURCE, CHART_VIEWPORT,
};
use kurbo::Affine;

use tokio;
use tracing::info;

const FAVICON: Asset = asset!("/assets/favicon.ico");
const MAIN_CSS: Asset = asset!("/assets/main.css");
const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");

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
                        }

                        session_proto::SetlistEvent::ActiveIndicesChanged(indices) => {
                            *ACTIVE_INDICES.write() = indices.clone();
                            *PLAYBACK_STATE.write() = if indices.is_playing {
                                daw_proto::PlayState::Playing
                            } else {
                                daw_proto::PlayState::Stopped
                            };
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
/// The split logic is handled inside PerformanceMainContent via SHOW_CHART_SPLIT signal.
#[component]
fn PerformanceWithChartToggle() -> Element {
    let show_chart = *SHOW_CHART_SPLIT.read();

    // Handle chart rendering and transparency when split is enabled
    #[cfg(feature = "desktop")]
    {
        let graphics = consume_context::<Arc<std::sync::Mutex<ChartGraphics>>>();

        // Query chart area bounds and render bounding box
        use_effect(move || {
            if show_chart {
                // Enable transparent mode
                document::eval(r#"document.documentElement.classList.add('transparent-mode');"#);

                // Query the chart area bounds after a small delay to ensure DOM is updated
                let graphics_clone = graphics.clone();
                spawn(async move {
                    // Small delay to let the DOM update
                    tokio::time::sleep(tokio::time::Duration::from_millis(50)).await;

                    // Query the chart-render-area element bounds
                    // Note: We need to account for devicePixelRatio on high-DPI displays
                    // getBoundingClientRect returns CSS pixels, but WGPU uses physical pixels
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
                                dpr: dpr,
                                cssX: rect.x,
                                cssY: rect.y,
                                cssWidth: rect.width,
                                cssHeight: rect.height
                            });
                        }
                        return "null";
                    "#,
                    );

                    // Parse the result and update bounds
                    match result.await {
                        Ok(value) => {
                            // Try to get as string first
                            if let Some(json_str) = value.as_str() {
                                if json_str != "null" {
                                    if let Ok(bounds) =
                                        serde_json::from_str::<serde_json::Value>(json_str)
                                    {
                                        // Physical pixel coordinates (scaled by DPR)
                                        let x = bounds["x"].as_f64().unwrap_or(0.0);
                                        let y = bounds["y"].as_f64().unwrap_or(0.0);
                                        let width = bounds["width"].as_f64().unwrap_or(0.0);
                                        let height = bounds["height"].as_f64().unwrap_or(0.0);
                                        let dpr = bounds["dpr"].as_f64().unwrap_or(1.0);
                                        let css_x = bounds["cssX"].as_f64().unwrap_or(0.0);
                                        let css_y = bounds["cssY"].as_f64().unwrap_or(0.0);

                                        tracing::info!(
                                            "Chart bounds: physical=({:.0}, {:.0}, {:.0}x{:.0}), css=({:.0}, {:.0}), dpr={:.2}",
                                            x, y, width, height, css_x, css_y, dpr
                                        );

                                        // Update the global signal (store physical pixel coordinates)
                                        *CHART_AREA_BOUNDS.write() =
                                            ChartAreaBounds::new(x, y, width, height);

                                        // Render bounding box at those coordinates
                                        if let Ok(mut gfx) = graphics_clone.lock() {
                                            gfx.render_bounds(x, y, width, height);
                                        }
                                        dioxus::desktop::window().window.request_redraw();
                                    }
                                }
                            }
                        }
                        Err(e) => {
                            tracing::warn!("Failed to get chart area bounds: {:?}", e);
                        }
                    }
                });
            } else {
                // Disable transparent mode
                document::eval(r#"document.documentElement.classList.remove('transparent-mode');"#);
                // Clear bounds
                *CHART_AREA_BOUNDS.write() = ChartAreaBounds::default();
            }
        });
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

                    manager.render_to_scene(&mut scene, bounds.width, bounds.height, transform);

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
