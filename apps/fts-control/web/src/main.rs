//! FTS Control Web App
//!
//! Dioxus-based web app that connects to the gateway via binary WebSocket
//! and uses the daw-control API to control the DAW.
//!
//! # Architecture
//!
//! Browser ─── roam-websocket (binary postcard) ───> gateway-ws ───> host ───> daw-standalone
//!
//! The web app uses the exact same `daw-control` API as desktop apps, making
//! UI components fully portable between platforms.
//!
//! Note: This crate is WASM-only. On native targets, it compiles to an empty binary.

#![cfg_attr(not(target_arch = "wasm32"), allow(unused))]

#[cfg(target_arch = "wasm32")]
mod actions;

#[cfg(target_arch = "wasm32")]
use actions::{ActionManager, ActionSource, CommandPalette};
#[cfg(target_arch = "wasm32")]
use daw_control::Daw;
#[cfg(target_arch = "wasm32")]
use daw_proto::Transport as TransportState;
#[cfg(target_arch = "wasm32")]
use dioxus::prelude::*;
#[cfg(target_arch = "wasm32")]
use roam::session::ConnectionHandle;
#[cfg(target_arch = "wasm32")]
use roam_session::{initiate_framed, HandshakeConfig, NoDispatcher};
#[cfg(target_arch = "wasm32")]
use roam_websocket::WsTransport;
#[cfg(target_arch = "wasm32")]
use std::sync::Arc;

#[cfg(target_arch = "wasm32")]
const FAVICON: Asset = asset!("/assets/favicon.ico");
#[cfg(target_arch = "wasm32")]
const MAIN_CSS: Asset = asset!("/assets/main.css");
#[cfg(target_arch = "wasm32")]
const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");

fn main() {
    #[cfg(target_arch = "wasm32")]
    {
        // Initialize tracing for browser console
        tracing_wasm::set_as_global_default();
        dioxus::launch(App);
    }

    #[cfg(not(target_arch = "wasm32"))]
    {
        eprintln!("This binary only runs on wasm32. Use `dx build` to compile for WASM.");
    }
}

// ============================================================================
// Everything below only compiles for WASM
// ============================================================================

/// Determine the WebSocket URL for connecting to gateway-ws.
///
/// Priority:
/// 1. Query param `?ws=ws://...` if present
/// 2. Same hostname as page, but always port 3030 (gateway-ws default)
/// 3. Fallback to ws://localhost:3030/ws
#[cfg(target_arch = "wasm32")]
fn get_websocket_url() -> String {
    // Check for explicit ws= query param (useful for testing)
    if let Some(url) = get_ws_query_param() {
        return url;
    }

    // Use the page's hostname but connect to gateway-ws port (3030)
    web_sys::window()
        .and_then(|w| w.location().hostname().ok())
        .map(|hostname| {
            // Use wss:// if page is https://, otherwise ws://
            let protocol = web_sys::window()
                .and_then(|w| w.location().protocol().ok())
                .map(|p| if p == "https:" { "wss" } else { "ws" })
                .unwrap_or("ws");
            // Always use port 3030 where gateway-ws listens
            format!("{protocol}://{hostname}:3030/ws")
        })
        .unwrap_or_else(|| "ws://localhost:3030/ws".to_string())
}

/// Check for ?ws=... query parameter
#[cfg(target_arch = "wasm32")]
fn get_ws_query_param() -> Option<String> {
    web_sys::window()
        .and_then(|w| w.location().search().ok())
        .and_then(|search| {
            for param in search.trim_start_matches('?').split('&') {
                if let Some(url) = param.strip_prefix("ws=") {
                    return Some(url.to_string());
                }
            }
            None
        })
}

/// Connect to the gateway WebSocket and return a connection handle
#[cfg(target_arch = "wasm32")]
async fn connect_to_gateway() -> Result<ConnectionHandle, String> {
    let ws_url = get_websocket_url();
    tracing::info!("Connecting to gateway at {}", ws_url);

    // Connect via roam-websocket (WASM implementation uses web_sys::WebSocket)
    let transport = WsTransport::connect(&ws_url)
        .await
        .map_err(|e| format!("WebSocket connect failed: {}", e))?;

    // Initiate the roam connection (we're the client/initiator)
    let (handle, _incoming, driver) =
        initiate_framed(transport, HandshakeConfig::default(), NoDispatcher)
            .await
            .map_err(|e| format!("Handshake failed: {}", e))?;

    // Spawn the driver to process messages in the background
    // roam_session::runtime::spawn works on both native (tokio) and WASM
    wasm_bindgen_futures::spawn_local(async move {
        if let Err(e) = driver.run().await {
            tracing::warn!("Connection driver ended: {}", e);
        }
    });

    tracing::info!("Connected to gateway successfully");
    Ok(handle)
}

#[cfg(target_arch = "wasm32")]
#[component]
fn App() -> Element {
    // Command palette state
    let mut palette_open = use_signal(|| false);
    let selected_source = use_signal(|| ActionSource::Standalone);
    let mut action_manager: Signal<Option<Arc<async_lock::RwLock<ActionManager>>>> =
        use_signal(|| None);

    // Initialize action manager
    use_effect(move || {
        spawn(async move {
            let manager = ActionManager::new().await;
            action_manager.set(Some(Arc::new(async_lock::RwLock::new(manager))));
        });
    });

    // Global keyboard shortcut for command palette (Cmd+Shift+P)
    let on_keydown = move |e: KeyboardEvent| {
        if e.modifiers().meta()
            && e.modifiers().shift()
            && e.key() == Key::Character("p".to_string())
        {
            palette_open.set(true);
        }
    };

    rsx! {
        document::Link { rel: "icon", href: FAVICON }
        document::Link { rel: "stylesheet", href: MAIN_CSS }
        document::Link { rel: "stylesheet", href: TAILWIND_CSS }

        div {
            onkeydown: on_keydown,
            tabindex: 0,

            DawController {
                action_manager: action_manager.clone(),
                palette_open: palette_open.clone(),
            }

            CommandPalette {
                is_open: palette_open,
                manager: action_manager,
                selected_source: selected_source,
            }
        }
    }
}

/// Connection state shared across components
#[cfg(target_arch = "wasm32")]
#[derive(Clone)]
enum ConnectionState {
    Disconnected,
    Connecting,
    Connected(ConnectionHandle),
    Error(String),
}

/// Transport state for UI display
#[cfg(target_arch = "wasm32")]
#[derive(Clone, Default)]
struct TransportDisplayState {
    is_playing: bool,
    is_recording: bool,
    is_looping: bool,
    position: f64,
    tempo: f64,
    playrate: f64,
}

/// Helper to extract position in seconds from transport state
#[cfg(target_arch = "wasm32")]
fn get_position_seconds(state: &TransportState) -> f64 {
    state
        .playhead_position
        .time
        .as_ref()
        .map(|t| t.to_seconds())
        .unwrap_or(0.0)
}

/// Create a TransportDisplayState from a TransportState
#[cfg(target_arch = "wasm32")]
fn to_display_state(state: &TransportState) -> TransportDisplayState {
    TransportDisplayState {
        is_playing: state.play_state == daw_proto::PlayState::Playing
            || state.play_state == daw_proto::PlayState::Recording,
        is_recording: state.play_state == daw_proto::PlayState::Recording,
        is_looping: state.looping,
        position: get_position_seconds(state),
        tempo: state.tempo.bpm,
        playrate: state.playrate,
    }
}

#[cfg(target_arch = "wasm32")]
#[derive(Props, Clone, PartialEq)]
struct DawControllerProps {
    action_manager: Signal<Option<Arc<async_lock::RwLock<ActionManager>>>>,
    palette_open: Signal<bool>,
}

#[cfg(target_arch = "wasm32")]
#[component]
fn DawController(mut props: DawControllerProps) -> Element {
    // Connection state
    let mut conn_state = use_signal(|| ConnectionState::Disconnected);
    let mut transport_state = use_signal(TransportDisplayState::default);
    let mut project_name = use_signal(|| String::from("No project"));

    // Tempo input state
    let mut tempo_input = use_signal(|| String::from("120"));
    // Position input state
    let mut position_input = use_signal(|| String::from("0"));
    // Playrate input state
    let mut playrate_input = use_signal(|| String::from("1.0"));

    // Connect on mount
    use_effect(move || {
        let action_manager = props.action_manager.clone();
        spawn(async move {
            conn_state.set(ConnectionState::Connecting);

            match connect_to_gateway().await {
                Ok(handle) => {
                    // Try to get current project info
                    let daw = Daw::new(handle.clone());
                    if let Ok(project) = daw.current_project().await {
                        project_name.set(project.guid().to_string());

                        // Get initial transport state
                        if let Ok(state) = project.transport().get_state().await {
                            let display = to_display_state(&state);
                            tempo_input.set(format!("{:.1}", display.tempo));
                            position_input.set(format!("{:.2}", display.position));
                            playrate_input.set(format!("{:.2}", display.playrate));
                            transport_state.set(display);
                        }
                    }

                    // Register the gateway as a remote action source
                    if let Some(manager_lock) = action_manager.read().as_ref() {
                        let mut manager = manager_lock.write().await;
                        let ws_url = get_websocket_url();
                        manager
                            .add_remote_host("Gateway".to_string(), ws_url, handle.clone())
                            .await;
                    }

                    conn_state.set(ConnectionState::Connected(handle));
                }
                Err(e) => {
                    conn_state.set(ConnectionState::Error(e));
                }
            }
        });
    });

    // Extract handle for button handlers
    let handle = match &*conn_state.read() {
        ConnectionState::Connected(h) => Some(h.clone()),
        _ => None,
    };

    // Helper macro to create transport action handlers
    macro_rules! transport_action {
        ($handle:expr, $transport_state:expr, $action:ident) => {{
            let handle = $handle.clone();
            let mut ts = $transport_state.clone();
            move |_| {
                if let Some(h) = handle.clone() {
                    spawn(async move {
                        let daw = Daw::new(h);
                        if let Ok(project) = daw.current_project().await {
                            let _ = project.transport().$action().await;
                            // Refresh state after action
                            if let Ok(state) = project.transport().get_state().await {
                                ts.set(to_display_state(&state));
                            }
                        }
                    });
                }
            }
        }};
    }

    // Playback control handlers
    let play = transport_action!(handle, transport_state, play);
    let pause = transport_action!(handle, transport_state, pause);
    let stop = transport_action!(handle, transport_state, stop);
    let play_pause = transport_action!(handle, transport_state, play_pause);
    let play_stop = transport_action!(handle, transport_state, play_stop);

    // Recording control handlers
    let record = transport_action!(handle, transport_state, record);
    let toggle_recording = transport_action!(handle, transport_state, toggle_recording);

    // Position control handlers
    let goto_start = transport_action!(handle, transport_state, goto_start);
    let goto_end = transport_action!(handle, transport_state, goto_end);

    // Loop control handlers
    let toggle_loop = transport_action!(handle, transport_state, toggle_loop);

    // Set tempo handler
    let set_tempo = {
        let handle = handle.clone();
        let mut ts = transport_state.clone();
        let tempo_val = tempo_input.clone();
        move |_| {
            if let Some(h) = handle.clone() {
                let tempo_str = tempo_val.read().clone();
                spawn(async move {
                    if let Ok(bpm) = tempo_str.parse::<f64>() {
                        let daw = Daw::new(h);
                        if let Ok(project) = daw.current_project().await {
                            let _ = project.transport().set_tempo(bpm).await;
                            if let Ok(state) = project.transport().get_state().await {
                                ts.set(to_display_state(&state));
                            }
                        }
                    }
                });
            }
        }
    };

    // Set position handler
    let set_position = {
        let handle = handle.clone();
        let mut ts = transport_state.clone();
        let pos_val = position_input.clone();
        move |_| {
            if let Some(h) = handle.clone() {
                let pos_str = pos_val.read().clone();
                spawn(async move {
                    if let Ok(seconds) = pos_str.parse::<f64>() {
                        let daw = Daw::new(h);
                        if let Ok(project) = daw.current_project().await {
                            let _ = project.transport().set_position(seconds).await;
                            if let Ok(state) = project.transport().get_state().await {
                                ts.set(to_display_state(&state));
                            }
                        }
                    }
                });
            }
        }
    };

    // Set playrate handler
    let set_playrate = {
        let handle = handle.clone();
        let mut ts = transport_state.clone();
        let rate_val = playrate_input.clone();
        move |_| {
            if let Some(h) = handle.clone() {
                let rate_str = rate_val.read().clone();
                spawn(async move {
                    if let Ok(rate) = rate_str.parse::<f64>() {
                        let daw = Daw::new(h);
                        if let Ok(project) = daw.current_project().await {
                            let _ = project.transport().set_playrate(rate).await;
                            if let Ok(state) = project.transport().get_state().await {
                                ts.set(to_display_state(&state));
                            }
                        }
                    }
                });
            }
        }
    };

    // Refresh state handler
    let refresh_state = {
        let handle = handle.clone();
        let mut ts = transport_state.clone();
        let mut tempo_inp = tempo_input.clone();
        let mut pos_inp = position_input.clone();
        let mut rate_inp = playrate_input.clone();
        move |_| {
            if let Some(h) = handle.clone() {
                spawn(async move {
                    let daw = Daw::new(h);
                    if let Ok(project) = daw.current_project().await {
                        if let Ok(state) = project.transport().get_state().await {
                            let display = to_display_state(&state);
                            tempo_inp.set(format!("{:.1}", display.tempo));
                            pos_inp.set(format!("{:.2}", display.position));
                            rate_inp.set(format!("{:.2}", display.playrate));
                            ts.set(display);
                        }
                    }
                });
            }
        }
    };

    let is_connected = handle.is_some();
    let ts = transport_state.read();

    // Open command palette handler
    let open_palette = move |_| {
        props.palette_open.set(true);
    };

    rsx! {
        div { class: "min-h-screen bg-gray-900 text-white p-8",
            // Header with title and command palette button
            div { class: "flex justify-between items-center mb-8",
                h1 { class: "text-3xl font-bold", "FTS Control" }
                div { class: "flex gap-4",
                    button {
                        class: "px-4 py-2 bg-blue-600 hover:bg-blue-700 rounded-lg text-sm transition-colors disabled:opacity-50",
                        disabled: !is_connected,
                        onclick: refresh_state,
                        "Refresh"
                    }
                    button {
                        class: "px-4 py-2 bg-gray-700 hover:bg-gray-600 rounded-lg text-sm flex items-center gap-2 transition-colors",
                        onclick: open_palette,
                        span { "Command Palette" }
                        span { class: "text-xs text-gray-400", "Cmd+Shift+P" }
                    }
                }
            }

            // Connection status
            div { class: "mb-6",
                match &*conn_state.read() {
                    ConnectionState::Disconnected => rsx! {
                        span { class: "text-gray-400", "Disconnected" }
                    },
                    ConnectionState::Connecting => rsx! {
                        span { class: "text-yellow-400", "Connecting..." }
                    },
                    ConnectionState::Connected(_) => rsx! {
                        span { class: "text-green-400", "Connected" }
                    },
                    ConnectionState::Error(e) => rsx! {
                        div { class: "text-red-400",
                            span { "Error: " }
                            span { "{e}" }
                        }
                    },
                }
            }

            // Project info
            div { class: "mb-6 text-gray-400",
                "Project: {project_name}"
            }

            // Status indicators
            div { class: "mb-6 flex gap-4 text-sm",
                div {
                    class: if ts.is_playing { "text-green-400" } else { "text-gray-500" },
                    if ts.is_playing { "Playing" } else { "Stopped" }
                }
                div {
                    class: if ts.is_recording { "text-red-400" } else { "text-gray-500" },
                    if ts.is_recording { "Recording" } else { "Not Recording" }
                }
                div {
                    class: if ts.is_looping { "text-blue-400" } else { "text-gray-500" },
                    if ts.is_looping { "Loop ON" } else { "Loop OFF" }
                }
            }

            // Transport Controls Section
            div { class: "bg-gray-800 rounded-lg p-6 mb-6",
                h2 { class: "text-xl font-semibold mb-4", "Transport" }

                // Playback Controls
                div { class: "mb-4",
                    h3 { class: "text-sm text-gray-400 mb-2", "Playback" }
                    div { class: "flex gap-2 flex-wrap",
                        button {
                            class: "px-4 py-2 bg-green-600 hover:bg-green-700 rounded-lg disabled:opacity-50 disabled:cursor-not-allowed transition-colors",
                            disabled: !is_connected,
                            onclick: play,
                            "Play"
                        }
                        button {
                            class: "px-4 py-2 bg-yellow-600 hover:bg-yellow-700 rounded-lg disabled:opacity-50 disabled:cursor-not-allowed transition-colors",
                            disabled: !is_connected,
                            onclick: pause,
                            "Pause"
                        }
                        button {
                            class: "px-4 py-2 bg-red-600 hover:bg-red-700 rounded-lg disabled:opacity-50 disabled:cursor-not-allowed transition-colors",
                            disabled: !is_connected,
                            onclick: stop,
                            "Stop"
                        }
                        button {
                            class: "px-4 py-2 bg-gray-600 hover:bg-gray-500 rounded-lg disabled:opacity-50 disabled:cursor-not-allowed transition-colors",
                            disabled: !is_connected,
                            onclick: play_pause,
                            "Play/Pause"
                        }
                        button {
                            class: "px-4 py-2 bg-gray-600 hover:bg-gray-500 rounded-lg disabled:opacity-50 disabled:cursor-not-allowed transition-colors",
                            disabled: !is_connected,
                            onclick: play_stop,
                            "Play/Stop"
                        }
                    }
                }

                // Recording Controls
                div { class: "mb-4",
                    h3 { class: "text-sm text-gray-400 mb-2", "Recording" }
                    div { class: "flex gap-2",
                        button {
                            class: "px-4 py-2 bg-red-700 hover:bg-red-800 rounded-lg disabled:opacity-50 disabled:cursor-not-allowed transition-colors",
                            disabled: !is_connected,
                            onclick: record,
                            "Record"
                        }
                        button {
                            class: "px-4 py-2 bg-gray-600 hover:bg-gray-500 rounded-lg disabled:opacity-50 disabled:cursor-not-allowed transition-colors",
                            disabled: !is_connected,
                            onclick: toggle_recording,
                            "Toggle Recording"
                        }
                    }
                }

                // Position Controls
                div { class: "mb-4",
                    h3 { class: "text-sm text-gray-400 mb-2", "Position" }
                    div { class: "flex gap-2 items-center flex-wrap",
                        button {
                            class: "px-4 py-2 bg-gray-600 hover:bg-gray-500 rounded-lg disabled:opacity-50 disabled:cursor-not-allowed transition-colors",
                            disabled: !is_connected,
                            onclick: goto_start,
                            "Go to Start"
                        }
                        button {
                            class: "px-4 py-2 bg-gray-600 hover:bg-gray-500 rounded-lg disabled:opacity-50 disabled:cursor-not-allowed transition-colors",
                            disabled: !is_connected,
                            onclick: goto_end,
                            "Go to End"
                        }
                        div { class: "flex items-center gap-2",
                            input {
                                r#type: "text",
                                class: "w-24 px-2 py-2 bg-gray-700 rounded text-white",
                                value: "{position_input}",
                                oninput: move |e| position_input.set(e.value()),
                            }
                            span { class: "text-gray-400", "sec" }
                            button {
                                class: "px-4 py-2 bg-blue-600 hover:bg-blue-700 rounded-lg disabled:opacity-50 disabled:cursor-not-allowed transition-colors",
                                disabled: !is_connected,
                                onclick: set_position,
                                "Set Position"
                            }
                        }
                        span { class: "text-gray-400 text-sm", "Current: {ts.position:.2}s" }
                    }
                }

                // Loop Control
                div { class: "mb-4",
                    h3 { class: "text-sm text-gray-400 mb-2", "Loop" }
                    div { class: "flex gap-2",
                        button {
                            class: "px-4 py-2 bg-blue-600 hover:bg-blue-700 rounded-lg disabled:opacity-50 disabled:cursor-not-allowed transition-colors",
                            disabled: !is_connected,
                            onclick: toggle_loop,
                            "Toggle Loop"
                        }
                    }
                }
            }

            // Tempo & Playrate Section
            div { class: "bg-gray-800 rounded-lg p-6 mb-6",
                h2 { class: "text-xl font-semibold mb-4", "Tempo & Playrate" }

                // Tempo Control
                div { class: "mb-4",
                    h3 { class: "text-sm text-gray-400 mb-2", "Tempo" }
                    div { class: "flex gap-2 items-center",
                        input {
                            r#type: "text",
                            class: "w-24 px-2 py-2 bg-gray-700 rounded text-white",
                            value: "{tempo_input}",
                            oninput: move |e| tempo_input.set(e.value()),
                        }
                        span { class: "text-gray-400", "BPM" }
                        button {
                            class: "px-4 py-2 bg-purple-600 hover:bg-purple-700 rounded-lg disabled:opacity-50 disabled:cursor-not-allowed transition-colors",
                            disabled: !is_connected,
                            onclick: set_tempo,
                            "Set Tempo"
                        }
                        span { class: "text-gray-400 text-sm", "Current: {ts.tempo:.1} BPM" }
                    }
                }

                // Playrate Control
                div {
                    h3 { class: "text-sm text-gray-400 mb-2", "Playrate (0.25 - 4.0)" }
                    div { class: "flex gap-2 items-center",
                        input {
                            r#type: "text",
                            class: "w-24 px-2 py-2 bg-gray-700 rounded text-white",
                            value: "{playrate_input}",
                            oninput: move |e| playrate_input.set(e.value()),
                        }
                        span { class: "text-gray-400", "x" }
                        button {
                            class: "px-4 py-2 bg-purple-600 hover:bg-purple-700 rounded-lg disabled:opacity-50 disabled:cursor-not-allowed transition-colors",
                            disabled: !is_connected,
                            onclick: set_playrate,
                            "Set Playrate"
                        }
                        span { class: "text-gray-400 text-sm", "Current: {ts.playrate:.2}x" }
                    }
                }
            }
        }
    }
}
