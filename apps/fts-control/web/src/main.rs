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

use daw_control::Daw;
use dioxus::prelude::*;
use roam::session::ConnectionHandle;
use roam_session::{initiate_framed, HandshakeConfig, NoDispatcher};
use roam_websocket::WsTransport;

const FAVICON: Asset = asset!("/assets/favicon.ico");
const MAIN_CSS: Asset = asset!("/assets/main.css");
const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");

fn main() {
    // Initialize tracing for browser console
    tracing_wasm::set_as_global_default();

    dioxus::launch(App);
}

/// Determine the WebSocket URL based on current page location
fn get_websocket_url() -> String {
    web_sys::window()
        .and_then(|w| w.location().host().ok())
        .map(|host| {
            // Use wss:// if page is https://, otherwise ws://
            let protocol = web_sys::window()
                .and_then(|w| w.location().protocol().ok())
                .map(|p| if p == "https:" { "wss" } else { "ws" })
                .unwrap_or("ws");
            format!("{protocol}://{host}/ws")
        })
        .unwrap_or_else(|| "ws://localhost:3030/ws".to_string())
}

/// Connect to the gateway WebSocket and return a connection handle
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

#[component]
fn App() -> Element {
    rsx! {
        document::Link { rel: "icon", href: FAVICON }
        document::Link { rel: "stylesheet", href: MAIN_CSS }
        document::Link { rel: "stylesheet", href: TAILWIND_CSS }

        DawController {}
    }
}

/// Connection state shared across components
#[derive(Clone)]
enum ConnectionState {
    Disconnected,
    Connecting,
    Connected(ConnectionHandle),
    Error(String),
}

#[component]
fn DawController() -> Element {
    // Connection state
    let mut conn_state = use_signal(|| ConnectionState::Disconnected);
    let mut is_playing = use_signal(|| false);
    let mut project_name = use_signal(|| String::from("No project"));

    // Connect on mount
    use_effect(move || {
        spawn(async move {
            conn_state.set(ConnectionState::Connecting);

            match connect_to_gateway().await {
                Ok(handle) => {
                    // Try to get current project info
                    let daw = Daw::new(handle.clone());
                    if let Ok(project) = daw.current_project().await {
                        project_name.set(project.guid().to_string());
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

    // Transport control handlers
    let play = {
        let handle = handle.clone();
        move |_| {
            if let Some(h) = handle.clone() {
                spawn(async move {
                    let daw = Daw::new(h);
                    if let Ok(project) = daw.current_project().await {
                        if project.transport().play().await.is_ok() {
                            is_playing.set(true);
                        }
                    }
                });
            }
        }
    };

    let stop = {
        let handle = handle.clone();
        move |_| {
            if let Some(h) = handle.clone() {
                spawn(async move {
                    let daw = Daw::new(h);
                    if let Ok(project) = daw.current_project().await {
                        if project.transport().stop().await.is_ok() {
                            is_playing.set(false);
                        }
                    }
                });
            }
        }
    };

    let is_connected = handle.is_some();

    rsx! {
        div { class: "min-h-screen bg-gray-900 text-white p-8",
            h1 { class: "text-3xl font-bold mb-8", "FTS Control" }

            // Connection status
            div { class: "mb-8",
                match &*conn_state.read() {
                    ConnectionState::Disconnected => rsx! {
                        span { class: "text-gray-400", "○ Disconnected" }
                    },
                    ConnectionState::Connecting => rsx! {
                        span { class: "text-yellow-400", "◌ Connecting..." }
                    },
                    ConnectionState::Connected(_) => rsx! {
                        span { class: "text-green-400", "● Connected" }
                    },
                    ConnectionState::Error(e) => rsx! {
                        div { class: "text-red-400",
                            span { "✕ Error: " }
                            span { "{e}" }
                        }
                    },
                }
            }

            // Project info
            div { class: "mb-8 text-gray-400",
                "Project: {project_name}"
            }

            // Transport controls
            div { class: "flex gap-4",
                button {
                    class: "px-6 py-3 bg-green-600 hover:bg-green-700 rounded-lg text-xl disabled:opacity-50 disabled:cursor-not-allowed transition-colors",
                    disabled: !is_connected,
                    onclick: play,
                    "Play"
                }
                button {
                    class: "px-6 py-3 bg-red-600 hover:bg-red-700 rounded-lg text-xl disabled:opacity-50 disabled:cursor-not-allowed transition-colors",
                    disabled: !is_connected,
                    onclick: stop,
                    "Stop"
                }
            }

            // Playing indicator
            if is_playing() {
                div { class: "mt-8 text-green-400 text-xl animate-pulse",
                    "Playing..."
                }
            }
        }
    }
}
