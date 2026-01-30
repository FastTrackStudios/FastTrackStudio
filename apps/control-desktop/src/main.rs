//! FTS Control Desktop
//!
//! A Dioxus desktop app that:
//! 1. Shows a local desktop UI for DAW control
//! 2. Runs an embedded HTTP server to serve control-web to phones/tablets
//! 3. Connects to REAPER via Unix socket (roam-stream) for DAW state
//!
//! Run with: dx serve (from apps/control-desktop)

mod roam_client;
mod server;
mod shared_state;

use dioxus::prelude::*;
use host_proto::PlaybackState;
use roam_client::{ConnectionStatus, ReaperInstance, RoamClient};
use shared_state::{get_shared_state, init_shared_state};
use std::sync::Arc;

fn main() {
    // Initialize logging
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive("control_desktop=debug".parse().unwrap())
                .add_directive("tower_http=debug".parse().unwrap())
                .add_directive("warn".parse().unwrap()),
        )
        .init();

    tracing::info!("Starting FTS Control Desktop");

    // Initialize shared state (creates RoamClient)
    let shared = init_shared_state();
    tracing::info!("Server will listen on port {}", shared.server_port);

    // Start the HTTP server in a background thread
    std::thread::spawn(|| {
        let rt = tokio::runtime::Runtime::new().unwrap();
        rt.block_on(async {
            if let Err(e) = server::run_server().await {
                tracing::error!("HTTP server error: {}", e);
            }
        });
    });

    // Launch the Dioxus desktop app
    dioxus::launch(App);
}

/// App state derived from shared state
#[derive(Clone, PartialEq)]
struct AppState {
    server_port: u16,
    roam_client: Arc<RoamClient>,
}

impl Default for AppState {
    fn default() -> Self {
        let shared = get_shared_state();
        Self {
            server_port: shared.server_port,
            roam_client: shared.roam_client.clone(),
        }
    }
}

#[component]
fn App() -> Element {
    let state = use_hook(AppState::default);
    let local_ip = use_hook(|| get_local_ip().unwrap_or_else(|| "localhost".to_string()));
    let mut available_instances = use_signal(Vec::<ReaperInstance>::new);
    let mut connection_status = use_signal(|| ConnectionStatus::Disconnected);
    let mut transport_state = use_signal(|| None::<PlaybackState>);

    // Scan for REAPER instances and auto-connect if only one found
    let state_for_scan = state.clone();
    use_effect(move || {
        let state = state_for_scan.clone();
        spawn(async move {
            let mut was_connected = false;

            loop {
                // Scan for instances
                let instances = roam_client::scan_reaper_instances();
                available_instances.set(instances.clone());

                // Update connection status
                let status = state.roam_client.status().await;
                connection_status.set(status.clone());

                let is_connected = matches!(status, ConnectionStatus::Connected(_));

                // Auto-connect if exactly one instance found and not connected
                if matches!(status, ConnectionStatus::Disconnected) && instances.len() == 1 {
                    tracing::info!("Auto-connecting to single REAPER instance");
                    let _ = state.roam_client.connect(instances[0].clone()).await;
                }

                // When newly connected, subscribe to transport stream
                if is_connected && !was_connected {
                    tracing::info!("Subscribing to transport stream");
                    if let Ok(mut rx) = state.roam_client.subscribe_transport().await {
                        // Spawn a task to receive transport updates
                        spawn(async move {
                            while let Ok(Some(ts)) = rx.recv().await {
                                transport_state.set(Some(ts));
                            }
                            tracing::info!("Transport stream ended");
                            transport_state.set(None);
                        });
                    }
                }

                // Clear transport state when disconnected
                if !is_connected && was_connected {
                    transport_state.set(None);
                }

                was_connected = is_connected;

                // Wait before next scan (1 second for instance discovery)
                tokio::time::sleep(std::time::Duration::from_secs(1)).await;
            }
        });
    });

    rsx! {
        style { {include_str!("styles.css")} }

        div { class: "app",
            // Header
            header { class: "header",
                h1 { "FTS Control" }
                p { class: "subtitle", "Desktop Controller" }
            }

            // Main content
            main { class: "content",
                // REAPER connection card
                ReaperConnectionCard {
                    state: state.clone(),
                    instances: available_instances(),
                    status: connection_status(),
                }

                // Transport controls (only shown when connected)
                if matches!(connection_status(), ConnectionStatus::Connected(_)) {
                    TransportControls {
                        state: state.clone(),
                        transport: transport_state(),
                    }
                }

                // Server status card
                div { class: "card",
                    h2 { "Web Server" }
                    div { class: "status-row",
                        span { class: "status-dot running" }
                        span { "Running on port {state.server_port}" }
                    }

                    div { class: "urls",
                        p { class: "url-label", "Access from this device:" }
                        a {
                            href: "http://localhost:{state.server_port}",
                            class: "url",
                            "http://localhost:{state.server_port}"
                        }

                        p { class: "url-label", "Access from other devices:" }
                        a {
                            href: "http://{local_ip}:{state.server_port}",
                            class: "url",
                            "http://{local_ip}:{state.server_port}"
                        }
                    }

                    div { class: "actions",
                        button {
                            class: "action-btn",
                            onclick: move |_| {
                                let url = format!("http://localhost:{}", state.server_port);
                                let _ = open::that(&url);
                            },
                            "Open Web UI"
                        }
                    }
                }
            }

            // Footer
            footer { class: "footer",
                p { "FTS Control v0.1.0" }
            }
        }
    }
}

#[component]
fn ReaperConnectionCard(
    state: AppState,
    instances: Vec<ReaperInstance>,
    status: ConnectionStatus,
) -> Element {
    let (status_class, status_text) = match &status {
        ConnectionStatus::Disconnected => ("disconnected", "Not connected".to_string()),
        ConnectionStatus::Connecting => ("connecting", "Connecting...".to_string()),
        ConnectionStatus::Connected(inst) => ("running", format!("Connected to {}", inst.display_name())),
        ConnectionStatus::Error(e) => ("error", format!("Error: {}", e)),
    };

    rsx! {
        div { class: "card",
            h2 { "REAPER Connection" }
            div { class: "status-row",
                span { class: "status-dot {status_class}" }
                span { "{status_text}" }
            }

            // Show available instances if not connected
            if !matches!(status, ConnectionStatus::Connected(_)) {
                if instances.is_empty() {
                    p { class: "hint",
                        "No REAPER instances found. Start REAPER with the socket-gateway extension."
                    }
                } else {
                    div { class: "instance-list",
                        p { class: "url-label", "Available instances:" }
                        for instance in instances {
                            {
                                let inst = instance.clone();
                                let state = state.clone();
                                rsx! {
                                    button {
                                        class: "action-btn instance-btn",
                                        onclick: move |_| {
                                            let state = state.clone();
                                            let inst = inst.clone();
                                            spawn(async move {
                                                let _ = state.roam_client.connect(inst).await;
                                            });
                                        },
                                        "{instance.display_name()}"
                                    }
                                }
                            }
                        }
                    }
                }
            } else {
                // Show disconnect button when connected
                div { class: "actions",
                    button {
                        class: "action-btn disconnect-btn",
                        onclick: move |_| {
                            let state = state.clone();
                            spawn(async move {
                                state.roam_client.disconnect().await;
                            });
                        },
                        "Disconnect"
                    }
                }
            }
        }
    }
}

#[component]
fn TransportControls(state: AppState, transport: Option<PlaybackState>) -> Element {
    let (is_playing, is_recording, position, tempo) = transport
        .as_ref()
        .map(|t| (t.is_playing, t.is_recording, t.position_seconds, t.tempo_bpm))
        .unwrap_or((false, false, 0.0, 120.0));

    // Format position as MM:SS.ms
    let minutes = (position / 60.0) as u32;
    let seconds = position % 60.0;
    let position_str = format!("{:02}:{:05.2}", minutes, seconds);

    rsx! {
        div { class: "card transport-card",
            h2 { "Transport" }

            // Position display
            div { class: "transport-position",
                span { class: "position-time", "{position_str}" }
                span { class: "tempo", "{tempo:.1} BPM" }
            }

            // Transport status
            div { class: "transport-status",
                if is_recording {
                    span { class: "status-badge recording", "REC" }
                } else if is_playing {
                    span { class: "status-badge playing", "PLAY" }
                } else {
                    span { class: "status-badge stopped", "STOP" }
                }
            }

            // Transport buttons
            div { class: "transport-buttons",
                button {
                    class: "transport-btn stop-btn",
                    onclick: {
                        let state = state.clone();
                        move |_| {
                            let state = state.clone();
                            spawn(async move {
                                let _ = state.roam_client.stop().await;
                            });
                        }
                    },
                    "Stop"
                }
                button {
                    class: "transport-btn play-btn",
                    class: if is_playing && !is_recording { "active" } else { "" },
                    onclick: {
                        let state = state.clone();
                        move |_| {
                            let state = state.clone();
                            spawn(async move {
                                let _ = state.roam_client.play().await;
                            });
                        }
                    },
                    "Play"
                }
                button {
                    class: "transport-btn record-btn",
                    class: if is_recording { "active" } else { "" },
                    onclick: {
                        let state = state.clone();
                        move |_| {
                            let state = state.clone();
                            spawn(async move {
                                let _ = state.roam_client.record().await;
                            });
                        }
                    },
                    "Record"
                }
            }
        }
    }
}

/// Get the local IP address for LAN access
fn get_local_ip() -> Option<String> {
    // Try to get a non-loopback IPv4 address
    if let Ok(interfaces) = local_ip_address::list_afinet_netifas() {
        for (_, ip) in interfaces {
            if let std::net::IpAddr::V4(v4) = ip {
                if !v4.is_loopback() && !v4.is_link_local() {
                    return Some(v4.to_string());
                }
            }
        }
    }
    None
}
