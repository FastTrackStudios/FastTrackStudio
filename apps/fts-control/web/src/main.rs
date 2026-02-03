//! FTS Control Web App
//!
//! Dioxus-based web app for controlling DAW sessions via the gateway-ws server.
//!
//! Features:
//! - Connection to gateway-ws for real DAW control
//! - Setlist navigation and progress visualization
//! - Transport controls (play/pause/stop)
//! - All state is fetched from the connected server (no mock data)

#![cfg_attr(not(target_arch = "wasm32"), allow(unused))]

#[cfg(target_arch = "wasm32")]
mod actions;
#[cfg(target_arch = "wasm32")]
mod connection;

#[cfg(target_arch = "wasm32")]
use dioxus::prelude::*;

#[cfg(target_arch = "wasm32")]
const FAVICON: Asset = asset!("/assets/favicon.ico");
#[cfg(target_arch = "wasm32")]
const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");

fn main() {
    #[cfg(target_arch = "wasm32")]
    {
        // Initialize tracing for browser console (INFO level to reduce noise)
        tracing_wasm::set_as_global_default_with_config(
            tracing_wasm::WASMLayerConfigBuilder::default()
                .set_max_level(tracing::Level::INFO)
                .build(),
        );
        dioxus::launch(App);
    }

    #[cfg(not(target_arch = "wasm32"))]
    {
        eprintln!("This binary only runs on wasm32. Use `dx build` to compile for WASM.");
    }
}

#[cfg(target_arch = "wasm32")]
#[component]
fn App() -> Element {
    use session_ui::{PerformanceLayout, TopBar};

    // Connection state management
    let (connection_state, connect) = connection::use_connection();

    // Active tab state
    let mut active_tab = use_signal(|| "performance".to_string());

    // Auto-connect on mount
    use_effect(move || {
        connect.call(());
    });

    rsx! {
        document::Link { rel: "icon", href: FAVICON }
        document::Link { rel: "stylesheet", href: TAILWIND_CSS }

        div {
            class: "flex flex-col h-screen w-full bg-background text-foreground",

            // Top bar with connection status
            TopBar {
                connection_state: connection_state(),
                active_tab: active_tab(),
                on_tab_click: Some(Callback::new(move |tab: String| {
                    active_tab.set(tab);
                })),
            }

            // Main content area
            div {
                class: "flex-1 overflow-hidden",
                match active_tab().as_str() {
                    "performance" => rsx! {
                        PerformanceLayout {}
                    },
                    "setlist" => rsx! {
                        SetlistView {}
                    },
                    "settings" => rsx! {
                        SettingsView { connection_state: connection_state }
                    },
                    _ => rsx! {
                        PerformanceLayout {}
                    },
                }
            }
        }
    }
}

/// Setlist management view (placeholder)
#[cfg(target_arch = "wasm32")]
#[component]
fn SetlistView() -> Element {
    rsx! {
        div {
            class: "flex items-center justify-center h-full",
            div {
                class: "text-center text-muted-foreground",
                h2 {
                    class: "text-2xl font-semibold mb-2",
                    "Setlist Management"
                }
                p { "Coming soon..." }
            }
        }
    }
}

/// Settings view with connection controls
#[cfg(target_arch = "wasm32")]
#[component]
fn SettingsView(connection_state: Signal<session_ui::ConnectionState>) -> Element {
    use session_ui::ConnectionState;

    let ws_url = connection::get_ws_url();

    rsx! {
        div {
            class: "p-8 max-w-2xl mx-auto",

            h2 {
                class: "text-2xl font-semibold mb-6",
                "Settings"
            }

            // Connection section
            div {
                class: "bg-card rounded-lg p-6 border border-border",

                h3 {
                    class: "text-lg font-medium mb-4",
                    "Connection"
                }

                div {
                    class: "space-y-4",

                    // WebSocket URL
                    div {
                        label {
                            class: "block text-sm font-medium text-muted-foreground mb-1",
                            "Gateway URL"
                        }
                        div {
                            class: "text-sm font-mono bg-secondary px-3 py-2 rounded",
                            "{ws_url}"
                        }
                        p {
                            class: "text-xs text-muted-foreground mt-1",
                            "Override with ?ws=ws://your-host:port/ws in the URL"
                        }
                    }

                    // Connection status
                    div {
                        label {
                            class: "block text-sm font-medium text-muted-foreground mb-1",
                            "Status"
                        }
                        div {
                            class: "flex items-center gap-2",
                            match connection_state() {
                                ConnectionState::Connected => rsx! {
                                    div { class: "w-3 h-3 rounded-full bg-green-500" }
                                    span { class: "text-green-500", "Connected" }
                                },
                                ConnectionState::Connecting => rsx! {
                                    div { class: "w-3 h-3 rounded-full bg-yellow-500 animate-pulse" }
                                    span { class: "text-yellow-500", "Connecting..." }
                                },
                                ConnectionState::Disconnected => rsx! {
                                    div { class: "w-3 h-3 rounded-full bg-red-500" }
                                    span { class: "text-red-500", "Disconnected" }
                                },
                            }
                        }
                    }

                    // Reconnect button
                    if matches!(connection_state(), ConnectionState::Disconnected) {
                        button {
                            class: "px-4 py-2 bg-primary text-primary-foreground rounded-md hover:bg-primary/90 transition-colors",
                            onclick: move |_| {
                                let (_, connect) = connection::use_connection();
                                connect.call(());
                            },
                            "Reconnect"
                        }
                    }
                }
            }
        }
    }
}
