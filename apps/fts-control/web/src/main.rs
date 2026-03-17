//! FTS Control Web App
//!
//! Dioxus-based web app for controlling DAW sessions via the gateway-ws server.
//! Shows the performance view with a navigator sidebar and main/chart toggle.

#![cfg_attr(not(target_arch = "wasm32"), allow(unused))]

#[cfg(target_arch = "wasm32")]
mod actions;
#[cfg(target_arch = "wasm32")]
mod chart_canvas;
#[cfg(target_arch = "wasm32")]
mod connection;
#[cfg(target_arch = "wasm32")]
mod web_client_handler;

#[cfg(target_arch = "wasm32")]
use dioxus::prelude::*;

#[cfg(target_arch = "wasm32")]
const FAVICON: Asset = asset!("/assets/favicon.ico");
#[cfg(target_arch = "wasm32")]
const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");

fn main() {
    #[cfg(target_arch = "wasm32")]
    {
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
    use session_ui::{ConnectionState, PerformanceLayout, PerformanceSidebar, TransportPanel};

    // Connection state management
    let (connection_state, connect) = connection::use_connection();

    // View toggle: "main" or "chart"
    let mut active_view = use_signal(|| "main".to_string());

    // Auto-connect on mount
    use_effect(move || {
        connect.call(());
    });

    rsx! {
        document::Link { rel: "icon", href: FAVICON }
        document::Link { rel: "stylesheet", href: TAILWIND_CSS }

        div {
            class: "flex flex-col h-screen w-full bg-background text-foreground",

            // Compact top bar with connection status and view toggle
            div {
                class: "flex items-center justify-between px-4 py-2 border-b border-border bg-card",

                // Left: connection indicator
                div {
                    class: "flex items-center gap-2",
                    match connection_state() {
                        ConnectionState::Connected => rsx! {
                            div { class: "w-2 h-2 rounded-full bg-green-500" }
                            span { class: "text-xs text-muted-foreground", "Connected" }
                        },
                        ConnectionState::Connecting => rsx! {
                            div { class: "w-2 h-2 rounded-full bg-yellow-500 animate-pulse" }
                            span { class: "text-xs text-muted-foreground", "Connecting..." }
                        },
                        ConnectionState::Disconnected => rsx! {
                            div { class: "w-2 h-2 rounded-full bg-red-500" }
                            span { class: "text-xs text-muted-foreground", "Disconnected" }
                        },
                    }
                }

                // Right: view toggle
                div {
                    class: "flex gap-1 bg-secondary rounded-lg p-1",
                    button {
                        class: if active_view() == "main" {
                            "px-3 py-1 text-sm rounded-md bg-primary text-primary-foreground"
                        } else {
                            "px-3 py-1 text-sm rounded-md text-muted-foreground hover:text-foreground"
                        },
                        onclick: move |_| active_view.set("main".to_string()),
                        "Performance"
                    }
                    button {
                        class: if active_view() == "chart" {
                            "px-3 py-1 text-sm rounded-md bg-primary text-primary-foreground"
                        } else {
                            "px-3 py-1 text-sm rounded-md text-muted-foreground hover:text-foreground"
                        },
                        onclick: move |_| active_view.set("chart".to_string()),
                        "Chart"
                    }
                }
            }

            if matches!(connection_state(), ConnectionState::Connected) {
                // Main area: sidebar + content (only when connected)
                div {
                    class: "flex flex-1 overflow-hidden",

                    // Left sidebar: navigator
                    div {
                        class: "w-64 flex-shrink-0 border-r border-border overflow-y-auto",
                        PerformanceSidebar {}
                    }

                    // Content area: performance view or chart + transport below
                    div {
                        class: "flex flex-col flex-1 overflow-hidden",

                        // Main content
                        div {
                            class: "flex-1 overflow-hidden",
                            match active_view().as_str() {
                                "chart" => rsx! {
                                    chart_canvas::ChartCanvas {}
                                },
                                _ => rsx! {
                                    PerformanceLayout {}
                                },
                            }
                        }

                        // Transport controls at bottom of content area
                        div {
                            class: "border-t border-border h-20",
                            TransportPanel {}
                        }
                    }
                }
            } else {
                // Waiting for connection
                div {
                    class: "flex-1 flex items-center justify-center",
                    div {
                        class: "text-center text-muted-foreground",
                        p { class: "text-lg", "Connecting to FTS Control..." }
                        p { class: "text-sm mt-2", "Waiting for desktop app" }
                    }
                }
            }
        }
    }
}
