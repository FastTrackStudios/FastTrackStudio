//! FTS Control - Remote DAW Control Surface
//!
//! A Dioxus web app for controlling REAPER via the ROAM HTTP bridge.
//! Designed for use on phones, tablets, and laptops on the local network.
//!
//! Architecture:
//! ```text
//! ┌──────────────────────┐     HTTP (port 3000)     ┌─────────────────────────┐
//! │  Phone/Tablet/Laptop │◄────────────────────────►│  HTTP Gateway Extension │
//! │  on Local Network    │     Static + WebSocket   │  (SHM Guest Process)    │
//! └──────────────────────┘                          └───────────┬─────────────┘
//!                                                               │ ROAM SHM
//!                                                   ┌───────────▼─────────────┐
//!                                                   │  REAPER Extension Host  │
//!                                                   │  (HostService, etc.)    │
//!                                                   └─────────────────────────┘
//! ```

mod components;
mod ws;

use components::TransportControls;
use dioxus::prelude::*;
use ws::{ConnectionStatus, ConnectionStatusIndicator, RoamProvider, use_roam_websocket};

// Static assets
const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");

/// Application routes
#[derive(Debug, Clone, Routable, PartialEq)]
#[rustfmt::skip]
pub enum Route {
    #[layout(Layout)]
    #[route("/")]
    Home {},
    #[route("/transport")]
    Transport {},
    #[route("/settings")]
    SettingsPage {},
}

fn main() {
    // Initialize panic hook and tracing for WASM
    #[cfg(target_arch = "wasm32")]
    {
        use tracing_subscriber::layer::SubscriberExt;
        use tracing_subscriber::util::SubscriberInitExt;

        console_error_panic_hook::set_once();

        let filter = tracing_subscriber::EnvFilter::new("warn,control=debug");

        tracing_subscriber::registry()
            .with(filter)
            .with(tracing_wasm::WASMLayer::new(
                tracing_wasm::WASMLayerConfig::default(),
            ))
            .init();
    }

    tracing::info!("Starting FTS Control");

    dioxus::launch(App);
}

// Inline SVG icon components
#[component]
fn IconMusic(class: String) -> Element {
    rsx! {
        svg {
            class: "{class}",
            fill: "none",
            stroke: "currentColor",
            stroke_width: "2",
            stroke_linecap: "round",
            stroke_linejoin: "round",
            view_box: "0 0 24 24",
            circle { cx: "8", cy: "18", r: "4" }
            path { d: "M12 18V2l7 4" }
        }
    }
}

#[component]
fn IconSettings(class: String) -> Element {
    rsx! {
        svg {
            class: "{class}",
            fill: "none",
            stroke: "currentColor",
            stroke_width: "2",
            stroke_linecap: "round",
            stroke_linejoin: "round",
            view_box: "0 0 24 24",
            path { d: "M12.22 2h-.44a2 2 0 0 0-2 2v.18a2 2 0 0 1-1 1.73l-.43.25a2 2 0 0 1-2 0l-.15-.08a2 2 0 0 0-2.73.73l-.22.38a2 2 0 0 0 .73 2.73l.15.1a2 2 0 0 1 1 1.72v.51a2 2 0 0 1-1 1.74l-.15.09a2 2 0 0 0-.73 2.73l.22.38a2 2 0 0 0 2.73.73l.15-.08a2 2 0 0 1 2 0l.43.25a2 2 0 0 1 1 1.73V20a2 2 0 0 0 2 2h.44a2 2 0 0 0 2-2v-.18a2 2 0 0 1 1-1.73l.43-.25a2 2 0 0 1 2 0l.15.08a2 2 0 0 0 2.73-.73l.22-.39a2 2 0 0 0-.73-2.73l-.15-.08a2 2 0 0 1-1-1.74v-.5a2 2 0 0 1 1-1.74l.15-.09a2 2 0 0 0 .73-2.73l-.22-.38a2 2 0 0 0-2.73-.73l-.15.08a2 2 0 0 1-2 0l-.43-.25a2 2 0 0 1-1-1.73V4a2 2 0 0 0-2-2z" }
            circle { cx: "12", cy: "12", r: "3" }
        }
    }
}

#[component]
fn IconWifi(class: String) -> Element {
    rsx! {
        svg {
            class: "{class}",
            fill: "none",
            stroke: "currentColor",
            stroke_width: "2",
            stroke_linecap: "round",
            stroke_linejoin: "round",
            view_box: "0 0 24 24",
            path { d: "M5 12.55a11 11 0 0 1 14.08 0" }
            path { d: "M1.42 9a16 16 0 0 1 21.16 0" }
            path { d: "M8.53 16.11a6 6 0 0 1 6.95 0" }
            line { x1: "12", y1: "20", x2: "12.01", y2: "20" }
        }
    }
}

#[component]
fn IconWifiOff(class: String) -> Element {
    rsx! {
        svg {
            class: "{class}",
            fill: "none",
            stroke: "currentColor",
            stroke_width: "2",
            stroke_linecap: "round",
            stroke_linejoin: "round",
            view_box: "0 0 24 24",
            line { x1: "1", y1: "1", x2: "23", y2: "23" }
            path { d: "M16.72 11.06A10.94 10.94 0 0 1 19 12.55" }
            path { d: "M5 12.55a10.94 10.94 0 0 1 5.17-2.39" }
            path { d: "M10.71 5.05A16 16 0 0 1 22.58 9" }
            path { d: "M1.42 9a15.91 15.91 0 0 1 4.7-2.88" }
            path { d: "M8.53 16.11a6 6 0 0 1 6.95 0" }
            line { x1: "12", y1: "20", x2: "12.01", y2: "20" }
        }
    }
}

#[component]
fn IconPlay(class: String) -> Element {
    rsx! {
        svg {
            class: "{class}",
            fill: "none",
            stroke: "currentColor",
            stroke_width: "2",
            stroke_linecap: "round",
            stroke_linejoin: "round",
            view_box: "0 0 24 24",
            polygon { points: "5,3 19,12 5,21" }
        }
    }
}

/// Root application component
#[component]
fn App() -> Element {
    rsx! {
        document::Link { rel: "stylesheet", href: TAILWIND_CSS }
        // Set viewport for mobile responsiveness
        document::Meta {
            name: "viewport",
            content: "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no"
        }
        // Theme color for mobile browsers
        document::Meta {
            name: "theme-color",
            content: "#0a0a0a"
        }
        RoamProvider {
            Router::<Route> {}
        }
    }
}

/// Main layout with navigation
#[component]
fn Layout() -> Element {
    let ws = use_roam_websocket();
    let route = use_route::<Route>();

    // Navigation items
    let nav_items = [
        (Route::Home {}, "Home", "home"),
        (Route::Transport {}, "Transport", "transport"),
        (Route::SettingsPage {}, "Settings", "settings"),
    ];

    rsx! {
        div { class: "min-h-screen bg-background text-foreground flex flex-col",
            // Header
            header { class: "sticky top-0 z-50 bg-background/80 backdrop-blur-xl border-b border-border",
                div { class: "flex items-center justify-between px-4 py-3",
                    // Logo
                    div { class: "flex items-center gap-2",
                        div { class: "w-8 h-8 rounded-lg bg-gradient-to-br from-primary to-primary/60 flex items-center justify-center",
                            IconMusic { class: "w-4 h-4 text-primary-foreground".to_string() }
                        }
                        span { class: "font-semibold", "FTS Control" }
                    }

                    // Connection status
                    ConnectionStatusIndicator {}
                }
            }

            // Main content
            main { class: "flex-1 overflow-auto",
                Outlet::<Route> {}
            }

            // Bottom navigation (mobile-style)
            nav { class: "sticky bottom-0 bg-background/80 backdrop-blur-xl border-t border-border safe-area-inset-bottom",
                div { class: "flex items-center justify-around py-2",
                    for (target, label, _) in nav_items.iter() {
                        Link {
                            to: target.clone(),
                            class: if route == *target {
                                "flex flex-col items-center gap-1 px-4 py-2 rounded-lg transition-colors text-primary"
                            } else {
                                "flex flex-col items-center gap-1 px-4 py-2 rounded-lg transition-colors text-muted-foreground"
                            },

                            // Icon based on route
                            match label {
                                &"Home" => rsx! { IconMusic { class: "w-5 h-5".to_string() } },
                                &"Transport" => rsx! { IconPlay { class: "w-5 h-5".to_string() } },
                                &"Settings" => rsx! { IconSettings { class: "w-5 h-5".to_string() } },
                                _ => rsx! {},
                            }

                            span { class: "text-xs", "{label}" }
                        }
                    }
                }
            }
        }
    }
}

/// Home page - quick overview and shortcuts
#[component]
fn Home() -> Element {
    let ws = use_roam_websocket();

    let status_message = match ws.status() {
        ConnectionStatus::Disconnected => "Not connected to DAW",
        ConnectionStatus::Connecting => "Connecting to DAW...",
        ConnectionStatus::Connected => "Connected to REAPER",
        ConnectionStatus::Error => "Connection error",
    };

    rsx! {
        div { class: "flex flex-col items-center justify-center min-h-[60vh] p-6 gap-8",
            // Connection status
            div { class: "flex flex-col items-center gap-4",
                match ws.status() {
                    ConnectionStatus::Connected => rsx! { IconWifi { class: "w-12 h-12 text-green-500".to_string() } },
                    ConnectionStatus::Connecting => rsx! { IconWifi { class: "w-12 h-12 text-yellow-500 animate-pulse".to_string() } },
                    _ => rsx! { IconWifiOff { class: "w-12 h-12 text-muted-foreground".to_string() } },
                }
                p { class: "text-lg text-muted-foreground", "{status_message}" }
            }

            // Quick actions when connected
            if ws.is_connected() {
                div { class: "grid grid-cols-2 gap-4 w-full max-w-sm",
                    Link {
                        to: Route::Transport {},
                        class: "flex flex-col items-center gap-2 p-6 rounded-2xl bg-card border border-border hover:bg-muted transition-colors",
                        IconPlay { class: "w-8 h-8 text-primary".to_string() }
                        span { class: "font-medium", "Transport" }
                    }

                    Link {
                        to: Route::SettingsPage {},
                        class: "flex flex-col items-center gap-2 p-6 rounded-2xl bg-card border border-border hover:bg-muted transition-colors",
                        IconSettings { class: "w-8 h-8 text-muted-foreground".to_string() }
                        span { class: "font-medium", "Settings" }
                    }
                }
            }

            // Retry button when disconnected
            if matches!(ws.status(), ConnectionStatus::Disconnected | ConnectionStatus::Error) {
                button {
                    class: "px-6 py-3 bg-primary text-primary-foreground rounded-xl font-medium hover:bg-primary/90 transition-colors",
                    onclick: move |_| {
                        let window = web_sys::window().expect("no window");
                        let location = window.location();
                        let protocol = location.protocol().unwrap_or_else(|_| "http:".to_string());
                        let host = location.host().unwrap_or_else(|_| "localhost:9250".to_string());
                        let ws_protocol = if protocol == "https:" { "wss:" } else { "ws:" };
                        let url = format!("{}//{}/api/@ws", ws_protocol, host);
                        ws.connect(&url);
                    },
                    "Retry Connection"
                }
            }
        }
    }
}

/// Transport control page
#[component]
fn Transport() -> Element {
    rsx! {
        div { class: "p-4",
            TransportControls {}
        }
    }
}

/// Settings page
#[component]
fn SettingsPage() -> Element {
    let ws = use_roam_websocket();

    rsx! {
        div { class: "p-6 space-y-6",
            h1 { class: "text-2xl font-bold", "Settings" }

            // Connection info
            div { class: "space-y-4",
                h2 { class: "text-lg font-semibold text-muted-foreground", "Connection" }

                div { class: "p-4 rounded-xl bg-card border border-border space-y-3",
                    div { class: "flex justify-between items-center",
                        span { class: "text-muted-foreground", "Status" }
                        ConnectionStatusIndicator {}
                    }

                    if let Some(error) = ws.last_error() {
                        div { class: "p-3 rounded-lg bg-red-500/10 border border-red-500/20",
                            p { class: "text-sm text-red-500", "{error}" }
                        }
                    }

                    // Manual reconnect
                    button {
                        class: "w-full py-3 bg-muted rounded-lg font-medium hover:bg-muted/80 transition-colors",
                        onclick: move |_| {
                            ws.disconnect();
                            let window = web_sys::window().expect("no window");
                            let location = window.location();
                            let protocol = location.protocol().unwrap_or_else(|_| "http:".to_string());
                            let host = location.host().unwrap_or_else(|_| "localhost:9250".to_string());
                            let ws_protocol = if protocol == "https:" { "wss:" } else { "ws:" };
                            let url = format!("{}//{}/api/@ws", ws_protocol, host);
                            ws.connect(&url);
                        },
                        "Reconnect"
                    }
                }
            }

            // About section
            div { class: "space-y-4",
                h2 { class: "text-lg font-semibold text-muted-foreground", "About" }

                div { class: "p-4 rounded-xl bg-card border border-border space-y-2",
                    div { class: "flex justify-between",
                        span { class: "text-muted-foreground", "App" }
                        span { "FTS Control" }
                    }
                    div { class: "flex justify-between",
                        span { class: "text-muted-foreground", "Version" }
                        span { "0.1.0" }
                    }
                }
            }
        }
    }
}
