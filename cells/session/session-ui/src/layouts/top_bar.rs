//! Top Bar Layout Component
//!
//! Navigation bar with connection status indicator.
//! Based on FastTrackStudio desktop app design.

use dioxus::prelude::*;
use lucide_dioxus::{CircleCheck, CircleX};

/// Connection state for the top bar
#[derive(Clone, Copy, Debug, PartialEq, Default)]
pub enum ConnectionState {
    #[default]
    Disconnected,
    Connecting,
    Connected,
}

/// Top bar component with navigation and connection status
///
/// Displays:
/// - Connection status indicator (left)
/// - Navigation tabs (center-left)
/// - App title (center)
/// - Optional action buttons (right)
#[component]
pub fn TopBar(
    /// Current connection state
    connection_state: ConnectionState,
    /// Current active tab/route
    #[props(default = "performance".to_string())]
    active_tab: String,
    /// Callback when a tab is clicked
    #[props(default)]
    on_tab_click: Option<Callback<String>>,
    /// Optional app title override
    #[props(default = "FTS Control".to_string())]
    app_title: String,
) -> Element {
    rsx! {
        div {
            class: "h-12 flex-shrink-0 border-b border-border bg-card flex items-center justify-between px-4",

            // Left section: Connection status + navigation
            div {
                class: "flex items-center gap-4",

                // Connection status badge
                ConnectionStatusBadge {
                    state: connection_state,
                }

                // Navigation tabs
                div {
                    class: "flex items-center gap-1 ml-6",

                    NavTab {
                        label: "Performance",
                        tab_id: "performance",
                        is_active: active_tab == "performance",
                        on_click: on_tab_click.clone(),
                    }

                    NavTab {
                        label: "Setlist",
                        tab_id: "setlist",
                        is_active: active_tab == "setlist",
                        on_click: on_tab_click.clone(),
                    }

                    NavTab {
                        label: "Settings",
                        tab_id: "settings",
                        is_active: active_tab == "settings",
                        on_click: on_tab_click.clone(),
                    }
                }
            }

            // Center: App title
            h1 {
                class: "text-lg font-semibold text-card-foreground",
                "{app_title}"
            }

            // Right section: placeholder for future actions
            div {
                class: "flex items-center gap-2",
                // Placeholder for future buttons (edit mode, etc.)
            }
        }
    }
}

/// Connection status badge component
#[component]
fn ConnectionStatusBadge(state: ConnectionState) -> Element {
    let (bg_class, text_class, label, icon_color) = match state {
        ConnectionState::Connected => (
            "bg-green-500/20",
            "text-green-500",
            "Connected",
            "currentColor",
        ),
        ConnectionState::Connecting => (
            "bg-yellow-500/20",
            "text-yellow-500",
            "Connecting...",
            "currentColor",
        ),
        ConnectionState::Disconnected => (
            "bg-red-500/20",
            "text-red-500",
            "Disconnected",
            "currentColor",
        ),
    };

    rsx! {
        div {
            class: "flex items-center gap-2 px-3 py-1.5 rounded-full text-sm font-medium {bg_class} {text_class}",

            // Status icon
            match state {
                ConnectionState::Connected => rsx! {
                    CircleCheck { size: 16, color: icon_color }
                },
                ConnectionState::Connecting => rsx! {
                    div {
                        class: "w-4 h-4 border-2 border-current border-t-transparent rounded-full animate-spin",
                    }
                },
                ConnectionState::Disconnected => rsx! {
                    CircleX { size: 16, color: icon_color }
                },
            }

            // Label
            span { "{label}" }
        }
    }
}

/// Navigation tab component
#[component]
fn NavTab(
    label: String,
    tab_id: String,
    is_active: bool,
    on_click: Option<Callback<String>>,
) -> Element {
    let active_class = if is_active {
        "bg-primary text-primary-foreground"
    } else {
        "text-muted-foreground hover:text-foreground hover:bg-accent"
    };

    rsx! {
        button {
            class: "px-4 py-2 rounded-md font-medium text-sm transition-colors {active_class}",
            onclick: move |_| {
                if let Some(callback) = &on_click {
                    callback.call(tab_id.clone());
                }
            },
            "{label}"
        }
    }
}
