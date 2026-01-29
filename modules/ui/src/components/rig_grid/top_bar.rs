//! Guitar rig top bar component.
//!
//! Provides navigation and controls for the guitar rig page:
//! - Sidebar toggle
//! - Modules button (opens module browser)
//! - View mode selector (Grid/Macro/Detail)
//! - Current preset name display
//! - Connection status indicator

use dioxus::prelude::*;
use fts::rig::{RIG_CONNECTED, RIG_CURRENT_PRESET, RIG_LOADING};

use super::view_mode::ModuleViewMode;

/// Props for the guitar rig top bar.
#[derive(Props, Clone, PartialEq)]
pub struct GuitarRigTopBarProps {
    /// Whether the left sidebar is currently open.
    pub sidebar_open: bool,
    /// Callback to toggle the sidebar.
    pub on_toggle_sidebar: Callback<()>,
    /// Callback to open the module browser modal.
    pub on_open_module_browser: Callback<()>,
    /// Current view mode.
    pub view_mode: ModuleViewMode,
    /// Callback when view mode changes.
    pub on_view_mode_change: Callback<ModuleViewMode>,
}

/// Top bar for the guitar rig page.
///
/// Features:
/// - Sidebar toggle (hamburger button)
/// - "Modules" button to open module browser
/// - View mode selector with icons
/// - Current preset name and category
/// - Connection status indicator
#[component]
pub fn GuitarRigTopBar(props: GuitarRigTopBarProps) -> Element {
    // Read global signals for display
    let preset = RIG_CURRENT_PRESET.read();
    let connected = *RIG_CONNECTED.read();
    let loading = *RIG_LOADING.read();

    rsx! {
        div { class: "h-14 flex items-center justify-between px-4 bg-zinc-900 border-b border-zinc-800",
            // Left section: Sidebar toggle + Modules button + View mode
            div { class: "flex items-center gap-3",
                // Sidebar toggle button
                button {
                    class: "p-2 rounded-lg hover:bg-zinc-800 transition-colors",
                    onclick: move |_| props.on_toggle_sidebar.call(()),
                    title: if props.sidebar_open { "Hide sidebar" } else { "Show sidebar" },
                    // Hamburger icon
                    svg {
                        class: "w-5 h-5 text-zinc-400",
                        fill: "none",
                        stroke: "currentColor",
                        stroke_width: "2",
                        view_box: "0 0 24 24",
                        path {
                            stroke_linecap: "round",
                            stroke_linejoin: "round",
                            d: if props.sidebar_open {
                                "M6 18L18 6M6 6l12 12"
                            } else {
                                "M4 6h16M4 12h16M4 18h16"
                            },
                        }
                    }
                }

                // Modules button
                button {
                    class: "flex items-center gap-2 px-3 py-1.5 rounded-lg bg-zinc-800 hover:bg-zinc-700 \
                            text-sm font-medium text-zinc-300 transition-colors",
                    onclick: move |_| props.on_open_module_browser.call(()),
                    // Grid/modules icon
                    svg {
                        class: "w-4 h-4",
                        fill: "none",
                        stroke: "currentColor",
                        stroke_width: "2",
                        view_box: "0 0 24 24",
                        path {
                            stroke_linecap: "round",
                            stroke_linejoin: "round",
                            d: "M4 5a1 1 0 011-1h4a1 1 0 011 1v4a1 1 0 01-1 1H5a1 1 0 01-1-1V5zM14 5a1 1 0 011-1h4a1 1 0 011 1v4a1 1 0 01-1 1h-4a1 1 0 01-1-1V5zM4 15a1 1 0 011-1h4a1 1 0 011 1v4a1 1 0 01-1 1H5a1 1 0 01-1-1v-4zM14 15a1 1 0 011-1h4a1 1 0 011 1v4a1 1 0 01-1 1h-4a1 1 0 01-1-1v-4z",
                        }
                    }
                    span { "Modules" }
                }

                // Separator
                div { class: "w-px h-6 bg-zinc-700" }

                // View mode selector
                div { class: "flex items-center gap-1 bg-zinc-800 rounded-lg p-1",
                    for mode in ModuleViewMode::all() {
                        ViewModeButton {
                            key: "{mode.display_name()}",
                            mode: *mode,
                            is_active: *mode == props.view_mode,
                            on_click: props.on_view_mode_change.clone(),
                        }
                    }
                }
            }

            // Center section: Current preset info
            div { class: "flex items-center gap-3",
                if let Some(ref preset) = *preset {
                    // Scene/preset indicator (Quad Cortex style)
                    span { class: "text-lg font-semibold text-green-500", "1A" }
                    span { class: "text-lg font-medium text-zinc-200", "{preset.name}" }
                    span { class: "text-sm text-zinc-500 bg-zinc-800 px-2 py-0.5 rounded",
                        "{preset.category}"
                    }
                } else {
                    span { class: "text-lg font-medium text-zinc-500", "No Preset Selected" }
                }
            }

            // Right section: Connection status + STOMP mode
            div { class: "flex items-center gap-3",
                // Stomp mode indicator (Quad Cortex style)
                div { class: "flex items-center gap-2 text-xs text-zinc-500",
                    span { "⚡ STOMP" }
                }

                // Separator
                div { class: "w-px h-6 bg-zinc-700" }

                // Connection status
                div { class: "flex items-center gap-2",
                    div {
                        class: if connected {
                            "w-2 h-2 rounded-full bg-green-500"
                        } else {
                            "w-2 h-2 rounded-full bg-red-500"
                        },
                    }
                    span { class: "text-xs text-zinc-500",
                        if loading {
                            "Loading..."
                        } else if connected {
                            "Connected"
                        } else {
                            "Disconnected"
                        }
                    }
                }
            }
        }
    }
}

/// Props for view mode button.
#[derive(Props, Clone, PartialEq)]
struct ViewModeButtonProps {
    mode: ModuleViewMode,
    is_active: bool,
    on_click: Callback<ModuleViewMode>,
}

/// Button for selecting a view mode.
#[component]
fn ViewModeButton(props: ViewModeButtonProps) -> Element {
    let mode = props.mode;

    rsx! {
        button {
            class: if props.is_active {
                "px-3 py-1.5 rounded text-xs font-medium bg-zinc-600 text-white transition-colors"
            } else {
                "px-3 py-1.5 rounded text-xs font-medium text-zinc-400 hover:text-zinc-200 transition-colors"
            },
            title: "{props.mode.display_name()} view",
            onclick: move |_| props.on_click.call(mode),
            span { class: "mr-1", "{props.mode.icon()}" }
            span { "{props.mode.display_name()}" }
        }
    }
}
