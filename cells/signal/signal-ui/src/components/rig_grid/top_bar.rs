//! Guitar rig top bar component.
//!
//! Provides navigation and controls for the guitar rig page:
//! - Sidebar toggle
//! - Modules button (opens module browser)
//! - View mode selector (Grid/Macro/Detail)
//! - Current preset name display
//! - Connection status indicator

use crate::hooks::graph_history::use_graph_history;
use crate::hooks::parameter_capture::use_parameter_capture;
use crate::prelude::*;
use crate::signals::{RIG_CONNECTED, RIG_CURRENT_PRESET, RIG_LOADING};

use super::view_mode::{ModuleViewMode, RigViewMode};

/// Props for the guitar rig top bar.
#[derive(Props, Clone, PartialEq)]
pub struct GuitarRigTopBarProps {
    /// Whether the left sidebar is currently open.
    pub sidebar_open: bool,
    /// Callback to toggle the sidebar.
    pub on_toggle_sidebar: Callback<()>,
    /// Callback to open the module browser modal.
    pub on_open_module_browser: Callback<()>,
    /// Current module view mode (Grid/Macro/Detail).
    pub module_view_mode: ModuleViewMode,
    /// Callback when module view mode changes.
    pub on_module_view_mode_change: Callback<ModuleViewMode>,
    /// Current rig view mode (Preset/Profile/Song).
    pub rig_view_mode: RigViewMode,
    /// Callback when rig view mode changes.
    pub on_rig_view_mode_change: Callback<RigViewMode>,
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

    // Undo/redo
    let history = use_graph_history();

    // Snapshot naming dialog state
    let mut snapshot_dialog_open = use_signal(|| false);
    let mut snapshot_name = use_signal(|| String::new());
    let save_snapshot = use_parameter_capture();

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

                // Undo / Redo buttons
                {
                    let undo_cb = history.undo.clone();
                    let redo_cb = history.redo.clone();
                    rsx! {
                        button {
                            class: "p-2 rounded-lg hover:bg-zinc-800 transition-colors \
                                    disabled:opacity-30 disabled:cursor-not-allowed",
                            disabled: !history.can_undo,
                            title: "Undo (Cmd+Z)",
                            onclick: move |_| undo_cb.call(()),
                            // Undo arrow icon (↺)
                            svg {
                                class: "w-4 h-4 text-zinc-400",
                                fill: "none",
                                stroke: "currentColor",
                                stroke_width: "2",
                                view_box: "0 0 24 24",
                                path {
                                    stroke_linecap: "round",
                                    stroke_linejoin: "round",
                                    d: "M3 10h10a5 5 0 015 5v2M3 10l4-4M3 10l4 4",
                                }
                            }
                        }
                        button {
                            class: "p-2 rounded-lg hover:bg-zinc-800 transition-colors \
                                    disabled:opacity-30 disabled:cursor-not-allowed",
                            disabled: !history.can_redo,
                            title: "Redo (Cmd+Shift+Z)",
                            onclick: move |_| redo_cb.call(()),
                            // Redo arrow icon (↻)
                            svg {
                                class: "w-4 h-4 text-zinc-400",
                                fill: "none",
                                stroke: "currentColor",
                                stroke_width: "2",
                                view_box: "0 0 24 24",
                                path {
                                    stroke_linecap: "round",
                                    stroke_linejoin: "round",
                                    d: "M21 10H11a5 5 0 00-5 5v2M21 10l-4-4M21 10l-4 4",
                                }
                            }
                        }
                    }
                }

                // Separator
                div { class: "w-px h-6 bg-zinc-700" }

                // Rig view mode selector (Preset/Profile/Song)
                div { class: "flex items-center gap-1 bg-zinc-800 rounded-lg p-1",
                    for mode in RigViewMode::all() {
                        RigModeButton {
                            key: "{mode.display_name()}",
                            mode: *mode,
                            is_active: *mode == props.rig_view_mode,
                            on_click: props.on_rig_view_mode_change.clone(),
                        }
                    }
                }

                // Separator
                div { class: "w-px h-6 bg-zinc-700" }

                // Module view mode selector (Grid/Macro/Detail)
                div { class: "flex items-center gap-1 bg-zinc-800 rounded-lg p-1",
                    for mode in ModuleViewMode::all() {
                        ModuleViewModeButton {
                            key: "{mode.display_name()}",
                            mode: *mode,
                            is_active: *mode == props.module_view_mode,
                            on_click: props.on_module_view_mode_change.clone(),
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

            // Right section: Save Snapshot + Connection status + STOMP mode
            div { class: "flex items-center gap-3",
                // Save Snapshot button
                button {
                    class: "flex items-center gap-2 px-3 py-1.5 rounded-lg bg-zinc-800 hover:bg-zinc-700 \
                            text-sm font-medium text-zinc-300 transition-colors",
                    onclick: move |_| {
                        snapshot_name.set(String::new());
                        snapshot_dialog_open.set(true);
                    },
                    title: "Save current rig state as a snapshot",
                    // Camera/snapshot icon
                    svg {
                        class: "w-4 h-4",
                        fill: "none",
                        stroke: "currentColor",
                        stroke_width: "2",
                        view_box: "0 0 24 24",
                        path {
                            stroke_linecap: "round",
                            stroke_linejoin: "round",
                            d: "M3 9a2 2 0 012-2h.93a2 2 0 001.664-.89l.812-1.22A2 2 0 0110.07 4h3.86a2 2 0 011.664.89l.812 1.22A2 2 0 0018.07 7H19a2 2 0 012 2v9a2 2 0 01-2 2H5a2 2 0 01-2-2V9z",
                        }
                        path {
                            stroke_linecap: "round",
                            stroke_linejoin: "round",
                            d: "M15 13a3 3 0 11-6 0 3 3 0 016 0z",
                        }
                    }
                    span { "Save Snapshot" }
                }

                // Separator
                div { class: "w-px h-6 bg-zinc-700" }

                // Stomp mode indicator (Quad Cortex style)
                div { class: "flex items-center gap-2 text-xs text-zinc-500",
                    span { "STOMP" }
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

        // Snapshot naming dialog (overlay)
        if snapshot_dialog_open() {
            SnapshotNamingDialog {
                name: snapshot_name(),
                on_name_change: move |new_name: String| snapshot_name.set(new_name),
                on_save: move |name: String| {
                    if !name.trim().is_empty() {
                        save_snapshot.call(name);
                        snapshot_dialog_open.set(false);
                    }
                },
                on_cancel: move |_| snapshot_dialog_open.set(false),
            }
        }
    }
}

/// Props for rig mode button.
#[derive(Props, Clone, PartialEq)]
struct RigModeButtonProps {
    mode: RigViewMode,
    is_active: bool,
    on_click: Callback<RigViewMode>,
}

/// Button for selecting a rig view mode (Preset/Profile/Song).
#[component]
fn RigModeButton(props: RigModeButtonProps) -> Element {
    let mode = props.mode;

    rsx! {
        button {
            class: if props.is_active {
                "px-3 py-1.5 rounded text-xs font-medium bg-blue-600 text-white transition-colors"
            } else {
                "px-3 py-1.5 rounded text-xs font-medium text-zinc-400 hover:text-zinc-200 transition-colors"
            },
            title: "{props.mode.display_name()} mode",
            onclick: move |_| props.on_click.call(mode),
            span { "{props.mode.display_name()}" }
        }
    }
}

/// Props for module view mode button.
#[derive(Props, Clone, PartialEq)]
struct ModuleViewModeButtonProps {
    mode: ModuleViewMode,
    is_active: bool,
    on_click: Callback<ModuleViewMode>,
}

/// Button for selecting a module view mode (Grid/Macro/Detail).
#[component]
fn ModuleViewModeButton(props: ModuleViewModeButtonProps) -> Element {
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

// ─── Snapshot Naming Dialog ──────────────────────────────────────────────

/// Props for the snapshot naming dialog.
#[derive(Props, Clone, PartialEq)]
struct SnapshotNamingDialogProps {
    /// Current name value.
    name: String,
    /// Callback when the name changes.
    on_name_change: Callback<String>,
    /// Callback when the user confirms (receives the final name).
    on_save: Callback<String>,
    /// Callback when the user cancels.
    on_cancel: Callback<()>,
}

/// Modal dialog for naming a snapshot before saving.
#[component]
fn SnapshotNamingDialog(props: SnapshotNamingDialogProps) -> Element {
    let name_for_save = props.name.clone();

    rsx! {
        // Backdrop
        div {
            class: "fixed inset-0 z-50 flex items-center justify-center bg-black/50",
            onclick: move |_| props.on_cancel.call(()),

            // Dialog box (stop click propagation)
            div {
                class: "bg-zinc-900 border border-zinc-700 rounded-xl shadow-2xl p-6 w-96",
                onclick: move |e| e.stop_propagation(),

                // Title
                h3 { class: "text-lg font-semibold text-zinc-100 mb-4",
                    "Save Snapshot"
                }

                // Name input
                div { class: "mb-4",
                    label { class: "block text-sm text-zinc-400 mb-1",
                        r#for: "snapshot-name",
                        "Snapshot Name"
                    }
                    input {
                        class: "w-full px-3 py-2 bg-zinc-800 border border-zinc-600 rounded-lg \
                                text-zinc-200 text-sm placeholder-zinc-500 \
                                focus:outline-none focus:border-blue-500 focus:ring-1 focus:ring-blue-500",
                        id: "snapshot-name",
                        r#type: "text",
                        placeholder: "e.g. Verse Clean, Solo Lead...",
                        value: "{props.name}",
                        autofocus: true,
                        oninput: move |e| props.on_name_change.call(e.value()),
                    }
                }

                // Actions
                div { class: "flex justify-end gap-2",
                    button {
                        class: "px-4 py-2 rounded-lg text-sm text-zinc-400 hover:text-zinc-200 \
                                hover:bg-zinc-800 transition-colors",
                        onclick: move |_| props.on_cancel.call(()),
                        "Cancel"
                    }
                    button {
                        class: "px-4 py-2 rounded-lg text-sm font-medium bg-blue-600 text-white \
                                hover:bg-blue-500 transition-colors disabled:opacity-50 disabled:cursor-not-allowed",
                        disabled: name_for_save.trim().is_empty(),
                        onclick: {
                            let name = props.name.clone();
                            move |_| props.on_save.call(name.clone())
                        },
                        "Save"
                    }
                }
            }
        }
    }
}
