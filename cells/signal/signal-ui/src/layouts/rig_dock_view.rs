//! Rig Dock View — the rig tab's own dock-based layout.
//!
//! Renders the rig as a fully independent docked view with:
//! - Sub-tab bar (Performance | Edit | Songs | Advanced)
//! - Rig top bar (mode selectors, view toggles) — Edit tab only
//! - DockRoot rendering a rig-specific window from DOCK_WORKSPACE
//! - Collapsible scene grid and snapshot panel at the bottom
//!
//! The top bar controls directly modify the rig dock layout:
//! - **Rig view mode** (Preset/Profile/Song) changes which side panels appear
//! - **Center view** (Node/Grid) changes which center panel is shown

use crate::components::advanced_inspector::AdvancedInspectorView;
use crate::components::performance::PerformanceView;
use crate::components::rig_grid::scene_grid::SceneGridPanel;
use crate::components::rig_grid::view_mode::{ModuleViewMode, RigViewMode};
use crate::components::song_editor::SongEditorView;
use crate::hooks::rig_state::use_rig_subscription;
use crate::prelude::*;
use crate::signals::{
    init_rig_service, RigEditorTab, RIG_CONNECTED, RIG_CURRENT_PRESET, RIG_EDITOR_TAB, RIG_LOADING,
};
use dock_dioxus::{DockRoot, DOCK_WORKSPACE, RIG_DOCK_WINDOW_ID};
use dock_proto::builder::DockLayoutBuilder as B;
use dock_proto::PanelId;

/// Build the rig dock layout for the given view mode and center panel.
///
/// Layout structure:
/// - **Preset**: `[PresetBrowser] | [center + detail]`
/// - **Profile**: `[PresetBrowser] | [center + detail] | [ProfileBrowser]`
/// - **Song**: `[PresetBrowser / ProfileBrowser] | [center + detail] | [SongParts / SongSelector]`
///
/// The center column is always: `[NodeGraph or GridEditor]` on top, `[DetailEditor]` below.
fn build_rig_layout(rig_mode: RigViewMode, center_panel: PanelId) -> dock_proto::DockLayout {
    // Center column: graph/grid on top, detail editor below
    let center = B::vertical()
        .top(B::tile(center_panel))
        .bottom(B::tile(PanelId::RigDetailEditor))
        .ratio(70.0)
        .build_node();

    match rig_mode {
        RigViewMode::Preset => {
            // [PresetBrowser 20%] | [center]
            B::horizontal()
                .left(B::tile(PanelId::PresetBrowser))
                .right(center)
                .ratio(20.0)
                .build()
        }
        RigViewMode::Profile => {
            // [PresetBrowser 20%] | [center] | [ProfileBrowser 20%]
            B::horizontal()
                .left(B::tile(PanelId::PresetBrowser))
                .right(
                    B::horizontal()
                        .left(center)
                        .right(B::tile(PanelId::ProfileBrowser))
                        .ratio(80.0)
                        .build_node(),
                )
                .ratio(20.0)
                .build()
        }
        RigViewMode::Song => {
            // [PresetBrowser / ProfileBrowser] | [center] | [SongParts / SongSelector]
            let left = B::vertical()
                .top(B::tile(PanelId::PresetBrowser))
                .bottom(B::tile(PanelId::ProfileBrowser))
                .ratio(60.0)
                .build_node();
            let right = B::vertical()
                .top(B::tile(PanelId::SongParts))
                .bottom(B::tile(PanelId::SongSelector))
                .ratio(50.0)
                .build_node();
            B::horizontal()
                .left(left)
                .right(
                    B::horizontal()
                        .left(center)
                        .right(right)
                        .ratio(75.0)
                        .build_node(),
                )
                .ratio(20.0)
                .build()
        }
    }
}

/// Resolve which PanelId to show in the center based on the module view mode.
fn center_panel_for_view(mode: ModuleViewMode) -> PanelId {
    match mode {
        ModuleViewMode::Flow | ModuleViewMode::FlowCompact => PanelId::RigNodeGraph,
        ModuleViewMode::Grid => PanelId::RigGridEditor,
    }
}

/// Apply a new layout to the rig dock window.
fn apply_rig_layout(rig_mode: RigViewMode, view_mode: ModuleViewMode) {
    let Some(rig_window_id) = *RIG_DOCK_WINDOW_ID.read() else {
        return;
    };
    let center = center_panel_for_view(view_mode);
    let layout = build_rig_layout(rig_mode, center);
    let mut workspace = DOCK_WORKSPACE.write();
    if let Some(window) = workspace.windows.get_mut(&rig_window_id) {
        window.layout = layout;
    }
}

/// Rig Dock View — the rig tab entry point.
///
/// Hosts a sub-tab bar, a custom rig top bar (in Edit mode), a dock layout,
/// and collapsible scene/snapshot panels at the bottom.
#[component]
pub fn RigDockView() -> Element {
    init_rig_service();
    use_rig_subscription();

    let active_tab = *RIG_EDITOR_TAB.read();
    let rig_window_id = *RIG_DOCK_WINDOW_ID.read();

    // Local state for rig controls
    let mut rig_view_mode = use_signal(|| RigViewMode::Song);
    let mut module_view_mode = use_signal(|| ModuleViewMode::Grid);
    let mut scene_grid_open = use_signal(|| false);
    let mut snapshot_panel_open = use_signal(|| false);

    // Apply layout on first render
    use_hook(|| {
        apply_rig_layout(RigViewMode::Song, ModuleViewMode::Grid);
    });

    rsx! {
        div { class: "h-full w-full flex flex-col bg-card overflow-hidden",
            // ── Sub-tab bar ──────────────────────────────────────
            div { class: "flex items-center gap-1 px-3 py-1.5 border-b border-border bg-zinc-900/60 flex-shrink-0",
                div { class: "flex items-center gap-0.5 bg-zinc-800/80 rounded-lg p-0.5",
                    for tab in RigEditorTab::all() {
                        RigSubTabButton {
                            key: "{tab.display_name()}",
                            tab: *tab,
                            is_active: *tab == active_tab,
                        }
                    }
                }
            }

            // ── Tab content ──────────────────────────────────────
            div { class: "flex-1 flex flex-col min-h-0 overflow-hidden",
                match active_tab {
                    RigEditorTab::Edit => rsx! {
                        // Rig top bar: mode + view selectors + preset info + status
                        RigTopBar {
                            rig_view_mode: rig_view_mode(),
                            on_rig_view_mode_change: move |mode: RigViewMode| {
                                rig_view_mode.set(mode);
                                apply_rig_layout(mode, module_view_mode());
                            },
                            module_view_mode: module_view_mode(),
                            on_module_view_mode_change: move |mode: ModuleViewMode| {
                                module_view_mode.set(mode);
                                apply_rig_layout(rig_view_mode(), mode);
                            },
                            scene_grid_open: scene_grid_open(),
                            on_toggle_scene_grid: move |_| {
                                scene_grid_open.set(!scene_grid_open());
                                if scene_grid_open() { snapshot_panel_open.set(false); }
                            },
                        }

                        // Dock root for rig window
                        div { class: "flex-1 overflow-hidden relative",
                            if let Some(window_id) = rig_window_id {
                                DockRoot { window_id: Some(window_id) }
                            } else {
                                div { class: "h-full w-full flex items-center justify-center text-muted-foreground",
                                    "Rig dock not initialized"
                                }
                            }
                        }

                        // Bottom collapsible panels
                        div { class: "flex-shrink-0 border-t border-border bg-background",
                            div { class: "flex items-center justify-center gap-4",
                                button {
                                    class: if scene_grid_open() {
                                        "flex items-center gap-2 px-3 py-1 text-xs font-medium text-foreground bg-muted/50 transition-colors"
                                    } else {
                                        "flex items-center gap-2 px-3 py-1 text-xs font-medium text-muted-foreground hover:text-foreground hover:bg-muted/50 transition-colors"
                                    },
                                    onclick: move |_| {
                                        scene_grid_open.set(!scene_grid_open());
                                        if scene_grid_open() { snapshot_panel_open.set(false); }
                                    },
                                    span { class: "text-[10px]",
                                        if scene_grid_open() { "\u{25BC}" } else { "\u{25B2}" }
                                    }
                                    "Scenes"
                                }
                                button {
                                    class: if snapshot_panel_open() {
                                        "flex items-center gap-2 px-3 py-1 text-xs font-medium text-foreground bg-muted/50 transition-colors"
                                    } else {
                                        "flex items-center gap-2 px-3 py-1 text-xs font-medium text-muted-foreground hover:text-foreground hover:bg-muted/50 transition-colors"
                                    },
                                    onclick: move |_| {
                                        snapshot_panel_open.set(!snapshot_panel_open());
                                        if snapshot_panel_open() { scene_grid_open.set(false); }
                                    },
                                    span { class: "text-[10px]",
                                        if snapshot_panel_open() { "\u{25BC}" } else { "\u{25B2}" }
                                    }
                                    "Snapshots"
                                }
                            }

                            if scene_grid_open() {
                                div { class: "h-48 overflow-hidden",
                                    SceneGridPanel { view_mode: rig_view_mode() }
                                }
                            }

                            if snapshot_panel_open() {
                                div { class: "h-72 overflow-hidden",
                                    crate::components::snapshot_test_harness::SnapshotTestHarness {}
                                }
                            }
                        }
                    },
                    RigEditorTab::Performance => rsx! {
                        PerformanceView {}
                    },
                    RigEditorTab::SongEditor => rsx! {
                        SongEditorView {}
                    },
                    RigEditorTab::Advanced => rsx! {
                        AdvancedInspectorView {}
                    },
                }
            }
        }
    }
}

// ── Rig Top Bar ──────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct RigTopBarProps {
    rig_view_mode: RigViewMode,
    on_rig_view_mode_change: Callback<RigViewMode>,
    module_view_mode: ModuleViewMode,
    on_module_view_mode_change: Callback<ModuleViewMode>,
    scene_grid_open: bool,
    on_toggle_scene_grid: Callback<()>,
}

/// Rig top bar — controls dock layout via mode and view selectors.
///
/// Contains:
/// - Rig view mode (Preset / Profile / Song) — changes side panels
/// - Center view (Node / Grid) — changes center panel
/// - Scene grid toggle
/// - Current preset info display
/// - Connection status
#[component]
fn RigTopBar(props: RigTopBarProps) -> Element {
    let preset = RIG_CURRENT_PRESET.read();
    let connected = *RIG_CONNECTED.read();
    let loading = *RIG_LOADING.read();

    rsx! {
        div { class: "h-11 flex items-center justify-between px-4 bg-zinc-900 border-b border-zinc-800 flex-shrink-0",
            // Left: Mode + View selectors
            div { class: "flex items-center gap-3",
                // Rig view mode (Preset / Profile / Song)
                div { class: "flex items-center gap-0.5 bg-zinc-800 rounded-lg p-0.5",
                    for mode in RigViewMode::all() {
                        {
                            let m = *mode;
                            let is_active = m == props.rig_view_mode;
                            rsx! {
                                button {
                                    class: if is_active {
                                        "px-3 py-1 rounded text-xs font-medium bg-blue-600 text-white transition-colors"
                                    } else {
                                        "px-3 py-1 rounded text-xs font-medium text-zinc-400 hover:text-zinc-200 transition-colors"
                                    },
                                    onclick: move |_| props.on_rig_view_mode_change.call(m),
                                    "{mode.display_name()}"
                                }
                            }
                        }
                    }
                }

                div { class: "w-px h-6 bg-zinc-700" }

                // Center view (Node / Grid)
                div { class: "flex items-center gap-0.5 bg-zinc-800 rounded-lg p-0.5",
                    {
                        let modes: &[(ModuleViewMode, &str)] = &[
                            (ModuleViewMode::Grid, "Grid"),
                            (ModuleViewMode::Flow, "Node"),
                        ];
                        rsx! {
                            for (mode, label) in modes.iter() {
                                {
                                    let m = *mode;
                                    let l = *label;
                                    let is_active = m == props.module_view_mode
                                        || (m == ModuleViewMode::Flow && props.module_view_mode == ModuleViewMode::FlowCompact);
                                    rsx! {
                                        button {
                                            class: if is_active {
                                                "px-3 py-1 rounded text-xs font-medium bg-zinc-600 text-white transition-colors"
                                            } else {
                                                "px-3 py-1 rounded text-xs font-medium text-zinc-400 hover:text-zinc-200 transition-colors"
                                            },
                                            onclick: move |_| props.on_module_view_mode_change.call(m),
                                            "{l}"
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                div { class: "w-px h-6 bg-zinc-700" }

                // Scene grid toggle
                button {
                    class: if props.scene_grid_open {
                        "flex items-center gap-1.5 px-3 py-1 rounded-lg bg-green-700 hover:bg-green-600 \
                         text-xs font-medium text-white transition-colors"
                    } else {
                        "flex items-center gap-1.5 px-3 py-1 rounded-lg bg-zinc-800 hover:bg-zinc-700 \
                         text-xs font-medium text-zinc-300 transition-colors"
                    },
                    onclick: move |_| props.on_toggle_scene_grid.call(()),
                    "Scenes"
                }
            }

            // Center: Current preset info
            div { class: "flex items-center gap-3",
                if let Some(ref preset) = *preset {
                    span { class: "text-sm font-semibold text-green-500", "1A" }
                    span { class: "text-sm font-medium text-zinc-200", "{preset.name}" }
                    span { class: "text-xs text-zinc-500 bg-zinc-800 px-2 py-0.5 rounded",
                        "{preset.category}"
                    }
                } else {
                    span { class: "text-sm font-medium text-zinc-500", "No Preset" }
                }
            }

            // Right: Connection status
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

// ── Sub-Tab Button ───────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct RigSubTabButtonProps {
    tab: RigEditorTab,
    is_active: bool,
}

#[component]
fn RigSubTabButton(props: RigSubTabButtonProps) -> Element {
    let tab = props.tab;

    rsx! {
        button {
            class: if props.is_active {
                "px-3 py-1.5 rounded-md text-xs font-medium bg-primary text-primary-foreground transition-colors"
            } else {
                "px-3 py-1.5 rounded-md text-xs font-medium text-zinc-400 hover:text-zinc-200 hover:bg-zinc-700/50 transition-colors"
            },
            onclick: move |_| {
                *RIG_EDITOR_TAB.write() = tab;
            },
            "{props.tab.display_name()}"
        }
    }
}
