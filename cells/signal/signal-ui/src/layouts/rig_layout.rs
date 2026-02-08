//! Top-level layout for the Rig tab and standalone dock panel components.
//!
//! Each rig sub-view is available as an independent dock panel:
//! - `RigGridPanel` — Node graph / signal flow canvas
//! - `PresetBrowserPanel` — Preset browser with fuzzy search
//! - `ProfileBrowserPanel` — Profile selector with scenes
//! - `SongPartsPanel` — Scenes list for current song
//! - `SongSelectorPanel` — Setlist + song list
//!
//! All panels read from global signals and use `use_rig_actions()` for
//! dispatching commands — no context provider needed.

use crate::components::rig_grid::guitar_rig_grid::GuitarRigGrid;
use crate::components::rig_grid::left_sidebar::GuitarRigLeftSidebar;
use crate::components::rig_grid::module_browser_modal::ModuleBrowserModal;
use crate::components::rig_grid::node_property_panel::NodePropertyPanel;
use crate::components::rig_grid::profile_sidebar::GuitarRigProfileSidebar;
use crate::components::rig_grid::version_history_panel::VersionHistoryPanel;
use crate::components::rig_grid::right_sidebar::{
    GuitarRigRightSidebar, SceneListPanel, SongListPanel,
};
use crate::components::rig_grid::view_mode::{ModuleViewMode, RigViewMode};
use crate::hooks::rig_actions::use_rig_actions;
use crate::hooks::rig_state::use_rig_subscription;
use crate::prelude::*;
use crate::signals::{init_rig_service, RIG_NODE_GRAPH, RIG_SELECTED_ENTITY};

/// Main layout for the Rig tab (legacy monolithic view).
///
/// Initializes the rig service, subscribes to events, and renders the
/// full 3-panel layout. Kept for backwards compatibility — the dock
/// system uses the individual panel components instead.
#[component]
pub fn RigLayout() -> Element {
    init_rig_service();
    use_rig_subscription();
    let actions = use_rig_actions();

    // Page-level state
    let mut sidebar_open = use_signal(|| true);
    let mut module_view_mode = use_signal(|| ModuleViewMode::Flow);
    let mut rig_view_mode = use_signal(|| RigViewMode::Song);
    let mut module_browser_open = use_signal(|| false);

    // Determine which right sidebar to show based on rig view mode
    let show_right_sidebar = matches!(rig_view_mode(), RigViewMode::Song | RigViewMode::Profile);
    let has_selection = RIG_SELECTED_ENTITY.read().is_some();

    rsx! {
        div { class: "h-full w-full flex flex-col bg-background overflow-hidden",
            // Top bar with sidebar toggle, view modes, preset info, status
            crate::components::rig_grid::top_bar::GuitarRigTopBar {
                sidebar_open: sidebar_open(),
                on_toggle_sidebar: move |_| sidebar_open.set(!sidebar_open()),
                on_open_module_browser: move |_| module_browser_open.set(true),
                module_view_mode: module_view_mode(),
                on_module_view_mode_change: move |mode| module_view_mode.set(mode),
                rig_view_mode: rig_view_mode(),
                on_rig_view_mode_change: move |mode| rig_view_mode.set(mode),
            }

            // Body: sidebars + main content
            div { class: "flex-1 flex overflow-hidden",
                GuitarRigLeftSidebar {
                    is_open: sidebar_open(),
                    rig_view_mode: rig_view_mode(),
                    on_preset_select: actions.load_preset.clone(),
                    on_preset_snapshot_select: Some(actions.load_preset_snapshot.clone()),
                    on_profile_select: actions.load_profile.clone(),
                    on_profile_scene_select: Some(actions.load_profile_scene.clone()),
                }

                div { class: "flex-1 overflow-hidden",
                    GuitarRigGrid { view_mode: module_view_mode() }
                }

                // Right panel area: property panel when selected, otherwise sidebar
                if has_selection {
                    div { class: "w-64 flex-shrink-0",
                        NodePropertyPanel {}
                    }
                } else if show_right_sidebar {
                    match rig_view_mode() {
                        RigViewMode::Song => rsx! {
                            GuitarRigRightSidebar {
                                on_scene_click: actions.go_to_scene.clone(),
                                on_song_click: actions.go_to_song.clone(),
                                on_prev_scene: actions.prev_scene.clone(),
                                on_next_scene: actions.next_scene.clone(),
                                on_prev_song: actions.prev_song.clone(),
                                on_next_song: actions.next_song.clone(),
                                on_setlist_change: actions.load_setlist.clone(),
                            }
                        },
                        RigViewMode::Profile => rsx! {
                            GuitarRigProfileSidebar {
                                on_profile_select: actions.load_profile.clone(),
                                on_profile_scene_select: Some(actions.load_profile_scene.clone()),
                            }
                        },
                        _ => rsx! {},
                    }
                }
            }

            ModuleBrowserModal {
                is_open: module_browser_open(),
                on_close: move |_| module_browser_open.set(false),
                on_add_module: move |block_type: signal_control::block::BlockType| {
                    use crate::components::rig_grid::node_graph::NodeGraph;
                    let position = RIG_NODE_GRAPH.read().find_open_position();
                    let name = format!("{:?}", block_type);
                    let module = NodeGraph::create_module_for_block_type(name, block_type, position);
                    RIG_NODE_GRAPH.write().add_module(module);
                    module_browser_open.set(false);
                },
            }
        }
    }
}

// ─── Standalone Dock Panel Components ────────────────────────────────────

/// Rig node graph panel — the main signal flow canvas.
///
/// Standalone dock panel wrapper around `GuitarRigGrid`.
#[component]
pub fn RigGridPanel() -> Element {
    init_rig_service();
    use_rig_subscription();

    rsx! {
        div { class: "h-full w-full overflow-hidden",
            GuitarRigGrid { view_mode: ModuleViewMode::Flow }
        }
    }
}

/// Preset browser panel — fuzzy-searchable preset list.
///
/// Standalone dock panel wrapper around the preset section of `GuitarRigLeftSidebar`.
#[component]
pub fn PresetBrowserPanel() -> Element {
    init_rig_service();
    use_rig_subscription();
    let actions = use_rig_actions();

    rsx! {
        div { class: "h-full w-full overflow-hidden",
            GuitarRigLeftSidebar {
                is_open: true,
                rig_view_mode: RigViewMode::Preset,
                on_preset_select: actions.load_preset,
                on_preset_snapshot_select: Some(actions.load_preset_snapshot),
                on_profile_select: actions.load_profile,
                on_profile_scene_select: Some(actions.load_profile_scene),
            }
        }
    }
}

/// Profile browser panel — profile selector with expandable scenes.
///
/// Standalone dock panel wrapper around `GuitarRigProfileSidebar`.
#[component]
pub fn ProfileBrowserPanel() -> Element {
    init_rig_service();
    use_rig_subscription();
    let actions = use_rig_actions();

    rsx! {
        div { class: "h-full w-full overflow-hidden",
            GuitarRigProfileSidebar {
                on_profile_select: actions.load_profile,
                on_profile_scene_select: Some(actions.load_profile_scene),
            }
        }
    }
}

/// Song parts panel — scenes list for the current song.
///
/// Shows current song header, scene list, and prev/next navigation.
#[component]
pub fn SongPartsPanel() -> Element {
    init_rig_service();
    use_rig_subscription();
    let actions = use_rig_actions();

    rsx! {
        SceneListPanel {
            on_scene_click: actions.go_to_scene,
            on_prev_scene: actions.prev_scene,
            on_next_scene: actions.next_scene,
        }
    }
}

/// Song selector panel — setlist dropdown + song list + navigation.
#[component]
pub fn SongSelectorPanel() -> Element {
    init_rig_service();
    use_rig_subscription();
    let actions = use_rig_actions();

    rsx! {
        SongListPanel {
            on_song_click: actions.go_to_song,
            on_prev_song: actions.prev_song,
            on_next_song: actions.next_song,
            on_setlist_change: actions.load_setlist,
        }
    }
}

/// Node property panel — shows editable properties for the selected node/module.
///
/// Standalone dock panel wrapper around `NodePropertyPanel`. Reads from
/// `RIG_SELECTED_ENTITY` and `RIG_NODE_GRAPH` global signals.
#[component]
pub fn NodePropertyDockPanel() -> Element {
    init_rig_service();
    use_rig_subscription();

    rsx! {
        NodePropertyPanel {}
    }
}

/// Version history panel — shows preset version history with restore and diff.
///
/// Standalone dock panel wrapper around `VersionHistoryPanel`. Reads from
/// `RIG_CURRENT_PRESET` and `RIG_PRESET_VERSIONS` global signals.
#[component]
pub fn VersionHistoryDockPanel() -> Element {
    init_rig_service();
    use_rig_subscription();

    rsx! {
        VersionHistoryPanel {}
    }
}
