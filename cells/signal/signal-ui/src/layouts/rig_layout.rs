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

use crate::components::module_editor::detail_panel::DetailPanel;
use crate::components::module_editor::grid_view::{
    BlockPickerDropdown, DynamicGridView, GridConnection, GridSelection, PICKER_CELL,
    PICKER_CLICK_POS,
};
use crate::components::module_editor::module_editor_view::CompositionSlot;
use crate::components::module_editor::UnifiedGridEditor;
use crate::components::rig_grid::guitar_rig_grid::GuitarRigGrid;
use crate::components::rig_grid::left_sidebar::GuitarRigLeftSidebar;
use crate::components::rig_grid::module_browser_modal::ModuleBrowserModal;
use crate::components::rig_grid::module_preset_manager::{
    ModulePresetBrowser, ModulePresetSaveDialog,
};
use crate::components::rig_grid::node_property_panel::NodePropertyPanel;
use crate::components::rig_grid::profile_sidebar::GuitarRigProfileSidebar;
use crate::components::rig_grid::right_sidebar::{
    GuitarRigRightSidebar, SceneListPanel, SongListPanel,
};
use crate::components::rig_grid::scene_grid::SceneGridPanel;
use crate::components::rig_grid::version_history_panel::VersionHistoryPanel;
use crate::components::rig_grid::view_mode::{ModuleViewMode, RigViewMode};
use crate::components::shared::{CreateEntityModal, EntityKind};
use crate::context::rig_grid::{use_rig_grid_state, RigGridStateProvider};
use crate::hooks::rig_actions::use_rig_actions;
use crate::hooks::rig_state::use_rig_subscription;
use crate::prelude::*;
use crate::signals::{
    init_rig_service, RIG_GRID_SELECTED_SLOT, RIG_GRID_SELECTION, RIG_MODULES, RIG_NODE_GRAPH,
};
use uuid::Uuid;

/// Main layout for the Rig tab (legacy monolithic view).
///
/// Initializes the rig service, subscribes to events, and renders the
/// full 3-panel layout. Kept for backwards compatibility — the dock
/// system uses the individual panel components instead.
///
/// Wraps the subtree in a `RigGridStateProvider` so all descendant
/// components can access rig grid selection state via `use_rig_grid_state()`.
#[component]
pub fn RigLayout() -> Element {
    init_rig_service();
    use_rig_subscription();

    rsx! {
        RigGridStateProvider {
            RigLayoutInner {}
        }
    }
}

/// Inner layout body — separated so that `use_rig_grid_state()` can read
/// from the context provided by `RigLayout`'s `RigGridStateProvider`.
#[component]
fn RigLayoutInner() -> Element {
    let actions = use_rig_actions();
    let grid_state = use_rig_grid_state();

    // Page-level state
    let mut sidebar_open = use_signal(|| false);
    let mut module_view_mode = use_signal(|| ModuleViewMode::Flow);
    let mut rig_view_mode = use_signal(|| RigViewMode::Song);
    let mut module_browser_open = use_signal(|| false);
    let mut scene_grid_open = use_signal(|| false);
    let mut snapshot_panel_open = use_signal(|| false);
    let mut create_modal_kind = use_signal(|| None::<EntityKind>);

    // Determine which right sidebar to show based on rig view mode
    let show_right_sidebar = matches!(rig_view_mode(), RigViewMode::Song | RigViewMode::Profile);
    let has_selection = grid_state.has_selection();

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
                scene_grid_open: scene_grid_open(),
                on_toggle_scene_grid: move |_| scene_grid_open.set(!scene_grid_open()),
            }

            // Body: sidebars + main content + optional bottom scene grid
            div { class: "flex-1 flex flex-col overflow-hidden",
                // Main row: left sidebar + graph + right sidebar
                div { class: "flex-1 flex overflow-hidden",
                    GuitarRigLeftSidebar {
                        is_open: sidebar_open(),
                        rig_view_mode: rig_view_mode(),
                        on_preset_select: actions.load_preset.clone(),
                        on_preset_snapshot_select: Some(actions.load_preset_snapshot.clone()),
                        on_profile_select: actions.load_profile.clone(),
                        on_profile_scene_select: Some(actions.load_profile_scene.clone()),
                        on_create_preset: Some(Callback::new(move |_| create_modal_kind.set(Some(EntityKind::Preset)))),
                        on_create_profile: Some(Callback::new(move |_| create_modal_kind.set(Some(EntityKind::Profile)))),
                    }

                    div { class: "flex-1 overflow-hidden",
                        match module_view_mode() {
                            ModuleViewMode::Grid => rsx! { UnifiedGridEditor {} },
                            mode => rsx! { GuitarRigGrid { view_mode: mode } },
                        }
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
                                    on_create_song: Some(Callback::new(move |_| create_modal_kind.set(Some(EntityKind::Song)))),
                                    on_create_scene: Some(Callback::new(move |_| create_modal_kind.set(Some(EntityKind::Scene)))),
                                }
                            },
                            RigViewMode::Profile => rsx! {
                                GuitarRigProfileSidebar {
                                    on_profile_select: actions.load_profile.clone(),
                                    on_profile_scene_select: Some(actions.load_profile_scene.clone()),
                                    on_create_profile: Some(Callback::new(move |_| create_modal_kind.set(Some(EntityKind::Profile)))),
                                }
                            },
                            _ => rsx! {},
                        }
                    }
                }

                // Bottom toggle bar + collapsible panels
                div { class: "flex-shrink-0 border-t border-border bg-background",
                    // Toggle buttons row
                    div { class: "flex items-center justify-center gap-4",
                        button {
                            class: if scene_grid_open() {
                                "flex items-center gap-2 px-3 py-1 text-xs font-medium text-foreground bg-muted/50 transition-colors"
                            } else {
                                "flex items-center gap-2 px-3 py-1 text-xs font-medium text-muted-foreground hover:text-foreground hover:bg-muted/50 transition-colors"
                            },
                            onclick: move |_| {
                                scene_grid_open.set(!scene_grid_open());
                                if !scene_grid_open() { } else { snapshot_panel_open.set(false); }
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
                                if !snapshot_panel_open() { } else { scene_grid_open.set(false); }
                            },
                            span { class: "text-[10px]",
                                if snapshot_panel_open() { "\u{25BC}" } else { "\u{25B2}" }
                            }
                            "Snapshots"
                        }
                    }

                    // Collapsible scene grid
                    if scene_grid_open() {
                        div { class: "h-48 overflow-hidden",
                            SceneGridPanel { view_mode: rig_view_mode() }
                        }
                    }

                    // Collapsible snapshot test harness
                    if snapshot_panel_open() {
                        div { class: "h-72 overflow-hidden",
                            crate::components::snapshot_test_harness::SnapshotTestHarness {}
                        }
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

            // Create entity modal (shared across all sidebar + buttons)
            if let Some(kind) = *create_modal_kind.read() {
                CreateEntityModal {
                    kind,
                    is_open: true,
                    on_submit: {
                        let create_preset = actions.create_preset.clone();
                        let create_profile = actions.create_profile.clone();
                        let create_song = actions.create_song.clone();
                        let create_scene = actions.create_scene.clone();
                        Callback::new(move |data| {
                            match kind {
                                EntityKind::Preset => create_preset.call(data),
                                EntityKind::Profile => create_profile.call(data),
                                EntityKind::Song => create_song.call(data),
                                EntityKind::Scene => create_scene.call(data),
                                EntityKind::Setlist => {}
                            }
                            create_modal_kind.set(None);
                        })
                    },
                    on_close: Callback::new(move |_| create_modal_kind.set(None)),
                }
            }

            // Module preset save/load dialogs (driven by global signals)
            ModulePresetSaveDialog {}
            ModulePresetBrowser {}
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
/// Owns modal state for creating new presets and profiles.
#[component]
pub fn PresetBrowserPanel() -> Element {
    init_rig_service();
    use_rig_subscription();
    let actions = use_rig_actions();

    let mut modal_kind = use_signal(|| None::<EntityKind>);

    rsx! {
        div { class: "h-full w-full overflow-hidden relative",
            GuitarRigLeftSidebar {
                is_open: true,
                rig_view_mode: RigViewMode::Preset,
                on_preset_select: actions.load_preset,
                on_preset_snapshot_select: Some(actions.load_preset_snapshot),
                on_profile_select: actions.load_profile,
                on_profile_scene_select: Some(actions.load_profile_scene),
                on_create_preset: Some(Callback::new(move |_| modal_kind.set(Some(EntityKind::Preset)))),
                on_create_profile: Some(Callback::new(move |_| modal_kind.set(Some(EntityKind::Profile)))),
            }

            if let Some(kind) = *modal_kind.read() {
                CreateEntityModal {
                    kind,
                    is_open: true,
                    on_submit: {
                        let create_preset = actions.create_preset.clone();
                        let create_profile = actions.create_profile.clone();
                        Callback::new(move |data| {
                            match kind {
                                EntityKind::Preset => create_preset.call(data),
                                EntityKind::Profile => create_profile.call(data),
                                _ => {}
                            }
                            modal_kind.set(None);
                        })
                    },
                    on_close: Callback::new(move |_| modal_kind.set(None)),
                }
            }
        }
    }
}

/// Profile browser panel — profile selector with expandable scenes.
///
/// Standalone dock panel wrapper around `GuitarRigProfileSidebar`.
/// Owns modal state for creating new profiles.
#[component]
pub fn ProfileBrowserPanel() -> Element {
    init_rig_service();
    use_rig_subscription();
    let actions = use_rig_actions();

    let mut modal_open = use_signal(|| false);

    rsx! {
        div { class: "h-full w-full overflow-hidden relative",
            GuitarRigProfileSidebar {
                on_profile_select: actions.load_profile,
                on_profile_scene_select: Some(actions.load_profile_scene),
                on_create_profile: Some(Callback::new(move |_| modal_open.set(true))),
            }

            CreateEntityModal {
                kind: EntityKind::Profile,
                is_open: *modal_open.read(),
                on_submit: {
                    let create_profile = actions.create_profile.clone();
                    Callback::new(move |data| {
                        create_profile.call(data);
                        modal_open.set(false);
                    })
                },
                on_close: Callback::new(move |_| modal_open.set(false)),
            }
        }
    }
}

/// Song parts panel — scenes list for the current song.
///
/// Shows current song header, scene list, and prev/next navigation.
/// Owns modal state for creating new scenes.
#[component]
pub fn SongPartsPanel() -> Element {
    init_rig_service();
    use_rig_subscription();
    let actions = use_rig_actions();

    let mut modal_open = use_signal(|| false);

    rsx! {
        div { class: "h-full w-full relative",
            SceneListPanel {
                on_scene_click: actions.go_to_scene,
                on_prev_scene: actions.prev_scene,
                on_next_scene: actions.next_scene,
                on_create_scene: Some(Callback::new(move |_| modal_open.set(true))),
            }

            CreateEntityModal {
                kind: EntityKind::Scene,
                is_open: *modal_open.read(),
                on_submit: {
                    let create_scene = actions.create_scene.clone();
                    Callback::new(move |data| {
                        create_scene.call(data);
                        modal_open.set(false);
                    })
                },
                on_close: Callback::new(move |_| modal_open.set(false)),
            }
        }
    }
}

/// Song selector panel — setlist dropdown + song list + navigation.
/// Owns modal state for creating new songs.
#[component]
pub fn SongSelectorPanel() -> Element {
    init_rig_service();
    use_rig_subscription();
    let actions = use_rig_actions();

    let mut modal_open = use_signal(|| false);

    rsx! {
        div { class: "h-full w-full relative",
            SongListPanel {
                on_song_click: actions.go_to_song,
                on_prev_song: actions.prev_song,
                on_next_song: actions.next_song,
                on_setlist_change: actions.load_setlist,
                on_create_song: Some(Callback::new(move |_| modal_open.set(true))),
            }

            CreateEntityModal {
                kind: EntityKind::Song,
                is_open: *modal_open.read(),
                on_submit: {
                    let create_song = actions.create_song.clone();
                    Callback::new(move |data| {
                        create_song.call(data);
                        modal_open.set(false);
                    })
                },
                on_close: Callback::new(move |_| modal_open.set(false)),
            }
        }
    }
}

/// Node property panel — shows editable properties for the selected node/module.
///
/// Standalone dock panel wrapper around `NodePropertyPanel`. Reads from
/// the rig grid context and `RIG_NODE_GRAPH` global signal.
///
/// Reads from `RIG_SELECTED_ENTITY` and `RIG_NODE_GRAPH` global signals directly.
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

/// Scene grid dock panel — Quad Cortex-style 4x2 scene tile grid.
///
/// Defaults to Song mode. Shows song scenes, profile scene templates,
/// or preset snapshots depending on the view mode.
#[component]
pub fn SceneGridDockPanel() -> Element {
    init_rig_service();
    use_rig_subscription();

    rsx! {
        SceneGridPanel { view_mode: RigViewMode::Song }
    }
}

/// Grid editor dock panel — the unified 2D block/module grid.
///
/// Standalone dock panel that owns its own chain/connections/selection state.
/// Writes the selected slot to the rig grid context so the detail editor
/// panel (in a separate dock tile) can read it.
///
/// Wraps its subtree in a `RigGridStateProvider` for dock-based usage
/// (where `RigLayout` is not the ancestor).
#[component]
pub fn RigGridEditorPanel() -> Element {
    init_rig_service();
    use_rig_subscription();

    rsx! {
        RigGridStateProvider {
            RigGridEditorPanelInner {}
        }
    }
}

/// Inner body of the grid editor panel — reads/writes the rig grid context.
///
/// Reads `RIG_MODULES` to build the composition chain reactively.
/// When modules change (preset loaded, etc.), the grid updates automatically.
#[component]
fn RigGridEditorPanelInner() -> Element {
    use crate::components::module_editor::unified_grid_editor::modules_to_composition_chain;
    use crate::signals::RIG_GRID_CHAIN_OVERRIDE;

    let grid_state = use_rig_grid_state();

    let mut connections = use_signal(Vec::<GridConnection>::new);
    let mut selection = use_signal(|| None::<GridSelection>);

    // Build chain from RIG_MODULES — reactive via GlobalSignal read
    let modules = RIG_MODULES.read();
    let base_chain = modules_to_composition_chain(&modules);
    drop(modules);

    // Track base chain identity so we can clear overrides on preset switch
    let mut prev_slot_ids = use_signal(Vec::<Uuid>::new);
    let current_ids: Vec<Uuid> = base_chain.iter().map(|s| s.id).collect();
    if *prev_slot_ids.read() != current_ids {
        prev_slot_ids.set(current_ids);
        if RIG_GRID_CHAIN_OVERRIDE.read().is_some() {
            *RIG_GRID_CHAIN_OVERRIDE.write() = None;
        }
    }

    // Use the global chain override if present, otherwise use computed chain
    let chain_data = RIG_GRID_CHAIN_OVERRIDE.read().clone().unwrap_or(base_chain);

    let conn_data = connections.cloned();
    let sel = selection.cloned();

    // Keep the global signals in sync with local selection so that
    // RigDetailEditorPanel (in a separate dock tile) can read it.
    {
        let chain_for_effect = chain_data.clone();
        use_effect(move || {
            let sel = selection.cloned();
            let slot = match &sel {
                Some(GridSelection::Block(id)) => {
                    chain_for_effect.iter().find(|s| s.id == *id).cloned()
                }
                _ => None,
            };
            *RIG_GRID_SELECTED_SLOT.write() = slot.clone();
            *RIG_GRID_SELECTION.write() = sel;
            grid_state.set_selected_slot(slot);
        });
    }

    rsx! {
        div { class: "h-full w-full relative overflow-hidden",
            DynamicGridView {
                chain: chain_data.clone(),
                selection: sel.clone(),
                connections: conn_data.clone(),
                on_chain_change: move |new_chain: Vec<CompositionSlot>| {
                    *RIG_GRID_CHAIN_OVERRIDE.write() = Some(new_chain);
                },
                on_connections_change: move |new_conns: Vec<GridConnection>| {
                    connections.set(new_conns);
                },
                on_select: move |s: Option<GridSelection>| {
                    tracing::info!("RigGridEditorPanelInner on_select: {:?}", s);
                    selection.set(s);
                },
                on_group_reorder: move |(from_name, to_name): (String, String)| {
                    let mut modules = RIG_MODULES.write();
                    let from_idx = modules.iter().position(|m| m.module_type.display_name() == from_name);
                    let to_idx = modules.iter().position(|m| m.module_type.display_name() == to_name);
                    if let (Some(a), Some(b)) = (from_idx, to_idx) {
                        modules.swap(a, b);
                    }
                },
                on_bypass_toggle: move |(sel, bypassed): (GridSelection, bool)| {
                    let mut modules = RIG_MODULES.write();
                    match sel {
                        GridSelection::Block(id) => {
                            for m in modules.iter_mut() {
                                if let Some(mb) = m.blocks.iter_mut().find(|b| b.id.as_uuid() == id) {
                                    mb.block.bypassed = bypassed;
                                    break;
                                }
                            }
                        }
                        GridSelection::Module(ref name) => {
                            if let Some(m) = modules.iter_mut().find(|m| m.module_type.display_name() == name) {
                                for mb in m.blocks.iter_mut() {
                                    mb.block.bypassed = bypassed;
                                }
                            }
                        }
                    }
                },
            }

            // Block picker portal — rendered above CSS transform stacking context
            if let Some((pc, pr)) = *PICKER_CELL.read() {
                {
                    let (click_x, click_y) = *PICKER_CLICK_POS.read();
                    rsx! {
                        BlockPickerDropdown {
                            col: pc,
                            row: pr,
                            click_x: click_x,
                            click_y: click_y,
                            on_add_slot: move |_new_slot: CompositionSlot| {
                                *PICKER_CELL.write() = None;
                            },
                            on_close: move |_| {
                                *PICKER_CELL.write() = None;
                            },
                        }
                    }
                }
            }
        }
    }
}

/// Detail editor dock panel — 3-column detail view for the selected block/module.
///
/// Reads the selection from `RIG_GRID_SELECTION` and the full composition chain
/// from `RIG_MODULES` (with `RIG_GRID_CHAIN_OVERRIDE` applied when present).
/// Uses global signals directly because dock panels are rendered in separate
/// subtrees without a shared context provider.
#[component]
pub fn RigDetailEditorPanel() -> Element {
    use crate::components::module_editor::unified_grid_editor::modules_to_composition_chain;
    use crate::signals::RIG_GRID_CHAIN_OVERRIDE;
    init_rig_service();
    use_rig_subscription();

    let sel = RIG_GRID_SELECTION.read().clone();
    let modules = RIG_MODULES.read();
    let base_chain = modules_to_composition_chain(&modules);
    drop(modules);

    let chain = RIG_GRID_CHAIN_OVERRIDE
        .read()
        .clone()
        .unwrap_or(base_chain.clone());

    let chain_for_cb = chain.clone();

    rsx! {
        div { class: "h-full w-full overflow-hidden bg-zinc-950/40",
            DetailPanel {
                selection: sel,
                chain: chain,
                on_preset_assigned: move |(slot_id, preset_id, preset_name): (Uuid, Uuid, String)| {
                    let mut new_chain = chain_for_cb.clone();
                    if let Some(slot) = new_chain.iter_mut().find(|s| s.id == slot_id) {
                        slot.block_preset_id = Some(preset_id);
                        slot.block_preset_name = Some(preset_name);
                        slot.is_template = false;
                    }
                    *RIG_GRID_CHAIN_OVERRIDE.write() = Some(new_chain);
                },
            }
        }
    }
}
