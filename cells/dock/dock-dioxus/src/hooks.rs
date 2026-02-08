//! Hooks for dock layout manipulation.
//!
//! Provides `DockActions` — a collection of callbacks that UI components
//! use to mutate the layout, switch tabs, and manage presets.

use crate::prelude::*;
use crate::signals::*;
use dock_proto::*;

/// Collection of dock action callbacks for UI components.
#[derive(Clone)]
pub struct DockActions {
    pub split_tile: Callback<(NodeId, SplitDirection, PanelId)>,
    pub close_tile: Callback<NodeId>,
    pub update_split_ratio: Callback<(NodeId, f64)>,
    pub switch_tab: Callback<(NodeId, usize)>,
    pub add_tab: Callback<(NodeId, PanelId)>,
    pub remove_tab: Callback<(NodeId, PanelId)>,
    pub load_preset: Callback<usize>,
    pub save_current_preset: Callback<()>,
    pub save_as_new_preset: Callback<String>,
    pub toggle_maximize: Callback<PanelId>,
    pub start_drag: Callback<(PanelId, NodeId)>,
    pub drop_panel: Callback<(NodeId, DropZone)>,
    pub cancel_drag: Callback<()>,
    pub persist_presets: Callback<()>,
}

/// Hook providing dock layout action callbacks.
pub fn use_dock_actions() -> DockActions {
    DockActions {
        split_tile: Callback::new(
            move |(node_id, direction, panel): (NodeId, SplitDirection, PanelId)| {
                let mut layout = DOCK_LAYOUT.write();
                layout.split_tile(node_id, direction, panel, 50.0);
            },
        ),
        close_tile: Callback::new(move |node_id: NodeId| {
            let mut layout = DOCK_LAYOUT.write();
            layout.close_tile(node_id);
        }),
        update_split_ratio: Callback::new(move |(node_id, ratio): (NodeId, f64)| {
            let mut layout = DOCK_LAYOUT.write();
            layout.update_split_ratio(node_id, ratio);
        }),
        switch_tab: Callback::new(move |(node_id, index): (NodeId, usize)| {
            let mut layout = DOCK_LAYOUT.write();
            if let Some(FlatNode::Tile { tabs, .. }) = layout.get_node_mut(node_id) {
                tabs.set_active(index);
            }
        }),
        add_tab: Callback::new(move |(node_id, panel): (NodeId, PanelId)| {
            let mut layout = DOCK_LAYOUT.write();
            if let Some(FlatNode::Tile { tabs, .. }) = layout.get_node_mut(node_id) {
                tabs.add_panel(panel);
            }
        }),
        remove_tab: Callback::new(move |(node_id, panel): (NodeId, PanelId)| {
            let mut layout = DOCK_LAYOUT.write();
            if let Some(FlatNode::Tile { tabs, .. }) = layout.get_node_mut(node_id) {
                tabs.remove_panel(panel);
            }
        }),
        load_preset: Callback::new(move |index: usize| {
            // Auto-save the current layout back into the departing preset
            let current_layout = DOCK_LAYOUT.read().clone();
            let current_index = *DOCK_ACTIVE_PRESET_INDEX.read();
            {
                let mut presets = DOCK_PRESETS.write();
                if let Some(departing) = presets.presets.get_mut(current_index) {
                    departing.layout = current_layout;
                }
            }

            // Load the target preset
            let presets = DOCK_PRESETS.read();
            if let Some(preset) = presets.presets.get(index) {
                *DOCK_LAYOUT.write() = preset.layout.clone();
                *DOCK_ACTIVE_PRESET_INDEX.write() = index;
            }
        }),
        save_current_preset: Callback::new(move |_: ()| {
            let layout = DOCK_LAYOUT.read().clone();
            let mut presets = DOCK_PRESETS.write();
            presets.save_active(layout);
        }),
        save_as_new_preset: Callback::new(move |name: String| {
            let layout = DOCK_LAYOUT.read().clone();
            let preset = DockPreset::new(name, layout);
            let mut presets = DOCK_PRESETS.write();
            presets.add_preset(preset);
            let new_index = presets.presets.len() - 1;
            presets.set_active(new_index);
            *DOCK_ACTIVE_PRESET_INDEX.write() = new_index;
        }),
        toggle_maximize: Callback::new(move |panel: PanelId| {
            let current = *DOCK_MAXIMIZED_PANEL.read();
            if current == Some(panel) {
                *DOCK_MAXIMIZED_PANEL.write() = None;
            } else {
                *DOCK_MAXIMIZED_PANEL.write() = Some(panel);
            }
        }),
        start_drag: Callback::new(move |(panel, source): (PanelId, NodeId)| {
            DOCK_DRAG_STATE.write().start(panel, source);
        }),
        drop_panel: Callback::new(move |(target, zone): (NodeId, DropZone)| {
            let drag = DOCK_DRAG_STATE.read().clone();
            if let Some(panel) = drag.dragging_panel {
                let mut layout = DOCK_LAYOUT.write();
                layout.move_panel(panel, target, zone);
            }
            DOCK_DRAG_STATE.write().end();
        }),
        cancel_drag: Callback::new(move |_: ()| {
            DOCK_DRAG_STATE.write().end();
        }),
        persist_presets: Callback::new(move |_: ()| {
            let presets = DOCK_PRESETS.read();
            if let Some(path) = dock_proto::persistence::default_presets_path() {
                if let Err(e) = dock_proto::save_presets_to_file(&presets, &path) {
                    tracing::warn!("Failed to save dock presets: {e}");
                }
            }
        }),
    }
}

/// Initialize the dock system with default or persisted presets.
///
/// Call this once at app startup. It tries to load saved presets
/// from disk, falling back to built-in defaults.
pub fn init_dock_presets() {
    let presets = if let Some(path) = dock_proto::persistence::default_presets_path() {
        match dock_proto::load_presets_from_file(&path) {
            Ok(Some(saved)) => saved,
            _ => dock_proto::default_presets(),
        }
    } else {
        dock_proto::default_presets()
    };

    // Load the first preset's layout as the active layout
    if let Some(first) = presets.presets.first() {
        *DOCK_LAYOUT.write() = first.layout.clone();
    }
    *DOCK_PRESETS.write() = presets;
    *DOCK_ACTIVE_PRESET_INDEX.write() = 0;
}
