//! Dock layout global signals.
//!
//! Global Dioxus signals for dock state. Components subscribe to these
//! for reactive layout updates.

use crate::prelude::*;
use dock_proto::{DockLayout, DropZone, NodeId, PanelId, PresetCollection};

/// The current active dock layout.
pub static DOCK_LAYOUT: GlobalSignal<DockLayout> =
    Signal::global(|| DockLayout::single(PanelId::Performance));

/// All available dock presets (screensets).
pub static DOCK_PRESETS: GlobalSignal<PresetCollection> =
    Signal::global(|| PresetCollection::new(Vec::new()));

/// Index of the currently active preset.
pub static DOCK_ACTIVE_PRESET_INDEX: GlobalSignal<usize> = Signal::global(|| 0);

/// Node ID of the split currently being resized (drag in progress).
pub static DOCK_RESIZING: GlobalSignal<Option<NodeId>> = Signal::global(|| None);

/// Panel that is currently maximized (takes full area), if any.
pub static DOCK_MAXIMIZED_PANEL: GlobalSignal<Option<PanelId>> = Signal::global(|| None);

/// State for drag-and-drop panel rearrangement.
pub static DOCK_DRAG_STATE: GlobalSignal<DragState> = Signal::global(DragState::default);

/// Tracks an in-progress panel drag operation.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct DragState {
    /// The panel currently being dragged, if any.
    pub dragging_panel: Option<PanelId>,
    /// The source node ID the panel is being dragged from.
    pub source_node: Option<NodeId>,
    /// The node currently being hovered over for a drop.
    pub hover_target: Option<NodeId>,
    /// Which drop zone the cursor is in on the hover target.
    pub hover_zone: Option<DropZone>,
}

impl DragState {
    /// Start dragging a panel from a specific node.
    pub fn start(&mut self, panel: PanelId, source: NodeId) {
        self.dragging_panel = Some(panel);
        self.source_node = Some(source);
        self.hover_target = None;
        self.hover_zone = None;
    }

    /// Update hover target and zone during drag.
    pub fn update_hover(&mut self, target: NodeId, zone: DropZone) {
        self.hover_target = Some(target);
        self.hover_zone = Some(zone);
    }

    /// Clear hover (cursor left a tile area).
    pub fn clear_hover(&mut self) {
        self.hover_target = None;
        self.hover_zone = None;
    }

    /// End the drag operation.
    pub fn end(&mut self) {
        *self = Self::default();
    }

    /// Whether a drag is currently in progress.
    pub fn is_dragging(&self) -> bool {
        self.dragging_panel.is_some()
    }
}
