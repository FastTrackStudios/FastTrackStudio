//! Interaction state types for the grid view — drag, pan, wire draft, selection.
//!
//! `InteractionMode` is the central state machine: exactly one interaction
//! at a time. Mirrors the `DragMode` pattern from `node_graph_view`.

use crate::prelude::*;
use uuid::Uuid;

// ─────────────────────────────────────────────────────────────────────────────
// Drag state
// ─────────────────────────────────────────────────────────────────────────────

/// Tracks an in-progress block drag on the grid.
#[derive(Debug, Clone, PartialEq)]
pub(super) struct GridDragState {
    /// Which slot is being dragged.
    pub(super) slot_id: Uuid,
    /// Original grid position before drag started.
    pub(super) origin_col: usize,
    pub(super) origin_row: usize,
    /// Mouse position at drag start (client coords).
    pub(super) start_mouse_x: f64,
    pub(super) start_mouse_y: f64,
    /// Current mouse position (client coords).
    pub(super) mouse_x: f64,
    pub(super) mouse_y: f64,
}

/// Tracks an in-progress wire connection from a port.
#[derive(Debug, Clone, PartialEq)]
pub(super) struct GridWireDraft {
    /// Which slot the wire originates from.
    pub(super) from_slot_id: Uuid,
    /// Pixel position of the source port center (in grid-local coords).
    pub(super) from_pos: (f64, f64),
    /// Whether the draft started from an output (right) port.
    pub(super) is_from_output: bool,
    /// Current mouse position in grid-local coords.
    pub(super) mouse_pos: (f64, f64),
}

/// Tracks an in-progress module group drag on the grid.
#[derive(Debug, Clone, PartialEq)]
pub(super) struct GroupDragState {
    /// Name of the module group being dragged.
    pub(super) group_name: String,
    /// Mouse position at drag start (client coords).
    pub(super) start_mouse_x: f64,
    pub(super) start_mouse_y: f64,
    /// Current mouse position (client coords).
    pub(super) mouse_x: f64,
    pub(super) mouse_y: f64,
    /// Whether shift is held (insert mode — shifts blocks instead of swapping).
    pub(super) shift_held: bool,
}

/// Where a dragged group would land if dropped.
#[derive(Debug, Clone, PartialEq)]
pub(super) enum GroupDropTarget {
    /// Swap with another module group.
    SwapWith(String),
    /// Move by a grid-cell delta (col_delta, row_delta) — always grid-aligned.
    MoveDelta(isize, isize),
}

// ─────────────────────────────────────────────────────────────────────────────
// Interaction mode (replaces 4 separate Option<T> signals)
// ─────────────────────────────────────────────────────────────────────────────

/// Exactly one interaction at a time. The type system guarantees you can't be
/// panning AND dragging a block simultaneously.
#[derive(Debug, Clone, PartialEq)]
pub(super) enum InteractionMode {
    /// No interaction in progress.
    Idle,
    /// Panning the canvas (middle-click drag).
    Pan {
        start_mouse_x: f64,
        start_mouse_y: f64,
        start_pan_x: f64,
        start_pan_y: f64,
    },
    /// Dragging a single block cell.
    BlockDrag(GridDragState),
    /// Dragging a module group by its title bar.
    GroupDrag(GroupDragState),
    /// Drawing a wire from a port.
    WireDraft(GridWireDraft),
}

impl InteractionMode {
    /// True if no interaction is in progress.
    pub fn is_idle(&self) -> bool {
        matches!(self, Self::Idle)
    }

    /// True if any drag/pan/wire is in progress (used for cursor + empty cell styling).
    pub fn is_any_drag(&self) -> bool {
        !matches!(self, Self::Idle)
    }

    /// The slot ID being dragged, if in `BlockDrag` mode.
    pub fn dragged_slot_id(&self) -> Option<Uuid> {
        match self {
            Self::BlockDrag(d) => Some(d.slot_id),
            _ => None,
        }
    }

    /// The group drag state, if in `GroupDrag` mode.
    pub fn group_drag(&self) -> Option<&GroupDragState> {
        match self {
            Self::GroupDrag(gd) => Some(gd),
            _ => None,
        }
    }

    /// The wire draft state, if in `WireDraft` mode.
    pub fn wire_draft(&self) -> Option<&GridWireDraft> {
        match self {
            Self::WireDraft(wd) => Some(wd),
            _ => None,
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Selection and connections (pub(crate) — used by rig_layout and other panels)
// ─────────────────────────────────────────────────────────────────────────────

/// An explicit connection between two blocks.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct GridConnection {
    pub from_slot_id: Uuid,
    pub to_slot_id: Uuid,
}

/// Explicit connections between blocks. When empty, cables fall back to
/// adjacency-based auto-wiring via `resolve_cables()`.
pub(crate) static GRID_CONNECTIONS: GlobalSignal<Vec<GridConnection>> = Signal::global(Vec::new);

/// What is currently selected in the grid — a single block or an entire module.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum GridSelection {
    /// An individual block (by slot ID).
    Block(Uuid),
    /// A module group (by group name).
    Module(String),
}

// ─────────────────────────────────────────────────────────────────────────────
// Block picker portal state (hoisted above the transform chain)
// ─────────────────────────────────────────────────────────────────────────────

/// Which grid cell (col, row) has the block picker open.
/// Set by DynamicGridView, rendered by the parent component (module_editor_view)
/// to escape the CSS transform stacking context.
pub(crate) static PICKER_CELL: GlobalSignal<Option<(usize, usize)>> = Signal::global(|| None);

/// Click position (client coords) for positioning the picker dropdown.
pub(crate) static PICKER_CLICK_POS: GlobalSignal<(f64, f64)> = Signal::global(|| (0.0, 0.0));
