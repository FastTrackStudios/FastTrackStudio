//! Drag state shared across views.
//!
//! Two drag modes:
//! - **Move**: whole event slides in time. Month view = whole days,
//!   week/day = minute-granular along the y axis.
//! - **Resize**: only the bottom edge of the event moves. Drag emits
//!   `Reschedule { start: unchanged, end: new }`.

use chrono::{DateTime, Utc};
use dioxus::prelude::*;

use crate::types::EventId;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DragKind {
    Move,
    ResizeEnd,
}

/// What the user is currently dragging. The `orig_start` /
/// `orig_end` snapshot is taken at drag-start so handlers can
/// compute deltas from a stable origin instead of relying on each
/// frame to read the latest state.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DragState {
    pub event: EventId,
    pub kind: DragKind,
    pub orig_start: DateTime<Utc>,
    pub orig_end: DateTime<Utc>,
}

#[derive(Clone, Copy)]
pub struct DragContext {
    pub state: Signal<Option<DragState>>,
}

pub fn use_drag_context() -> DragContext {
    use_context::<DragContext>()
}

/// MIME used by HTML5 `DataTransfer` for cross-view drag carrying
/// the event id. Same pattern as `view-kanban`.
pub(crate) const DT_MIME: &str = "text/x-calendar-event-id";
