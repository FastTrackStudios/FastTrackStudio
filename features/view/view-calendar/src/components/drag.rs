//! Drag state shared across views.
//!
//! Three drag modes:
//! - **Move**: whole event slides in time.
//! - **ResizeStart**: only the top edge moves. Emits
//!   `Reschedule { start: new, end: unchanged }`.
//! - **ResizeEnd**: only the bottom edge moves. Emits
//!   `Reschedule { start: unchanged, end: new }`.
//!
//! Two signals:
//! - [`DragState`] — set at drag-start, cleared at drop/end. Tells
//!   chips whether they're the dragged event (for opacity tricks)
//!   and gives handlers a stable origin to compute deltas from.
//! - [`Ghost`] — snapped preview of where the drag *will land*,
//!   updated every `ondragover` frame. Drives the live block we
//!   render at the snapped position. Without this, the native HTML5
//!   drag image is the only visual feedback and it doesn't snap.

use chrono::{DateTime, NaiveDate, Utc};
use dioxus::prelude::*;

use crate::types::{ColorTag, EventId};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DragKind {
    Move,
    ResizeStart,
    ResizeEnd,
}

/// What the user is currently dragging. The `orig_start` /
/// `orig_end` snapshot is taken at drag-start so handlers can
/// compute deltas from a stable origin instead of relying on each
/// frame to read the latest state. `color` + `title` are carried so
/// the live ghost block can render without re-looking-up the event.
///
/// `committed` flips from `false` to `true` the first time the
/// pointer moves past the drag threshold. Until then the
/// pointerdown is treated as a click — the chip doesn't fade and
/// the root pointerup leaves the event alone, so the onclick
/// handler can fire and open the editor.
#[derive(Clone, Debug, PartialEq)]
pub struct DragState {
    pub event: EventId,
    pub kind: DragKind,
    pub orig_start: DateTime<Utc>,
    pub orig_end: DateTime<Utc>,
    pub color: ColorTag,
    pub title: String,
    /// Page x/y at pointerdown — used to compute movement delta for
    /// the commit threshold.
    pub start_page_x: f64,
    pub start_page_y: f64,
    pub committed: bool,
}

/// Pixel movement required before a pointerdown becomes a drag.
/// Matches the gantt drag handling.
pub(crate) const DRAG_THRESHOLD_PX: i64 = 4;

/// Snapped preview of the drag's current target. Re-computed every
/// `ondragover` tick by the column under the cursor.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ghost {
    pub event: EventId,
    pub date: NaiveDate,
    /// Minutes from the start of `date` (UTC).
    pub start_min: i64,
    pub end_min: i64,
    pub color: ColorTag,
    pub title: String,
}

#[derive(Clone, Copy)]
pub struct DragContext {
    pub state: Signal<Option<DragState>>,
    pub ghost: Signal<Option<Ghost>>,
}

pub fn use_drag_context() -> DragContext {
    use_context::<DragContext>()
}

/// An in-flight drag of a *plan block* (the day-plan overlay), separate
/// from event drags. Move-only for now: the whole block slides in time
/// within its day. `cur_*` track the snapped live position; `committed`
/// flips once the pointer moves past the threshold so a plain click
/// still opens the block editor.
#[derive(Clone, Debug, PartialEq)]
pub struct BlockDrag {
    pub block_id: String,
    pub date: NaiveDate,
    pub orig_start_min: i64,
    pub orig_end_min: i64,
    /// Minutes between the block's start and where the user grabbed it,
    /// so the block doesn't jump to align its top with the cursor.
    pub grab_offset_min: i64,
    pub cur_start_min: i64,
    pub cur_end_min: i64,
    pub start_page_y: f64,
    pub committed: bool,
}

#[derive(Clone, Copy)]
pub struct BlockDragContext {
    pub drag: Signal<Option<BlockDrag>>,
}

pub fn use_block_drag_context() -> BlockDragContext {
    use_context::<BlockDragContext>()
}

/// MIME used by HTML5 `DataTransfer` for cross-view drag carrying
/// the event id. Same pattern as `view-kanban`.
pub(crate) const DT_MIME: &str = "text/x-calendar-event-id";

/// Install (once) a document-level dragstart listener that
/// suppresses the browser's native cursor-following preview for any
/// element marked `data-cal-drag`. Without this the native ghost
/// image floats around in addition to our snapped block, which is
/// confusing.
///
/// The trick: pass a 1×1 transparent PNG to
/// `DataTransfer.setDragImage` to displace the native preview.
pub(crate) fn install_drag_image_suppressor() {
    let _ = dioxus::document::eval(
        r"
        if (!window.__dxCalDragImgSetup) {
            window.__dxCalDragImgSetup = true;
            const img = new Image();
            img.src = 'data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7';
            document.addEventListener('dragstart', (e) => {
                const t = e.target;
                if (t && t.closest && t.closest('[data-cal-drag]') && e.dataTransfer) {
                    e.dataTransfer.setDragImage(img, 0, 0);
                }
            }, true);
        }
        ",
    );
}
