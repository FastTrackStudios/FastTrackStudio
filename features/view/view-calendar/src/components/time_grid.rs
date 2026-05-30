//! Shared time-grid view used by week (`days.len() == 7`) and day
//! (`days.len() == 1`) variants.
//!
//! Pixel-based vertical layout (48 px/hour) so drop-y math is
//! direct. Overlap column-splitting comes from
//! [`crate::layout::day_overlap_layout`]: events that share time
//! get split into sub-columns. Sweep-to-create on the column
//! background gives the Google-style click-and-drag event creation.

use chrono::{Datelike, Duration, NaiveDate};
use dioxus::html::input_data::MouseButton;
use dioxus::prelude::*;

use crate::layout::{TimeBlockPlacement, day_overlap_layout};
use crate::store::CalendarMutation;
use crate::time::{day_start_utc, hour_labels};
use crate::types::{CalendarEvent, EventId, TemplateBlock};

use super::all_day_strip::AllDayStrip;
use super::drag::{
    BlockDrag, BlockDragKind, DRAG_THRESHOLD_PX, DragKind, Ghost, use_block_drag_context,
    use_drag_context,
};
use super::event_chip::EventChip;
use super::now_line::NowLine;
use super::style::{chip_palette, template_palette};

const PX_PER_HOUR: i64 = 48;
const COL_HEIGHT_PX: i64 = PX_PER_HOUR * 24;
const SNAP_MINUTES: i64 = 15;

#[derive(Props, Clone, PartialEq)]
pub struct TimeGridViewProps {
    pub days: Vec<NaiveDate>,
    pub events: Vec<CalendarEvent>,
    /// Faded day-plan blocks drawn behind real events.
    #[props(default)]
    pub template_blocks: Vec<TemplateBlock>,
    /// Fired with `(date, block_id)` when a plan block is clicked.
    #[props(default)]
    pub on_block_click: Option<EventHandler<(NaiveDate, String)>>,
    /// Fired with `(date, block_id, payload)` on a drop onto a block.
    #[props(default)]
    pub on_block_drop: Option<EventHandler<(NaiveDate, String, String)>>,
    /// Fired with `(date, block_id, start_min, end_min)` on a block drag.
    #[props(default)]
    pub on_block_edit: Option<EventHandler<crate::types::BlockEdit>>,
    #[props(default = false)]
    pub readonly: bool,
    pub on_event: EventHandler<CalendarMutation>,
    pub on_open_editor: EventHandler<EventId>,
}

#[component]
pub fn TimeGridView(props: TimeGridViewProps) -> Element {
    let today = chrono::Local::now().date_naive();
    // Split: all-day events go to the top strip, timed events to
    // the time grid itself. `position` would mis-classify multi-day
    // timed events; we treat duration ≥ 24h with midnight-aligned
    // start as effectively all-day too, so a "Vacation" event the
    // user created via month view (start at 00:00, end exclusive)
    // also shows in the all-day strip.
    let (all_day_events, timed_events): (Vec<_>, Vec<_>) = props
        .events
        .iter()
        .cloned()
        .partition(|ev| ev.all_day || is_effectively_all_day(ev));

    rsx! {
        div { class: "flex flex-col h-full w-full",
            // Day header strip
            div { class: "grid border-b border-border/40",
                style: "grid-template-columns: 56px repeat({props.days.len()}, 1fr);",
                div {}
                for date in props.days.iter() {
                    {
                        let is_today = *date == today;
                        let day_name = date.format("%a").to_string();
                        let day_num = date.day();
                        let header_cycle_kind = crate::cycle_overlay::kind_for_date(*date);
                        let header_tint = if is_today {
                            "bg-primary/[0.04]"
                        } else {
                            crate::cycle_overlay::cell_tint_class(header_cycle_kind)
                        };
                        let cell_cls = format!(
                            "flex items-center justify-center gap-2 py-1.5 text-xs {header_tint}"
                        );
                        rsx! {
                            div {
                                key: "{date}",
                                class: "{cell_cls}",
                                span {
                                    class: if is_today {
                                        "text-primary font-semibold uppercase tracking-wider text-[10px]"
                                    } else {
                                        "text-muted-foreground uppercase tracking-wider text-[10px]"
                                    },
                                    "{day_name}"
                                }
                                span {
                                    class: if is_today {
                                        "font-semibold bg-primary text-primary-foreground rounded-full w-6 h-6 flex items-center justify-center text-sm"
                                    } else {
                                        "font-medium text-sm"
                                    },
                                    "{day_num}"
                                }
                            }
                        }
                    }
                }
            }
            // All-day strip
            AllDayStrip {
                days: props.days.clone(),
                events: all_day_events,
                readonly: props.readonly,
                on_event: props.on_event,
                on_open_editor: props.on_open_editor,
            }
            // Scrollable grid body
            div { class: "flex-1 min-h-0 overflow-y-auto",
                div {
                    class: "grid relative",
                    style: "grid-template-columns: 56px repeat({props.days.len()}, 1fr); height: {COL_HEIGHT_PX}px;",
                    HourAxis {}
                    for (idx, date) in props.days.iter().enumerate() {
                        DayColumn {
                            key: "{date}",
                            date: *date,
                            placements: day_overlap_layout(*date, &timed_events),
                            template_blocks: props.template_blocks.clone(),
                            on_block_click: props.on_block_click,
                            on_block_drop: props.on_block_drop,
                            on_block_edit: props.on_block_edit,
                            is_last: idx == props.days.len() - 1,
                            readonly: props.readonly,
                            on_event: props.on_event,
                            on_open_editor: props.on_open_editor,
                        }
                    }
                    // Now-line overlay — placed after columns so
                    // it stacks on top.
                    NowLine { days: props.days.clone(), px_per_hour: PX_PER_HOUR }
                }
            }
        }
    }
}

/// Heuristic: a 24h+ duration event starting at midnight is
/// effectively an all-day event regardless of the `all_day` flag.
/// Lets month-view-created "Vacation" events render in the all-day
/// strip without forcing a data migration.
fn is_effectively_all_day(ev: &CalendarEvent) -> bool {
    let dur = (ev.end - ev.start).num_minutes();
    let starts_at_midnight = ev.start.time() == chrono::NaiveTime::MIN;
    dur >= 24 * 60 && starts_at_midnight
}

#[component]
fn HourAxis() -> Element {
    rsx! {
        div { class: "relative border-r border-border/40 text-[10px] text-muted-foreground",
            for (h, label) in hour_labels() {
                div {
                    key: "{h}",
                    class: "absolute right-1 -translate-y-2",
                    style: "top: {h * PX_PER_HOUR as u32}px;",
                    "{label}"
                }
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
struct DayColumnProps {
    date: NaiveDate,
    placements: Vec<TimeBlockPlacement>,
    #[props(default)]
    template_blocks: Vec<TemplateBlock>,
    /// Fired with `(date, block_id)` when a plan block is clicked.
    #[props(default)]
    on_block_click: Option<EventHandler<(NaiveDate, String)>>,
    /// Fired with `(date, block_id, payload)` when something is dropped
    /// on a plan block. `payload` is the dropped item's
    /// [`TASK_DROP_MIME`] data (`"id|title"`).
    #[props(default)]
    on_block_drop: Option<EventHandler<(NaiveDate, String, String)>>,
    /// Fired with `(date, block_id, start_min, end_min)` after a block
    /// is dragged to a new time.
    #[props(default)]
    on_block_edit: Option<EventHandler<crate::types::BlockEdit>>,
    is_last: bool,
    readonly: bool,
    on_event: EventHandler<CalendarMutation>,
    on_open_editor: EventHandler<EventId>,
}

/// `DataTransfer` MIME for dragging a task/project onto a plan block.
/// Payload is `"id|title"`.
pub const TASK_DROP_MIME: &str = "text/x-task-assign";

/// Sweep state: a `(start_min, current_min)` range the user is
/// dragging out with the primary mouse button. While `Some` the
/// column shows a ghost block; on `mouseup` it commits to a
/// `Create` mutation. Clamped to a single day — drag past
/// midnight just sticks at the day edge.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Sweep {
    anchor_min: i64,
    current_min: i64,
}

impl Sweep {
    fn range(self) -> (i64, i64) {
        let a = self.anchor_min.min(self.current_min);
        let b = self.anchor_min.max(self.current_min);
        (
            snap_minutes(a),
            snap_minutes(b).max(snap_minutes(a) + SNAP_MINUTES),
        )
    }
}

#[component]
fn DayColumn(props: DayColumnProps) -> Element {
    let mut ctx = use_drag_context();
    let date = props.date;
    let on_event = props.on_event;
    let on_open_editor = props.on_open_editor;
    let today = chrono::Local::now().date_naive();
    let is_today = date == today;
    let border_r = if props.is_last { "" } else { "border-r" };
    // Cyclic overlay — reset / bonus weeks paint the column.
    // Today's primary tint wins over the cycle tint.
    let cycle_kind = crate::cycle_overlay::kind_for_date(date);
    let column_bg = if is_today {
        "bg-primary/[0.04]"
    } else {
        crate::cycle_overlay::cell_tint_class(cycle_kind)
    };
    let mut sweep: Signal<Option<Sweep>> = use_signal(|| None);

    // Day-plan blocks for this column's date, pre-resolved to pixel
    // geometry. Faded, clickable placement guides.
    struct TplPlacement {
        id: String,
        start_min: i64,
        end_min: i64,
        top: i64,
        h: i64,
        pal: &'static str,
        label: String,
        assignment: Option<String>,
    }
    let template_placements: Vec<TplPlacement> = props
        .template_blocks
        .iter()
        .filter(|tb| tb.date == date && tb.end_min > tb.start_min)
        .map(|tb| TplPlacement {
            id: tb.id.clone(),
            start_min: i64::from(tb.start_min),
            end_min: i64::from(tb.end_min),
            top: i64::from(tb.start_min) * PX_PER_HOUR / 60,
            h: i64::from(tb.end_min - tb.start_min) * PX_PER_HOUR / 60,
            pal: template_palette(tb.color),
            label: tb.label.clone(),
            assignment: tb.assignment.clone(),
        })
        .collect();
    let on_block_click = props.on_block_click;
    let on_block_drop = props.on_block_drop;
    let on_block_edit = props.on_block_edit;
    let mut block_ctx = use_block_drag_context();
    // The drag whose *target* day is this column (drives the ghost).
    let active_block = block_ctx
        .drag
        .read()
        .as_ref()
        .filter(|d| d.date == date && d.committed)
        .cloned();
    // The id of the block whose *origin* day is this column — dim it
    // while a (possibly cross-day) drag of it is in flight.
    let dragging_orig_id = block_ctx
        .drag
        .read()
        .as_ref()
        .filter(|d| d.orig_date == date && d.committed)
        .map(|d| d.block_id.clone());

    rsx! {
        div {
            class: "relative border-border/40 {border_r} {column_bg}",
            // Plan-block drag — handled at the column level so moves
            // bubble here regardless of which child the pointer is over.
            onpointermove: move |e: Event<PointerData>| {
                let snap = block_ctx.drag.peek().clone();
                let Some(mut bd) = snap else { return };
                // Resize stays on the block's own day; only a Move roams
                // across columns.
                let is_resize = !matches!(bd.kind, BlockDragKind::Move);
                if is_resize && bd.orig_date != date {
                    return;
                }
                let page_y = e.data().page_coordinates().y;
                if !bd.committed {
                    if (page_y - bd.start_page_y).abs() < DRAG_THRESHOLD_PX as f64 {
                        return;
                    }
                    bd.committed = true;
                }
                let col_y = e.data().element_coordinates().y as i64;
                let pointer_min = px_to_minutes(col_y);
                match bd.kind {
                    BlockDragKind::Move => {
                        bd.date = date; // follow the cursor's column
                        let dur = bd.orig_end_min - bd.orig_start_min;
                        let s = snap_minutes(pointer_min - bd.grab_offset_min)
                            .clamp(0, (1440 - dur).max(0));
                        bd.cur_start_min = s;
                        bd.cur_end_min = s + dur;
                    }
                    BlockDragKind::ResizeEnd => {
                        bd.cur_start_min = bd.orig_start_min;
                        bd.cur_end_min =
                            snap_minutes(pointer_min).clamp(bd.orig_start_min + SNAP_MINUTES, 1440);
                    }
                    BlockDragKind::ResizeStart => {
                        bd.cur_start_min =
                            snap_minutes(pointer_min).clamp(0, bd.orig_end_min - SNAP_MINUTES);
                        bd.cur_end_min = bd.orig_end_min;
                    }
                }
                block_ctx.drag.set(Some(bd));
            },
            // Hour grid lines (solid).
            for h in 1..24u32 {
                div {
                    key: "h-{h}",
                    class: "absolute left-0 right-0 border-t border-border/30 pointer-events-none",
                    style: "top: {h as i64 * PX_PER_HOUR}px;",
                }
            }
            // Half-hour grid lines (dashed, very faint) — improves
            // legibility for 30/15-minute events.
            for h in 0..24u32 {
                div {
                    key: "h2-{h}",
                    class: "absolute left-0 right-0 border-t border-dashed border-border/15 pointer-events-none",
                    style: "top: {h as i64 * PX_PER_HOUR + PX_PER_HOUR / 2}px;",
                }
            }
            // Background surface — captures all pointer-based input
            // for this column (drag-tracking + sweep-to-create).
            // Sits BELOW the event blocks so chips win the initial
            // pointerdown; the chip body sets `pointer-events: none`
            // while it is being dragged so subsequent pointermoves
            // fall through to this surface as the user drags
            // across.
            div {
                class: "absolute inset-0",
                style: "touch-action: none;",
                // Snap-preview during an active drag. The drag isn't
                // "committed" until the pointer has moved past the
                // threshold — a pure click leaves DragState in place
                // but never updates the ghost, so onclick on the
                // chip wins.
                onpointermove: move |e: Event<PointerData>| {
                    if props.readonly { return; }
                    let snapshot = ctx.state.peek().clone();
                    let Some(mut ds) = snapshot else { return };
                    if !ds.committed {
                        let p = e.data().page_coordinates();
                        let dx = (p.x - ds.start_page_x).abs();
                        let dy = (p.y - ds.start_page_y).abs();
                        if dx < super::drag::DRAG_THRESHOLD_PX as f64
                            && dy < super::drag::DRAG_THRESHOLD_PX as f64
                        {
                            return;
                        }
                        ds.committed = true;
                        ctx.state.set(Some(ds.clone()));
                    }
                    let y = e.data().element_coordinates().y as i64;
                    let snap_min = snap_minutes(px_to_minutes(y));
                    let (start_min, end_min) = ghost_range(ds.kind, ds.orig_start, ds.orig_end, snap_min, date);
                    ctx.ghost.set(Some(Ghost {
                        event: ds.event,
                        date,
                        start_min,
                        end_min,
                        color: ds.color,
                        title: ds.title.clone(),
                    }));
                },
                // Commit on pointerup. Either commits the drag or
                // commits a sweep-to-create — whichever is active.
                onpointerup: move |e: Event<PointerData>| {
                    if props.readonly { return; }
                    let drag_snapshot = ctx.state.peek().clone();
                    if let Some(ds) = drag_snapshot {
                        let y = e.data().element_coordinates().y as i64;
                        let drop_min = snap_minutes(px_to_minutes(y));
                        match ds.kind {
                            DragKind::Move => {
                                let new_start = day_start_utc(date) + Duration::minutes(drop_min);
                                let duration = ds.orig_end - ds.orig_start;
                                on_event.call(CalendarMutation::Reschedule {
                                    id: ds.event,
                                    start: new_start,
                                    end: new_start + duration,
                                });
                            }
                            DragKind::ResizeEnd => {
                                let origin_min_from_day = (ds.orig_start - day_start_utc(date)).num_minutes();
                                let min_end_min = origin_min_from_day + SNAP_MINUTES;
                                let new_end = day_start_utc(date)
                                    + Duration::minutes(drop_min.max(min_end_min));
                                on_event.call(CalendarMutation::Reschedule {
                                    id: ds.event,
                                    start: ds.orig_start,
                                    end: new_end,
                                });
                            }
                            DragKind::ResizeStart => {
                                let origin_end_min_from_day = (ds.orig_end - day_start_utc(date)).num_minutes();
                                let max_start_min = origin_end_min_from_day - SNAP_MINUTES;
                                let new_start = day_start_utc(date)
                                    + Duration::minutes(drop_min.min(max_start_min));
                                on_event.call(CalendarMutation::Reschedule {
                                    id: ds.event,
                                    start: new_start,
                                    end: ds.orig_end,
                                });
                            }
                        }
                        ctx.state.set(None);
                        ctx.ghost.set(None);
                        return;
                    }
                    // No drag in flight — commit sweep-to-create.
                    let Some(s) = sweep.take() else { return };
                    let (start_min, end_min) = s.range();
                    let start = day_start_utc(date) + Duration::minutes(start_min);
                    let end = day_start_utc(date) + Duration::minutes(end_min);
                    let end = if (end - start).num_minutes() <= SNAP_MINUTES {
                        start + Duration::hours(1)
                    } else {
                        end
                    };
                    let event = CalendarEvent::new("New event", start, end);
                    on_event.call(CalendarMutation::Create { event });
                },
                // Sweep to create — primary button only. Click
                // without sweep falls through to the pointerup
                // branch and produces a 1-hour event at the
                // clicked slot.
                onmousedown: move |e: MouseEvent| {
                    if props.readonly { return; }
                    if e.data().trigger_button() != Some(MouseButton::Primary) { return; }
                    let y = e.data().element_coordinates().y as i64;
                    let anchor = snap_minutes(px_to_minutes(y));
                    sweep.set(Some(Sweep { anchor_min: anchor, current_min: anchor }));
                },
                onmousemove: move |e: MouseEvent| {
                    if props.readonly { return; }
                    let Some(mut s) = *sweep.peek() else { return };
                    let y = e.data().element_coordinates().y as i64;
                    s.current_min = snap_minutes(px_to_minutes(y));
                    sweep.set(Some(s));
                },
                onmouseleave: move |_| {
                    // Bail on the sweep if the pointer leaves the
                    // column — avoids ghost blocks lingering after
                    // a drag escapes the day. Drag-state survives
                    // because it belongs to DragContext, not the
                    // column's local sweep.
                    sweep.set(None);
                },
            }
            // Sweep ghost block.
            if let Some(s) = *sweep.read() {
                {
                    let (a, b) = s.range();
                    let top = minutes_to_px(a);
                    let h = minutes_to_px(b - a).max(2);
                    rsx! {
                        div {
                            class: "absolute left-1 right-1 rounded-sm bg-primary/20 border border-primary/60 pointer-events-none",
                            style: "top: {top}px; height: {h}px;",
                        }
                    }
                }
            }
            // Day-plan blocks — clickable guides above the sweep
            // surface but behind real events. Clicking one asks the
            // consumer to edit that date's plan.
            for tp in template_placements.iter() {
                {
                    let block_id = tp.id.clone();
                    let drop_id = tp.id.clone();
                    let down_id = tp.id.clone();
                    let rs_id = tp.id.clone();
                    let re_id = tp.id.clone();
                    let bstart = tp.start_min;
                    let bend = tp.end_min;
                    let draggable_block = on_block_edit.is_some();
                    let being_dragged = dragging_orig_id.as_deref() == Some(tp.id.as_str());
                    let interactive = on_block_click.is_some() || on_block_drop.is_some() || draggable_block;
                    let cursor = if draggable_block {
                        "cursor-grab active:cursor-grabbing hover:brightness-125"
                    } else if on_block_click.is_some() {
                        "cursor-pointer hover:brightness-125"
                    } else {
                        ""
                    };
                    // While this block is being dragged, dim it + let
                    // pointer events fall through to the column handler.
                    let pe = if interactive && !being_dragged { "" } else { "pointer-events-none" };
                    let dim = if being_dragged { "opacity-30" } else { "" };
                    // Build a drag for this block in `kind` mode.
                    let mk_drag = move |kind: BlockDragKind, id: String, grab: i64, page_y: f64| BlockDrag {
                        block_id: id,
                        kind,
                        orig_date: date,
                        date,
                        orig_start_min: bstart,
                        orig_end_min: bend,
                        grab_offset_min: grab,
                        cur_start_min: bstart,
                        cur_end_min: bend,
                        start_page_y: page_y,
                        committed: false,
                    };
                    rsx! {
                        div {
                            key: "tpl-{tp.id}-{tp.top}",
                            class: "absolute left-0.5 right-0.5 rounded-md border border-dashed overflow-hidden transition {cursor} {pe} {dim} {tp.pal}",
                            style: "top: {tp.top}px; height: {tp.h}px;",
                            onpointerdown: move |e: Event<PointerData>| {
                                if !draggable_block {
                                    return;
                                }
                                let elem_y = e.data().element_coordinates().y as i64;
                                let page_y = e.data().page_coordinates().y;
                                block_ctx.drag.set(Some(mk_drag(
                                    BlockDragKind::Move,
                                    down_id.clone(),
                                    px_to_minutes(elem_y),
                                    page_y,
                                )));
                            },
                            onclick: move |_| {
                                if let Some(cb) = on_block_click {
                                    cb.call((date, block_id.clone()));
                                }
                            },
                            ondragover: move |e: Event<DragData>| {
                                if on_block_drop.is_some() {
                                    e.prevent_default();
                                }
                            },
                            ondrop: move |e: Event<DragData>| {
                                e.prevent_default();
                                if let Some(cb) = on_block_drop {
                                    if let Some(p) = e.data().data_transfer().get_data(TASK_DROP_MIME)
                                    {
                                        if !p.is_empty() {
                                            cb.call((date, drop_id.clone(), p));
                                        }
                                    }
                                }
                            },
                            // Resize handles (top / bottom edges).
                            if draggable_block {
                                div {
                                    class: "absolute inset-x-0 top-0 z-10 h-1.5 cursor-ns-resize",
                                    onpointerdown: move |e: Event<PointerData>| {
                                        e.stop_propagation();
                                        let page_y = e.data().page_coordinates().y;
                                        block_ctx.drag.set(Some(mk_drag(
                                            BlockDragKind::ResizeStart,
                                            rs_id.clone(),
                                            0,
                                            page_y,
                                        )));
                                    },
                                }
                                div {
                                    class: "absolute inset-x-0 bottom-0 z-10 h-1.5 cursor-ns-resize",
                                    onpointerdown: move |e: Event<PointerData>| {
                                        e.stop_propagation();
                                        let page_y = e.data().page_coordinates().y;
                                        block_ctx.drag.set(Some(mk_drag(
                                            BlockDragKind::ResizeEnd,
                                            re_id.clone(),
                                            0,
                                            page_y,
                                        )));
                                    },
                                }
                            }
                            span {
                                class: "block px-1.5 py-0.5 text-[10px] font-medium leading-tight truncate",
                                "{tp.label}"
                            }
                            if let Some(a) = tp.assignment.as_ref() {
                                span {
                                    class: "block px-1.5 text-[10px] leading-tight truncate opacity-80 italic",
                                    "{a}"
                                }
                            }
                        }
                    }
                }
            }
            // Live snapped ghost of the block being dragged.
            if let Some(bd) = active_block.as_ref() {
                {
                    let top = minutes_to_px(bd.cur_start_min);
                    let h = minutes_to_px(bd.cur_end_min - bd.cur_start_min).max(2);
                    rsx! {
                        div {
                            class: "absolute left-0.5 right-0.5 z-20 rounded-md border-2 border-primary/70 bg-primary/15 pointer-events-none",
                            style: "top: {top}px; height: {h}px;",
                        }
                    }
                }
            }
            // Events on top — clickable above the sweep surface.
            for placement in props.placements.iter() {
                {
                    let id = placement.event.id;
                    let style = block_style(placement);
                    let event = placement.event.clone();
                    rsx! {
                        EventChip {
                            key: "{id}",
                            event,
                            position_style: style,
                            readonly: props.readonly,
                            on_click: move |()| on_open_editor.call(id),
                        }
                    }
                }
            }
            // Snapped ghost — only renders when this column is the
            // current drop target.
            if let Some(g) = ctx.ghost.read().clone() {
                if g.date == date {
                    {
                        let palette = chip_palette(g.color);
                        let top = minutes_to_px(g.start_min);
                        let h = minutes_to_px(g.end_min - g.start_min).max(8);
                        let label = format_ghost_label(g.start_min, g.end_min);
                        let body = palette.body;
                        rsx! {
                            div {
                                class: "absolute left-1 right-1 rounded-md pointer-events-none z-30 shadow-lg ring-2 ring-white/30 px-2 py-1 overflow-hidden {body}",
                                style: "top: {top}px; height: {h}px;",
                                div { class: "text-[11px] leading-4 font-semibold truncate", "{g.title}" }
                                div { class: "text-[10px] leading-3 opacity-90 truncate", "{label}" }
                            }
                        }
                    }
                }
            }
        }
    }
}

fn format_ghost_label(start_min: i64, end_min: i64) -> String {
    fn fmt(min: i64) -> String {
        let h = (min / 60).rem_euclid(24);
        let m = min.rem_euclid(60);
        let (h12, suf) = if h == 0 {
            (12, "AM")
        } else if h < 12 {
            (h, "AM")
        } else if h == 12 {
            (12, "PM")
        } else {
            (h - 12, "PM")
        };
        if m == 0 {
            format!("{h12}{suf}")
        } else {
            format!("{h12}:{m:02}{suf}")
        }
    }
    format!("{} – {}", fmt(start_min), fmt(end_min))
}

/// Compute snapped `(start_min, end_min)` for a ghost preview given
/// the current drag kind and where the cursor is pointing
/// (`snap_min` is already 15-min snapped). For Move the drop point
/// becomes the new start; for resize the dragged edge moves and the
/// other edge stays put — clamped so the event keeps at least one
/// snap interval of length.
fn ghost_range(
    kind: DragKind,
    orig_start: chrono::DateTime<chrono::Utc>,
    orig_end: chrono::DateTime<chrono::Utc>,
    snap_min: i64,
    target_date: NaiveDate,
) -> (i64, i64) {
    let orig_start_min_from_day = (orig_start - day_start_utc(target_date)).num_minutes();
    let orig_end_min_from_day = (orig_end - day_start_utc(target_date)).num_minutes();
    let duration = (orig_end - orig_start).num_minutes();
    match kind {
        DragKind::Move => (snap_min, snap_min + duration),
        DragKind::ResizeEnd => {
            let min_end = orig_start_min_from_day + SNAP_MINUTES;
            (orig_start_min_from_day, snap_min.max(min_end))
        }
        DragKind::ResizeStart => {
            let max_start = orig_end_min_from_day - SNAP_MINUTES;
            (snap_min.min(max_start), orig_end_min_from_day)
        }
    }
}

/// Build the CSS `style` string for an event block — vertical
/// position + height come from the placement's `top_min` /
/// `height_min`, horizontal sub-column from `column` /
/// `cluster_size`. Inset by 2px on each side so adjacent columns
/// have a thin gutter (Google's look).
fn block_style(p: &TimeBlockPlacement) -> String {
    let top = minutes_to_px(p.top_min);
    let h = minutes_to_px(p.height_min);
    let width_pct = 100.0_f32 / f32::from(p.cluster_size);
    let left_pct = f32::from(p.column) * width_pct;
    // Inner gutter via percentage subtraction would over-shrink
    // narrow clusters; instead use calc() so each block loses a
    // fixed 3px of effective width regardless of column count.
    format!(
        "top: {top}px; height: {h}px; left: calc({left_pct:.4}% + 2px); width: calc({width_pct:.4}% - 3px);"
    )
}

fn minutes_to_px(min: i64) -> i64 {
    (min * PX_PER_HOUR) / 60
}

fn px_to_minutes(px: i64) -> i64 {
    (px * 60) / PX_PER_HOUR
}

fn snap_minutes(min: i64) -> i64 {
    (min / SNAP_MINUTES) * SNAP_MINUTES
}
