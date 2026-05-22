//! Month view — 6×7 day grid with horizontal event chips.
//!
//! Cross-day chip spanning (Google's connected pill) is deferred:
//! v1 renders an event in every day-cell it overlaps as a separate
//! Bar chip. Click empty space in a cell to create a 1-hour event
//! starting at 9 AM that day; drop a dragged chip to reschedule
//! preserving duration.

use chrono::{Datelike, NaiveDate};
use dioxus::prelude::*;
use uuid::Uuid;

use crate::store::CalendarMutation;
use crate::time::{day_end_utc, day_start_utc, month_grid, shift_days};
use crate::types::{CalendarEvent, EventId};

use super::drag::{DT_MIME, use_drag_context};
use super::event_chip::{ChipShape, EventChip};

#[derive(Props, Clone, PartialEq)]
pub struct MonthViewProps {
    /// Any date inside the month to render.
    pub anchor: NaiveDate,
    pub events: Vec<CalendarEvent>,
    #[props(default = false)]
    pub readonly: bool,
    pub on_event: EventHandler<CalendarMutation>,
    pub on_open_editor: EventHandler<EventId>,
}

#[component]
pub fn MonthView(props: MonthViewProps) -> Element {
    let grid = month_grid(props.anchor);
    let cur_month = props.anchor.month();
    let today = chrono::Local::now().date_naive();

    let weekday_labels = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"];

    rsx! {
        div { class: "flex flex-col h-full w-full",
            // Weekday header
            div { class: "grid grid-cols-7 border-b border-border/40 text-xs text-muted-foreground",
                for label in weekday_labels {
                    div { key: "{label}", class: "px-2 py-1 text-center font-medium", "{label}" }
                }
            }
            // 6×7 grid
            div { class: "grid grid-cols-7 grid-rows-6 flex-1 min-h-0",
                for (row_idx, row) in grid.iter().enumerate() {
                    for (col_idx, date) in row.iter().enumerate() {
                        DayCell {
                            key: "{date}",
                            date: *date,
                            is_other_month: date.month() != cur_month,
                            is_today: *date == today,
                            is_last_row: row_idx == 5,
                            is_last_col: col_idx == 6,
                            events: cell_events(&props.events, *date),
                            readonly: props.readonly,
                            on_event: props.on_event,
                            on_open_editor: props.on_open_editor,
                        }
                    }
                }
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
struct DayCellProps {
    date: NaiveDate,
    is_other_month: bool,
    is_today: bool,
    is_last_row: bool,
    is_last_col: bool,
    events: Vec<CalendarEvent>,
    readonly: bool,
    on_event: EventHandler<CalendarMutation>,
    on_open_editor: EventHandler<EventId>,
}

#[component]
fn DayCell(props: DayCellProps) -> Element {
    let ctx = use_drag_context();
    let date = props.date;
    let on_event = props.on_event;
    let on_open_editor = props.on_open_editor;

    let mut bg = if props.is_other_month {
        "bg-background/40 text-muted-foreground"
    } else {
        "bg-background"
    };
    if props.is_today {
        bg = "bg-primary/10";
    }
    let border_r = if props.is_last_col { "" } else { "border-r" };
    let border_b = if props.is_last_row { "" } else { "border-b" };

    rsx! {
        div {
            class: "min-h-0 overflow-hidden flex flex-col gap-0.5 p-1 border-border/40 {bg} {border_r} {border_b}",
            // Drop target: shift dragged event so its `start` lands
            // on this date (preserving duration).
            ondragover: move |e: Event<DragData>| {
                if props.readonly { return; }
                if ctx.state.peek().is_none() { return; }
                e.prevent_default();
            },
            ondrop: move |e: Event<DragData>| {
                if props.readonly { return; }
                e.prevent_default();
                let dt = e.data().data_transfer();
                let Ok(id) = dt.get_data(DT_MIME).unwrap_or_default().parse::<Uuid>() else { return };
                let snapshot = ctx.state.peek().clone();
                let Some(ds) = snapshot else { return };
                if ds.event != id { return; }
                // Compute day delta from the dragged event's
                // original start to the cell's date.
                let orig_day = ds.orig_start.date_naive();
                let delta = (date - orig_day).num_days();
                let (start, end) = shift_days(ds.orig_start, ds.orig_end, delta);
                on_event.call(CalendarMutation::Reschedule { id, start, end });
            },
            // Click empty area = create. Click on a chip bubbles
            // up but the chip's `onclick` calls stop_propagation.
            onclick: move |_| {
                if props.readonly { return; }
                let start = day_start_utc(date) + chrono::Duration::hours(9);
                let end = start + chrono::Duration::hours(1);
                let event = CalendarEvent::new("New event", start, end);
                on_event.call(CalendarMutation::Create { event });
            },
            // Day number
            div {
                class: "flex items-center justify-end px-1",
                span {
                    class: if props.is_today {
                        "text-xs font-semibold bg-primary text-primary-foreground rounded-full w-5 h-5 flex items-center justify-center"
                    } else {
                        "text-xs"
                    },
                    "{date.day()}"
                }
            }
            // Events
            div { class: "flex flex-col gap-0.5 overflow-hidden",
                for ev in props.events.iter() {
                    {
                        let id = ev.id;
                        rsx! {
                            EventChip {
                                key: "{id}",
                                event: ev.clone(),
                                shape: ChipShape::Bar,
                                readonly: props.readonly,
                                on_click: move |_| on_open_editor.call(id),
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Events overlapping `[date 00:00, date+1 00:00)`, sorted by start
/// time so the chip stack reads top-to-bottom in chronological
/// order.
fn cell_events(events: &[CalendarEvent], date: NaiveDate) -> Vec<CalendarEvent> {
    let s = day_start_utc(date);
    let e = day_end_utc(date);
    let mut hits: Vec<CalendarEvent> = events
        .iter()
        .filter(|ev| ev.end > s && ev.start < e)
        .cloned()
        .collect();
    hits.sort_by_key(|ev| ev.start);
    hits
}
