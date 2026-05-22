//! Shared time-grid view used by week (`days.len() == 7`) and day
//! (`days.len() == 1`) variants.
//!
//! Layout is hand-rolled in absolute pixels so drop-y math is
//! direct (no element-size measurement needed). `PX_PER_HOUR` ×
//! 24 = column height; every minute is `PX_PER_HOUR / 60` px.

use chrono::{Datelike, Duration, NaiveDate, Utc};
use dioxus::prelude::*;
use uuid::Uuid;

use crate::store::CalendarMutation;
use crate::time::{day_start_utc, hour_labels};
use crate::types::{CalendarEvent, EventId};

use super::drag::{DT_MIME, DragKind, use_drag_context};
use super::event_chip::{ChipShape, EventChip};

const PX_PER_HOUR: i64 = 48;
const COL_HEIGHT_PX: i64 = PX_PER_HOUR * 24;
const SNAP_MINUTES: i64 = 15;

#[derive(Props, Clone, PartialEq)]
pub struct TimeGridViewProps {
    pub days: Vec<NaiveDate>,
    pub events: Vec<CalendarEvent>,
    #[props(default = false)]
    pub readonly: bool,
    pub on_event: EventHandler<CalendarMutation>,
    pub on_open_editor: EventHandler<EventId>,
}

#[component]
pub fn TimeGridView(props: TimeGridViewProps) -> Element {
    let today = chrono::Local::now().date_naive();

    rsx! {
        div { class: "flex flex-col h-full w-full",
            // Day header strip
            div { class: "grid border-b border-border/40",
                style: "grid-template-columns: 56px repeat({props.days.len()}, 1fr);",
                div {} // empty corner above the hour axis
                for date in props.days.iter() {
                    {
                        let is_today = *date == today;
                        let day_name = date.format("%a").to_string();
                        let day_num = date.day();
                        rsx! {
                            div {
                                key: "{date}",
                                class: "flex items-center justify-center gap-2 py-1 text-xs",
                                span { class: "text-muted-foreground", "{day_name}" }
                                span {
                                    class: if is_today {
                                        "font-semibold bg-primary text-primary-foreground rounded-full w-5 h-5 flex items-center justify-center"
                                    } else {
                                        "font-medium"
                                    },
                                    "{day_num}"
                                }
                            }
                        }
                    }
                }
            }
            // Scrollable grid body
            div { class: "flex-1 min-h-0 overflow-y-auto",
                div {
                    class: "grid relative",
                    style: "grid-template-columns: 56px repeat({props.days.len()}, 1fr); height: {COL_HEIGHT_PX}px;",
                    // Hour axis
                    HourAxis {}
                    // Day columns
                    for (idx, date) in props.days.iter().enumerate() {
                        DayColumn {
                            key: "{date}",
                            date: *date,
                            events: column_events(&props.events, *date),
                            is_last: idx == props.days.len() - 1,
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
    events: Vec<CalendarEvent>,
    is_last: bool,
    readonly: bool,
    on_event: EventHandler<CalendarMutation>,
    on_open_editor: EventHandler<EventId>,
}

#[component]
fn DayColumn(props: DayColumnProps) -> Element {
    let ctx = use_drag_context();
    let date = props.date;
    let on_event = props.on_event;
    let on_open_editor = props.on_open_editor;
    let border_r = if props.is_last { "" } else { "border-r" };

    rsx! {
        div {
            class: "relative border-border/40 {border_r}",
            // Hour grid lines (decorative)
            for h in 1..24u32 {
                div {
                    key: "h-{h}",
                    class: "absolute left-0 right-0 border-t border-border/20 pointer-events-none",
                    style: "top: {h as i64 * PX_PER_HOUR}px;",
                }
            }
            // Drop / click-create surface
            div {
                class: "absolute inset-0",
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
                    let y = e.data().element_coordinates().y as i64;
                    let drop_min = snap_minutes(px_to_minutes(y));
                    match ds.kind {
                        DragKind::Move => {
                            // New start = drop_min minutes into `date`.
                            let new_start = day_start_utc(date)
                                + Duration::minutes(drop_min);
                            let duration = ds.orig_end - ds.orig_start;
                            on_event.call(CalendarMutation::Reschedule {
                                id,
                                start: new_start,
                                end: new_start + duration,
                            });
                        }
                        DragKind::ResizeEnd => {
                            // End-edge drag: replace `end` only.
                            // Keep ≥ 15 min duration.
                            let new_end = day_start_utc(date)
                                + Duration::minutes(drop_min.max(
                                    (ds.orig_start.signed_duration_since(day_start_utc(date)))
                                        .num_minutes() + SNAP_MINUTES,
                                ));
                            on_event.call(CalendarMutation::Reschedule {
                                id,
                                start: ds.orig_start,
                                end: new_end,
                            });
                        }
                    }
                },
                onclick: move |e: MouseEvent| {
                    if props.readonly { return; }
                    let y = e.data().element_coordinates().y as i64;
                    let start_min = snap_minutes(px_to_minutes(y));
                    let start = day_start_utc(date) + Duration::minutes(start_min);
                    let end = start + Duration::hours(1);
                    let event = CalendarEvent::new("New event", start, end);
                    on_event.call(CalendarMutation::Create { event });
                },
            }
            // Events (above the click-surface so they receive clicks)
            for ev in props.events.iter() {
                {
                    let id = ev.id;
                    let style = block_style(date, ev.start, ev.end);
                    rsx! {
                        EventChip {
                            key: "{id}",
                            event: ev.clone(),
                            shape: ChipShape::Block,
                            position_style: style,
                            readonly: props.readonly,
                            on_click: move |_| on_open_editor.call(id),
                        }
                    }
                }
            }
        }
    }
}

/// Events whose `[start, end)` overlaps `[date 00:00, date+1 00:00)`,
/// ordered by start.
fn column_events(events: &[CalendarEvent], date: NaiveDate) -> Vec<CalendarEvent> {
    let s = day_start_utc(date);
    let e = s + Duration::days(1);
    let mut hits: Vec<CalendarEvent> = events
        .iter()
        .filter(|ev| ev.end > s && ev.start < e)
        .cloned()
        .collect();
    hits.sort_by_key(|ev| ev.start);
    hits
}

fn block_style(day: NaiveDate, start: chrono::DateTime<Utc>, end: chrono::DateTime<Utc>) -> String {
    let day_start = day_start_utc(day);
    let day_end = day_start + Duration::days(1);
    let clipped_start = start.max(day_start);
    let clipped_end = end.min(day_end);
    let top_min = (clipped_start - day_start).num_minutes().max(0);
    let dur_min = (clipped_end - clipped_start).num_minutes().max(15);
    let top_px = minutes_to_px(top_min);
    let h_px = minutes_to_px(dur_min);
    format!("top: {top_px}px; height: {h_px}px;")
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
