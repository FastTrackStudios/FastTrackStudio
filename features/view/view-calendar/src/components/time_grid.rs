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
use uuid::Uuid;

use crate::layout::{TimeBlockPlacement, day_overlap_layout};
use crate::store::CalendarMutation;
use crate::time::{day_start_utc, hour_labels};
use crate::types::{CalendarEvent, EventId};

use super::all_day_strip::AllDayStrip;
use super::drag::{DT_MIME, DragKind, use_drag_context};
use super::event_chip::EventChip;
use super::now_line::NowLine;

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
    is_last: bool,
    readonly: bool,
    on_event: EventHandler<CalendarMutation>,
    on_open_editor: EventHandler<EventId>,
}

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
    let ctx = use_drag_context();
    let date = props.date;
    let on_event = props.on_event;
    let on_open_editor = props.on_open_editor;
    let border_r = if props.is_last { "" } else { "border-r" };
    let mut sweep: Signal<Option<Sweep>> = use_signal(|| None);

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
            // Drop / sweep surface (catches background interactions).
            // Sits BELOW the event blocks so chips win the click race.
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
                            let new_start = day_start_utc(date) + Duration::minutes(drop_min);
                            let duration = ds.orig_end - ds.orig_start;
                            on_event.call(CalendarMutation::Reschedule {
                                id,
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
                                id,
                                start: ds.orig_start,
                                end: new_end,
                            });
                        }
                    }
                },
                // Sweep to create — primary button only. Click
                // without sweep falls through to the mouseup
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
                onmouseup: move |_| {
                    if props.readonly { return; }
                    let Some(s) = sweep.take() else { return };
                    let (start_min, end_min) = s.range();
                    let start = day_start_utc(date) + Duration::minutes(start_min);
                    let end = day_start_utc(date) + Duration::minutes(end_min);
                    // If the user just clicked without dragging,
                    // `end_min` was bumped to `start_min + 15`. We
                    // upgrade that to a 1-hour default — matches the
                    // month-view click-create behavior and avoids
                    // creating a sliver event.
                    let end = if (end - start).num_minutes() <= SNAP_MINUTES {
                        start + Duration::hours(1)
                    } else {
                        end
                    };
                    let event = CalendarEvent::new("New event", start, end);
                    on_event.call(CalendarMutation::Create { event });
                },
                onmouseleave: move |_| {
                    // Bail on the sweep if the pointer leaves the
                    // column — avoids ghost blocks lingering after
                    // a drag escapes the day.
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
                            on_click: move |_| on_open_editor.call(id),
                        }
                    }
                }
            }
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
    let width_pct = 100.0_f32 / p.cluster_size as f32;
    let left_pct = p.column as f32 * width_pct;
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
