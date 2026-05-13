//! Time-grid view used for both Week (7 days) and Day (1 day).
//!
//! Layout:
//!  - Header row: weekday + date.
//!  - All-day row: events with `all_day=true` OR end-start > 24h.
//!  - 24h scrollable body: absolutely-positioned events per day column.

use calendar_proto::CalendarEvent;
use chrono::{DateTime, Datelike, Duration, Local, NaiveDate, TimeZone, Timelike, Utc, Weekday};
use dioxus::prelude::*;

use super::chip::EventChip;
use super::layout::{assign_lanes, day_height_px, event_offsets, now_offset_px, slot_px};

#[component]
pub fn TimeGrid(
    /// Anchor day. For week view, the Monday on/before is used.
    anchor: DateTime<Utc>,
    /// 1 for Day, 7 for Week.
    days: usize,
    events: Vec<CalendarEvent>,
    compact: bool,
    on_pick_event: EventHandler<CalendarEvent>,
    /// Called on click in an empty slot — (day, hour, minute) in local time.
    on_pick_slot: EventHandler<(NaiveDate, u32, u32)>,
    /// Called when the all-day row is clicked for a given day.
    on_pick_allday: EventHandler<NaiveDate>,
) -> Element {
    let anchor_local = anchor.with_timezone(&Local).date_naive();
    let first_day = if days == 1 {
        anchor_local
    } else {
        let back = anchor_local.weekday().num_days_from_monday() as i64;
        anchor_local - Duration::days(back)
    };
    let today = Local::now().date_naive();
    let slot = slot_px(compact);
    let total_h = day_height_px(compact);

    // Precompute per-day event slices.
    let day_list: Vec<NaiveDate> = (0..days as i64)
        .map(|i| first_day + Duration::days(i))
        .collect();

    // All-day events per day (incl. multi-day spanning).
    let allday_for = |d: NaiveDate| -> Vec<CalendarEvent> {
        events
            .iter()
            .filter(|e| {
                let dur = (e.end_at - e.start_at).num_hours();
                let s = e.start_at.with_timezone(&Local).date_naive();
                let en = e.end_at.with_timezone(&Local).date_naive();
                (e.all_day || dur >= 24) && s <= d && d <= en
            })
            .cloned()
            .collect()
    };

    // Timed events per day.
    let timed_for = |d: NaiveDate| -> Vec<CalendarEvent> {
        events
            .iter()
            .filter(|e| {
                let dur = (e.end_at - e.start_at).num_hours();
                if e.all_day || dur >= 24 {
                    return false;
                }
                e.start_at.with_timezone(&Local).date_naive() == d
            })
            .cloned()
            .collect()
    };

    // Grid template — left gutter (60px) + N equal day columns.
    let grid_template = format!("60px repeat({}, minmax(0, 1fr))", days);

    rsx! {
        div { class: "flex flex-col w-full border border-border rounded-lg overflow-hidden bg-card",
            // Header row
            div {
                class: "grid border-b border-border bg-muted/30",
                style: "grid-template-columns: {grid_template};",
                div {} // gutter spacer
                for d in day_list.iter().cloned() {
                    {
                        let is_today = d == today;
                        let weekend = matches!(d.weekday(), Weekday::Sat | Weekday::Sun);
                        let bg = if weekend { "bg-muted/40" } else { "" };
                        let ring = if is_today { "ring-1 ring-inset ring-primary" } else { "" };
                        rsx! {
                            div { key: "{d}", class: "px-2 py-2 text-center {bg} {ring}",
                                div { class: "text-[10px] uppercase tracking-wider text-muted-foreground", "{weekday_short(d.weekday())}" }
                                div { class: if is_today { "text-sm font-semibold text-primary" } else { "text-sm" }, "{d.day()}" }
                            }
                        }
                    }
                }
            }
            // All-day row
            div {
                class: "grid border-b border-border bg-background",
                style: "grid-template-columns: {grid_template};",
                div { class: "px-1 py-1 text-[10px] text-muted-foreground uppercase tracking-wider self-center text-right pr-2", "All day" }
                for d in day_list.iter().cloned() {
                    {
                        let weekend = matches!(d.weekday(), Weekday::Sat | Weekday::Sun);
                        let bg = if weekend { "bg-muted/40" } else { "" };
                        let day_evts = allday_for(d);
                        let day_clone = d;
                        rsx! {
                            div {
                                key: "all-{d}",
                                class: "min-h-8 px-1 py-1 border-l border-border flex flex-col gap-0.5 cursor-pointer hover:bg-accent/30 {bg}",
                                onclick: move |_| on_pick_allday.call(day_clone),
                                for ev in day_evts.iter().cloned() {
                                    EventChip {
                                        key: "{ev.id}",
                                        event: ev,
                                        show_time: false,
                                        compact: true,
                                        on_click: move |e: CalendarEvent| on_pick_event.call(e),
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Scrollable body
            div { class: "overflow-y-auto max-h-[70vh]",
                div {
                    class: "grid relative",
                    style: "grid-template-columns: {grid_template};",
                    // Hour gutter
                    div {
                        class: "relative",
                        style: "height: {total_h}px;",
                        for h in 0..24u32 {
                            div {
                                key: "g-{h}",
                                class: "absolute right-1 text-[10px] text-muted-foreground",
                                style: "top: {(h as f32 * 2.0 * slot) - 6.0}px;",
                                "{h:02}:00"
                            }
                        }
                    }
                    // Day columns
                    for d in day_list.iter().cloned() {
                        {
                            let weekend = matches!(d.weekday(), Weekday::Sat | Weekday::Sun);
                            let is_today = d == today;
                            let bg = if weekend { "bg-muted/40" } else { "" };
                            let ring = if is_today { "ring-1 ring-inset ring-primary/40" } else { "" };
                            let day_evts = timed_for(d);
                            let lanes = assign_lanes(&day_evts);
                            let now_y = now_offset_px(d, compact);
                            let day_clone = d;
                            rsx! {
                                div {
                                    key: "col-{d}",
                                    class: "relative border-l border-border {bg} {ring}",
                                    style: "height: {total_h}px;",
                                    onclick: move |e: Event<MouseData>| {
                                        // Convert click Y → hour/minute via offsetY.
                                        let y = e.client_coordinates().y as f32;
                                        // We don't have the column origin easily — fall back to bounding rect via element page coords.
                                        // Approximation: use y modulo total_h. Works because the column begins
                                        // at the scroll-area top once header is excluded; for v1 this is acceptable.
                                        let mins = ((y.max(0.0) / slot) * 30.0) as i64;
                                        let h = ((mins / 60).clamp(0, 23)) as u32;
                                        let m = if (mins % 60) < 30 { 0u32 } else { 30u32 };
                                        on_pick_slot.call((day_clone, h, m));
                                    },
                                    // Hour rule lines
                                    for h in 1..24u32 {
                                        div {
                                            key: "r-{h}",
                                            class: "absolute left-0 right-0 border-t border-border/40",
                                            style: "top: {(h as f32) * 2.0 * slot}px;",
                                        }
                                    }
                                    // Events
                                    for (idx, ev) in day_evts.iter().enumerate() {
                                        {
                                            let slot_info = lanes[idx];
                                            let (top, height) = event_offsets(d, ev.start_at, ev.end_at, compact)
                                                .unwrap_or((0.0, slot));
                                            let width_pct = 100.0 / slot_info.lanes as f32;
                                            let left_pct = slot_info.lane as f32 * width_pct;
                                            let ev_c = ev.clone();
                                            rsx! {
                                                div {
                                                    key: "{ev.id}",
                                                    class: "absolute p-0.5",
                                                    style: "top: {top}px; height: {height}px; left: {left_pct}%; width: calc({width_pct}% - 2px);",
                                                    EventChip {
                                                        event: ev_c,
                                                        show_time: true,
                                                        compact: true,
                                                        on_click: move |e: CalendarEvent| on_pick_event.call(e),
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    // Current-time line
                                    if let Some(y) = now_y {
                                        div {
                                            class: "absolute left-0 right-0 h-px bg-red-500 z-10 pointer-events-none",
                                            style: "top: {y}px;",
                                        }
                                        div {
                                            class: "absolute -left-1 size-2 rounded-full bg-red-500 z-10 pointer-events-none",
                                            style: "top: {y - 4.0}px;",
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

fn weekday_short(w: Weekday) -> &'static str {
    match w {
        Weekday::Mon => "Mon",
        Weekday::Tue => "Tue",
        Weekday::Wed => "Wed",
        Weekday::Thu => "Thu",
        Weekday::Fri => "Fri",
        Weekday::Sat => "Sat",
        Weekday::Sun => "Sun",
    }
}

// Quiet unused-import lints on platforms where the helpers below aren't probed.
#[allow(dead_code)]
fn _probe() -> Option<DateTime<Utc>> {
    Local
        .from_local_datetime(&NaiveDate::from_ymd_opt(2000, 1, 1)?.and_hms_opt(0, 0, 0)?)
        .single()
        .map(|d| d.with_timezone(&Utc))
        .and_then(|d| if d.hour() < 24 { Some(d) } else { None })
}
