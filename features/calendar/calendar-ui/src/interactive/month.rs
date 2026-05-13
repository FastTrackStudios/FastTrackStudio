//! Month-grid view: 6 rows × 7 cols of day cells.

use calendar_proto::CalendarEvent;
use chrono::{Datelike, Duration, Local, NaiveDate, TimeZone, Utc, Weekday};
use dioxus::prelude::*;

use super::chip::EventChip;

#[component]
pub fn MonthGrid(
    /// Any datetime inside the month to render.
    anchor: chrono::DateTime<Utc>,
    events: Vec<CalendarEvent>,
    on_pick_event: EventHandler<CalendarEvent>,
    on_pick_day: EventHandler<NaiveDate>,
) -> Element {
    let anchor_local = anchor.with_timezone(&Local).date_naive();
    let first_of_month = NaiveDate::from_ymd_opt(anchor_local.year(), anchor_local.month(), 1)
        .unwrap_or(anchor_local);
    // Roll back to the Monday on/before the first of the month.
    let offset_back = first_of_month.weekday().num_days_from_monday() as i64;
    let grid_start = first_of_month - chrono::Duration::days(offset_back);

    let today = Local::now().date_naive();
    let weekdays = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"];

    rsx! {
        div { class: "flex flex-col gap-1 w-full",
            div { class: "grid grid-cols-7 gap-1 text-[11px] uppercase tracking-wider text-muted-foreground",
                for w in weekdays.iter() {
                    div { class: "px-2 py-1", "{w}" }
                }
            }
            div { class: "grid grid-cols-7 grid-rows-6 gap-1",
                for row in 0..6 {
                    for col in 0..7 {
                        {
                            let day = grid_start + Duration::days((row * 7 + col) as i64);
                            let in_month = day.month() == anchor_local.month();
                            let is_today = day == today;
                            let is_weekend = matches!(day.weekday(), Weekday::Sat | Weekday::Sun);
                            let day_events: Vec<CalendarEvent> = events
                                .iter()
                                .filter(|e| {
                                    let s = e.start_at.with_timezone(&Local).date_naive();
                                    let en = e.end_at.with_timezone(&Local).date_naive();
                                    s <= day && day <= en
                                })
                                .cloned()
                                .collect();
                            let visible: Vec<CalendarEvent> = day_events.iter().take(3).cloned().collect();
                            let extra = day_events.len().saturating_sub(visible.len());
                            let day_clone = day;
                            let muted = if in_month { "" } else { "opacity-50" };
                            let today_ring = if is_today { "ring-1 ring-primary" } else { "border border-border" };
                            let weekend_bg = if is_weekend { "bg-muted/40" } else { "bg-card" };
                            rsx! {
                                div {
                                    key: "{day}",
                                    class: "rounded-md p-1 min-h-24 flex flex-col gap-1 cursor-pointer hover:bg-accent/40 {muted} {today_ring} {weekend_bg}",
                                    onclick: move |_| on_pick_day.call(day_clone),
                                    div { class: "flex items-center justify-between",
                                        span {
                                            class: if is_today { "text-xs font-semibold text-primary" } else { "text-xs text-muted-foreground" },
                                            "{day.day()}"
                                        }
                                    }
                                    div { class: "flex flex-col gap-0.5 overflow-hidden",
                                        for ev in visible.iter().cloned() {
                                            EventChip {
                                                key: "{ev.id}",
                                                event: ev,
                                                show_time: true,
                                                compact: true,
                                                on_click: move |e: CalendarEvent| on_pick_event.call(e),
                                            }
                                        }
                                        if extra > 0 {
                                            span { class: "text-[10px] text-muted-foreground px-1",
                                                "+{extra} more"
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
}

// Silence chrono::TimeZone unused-import on some configs.
#[allow(dead_code)]
fn _probe(_: chrono::DateTime<Utc>) -> Option<NaiveDate> {
    Local
        .from_utc_datetime(&NaiveDate::from_ymd_opt(2000, 1, 1)?.and_hms_opt(0, 0, 0)?)
        .date_naive()
        .into()
}
