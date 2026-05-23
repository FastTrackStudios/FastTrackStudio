//! `/schedule` — full month/week/day calendar.
//!
//! Wires [`view_calendar::Calendar`] against an in-memory
//! [`CalendarState`]. Mutations from drag/edit bubble through
//! `on_event` and are applied via the store's reducer. Persistence
//! to a CRDT/file vault is a follow-up — the component is
//! storage-agnostic by design.

use chrono::{Datelike, Duration, NaiveDate, TimeZone, Utc};
use dioxus::prelude::*;
use view_calendar::store::{CalendarState, apply};
use view_calendar::{Calendar, CalendarEvent, ColorTag, ViewMode};

#[component]
pub fn ScheduleView() -> Element {
    let mut state = use_signal(seed_state);

    let events: Vec<CalendarEvent> = state.read().events.values().cloned().collect();

    rsx! {
        div { class: "h-[calc(100vh-3.5rem)] lg:h-screen p-4 flex flex-col gap-3 overflow-hidden",
            Calendar {
                events,
                initial_view: Some(ViewMode::Week),
                on_event: move |mu| apply(&mut state.write(), &mu),
            }
        }
    }
}

fn seed_state() -> CalendarState {
    let mut s = CalendarState::default();
    let today: NaiveDate = chrono::Local::now().date_naive();
    let monday = today - Duration::days(today.weekday().num_days_from_monday() as i64);

    for (offset, hour, minutes, title, color) in [
        (0, 9, 60, "Standup", ColorTag::Info),
        (0, 14, 90, "Design review", ColorTag::Primary),
        (1, 10, 30, "1:1 — Alex", ColorTag::Neutral),
        (1, 13, 60, "Lunch w/ Sam", ColorTag::Success),
        (2, 11, 120, "Schedule deep work", ColorTag::Warning),
        (3, 9, 45, "Roadmap sync", ColorTag::Primary),
        (4, 16, 60, "Retro", ColorTag::Danger),
    ] {
        let start = Utc.from_utc_datetime(
            &(monday + Duration::days(offset))
                .and_hms_opt(hour, 0, 0)
                .unwrap(),
        );
        let end = start + Duration::minutes(minutes);
        let mut ev = CalendarEvent::new(title, start, end);
        ev.color = color;
        s.events.insert(ev.id, ev);
    }
    s
}
