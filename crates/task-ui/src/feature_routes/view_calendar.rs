//! `/views/calendar` — stub demo of the `view-calendar` crate.
//!
//! No CRDT wiring yet; events live in a local signal and mutations
//! are applied via the calendar's event stream. Drop-in replacement
//! for a real `TaskRepoLoro`-backed wrapper later.

use chrono::{Duration, TimeZone, Utc};
use dioxus::prelude::*;
use fts_ui::prelude::*;
use view::calendar::{
    Calendar, CalendarEvent, CalendarMutation, CalendarState, ColorTag, store::apply,
};

#[component]
pub fn CalendarView() -> Element {
    let mut state = use_signal(seed_state);

    let on_event = EventHandler::new(move |mu: CalendarMutation| {
        state.with_mut(|s| apply(s, &mu));
    });

    rsx! {
        div { class: "h-[calc(100vh-3.5rem)] p-4 flex flex-col gap-3",
            Heading { level: HeadingLevel::H1, "Calendar" }
            Text { variant: TextVariant::Muted,
                "Click empty space to create. Click an event to edit. Drag to reschedule (week/day also supports bottom-edge resize)."
            }
            div { class: "flex-1 min-h-0 border border-border/60 rounded-lg overflow-hidden",
                Calendar { events: state.read().events.values().cloned().collect(), on_event }
            }
        }
    }
}

fn seed_state() -> CalendarState {
    let mut state = CalendarState::default();
    let today = chrono::Local::now().date_naive();
    let day_start = Utc.from_utc_datetime(&today.and_hms_opt(0, 0, 0).expect("midnight"));

    let push = |state: &mut CalendarState,
                title: &str,
                day_offset: i64,
                hour: i64,
                dur_h: i64,
                color: ColorTag| {
        let start = day_start + Duration::days(day_offset) + Duration::hours(hour);
        let end = start + Duration::hours(dur_h);
        let mut ev = CalendarEvent::new(title, start, end);
        ev.color = color;
        state.events.insert(ev.id, ev);
    };

    push(&mut state, "Standup", 0, 9, 1, ColorTag::Primary);
    push(&mut state, "Design review", 0, 14, 2, ColorTag::Info);
    push(&mut state, "1:1 with Sam", 1, 10, 1, ColorTag::Success);
    push(
        &mut state,
        "Ship view-calendar",
        2,
        16,
        1,
        ColorTag::Warning,
    );
    push(&mut state, "Vacation", 3, 0, 24, ColorTag::Neutral);
    state
}
