//! Root Calendar component — owns toolbar state (anchor date +
//! view mode + editor open state), provides drag context, dispatches
//! `CalendarMutation`s upward, and swaps in the right view.

use chrono::{Days, Months, NaiveDate, TimeZone};
use dioxus::prelude::*;

use crate::store::CalendarMutation;
use crate::time::week_start;
use crate::types::{CalendarEvent, EventId, ViewMode};

use super::day_view::DayView;
use super::drag::DragContext;
use super::event_editor::EventEditor;
use super::month_view::MonthView;
use super::toolbar::Toolbar;
use super::week_view::WeekView;

#[derive(Props, Clone, PartialEq)]
pub struct CalendarProps {
    pub events: Vec<CalendarEvent>,
    /// Defaults to today.
    #[props(default)]
    pub initial_anchor: Option<NaiveDate>,
    /// Defaults to `ViewMode::Week`.
    #[props(default)]
    pub initial_view: Option<ViewMode>,
    #[props(default = false)]
    pub readonly: bool,
    pub on_event: EventHandler<CalendarMutation>,
}

#[component]
pub fn Calendar(props: CalendarProps) -> Element {
    let today = chrono::Local::now().date_naive();
    let mut anchor = use_signal(|| props.initial_anchor.unwrap_or(today));
    let mut view = use_signal(|| props.initial_view.unwrap_or_default());
    let mut editing: Signal<Option<EventId>> = use_signal(|| None);

    use_context_provider(|| DragContext {
        state: Signal::new(None),
    });

    let on_event = props.on_event;
    let events = props.events.clone();
    let events_for_view = events.clone();

    let selected = editing
        .read()
        .and_then(|id| events.iter().find(|e| e.id == id).cloned());

    rsx! {
        div { class: "flex flex-col h-full w-full",
            Toolbar {
                anchor: *anchor.read(),
                view: *view.read(),
                on_prev: move |_| anchor.with_mut(|d| *d = step(*d, *view.read(), -1)),
                on_next: move |_| anchor.with_mut(|d| *d = step(*d, *view.read(), 1)),
                on_today: move |_| anchor.set(today),
                on_view_change: move |v: ViewMode| view.set(v),
                on_create: move |_| {
                    let start = chrono::Utc::now().date_naive().and_hms_opt(9, 0, 0)
                        .expect("9 am") ;
                    let start = chrono::Utc.from_utc_datetime(&start);
                    let end = start + chrono::Duration::hours(1);
                    let event = CalendarEvent::new("New event", start, end);
                    let id = event.id;
                    on_event.call(CalendarMutation::Create { event });
                    editing.set(Some(id));
                },
                readonly: props.readonly,
            }
            div { class: "flex-1 min-h-0",
                match *view.read() {
                    ViewMode::Month => rsx! {
                        MonthView {
                            anchor: *anchor.read(),
                            events: events_for_view,
                            readonly: props.readonly,
                            on_event,
                            on_open_editor: move |id| editing.set(Some(id)),
                        }
                    },
                    ViewMode::Week => rsx! {
                        WeekView {
                            anchor: *anchor.read(),
                            events: events_for_view,
                            readonly: props.readonly,
                            on_event,
                            on_open_editor: move |id| editing.set(Some(id)),
                        }
                    },
                    ViewMode::Day => rsx! {
                        DayView {
                            anchor: *anchor.read(),
                            events: events_for_view,
                            readonly: props.readonly,
                            on_event,
                            on_open_editor: move |id| editing.set(Some(id)),
                        }
                    },
                }
            }
            if let Some(ev) = selected {
                EventEditor {
                    event: ev,
                    open: true,
                    on_close: move |_| editing.set(None),
                    on_event,
                }
            }
        }
    }
}

/// Step `anchor` by `dir` (-1 = prev, +1 = next) in the unit
/// matching `view`.
fn step(anchor: NaiveDate, view: ViewMode, dir: i64) -> NaiveDate {
    match view {
        ViewMode::Month => {
            if dir < 0 {
                anchor
                    .checked_sub_months(Months::new((-dir) as u32))
                    .unwrap_or(anchor)
            } else {
                anchor
                    .checked_add_months(Months::new(dir as u32))
                    .unwrap_or(anchor)
            }
        }
        ViewMode::Week => {
            let base = week_start(anchor);
            if dir < 0 {
                base.checked_sub_days(Days::new((-dir * 7) as u64))
                    .unwrap_or(base)
            } else {
                base.checked_add_days(Days::new((dir * 7) as u64))
                    .unwrap_or(base)
            }
        }
        ViewMode::Day => {
            if dir < 0 {
                anchor
                    .checked_sub_days(Days::new((-dir) as u64))
                    .unwrap_or(anchor)
            } else {
                anchor
                    .checked_add_days(Days::new(dir as u64))
                    .unwrap_or(anchor)
            }
        }
    }
}
