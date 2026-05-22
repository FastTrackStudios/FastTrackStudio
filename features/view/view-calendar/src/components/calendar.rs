//! Root Calendar component — owns toolbar state (anchor date +
//! view mode + editor open state), provides drag context, dispatches
//! `CalendarMutation`s upward, and swaps in the right view.

use chrono::{Days, Months, NaiveDate, TimeZone};
use dioxus::prelude::*;

use crate::recurrence::expand_all;
use crate::store::CalendarMutation;
use crate::time::{day_start_utc, month_grid, week_days, week_start};
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

    // Expand any recurring masters to instances inside the visible
    // range so the views stay recurrence-agnostic. Buffered by one
    // day on each side so multi-day chips that start *just* before
    // the visible window still render with the correct edge.
    let (vis_start, vis_end) = visible_range(*anchor.read(), *view.read());
    let events_for_view = expand_all(&events, vis_start, vis_end);

    // Editor opens against the master (so series edits apply
    // globally for v1) — look up by id, not by expanded instance.
    let selected = editing
        .read()
        .and_then(|id| events.iter().find(|e| e.id == id).cloned());

    // Keyboard shortcuts — only fire when no editor sheet is open
    // (otherwise typing in the title input would jump views). The
    // outer div takes focus on mount; arrow keys / single-letter
    // shortcuts work without an explicit click.
    let editor_open = editing.read().is_some();
    let on_keydown = move |e: KeyboardEvent| {
        if editor_open {
            return;
        }
        let key = e.data().key();
        match key {
            Key::Character(ref s) => {
                let s = s.to_ascii_lowercase();
                match s.as_str() {
                    "t" => anchor.set(today),
                    "1" => view.set(ViewMode::Day),
                    "2" => view.set(ViewMode::Week),
                    "3" => view.set(ViewMode::Month),
                    "n" => {
                        if !props.readonly {
                            let start = chrono::Utc::now();
                            let end = start + chrono::Duration::hours(1);
                            let event = CalendarEvent::new("New event", start, end);
                            let id = event.id;
                            on_event.call(CalendarMutation::Create { event });
                            editing.set(Some(id));
                        }
                    }
                    _ => {}
                }
            }
            Key::ArrowLeft => anchor.with_mut(|d| *d = step(*d, *view.read(), -1)),
            Key::ArrowRight => anchor.with_mut(|d| *d = step(*d, *view.read(), 1)),
            _ => {}
        }
    };

    rsx! {
        div {
            class: "flex flex-col h-full w-full outline-none",
            tabindex: 0,
            autofocus: true,
            onkeydown: on_keydown,
            Toolbar {
                anchor: *anchor.read(),
                view: *view.read(),
                on_prev: move |()| anchor.with_mut(|d| *d = step(*d, *view.read(), -1)),
                on_next: move |()| anchor.with_mut(|d| *d = step(*d, *view.read(), 1)),
                on_today: move |()| anchor.set(today),
                on_view_change: move |v: ViewMode| view.set(v),
                on_create: move |()| {
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
                            on_zoom_to_day: move |d: NaiveDate| {
                                anchor.set(d);
                                view.set(ViewMode::Day);
                            },
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
                    on_close: move |()| editing.set(None),
                    on_event,
                }
            }
        }
    }
}

/// The UTC `[start, end)` window covered by the current view, with
/// a one-day padding on each side so chips that lap the edge still
/// render correctly.
fn visible_range(
    anchor: NaiveDate,
    view: ViewMode,
) -> (chrono::DateTime<chrono::Utc>, chrono::DateTime<chrono::Utc>) {
    let (start, end) = match view {
        ViewMode::Month => {
            let grid = month_grid(anchor);
            (grid[0][0], grid[5][6] + Days::new(1))
        }
        ViewMode::Week => {
            let days = week_days(anchor);
            (days[0], days[6] + Days::new(1))
        }
        ViewMode::Day => (anchor, anchor + Days::new(1)),
    };
    (
        day_start_utc(start) - chrono::Duration::days(1),
        day_start_utc(end) + chrono::Duration::days(1),
    )
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
