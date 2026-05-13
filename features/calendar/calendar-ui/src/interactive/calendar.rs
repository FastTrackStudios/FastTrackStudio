//! Top-level `InteractiveCalendar` — orchestrates view selection,
//! navigation, search, dialog state, and keyboard shortcuts.
//!
//! FUTURE (deliberately not implemented in v1):
//!  - Drag-to-move / drag-to-resize events.
//!  - Multi-event drag, multi-select.
//!  - Timezone selector + per-event TZ.
//!  - RRULE expansion (recurring events render only at their seed).
//!  - iCal import/export.
//!  - CmdK-style jump-to-date / quick search modal.
//!  - Smarter overlap colouring (we only do greedy lane assignment).

use calendar_proto::{CalendarEvent, CalendarEventCreate, CalendarEventUpdate};
use chrono::{DateTime, Datelike, Duration, Local, NaiveDate, TimeZone, Utc};
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{ChevronLeft, ChevronRight, Plus, Search};
use fts_ui::prelude::*;
use uuid::Uuid;

use super::agenda::AgendaList;
use super::dialog::{EventDialog, EventDialogMode};
use super::mini_month::MiniMonth;
use super::month::MonthGrid;
use super::week::TimeGrid;

/// The four supported view modes.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CalendarView {
    Month,
    Week,
    Day,
    Agenda,
}

impl CalendarView {
    fn as_str(self) -> &'static str {
        match self {
            Self::Month => "month",
            Self::Week => "week",
            Self::Day => "day",
            Self::Agenda => "agenda",
        }
    }
    fn from_str(s: &str) -> Self {
        match s {
            "week" => Self::Week,
            "day" => Self::Day,
            "agenda" => Self::Agenda,
            _ => Self::Month,
        }
    }
}

#[component]
pub fn InteractiveCalendar(
    events: Vec<CalendarEvent>,
    initial_view: Option<CalendarView>,
    initial_date: Option<DateTime<Utc>>,
    #[props(default = true)] editable: bool,
    on_create: EventHandler<CalendarEventCreate>,
    on_update: EventHandler<(Uuid, CalendarEventUpdate)>,
    on_delete: EventHandler<Uuid>,
) -> Element {
    let mut view = use_signal(|| initial_view.unwrap_or(CalendarView::Week));
    let mut cursor = use_signal(|| initial_date.unwrap_or_else(Utc::now));
    let mut compact = use_signal(|| false);
    let search = use_signal(String::new);

    let mut dialog_open = use_signal(|| false);
    let mut dialog_mode = use_signal(|| EventDialogMode::Create {
        start: Utc::now(),
        end: Utc::now() + Duration::minutes(30),
        all_day: false,
    });

    // Filter events by the search box.
    let filtered: Vec<CalendarEvent> = {
        let q = search.read().trim().to_lowercase();
        if q.is_empty() {
            events.clone()
        } else {
            events
                .iter()
                .filter(|e| {
                    e.title.to_lowercase().contains(&q)
                        || e.description
                            .as_deref()
                            .map(|d| d.to_lowercase().contains(&q))
                            .unwrap_or(false)
                        || e.tags.iter().any(|t| t.to_lowercase().contains(&q))
                })
                .cloned()
                .collect()
        }
    };

    // Period label
    let period_label = {
        let local = cursor.read().with_timezone(&Local).date_naive();
        match *view.read() {
            CalendarView::Month => local.format("%B %Y").to_string(),
            CalendarView::Week => {
                let back = local.weekday().num_days_from_monday() as i64;
                let monday = local - Duration::days(back);
                let sunday = monday + Duration::days(6);
                format!(
                    "{} – {}",
                    monday.format("%b %-d"),
                    sunday.format("%b %-d, %Y")
                )
            }
            CalendarView::Day => local.format("%A, %B %-d, %Y").to_string(),
            CalendarView::Agenda => local.format("around %B %-d, %Y").to_string(),
        }
    };

    // Reusable callbacks (Copy via `Callback::new`).
    let shift: Callback<i64> = Callback::new(move |dir: i64| {
        let cur = *cursor.peek();
        let local = cur.with_timezone(&Local);
        let new_local = match *view.peek() {
            CalendarView::Month => {
                let y = local.year();
                let m = local.month() as i64 + dir;
                let (ny, nm) = if m < 1 {
                    (y - 1, 12i64)
                } else if m > 12 {
                    (y + 1, 1i64)
                } else {
                    (y, m)
                };
                NaiveDate::from_ymd_opt(ny, nm as u32, 1)
                    .and_then(|d| d.and_hms_opt(0, 0, 0))
                    .and_then(|ndt| Local.from_local_datetime(&ndt).single())
            }
            CalendarView::Week => Some(local + Duration::days(7 * dir)),
            CalendarView::Day => Some(local + Duration::days(dir)),
            CalendarView::Agenda => Some(local + Duration::days(7 * dir)),
        };
        if let Some(nl) = new_local {
            cursor.set(nl.with_timezone(&Utc));
        }
    });

    let open_create_for: Callback<(DateTime<Utc>, DateTime<Utc>, bool)> =
        Callback::new(move |(start, end, all_day)| {
            dialog_mode.set(EventDialogMode::Create {
                start,
                end,
                all_day,
            });
            dialog_open.set(true);
        });

    let open_edit: Callback<CalendarEvent> = Callback::new(move |ev: CalendarEvent| {
        dialog_mode.set(EventDialogMode::Edit(ev));
        dialog_open.set(true);
    });

    let on_pick_slot: Callback<(NaiveDate, u32, u32)> =
        Callback::new(move |(date, hour, minute): (NaiveDate, u32, u32)| {
            if let Some(ndt) = date.and_hms_opt(hour, minute, 0) {
                if let Some(local_dt) = Local.from_local_datetime(&ndt).single() {
                    let start = local_dt.with_timezone(&Utc);
                    let end = start + Duration::minutes(30);
                    open_create_for.call((start, end, false));
                }
            }
        });
    let on_pick_allday: Callback<NaiveDate> = Callback::new(move |date: NaiveDate| {
        if let Some(ndt) = date.and_hms_opt(0, 0, 0) {
            if let Some(local_dt) = Local.from_local_datetime(&ndt).single() {
                let start = local_dt.with_timezone(&Utc);
                let end = start + Duration::days(1);
                open_create_for.call((start, end, true));
            }
        }
    });

    let key_handler = move |e: KeyboardEvent| {
        let s = e.key().to_string();
        match s.as_str() {
            "Escape" => {
                dialog_open.set(false);
            }
            "j" => shift.call(-1),
            "k" => shift.call(1),
            "t" => cursor.set(Utc::now()),
            "m" => view.set(CalendarView::Month),
            "w" => view.set(CalendarView::Week),
            "d" => view.set(CalendarView::Day),
            "a" => view.set(CalendarView::Agenda),
            "n" => {
                let s = *cursor.peek();
                open_create_for.call((s, s + Duration::minutes(30), false));
            }
            _ => {}
        }
    };

    rsx! {
        div {
            class: "flex flex-col gap-4 outline-none",
            tabindex: "0",
            onkeydown: key_handler,

            // Toolbar
            HStack { class: "items-center justify-between flex-wrap gap-2",
                HStack { class: "items-center gap-2",
                    Button {
                        variant: ButtonVariant::Outline,
                        size: ButtonSize::Small,
                        on_click: move |_| shift.call(-1),
                        ChevronLeft { size: 14 }
                    }
                    Button {
                        variant: ButtonVariant::Outline,
                        size: ButtonSize::Small,
                        on_click: move |_| cursor.set(Utc::now()),
                        "Today"
                    }
                    Button {
                        variant: ButtonVariant::Outline,
                        size: ButtonSize::Small,
                        on_click: move |_| shift.call(1),
                        ChevronRight { size: 14 }
                    }
                    Heading { level: HeadingLevel::H3, "{period_label}" }
                }
                HStack { class: "items-center gap-2 flex-wrap",
                    div { class: "relative",
                        div { class: "absolute left-2 top-1/2 -translate-y-1/2 text-muted-foreground pointer-events-none z-10",
                            Search { size: 14 }
                        }
                        Input {
                            value: search,
                            placeholder: "Search…",
                            class: "pl-7 w-48",
                        }
                    }
                    SegmentedControl {
                        value: view.read().as_str().to_string(),
                        on_change: move |v: String| view.set(CalendarView::from_str(&v)),
                        options: vec![
                            ("month".into(), "Month".into()),
                            ("week".into(), "Week".into()),
                            ("day".into(), "Day".into()),
                            ("agenda".into(), "Agenda".into()),
                        ],
                    }
                    SegmentedControl {
                        size: SegmentedControlSize::Small,
                        value: if *compact.read() { "compact".to_string() } else { "comfortable".to_string() },
                        on_change: move |v: String| compact.set(v == "compact"),
                        options: vec![
                            ("comfortable".into(), "Comfortable".into()),
                            ("compact".into(), "Compact".into()),
                        ],
                    }
                    if editable {
                        Button {
                            variant: ButtonVariant::Primary,
                            size: ButtonSize::Small,
                            on_click: move |_| {
                                let s = *cursor.peek();
                                open_create_for.call((s, s + Duration::minutes(30), false));
                            },
                            Plus { size: 14 }
                            " New"
                        }
                    }
                }
            }

            // Main row: sidebar + view
            div { class: "grid grid-cols-1 lg:grid-cols-[16rem_1fr] gap-4 items-start",
                aside { class: "flex flex-col gap-3",
                    MiniMonth {
                        selected: *cursor.read(),
                        on_select: move |dt: DateTime<Utc>| cursor.set(dt),
                    }
                    div { class: "rounded-lg border border-border bg-card p-3 text-xs text-muted-foreground flex flex-col gap-1",
                        SectionHeader { label: "Shortcuts".to_string(), size: SectionHeaderSize::Small }
                        div { class: "flex justify-between", span { "Prev / Next" } span { class: "font-mono", "j / k" } }
                        div { class: "flex justify-between", span { "Today" } span { class: "font-mono", "t" } }
                        div { class: "flex justify-between", span { "Views" } span { class: "font-mono", "m w d a" } }
                        div { class: "flex justify-between", span { "New event" } span { class: "font-mono", "n" } }
                        div { class: "flex justify-between", span { "Close dialog" } span { class: "font-mono", "Esc" } }
                    }
                }

                section { class: "min-w-0",
                    {
                        let v = *view.read();
                        let cur = *cursor.read();
                        let comp = *compact.read();
                        let events_v = filtered.clone();
                        match v {
                            CalendarView::Month => rsx! {
                                MonthGrid {
                                    anchor: cur,
                                    events: events_v,
                                    on_pick_event: move |e: CalendarEvent| open_edit.call(e),
                                    on_pick_day: move |d: NaiveDate| on_pick_allday.call(d),
                                }
                            },
                            CalendarView::Week => rsx! {
                                TimeGrid {
                                    anchor: cur,
                                    days: 7usize,
                                    events: events_v,
                                    compact: comp,
                                    on_pick_event: move |e: CalendarEvent| open_edit.call(e),
                                    on_pick_slot: move |t: (NaiveDate, u32, u32)| on_pick_slot.call(t),
                                    on_pick_allday: move |d: NaiveDate| on_pick_allday.call(d),
                                }
                            },
                            CalendarView::Day => rsx! {
                                TimeGrid {
                                    anchor: cur,
                                    days: 1usize,
                                    events: events_v,
                                    compact: comp,
                                    on_pick_event: move |e: CalendarEvent| open_edit.call(e),
                                    on_pick_slot: move |t: (NaiveDate, u32, u32)| on_pick_slot.call(t),
                                    on_pick_allday: move |d: NaiveDate| on_pick_allday.call(d),
                                }
                            },
                            CalendarView::Agenda => rsx! {
                                AgendaList {
                                    anchor: cur,
                                    events: events_v,
                                    on_pick_event: move |e: CalendarEvent| open_edit.call(e),
                                }
                            },
                        }
                    }
                }
            }

            EventDialog {
                open: *dialog_open.read(),
                mode: dialog_mode.read().clone(),
                editable,
                on_close: move |_| dialog_open.set(false),
                on_save_create: move |c: CalendarEventCreate| on_create.call(c),
                on_save_update: move |u: (Uuid, CalendarEventUpdate)| on_update.call(u),
                on_delete: move |id: Uuid| on_delete.call(id),
            }
        }
    }
}
