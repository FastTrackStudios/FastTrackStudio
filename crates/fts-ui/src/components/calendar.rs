//! Calendar — shadcn v4 maia style month calendar for date picking.

use chrono::{Datelike, Duration, Local, NaiveDate};
use dioxus::prelude::*;

// ---------------------------------------------------------------------------
// Calendar
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct CalendarProps {
    /// The currently selected date, if any.
    #[props(default)]
    pub selected: Option<NaiveDate>,

    /// Callback fired when the user clicks a day.
    #[props(default)]
    pub on_select: Option<Callback<NaiveDate>>,

    /// Extra CSS classes on the root element.
    #[props(default)]
    pub class: String,
}

/// A month-view calendar widget.
#[component]
pub fn Calendar(props: CalendarProps) -> Element {
    let today = Local::now().date_naive();
    let mut current_month = use_signal(|| first_of_month(today));

    let month_start = *current_month.read();
    let year = month_start.year();
    let month = month_start.month();

    // Label for the header, e.g. "April 2026"
    let month_label = format!("{} {}", month_name(month), year);

    // Build the 42-cell grid (6 weeks).
    let grid_start = prev_monday(month_start);
    let cells: Vec<NaiveDate> = (0..42).map(|i| grid_start + Duration::days(i)).collect();

    let selected = props.selected;
    let on_select = props.on_select;

    rsx! {
        div {
            class: crate::cn::merge_slice(&["p-3 bg-background w-fit", props.class.as_str()]),

            // ---- Header ----
            div { class: "flex items-center justify-between mb-4",
                button {
                    class: "inline-flex items-center justify-center size-8 rounded-lg hover:bg-muted transition-colors",
                    r#type: "button",
                    onclick: move |_| {
                        current_month.set(prev_month(month_start));
                    },
                    svg {
                        xmlns: "http://www.w3.org/2000/svg",
                        width: "16",
                        height: "16",
                        view_box: "0 0 24 24",
                        fill: "none",
                        stroke: "currentColor",
                        stroke_width: "2",
                        stroke_linecap: "round",
                        stroke_linejoin: "round",
                        path { d: "m15 18-6-6 6-6" }
                    }
                }

                span { class: "text-sm font-medium select-none", "{month_label}" }

                button {
                    class: "inline-flex items-center justify-center size-8 rounded-lg hover:bg-muted transition-colors",
                    r#type: "button",
                    onclick: move |_| {
                        current_month.set(next_month(month_start));
                    },
                    svg {
                        xmlns: "http://www.w3.org/2000/svg",
                        width: "16",
                        height: "16",
                        view_box: "0 0 24 24",
                        fill: "none",
                        stroke: "currentColor",
                        stroke_width: "2",
                        stroke_linecap: "round",
                        stroke_linejoin: "round",
                        path { d: "m9 18 6-6-6-6" }
                    }
                }
            }

            // ---- Weekday headers ----
            div { class: "flex",
                for label in WEEKDAY_LABELS.iter() {
                    div { class: "flex-1 text-center text-xs font-normal text-muted-foreground select-none py-1",
                        "{label}"
                    }
                }
            }

            // ---- Day grid ----
            for week in cells.chunks(7) {
                div { class: "flex mt-1",
                    for &day in week.iter() {
                        {day_button(day, month, today, selected, on_select)}
                    }
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Day button helper
// ---------------------------------------------------------------------------

fn day_button(
    day: NaiveDate,
    current_month: u32,
    today: NaiveDate,
    selected: Option<NaiveDate>,
    on_select: Option<Callback<NaiveDate>>,
) -> Element {
    let is_today = day == today;
    let is_selected = selected == Some(day);
    let is_outside = day.month() != current_month;

    let mut cls = String::from(
        "flex-1 flex items-center justify-center size-8 rounded-full text-sm transition-colors cursor-pointer select-none",
    );

    if is_selected {
        cls.push_str(" bg-primary text-primary-foreground hover:bg-primary/80");
    } else if is_outside {
        cls.push_str(" text-muted-foreground/50");
    } else {
        cls.push_str(" hover:bg-muted");
    }

    if is_today {
        cls.push_str(" font-semibold");
    }

    rsx! {
        button {
            r#type: "button",
            class: "{cls}",
            onclick: move |_| {
                if let Some(cb) = on_select {
                    cb.call(day);
                }
            },
            "{day.day()}"
        }
    }
}

// ---------------------------------------------------------------------------
// Date helpers
// ---------------------------------------------------------------------------

const WEEKDAY_LABELS: [&str; 7] = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"];

/// First day of the month containing `d`.
fn first_of_month(d: NaiveDate) -> NaiveDate {
    NaiveDate::from_ymd_opt(d.year(), d.month(), 1).unwrap()
}

/// Monday of the week containing `d`.
fn prev_monday(d: NaiveDate) -> NaiveDate {
    let wd = d.weekday().num_days_from_monday(); // Mon=0
    d - Duration::days(wd as i64)
}

/// First day of the previous month.
fn prev_month(d: NaiveDate) -> NaiveDate {
    if d.month() == 1 {
        NaiveDate::from_ymd_opt(d.year() - 1, 12, 1).unwrap()
    } else {
        NaiveDate::from_ymd_opt(d.year(), d.month() - 1, 1).unwrap()
    }
}

/// First day of the next month.
fn next_month(d: NaiveDate) -> NaiveDate {
    if d.month() == 12 {
        NaiveDate::from_ymd_opt(d.year() + 1, 1, 1).unwrap()
    } else {
        NaiveDate::from_ymd_opt(d.year(), d.month() + 1, 1).unwrap()
    }
}

/// English month name.
fn month_name(m: u32) -> &'static str {
    match m {
        1 => "January",
        2 => "February",
        3 => "March",
        4 => "April",
        5 => "May",
        6 => "June",
        7 => "July",
        8 => "August",
        9 => "September",
        10 => "October",
        11 => "November",
        12 => "December",
        _ => "Unknown",
    }
}
