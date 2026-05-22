//! Side sheet that opens when a chip is clicked. Edits title + time
//! window + color + all-day flag + description, and offers a
//! destructive delete.

use chrono::{Datelike, NaiveDate, NaiveDateTime, NaiveTime, TimeZone, Timelike, Utc};
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::Trash2;
use fts_ui::prelude::*;

use crate::store::CalendarMutation;
use crate::types::{CalendarEvent, ColorTag, EventId};

const COLOR_OPTIONS: &[ColorTag] = &[
    ColorTag::Neutral,
    ColorTag::Primary,
    ColorTag::Success,
    ColorTag::Warning,
    ColorTag::Danger,
    ColorTag::Info,
];

#[derive(Props, Clone, PartialEq)]
pub struct EventEditorProps {
    pub event: CalendarEvent,
    pub open: bool,
    pub on_close: EventHandler<()>,
    pub on_event: EventHandler<CalendarMutation>,
}

#[component]
pub fn EventEditor(props: EventEditorProps) -> Element {
    let event = props.event.clone();
    let id: EventId = event.id;
    let on_event = props.on_event;
    let on_close = props.on_close;

    rsx! {
        Sheet {
            open: props.open,
            on_close: move |_| on_close.call(()),
            SheetHeader {
                SheetTitle { "Edit event" }
                SheetDescription { "Changes apply immediately." }
            }
            div { class: "flex flex-col gap-4 mt-2",
                // Title
                div { class: "flex flex-col gap-1.5",
                    Label { "Title" }
                    input {
                        class: "w-full bg-background border border-input rounded px-2 py-1 text-sm",
                        value: "{event.title}",
                        oninput: move |e: FormEvent| {
                            on_event.call(CalendarMutation::Rename { id, title: e.value() });
                        },
                    }
                }
                // All-day toggle
                label { class: "flex items-center gap-2 text-sm",
                    input {
                        r#type: "checkbox",
                        checked: event.all_day,
                        onchange: move |e: FormEvent| {
                            on_event.call(CalendarMutation::SetAllDay { id, all_day: e.value() == "true" });
                        },
                    }
                    "All day"
                }
                // Start / end
                DateTimeField {
                    label: "Starts",
                    value: event.start,
                    all_day: event.all_day,
                    on_change: move |dt: chrono::DateTime<chrono::Utc>| {
                        on_event.call(CalendarMutation::Reschedule {
                            id,
                            start: dt,
                            end: event.end.max(dt + chrono::Duration::minutes(15)),
                        });
                    },
                }
                DateTimeField {
                    label: "Ends",
                    value: event.end,
                    all_day: event.all_day,
                    on_change: move |dt: chrono::DateTime<chrono::Utc>| {
                        on_event.call(CalendarMutation::Reschedule {
                            id,
                            start: event.start,
                            end: dt.max(event.start + chrono::Duration::minutes(15)),
                        });
                    },
                }
                // Color swatches
                div { class: "flex flex-col gap-1.5",
                    Label { "Color" }
                    div { class: "flex items-center gap-2",
                        for color in COLOR_OPTIONS.iter() {
                            {
                                let c = *color;
                                let stem = c.stem();
                                let ring = if c == event.color { "ring-2 ring-foreground/60" } else { "" };
                                let key = format!("swatch-{stem}");
                                rsx! {
                                    button {
                                        key: "{key}",
                                        r#type: "button",
                                        class: "w-6 h-6 rounded-full bg-{stem}-500 {ring} transition-shadow",
                                        title: "{stem}",
                                        onclick: move |_| {
                                            on_event.call(CalendarMutation::Recolor { id, color: c });
                                        },
                                    }
                                }
                            }
                        }
                    }
                }
                // Description
                div { class: "flex flex-col gap-1.5",
                    Label { "Description" }
                    textarea {
                        class: "w-full bg-background border border-input rounded px-2 py-1 text-sm",
                        rows: 4,
                        value: event.description.clone().unwrap_or_default(),
                        oninput: move |e: FormEvent| {
                            let v = e.value();
                            let description = if v.is_empty() { None } else { Some(v) };
                            on_event.call(CalendarMutation::SetDescription { id, description });
                        },
                    }
                }
            }
            SheetFooter {
                Button {
                    variant: ButtonVariant::Outline,
                    on_click: move |_| {
                        on_event.call(CalendarMutation::Remove { id });
                        on_close.call(());
                    },
                    Trash2 { size: 14 }
                    "Delete"
                }
                Button {
                    on_click: move |_| on_close.call(()),
                    "Done"
                }
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
struct DateTimeFieldProps {
    label: &'static str,
    value: chrono::DateTime<chrono::Utc>,
    all_day: bool,
    on_change: EventHandler<chrono::DateTime<chrono::Utc>>,
}

/// Side-by-side native `date` + `time` inputs. The two fields are
/// stitched back together when either commits — keeps the editor
/// free of timezone juggling for v1 (we treat the user's local
/// inputs as already UTC-aligned).
#[component]
fn DateTimeField(props: DateTimeFieldProps) -> Element {
    let naive = props.value.naive_utc();
    let date_str = naive.format("%Y-%m-%d").to_string();
    let time_str = naive.format("%H:%M").to_string();
    let on_change = props.on_change;
    let cur = props.value;

    rsx! {
        div { class: "flex flex-col gap-1.5",
            Label { "{props.label}" }
            div { class: "flex items-center gap-2",
                input {
                    r#type: "date",
                    class: "bg-background border border-input rounded px-2 py-1 text-sm",
                    value: "{date_str}",
                    onchange: move |e: FormEvent| {
                        if let Some(dt) = recombine(Some(&e.value()), None, cur) {
                            on_change.call(dt);
                        }
                    },
                }
                if !props.all_day {
                    input {
                        r#type: "time",
                        class: "bg-background border border-input rounded px-2 py-1 text-sm",
                        value: "{time_str}",
                        onchange: move |e: FormEvent| {
                            if let Some(dt) = recombine(None, Some(&e.value()), cur) {
                                on_change.call(dt);
                            }
                        },
                    }
                }
            }
        }
    }
}

fn recombine(
    date: Option<&str>,
    time: Option<&str>,
    current: chrono::DateTime<chrono::Utc>,
) -> Option<chrono::DateTime<chrono::Utc>> {
    let cur_naive = current.naive_utc();
    let new_date = match date {
        Some(s) => NaiveDate::parse_from_str(s, "%Y-%m-%d").ok()?,
        None => NaiveDate::from_ymd_opt(cur_naive.year(), cur_naive.month(), cur_naive.day())?,
    };
    let new_time = match time {
        Some(s) => NaiveTime::parse_from_str(s, "%H:%M").ok()?,
        None => NaiveTime::from_hms_opt(cur_naive.hour(), cur_naive.minute(), 0)?,
    };
    let combined = NaiveDateTime::new(new_date, new_time);
    Some(Utc.from_utc_datetime(&combined))
}
