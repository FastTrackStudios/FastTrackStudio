//! Calendar feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! The component split mirrors the calendar domain:
//!
//! - [`CalendarEventList`]       — full collection view, dispatches `on_delete`
//! - [`CalendarEventRow`]        — single-row presentation (composable into other lists)
//! - [`CalendarEventCreateForm`] — minimal new-event form, emits the create payload

use calendar_proto::{CalendarEvent, CalendarEventCreate};
use chrono::{Duration, Utc};
use dioxus::prelude::*;
use uuid::Uuid;

#[component]
pub fn CalendarEventList(items: Vec<CalendarEvent>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            div { class: "text-sm text-slate-500",
                "No calendar events yet. Add one above."
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for event in items.iter().cloned() {
                CalendarEventRow {
                    key: "{event.id}",
                    event: event.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn CalendarEventRow(event: CalendarEvent, on_delete: EventHandler<Uuid>) -> Element {
    let id = event.id;
    let when = event.start_at.format("%Y-%m-%d %H:%M").to_string();
    rsx! {
        div { class: "flex items-center justify-between rounded-md border border-slate-800 bg-slate-900 px-4 py-3",
            div { class: "flex flex-col",
                span { class: "text-sm font-medium text-slate-100", "{event.title}" }
                span { class: "text-xs text-slate-500",
                    "{when} · {event.status}"
                }
            }
            button {
                class: "text-xs text-slate-500 hover:text-rose-400",
                onclick: move |_| on_delete.call(id),
                "Delete"
            }
        }
    }
}

#[component]
pub fn CalendarEventCreateForm(on_submit: EventHandler<CalendarEventCreate>) -> Element {
    let mut title = use_signal(String::new);
    let mut description = use_signal(String::new);
    rsx! {
        form {
            class: "flex flex-wrap gap-2",
            onsubmit: move |evt| {
                evt.prevent_default();
                let t = title.read().clone();
                if t.trim().is_empty() {
                    return;
                }
                let start_at = Utc::now();
                let end_at = start_at + Duration::hours(1);
                let payload = CalendarEventCreate {
                    title: t,
                    description: trim_to_option(description.read().clone()),
                    start_at,
                    end_at,
                    all_day: false,
                    location_id: None,
                    location_text: None,
                    rrule: None,
                    organizer: None,
                    attendees: Vec::new(),
                    calendar_id: None,
                    status: "confirmed".into(),
                    tags: Vec::new(),
                };
                on_submit.call(payload);
                title.set(String::new());
                description.set(String::new());
            },
            input {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Title (required)",
                value: "{title}",
                oninput: move |evt| title.set(evt.value()),
            }
            textarea {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Description",
                value: "{description}",
                oninput: move |evt| description.set(evt.value()),
            }
            button {
                r#type: "submit",
                class: "rounded-md bg-cyan-500 px-4 py-2 text-sm font-semibold text-slate-950 hover:bg-cyan-400",
                "Add event"
            }
        }
    }
}

fn trim_to_option(s: String) -> Option<String> {
    let t = s.trim();
    if t.is_empty() {
        None
    } else {
        Some(t.to_string())
    }
}
