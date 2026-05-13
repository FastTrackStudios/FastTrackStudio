//! Timer feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! The component split mirrors the timer domain:
//!
//! - [`TimeEntryList`]       — full collection view, dispatches `on_delete` / `on_stop`
//! - [`TimeEntryRow`]        — single-row presentation (composable into other lists)
//! - [`TimeEntryCreateForm`] — minimal "start a timer" form, emits the create payload

use chrono::{DateTime, Utc};
use dioxus::prelude::*;
use timer_proto::{TimeEntry, TimeEntryCreate};
use uuid::Uuid;

#[component]
pub fn TimeEntryList(
    items: Vec<TimeEntry>,
    on_delete: EventHandler<Uuid>,
    on_stop: EventHandler<Uuid>,
) -> Element {
    if items.is_empty() {
        return rsx! {
            div { class: "text-sm text-slate-500",
                "No time entries yet. Start a timer above."
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for entry in items.iter().cloned() {
                TimeEntryRow {
                    key: "{entry.id}",
                    entry: entry.clone(),
                    on_delete: move |id| on_delete.call(id),
                    on_stop: move |id| on_stop.call(id),
                }
            }
        }
    }
}

#[component]
pub fn TimeEntryRow(
    entry: TimeEntry,
    on_delete: EventHandler<Uuid>,
    on_stop: EventHandler<Uuid>,
) -> Element {
    let id = entry.id;
    let running = entry.end_time.is_none();
    let description = entry
        .description
        .clone()
        .unwrap_or_else(|| "(no description)".into());
    let user = entry.user.clone().unwrap_or_else(|| "unknown".into());
    let duration = duration_string(entry.start_time, entry.end_time);
    rsx! {
        div { class: "flex items-center justify-between rounded-md border border-slate-800 bg-slate-900 px-4 py-3",
            div { class: "flex flex-col",
                span { class: "text-sm font-medium text-slate-100", "{description}" }
                span { class: "text-xs text-slate-500",
                    "{user} · {duration}"
                    if running {
                        span { class: "text-yellow-300", " · running" }
                    }
                }
            }
            div { class: "flex items-center gap-3",
                if running {
                    button {
                        class: "text-xs text-slate-400 hover:text-yellow-300",
                        onclick: move |_| on_stop.call(id),
                        "Stop"
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
}

#[component]
pub fn TimeEntryCreateForm(on_submit: EventHandler<TimeEntryCreate>) -> Element {
    let mut description = use_signal(String::new);
    let mut user = use_signal(String::new);
    rsx! {
        form {
            class: "flex flex-wrap gap-2",
            onsubmit: move |evt| {
                evt.prevent_default();
                let payload = TimeEntryCreate {
                    task_id: None,
                    user: trim_to_option(user.read().clone()),
                    start_time: Utc::now(),
                    end_time: None,
                    description: trim_to_option(description.read().clone()),
                    billable: false,
                    billable_rate_cents: None,
                    tags: Vec::new(),
                    invoiced_at: None,
                };
                on_submit.call(payload);
                description.set(String::new());
                user.set(String::new());
            },
            input {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Description",
                value: "{description}",
                oninput: move |evt| description.set(evt.value()),
            }
            input {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "User (e.g. cody)",
                value: "{user}",
                oninput: move |evt| user.set(evt.value()),
            }
            button {
                r#type: "submit",
                class: "rounded-md bg-cyan-500 px-4 py-2 text-sm font-semibold text-slate-950 hover:bg-cyan-400",
                "Start timer"
            }
        }
    }
}

fn duration_string(start: DateTime<Utc>, end: Option<DateTime<Utc>>) -> String {
    let end = end.unwrap_or_else(Utc::now);
    let total = (end - start).num_seconds().max(0);
    if total < 60 {
        format!("{}s", total)
    } else {
        let m = total / 60;
        let s = total % 60;
        format!("{}m {}s", m, s)
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
