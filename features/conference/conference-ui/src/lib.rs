//! Conference feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! - [`MeetingList`]  — full collection view, dispatches `on_delete`
//! - [`MeetingRow`]   — single-row presentation
//! - [`MeetingCreateForm`] — minimal new-meeting form, emits the create payload

use conference_proto::{Meeting, MeetingCreate};
use dioxus::prelude::*;
use uuid::Uuid;

#[component]
pub fn MeetingList(items: Vec<Meeting>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            div { class: "text-sm text-slate-500",
                "No meetings yet. Add one above."
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for meeting in items.iter().cloned() {
                MeetingRow {
                    key: "{meeting.id}",
                    meeting: meeting.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn MeetingRow(meeting: Meeting, on_delete: EventHandler<Uuid>) -> Element {
    let id = meeting.id;
    let scheduled = meeting.scheduled_at.format("%Y-%m-%d %H:%M").to_string();
    rsx! {
        div { class: "flex items-center justify-between rounded-md border border-slate-800 bg-slate-900 px-4 py-3",
            div { class: "flex flex-col",
                span { class: "text-sm font-medium text-slate-100", "{meeting.name}" }
                span { class: "text-xs text-slate-500",
                    "{meeting.status} · scheduled {scheduled}"
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
pub fn MeetingCreateForm(on_submit: EventHandler<MeetingCreate>) -> Element {
    let mut name = use_signal(String::new);
    let mut host_user = use_signal(String::new);
    rsx! {
        form {
            class: "flex flex-wrap gap-2",
            onsubmit: move |evt| {
                evt.prevent_default();
                let n = name.read().clone();
                if n.trim().is_empty() {
                    return;
                }
                let payload = MeetingCreate {
                    name: n,
                    host_user: trim_to_option(host_user.read().clone()),
                    scheduled_at: chrono::Utc::now(),
                    started_at: None,
                    ended_at: None,
                    status: "scheduled".into(),
                    recording_url: None,
                    notes: None,
                    participants: Vec::new(),
                    tags: Vec::new(),
                };
                on_submit.call(payload);
                name.set(String::new());
                host_user.set(String::new());
            },
            input {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Name (required)",
                value: "{name}",
                oninput: move |evt| name.set(evt.value()),
            }
            input {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Host user",
                value: "{host_user}",
                oninput: move |evt| host_user.set(evt.value()),
            }
            button {
                r#type: "submit",
                class: "rounded-md bg-cyan-500 px-4 py-2 text-sm font-semibold text-slate-950 hover:bg-cyan-400",
                "Add meeting"
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
