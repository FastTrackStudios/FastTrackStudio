//! Fitness feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! v1 scope: `WorkoutSession` only. Exercise / Routine / SetLog / BodyMeasurement come later.

use chrono::Utc;
use dioxus::prelude::*;
use fitness_proto::{WorkoutSession, WorkoutSessionCreate};
use uuid::Uuid;

#[component]
pub fn WorkoutSessionList(items: Vec<WorkoutSession>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            div { class: "text-sm text-slate-500",
                "No workout sessions yet. Add one above."
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for session in items.iter().cloned() {
                WorkoutSessionRow {
                    key: "{session.id}",
                    session: session.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn WorkoutSessionRow(session: WorkoutSession, on_delete: EventHandler<Uuid>) -> Element {
    let id = session.id;
    let started = session.started_at.format("%Y-%m-%d %H:%M").to_string();
    let mood = session.mood.clone().unwrap_or_else(|| "ok".into());
    let meta = format!("{} · {}", started, mood);
    let live = session.ended_at.is_none();
    rsx! {
        div { class: "flex items-center justify-between rounded-md border border-slate-800 bg-slate-900 px-4 py-3",
            div { class: "flex flex-col",
                div { class: "flex items-center gap-2",
                    span { class: "text-sm font-medium text-slate-100", "{session.name}" }
                    if live {
                        span { class: "rounded-sm bg-yellow-500/20 px-1.5 py-0.5 text-[10px] font-semibold uppercase text-yellow-400",
                            "live"
                        }
                    }
                }
                span { class: "text-xs text-slate-500", "{meta}" }
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
pub fn WorkoutSessionCreateForm(on_submit: EventHandler<WorkoutSessionCreate>) -> Element {
    let mut name = use_signal(String::new);
    let mut mood = use_signal(String::new);
    rsx! {
        form {
            class: "flex flex-wrap gap-2",
            onsubmit: move |evt| {
                evt.prevent_default();
                let n = name.read().clone();
                if n.trim().is_empty() {
                    return;
                }
                let payload = WorkoutSessionCreate {
                    routine_id: None,
                    name: n,
                    started_at: Utc::now(),
                    ended_at: None,
                    notes: None,
                    mood: trim_to_option(mood.read().clone()),
                    tags: Vec::new(),
                };
                on_submit.call(payload);
                name.set(String::new());
                mood.set(String::new());
            },
            input {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Name (required)",
                value: "{name}",
                oninput: move |evt| name.set(evt.value()),
            }
            input {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Mood",
                value: "{mood}",
                oninput: move |evt| mood.set(evt.value()),
            }
            button {
                r#type: "submit",
                class: "rounded-md bg-cyan-500 px-4 py-2 text-sm font-semibold text-slate-950 hover:bg-cyan-400",
                "Start session"
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
