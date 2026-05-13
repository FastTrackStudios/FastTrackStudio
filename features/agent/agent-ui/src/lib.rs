//! Agent feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! - [`AgentRunList`]  — full collection view, dispatches `on_delete`
//! - [`AgentRunRow`]   — single-row presentation
//! - [`AgentRunCreateForm`] — minimal new-run form, emits the create payload

use agent_proto::{AgentRun, AgentRunCreate};
use dioxus::prelude::*;
use uuid::Uuid;

#[component]
pub fn AgentRunList(items: Vec<AgentRun>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            div { class: "text-sm text-slate-500",
                "No agent runs yet. Add one above."
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for run in items.iter().cloned() {
                AgentRunRow {
                    key: "{run.id}",
                    run: run.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn AgentRunRow(run: AgentRun, on_delete: EventHandler<Uuid>) -> Element {
    let id = run.id;
    rsx! {
        div { class: "flex items-center justify-between rounded-md border border-slate-800 bg-slate-900 px-4 py-3",
            div { class: "flex flex-col",
                span { class: "text-sm font-medium text-slate-100", "{run.name}" }
                span { class: "text-xs text-slate-500",
                    "{run.kind} · {run.status}"
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
pub fn AgentRunCreateForm(on_submit: EventHandler<AgentRunCreate>) -> Element {
    let mut name = use_signal(String::new);
    let mut kind = use_signal(String::new);
    let mut prompt = use_signal(String::new);
    rsx! {
        form {
            class: "flex flex-wrap gap-2",
            onsubmit: move |evt| {
                evt.prevent_default();
                let n = name.read().clone();
                let k = kind.read().clone();
                let p = prompt.read().clone();
                if n.trim().is_empty() || k.trim().is_empty() || p.trim().is_empty() {
                    return;
                }
                let payload = AgentRunCreate {
                    name: n,
                    kind: k,
                    prompt: p,
                    status: "queued".into(),
                    task_id: None,
                    started_at: None,
                    completed_at: None,
                    result: None,
                    error_message: None,
                    tokens_used: None,
                    cost_cents: None,
                    tags: Vec::new(),
                };
                on_submit.call(payload);
                name.set(String::new());
                kind.set(String::new());
                prompt.set(String::new());
            },
            input {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Name (required)",
                value: "{name}",
                oninput: move |evt| name.set(evt.value()),
            }
            input {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Kind (required)",
                value: "{kind}",
                oninput: move |evt| kind.set(evt.value()),
            }
            textarea {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Prompt (required)",
                value: "{prompt}",
                oninput: move |evt| prompt.set(evt.value()),
            }
            button {
                r#type: "submit",
                class: "rounded-md bg-cyan-500 px-4 py-2 text-sm font-semibold text-slate-950 hover:bg-cyan-400",
                "Add run"
            }
        }
    }
}
