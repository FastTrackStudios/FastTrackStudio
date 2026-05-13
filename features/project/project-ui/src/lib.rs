//! Project feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! Scoped to the `Project` entity for v1; Task/Cycle/Milestone components
//! land in later iterations.
//!
//! - [`ProjectList`]       — full collection view, dispatches `on_delete`
//! - [`ProjectRow`]        — single-row presentation (composable into other lists)
//! - [`ProjectCreateForm`] — minimal new-project form, emits the create payload

use dioxus::prelude::*;
use project_proto::{Project, ProjectCreate};
use uuid::Uuid;

#[component]
pub fn ProjectList(items: Vec<Project>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            div { class: "text-sm text-slate-500",
                "No projects yet. Add one above."
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for project in items.iter().cloned() {
                ProjectRow {
                    key: "{project.id}",
                    project: project.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn ProjectRow(project: Project, on_delete: EventHandler<Uuid>) -> Element {
    let id = project.id;
    let kind = project
        .project_type
        .clone()
        .unwrap_or_else(|| "project".into());
    let swatch_style = match project.color.as_deref() {
        Some(c) if !c.trim().is_empty() => format!("background-color: {}", c),
        _ => "background-color: transparent".to_string(),
    };
    rsx! {
        div { class: "flex items-center justify-between rounded-md border border-slate-800 bg-slate-900 px-4 py-3",
            div { class: "flex items-center gap-3",
                span {
                    class: "h-1.5 w-1.5 rounded-sm border border-slate-700",
                    style: "{swatch_style}",
                }
                div { class: "flex flex-col",
                    span { class: "text-sm font-bold text-slate-100", "{project.name}" }
                    span { class: "text-xs text-slate-500",
                        "{project.status} · {kind}"
                    }
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
pub fn ProjectCreateForm(on_submit: EventHandler<ProjectCreate>) -> Element {
    let mut name = use_signal(String::new);
    let mut description = use_signal(String::new);
    let mut project_type = use_signal(String::new);
    let mut color = use_signal(String::new);
    rsx! {
        form {
            class: "flex flex-col gap-2",
            onsubmit: move |evt| {
                evt.prevent_default();
                let n = name.read().clone();
                if n.trim().is_empty() {
                    return;
                }
                let payload = ProjectCreate {
                    name: n,
                    description: trim_to_option(description.read().clone()),
                    status: "active".into(),
                    project_type: trim_to_option(project_type.read().clone()),
                    color: trim_to_option(color.read().clone()),
                    owner: None,
                };
                on_submit.call(payload);
                name.set(String::new());
                description.set(String::new());
                project_type.set(String::new());
                color.set(String::new());
            },
            div { class: "flex flex-wrap gap-2",
                input {
                    class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                    placeholder: "Name (required)",
                    value: "{name}",
                    oninput: move |evt| name.set(evt.value()),
                }
                input {
                    class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                    placeholder: "audio-production",
                    value: "{project_type}",
                    oninput: move |evt| project_type.set(evt.value()),
                }
                input {
                    class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                    placeholder: "#06b6d4",
                    value: "{color}",
                    oninput: move |evt| color.set(evt.value()),
                }
            }
            textarea {
                class: "min-h-20 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Description (optional)",
                value: "{description}",
                oninput: move |evt| description.set(evt.value()),
            }
            div { class: "flex justify-end",
                button {
                    r#type: "submit",
                    class: "rounded-md bg-cyan-500 px-4 py-2 text-sm font-semibold text-slate-950 hover:bg-cyan-400",
                    "Add project"
                }
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
