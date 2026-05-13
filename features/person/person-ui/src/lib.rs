//! Person feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! Scoped to the `Person` entity for v1; `Client` and `Team` come later.
//!
//! - [`PersonList`]       — full collection view, dispatches `on_delete`
//! - [`PersonRow`]        — single-row presentation (composable into other lists)
//! - [`PersonCreateForm`] — minimal new-person form, emits the create payload

use dioxus::prelude::*;
use person_proto::{Person, PersonCreate};
use uuid::Uuid;

#[component]
pub fn PersonList(items: Vec<Person>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            div { class: "text-sm text-slate-500",
                "No people yet. Add one above."
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for person in items.iter().cloned() {
                PersonRow {
                    key: "{person.id}",
                    person: person.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn PersonRow(person: Person, on_delete: EventHandler<Uuid>) -> Element {
    let id = person.id;
    let meta = [person.email.clone(), person.role.clone()]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>()
        .join(" · ");
    rsx! {
        div { class: "flex items-center justify-between rounded-md border border-slate-800 bg-slate-900 px-4 py-3",
            div { class: "flex flex-col",
                span { class: "text-sm font-medium text-slate-100", "{person.name}" }
                if !meta.is_empty() {
                    span { class: "text-xs text-slate-500", "{meta}" }
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
pub fn PersonCreateForm(on_submit: EventHandler<PersonCreate>) -> Element {
    let mut name = use_signal(String::new);
    let mut email = use_signal(String::new);
    let mut phone = use_signal(String::new);
    let mut role = use_signal(String::new);
    rsx! {
        form {
            class: "flex flex-wrap gap-2",
            onsubmit: move |evt| {
                evt.prevent_default();
                let n = name.read().clone();
                if n.trim().is_empty() {
                    return;
                }
                let payload = PersonCreate {
                    name: n,
                    email: trim_to_option(email.read().clone()),
                    phone: trim_to_option(phone.read().clone()),
                    role: trim_to_option(role.read().clone()),
                    client_id: None,
                    team_id: None,
                    notes: None,
                    tags: Vec::new(),
                };
                on_submit.call(payload);
                name.set(String::new());
                email.set(String::new());
                phone.set(String::new());
                role.set(String::new());
            },
            input {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Name (required)",
                value: "{name}",
                oninput: move |evt| name.set(evt.value()),
            }
            input {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Email",
                value: "{email}",
                oninput: move |evt| email.set(evt.value()),
            }
            input {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Phone",
                value: "{phone}",
                oninput: move |evt| phone.set(evt.value()),
            }
            input {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Role",
                value: "{role}",
                oninput: move |evt| role.set(evt.value()),
            }
            button {
                r#type: "submit",
                class: "rounded-md bg-cyan-500 px-4 py-2 text-sm font-semibold text-slate-950 hover:bg-cyan-400",
                "Add person"
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
