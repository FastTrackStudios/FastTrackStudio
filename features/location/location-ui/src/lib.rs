//! Location feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! The component split mirrors the location domain:
//!
//! - [`LocationList`]  — full collection view, dispatches `on_delete`
//! - [`LocationRow`]   — single-row presentation (composable into other lists)
//! - [`LocationCreateForm`] — minimal new-location form, emits the create payload

use dioxus::prelude::*;
use location_proto::{Location, LocationCreate};
use uuid::Uuid;

#[component]
pub fn LocationList(items: Vec<Location>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            div { class: "text-sm text-slate-500",
                "No locations yet. Add one above."
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for location in items.iter().cloned() {
                LocationRow {
                    key: "{location.id}",
                    location: location.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn LocationRow(location: Location, on_delete: EventHandler<Uuid>) -> Element {
    let id = location.id;
    let locality_parts: Vec<String> = [
        location.city.clone(),
        location.state.clone(),
        location.country_code.clone(),
    ]
    .into_iter()
    .flatten()
    .filter(|s| !s.trim().is_empty())
    .collect();
    let locality = locality_parts.join(" · ");
    rsx! {
        div { class: "flex items-center justify-between rounded-md border border-slate-800 bg-slate-900 px-4 py-3",
            div { class: "flex flex-col",
                span { class: "text-sm font-medium text-slate-100", "{location.name}" }
                if !locality.is_empty() {
                    span { class: "text-xs text-slate-500", "{locality}" }
                }
                if let Some(kind) = location.kind.clone() {
                    span { class: "text-xs text-slate-500", "{kind}" }
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
pub fn LocationCreateForm(on_submit: EventHandler<LocationCreate>) -> Element {
    let mut name = use_signal(String::new);
    let mut city = use_signal(String::new);
    let mut state = use_signal(String::new);
    let mut country_code = use_signal(String::new);
    rsx! {
        form {
            class: "flex flex-wrap gap-2",
            onsubmit: move |evt| {
                evt.prevent_default();
                let n = name.read().clone();
                if n.trim().is_empty() {
                    return;
                }
                let payload = LocationCreate {
                    name: n,
                    kind: None,
                    address1: None,
                    address2: None,
                    city: trim_to_option(city.read().clone()),
                    state: trim_to_option(state.read().clone()),
                    postal_code: None,
                    country_code: trim_to_option(country_code.read().clone()),
                    contact_name: None,
                    contact_email: None,
                    parent_id: None,
                    notes: None,
                    tags: Vec::new(),
                };
                on_submit.call(payload);
                name.set(String::new());
                city.set(String::new());
                state.set(String::new());
                country_code.set(String::new());
            },
            input {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Name (required)",
                value: "{name}",
                oninput: move |evt| name.set(evt.value()),
            }
            input {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "City",
                value: "{city}",
                oninput: move |evt| city.set(evt.value()),
            }
            input {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "State",
                value: "{state}",
                oninput: move |evt| state.set(evt.value()),
            }
            input {
                class: "w-24 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "CC",
                maxlength: "2",
                value: "{country_code}",
                oninput: move |evt| country_code.set(evt.value()),
            }
            button {
                r#type: "submit",
                class: "rounded-md bg-cyan-500 px-4 py-2 text-sm font-semibold text-slate-950 hover:bg-cyan-400",
                "Add location"
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
