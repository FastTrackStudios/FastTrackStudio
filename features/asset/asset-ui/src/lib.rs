//! Asset feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! The component split mirrors the asset domain:
//!
//! - [`AssetList`]  — full collection view, dispatches `on_delete`
//! - [`AssetRow`]   — single-row presentation (composable into other lists)
//! - [`AssetCreateForm`] — minimal new-asset form, emits the create payload

use asset_proto::{Asset, AssetCreate};
use dioxus::prelude::*;
use uuid::Uuid;

#[component]
pub fn AssetList(items: Vec<Asset>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            div { class: "text-sm text-slate-500",
                "No assets yet. Add one above."
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for asset in items.iter().cloned() {
                AssetRow {
                    key: "{asset.id}",
                    asset: asset.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn AssetRow(asset: Asset, on_delete: EventHandler<Uuid>) -> Element {
    let id = asset.id;
    let manufacturer = asset.manufacturer.clone().unwrap_or_else(|| "—".into());
    let model = asset.model.clone().unwrap_or_else(|| "—".into());
    rsx! {
        div { class: "flex items-center justify-between rounded-md border border-slate-800 bg-slate-900 px-4 py-3",
            div { class: "flex flex-col",
                span { class: "text-sm font-medium text-slate-100", "{asset.name}" }
                span { class: "text-xs text-slate-500",
                    "{manufacturer} · {model} · {asset.status}"
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
pub fn AssetCreateForm(on_submit: EventHandler<AssetCreate>) -> Element {
    let mut name = use_signal(String::new);
    let mut manufacturer = use_signal(String::new);
    let mut model = use_signal(String::new);
    rsx! {
        form {
            class: "flex flex-wrap gap-2",
            onsubmit: move |evt| {
                evt.prevent_default();
                let n = name.read().clone();
                if n.trim().is_empty() {
                    return;
                }
                let payload = AssetCreate {
                    name: n,
                    status: "active".into(),
                    manufacturer: trim_to_option(manufacturer.read().clone()),
                    model: trim_to_option(model.read().clone()),
                    serial_number: None,
                    owner_id: None,
                    location_id: None,
                    notes: None,
                    tags: Vec::new(),
                    acquired_at: None,
                };
                on_submit.call(payload);
                name.set(String::new());
                manufacturer.set(String::new());
                model.set(String::new());
            },
            input {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Name (required)",
                value: "{name}",
                oninput: move |evt| name.set(evt.value()),
            }
            input {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Manufacturer",
                value: "{manufacturer}",
                oninput: move |evt| manufacturer.set(evt.value()),
            }
            input {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Model",
                value: "{model}",
                oninput: move |evt| model.set(evt.value()),
            }
            button {
                r#type: "submit",
                class: "rounded-md bg-cyan-500 px-4 py-2 text-sm font-semibold text-slate-950 hover:bg-cyan-400",
                "Add asset"
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
