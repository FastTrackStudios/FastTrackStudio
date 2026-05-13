//! Inventory feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! v1 scope: `PantryItem` only. `FoodProduct` / `ShoppingListItem` come later.

use dioxus::prelude::*;
use inventory_proto::{PantryItem, PantryItemCreate};
use uuid::Uuid;

#[component]
pub fn PantryItemList(items: Vec<PantryItem>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            div { class: "text-sm text-slate-500",
                "No pantry items yet. Add one above."
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for item in items.iter().cloned() {
                PantryItemRow {
                    key: "{item.id}",
                    item: item.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn PantryItemRow(item: PantryItem, on_delete: EventHandler<Uuid>) -> Element {
    let id = item.id;
    let qty = format_qty(item.qty_thousandths, &item.unit);
    let location = item.location.clone().unwrap_or_else(|| "pantry".into());
    let meta = format!("{} · {}", qty, location);
    rsx! {
        div { class: "flex items-center justify-between rounded-md border border-slate-800 bg-slate-900 px-4 py-3",
            div { class: "flex flex-col",
                span { class: "text-sm font-medium text-slate-100", "{item.name}" }
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
pub fn PantryItemCreateForm(on_submit: EventHandler<PantryItemCreate>) -> Element {
    let mut name = use_signal(String::new);
    let mut qty = use_signal(String::new);
    let mut unit = use_signal(|| "g".to_string());
    rsx! {
        form {
            class: "flex flex-wrap gap-2",
            onsubmit: move |evt| {
                evt.prevent_default();
                let n = name.read().clone();
                if n.trim().is_empty() {
                    return;
                }
                let q_str = qty.read().clone();
                let q_f: f64 = match q_str.trim().parse() {
                    Ok(v) => v,
                    Err(_) => return,
                };
                let u = unit.read().clone();
                if u.trim().is_empty() {
                    return;
                }
                let payload = PantryItemCreate {
                    product_id: None,
                    name: n,
                    qty_thousandths: (q_f * 1000.0) as i64,
                    unit: u,
                    location: Some("pantry".into()),
                    expires_at: None,
                    opened_at: None,
                    notes: None,
                    tags: Vec::new(),
                };
                on_submit.call(payload);
                name.set(String::new());
                qty.set(String::new());
                unit.set("g".into());
            },
            input {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Name (required)",
                value: "{name}",
                oninput: move |evt| name.set(evt.value()),
            }
            input {
                class: "w-28 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Qty",
                r#type: "number",
                step: "any",
                value: "{qty}",
                oninput: move |evt| qty.set(evt.value()),
            }
            input {
                class: "w-24 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Unit",
                value: "{unit}",
                oninput: move |evt| unit.set(evt.value()),
            }
            button {
                r#type: "submit",
                class: "rounded-md bg-cyan-500 px-4 py-2 text-sm font-semibold text-slate-950 hover:bg-cyan-400",
                "Add item"
            }
        }
    }
}

fn format_qty(q: i64, unit: &str) -> String {
    let raw = format!("{:.3}", q as f64 / 1000.0);
    let trimmed = if raw.contains('.') {
        let t = raw.trim_end_matches('0');
        let t = t.trim_end_matches('.');
        t.to_string()
    } else {
        raw
    };
    format!("{} {}", trimmed, unit)
}
