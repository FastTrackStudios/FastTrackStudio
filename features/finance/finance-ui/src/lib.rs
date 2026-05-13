//! Finance feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! v1 covers `Revenue` only; `Expense` UI lands later.
//!
//! - [`RevenueList`]        — full collection view, dispatches `on_delete`
//! - [`RevenueRow`]         — single-row presentation
//! - [`RevenueCreateForm`]  — minimal new-revenue form, emits create payload

use chrono::Utc;
use dioxus::prelude::*;
use finance_proto::{Revenue, RevenueCreate};
use uuid::Uuid;

#[component]
pub fn RevenueList(items: Vec<Revenue>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            div { class: "text-sm text-slate-500",
                "No revenue yet. Add one above."
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for rev in items.iter().cloned() {
                RevenueRow {
                    key: "{rev.id}",
                    rev: rev.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn RevenueRow(rev: Revenue, on_delete: EventHandler<Uuid>) -> Element {
    let id = rev.id;
    let meta = format!(
        "{} {} · {}",
        format_cents(rev.amount_cents),
        rev.currency,
        rev.received_at.format("%Y-%m-%d")
    );
    rsx! {
        div { class: "flex items-center justify-between rounded-md border border-slate-800 bg-slate-900 px-4 py-3",
            div { class: "flex flex-col",
                span { class: "text-sm font-medium text-slate-100", "{rev.source}" }
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
pub fn RevenueCreateForm(on_submit: EventHandler<RevenueCreate>) -> Element {
    let mut source = use_signal(String::new);
    let mut amount = use_signal(String::new);
    let mut currency = use_signal(|| "USD".to_string());
    rsx! {
        form {
            class: "flex flex-wrap gap-2",
            onsubmit: move |evt| {
                evt.prevent_default();
                let s = source.read().clone();
                let a = amount.read().clone();
                let c = currency.read().clone();
                if s.trim().is_empty() || c.trim().is_empty() {
                    return;
                }
                let amount_cents = match a.trim().parse::<i64>() {
                    Ok(v) => v,
                    Err(_) => return,
                };
                let payload = RevenueCreate {
                    source: s,
                    client_id: None,
                    invoice_id: None,
                    amount_cents,
                    currency: c,
                    received_at: Utc::now(),
                    notes: None,
                    tags: Vec::new(),
                };
                on_submit.call(payload);
                source.set(String::new());
                amount.set(String::new());
                currency.set("USD".into());
            },
            input {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Source (required)",
                value: "{source}",
                oninput: move |evt| source.set(evt.value()),
            }
            input {
                class: "w-32 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Amount (cents)",
                r#type: "number",
                value: "{amount}",
                oninput: move |evt| amount.set(evt.value()),
            }
            input {
                class: "w-24 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "USD",
                maxlength: "3",
                value: "{currency}",
                oninput: move |evt| currency.set(evt.value()),
            }
            button {
                r#type: "submit",
                class: "rounded-md bg-cyan-500 px-4 py-2 text-sm font-semibold text-slate-950 hover:bg-cyan-400",
                "Add revenue"
            }
        }
    }
}

fn format_cents(c: i64) -> String {
    format!("{:.2}", c as f64 / 100.0)
}
