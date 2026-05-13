//! Invoice feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! v1 covers the `Invoice` header only; `InvoiceLine` UI lands later.
//!
//! - [`InvoiceList`]        — full collection view, dispatches `on_delete`
//! - [`InvoiceRow`]         — single-row presentation
//! - [`InvoiceCreateForm`]  — minimal new-invoice form, emits create payload

use chrono::Utc;
use dioxus::prelude::*;
use invoice_proto::{Invoice, InvoiceCreate};
use uuid::Uuid;

#[component]
pub fn InvoiceList(items: Vec<Invoice>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            div { class: "text-sm text-slate-500",
                "No invoices yet. Add one above."
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for invoice in items.iter().cloned() {
                InvoiceRow {
                    key: "{invoice.id}",
                    invoice: invoice.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn InvoiceRow(invoice: Invoice, on_delete: EventHandler<Uuid>) -> Element {
    let id = invoice.id;
    let meta = format!(
        "{} · {} {}",
        invoice.status,
        invoice.currency,
        format_cents(invoice.total_cents)
    );
    rsx! {
        div { class: "flex items-center justify-between rounded-md border border-slate-800 bg-slate-900 px-4 py-3",
            div { class: "flex flex-col",
                span { class: "text-sm font-medium text-slate-100", "{invoice.number}" }
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
pub fn InvoiceCreateForm(on_submit: EventHandler<InvoiceCreate>) -> Element {
    let mut number = use_signal(String::new);
    let mut currency = use_signal(|| "USD".to_string());
    rsx! {
        form {
            class: "flex flex-wrap gap-2",
            onsubmit: move |evt| {
                evt.prevent_default();
                let n = number.read().clone();
                let c = currency.read().clone();
                if n.trim().is_empty() || c.trim().is_empty() {
                    return;
                }
                let payload = InvoiceCreate {
                    number: n,
                    client_id: Uuid::new_v4(),
                    status: "draft".into(),
                    issue_date: Utc::now(),
                    due_date: None,
                    paid_at: None,
                    currency: c,
                    subtotal_cents: 0,
                    tax_cents: 0,
                    total_cents: 0,
                    notes: None,
                    tags: Vec::new(),
                };
                on_submit.call(payload);
                number.set(String::new());
                currency.set("USD".into());
            },
            input {
                class: "flex-1 min-w-40 rounded-md border border-slate-700 bg-slate-900 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500",
                placeholder: "Number (required)",
                value: "{number}",
                oninput: move |evt| number.set(evt.value()),
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
                "Add invoice"
            }
        }
    }
}

fn format_cents(c: i64) -> String {
    format!("{:.2}", c as f64 / 100.0)
}
