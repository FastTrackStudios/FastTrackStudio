//! Invoice feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! v1 covers the `Invoice` header only; the Invoice-Ninja-style
//! invoicing surface lives in [`ninja`].
//!
//! - [`InvoiceList`]        — full collection view, dispatches `on_delete`
//! - [`InvoiceRow`]         — single-row presentation
//! - [`InvoiceCreateForm`]  — minimal new-invoice form, emits create payload

pub mod ninja;
pub use ninja::*;

use chrono::Utc;
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{CheckCheck, CircleAlert, Clock, FileText, Plus, Receipt, Trash2};
use fts_ui::prelude::*;
use invoice_proto::{Invoice, InvoiceCreate};
use uuid::Uuid;

#[component]
pub fn InvoiceList(items: Vec<Invoice>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            EmptyState {
                message: "No invoices yet. Add one above.",
                icon: rsx! { FileText { size: 32 } },
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
    let meta = format!("{} {}", invoice.currency, format_cents(invoice.total_cents));
    let number = invoice.number.clone();
    let status = invoice.status.clone();
    let status_variant = match status.as_str() {
        "paid" => StatusBadgeVariant::Success,
        "overdue" => StatusBadgeVariant::Danger,
        "sent" => StatusBadgeVariant::Neutral,
        _ => StatusBadgeVariant::Warning,
    };
    rsx! {
        Item {
            ItemContent {
                ItemTitle { "{number}" }
                ItemDescription { "{meta}" }
            }
            ItemActions { class: "gap-2",
                StatusBadge { variant: status_variant, label: "{status}" }
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: move |_| on_delete.call(id),
                    Trash2 { size: 14 }
                }
            }
        }
    }
}

#[component]
pub fn InvoiceCreateForm(on_submit: EventHandler<InvoiceCreate>) -> Element {
    let mut number = use_signal(String::new);
    let mut currency = use_signal(|| "USD".to_string());

    rsx! {
        Card {
            CardHeader {
                CardTitle { "Add invoice" }
            }
            CardContent { class: "flex flex-col gap-3",
                div { class: "flex flex-wrap gap-2",
                    Input {
                        value: number,
                        placeholder: "Number (required)",
                        class: "flex-1 min-w-40",
                    }
                    Input {
                        value: currency,
                        placeholder: "USD",
                        class: "w-24",
                    }
                }
                div { class: "flex items-center gap-3",
                    div { class: "flex-1" }
                    Button {
                        on_click: move |_| {
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
                                discount_cents: 0,
                                tax_rate_bps: 0,
                                tax_inclusive: false,
                                tax_cents: 0,
                                total_cents: 0,
                                balance_cents: 0,
                                notes: None,
                                tags: Vec::new(),
                            };
                            on_submit.call(payload);
                            number.set(String::new());
                            currency.set("USD".into());
                        },
                        Plus { size: 14 }
                        " Add invoice"
                    }
                }
            }
        }
    }
}

fn format_cents(c: i64) -> String {
    format!("{:.2}", c as f64 / 100.0)
}

/// Purpose-built invoice dashboard. Summarises totals by status and lets the
/// user pivot the list by paid / outstanding / overdue.
#[component]
pub fn InvoiceDashboard(
    items: Vec<Invoice>,
    status: String,
    on_create: EventHandler<InvoiceCreate>,
    on_delete: EventHandler<Uuid>,
) -> Element {
    let mut tab = use_signal(|| "all".to_string());

    let total = items.len();
    let now = Utc::now();

    let mut paid_cents: i64 = 0;
    let mut outstanding_cents: i64 = 0;
    let mut overdue_cents: i64 = 0;
    let mut paid_n = 0usize;
    let mut outstanding_n = 0usize;
    let mut overdue_n = 0usize;
    for inv in &items {
        match inv.status.as_str() {
            "paid" => {
                paid_cents += inv.total_cents;
                paid_n += 1;
            }
            "overdue" => {
                overdue_cents += inv.total_cents;
                overdue_n += 1;
                outstanding_cents += inv.total_cents;
                outstanding_n += 1;
            }
            _ => {
                outstanding_cents += inv.total_cents;
                outstanding_n += 1;
                if let Some(due) = inv.due_date {
                    if inv.paid_at.is_none() && due < now {
                        overdue_cents += inv.total_cents;
                        overdue_n += 1;
                    }
                }
            }
        }
    }

    let primary_currency = items
        .first()
        .map(|i| i.currency.clone())
        .unwrap_or_else(|| "USD".into());

    let current = tab.read().clone();
    let filtered: Vec<Invoice> = match current.as_str() {
        "paid" => items
            .iter()
            .filter(|i| i.status == "paid")
            .cloned()
            .collect(),
        "outstanding" => items
            .iter()
            .filter(|i| i.status != "paid")
            .cloned()
            .collect(),
        "overdue" => items
            .iter()
            .filter(|i| {
                i.status == "overdue"
                    || (i.paid_at.is_none()
                        && matches!(i.due_date, Some(d) if d < now)
                        && i.status != "paid")
            })
            .cloned()
            .collect(),
        _ => items.clone(),
    };

    rsx! {
        VStack { class: "gap-6",
            SectionHeader {
                label: "Invoices",
                trailing: rsx! {
                    HStack { class: "gap-2 items-center",
                        StatusDot {
                            color: if overdue_n > 0 { StatusDotColor::Danger } else { StatusDotColor::Success },
                            size: StatusDotSize::Small,
                        }
                        Text { variant: TextVariant::Muted, "{status}" }
                    }
                },
            }

            HStack { class: "gap-3 items-start",
                div { class: "rounded-md bg-sky-500/10 p-2 text-sky-500",
                    FileText { size: 24 }
                }
                VStack { class: "gap-1",
                    Heading { level: HeadingLevel::H1, "Invoices" }
                    Text { variant: TextVariant::Muted,
                        "Issue invoices, track who owes you what, and flag overdue accounts."
                    }
                }
            }

            div { class: "grid grid-cols-1 sm:grid-cols-4 gap-3",
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Issued" }
                            Receipt { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{total}" }
                        Text { variant: TextVariant::Muted, "invoice(s)" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Paid" }
                            CheckCheck { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2,
                            "{primary_currency} {format_cents(paid_cents)}"
                        }
                        Text { variant: TextVariant::Muted, "{paid_n} settled" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Outstanding" }
                            Clock { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2,
                            "{primary_currency} {format_cents(outstanding_cents)}"
                        }
                        Text { variant: TextVariant::Muted, "{outstanding_n} open" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Overdue" }
                            CircleAlert { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2,
                            "{primary_currency} {format_cents(overdue_cents)}"
                        }
                        Text { variant: TextVariant::Muted, "{overdue_n} late" }
                    }
                }
            }

            HStack { class: "gap-2 items-center",
                Text { variant: TextVariant::Muted, "Status:" }
                Button {
                    variant: if current == "all" { ButtonVariant::Primary } else { ButtonVariant::Outline },
                    size: ButtonSize::Small,
                    on_click: move |_| tab.set("all".into()),
                    "All"
                }
                Button {
                    variant: if current == "outstanding" { ButtonVariant::Primary } else { ButtonVariant::Outline },
                    size: ButtonSize::Small,
                    on_click: move |_| tab.set("outstanding".into()),
                    "Outstanding"
                }
                Button {
                    variant: if current == "overdue" { ButtonVariant::Primary } else { ButtonVariant::Outline },
                    size: ButtonSize::Small,
                    on_click: move |_| tab.set("overdue".into()),
                    "Overdue"
                }
                Button {
                    variant: if current == "paid" { ButtonVariant::Primary } else { ButtonVariant::Outline },
                    size: ButtonSize::Small,
                    on_click: move |_| tab.set("paid".into()),
                    "Paid"
                }
            }

            InvoiceCreateForm { on_submit: move |p| on_create.call(p) }

            Divider {}

            SectionHeader {
                label: "Records",
                trailing: rsx! { Badge { variant: BadgeVariant::Secondary, "{filtered.len()}" } },
            }
            InvoiceList { items: filtered, on_delete: move |id| on_delete.call(id) }
        }
    }
}
