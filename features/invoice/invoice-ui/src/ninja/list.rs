//! `InvoiceListView` — filterable list of invoices with toolbar.

use std::collections::HashMap;

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Search, X};
use fts_ui::prelude::*;
use invoice_proto::{Client, Invoice, Payment};
use uuid::Uuid;

use super::common::{format_money, invoice_status_variant};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct InvoiceListFilter {
    /// Empty vec = "all statuses".
    pub statuses: Vec<String>,
    pub client_id: Option<Uuid>,
    pub search: String,
}

impl InvoiceListFilter {
    pub fn is_empty(&self) -> bool {
        self.statuses.is_empty() && self.client_id.is_none() && self.search.trim().is_empty()
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct InvoiceListViewProps {
    pub invoices: Vec<Invoice>,
    pub clients_by_id: HashMap<Uuid, Client>,
    pub payments_by_invoice: HashMap<Uuid, Vec<Payment>>,
    pub filter: InvoiceListFilter,
    pub on_filter_change: EventHandler<InvoiceListFilter>,
    pub on_open: EventHandler<Uuid>,
    pub on_duplicate: EventHandler<Uuid>,
    pub on_delete: EventHandler<Uuid>,
    pub on_mark_sent: EventHandler<Uuid>,
    pub on_record_payment: EventHandler<Uuid>,
}

#[component]
pub fn InvoiceListView(props: InvoiceListViewProps) -> Element {
    let f = props.filter.clone();
    let filtered = apply_invoice_filter(&props.invoices, &props.clients_by_id, &f);

    let on_filter_change = props.on_filter_change;
    let f_for_status = f.clone();
    let f_for_client = f.clone();
    let f_for_search = f.clone();
    let f_for_clear = f.clone();

    // SegmentedControl value mapping: All / Draft / Sent / Paid / Overdue.
    let status_value = match f.statuses.as_slice() {
        [] => "all",
        [s] => match s.as_str() {
            "draft" => "draft",
            "sent" => "sent",
            "paid" => "paid",
            "overdue" => "overdue",
            _ => "all",
        },
        _ => "all",
    }
    .to_string();

    rsx! {
        div { class: "flex flex-col gap-3",
            // Toolbar
            div { class: "flex flex-wrap items-center gap-2",
                SegmentedControl {
                    value: status_value,
                    on_change: move |v: String| {
                        let mut next = f_for_status.clone();
                        next.statuses = match v.as_str() {
                            "draft" => vec!["draft".into()],
                            "sent" => vec!["sent".into(), "viewed".into(), "partial".into()],
                            "paid" => vec!["paid".into()],
                            "overdue" => vec!["overdue".into()],
                            _ => vec![],
                        };
                        on_filter_change.call(next);
                    },
                    options: vec![
                        ("all".into(), "All".into()),
                        ("draft".into(), "Draft".into()),
                        ("sent".into(), "Open".into()),
                        ("paid".into(), "Paid".into()),
                        ("overdue".into(), "Overdue".into()),
                    ],
                    size: SegmentedControlSize::Small,
                }

                ClientFilter {
                    clients: props.clients_by_id.clone(),
                    value: f.client_id,
                    on_change: move |id: Option<Uuid>| {
                        let mut next = f_for_client.clone();
                        next.client_id = id;
                        on_filter_change.call(next);
                    },
                }

                div { class: "flex items-center gap-2 rounded-md border bg-background px-2 py-1 min-w-60",
                    Search { size: 14 }
                    input {
                        class: "flex-1 bg-transparent text-sm outline-none placeholder:text-muted-foreground",
                        placeholder: "Search number, notes…",
                        value: "{f.search}",
                        oninput: move |e| {
                            let mut next = f_for_search.clone();
                            next.search = e.value();
                            on_filter_change.call(next);
                        },
                    }
                }

                if !f.is_empty() {
                    Button {
                        variant: ButtonVariant::Ghost,
                        size: ButtonSize::Small,
                        on_click: move |_| {
                            let _ = &f_for_clear;
                            on_filter_change.call(InvoiceListFilter::default());
                        },
                        X { size: 12 }
                        " Clear"
                    }
                }
            }

            // Table header
            div { class: "grid items-center gap-2 px-3 py-2 text-xs text-muted-foreground border-b",
                style: "grid-template-columns: 120px 1fr 90px 100px 100px 110px 110px 36px;",
                span { "Number" }
                span { "Client" }
                span { "Status" }
                span { "Issued" }
                span { "Due" }
                span { class: "text-right", "Total" }
                span { class: "text-right", "Balance" }
                span {}
            }

            if filtered.is_empty() {
                EmptyState {
                    message: "No invoices match the current filter.",
                }
            } else {
                for inv in filtered.iter().cloned() {
                    InvoiceListRow {
                        key: "{inv.id}",
                        invoice: inv.clone(),
                        client: props.clients_by_id.get(&inv.client_id).cloned(),
                        payments: props.payments_by_invoice.get(&inv.id).cloned().unwrap_or_default(),
                        on_open: props.on_open,
                        on_duplicate: props.on_duplicate,
                        on_delete: props.on_delete,
                        on_mark_sent: props.on_mark_sent,
                        on_record_payment: props.on_record_payment,
                    }
                }
            }
        }
    }
}

#[component]
fn InvoiceListRow(
    invoice: Invoice,
    client: Option<Client>,
    payments: Vec<Payment>,
    on_open: EventHandler<Uuid>,
    on_duplicate: EventHandler<Uuid>,
    on_delete: EventHandler<Uuid>,
    on_mark_sent: EventHandler<Uuid>,
    on_record_payment: EventHandler<Uuid>,
) -> Element {
    let id = invoice.id;
    let number = invoice.number.clone();
    let status = invoice.status.clone();
    let variant = invoice_status_variant(&status);
    let issued = invoice.issue_date.format("%Y-%m-%d").to_string();
    let due = invoice
        .due_date
        .map(|d| d.format("%Y-%m-%d").to_string())
        .unwrap_or_else(|| "—".into());
    let total = format_money(invoice.total_cents, &invoice.currency);
    let balance = format_money(invoice.balance_cents, &invoice.currency);

    let client_name = client
        .as_ref()
        .map(|c| c.name.clone())
        .unwrap_or_else(|| "(no client)".into());
    let avatar_initials: String = client_name
        .split_whitespace()
        .take(2)
        .filter_map(|w| w.chars().next())
        .collect::<String>()
        .to_uppercase();

    let _ = payments; // reserved for richer row preview later

    rsx! {
        ContextMenu {
            ContextMenuTrigger {
                button {
                    class: "grid items-center gap-2 px-3 text-sm hover:bg-accent/30 border-b w-full text-left",
                    style: "grid-template-columns: 120px 1fr 90px 100px 100px 110px 110px 36px; height: 40px;",
                    onclick: move |_| on_open.call(id),
                    span { class: "font-mono text-xs tabular-nums", "{number}" }
                    span { class: "flex items-center gap-2 min-w-0",
                        span {
                            class: "inline-flex size-6 items-center justify-center rounded-full bg-muted text-[10px] font-medium",
                            "{avatar_initials}"
                        }
                        span { class: "truncate", "{client_name}" }
                    }
                    span {
                        StatusBadge { variant, label: status.clone() }
                    }
                    span { class: "text-xs text-muted-foreground tabular-nums", "{issued}" }
                    span { class: "text-xs text-muted-foreground tabular-nums", "{due}" }
                    span { class: "text-right font-mono text-xs tabular-nums", "{total}" }
                    span { class: "text-right font-mono text-xs tabular-nums", "{balance}" }
                    span {}
                }
            }
            ContextMenuContent {
                ContextMenuItem {
                    value: "open".to_string(),
                    index: 0,
                    on_select: move |_| on_open.call(id),
                    "Open"
                }
                ContextMenuItem {
                    value: "duplicate".to_string(),
                    index: 1,
                    on_select: move |_| on_duplicate.call(id),
                    "Duplicate"
                }
                ContextMenuItem {
                    value: "mark-sent".to_string(),
                    index: 2,
                    on_select: move |_| on_mark_sent.call(id),
                    "Mark sent"
                }
                ContextMenuItem {
                    value: "record-payment".to_string(),
                    index: 3,
                    on_select: move |_| on_record_payment.call(id),
                    "Record payment"
                }
                ContextMenuSeparator {}
                ContextMenuItem {
                    value: "delete".to_string(),
                    index: 4,
                    destructive: true,
                    on_select: move |_| on_delete.call(id),
                    "Delete"
                }
            }
        }
    }
}

#[component]
fn ClientFilter(
    clients: HashMap<Uuid, Client>,
    value: Option<Uuid>,
    on_change: EventHandler<Option<Uuid>>,
) -> Element {
    let label = value
        .and_then(|id| clients.get(&id))
        .map(|c| c.name.clone())
        .unwrap_or_else(|| "All clients".to_string());

    let mut sorted: Vec<Client> = clients.values().cloned().collect();
    sorted.sort_by(|a, b| a.name.cmp(&b.name));

    rsx! {
        Popover {
            PopoverTrigger {
                Button {
                    variant: ButtonVariant::Outline,
                    size: ButtonSize::Small,
                    "{label}"
                }
            }
            PopoverContent { class: "p-1 min-w-56 max-h-80 overflow-auto",
                button {
                    class: "w-full rounded px-2 py-1 text-left text-sm hover:bg-accent",
                    onclick: move |_| on_change.call(None),
                    "All clients"
                }
                for c in sorted.iter() {
                    {
                        let cid = c.id;
                        let cname = c.name.clone();
                        let on = value == Some(cid);
                        rsx! {
                            button {
                                key: "{cid}",
                                class: if on { "w-full rounded px-2 py-1 text-left text-sm bg-accent" } else { "w-full rounded px-2 py-1 text-left text-sm hover:bg-accent" },
                                onclick: move |_| on_change.call(Some(cid)),
                                "{cname}"
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Pure filter — exposed so callers / dashboard stats don't reimplement.
pub fn apply_invoice_filter(
    invoices: &[Invoice],
    _clients: &HashMap<Uuid, Client>,
    f: &InvoiceListFilter,
) -> Vec<Invoice> {
    let needle = f.search.trim().to_lowercase();
    invoices
        .iter()
        .filter(|inv| {
            if !f.statuses.is_empty() && !f.statuses.iter().any(|s| s == &inv.status) {
                return false;
            }
            if let Some(cid) = f.client_id {
                if inv.client_id != cid {
                    return false;
                }
            }
            if !needle.is_empty() {
                let in_number = inv.number.to_lowercase().contains(&needle);
                let in_notes = inv
                    .notes
                    .as_deref()
                    .map(|n| n.to_lowercase().contains(&needle))
                    .unwrap_or(false);
                if !in_number && !in_notes {
                    return false;
                }
            }
            true
        })
        .cloned()
        .collect()
}
