//! `InvoiceNinjaDashboard` — stats row + filterable invoice list.
//! Caller wraps the editor in a Sheet on `on_open`.

use std::collections::HashMap;

use chrono::{Datelike, Utc};
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Clock, FileText, Plus, Receipt, TriangleAlert};
use fts_ui::prelude::*;
use invoice_proto::{Client, Invoice, Payment};
use uuid::Uuid;

use super::common::format_money;
use super::list::{InvoiceListFilter, InvoiceListView};

#[derive(Props, Clone, PartialEq)]
pub struct InvoiceNinjaDashboardProps {
    pub invoices: Vec<Invoice>,
    pub lines_by_invoice: HashMap<Uuid, Vec<invoice_proto::InvoiceLine>>,
    pub clients: Vec<Client>,
    pub payments_by_invoice: HashMap<Uuid, Vec<Payment>>,
    pub on_open: EventHandler<Uuid>,
    pub on_new: EventHandler<()>,
    pub on_open_bridge: EventHandler<()>,
    pub on_duplicate: EventHandler<Uuid>,
    pub on_delete: EventHandler<Uuid>,
    pub on_mark_sent: EventHandler<Uuid>,
    pub on_record_payment: EventHandler<Uuid>,
}

#[component]
pub fn InvoiceNinjaDashboard(props: InvoiceNinjaDashboardProps) -> Element {
    let _ = &props.lines_by_invoice; // reserved for hover / preview surfaces

    let filter = use_signal(InvoiceListFilter::default);

    let primary_currency = props
        .invoices
        .first()
        .map(|i| i.currency.clone())
        .unwrap_or_else(|| "USD".into());

    let now = Utc::now();
    let mut total_invoiced: i64 = 0;
    let mut outstanding: i64 = 0;
    let mut paid_this_month: i64 = 0;
    let mut overdue_count: usize = 0;
    let cur_month = now.month();
    let cur_year = now.year();
    for inv in &props.invoices {
        total_invoiced += inv.total_cents;
        if inv.status != "paid" && inv.status != "cancelled" {
            outstanding += inv.balance_cents.max(0);
        }
        if inv.status == "overdue" {
            overdue_count += 1;
        } else if let Some(due) = inv.due_date {
            if inv.status != "paid" && due < now {
                overdue_count += 1;
            }
        }
        if let Some(paid_at) = inv.paid_at {
            if paid_at.month() == cur_month && paid_at.year() == cur_year {
                paid_this_month += inv.total_cents;
            }
        }
    }

    let clients_by_id: HashMap<Uuid, Client> =
        props.clients.iter().map(|c| (c.id, c.clone())).collect();

    let filter_for_view = filter.read().clone();

    rsx! {
        VStack { class: "gap-6",
            // Header
            HStack { class: "items-start justify-between gap-3",
                HStack { class: "gap-3 items-start",
                    div { class: "rounded-md bg-sky-500/10 p-2 text-sky-500",
                        Receipt { size: 24 }
                    }
                    VStack { class: "gap-1",
                        Heading { level: HeadingLevel::H1, "Invoices" }
                        Text { variant: TextVariant::Muted,
                            "Issue invoices, track who owes you what, and pull billable time into drafts."
                        }
                    }
                }
                HStack { class: "gap-2",
                    Button {
                        variant: ButtonVariant::Outline,
                        on_click: move |_| props.on_open_bridge.call(()),
                        Clock { size: 14 }
                        " Bill time"
                    }
                    Button {
                        variant: ButtonVariant::Primary,
                        on_click: move |_| props.on_new.call(()),
                        Plus { size: 14 }
                        " New invoice"
                    }
                }
            }

            // Stats
            div { class: "grid grid-cols-1 sm:grid-cols-4 gap-3",
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Total invoiced" }
                            FileText { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2,
                            "{format_money(total_invoiced, &primary_currency)}"
                        }
                        Text { variant: TextVariant::Muted, "{props.invoices.len()} invoice(s)" }
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
                            "{format_money(outstanding, &primary_currency)}"
                        }
                        Text { variant: TextVariant::Muted, "open balances" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Paid this month" }
                            Receipt { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2,
                            "{format_money(paid_this_month, &primary_currency)}"
                        }
                        Text { variant: TextVariant::Muted, "month-to-date" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Overdue" }
                            TriangleAlert { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{overdue_count}" }
                        Text { variant: TextVariant::Muted, "invoice(s) past due" }
                    }
                }
            }

            Divider {}

            InvoiceListView {
                invoices: props.invoices.clone(),
                clients_by_id: clients_by_id.clone(),
                payments_by_invoice: props.payments_by_invoice.clone(),
                filter: filter_for_view,
                on_filter_change: move |f: InvoiceListFilter| filter.clone().set(f),
                on_open: props.on_open,
                on_duplicate: props.on_duplicate,
                on_delete: props.on_delete,
                on_mark_sent: props.on_mark_sent,
                on_record_payment: props.on_record_payment,
            }
        }
    }
}
