//! `/invoices` — real, persisted invoicing over the finance `Invoicing`
//! service. Three things:
//!
//! 1. **Uninvoiced** — billable time not yet on any invoice, per project;
//!    generate a draft invoice from it.
//! 2. **Invoices** — the persisted list with Draft / Sent / Paid /
//!    Partially-paid status; mark sent, record payment, delete drafts.
//! 3. **Preview** — a printable card for the selected invoice.
//!
//! State is the shared optimistic invoice store ([`crate::stores`]):
//! mutations patch the list instantly and reconcile against the server
//! (rollback + tray notification on failure) — no refresh counter. The
//! *derived* uninvoiced view can't be reconciled client-side, so the
//! mutations invalidate its reactivity key and it refetches.

use std::collections::HashMap;

use chrono::Utc;
use dioxus::prelude::*;
use fts_ui::prelude::*;
use uuid::Uuid;

use finance_proto::GenerateInvoice;
use finance_proto::invoice::{Invoice, InvoiceStatus};

use crate::orgs::{OrgMeta, OrgSelection};
use crate::stores;

fn money(cents: i64) -> String {
    let neg = cents < 0;
    let v = cents.unsigned_abs();
    format!("{}${}.{:02}", if neg { "-" } else { "" }, v / 100, v % 100)
}

/// `(label, tailwind classes)` for a status chip.
fn status_chip(s: &InvoiceStatus) -> (&'static str, &'static str) {
    match s {
        InvoiceStatus::Draft => ("Draft", "bg-muted text-muted-foreground"),
        InvoiceStatus::Sent => ("Unpaid", "bg-amber-500/15 text-amber-400"),
        InvoiceStatus::Viewed => ("Viewed", "bg-amber-500/15 text-amber-400"),
        InvoiceStatus::PartiallyPaid => ("Partial", "bg-amber-500/15 text-amber-400"),
        InvoiceStatus::Paid => ("Paid", "bg-emerald-500/15 text-emerald-400"),
        InvoiceStatus::Overdue => ("Overdue", "bg-destructive/15 text-destructive"),
        InvoiceStatus::Cancelled => ("Cancelled", "bg-muted text-muted-foreground"),
        InvoiceStatus::Reversed => ("Reversed", "bg-muted text-muted-foreground"),
    }
}

const FIELD: &str = "rounded-md border border-border bg-background px-2.5 py-1.5 text-sm outline-none focus:ring-2 focus:ring-primary/40";

#[component]
pub fn InvoicesView() -> Element {
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();
    let slugs = use_memo(move || crate::orgs::selected_slugs(&selection.read(), &org_list.read()));
    let mut selected = use_signal(|| None::<Uuid>);

    // The shared optimistic store: one AtomResult for the invoice list.
    let invoices = stores::use_invoice_list();

    // Uninvoiced time is *derived* server-side (it reshapes whenever an
    // invoice is generated/deleted), so it can't reconcile from the
    // store — settled invoice mutations invalidate this reactivity key
    // and the resource refetches.
    let reactivity = architect::try_use_reactivity();
    let uninvoiced = use_resource(move || {
        if let Some(r) = reactivity {
            r.track(stores::UNINVOICED_KEY);
        }
        async move { crate::feeds::fetch_uninvoiced_multi(&slugs()).await }
    });
    let projects = use_resource(move || async move {
        crate::feeds::fetch_projects(&slugs())
            .await
            .unwrap_or_default()
            .into_iter()
            .map(|p| (p.id, p.title))
            .collect::<HashMap<Uuid, String>>()
    });

    let store = stores::use_invoice_store();
    let un_rows = uninvoiced.read().clone().unwrap_or_default();
    let inv_rows: Vec<(architect::Id<Uuid>, stores::OrgInvoice)> =
        invoices.value().cloned().unwrap_or_default();
    let load_err = invoices.error().cloned();
    let first_load = invoices.is_waiting() && invoices.value().is_none();
    let proj_names = projects.read().clone().unwrap_or_default();

    let selected_inv = selected().and_then(|id| {
        inv_rows
            .iter()
            .find(|(_, r)| r.invoice.id == id)
            .map(|(_, r)| r.invoice.clone())
    });

    rsx! {
        div { class: "mx-auto flex w-full max-w-3xl flex-col gap-5 p-4 sm:p-6 lg:p-8",
            header { class: "flex flex-col gap-1",
                span { class: "text-[0.7rem] font-semibold uppercase tracking-[0.18em] text-muted-foreground",
                    "Billing"
                }
                Heading { level: HeadingLevel::H1, class: "tracking-tight", "Invoices" }
            }

            // ── Uninvoiced time → generate ─────────────────────────
            if !un_rows.is_empty() {
                div { class: "flex flex-col gap-2",
                    Heading { level: HeadingLevel::H3, "Ready to invoice" }
                    div { class: "flex flex-col gap-2",
                        for (slug , g) in un_rows {
                            UninvoicedRow {
                                key: "{slug}:{g.tag}:{g.project_id:?}",
                                slug: slug.clone(),
                                group: g.clone(),
                                label: g.project_id.and_then(|p| proj_names.get(&p).cloned()).unwrap_or_else(|| if g.tag.is_empty() { "General".into() } else { g.tag.clone() }),
                            }
                        }
                    }
                }
            }

            // ── Persisted invoices ─────────────────────────────────
            div { class: "flex flex-col gap-2",
                Heading { level: HeadingLevel::H3, "Invoices" }
                if first_load {
                    crate::states::LoadingState {}
                } else if inv_rows.is_empty() {
                    if let Some(err) = load_err {
                        crate::states::ErrorState {
                            title: "Couldn't load invoices",
                            message: err,
                            on_retry: move |()| store.reload(),
                        }
                    } else {
                        crate::states::EmptyState {
                            title: "No invoices yet",
                            hint: "Generate one from billable time above.",
                        }
                    }
                } else {
                    div { class: "flex flex-col divide-y divide-border/50 rounded-xl border border-border/60 bg-card/40",
                        for (id , row) in inv_rows {
                            InvoiceRow {
                                key: "{id}",
                                pending: id.is_temp(),
                                slug: row.slug.clone(),
                                invoice: row.invoice.clone(),
                                on_view: move |id| selected.set(Some(id)),
                            }
                        }
                    }
                }
            }

            // ── Printable preview of the selected invoice ──────────
            if let Some(inv) = selected_inv {
                InvoicePreview { invoice: inv }
            }
        }
    }
}

#[component]
fn UninvoicedRow(slug: String, group: finance_proto::UninvoicedGroup, label: String) -> Element {
    let muts = stores::use_invoice_mutations();
    let mut open = use_signal(|| false);
    let mut client = use_signal(|| label.clone());
    let mut net_days = use_signal(|| "30".to_string());

    let hrs = group.seconds as f64 / 3600.0;
    let project_id = group.project_id;
    let tag = group.tag.clone();
    let amount_minor = group.amount_minor;
    let currency = group.currency.clone();
    // Sub-label distinguishes a project bucket from a tag / general one.
    let kind = if group.project_id.is_some() {
        "project"
    } else if group.tag.is_empty() {
        "general"
    } else {
        "tag"
    };

    // Optimistic: a draft invoice (seeded with this group's totals)
    // appears in the list instantly and reconciles to the generated
    // one; the uninvoiced view refetches when the mutation settles.
    let generate = move |_| {
        if client.read().trim().is_empty() {
            return;
        }
        let req = GenerateInvoice {
            project_id,
            tag: tag.clone(),
            client_name: client.read().clone(),
            since: String::new(),
            until: String::new(),
            net_days: net_days.read().parse().unwrap_or(30),
        };
        muts.generate(slug.clone(), req, amount_minor, currency.clone());
        open.set(false);
    };

    rsx! {
        div { class: "flex flex-col gap-2 rounded-lg border border-primary/30 bg-primary/5 px-3 py-2.5",
            div { class: "flex items-center justify-between gap-3",
                div { class: "flex min-w-0 flex-col",
                    div { class: "flex items-center gap-1.5",
                        span { class: "truncate text-sm font-medium text-foreground", "{label}" }
                        if kind != "project" {
                            span { class: "rounded bg-muted px-1.5 py-px text-[10px] text-muted-foreground", "{kind}" }
                        }
                    }
                    span { class: "text-xs text-muted-foreground",
                        "{group.session_count} sessions · {hrs:.1}h · {money(group.amount_minor)}"
                    }
                }
                Button {
                    variant: ButtonVariant::Primary,
                    size: ButtonSize::Small,
                    on_click: move |_| open.toggle(),
                    "Generate"
                }
            }
            if open() {
                div { class: "flex flex-wrap items-end gap-2",
                    label { class: "flex flex-1 flex-col gap-1 text-xs text-muted-foreground",
                        "Bill to"
                        input { class: "{FIELD}", value: "{client}", oninput: move |e| client.set(e.value()) }
                    }
                    label { class: "flex w-20 flex-col gap-1 text-xs text-muted-foreground",
                        "Net days"
                        input { class: "{FIELD}", r#type: "number", value: "{net_days}", oninput: move |e| net_days.set(e.value()) }
                    }
                    Button {
                        variant: ButtonVariant::Primary,
                        size: ButtonSize::Small,
                        on_click: generate,
                        "Create draft"
                    }
                }
            }
        }
    }
}

#[component]
fn InvoiceRow(
    slug: String,
    invoice: Invoice,
    pending: bool,
    on_view: EventHandler<Uuid>,
) -> Element {
    let muts = stores::use_invoice_mutations();
    // Hold the slug in a Copy `Signal` so `act` stays `Copy` across the
    // multiple button closures below.
    let slug = use_signal(|| slug);
    let id = invoice.id;
    let (badge, badge_cls) = status_chip(&invoice.status);
    let number = if invoice.number.is_empty() {
        "Draft".to_string()
    } else {
        invoice.number.clone()
    };
    let total = money(invoice.total_minor);
    let balance = invoice.balance_minor;
    let is_draft = invoice.status == InvoiceStatus::Draft;
    let is_open = matches!(
        invoice.status,
        InvoiceStatus::Sent
            | InvoiceStatus::Viewed
            | InvoiceStatus::PartiallyPaid
            | InvoiceStatus::Overdue
    );
    let date = invoice.issue_date.clone();

    // Optimistic: the row flips status / vanishes instantly, then
    // reconciles against the server (rollback + tray on failure).
    let act = move |kind: &'static str| match kind {
        "sent" => muts.mark_sent(slug(), id),
        "pay" => {
            let date = Utc::now().date_naive().to_string();
            muts.record_payment(slug(), id, balance, date);
        }
        "delete" => muts.delete(slug(), id),
        _ => {}
    };

    let row_cls = if pending { " opacity-60" } else { "" };

    rsx! {
        div { class: "flex items-center justify-between gap-3 px-3 py-2.5{row_cls}",
            button {
                class: "flex min-w-0 flex-1 items-center gap-2 text-left",
                onclick: move |_| on_view.call(id),
                span { class: "shrink-0 rounded px-1.5 py-px text-[10px] font-medium {badge_cls}", "{badge}" }
                div { class: "flex min-w-0 flex-col",
                    span { class: "truncate text-sm text-foreground", "{number}" }
                    span { class: "text-xs text-muted-foreground", "{date}" }
                }
            }
            div { class: "flex shrink-0 items-center gap-3",
                div { class: "flex flex-col items-end",
                    span { class: "font-mono text-sm tabular-nums text-foreground", "{total}" }
                    if balance > 0 {
                        span { class: "font-mono text-[11px] tabular-nums text-amber-400", "{money(balance)} due" }
                    }
                }
                div { class: "flex items-center gap-1",
                    if is_draft {
                        Button { variant: ButtonVariant::Secondary, size: ButtonSize::Small, on_click: move |_| act("sent"), "Send" }
                        Button { variant: ButtonVariant::Ghost, size: ButtonSize::Small, on_click: move |_| act("delete"), "Delete" }
                    }
                    if is_open {
                        Button { variant: ButtonVariant::Primary, size: ButtonSize::Small, on_click: move |_| act("pay"), "Mark paid" }
                    }
                }
            }
        }
    }
}

#[component]
fn InvoicePreview(invoice: Invoice) -> Element {
    let number = if invoice.number.is_empty() {
        "DRAFT".to_string()
    } else {
        invoice.number.clone()
    };
    rsx! {
        div { class: "flex flex-col gap-3",
            div { id: "invoice-print",
                class: "flex flex-col gap-4 rounded-xl border border-border bg-white p-6 text-slate-900 shadow-sm",
                div { class: "flex items-start justify-between",
                    div {
                        div { class: "text-lg font-bold", "Invoice" }
                        div { class: "text-sm text-slate-500", "{number}" }
                    }
                    div { class: "text-right text-sm",
                        div { class: "text-slate-500", "Issued" }
                        div { "{invoice.issue_date}" }
                        if !invoice.due_date.is_empty() {
                            div { class: "text-slate-500", "Due {invoice.due_date}" }
                        }
                    }
                }
                table { class: "w-full text-sm",
                    thead {
                        tr { class: "border-b border-slate-200 text-left text-slate-500",
                            th { class: "py-1.5 font-medium", "Description" }
                            th { class: "py-1.5 text-right font-medium", "Hours" }
                            th { class: "py-1.5 text-right font-medium", "Amount" }
                        }
                    }
                    tbody {
                        for li in invoice.line_items.0.iter() {
                            tr { key: "{li.id}", class: "border-b border-slate-100",
                                td { class: "py-1.5", "{li.description}" }
                                td { class: "py-1.5 text-right tabular-nums", {format!("{:.2}", li.quantity_milli as f64 / 1000.0)} }
                                td { class: "py-1.5 text-right tabular-nums", "{money(li.line_total_minor)}" }
                            }
                        }
                    }
                    tfoot {
                        tr { class: "font-semibold",
                            td { class: "py-2", "Total" }
                            td {}
                            td { class: "py-2 text-right tabular-nums", "{money(invoice.total_minor)}" }
                        }
                        if invoice.amount_paid_minor > 0 {
                            tr { class: "text-emerald-700",
                                td { class: "py-0.5", "Paid" }
                                td {}
                                td { class: "py-0.5 text-right tabular-nums", "{money(invoice.amount_paid_minor)}" }
                            }
                            tr { class: "font-semibold",
                                td { class: "py-0.5", "Balance" }
                                td {}
                                td { class: "py-0.5 text-right tabular-nums", "{money(invoice.balance_minor)}" }
                            }
                        }
                    }
                }
            }
            div { class: "flex justify-end",
                Button {
                    variant: ButtonVariant::Primary,
                    on_click: move |_| {
                        let _ = dioxus::document::eval("window.print()");
                    },
                    "Print / Save PDF"
                }
            }
        }
    }
}
