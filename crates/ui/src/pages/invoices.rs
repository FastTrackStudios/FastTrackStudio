//! `/invoices` — build a client invoice from billable timer sessions.
//!
//! The finance Invoicing service isn't mounted over vox, so this
//! computes the invoice client-side from the mounted timer
//! `list_sessions`: pick a project + date range, group its billable
//! closed sessions by description, sum hours × rate, and render a
//! printable preview (browser print → PDF). Mirrors the CLI's
//! `build_invoice_from_sessions`.

use std::collections::BTreeMap;

use chrono::Utc;
use dioxus::prelude::*;
use fts_ui::prelude::*;
use uuid::Uuid;

use crate::orgs::{OrgMeta, OrgSelection};

fn money(cents: i64) -> String {
    format!("${:.2}", cents as f64 / 100.0)
}

const FIELD: &str = "rounded-md border border-border bg-background px-3 py-2 text-sm outline-none focus:ring-2 focus:ring-primary/40";

#[component]
pub fn InvoicesView() -> Element {
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();
    let slugs = use_memo(move || crate::orgs::selected_slugs(&selection.read(), &org_list.read()));

    let sessions =
        use_resource(move || async move { crate::feeds::fetch_sessions_multi(&slugs()).await });
    let projects = use_resource(move || async move {
        crate::feeds::fetch_projects(&slugs())
            .await
            .unwrap_or_default()
            .into_iter()
            .map(|p| (p.id, p.title))
            .collect::<std::collections::HashMap<Uuid, String>>()
    });

    let mut project = use_signal(|| None::<Uuid>);
    let mut from = use_signal(String::new);
    let mut to = use_signal(String::new);
    let mut client = use_signal(String::new);
    let mut number = use_signal(String::new);

    let rows = sessions.read().clone().unwrap_or_default();
    let proj_names = projects.read().clone().unwrap_or_default();

    // Projects that actually have billable closed sessions (the
    // invoiceable set) → dropdown options.
    let mut invoiceable: BTreeMap<Uuid, String> = BTreeMap::new();
    for (_, s) in &rows {
        if s.billable && s.end_time.is_some() {
            if let Some(pid) = s.project_id {
                invoiceable.entry(pid).or_insert_with(|| {
                    proj_names
                        .get(&pid)
                        .cloned()
                        .unwrap_or_else(|| "Unknown".into())
                });
            }
        }
    }

    // Build the line items for the selected project in [from, to].
    let from_d = from.read().clone();
    let to_d = to.read().clone();
    let mut lines: BTreeMap<String, (i64, i64)> = BTreeMap::new();
    let mut total_secs = 0i64;
    let mut total_cents = 0i64;
    if let Some(pid) = project() {
        for (_, s) in &rows {
            if s.project_id != Some(pid) || !s.billable {
                continue;
            }
            let Some(end) = s.end_time else { continue };
            let day = s.start_time.date_naive().to_string();
            if !from_d.is_empty() && day < from_d {
                continue;
            }
            if !to_d.is_empty() && day > to_d {
                continue;
            }
            let secs = (end - s.start_time).num_seconds().max(0);
            let cents = secs * s.rate_cents / 3600;
            let desc = if s.description.trim().is_empty() {
                "(no description)".to_string()
            } else {
                s.description.clone()
            };
            let e = lines.entry(desc).or_default();
            e.0 += secs;
            e.1 += cents;
            total_secs += secs;
            total_cents += cents;
        }
    }

    let project_title = project().and_then(|p| proj_names.get(&p).cloned());
    let bill_to = if client.read().trim().is_empty() {
        project_title.clone().unwrap_or_default()
    } else {
        client.read().clone()
    };
    let today = Utc::now().date_naive().to_string();
    let inv_no = if number.read().trim().is_empty() {
        "DRAFT".to_string()
    } else {
        number.read().clone()
    };

    rsx! {
        div { class: "mx-auto flex w-full max-w-3xl flex-col gap-5 p-4 sm:p-6 lg:p-8",
            header { class: "flex flex-col gap-1",
                span { class: "text-[0.7rem] font-semibold uppercase tracking-[0.18em] text-muted-foreground",
                    "Billing"
                }
                Heading { level: HeadingLevel::H1, class: "tracking-tight", "Invoices" }
            }

            // ── Controls ───────────────────────────────────────────
            div { class: "flex flex-col gap-3 rounded-xl border border-border/60 bg-card/40 p-4",
                div { class: "flex flex-col gap-1",
                    span { class: "text-xs text-muted-foreground", "Project" }
                    select {
                        class: "{FIELD}",
                        onchange: move |e| {
                            project.set(Uuid::parse_str(&e.value()).ok());
                        },
                        option { value: "", "Select a project…" }
                        for (pid , name) in invoiceable.iter() {
                            option { value: "{pid}", "{name}" }
                        }
                    }
                }
                div { class: "grid grid-cols-2 gap-3",
                    label { class: "flex flex-col gap-1 text-xs text-muted-foreground",
                        "From"
                        input { class: "{FIELD}", r#type: "date", value: "{from}", oninput: move |e| from.set(e.value()) }
                    }
                    label { class: "flex flex-col gap-1 text-xs text-muted-foreground",
                        "To"
                        input { class: "{FIELD}", r#type: "date", value: "{to}", oninput: move |e| to.set(e.value()) }
                    }
                }
                div { class: "grid grid-cols-2 gap-3",
                    label { class: "flex flex-col gap-1 text-xs text-muted-foreground",
                        "Bill to"
                        input { class: "{FIELD}", placeholder: "{project_title.clone().unwrap_or_default()}", value: "{client}", oninput: move |e| client.set(e.value()) }
                    }
                    label { class: "flex flex-col gap-1 text-xs text-muted-foreground",
                        "Invoice #"
                        input { class: "{FIELD}", placeholder: "INV-2026-001", value: "{number}", oninput: move |e| number.set(e.value()) }
                    }
                }
            }

            // ── Preview (light card so it prints cleanly) ──────────
            if project().is_some() && !lines.is_empty() {
                div { id: "invoice-print",
                    class: "flex flex-col gap-4 rounded-xl border border-border bg-white p-6 text-slate-900 shadow-sm",
                    div { class: "flex items-start justify-between",
                        div {
                            div { class: "text-lg font-bold", "Invoice" }
                            div { class: "text-sm text-slate-500", "{inv_no}" }
                        }
                        div { class: "text-right text-sm",
                            div { class: "text-slate-500", "Date" }
                            div { "{today}" }
                        }
                    }
                    div { class: "text-sm",
                        div { class: "text-slate-500", "Bill to" }
                        div { class: "font-medium", "{bill_to}" }
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
                            for (desc , (secs , cents)) in lines.iter() {
                                tr { key: "{desc}", class: "border-b border-slate-100",
                                    td { class: "py-1.5", "{desc}" }
                                    td { class: "py-1.5 text-right tabular-nums", {format!("{:.2}", *secs as f64 / 3600.0)} }
                                    td { class: "py-1.5 text-right tabular-nums", "{money(*cents)}" }
                                }
                            }
                        }
                        tfoot {
                            tr { class: "font-semibold",
                                td { class: "py-2", "Total" }
                                td { class: "py-2 text-right tabular-nums", {format!("{:.2}", total_secs as f64 / 3600.0)} }
                                td { class: "py-2 text-right tabular-nums", "{money(total_cents)}" }
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
            } else if project().is_some() {
                div { class: "rounded-lg border border-dashed border-border px-4 py-10 text-center",
                    Text { variant: TextVariant::Muted, "No billable sessions for this project in the selected range." }
                }
            } else {
                Text { variant: TextVariant::Muted, class: "text-sm", "Pick a project to build an invoice from its billable time." }
            }
        }
    }
}
