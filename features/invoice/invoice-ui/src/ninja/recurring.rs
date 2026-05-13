//! Recurring invoice surfaces.
//!
//! - [`RecurringInvoiceList`] — table of templates with pause / resume,
//!   generate-now, edit, delete actions.
//! - [`RecurringInvoiceEditor`] — minimal editor (client, frequency,
//!   next/end dates, tax/discount, line items, notes).
//!
//! v1 duplicates a chunk of `InvoiceEditor`'s line-items behaviour
//! rather than extracting a shared sub-component. The line-items
//! grid is small enough (~80 LoC) that the duplication is bearable;
//! a follow-up should factor it out once a third caller emerges.

use chrono::{Local, NaiveDate, TimeZone, Utc};
use dioxus::prelude::*;
use fts_ui::components::ComboboxItemData;
use fts_ui::lucide_dioxus::{Pause, Play, Plus, RefreshCw, Save, Trash2};
use fts_ui::prelude::*;
use invoice_proto::{
    Client, RECURRING_INVOICE_FREQUENCIES, RecurringInvoice, RecurringInvoiceLine,
};
use uuid::Uuid;

use super::client_picker::ClientPicker;
use super::common::{format_money, parse_money_input};

// ── RecurringInvoiceList ──────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct RecurringInvoiceListProps {
    pub items: Vec<RecurringInvoice>,
    pub clients: Vec<Client>,
    pub on_open: EventHandler<Uuid>,
    pub on_toggle_status: EventHandler<Uuid>,
    pub on_generate_now: EventHandler<Uuid>,
    pub on_delete: EventHandler<Uuid>,
    pub on_new: EventHandler<()>,
}

#[component]
pub fn RecurringInvoiceList(props: RecurringInvoiceListProps) -> Element {
    let by_client: std::collections::HashMap<Uuid, String> = props
        .clients
        .iter()
        .map(|c| (c.id, c.name.clone()))
        .collect();

    rsx! {
        VStack { class: "gap-4",
            HStack { class: "items-center justify-between",
                Heading { level: HeadingLevel::H2, "Recurring templates" }
                Button {
                    variant: ButtonVariant::Primary,
                    on_click: move |_| props.on_new.call(()),
                    Plus { size: 14 }
                    " New template"
                }
            }

            if props.items.is_empty() {
                EmptyState {
                    message: "No recurring invoices yet. Create one to auto-generate invoices on a cadence.",
                    icon: rsx! { RefreshCw { size: 32 } },
                }
            } else {
                Card {
                    CardContent { class: "p-0 overflow-hidden",
                        div { class: "grid items-center gap-2 px-4 py-2 text-xs uppercase tracking-wide text-muted-foreground border-b",
                            style: "grid-template-columns: 1.4fr 100px 130px 100px 110px 180px;",
                            span { "Client" }
                            span { "Frequency" }
                            span { "Next issue" }
                            span { "Status" }
                            span { class: "text-right", "Total" }
                            span { class: "text-right", "Actions" }
                        }
                        for r in props.items.iter().cloned() {
                            RecurringRow {
                                key: "{r.id}",
                                row: r.clone(),
                                client_name: by_client.get(&r.client_id).cloned().unwrap_or_else(|| "(missing)".into()),
                                on_open: move |id| props.on_open.call(id),
                                on_toggle_status: move |id| props.on_toggle_status.call(id),
                                on_generate_now: move |id| props.on_generate_now.call(id),
                                on_delete: move |id| props.on_delete.call(id),
                            }
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn RecurringRow(
    row: RecurringInvoice,
    client_name: String,
    on_open: EventHandler<Uuid>,
    on_toggle_status: EventHandler<Uuid>,
    on_generate_now: EventHandler<Uuid>,
    on_delete: EventHandler<Uuid>,
) -> Element {
    let id = row.id;
    let next = row.next_issue_date.format("%b %-d, %Y").to_string();
    let status_variant = match row.status.as_str() {
        "active" => StatusBadgeVariant::Success,
        "paused" => StatusBadgeVariant::Warning,
        _ => StatusBadgeVariant::Neutral,
    };
    let total_label = format_money(
        row.subtotal_cents - row.discount_cents
            + (row.subtotal_cents * row.tax_rate_bps as i64 / 10_000),
        &row.currency,
    );
    let is_paused = row.status == "paused";

    rsx! {
        div { class: "grid items-center gap-2 px-4 py-2 text-sm border-b last:border-b-0 hover:bg-accent/30",
            style: "grid-template-columns: 1.4fr 100px 130px 100px 110px 180px;",
            span { class: "truncate", "{client_name}" }
            span { class: "capitalize", "{row.frequency}" }
            span { class: "tabular-nums", "{next}" }
            span {
                StatusBadge { variant: status_variant, label: row.status.clone() }
            }
            span { class: "text-right tabular-nums font-mono text-xs", "{total_label}" }
            div { class: "flex justify-end gap-1",
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: move |_| on_toggle_status.call(id),
                    if is_paused {
                        Play { size: 12 }
                    } else {
                        Pause { size: 12 }
                    }
                }
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: move |_| on_generate_now.call(id),
                    RefreshCw { size: 12 }
                }
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: move |_| on_open.call(id),
                    "Edit"
                }
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: move |_| on_delete.call(id),
                    Trash2 { size: 12 }
                }
            }
        }
    }
}

// ── RecurringInvoiceDraft + Editor ───────────────────────────────────

#[derive(Clone, Debug, PartialEq)]
pub struct RecurringInvoiceDraft {
    pub recurring: RecurringInvoice,
    pub lines: Vec<RecurringInvoiceLine>,
}

#[derive(Props, Clone, PartialEq)]
pub struct RecurringInvoiceEditorProps {
    pub draft: RecurringInvoiceDraft,
    pub clients: Vec<Client>,
    pub on_change: EventHandler<RecurringInvoiceDraft>,
    pub on_save: EventHandler<()>,
}

#[component]
pub fn RecurringInvoiceEditor(props: RecurringInvoiceEditorProps) -> Element {
    let draft = props.draft.clone();
    let on_change = props.on_change;

    let freq_items: Vec<ComboboxItemData> = RECURRING_INVOICE_FREQUENCIES
        .iter()
        .map(|f| ComboboxItemData::new(f.to_string(), f.to_string()))
        .collect();
    let freq_signal = use_signal(|| draft.recurring.frequency.clone());

    {
        let d = draft.clone();
        use_effect(move || {
            let v = freq_signal.read().clone();
            if v != d.recurring.frequency && !v.is_empty() {
                let mut next = d.clone();
                next.recurring.frequency = v;
                on_change.call(next);
            }
        });
    }

    let currency_items: Vec<ComboboxItemData> = ["USD", "EUR", "GBP", "CAD", "AUD", "JPY"]
        .iter()
        .map(|c| ComboboxItemData::new(c.to_string(), c.to_string()))
        .collect();
    let currency_signal = use_signal(|| draft.recurring.currency.clone());
    {
        let d = draft.clone();
        use_effect(move || {
            let cur = currency_signal.read().clone();
            if cur != d.recurring.currency && !cur.is_empty() {
                let mut next = d.clone();
                next.recurring.currency = cur;
                on_change.call(next);
            }
        });
    }

    let next_str = draft
        .recurring
        .next_issue_date
        .date_naive()
        .format("%Y-%m-%d")
        .to_string();
    let end_str = draft
        .recurring
        .end_date
        .map(|d| d.date_naive().format("%Y-%m-%d").to_string())
        .unwrap_or_default();

    let subtotal: i64 = draft.lines.iter().map(|l| l.amount_cents).sum();

    rsx! {
        div { class: "grid grid-cols-1 lg:grid-cols-[1fr_280px] gap-4",
            Card {
                CardContent { class: "flex flex-col gap-4",

                    div { class: "grid grid-cols-1 sm:grid-cols-[1fr_140px] gap-3",
                        div { class: "flex flex-col gap-1",
                            Label { "Client" }
                            ClientPicker {
                                clients: props.clients.clone(),
                                value: Some(draft.recurring.client_id).filter(|id| *id != Uuid::nil()),
                                on_change: {
                                    let d = draft.clone();
                                    move |id: Option<Uuid>| {
                                        let mut next = d.clone();
                                        next.recurring.client_id = id.unwrap_or(Uuid::nil());
                                        on_change.call(next);
                                    }
                                },
                                on_create_request: move |_| {},
                            }
                        }
                        div { class: "flex flex-col gap-1",
                            Label { "Currency" }
                            Combobox {
                                value: currency_signal,
                                ComboboxTrigger { placeholder: "USD" }
                                ComboboxContent { items: currency_items }
                            }
                        }
                    }

                    div { class: "grid grid-cols-3 gap-3",
                        div { class: "flex flex-col gap-1",
                            Label { "Frequency" }
                            Combobox {
                                value: freq_signal,
                                ComboboxTrigger { placeholder: "monthly" }
                                ComboboxContent { items: freq_items }
                            }
                        }
                        div { class: "flex flex-col gap-1",
                            Label { "Next issue date" }
                            input {
                                r#type: "date",
                                class: "h-9 rounded border bg-background px-2 text-sm",
                                value: "{next_str}",
                                oninput: {
                                    let d = draft.clone();
                                    move |e| {
                                        if let Ok(date) = NaiveDate::parse_from_str(&e.value(), "%Y-%m-%d") {
                                            let naive = date.and_hms_opt(12, 0, 0).unwrap_or_default();
                                            if let Some(local) = Local.from_local_datetime(&naive).single() {
                                                let mut next = d.clone();
                                                next.recurring.next_issue_date = local.with_timezone(&Utc);
                                                on_change.call(next);
                                            }
                                        }
                                    }
                                },
                            }
                        }
                        div { class: "flex flex-col gap-1",
                            Label { "End date (optional)" }
                            input {
                                r#type: "date",
                                class: "h-9 rounded border bg-background px-2 text-sm",
                                value: "{end_str}",
                                oninput: {
                                    let d = draft.clone();
                                    move |e| {
                                        let v = e.value();
                                        let mut next = d.clone();
                                        if v.trim().is_empty() {
                                            next.recurring.end_date = None;
                                            on_change.call(next);
                                            return;
                                        }
                                        if let Ok(date) = NaiveDate::parse_from_str(&v, "%Y-%m-%d") {
                                            let naive = date.and_hms_opt(12, 0, 0).unwrap_or_default();
                                            if let Some(local) = Local.from_local_datetime(&naive).single() {
                                                next.recurring.end_date = Some(local.with_timezone(&Utc));
                                                on_change.call(next);
                                            }
                                        }
                                    }
                                },
                            }
                        }
                    }

                    Divider {}

                    // Line items — small grid, no per-cell live edit
                    // beyond description / qty / price. v1 duplicates
                    // the InvoiceEditor pattern verbatim.
                    LinesGrid {
                        draft: draft.clone(),
                        on_change,
                    }

                    Divider {}

                    NotesField {
                        draft: draft.clone(),
                        on_change,
                    }
                }
            }

            div { class: "lg:sticky lg:top-4 self-start",
                Card {
                    CardContent { class: "flex flex-col gap-3",
                        Text { variant: TextVariant::Muted, "Template totals" }
                        div { class: "flex justify-between text-sm",
                            span { "Lines subtotal" }
                            span { class: "tabular-nums",
                                "{format_money(subtotal, &draft.recurring.currency)}"
                            }
                        }
                        div { class: "flex justify-between text-sm",
                            span { "Generated so far" }
                            span { class: "tabular-nums", "{draft.recurring.generated_count}" }
                        }
                        Divider {}
                        Button {
                            variant: ButtonVariant::Primary,
                            on_click: move |_| props.on_save.call(()),
                            Save { size: 14 }
                            " Save template"
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn LinesGrid(
    draft: RecurringInvoiceDraft,
    on_change: EventHandler<RecurringInvoiceDraft>,
) -> Element {
    let lines = draft.lines.clone();
    rsx! {
        div { class: "flex flex-col gap-1",
            div { class: "grid items-center gap-2 px-1 py-1 text-xs text-muted-foreground border-b",
                style: "grid-template-columns: 1fr 80px 110px 110px 32px;",
                span { "Description" }
                span { class: "text-right", "Qty" }
                span { class: "text-right", "Price" }
                span { class: "text-right", "Amount" }
                span {}
            }
            for (idx, line) in lines.iter().cloned().enumerate() {
                LineRow {
                    key: "{line.id}",
                    line: line.clone(),
                    currency: draft.recurring.currency.clone(),
                    on_update: {
                        let d = draft.clone();
                        move |updated: RecurringInvoiceLine| {
                            let mut next = d.clone();
                            if idx < next.lines.len() {
                                next.lines[idx] = updated;
                            }
                            on_change.call(next);
                        }
                    },
                    on_delete: {
                        let d = draft.clone();
                        move |_| {
                            let mut next = d.clone();
                            if idx < next.lines.len() {
                                next.lines.remove(idx);
                            }
                            on_change.call(next);
                        }
                    },
                }
            }
            button {
                class: "flex items-center gap-2 rounded px-2 py-2 text-sm text-muted-foreground hover:bg-accent/50 border border-dashed mt-1",
                onclick: {
                    let d = draft.clone();
                    move |_| {
                        let mut next = d.clone();
                        let sort_index = next.lines.len() as i64;
                        next.lines.push(RecurringInvoiceLine {
                            id: Uuid::new_v4(),
                            recurring_invoice_id: next.recurring.id,
                            project_id: None,
                            description: String::new(),
                            quantity_thousandths: 1000,
                            unit_price_cents: 0,
                            amount_cents: 0,
                            sort_index,
                            created_at: Utc::now(),
                            updated_at: Utc::now(),
                        });
                        on_change.call(next);
                    }
                },
                Plus { size: 12 }
                " Add line"
            }
        }
    }
}

#[component]
fn LineRow(
    line: RecurringInvoiceLine,
    currency: String,
    on_update: EventHandler<RecurringInvoiceLine>,
    on_delete: EventHandler<()>,
) -> Element {
    let desc_signal = use_signal(|| line.description.clone());
    let qty_str = use_signal(|| format_qty_input(line.quantity_thousandths));
    let price_str = use_signal(|| format!("{:.2}", line.unit_price_cents as f64 / 100.0));
    let amount_label = format_money(line.amount_cents, &currency);

    let line_for_desc = line.clone();
    use_effect(move || {
        let v = desc_signal.read().clone();
        if v != line_for_desc.description {
            let mut next = line_for_desc.clone();
            next.description = v;
            on_update.call(next);
        }
    });
    let line_for_qty = line.clone();
    use_effect(move || {
        let v = qty_str.read().clone();
        let new_qty = parse_qty_input(&v).unwrap_or(0);
        if new_qty != line_for_qty.quantity_thousandths {
            let mut next = line_for_qty.clone();
            next.quantity_thousandths = new_qty;
            next.amount_cents = new_qty.saturating_mul(line_for_qty.unit_price_cents) / 1000;
            on_update.call(next);
        }
    });
    let line_for_price = line.clone();
    use_effect(move || {
        let v = price_str.read().clone();
        let new_price = parse_money_input(&v).unwrap_or(0);
        if new_price != line_for_price.unit_price_cents {
            let mut next = line_for_price.clone();
            next.unit_price_cents = new_price;
            next.amount_cents = line_for_price
                .quantity_thousandths
                .saturating_mul(new_price)
                / 1000;
            on_update.call(next);
        }
    });

    rsx! {
        div { class: "grid items-center gap-2 px-1 py-1 text-sm border-b",
            style: "grid-template-columns: 1fr 80px 110px 110px 32px;",
            Input { value: desc_signal, placeholder: "Description" }
            Input { value: qty_str, placeholder: "1" }
            Input { value: price_str, placeholder: "0.00" }
            span { class: "text-right tabular-nums font-mono text-xs", "{amount_label}" }
            Button {
                variant: ButtonVariant::Ghost,
                size: ButtonSize::Small,
                on_click: move |_| on_delete.call(()),
                Trash2 { size: 12 }
            }
        }
    }
}

#[component]
fn NotesField(
    draft: RecurringInvoiceDraft,
    on_change: EventHandler<RecurringInvoiceDraft>,
) -> Element {
    let initial = draft.recurring.notes.clone().unwrap_or_default();
    let value = use_signal(|| initial.clone());
    let d_for = draft.clone();
    use_effect(move || {
        let v = value.read().clone();
        let cur = d_for.recurring.notes.clone().unwrap_or_default();
        if v != cur {
            let mut next = d_for.clone();
            next.recurring.notes = if v.trim().is_empty() { None } else { Some(v) };
            on_change.call(next);
        }
    });
    rsx! {
        div { class: "flex flex-col gap-1",
            Label { "Notes (copied onto each generated invoice)" }
            Textarea { value, placeholder: "Payment terms, …" }
        }
    }
}

fn format_qty_input(thou: i64) -> String {
    if thou % 1000 == 0 {
        (thou / 1000).to_string()
    } else {
        format!("{:.3}", thou as f64 / 1000.0)
    }
}

fn parse_qty_input(s: &str) -> Option<i64> {
    let cleaned: String = s
        .chars()
        .filter(|c| c.is_ascii_digit() || *c == '.')
        .collect();
    if cleaned.is_empty() {
        return None;
    }
    let parsed: f64 = cleaned.parse().ok()?;
    Some((parsed * 1000.0).round() as i64)
}
