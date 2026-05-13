//! `InvoiceEditor` — two-column invoice editor.
//!
//! Left: form (client, metadata, lines, notes).
//! Right: sticky totals breakdown + actions.
//!
//! Caller owns the editable copy via `InvoiceDraft` and receives a
//! single coarse `on_change(InvoiceDraft)` event.

use chrono::{Local, NaiveDate, TimeZone, Utc};
use dioxus::prelude::*;
use fts_ui::components::ComboboxItemData;
use fts_ui::lucide_dioxus::{CircleCheck, Mail, Plus, Save, Send, Trash2};
use fts_ui::prelude::*;
use invoice_proto::{Client, ClientCreate, Invoice, InvoiceLine};
use uuid::Uuid;

use super::client_picker::ClientPicker;
use super::common::{compute_totals, format_money, invoice_status_variant, parse_money_input};
use super::status::InvoiceStatusPipeline;

#[derive(Clone, Debug, PartialEq)]
pub struct InvoiceDraft {
    pub invoice: Invoice,
    pub lines: Vec<InvoiceLine>,
}

#[derive(Props, Clone, PartialEq)]
pub struct InvoiceEditorProps {
    pub draft: InvoiceDraft,
    pub clients: Vec<Client>,
    pub on_change: EventHandler<InvoiceDraft>,
    pub on_save_draft: EventHandler<()>,
    pub on_send: EventHandler<()>,
    pub on_record_payment: EventHandler<()>,
    pub on_create_client: EventHandler<ClientCreate>,
    #[props(default = false)]
    pub show_client_create: bool,
    #[props(default)]
    pub on_client_create_open: Option<EventHandler<bool>>,
    /// Optional handler for "Download PDF".
    ///
    /// **Web build:** the route wires this to open `InvoicePreview` in
    /// a new window and trigger `window.print()` — printpdf doesn't
    /// compile for wasm32.
    ///
    /// **Native build:** the route wires this to
    /// `invoice::pdf::render_invoice_pdf` and writes the resulting
    /// bytes to disk.
    #[props(default)]
    pub on_download_pdf: Option<EventHandler<()>>,
}

#[component]
pub fn InvoiceEditor(props: InvoiceEditorProps) -> Element {
    let draft = props.draft.clone();
    let on_change = props.on_change;
    let on_create_client_request = props.on_client_create_open;

    let status = draft.invoice.status.clone();
    let currency = draft.invoice.currency.clone();

    // Date inputs.
    let issue_str = draft
        .invoice
        .issue_date
        .date_naive()
        .format("%Y-%m-%d")
        .to_string();
    let due_str = draft
        .invoice
        .due_date
        .map(|d| d.date_naive().format("%Y-%m-%d").to_string())
        .unwrap_or_default();

    let currency_items: Vec<ComboboxItemData> = ["USD", "EUR", "GBP", "CAD", "AUD", "JPY"]
        .iter()
        .map(|c| ComboboxItemData::new(c.to_string(), c.to_string()))
        .collect();
    let currency_signal = use_signal(|| currency.clone());

    // Sync currency edits back into the draft.
    {
        let draft_for_cur = draft.clone();
        let on_change_cur = on_change;
        use_effect(move || {
            let cur = currency_signal.read().clone();
            if cur != draft_for_cur.invoice.currency && !cur.is_empty() {
                let mut next = draft_for_cur.clone();
                next.invoice.currency = cur;
                on_change_cur.call(next);
            }
        });
    }

    let payments_sum = 0; // Editor doesn't know payments; right rail derives balance from invoice.balance_cents below.
    let totals = compute_totals(
        &draft.lines,
        draft.invoice.discount_cents,
        draft.invoice.tax_rate_bps,
        draft.invoice.tax_inclusive,
        payments_sum,
    );
    let live_balance = draft
        .invoice
        .balance_cents
        .max(totals.total - 0)
        .min(totals.total)
        .max(0);

    let show_balance = status != "draft";

    rsx! {
        div { class: "grid grid-cols-1 lg:grid-cols-[1fr_300px] gap-4",

                // ── Left column ───────────────────────────────────────
                Card {
                    CardContent { class: "flex flex-col gap-4",

                        // Client + currency
                        div { class: "grid grid-cols-1 sm:grid-cols-[1fr_140px] gap-3",
                            div { class: "flex flex-col gap-1",
                                Label { "Client" }
                                ClientPicker {
                                    clients: props.clients.clone(),
                                    value: Some(draft.invoice.client_id).filter(|id| *id != Uuid::nil()),
                                    on_change: {
                                        let d = draft.clone();
                                        let on_change = on_change;
                                        move |id: Option<Uuid>| {
                                            let mut next = d.clone();
                                            next.invoice.client_id = id.unwrap_or(Uuid::nil());
                                            on_change.call(next);
                                        }
                                    },
                                    on_create_request: move |_| {
                                        if let Some(h) = on_create_client_request.as_ref() {
                                            h.call(true);
                                        }
                                    },
                                }
                            }
                            div { class: "flex flex-col gap-1",
                                Label { "Currency" }
                                Combobox {
                                    value: currency_signal,
                                    ComboboxTrigger { placeholder: "USD".to_string() }
                                    ComboboxContent { items: currency_items }
                                }
                            }
                        }

                        // Metadata grid
                        div { class: "grid grid-cols-2 gap-3",
                            NumberField {
                                draft: draft.clone(),
                                on_change,
                            }
                            PoField {
                                draft: draft.clone(),
                                on_change,
                            }
                            DateField {
                                label: "Issue date".to_string(),
                                value: issue_str.clone(),
                                on_set: {
                                    let d = draft.clone();
                                    let on_change = on_change;
                                    move |new_str: String| {
                                        if let Ok(date) = NaiveDate::parse_from_str(&new_str, "%Y-%m-%d") {
                                            let naive = date.and_hms_opt(12, 0, 0).unwrap_or_default();
                                            if let Some(local) = Local.from_local_datetime(&naive).single() {
                                                let mut next = d.clone();
                                                next.invoice.issue_date = local.with_timezone(&Utc);
                                                on_change.call(next);
                                            }
                                        }
                                    }
                                },
                            }
                            DateField {
                                label: "Due date".to_string(),
                                value: due_str.clone(),
                                on_set: {
                                    let d = draft.clone();
                                    let on_change = on_change;
                                    move |new_str: String| {
                                        let mut next = d.clone();
                                        if new_str.trim().is_empty() {
                                            next.invoice.due_date = None;
                                            on_change.call(next);
                                            return;
                                        }
                                        if let Ok(date) = NaiveDate::parse_from_str(&new_str, "%Y-%m-%d") {
                                            let naive = date.and_hms_opt(12, 0, 0).unwrap_or_default();
                                            if let Some(local) = Local.from_local_datetime(&naive).single() {
                                                next.invoice.due_date = Some(local.with_timezone(&Utc));
                                                on_change.call(next);
                                            }
                                        }
                                    }
                                },
                            }
                        }

                        Divider {}

                        // Tax + discount row
                        TaxDiscountRow {
                            draft: draft.clone(),
                            on_change,
                        }

                        Divider {}

                        // Lines table
                        LinesTable {
                            draft: draft.clone(),
                            on_change,
                        }

                        Divider {}

                        // Notes
                        Label { "Notes" }
                        NotesField {
                            draft: draft.clone(),
                            on_change,
                        }
                    }
                }

                // ── Right rail ────────────────────────────────────────
                div { class: "lg:sticky lg:top-4 self-start",
                    Card {
                        CardContent { class: "flex flex-col gap-4",
                            HStack { class: "items-center gap-2",
                                StatusBadge {
                                    variant: invoice_status_variant(&status),
                                    label: status.clone(),
                                }
                            }
                            InvoiceStatusPipeline {
                                status: status.clone(),
                                overdue: status == "overdue",
                            }

                            Divider {}

                            div { class: "flex flex-col gap-1 text-sm",
                                div { class: "flex justify-between",
                                    span { class: "text-muted-foreground", "Subtotal" }
                                    span { class: "tabular-nums",
                                        "{format_money(totals.subtotal, &draft.invoice.currency)}"
                                    }
                                }
                                if draft.invoice.discount_cents != 0 {
                                    div { class: "flex justify-between",
                                        span { class: "text-muted-foreground", "Discount" }
                                        span { class: "tabular-nums",
                                            "-{format_money(draft.invoice.discount_cents, &draft.invoice.currency)}"
                                        }
                                    }
                                }
                                if draft.invoice.tax_rate_bps > 0 {
                                    div { class: "flex justify-between",
                                        span { class: "text-muted-foreground",
                                            "Tax ({(draft.invoice.tax_rate_bps as f64) / 100.0}%)"
                                        }
                                        span { class: "tabular-nums",
                                            "{format_money(totals.tax, &draft.invoice.currency)}"
                                        }
                                    }
                                }
                                div { class: "flex justify-between border-t pt-1 font-semibold",
                                    span { "Total" }
                                    span { class: "tabular-nums",
                                        "{format_money(totals.total, &draft.invoice.currency)}"
                                    }
                                }
                                if show_balance {
                                    div { class: "flex justify-between text-sm",
                                        span { class: "text-muted-foreground", "Balance due" }
                                        span { class: "tabular-nums font-medium",
                                            "{format_money(live_balance, &draft.invoice.currency)}"
                                        }
                                    }
                                }
                            }

                            Divider {}

                            div { class: "flex flex-col gap-2",
                                if status == "draft" {
                                    Button {
                                        variant: ButtonVariant::Outline,
                                        on_click: move |_| props.on_save_draft.call(()),
                                        Save { size: 14 }
                                        " Save draft"
                                    }
                                    Button {
                                        variant: ButtonVariant::Primary,
                                        on_click: move |_| props.on_send.call(()),
                                        Send { size: 14 }
                                        " Send"
                                    }
                                } else {
                                    Button {
                                        variant: ButtonVariant::Outline,
                                        on_click: move |_| props.on_save_draft.call(()),
                                        Save { size: 14 }
                                        " Save"
                                    }
                                }
                                if let Some(handler) = props.on_download_pdf.as_ref().cloned() {
                                    Button {
                                        variant: ButtonVariant::Outline,
                                        on_click: move |_| handler.call(()),
                                        " Download PDF"
                                    }
                                }
                                if matches!(status.as_str(), "sent" | "viewed" | "partial" | "overdue") {
                                    Button {
                                        variant: ButtonVariant::Primary,
                                        on_click: move |_| props.on_record_payment.call(()),
                                        Mail { size: 14 }
                                        " Record payment"
                                    }
                                    Button {
                                        variant: ButtonVariant::Ghost,
                                        on_click: {
                                            let d = draft.clone();
                                            let on_change = on_change;
                                            move |_| {
                                                let mut next = d.clone();
                                                next.invoice.status = "paid".into();
                                                next.invoice.paid_at = Some(Utc::now());
                                                next.invoice.balance_cents = 0;
                                                on_change.call(next);
                                            }
                                        },
                                        CircleCheck { size: 14 }
                                        " Mark paid"
                                    }
                                }
                            }
                        }
                    }
                }
        }
    }
}

#[component]
fn NumberField(draft: InvoiceDraft, on_change: EventHandler<InvoiceDraft>) -> Element {
    let value = use_signal(|| draft.invoice.number.clone());
    let draft_for_effect = draft.clone();
    use_effect(move || {
        let v = value.read().clone();
        if v != draft_for_effect.invoice.number {
            let mut next = draft_for_effect.clone();
            next.invoice.number = v;
            on_change.call(next);
        }
    });
    rsx! {
        div { class: "flex flex-col gap-1",
            Label { "Number" }
            Input { value, placeholder: "INV-2026-0001" }
        }
    }
}

#[component]
fn PoField(draft: InvoiceDraft, on_change: EventHandler<InvoiceDraft>) -> Element {
    // PO stored in tags as "po:XXX" — first match wins.
    let initial_po = draft
        .invoice
        .tags
        .iter()
        .find_map(|t| t.strip_prefix("po:").map(|s| s.to_string()))
        .unwrap_or_default();
    let value = use_signal(|| initial_po.clone());
    let draft_for_effect = draft.clone();
    use_effect(move || {
        let v = value.read().clone();
        let current = draft_for_effect
            .invoice
            .tags
            .iter()
            .find_map(|t| t.strip_prefix("po:").map(|s| s.to_string()))
            .unwrap_or_default();
        if v != current {
            let mut next = draft_for_effect.clone();
            next.invoice.tags.retain(|t| !t.starts_with("po:"));
            if !v.trim().is_empty() {
                next.invoice.tags.push(format!("po:{}", v.trim()));
            }
            on_change.call(next);
        }
    });
    rsx! {
        div { class: "flex flex-col gap-1",
            Label { "PO #" }
            Input { value, placeholder: "(optional)" }
        }
    }
}

#[component]
fn DateField(label: String, value: String, on_set: EventHandler<String>) -> Element {
    rsx! {
        div { class: "flex flex-col gap-1",
            Label { "{label}" }
            input {
                r#type: "date",
                class: "h-9 rounded border bg-background px-2 text-sm",
                value: "{value}",
                oninput: move |e| on_set.call(e.value()),
            }
        }
    }
}

#[component]
fn NotesField(draft: InvoiceDraft, on_change: EventHandler<InvoiceDraft>) -> Element {
    let initial = draft.invoice.notes.clone().unwrap_or_default();
    let value = use_signal(|| initial.clone());
    let draft_for_effect = draft.clone();
    use_effect(move || {
        let v = value.read().clone();
        let cur = draft_for_effect.invoice.notes.clone().unwrap_or_default();
        if v != cur {
            let mut next = draft_for_effect.clone();
            next.invoice.notes = if v.trim().is_empty() { None } else { Some(v) };
            on_change.call(next);
        }
    });
    rsx! {
        Textarea { value, placeholder: "Payment terms, thank-you note, …".to_string() }
    }
}

#[component]
fn TaxDiscountRow(draft: InvoiceDraft, on_change: EventHandler<InvoiceDraft>) -> Element {
    let initial_tax = if draft.invoice.tax_rate_bps == 0 {
        String::new()
    } else {
        format!("{:.2}", draft.invoice.tax_rate_bps as f64 / 100.0)
    };
    let tax_str = use_signal(|| initial_tax.clone());
    let inclusive = use_signal(|| draft.invoice.tax_inclusive);
    let initial_discount = if draft.invoice.discount_cents == 0 {
        String::new()
    } else {
        format!("{:.2}", draft.invoice.discount_cents as f64 / 100.0)
    };
    let discount_str = use_signal(|| initial_discount.clone());

    // Tax → bps
    {
        let d = draft.clone();
        use_effect(move || {
            let raw = tax_str.read().clone();
            let cleaned: String = raw
                .chars()
                .filter(|c| c.is_ascii_digit() || *c == '.')
                .collect();
            let parsed = cleaned.parse::<f64>().ok().unwrap_or(0.0);
            let new_bps = (parsed * 100.0).round() as i32;
            if new_bps != d.invoice.tax_rate_bps {
                let mut next = d.clone();
                next.invoice.tax_rate_bps = new_bps;
                on_change.call(next);
            }
        });
    }
    // Inclusive
    {
        let d = draft.clone();
        use_effect(move || {
            let v = *inclusive.read();
            if v != d.invoice.tax_inclusive {
                let mut next = d.clone();
                next.invoice.tax_inclusive = v;
                on_change.call(next);
            }
        });
    }
    // Discount → cents
    {
        let d = draft.clone();
        use_effect(move || {
            let raw = discount_str.read().clone();
            let new_cents = parse_money_input(&raw).unwrap_or(0);
            if new_cents != d.invoice.discount_cents {
                let mut next = d.clone();
                next.invoice.discount_cents = new_cents;
                on_change.call(next);
            }
        });
    }

    rsx! {
        div { class: "grid grid-cols-3 gap-3 items-end",
            div { class: "flex flex-col gap-1",
                Label { "Tax rate %" }
                Input { value: tax_str, placeholder: "0" }
            }
            div { class: "flex flex-col gap-1",
                Label { "Inclusive" }
                HStack { class: "items-center gap-2 h-9",
                    Switch { checked: inclusive }
                    Text { variant: TextVariant::Muted,
                        if *inclusive.read() { "Tax in subtotal" } else { "Tax added" }
                    }
                }
            }
            div { class: "flex flex-col gap-1",
                Label { "Discount" }
                Input { value: discount_str, placeholder: "0.00" }
            }
        }
    }
}

#[component]
fn LinesTable(draft: InvoiceDraft, on_change: EventHandler<InvoiceDraft>) -> Element {
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
                    currency: draft.invoice.currency.clone(),
                    on_update: {
                        let d = draft.clone();
                        let on_change = on_change;
                        move |updated: InvoiceLine| {
                            let mut next = d.clone();
                            if idx < next.lines.len() {
                                next.lines[idx] = updated;
                            }
                            on_change.call(next);
                        }
                    },
                    on_delete: {
                        let d = draft.clone();
                        let on_change = on_change;
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

            // Ghost add row
            button {
                class: "flex items-center gap-2 rounded px-2 py-2 text-sm text-muted-foreground hover:bg-accent/50 border border-dashed mt-1",
                onclick: {
                    let d = draft.clone();
                    let on_change = on_change;
                    move |_| {
                        let mut next = d.clone();
                        let sort_index = next.lines.len() as i64;
                        next.lines.push(InvoiceLine {
                            id: Uuid::new_v4(),
                            invoice_id: next.invoice.id,
                            project_id: None,
                            time_entry_id: None,
                            description: String::new(),
                            quantity_thousandths: 1000,
                            unit_price_cents: 0,
                            amount_cents: 0,
                            tax_rate_bps: None,
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
    line: InvoiceLine,
    currency: String,
    on_update: EventHandler<InvoiceLine>,
    on_delete: EventHandler<()>,
) -> Element {
    let desc_signal = use_signal(|| line.description.clone());
    let qty_str = use_signal(|| format_qty_input(line.quantity_thousandths));
    let price_str = use_signal(|| format!("{:.2}", line.unit_price_cents as f64 / 100.0));

    let amount_label = format_money(line.amount_cents, &currency);

    let line_for_desc = line.clone();
    let on_update_desc = on_update;
    use_effect(move || {
        let v = desc_signal.read().clone();
        if v != line_for_desc.description {
            let mut next = line_for_desc.clone();
            next.description = v;
            on_update_desc.call(next);
        }
    });

    let line_for_qty = line.clone();
    let on_update_qty = on_update;
    use_effect(move || {
        let v = qty_str.read().clone();
        let new_qty = parse_qty_input(&v).unwrap_or(0);
        if new_qty != line_for_qty.quantity_thousandths {
            let mut next = line_for_qty.clone();
            next.quantity_thousandths = new_qty;
            next.amount_cents = new_qty.saturating_mul(line_for_qty.unit_price_cents) / 1000;
            on_update_qty.call(next);
        }
    });

    let line_for_price = line.clone();
    let on_update_price = on_update;
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
            on_update_price.call(next);
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
