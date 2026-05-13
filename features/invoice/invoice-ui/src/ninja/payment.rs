//! `PaymentRecordDialog` — modal listing existing payments plus a
//! form to record a new one.

use chrono::{Local, NaiveDate, TimeZone, Utc};
use dioxus::prelude::*;
use fts_ui::components::ComboboxItemData;
use fts_ui::prelude::*;
use invoice_proto::{Invoice, PAYMENT_METHODS, Payment, PaymentInput};

use super::common::{format_money, parse_money_input};

#[derive(Props, Clone, PartialEq)]
pub struct PaymentRecordDialogProps {
    pub open: bool,
    pub invoice: Invoice,
    pub payments: Vec<Payment>,
    pub on_submit: EventHandler<PaymentInput>,
    pub on_close: EventHandler<()>,
}

#[component]
pub fn PaymentRecordDialog(props: PaymentRecordDialogProps) -> Element {
    if !props.open {
        return rsx! {};
    }

    let invoice = props.invoice.clone();
    let currency = invoice.currency.clone();
    let default_amount = invoice.balance_cents.max(0);

    let amount = use_signal(|| {
        if default_amount > 0 {
            format!("{:.2}", default_amount as f64 / 100.0)
        } else {
            String::new()
        }
    });
    let date_str = use_signal(|| Utc::now().date_naive().format("%Y-%m-%d").to_string());
    let method = use_signal(|| "stripe".to_string());
    let reference = use_signal(String::new);
    let notes = use_signal(String::new);

    let method_items: Vec<ComboboxItemData> = PAYMENT_METHODS
        .iter()
        .map(|m| ComboboxItemData::new(m.to_string(), (*m).to_string()))
        .collect();

    let payments = props.payments.clone();
    let currency_for_list = currency.clone();
    let on_submit = props.on_submit;
    let on_close = props.on_close;

    let do_submit = use_callback(move |_: ()| {
        let amt = parse_money_input(&amount.read());
        let Some(amt) = amt else {
            return;
        };
        if amt <= 0 {
            return;
        }
        let d = NaiveDate::parse_from_str(&date_str.read(), "%Y-%m-%d");
        let paid_at = d
            .ok()
            .and_then(|d| d.and_hms_opt(12, 0, 0))
            .and_then(|naive| Local.from_local_datetime(&naive).single())
            .map(|local| local.with_timezone(&Utc))
            .unwrap_or_else(Utc::now);

        let reference_v = reference.read().clone();
        let notes_v = notes.read().clone();
        let payload = PaymentInput {
            amount_cents: amt,
            paid_at,
            method: method.read().clone(),
            reference: if reference_v.trim().is_empty() {
                None
            } else {
                Some(reference_v)
            },
            notes: if notes_v.trim().is_empty() {
                None
            } else {
                Some(notes_v)
            },
        };
        on_submit.call(payload);
        on_close.call(());
    });

    rsx! {
        Dialog {
            open: true,
            is_modal: true,
            on_open_change: move |open: bool| {
                if !open {
                    on_close.call(());
                }
            },
            DialogHeader {
                DialogTitle { "Record payment" }
                DialogDescription { "Invoice {invoice.number}" }
            }

            div { class: "flex flex-col gap-4",
                // Existing payments
                if !payments.is_empty() {
                    div { class: "rounded border bg-muted/30 p-2 max-h-32 overflow-auto",
                        div { class: "text-xs text-muted-foreground mb-1 font-medium",
                            "Previous payments"
                        }
                        for p in payments.iter() {
                            div { key: "{p.id}",
                                class: "grid grid-cols-4 gap-2 text-xs tabular-nums py-0.5",
                                span { "{p.paid_at.format(\"%Y-%m-%d\")}" }
                                span { class: "text-right", "{format_money(p.amount_cents, &currency_for_list)}" }
                                span { "{p.method}" }
                                span { class: "text-muted-foreground truncate",
                                    "{p.reference.clone().unwrap_or_default()}"
                                }
                            }
                        }
                    }
                }

                div { class: "grid grid-cols-2 gap-3",
                    div { class: "flex flex-col gap-1",
                        Label { "Amount" }
                        Input { value: amount, placeholder: "0.00" }
                    }
                    div { class: "flex flex-col gap-1",
                        Label { "Paid on" }
                        input {
                            r#type: "date",
                            class: "h-9 rounded border bg-background px-2 text-sm",
                            value: "{date_str}",
                            oninput: move |e| date_str.clone().set(e.value()),
                        }
                    }
                }

                Label { "Method" }
                Combobox {
                    value: method,
                    ComboboxTrigger { placeholder: "stripe".to_string() }
                    ComboboxContent { items: method_items }
                }

                Label { "Reference" }
                Input { value: reference, placeholder: "ch_abc123 / check #1042 / …" }

                Label { "Notes" }
                Textarea { value: notes, placeholder: "Optional notes".to_string() }
            }

            DialogFooter {
                Button {
                    variant: ButtonVariant::Ghost,
                    on_click: move |_| on_close.call(()),
                    "Cancel"
                }
                Button {
                    variant: ButtonVariant::Primary,
                    on_click: move |_| do_submit.call(()),
                    "Record payment"
                }
            }
        }
    }
}
