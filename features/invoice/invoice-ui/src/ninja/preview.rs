//! `InvoicePreview` — printable letterhead-style invoice rendering.

use dioxus::prelude::*;
use invoice_proto::{Client, Invoice, InvoiceLine, Payment};

use super::common::{compute_totals, format_money, format_qty_thousandths};

#[derive(Props, Clone, PartialEq)]
pub struct InvoicePreviewProps {
    pub invoice: Invoice,
    pub lines: Vec<InvoiceLine>,
    pub client: Client,
    pub payments: Vec<Payment>,
}

#[component]
pub fn InvoicePreview(props: InvoicePreviewProps) -> Element {
    let inv = props.invoice.clone();
    let client = props.client.clone();
    let lines = props.lines.clone();
    let payments_sum: i64 = props.payments.iter().map(|p| p.amount_cents).sum();

    let totals = compute_totals(
        &lines,
        inv.discount_cents,
        inv.tax_rate_bps,
        inv.tax_inclusive,
        payments_sum,
    );

    let issue = inv.issue_date.format("%b %-d, %Y").to_string();
    let due = inv
        .due_date
        .map(|d| d.format("%b %-d, %Y").to_string())
        .unwrap_or_else(|| "—".into());

    let tax_label = if inv.tax_rate_bps > 0 {
        format!("Tax ({:.2}%)", inv.tax_rate_bps as f64 / 100.0)
    } else {
        "Tax".to_string()
    };

    rsx! {
        div {
            class: "rounded-lg border bg-white text-neutral-900 shadow-sm p-10 max-w-3xl mx-auto",
            style: "color: #111; background: #fff;",

            // Header
            div { class: "flex items-start justify-between mb-8",
                div { class: "flex flex-col gap-1",
                    div { class: "text-2xl font-semibold", "FastTrack Studio" }
                    div { class: "text-sm text-neutral-600",
                        "1 Production Lane"
                        br {}
                        "Portland, OR 97204"
                        br {}
                        "billing@fasttrack.test"
                    }
                }
                div { class: "flex flex-col items-end gap-1",
                    div { class: "text-3xl font-light tracking-tight", "INVOICE" }
                    div { class: "font-mono text-sm text-neutral-600", "{inv.number}" }
                }
            }

            // Bill-to + dates
            div { class: "grid grid-cols-2 gap-8 mb-8",
                div { class: "flex flex-col gap-1",
                    div { class: "text-xs uppercase tracking-wide text-neutral-500", "Bill to" }
                    div { class: "text-sm font-medium", "{client.name}" }
                    if let Some(email) = client.email.as_deref() {
                        div { class: "text-sm text-neutral-600", "{email}" }
                    }
                    if let Some(line1) = client.billing_address_line1.as_deref() {
                        div { class: "text-sm text-neutral-600", "{line1}" }
                    }
                    if let Some(city) = client.billing_city.as_deref() {
                        div { class: "text-sm text-neutral-600",
                            "{city}"
                            if let Some(region) = client.billing_region.as_deref() {
                                ", {region}"
                            }
                            if let Some(zip) = client.billing_postal_code.as_deref() {
                                " {zip}"
                            }
                        }
                    }
                }
                div { class: "flex flex-col gap-1 text-right",
                    div { class: "grid grid-cols-2 gap-2 text-sm",
                        span { class: "text-neutral-500", "Issued" }
                        span { class: "tabular-nums", "{issue}" }
                        span { class: "text-neutral-500", "Due" }
                        span { class: "tabular-nums", "{due}" }
                        span { class: "text-neutral-500", "Status" }
                        span { class: "capitalize", "{inv.status}" }
                    }
                }
            }

            // Lines
            div { class: "mb-6",
                div { class: "grid items-center gap-2 border-b py-2 text-xs uppercase tracking-wide text-neutral-500",
                    style: "grid-template-columns: 1fr 80px 110px 110px;",
                    span { "Description" }
                    span { class: "text-right", "Qty" }
                    span { class: "text-right", "Price" }
                    span { class: "text-right", "Amount" }
                }
                for line in lines.iter() {
                    div { key: "{line.id}",
                        class: "grid items-center gap-2 border-b py-2 text-sm",
                        style: "grid-template-columns: 1fr 80px 110px 110px;",
                        span { "{line.description}" }
                        span { class: "text-right tabular-nums",
                            "{format_qty_thousandths(line.quantity_thousandths)}"
                        }
                        span { class: "text-right tabular-nums",
                            "{format_money(line.unit_price_cents, &inv.currency)}"
                        }
                        span { class: "text-right tabular-nums",
                            "{format_money(line.amount_cents, &inv.currency)}"
                        }
                    }
                }
            }

            // Totals
            div { class: "flex justify-end mb-6",
                div { class: "w-72 flex flex-col gap-1 text-sm",
                    div { class: "flex justify-between",
                        span { class: "text-neutral-500", "Subtotal" }
                        span { class: "tabular-nums",
                            "{format_money(totals.subtotal, &inv.currency)}"
                        }
                    }
                    if inv.discount_cents != 0 {
                        div { class: "flex justify-between",
                            span { class: "text-neutral-500", "Discount" }
                            span { class: "tabular-nums",
                                "-{format_money(inv.discount_cents, &inv.currency)}"
                            }
                        }
                    }
                    if inv.tax_rate_bps > 0 {
                        div { class: "flex justify-between",
                            span { class: "text-neutral-500", "{tax_label}" }
                            span { class: "tabular-nums",
                                "{format_money(totals.tax, &inv.currency)}"
                            }
                        }
                    }
                    div { class: "flex justify-between border-t pt-1 font-semibold",
                        span { "Total" }
                        span { class: "tabular-nums",
                            "{format_money(totals.total, &inv.currency)}"
                        }
                    }
                    if payments_sum > 0 {
                        div { class: "flex justify-between",
                            span { class: "text-neutral-500", "Paid" }
                            span { class: "tabular-nums",
                                "-{format_money(payments_sum, &inv.currency)}"
                            }
                        }
                        div { class: "flex justify-between font-semibold",
                            span { "Balance due" }
                            span { class: "tabular-nums",
                                "{format_money(totals.balance, &inv.currency)}"
                            }
                        }
                    }
                }
            }

            // Notes
            if let Some(notes) = inv.notes.as_deref() {
                div { class: "border-t pt-4 text-sm text-neutral-600 whitespace-pre-wrap",
                    "{notes}"
                }
            }
        }
    }
}
