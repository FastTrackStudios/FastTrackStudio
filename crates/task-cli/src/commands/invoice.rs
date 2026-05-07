#![allow(clippy::needless_lifetimes, clippy::manual_strip)]
#![allow(unused_imports)]

use crate::*;

#[derive(Subcommand)]
pub(crate) enum InvoiceCommands {
    /// Create an invoice from uninvoiced billable entries for a client
    Create {
        client: String,
        /// Start of billing window (YYYY-MM-DD, inclusive)
        #[arg(long)]
        from: Option<String>,
        /// End of billing window (YYYY-MM-DD, inclusive)
        #[arg(long)]
        to: Option<String>,
        /// Fallback hourly rate in cents if cascade resolves to 0
        #[arg(long)]
        rate: Option<u32>,
        /// Invoice-level tax rate as a percentage, e.g. 8.5
        #[arg(long)]
        tax: Option<f64>,
        /// Invoice-level discount as a percentage
        #[arg(long)]
        discount: Option<f64>,
        #[arg(long)]
        po: Option<String>,
        #[arg(long)]
        notes: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// List invoices (newest first)
    List {
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        client: Option<String>,
        #[arg(long)]
        year: Option<i32>,
        #[arg(long)]
        json: bool,
    },
    /// Show billable/unbilled time, invoice balances, and aging
    Report {
        #[arg(long)]
        json: bool,
    },
    /// Show a single invoice
    Show {
        id: String,
        /// Print the full rendered markdown body instead of a summary
        #[arg(long)]
        md: bool,
        #[arg(long)]
        json: bool,
    },
    /// Mark an invoice as sent (sets sent_at, status → Sent)
    Send { id: String },
    /// Record a payment against an invoice
    Pay {
        id: String,
        /// Amount in cents (e.g. 50000 = $500)
        #[arg(long)]
        amount: u64,
        #[arg(long, default_value = "")]
        method: String,
        #[arg(long)]
        reference: Option<String>,
        #[arg(long)]
        notes: Option<String>,
    },
    /// Cancel an invoice
    Cancel {
        id: String,
        #[arg(long)]
        reason: Option<String>,
    },
}

pub(crate) async fn run_remote_invoice_command(
    remote: &RemoteVoxConfig,
    actor: Option<&str>,
    command: InvoiceCommands,
) -> eyre::Result<()> {
    let service = remote.invoice().await?;
    let repo = remote.invoice_repo().await?;
    match command {
        InvoiceCommands::Create {
            client: client_name,
            from,
            to,
            rate,
            tax,
            discount,
            po,
            notes,
            json,
        } => {
            let invoice = service
                .create_invoice_from_entries(task_core::InvoiceCreateRequest {
                    client_name,
                    from: from.as_deref().map(parse_date_start).transpose()?,
                    to: to.as_deref().map(parse_date_end).transpose()?,
                    fallback_rate: rate,
                    tax_rate_percent: tax,
                    discount_percent: discount,
                    po_number: po,
                    public_notes: notes,
                    actor: actor.map(str::to_string),
                })
                .await?;
            if json {
                println!("{}", facet_json::to_string(&invoice).unwrap_or_default());
            } else {
                print_invoice_detail(&invoice);
            }
        }
        InvoiceCommands::List {
            status,
            client: client_name,
            year,
            json,
        } => {
            let invoices: Vec<task_core::Invoice> = remote_list_invoices_with_client(&repo)
                .await?
                .into_iter()
                .filter(|i| match &status {
                    Some(s) => format!("{:?}", i.status).eq_ignore_ascii_case(s),
                    None => true,
                })
                .filter(|i| match &client_name {
                    Some(c) => i.client.0.eq_ignore_ascii_case(c),
                    None => true,
                })
                .filter(|i| match year {
                    Some(y) => i.issue_date.format("%Y").to_string() == format!("{y:04}"),
                    None => true,
                })
                .collect();
            if json {
                print_invoices_json(&invoices);
            } else {
                print_invoices_table(&invoices);
            }
        }
        InvoiceCommands::Report { json } => {
            let report = service.finance_report().await?;
            print_finance_report(&report, json);
        }
        InvoiceCommands::Show { id, md, json } => {
            let invoice = remote_find_invoice_with_client(&repo, &id)
                .await?
                .ok_or_else(|| eyre::eyre!("Invoice not found: {id}"))?;
            if json {
                println!("{}", facet_json::to_string(&invoice).unwrap_or_default());
            } else if md {
                println!("{}", task_core::invoice::render_invoice_body(&invoice));
            } else {
                print_invoice_detail(&invoice);
            }
        }
        InvoiceCommands::Send { id } => {
            let invoice = service.send_invoice(id, actor.map(str::to_string)).await?;
            println!(
                "Sent invoice {} — ${:.2} due {}.",
                invoice.id,
                invoice.total_cents() as f64 / 100.0,
                invoice.due_date
            );
        }
        InvoiceCommands::Pay {
            id,
            amount,
            method,
            reference,
            notes,
        } => {
            let invoice = service
                .record_invoice_payment(task_core::InvoicePaymentRequest {
                    invoice_id: id,
                    amount_cents: amount,
                    method: if method.is_empty() {
                        None
                    } else {
                        Some(method)
                    },
                    reference,
                    notes,
                    actor: actor.map(str::to_string),
                })
                .await?;
            println!(
                "Recorded ${:.2} against {}. Balance: ${:.2}. Status: {:?}",
                amount as f64 / 100.0,
                invoice.id,
                invoice.balance_cents() as f64 / 100.0,
                invoice.status
            );
        }
        InvoiceCommands::Cancel { id, reason } => {
            let invoice = service
                .cancel_invoice(id, reason, actor.map(str::to_string))
                .await?;
            println!("Cancelled invoice {}.", invoice.id);
        }
    }
    Ok(())
}
