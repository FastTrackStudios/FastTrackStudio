//! SeaORM-backed [`InvoiceService`] implementation.
//!
//! Wraps a [`crate::invoice::InvoiceRepo`] and exposes the lifecycle
//! mutations (send, payment, cancel) plus a repo-driven
//! [`BusinessFinanceReport`].
//!
//! Time-entry rollup is intentionally minimal here: we don't yet have a
//! `TimeService` impl backed by SeaORM, so `finance_report` reports the
//! invoice side of the picture and leaves the unbilled-entry buckets
//! empty. Once a time-entry repo lands, the same `build_finance_report`
//! helper can be fed real `TimeEntryContext`s.

use chrono::{NaiveDate, Utc};
use uuid::Uuid;

use super::helpers::{convert_model, convert_ref};
use crate::invoice::{
    Invoice, InvoiceApi, InvoiceApiList, InvoiceApiUpdate, InvoiceRepo, InvoiceStatus, Payment,
};
use crate::service::{
    BusinessFinanceClientSummary, BusinessFinanceReport, InvoiceAgingBucket, InvoiceCreateRequest,
    InvoicePaymentRequest, InvoiceService, TimeEntryContext, VaultError,
};

/// Typed requirements for [`InvoiceServiceImpl`].
pub struct InvoiceServiceDeps<R> {
    pub invoice_repo: R,
}

#[derive(Clone)]
pub struct InvoiceServiceImpl<R> {
    invoice_repo: R,
}

impl<R> InvoiceServiceImpl<R> {
    pub fn new(deps: InvoiceServiceDeps<R>) -> Self {
        Self {
            invoice_repo: deps.invoice_repo,
        }
    }
}

impl<R> InvoiceServiceImpl<R>
where
    R: InvoiceRepo,
{
    async fn list_invoice_models(&self) -> Result<Vec<Invoice>, VaultError> {
        self.invoice_repo
            .list_invoices(None, None, None, Some(10_000))
            .await
            .map_err(VaultError::ParseError)?
            .into_iter()
            .map(convert_model::<InvoiceApiList, Invoice>)
            .collect()
    }

    async fn find_invoice(&self, invoice_id: &str) -> Result<Invoice, VaultError> {
        self.list_invoice_models()
            .await?
            .into_iter()
            .find(|invoice| {
                invoice.id.eq_ignore_ascii_case(invoice_id)
                    || invoice.uuid.to_string().eq_ignore_ascii_case(invoice_id)
            })
            .ok_or_else(|| VaultError::NotFound(invoice_id.to_string()))
    }

    async fn save_invoice(&self, invoice: &Invoice) -> Result<Invoice, VaultError> {
        let update = convert_ref::<Invoice, InvoiceApiUpdate>(invoice)?;
        self.invoice_repo
            .update_invoice(invoice.uuid.to_string(), update)
            .await
            .map_err(VaultError::ParseError)
            .and_then(convert_model::<InvoiceApi, Invoice>)
    }
}

impl<R> InvoiceService for InvoiceServiceImpl<R>
where
    R: InvoiceRepo,
{
    async fn create_invoice_from_entries(
        &self,
        request: InvoiceCreateRequest,
    ) -> Result<Invoice, VaultError> {
        // Without a SeaORM-backed time-entry repo, we can't roll up
        // `TimeEntry` rows here. Surface that clearly so the caller can
        // fall back to a repo-direct create until that lands.
        let _ = request;
        Err(VaultError::IoError(
            "provider not configured: create_invoice_from_entries (needs time-entry repo)".into(),
        ))
    }

    async fn finance_report(&self) -> BusinessFinanceReport {
        let today = chrono::Local::now().date_naive();
        let invoices = self.list_invoice_models().await.unwrap_or_default();
        build_finance_report(Vec::new(), invoices, today)
    }

    async fn send_invoice(
        &self,
        invoice_id: String,
        _actor: Option<String>,
    ) -> Result<Invoice, VaultError> {
        let mut invoice = self.find_invoice(&invoice_id).await?;
        if matches!(
            invoice.status,
            InvoiceStatus::Cancelled | InvoiceStatus::Refunded
        ) {
            return Err(VaultError::IoError(format!(
                "cannot send {:?} invoice",
                invoice.status
            )));
        }
        let now = Utc::now();
        if invoice.sent_at.is_none() {
            invoice.sent_at = Some(now);
            invoice.status = InvoiceStatus::Sent;
        }
        invoice.date_modified = Some(now);
        self.save_invoice(&invoice).await
    }

    async fn record_invoice_payment(
        &self,
        request: InvoicePaymentRequest,
    ) -> Result<Invoice, VaultError> {
        let mut invoice = self.find_invoice(&request.invoice_id).await?;
        let now = Utc::now();
        invoice.payments.push(Payment {
            id: Uuid::new_v4().to_string(),
            amount_cents: request.amount_cents,
            received_at: now,
            method: request.method.unwrap_or_default(),
            reference: request.reference,
            recorded_by: request.actor,
            notes: request.notes,
        });
        invoice.date_modified = Some(now);
        let today = chrono::Local::now().date_naive();
        invoice.status = invoice.derive_status(today);
        if matches!(invoice.status, InvoiceStatus::Paid) && invoice.paid_at.is_none() {
            invoice.paid_at = Some(now);
        }
        self.save_invoice(&invoice).await
    }

    async fn cancel_invoice(
        &self,
        invoice_id: String,
        reason: Option<String>,
        _actor: Option<String>,
    ) -> Result<Invoice, VaultError> {
        let mut invoice = self.find_invoice(&invoice_id).await?;
        if matches!(invoice.status, InvoiceStatus::Cancelled) {
            return Ok(invoice);
        }
        let now = Utc::now();
        invoice.status = InvoiceStatus::Cancelled;
        invoice.cancelled_at = Some(now);
        invoice.cancelled_reason = reason;
        invoice.date_modified = Some(now);
        self.save_invoice(&invoice).await
    }
}

/// Roll up time entries and invoices into the finance report.
///
/// Mirrors the pre-cd6b026 helper. Unbilled time entries can be empty
/// when the caller has no time-entry repo wired yet — invoice-side
/// totals (open balance, aging, drafts) still report correctly.
pub(crate) fn build_finance_report(
    time_entries: Vec<TimeEntryContext>,
    invoices: Vec<Invoice>,
    today: NaiveDate,
) -> BusinessFinanceReport {
    let billable_entries: Vec<TimeEntryContext> = time_entries
        .into_iter()
        .filter(|entry| entry.entry.billable && !entry.entry.is_running())
        .collect();
    let mut unbilled_entries: Vec<TimeEntryContext> = billable_entries
        .iter()
        .filter(|entry| entry.entry.invoiced_at.is_none())
        .cloned()
        .collect();
    unbilled_entries.sort_by_key(|entry| entry.entry.start_time);

    let billable_minutes: u32 = billable_entries
        .iter()
        .map(|entry| entry.entry.duration_minutes())
        .sum();
    let unbilled_minutes: u32 = unbilled_entries
        .iter()
        .map(|entry| entry.entry.duration_minutes())
        .sum();
    let unbilled_cents: u64 = unbilled_entries.iter().map(time_entry_cents).sum();

    let invoiced_cents: u64 = invoices.iter().map(Invoice::total_cents).sum();
    let paid_cents: u64 = invoices.iter().map(Invoice::paid_cents).sum();

    let open_invoices: Vec<Invoice> = invoices
        .iter()
        .filter(|invoice| invoice.balance_cents() > 0 && invoice.cancelled_at.is_none())
        .cloned()
        .collect();
    let open_invoice_cents: u64 = open_invoices.iter().map(Invoice::balance_cents).sum();
    let overdue_invoice_cents: u64 = open_invoices
        .iter()
        .filter(|invoice| invoice.due_date < today)
        .map(Invoice::balance_cents)
        .sum();
    let draft_invoices: Vec<Invoice> = invoices
        .iter()
        .filter(|invoice| matches!(invoice.status, InvoiceStatus::Draft))
        .cloned()
        .collect();

    let mut clients = std::collections::BTreeMap::<String, BusinessFinanceClientSummary>::new();
    for entry in &unbilled_entries {
        let client_name = entry
            .client_name
            .clone()
            .unwrap_or_else(|| "Unassigned".to_string());
        let summary =
            clients
                .entry(client_name.clone())
                .or_insert_with(|| BusinessFinanceClientSummary {
                    client_name,
                    ..Default::default()
                });
        summary.unbilled_minutes += entry.entry.duration_minutes();
        summary.unbilled_cents += time_entry_cents(entry);
    }
    for invoice in &open_invoices {
        let client_name = strip_wikilink_brackets(&invoice.client.0);
        let summary =
            clients
                .entry(client_name.clone())
                .or_insert_with(|| BusinessFinanceClientSummary {
                    client_name,
                    ..Default::default()
                });
        summary.open_invoice_cents += invoice.balance_cents();
        if invoice.due_date < today {
            summary.overdue_invoice_cents += invoice.balance_cents();
        }
    }

    BusinessFinanceReport {
        generated_at: Utc::now(),
        today: today.to_string(),
        billable_minutes,
        unbilled_minutes,
        unbilled_cents,
        invoiced_cents,
        paid_cents,
        open_invoice_cents,
        overdue_invoice_cents,
        clients: clients.into_values().collect(),
        aging: invoice_aging(&open_invoices, today),
        draft_invoices,
        open_invoices,
        unbilled_entries,
    }
}

fn time_entry_cents(entry: &TimeEntryContext) -> u64 {
    let minutes = entry.entry.duration_minutes() as u64;
    let rate = entry.effective_rate(None) as u64;
    ((minutes * rate) + 30) / 60
}

fn invoice_aging(invoices: &[Invoice], today: NaiveDate) -> Vec<InvoiceAgingBucket> {
    let mut buckets = vec![
        InvoiceAgingBucket {
            name: "current".into(),
            ..Default::default()
        },
        InvoiceAgingBucket {
            name: "1-30".into(),
            ..Default::default()
        },
        InvoiceAgingBucket {
            name: "31-60".into(),
            ..Default::default()
        },
        InvoiceAgingBucket {
            name: "61-90".into(),
            ..Default::default()
        },
        InvoiceAgingBucket {
            name: "90+".into(),
            ..Default::default()
        },
    ];
    for invoice in invoices {
        let overdue_days = (today - invoice.due_date).num_days();
        let index = if overdue_days <= 0 {
            0
        } else if overdue_days <= 30 {
            1
        } else if overdue_days <= 60 {
            2
        } else if overdue_days <= 90 {
            3
        } else {
            4
        };
        buckets[index].invoice_count += 1;
        buckets[index].balance_cents += invoice.balance_cents();
    }
    buckets
}

fn strip_wikilink_brackets(s: &str) -> String {
    s.trim()
        .trim_start_matches("[[")
        .trim_end_matches("]]")
        .to_string()
}
