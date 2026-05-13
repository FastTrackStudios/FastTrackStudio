//! Facade for the `invoice` feature. Wire types are always
//! re-exported; backends + transport adapters are feature-gated.

pub use invoice_proto::*;

/// Native PDF rendering for an invoice. Gated behind the `pdf` cargo
/// feature *and* `cfg(not(target_arch = "wasm32"))` — printpdf's
/// `time`-crate transitive dep doesn't compile for wasm32, so web
/// builds use a `window.print()` fallback off `InvoicePreview`.
#[cfg(all(feature = "pdf", not(target_arch = "wasm32")))]
pub mod pdf;

/// CRDT source of truth + SeaORM persistence. Construct one
/// `CrdtDoc` per collaboration boundary and hand the `*RepoLoro`
/// newtypes to the vox dispatcher.
#[cfg(feature = "server")]
pub mod server {
    pub use crdt::{CrdtDoc, Persistence};
    pub use invoice_crdt::{
        ClientEntity, ClientRepoLoro, InvoiceEntity, InvoiceLineEntity, InvoiceLineRepoLoro,
        InvoiceRepoLoro, PaymentEntity, PaymentRepoLoro, RecurringInvoiceEntity,
        RecurringInvoiceLineEntity, RecurringInvoiceLineRepoLoro, RecurringInvoiceRepoLoro,
    };
    pub use invoice_db::{InvoiceMigrator, SeaOrmPersistence};

    use architect::Page;
    use chrono::{DateTime, Utc};
    use invoice_proto::{
        Invoice, InvoiceCreate, InvoiceLineCreate, InvoiceLineRepo, InvoiceRepo,
        InvoiceServiceError, InvoiceUpdate, Payment, PaymentCreate, PaymentInput, PaymentRepo,
        RecurringInvoiceLineRepo, RecurringInvoiceRepo, RecurringInvoiceUpdate, next_date_after,
    };
    use uuid::Uuid;

    /// CRDT-backed implementation of [`InvoiceService`].
    ///
    /// Lives in the parent crate (rather than `invoice-crdt`) so the
    /// `InvoiceService` trait can stay wire-only inside `invoice-proto`
    /// — the impl needs access to *three* repos (Invoice, Payment,
    /// InvoiceLine), which would force `invoice-crdt` to depend on
    /// itself for the trait if implemented there.
    #[derive(Clone)]
    pub struct InvoiceServiceImpl {
        pub invoices: InvoiceRepoLoro,
        pub payments: PaymentRepoLoro,
        pub invoice_lines: InvoiceLineRepoLoro,
        pub recurring: RecurringInvoiceRepoLoro,
        pub recurring_lines: RecurringInvoiceLineRepoLoro,
    }

    impl InvoiceServiceImpl {
        pub fn new(doc: &CrdtDoc) -> Self {
            Self {
                invoices: InvoiceRepoLoro::new(doc),
                payments: PaymentRepoLoro::new(doc),
                invoice_lines: InvoiceLineRepoLoro::new(doc),
                recurring: RecurringInvoiceRepoLoro::new(doc),
                recurring_lines: RecurringInvoiceLineRepoLoro::new(doc),
            }
        }

        /// Mark `invoice_id` as paid at `paid_at`. Sets `status =
        /// "paid"` and stamps `paid_at`. Does not touch `balance_cents`
        /// — callers should record an explicit `Payment` via
        /// `record_payment` to keep the balance accurate.
        pub async fn mark_paid(
            &self,
            invoice_id: Uuid,
            paid_at: DateTime<Utc>,
        ) -> Result<(), InvoiceServiceError> {
            // Verify the invoice exists; surface NotFound cleanly.
            self.invoices
                .get(invoice_id)
                .await
                .map_err(|_| InvoiceServiceError::NotFound)?;
            self.invoices
                .update(
                    invoice_id,
                    InvoiceUpdate {
                        status: Some("paid".into()),
                        paid_at: Some(Some(paid_at)),
                        ..Default::default()
                    },
                )
                .await
                .map_err(|e| InvoiceServiceError::Internal(e.to_string()))?;
            Ok(())
        }

        /// Create a `Payment` against `invoice_id`, then recompute
        /// `Invoice.balance_cents = total_cents - Σ payments` and
        /// patch the invoice. Returns the created payment so callers
        /// can render a receipt id / reference back to the user.
        pub async fn record_payment(
            &self,
            invoice_id: Uuid,
            input: PaymentInput,
        ) -> Result<Payment, InvoiceServiceError> {
            // 1. Confirm the invoice exists and grab the total.
            let invoice = self
                .invoices
                .get(invoice_id)
                .await
                .map_err(|_| InvoiceServiceError::NotFound)?;

            // 2. Write the new Payment row.
            let payment = self
                .payments
                .create(PaymentCreate {
                    invoice_id,
                    amount_cents: input.amount_cents,
                    paid_at: input.paid_at,
                    method: input.method,
                    reference: input.reference,
                    notes: input.notes,
                })
                .await
                .map_err(|e| InvoiceServiceError::Internal(e.to_string()))?;

            // 3. Sum all payments for this invoice (Loro list filter)
            //    and recompute the cached balance. Demo data only —
            //    we walk the full list rather than maintaining an
            //    incremental aggregate.
            let all = self
                .payments
                .list(
                    Page {
                        index: 0,
                        size: 10_000,
                    },
                    None,
                    None,
                )
                .await
                .map_err(|e| InvoiceServiceError::Internal(e.to_string()))?;
            let paid_sum: i64 = all
                .items
                .iter()
                .filter(|p| p.invoice_id == invoice_id)
                .map(|p| p.amount_cents)
                .sum();
            let new_balance = invoice.total_cents - paid_sum;

            self.invoices
                .update(
                    invoice_id,
                    InvoiceUpdate {
                        balance_cents: Some(new_balance),
                        ..Default::default()
                    },
                )
                .await
                .map_err(|e| InvoiceServiceError::Internal(e.to_string()))?;

            Ok(payment)
        }

        /// Generate an `Invoice` (plus copied lines) from a
        /// `RecurringInvoice` template. Advances the template's
        /// `next_issue_date` by one period, increments
        /// `generated_count`, and stamps `last_generated_at`.
        pub async fn generate_from_recurring(
            &self,
            recurring_id: Uuid,
        ) -> Result<Invoice, InvoiceServiceError> {
            let tmpl = self
                .recurring
                .get(recurring_id)
                .await
                .map_err(|_| InvoiceServiceError::NotFound)?;

            // Stop once we've passed the configured end date.
            if let Some(end) = tmpl.end_date {
                if tmpl.next_issue_date > end {
                    return Err(InvoiceServiceError::InvalidInput(
                        "recurring template has ended".into(),
                    ));
                }
            }

            // Pull lines for the template.
            let lines = self
                .recurring_lines
                .list(
                    Page {
                        index: 0,
                        size: 10_000,
                    },
                    None,
                    None,
                )
                .await
                .map_err(|e| InvoiceServiceError::Internal(e.to_string()))?;
            let mut tmpl_lines: Vec<_> = lines
                .items
                .into_iter()
                .filter(|l| l.recurring_invoice_id == recurring_id)
                .collect();
            tmpl_lines.sort_by_key(|l| l.sort_index);

            // Compute subtotal / tax / total from lines so the
            // generated invoice has accurate header numbers.
            let subtotal: i64 = tmpl_lines.iter().map(|l| l.amount_cents).sum();
            let discounted = (subtotal - tmpl.discount_cents).max(0);
            let (tax, total) = if tmpl.tax_inclusive {
                let tax =
                    discounted * tmpl.tax_rate_bps as i64 / (10_000 + tmpl.tax_rate_bps as i64);
                (tax, discounted)
            } else {
                let tax = discounted * tmpl.tax_rate_bps as i64 / 10_000;
                (tax, discounted + tax)
            };

            let issue = tmpl.next_issue_date;
            let number = format!(
                "INV-R{}-{}",
                tmpl.generated_count + 1,
                issue.format("%Y%m%d")
            );

            let invoice = self
                .invoices
                .create(InvoiceCreate {
                    number,
                    client_id: tmpl.client_id,
                    status: "draft".into(),
                    issue_date: issue,
                    due_date: Some(issue + chrono::Duration::days(30)),
                    paid_at: None,
                    currency: tmpl.currency.clone(),
                    subtotal_cents: subtotal,
                    discount_cents: tmpl.discount_cents,
                    tax_rate_bps: tmpl.tax_rate_bps,
                    tax_inclusive: tmpl.tax_inclusive,
                    tax_cents: tax,
                    total_cents: total,
                    balance_cents: total,
                    notes: tmpl.notes.clone(),
                    tags: {
                        let mut t = tmpl.tags.clone();
                        t.push("from-recurring".into());
                        t
                    },
                })
                .await
                .map_err(|e| InvoiceServiceError::Internal(e.to_string()))?;

            for l in &tmpl_lines {
                let _ = self
                    .invoice_lines
                    .create(InvoiceLineCreate {
                        invoice_id: invoice.id,
                        project_id: l.project_id,
                        time_entry_id: None,
                        description: l.description.clone(),
                        quantity_thousandths: l.quantity_thousandths,
                        unit_price_cents: l.unit_price_cents,
                        amount_cents: l.amount_cents,
                        tax_rate_bps: None,
                        sort_index: l.sort_index,
                    })
                    .await;
            }

            // Advance the template.
            let next = next_date_after(issue, &tmpl.frequency);
            let now = Utc::now();
            self.recurring
                .update(
                    recurring_id,
                    RecurringInvoiceUpdate {
                        next_issue_date: Some(next),
                        last_generated_at: Some(Some(now)),
                        generated_count: Some(tmpl.generated_count + 1),
                        ..Default::default()
                    },
                )
                .await
                .map_err(|e| InvoiceServiceError::Internal(e.to_string()))?;

            Ok(invoice)
        }
    }
}

#[cfg(feature = "server-axum")]
pub use architect::axum_ws;
