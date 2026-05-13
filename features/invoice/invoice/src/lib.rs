//! Facade for the `invoice` feature. Wire types are always
//! re-exported; backends + transport adapters are feature-gated.

pub use invoice_proto::*;

/// CRDT source of truth + SeaORM persistence. Construct one
/// `CrdtDoc` per collaboration boundary and hand the `*RepoLoro`
/// newtypes to the vox dispatcher.
#[cfg(feature = "server")]
pub mod server {
    pub use crdt::{CrdtDoc, Persistence};
    pub use invoice_crdt::{
        ClientEntity, ClientRepoLoro, InvoiceEntity, InvoiceLineEntity, InvoiceLineRepoLoro,
        InvoiceRepoLoro, PaymentEntity, PaymentRepoLoro,
    };
    pub use invoice_db::{InvoiceMigrator, SeaOrmPersistence};

    use architect::Page;
    use chrono::{DateTime, Utc};
    use invoice_proto::{
        InvoiceRepo, InvoiceServiceError, InvoiceUpdate, Payment, PaymentCreate, PaymentInput,
        PaymentRepo,
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
    }

    impl InvoiceServiceImpl {
        pub fn new(doc: &CrdtDoc) -> Self {
            Self {
                invoices: InvoiceRepoLoro::new(doc),
                payments: PaymentRepoLoro::new(doc),
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
    }
}

#[cfg(feature = "server-axum")]
pub use architect::axum_ws;
