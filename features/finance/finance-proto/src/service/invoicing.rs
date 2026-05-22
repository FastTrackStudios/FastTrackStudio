//! Non-CRUD invoicing workflow operations.
//!
//! Invoice / Payment / RecurringSchedule CRUD comes from the
//! architect-emitted `InvoiceRepo` / `PaymentRepo` /
//! `RecurringScheduleRepo` traits.

use uuid::Uuid;

use crate::error::FinanceError;
use crate::invoice::Invoice;

/// One allocation row for [`Invoicing::record_payment`].
#[derive(Debug, Clone, PartialEq, facet::Facet)]
pub struct PaymentAllocation {
    pub invoice_id: Uuid,
    pub amount_minor: i64,
}

/// Inputs for [`Invoicing::record_payment`]. Wraps the
/// `PaymentCreate` shape with the allocation list so the call
/// is one-shot.
#[derive(Debug, Clone, PartialEq, facet::Facet)]
pub struct RecordPayment {
    pub book_id: Uuid,
    pub party_id: Uuid,
    /// ISO-8601 date.
    pub date: String,
    pub amount_minor: i64,
    pub currency: String,
    pub exchange_rate_micro: i64,
    pub method: crate::payment::PaymentMethod,
    pub reference: String,
    pub notes: String,
    pub allocations: Vec<PaymentAllocation>,
}

#[architect::rpc]
pub trait Invoicing {
    /// Issue an invoice: assigns a number from the book's
    /// counter, locks the line items, posts the AR ↔ income
    /// transaction to the ledger.
    fn mark_sent(&self, id: Uuid) -> Result<Invoice, FinanceError>;

    /// Reverse an issued invoice via a credit note. Returns the
    /// new credit's id.
    fn void_with_credit(&self, id: Uuid, reason: String) -> Result<Uuid, FinanceError>;

    /// Record a payment and allocate it across invoices/credits
    /// in one transaction. Sum of allocations must be ≤
    /// `amount_minor`; remainder becomes an open credit on the
    /// party.
    fn record_payment(&self, payload: RecordPayment) -> Result<Uuid, FinanceError>;

    /// Refund (full or partial).
    fn refund_payment(
        &self,
        id: Uuid,
        amount_minor: i64,
        reason: String,
    ) -> Result<(), FinanceError>;

    /// Manually trigger the generator for a single schedule.
    /// Returns the generated invoice's id, or `None` if not due
    /// / paused / completed.
    fn run_schedule_once(&self, id: Uuid) -> Result<Option<Uuid>, FinanceError>;
}
