//! Invoice entity — markdown-backed billing document.
//!
//! Invoices live as `invoices/<id>.md` files in the vault with YAML
//! frontmatter. They roll up billable [`TimeEntry`](crate::task::TimeEntry)
//! rows from a client's tasks into a document the team can preview, print,
//! or email.
//!
//! # Data model
//!
//! - [`Invoice`] holds the header (client, dates, currency, tax, notes),
//!   line items, payments, and the entry ids it bills.
//! - [`InvoiceLine`] is one row on the invoice — task + hours + rate.
//! - [`Payment`] records a received payment; multiple payments against
//!   one invoice are allowed (partial, installments).
//! - [`InvoiceStatus`] is the lifecycle: Draft → Sent → PartiallyPaid →
//!   Paid (or Overdue / Cancelled / Refunded).
//!
//! All money is stored as integer cents (`u32` / `u64`) — no floats in
//! the schema. Rendering converts to dollars for display only.

use chrono::{DateTime, NaiveDate, Utc};
use facet::Facet;

use crate::task::WikiLink;

/// An invoice billed to a client.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Invoice {
    /// Human-readable id, e.g. "INV-2026-0001". Also the file stem under
    /// `invoices/`. Generated at create-time from the year + next_sequence.
    pub id: String,

    /// Monotonic sequence within a company-year scope. Used to build the id
    /// and to order invoices chronologically.
    pub number: u32,

    /// Where the invoice sits in the lifecycle.
    pub status: InvoiceStatus,

    /// Client this invoice bills. Stored as a wikilink so it resolves to
    /// the `clients/<Name>.md` note on display.
    pub client: WikiLink,

    /// Date the invoice was issued (usually the create date).
    pub issue_date: NaiveDate,

    /// Payment due date. Derived from client.payment_terms_days at create
    /// time but stored explicitly so it survives later client edits.
    pub due_date: NaiveDate,

    /// ISO 4217 currency code (e.g. "USD"). Inherited from the client.
    #[facet(default)]
    pub currency_code: String,

    /// Line items. One per (task, rate) pair.
    #[facet(default)]
    pub line_items: Vec<InvoiceLine>,

    /// Invoice-level tax rate as a percentage (e.g. 8.5 for 8.5%). Applied
    /// to the subtotal after discounts. We keep a single slot rather than
    /// Invoice Ninja's three for simplicity — add multi-rate later if
    /// needed.
    pub tax_rate_percent: Option<f64>,

    /// Invoice-level discount as a percentage (0..100). Applied before tax.
    pub discount_percent: Option<f64>,

    /// Optional purchase-order number provided by the client.
    pub po_number: Option<String>,

    /// Notes shown on the invoice body (terms, thanks, payment methods).
    pub public_notes: Option<String>,

    /// Internal notes not shown to the client.
    pub private_notes: Option<String>,

    /// Payments received against this invoice. Running balance =
    /// total_cents() − payments.sum(amount_cents).
    #[facet(default)]
    pub payments: Vec<Payment>,

    /// IDs of the TimeEntries this invoice bills. Used to prevent
    /// double-billing — `VaultServiceImpl::mark_entries_invoiced` flips
    /// `TimeEntry.invoiced_at` on each of these after the invoice is
    /// created.
    #[facet(default)]
    pub entry_ids: Vec<String>,

    /// When the invoice was marked Sent (via `task invoice send`).
    pub sent_at: Option<DateTime<Utc>>,

    /// When the invoice was fully paid.
    pub paid_at: Option<DateTime<Utc>>,

    /// When the invoice was cancelled, if applicable.
    pub cancelled_at: Option<DateTime<Utc>>,

    /// Free-form reason for a cancellation.
    pub cancelled_reason: Option<String>,

    /// Who created the invoice.
    pub created_by: Option<String>,

    pub date_created: Option<DateTime<Utc>>,
    pub date_modified: Option<DateTime<Utc>>,
}

/// One billable line on an invoice. For time-based invoicing,
/// `hours` × `rate_cents` = `subtotal_cents`.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct InvoiceLine {
    /// Stable id for audit trails and edits.
    pub id: String,

    /// Task that generated this line (display-only; the canonical link
    /// is `Invoice.entry_ids`).
    pub task_title: String,

    /// What's being billed. Free-form — defaults to the task title but
    /// the biller can rewrite for a cleaner client-facing description.
    pub description: String,

    /// Hours billed. f64 because partial hours are normal (e.g. 1.25h).
    pub hours: f64,

    /// Hourly rate in cents. Resolved via the cascade at invoice-create
    /// time and frozen here — later edits to Project/Client defaults
    /// don't retroactively change an invoice.
    pub rate_cents: u32,

    /// Per-line tax rate percentage. Falls back to Invoice.tax_rate_percent
    /// when None.
    pub tax_rate_percent: Option<f64>,

    /// Per-line discount percentage (0..100).
    pub discount_percent: Option<f64>,
}

impl InvoiceLine {
    /// Pre-discount, pre-tax amount in cents.
    pub fn subtotal_cents(&self) -> u64 {
        ((self.hours * self.rate_cents as f64).round() as i64).max(0) as u64
    }

    /// Amount after the per-line discount (still pre-tax).
    pub fn net_cents(&self) -> u64 {
        let sub = self.subtotal_cents() as f64;
        let d = self.discount_percent.unwrap_or(0.0).clamp(0.0, 100.0);
        ((sub * (1.0 - d / 100.0)).round() as i64).max(0) as u64
    }
}

/// A payment received against an invoice.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Payment {
    pub id: String,
    /// Cents received.
    pub amount_cents: u64,
    pub received_at: DateTime<Utc>,
    /// Free-form method: "ach", "check", "cash", "stripe", "wire", etc.
    #[facet(default)]
    pub method: String,
    /// Bank reference, check number, transaction id, etc.
    pub reference: Option<String>,
    /// Who recorded the payment.
    pub recorded_by: Option<String>,
    pub notes: Option<String>,
}

/// Invoice lifecycle. `Void` is reserved for a future "issued but
/// rescinded" state; for now cancellation is handled via
/// `Invoice.cancelled_at`.
#[derive(Debug, Clone, PartialEq, Facet)]
#[repr(u8)]
pub enum InvoiceStatus {
    /// Created but not sent.
    Draft,
    /// Sent to the client — awaiting payment.
    Sent,
    /// Some but not all of the balance has been paid.
    PartiallyPaid,
    /// Fully paid.
    Paid,
    /// Past due_date, not paid in full.
    Overdue,
    /// Cancelled / nullified.
    Cancelled,
    /// Paid then refunded.
    Refunded,
}

impl Default for InvoiceStatus {
    fn default() -> Self {
        InvoiceStatus::Draft
    }
}

impl Invoice {
    /// Sum of line net amounts (post per-line discount, pre invoice-level
    /// discount and tax).
    pub fn lines_net_cents(&self) -> u64 {
        self.line_items.iter().map(|l| l.net_cents()).sum()
    }

    /// Amount after the invoice-level discount.
    pub fn discounted_cents(&self) -> u64 {
        let net = self.lines_net_cents() as f64;
        let d = self.discount_percent.unwrap_or(0.0).clamp(0.0, 100.0);
        ((net * (1.0 - d / 100.0)).round() as i64).max(0) as u64
    }

    /// Tax owed in cents. Uses per-line tax rates when a line has one,
    /// otherwise falls back to the invoice-level rate.
    pub fn tax_cents(&self) -> u64 {
        let invoice_rate = self.tax_rate_percent.unwrap_or(0.0).max(0.0);
        let discount = self.discount_percent.unwrap_or(0.0).clamp(0.0, 100.0);
        let discount_factor = 1.0 - discount / 100.0;

        let mut tax = 0.0f64;
        for l in &self.line_items {
            let rate = l.tax_rate_percent.unwrap_or(invoice_rate).max(0.0);
            if rate == 0.0 {
                continue;
            }
            let net = l.net_cents() as f64 * discount_factor;
            tax += net * rate / 100.0;
        }
        tax.round().max(0.0) as u64
    }

    /// Grand total: discounted lines + tax.
    pub fn total_cents(&self) -> u64 {
        self.discounted_cents() + self.tax_cents()
    }

    /// Sum of recorded payments.
    pub fn paid_cents(&self) -> u64 {
        self.payments.iter().map(|p| p.amount_cents).sum()
    }

    /// Remaining balance. Can go negative if overpaid (reported as zero
    /// here; the caller can check `paid_cents > total_cents` separately).
    pub fn balance_cents(&self) -> u64 {
        self.total_cents().saturating_sub(self.paid_cents())
    }

    /// Recompute the status based on balance vs. total and date vs. due.
    /// Does not touch terminal states (Cancelled, Refunded) — those are
    /// explicit transitions only.
    pub fn derive_status(&self, today: NaiveDate) -> InvoiceStatus {
        if matches!(
            self.status,
            InvoiceStatus::Cancelled | InvoiceStatus::Refunded
        ) {
            return self.status.clone();
        }
        let total = self.total_cents();
        let paid = self.paid_cents();
        if total > 0 && paid >= total {
            return InvoiceStatus::Paid;
        }
        if paid > 0 {
            return InvoiceStatus::PartiallyPaid;
        }
        if self.sent_at.is_some() && today > self.due_date {
            return InvoiceStatus::Overdue;
        }
        if self.sent_at.is_some() {
            return InvoiceStatus::Sent;
        }
        InvoiceStatus::Draft
    }
}

/// Format the stable id: "INV-{year}-{number:04}".
pub fn format_invoice_id(year: i32, number: u32) -> String {
    format!("INV-{year:04}-{number:04}")
}

/// Render the markdown body of an invoice file — a human-readable view
/// over the frontmatter totals. This is regenerated whenever the invoice
/// is saved, so keep it deterministic.
pub fn render_invoice_body(inv: &Invoice) -> String {
    use std::fmt::Write;
    let mut out = String::new();

    let _ = writeln!(out, "# Invoice {}", inv.id);
    let _ = writeln!(out);
    let _ = writeln!(out, "**Client:** {}", inv.client.0);
    let _ = writeln!(out, "**Status:** {:?}", inv.status);
    let _ = writeln!(out, "**Issued:** {}", inv.issue_date);
    let _ = writeln!(out, "**Due:** {}", inv.due_date);
    if let Some(po) = &inv.po_number {
        let _ = writeln!(out, "**PO:** {po}");
    }
    if !inv.currency_code.is_empty() {
        let _ = writeln!(out, "**Currency:** {}", inv.currency_code);
    }
    let _ = writeln!(out);

    // Line items table.
    let _ = writeln!(out, "## Line items");
    let _ = writeln!(out);
    let _ = writeln!(out, "| Task | Description | Hours | Rate | Amount |");
    let _ = writeln!(out, "|------|-------------|------:|-----:|-------:|");
    for l in &inv.line_items {
        let _ = writeln!(
            out,
            "| {} | {} | {:.2} | ${:.2} | ${:.2} |",
            l.task_title,
            l.description,
            l.hours,
            l.rate_cents as f64 / 100.0,
            l.net_cents() as f64 / 100.0,
        );
    }
    let _ = writeln!(out);

    // Totals block.
    let _ = writeln!(out, "## Totals");
    let _ = writeln!(out);
    let _ = writeln!(
        out,
        "- Subtotal: ${:.2}",
        inv.lines_net_cents() as f64 / 100.0
    );
    if let Some(d) = inv.discount_percent {
        if d > 0.0 {
            let _ = writeln!(
                out,
                "- Discount ({}%): -${:.2}",
                d,
                (inv.lines_net_cents().saturating_sub(inv.discounted_cents())) as f64 / 100.0
            );
        }
    }
    if inv.tax_cents() > 0 {
        let _ = writeln!(out, "- Tax: ${:.2}", inv.tax_cents() as f64 / 100.0);
    }
    let _ = writeln!(out, "- **Total: ${:.2}**", inv.total_cents() as f64 / 100.0);
    if inv.paid_cents() > 0 {
        let _ = writeln!(out, "- Paid: ${:.2}", inv.paid_cents() as f64 / 100.0);
        let _ = writeln!(
            out,
            "- **Balance: ${:.2}**",
            inv.balance_cents() as f64 / 100.0
        );
    }
    let _ = writeln!(out);

    // Payments table.
    if !inv.payments.is_empty() {
        let _ = writeln!(out, "## Payments");
        let _ = writeln!(out);
        let _ = writeln!(out, "| Date | Amount | Method | Reference |");
        let _ = writeln!(out, "|------|-------:|--------|-----------|");
        for p in &inv.payments {
            let _ = writeln!(
                out,
                "| {} | ${:.2} | {} | {} |",
                p.received_at.format("%Y-%m-%d"),
                p.amount_cents as f64 / 100.0,
                if p.method.is_empty() {
                    "—"
                } else {
                    &p.method
                },
                p.reference.as_deref().unwrap_or("—"),
            );
        }
        let _ = writeln!(out);
    }

    // Notes.
    if let Some(ref n) = inv.public_notes {
        let _ = writeln!(out, "## Notes");
        let _ = writeln!(out);
        let _ = writeln!(out, "{n}");
        let _ = writeln!(out);
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_line(hours: f64, rate: u32) -> InvoiceLine {
        InvoiceLine {
            id: "l1".into(),
            task_title: "Mix".into(),
            description: "Mixing".into(),
            hours,
            rate_cents: rate,
            tax_rate_percent: None,
            discount_percent: None,
        }
    }

    #[test]
    fn line_subtotal_and_net() {
        let l = sample_line(1.5, 12_000);
        assert_eq!(l.subtotal_cents(), 18_000); // 1.5h × $120
        assert_eq!(l.net_cents(), 18_000);

        let mut l = l;
        l.discount_percent = Some(10.0);
        assert_eq!(l.net_cents(), 16_200);
    }

    #[test]
    fn invoice_total_with_tax_and_discount() {
        let inv = Invoice {
            id: "INV-2026-0001".into(),
            number: 1,
            client: WikiLink("Acme".into()),
            issue_date: chrono::NaiveDate::from_ymd_opt(2026, 4, 17).unwrap(),
            due_date: chrono::NaiveDate::from_ymd_opt(2026, 5, 17).unwrap(),
            line_items: vec![sample_line(2.0, 10_000), sample_line(1.0, 20_000)],
            tax_rate_percent: Some(10.0),
            discount_percent: Some(5.0),
            ..Default::default()
        };
        // lines: 2*100 + 1*200 = $400
        assert_eq!(inv.lines_net_cents(), 40_000);
        // 5% discount → $380
        assert_eq!(inv.discounted_cents(), 38_000);
        // 10% tax on discounted net → $38
        assert_eq!(inv.tax_cents(), 3_800);
        // Grand total: $418
        assert_eq!(inv.total_cents(), 41_800);
    }

    #[test]
    fn per_line_tax_overrides_invoice_level() {
        let mut line_gst = sample_line(1.0, 10_000);
        line_gst.tax_rate_percent = Some(5.0);
        let inv = Invoice {
            tax_rate_percent: Some(10.0),
            line_items: vec![line_gst, sample_line(1.0, 10_000)],
            ..Default::default()
        };
        // line 1: $100 × 5% = $5
        // line 2: $100 × 10% = $10
        assert_eq!(inv.tax_cents(), 1_500);
    }

    #[test]
    fn balance_reflects_payments() {
        let inv = Invoice {
            line_items: vec![sample_line(1.0, 10_000)],
            payments: vec![Payment {
                id: "p1".into(),
                amount_cents: 6_000,
                received_at: Utc::now(),
                ..Default::default()
            }],
            ..Default::default()
        };
        assert_eq!(inv.total_cents(), 10_000);
        assert_eq!(inv.paid_cents(), 6_000);
        assert_eq!(inv.balance_cents(), 4_000);
    }

    #[test]
    fn derive_status_lifecycle() {
        let today = chrono::NaiveDate::from_ymd_opt(2026, 6, 1).unwrap();

        // Draft: not sent, no payments.
        let inv = Invoice {
            due_date: chrono::NaiveDate::from_ymd_opt(2026, 5, 17).unwrap(),
            line_items: vec![sample_line(1.0, 10_000)],
            ..Default::default()
        };
        assert_eq!(inv.derive_status(today), InvoiceStatus::Draft);

        // Sent: no payments, not yet overdue.
        let future = chrono::NaiveDate::from_ymd_opt(2026, 7, 1).unwrap();
        let mut inv_sent = inv.clone();
        inv_sent.due_date = future;
        inv_sent.sent_at = Some(Utc::now());
        assert_eq!(inv_sent.derive_status(today), InvoiceStatus::Sent);

        // Overdue: sent, past due, no payment.
        let mut inv_overdue = inv.clone();
        inv_overdue.sent_at = Some(Utc::now());
        assert_eq!(inv_overdue.derive_status(today), InvoiceStatus::Overdue);

        // PartiallyPaid: some payment recorded.
        let mut inv_partial = inv.clone();
        inv_partial.sent_at = Some(Utc::now());
        inv_partial.payments.push(Payment {
            id: "p".into(),
            amount_cents: 4_000,
            received_at: Utc::now(),
            ..Default::default()
        });
        assert_eq!(
            inv_partial.derive_status(today),
            InvoiceStatus::PartiallyPaid
        );

        // Paid: full payment.
        let mut inv_paid = inv.clone();
        inv_paid.payments.push(Payment {
            id: "p".into(),
            amount_cents: 10_000,
            received_at: Utc::now(),
            ..Default::default()
        });
        assert_eq!(inv_paid.derive_status(today), InvoiceStatus::Paid);

        // Cancelled sticks.
        let mut inv_cancelled = inv.clone();
        inv_cancelled.status = InvoiceStatus::Cancelled;
        assert_eq!(inv_cancelled.derive_status(today), InvoiceStatus::Cancelled);
    }
}
