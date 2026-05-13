//! `invoice-proto` — wire contract for the `invoice` feature.
//!
//! Two top-level entities:
//!
//! - `Invoice`     — header (number, client, status, totals)
//! - `InvoiceLine` — line item belonging to an invoice
//!
//! Each is a separate `architect::Entity` with its own Repo trait —
//! the scaffolder treats them uniformly (one LoroMap per entity type,
//! UUID-keyed). Domain operations like marking paid live in
//! `InvoiceService`.

pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

// ── Invoice ───────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "invoices", repo)]
pub struct Invoice {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    pub number: String,

    #[architect(filterable)]
    pub client_id: Uuid,

    #[architect(filterable, sortable)]
    pub status: String,

    #[architect(filterable, sortable)]
    pub issue_date: DateTime<Utc>,

    #[architect(filterable, sortable)]
    pub due_date: Option<DateTime<Utc>>,

    #[architect(filterable, sortable)]
    pub paid_at: Option<DateTime<Utc>>,

    #[architect(filterable)]
    pub currency: String,

    #[architect(filterable, sortable)]
    pub subtotal_cents: i64,

    pub tax_cents: i64,

    #[architect(filterable, sortable)]
    pub total_cents: i64,

    #[architect(fulltext)]
    pub notes: Option<String>,

    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── InvoiceLine ───────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "invoice_lines", repo)]
pub struct InvoiceLine {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable)]
    pub invoice_id: Uuid,

    #[architect(fulltext)]
    pub description: String,

    pub quantity_thousandths: i64,

    pub unit_price_cents: i64,

    #[architect(sortable)]
    pub amount_cents: i64,

    #[architect(sortable)]
    pub sort_index: i64,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── InvoiceService ────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum InvoiceServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait InvoiceService {
    /// Mark an invoice as paid at the given timestamp.
    async fn mark_paid(
        &self,
        invoice_id: Uuid,
        paid_at: DateTime<Utc>,
    ) -> Result<(), InvoiceServiceError>;
}
