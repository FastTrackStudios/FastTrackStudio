//! `finance-proto` — wire contract for the `finance` feature.
//!
//! Two top-level entities:
//!
//! - `Revenue` — money in (from client work, royalties, licensing)
//! - `Expense` — money out (rent, gear, subscriptions, travel)
//!
//! Each is a separate `architect::Entity` with its own Repo trait.
//! Domain operations like recording a payment against an invoice
//! live in `FinanceService`.

pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

// ── Revenue ───────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "revenues", repo)]
pub struct Revenue {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, fulltext)]
    pub source: String,

    #[architect(filterable)]
    pub client_id: Option<Uuid>,

    #[architect(filterable)]
    pub invoice_id: Option<Uuid>,

    #[architect(filterable, sortable)]
    pub amount_cents: i64,

    #[architect(filterable)]
    pub currency: String,

    #[architect(filterable, sortable)]
    pub received_at: DateTime<Utc>,

    pub notes: Option<String>,

    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── Expense ───────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "expenses", repo)]
pub struct Expense {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable)]
    pub category: String,

    #[architect(filterable)]
    pub vendor: Option<String>,

    #[architect(filterable, sortable)]
    pub amount_cents: i64,

    #[architect(filterable)]
    pub currency: String,

    #[architect(filterable, sortable)]
    pub spent_at: DateTime<Utc>,

    #[architect(filterable)]
    pub project_id: Option<Uuid>,

    #[architect(filterable)]
    pub tax_deductible: bool,

    pub receipt_url: Option<String>,

    #[architect(fulltext)]
    pub notes: Option<String>,

    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── FinanceService ────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum FinanceServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait FinanceService {
    /// Record a payment for the given invoice, in cents.
    async fn record_payment(
        &self,
        invoice_id: Uuid,
        amount_cents: i64,
    ) -> Result<(), FinanceServiceError>;
}
