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
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::InvoiceNumber"))]
    pub number: String,

    #[architect(filterable)]
    pub client_id: Uuid,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::InvoiceStatus"))]
    pub status: String,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub issue_date: DateTime<Utc>,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::FutureDateTime"))]
    pub due_date: Option<DateTime<Utc>>,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub paid_at: Option<DateTime<Utc>>,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::Currency"))]
    pub currency: String,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "5_000i64..2_000_000"))]
    pub subtotal_cents: i64,

    #[cfg_attr(feature = "fake", dummy(faker = "0i64..200_000"))]
    pub tax_cents: i64,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "5_000i64..2_200_000"))]
    pub total_cents: i64,

    #[architect(fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Sentence(3..10)")
    )]
    pub notes: Option<String>,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::FinanceTags"))]
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
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::LineDescription"))]
    pub description: String,

    #[cfg_attr(feature = "fake", dummy(faker = "1_000i64..50_000"))]
    pub quantity_thousandths: i64,

    #[cfg_attr(feature = "fake", dummy(faker = "1_000i64..50_000"))]
    pub unit_price_cents: i64,

    #[architect(sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "1_000i64..200_000"))]
    pub amount_cents: i64,

    #[architect(sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "0i64..20"))]
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

// ── Fake-data fakers ──────────────────────────────────────────────────
//
// Domain-tightened fakers. Keeps seeded data looking like real invoice
// records (status enums, USD/EUR currencies, `INV-2024-0042` numbers,
// recent timestamps, business-scale dollar amounts) instead of fake's
// default lorem-ipsum / full-ISO / ±9999-year output.

#[cfg(feature = "fake")]
pub mod fakers {
    use chrono::{DateTime, Duration, Utc};
    use fake::Dummy;
    use fake::rand::{Rng, seq::IndexedRandom};

    fn pick<R: Rng + ?Sized>(rng: &mut R, values: &[&str]) -> String {
        (*values.choose(rng).unwrap()).to_string()
    }

    pub struct InvoiceStatus;
    impl Dummy<InvoiceStatus> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &InvoiceStatus, rng: &mut R) -> Self {
            pick(
                rng,
                &["draft", "sent", "paid", "overdue", "void", "partial"],
            )
        }
    }

    pub struct Currency;
    impl Dummy<Currency> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &Currency, rng: &mut R) -> Self {
            // Weighted toward USD/EUR
            const VALUES: &[&str] = &[
                "USD", "USD", "USD", "USD", "EUR", "EUR", "EUR", "GBP", "GBP", "CAD", "AUD", "JPY",
            ];
            pick(rng, VALUES)
        }
    }

    pub struct InvoiceNumber;
    impl Dummy<InvoiceNumber> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &InvoiceNumber, rng: &mut R) -> Self {
            let year = rng.random_range(2023..=2026);
            let n: u32 = rng.random_range(1..9999);
            format!("INV-{}-{:04}", year, n)
        }
    }

    pub struct LineDescription;
    impl Dummy<LineDescription> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &LineDescription, rng: &mut R) -> Self {
            const VALUES: &[&str] = &[
                "Consulting services",
                "Mixing & mastering session",
                "Website redesign — phase 1",
                "Sound design package",
                "Hourly engineering time",
                "Monthly retainer",
                "Plugin development",
                "Beat license — exclusive",
                "Studio rental — full day",
                "Audio post-production",
                "Album mastering",
                "Sync license fee",
            ];
            pick(rng, VALUES)
        }
    }

    pub struct RecentDateTime;
    impl Dummy<RecentDateTime> for DateTime<Utc> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &RecentDateTime, rng: &mut R) -> Self {
            Utc::now() - Duration::days(rng.random_range(0..365))
        }
    }

    pub struct FutureDateTime;
    impl Dummy<FutureDateTime> for DateTime<Utc> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &FutureDateTime, rng: &mut R) -> Self {
            Utc::now() + Duration::days(rng.random_range(0..60))
        }
    }

    pub struct FinanceTags;
    impl Dummy<FinanceTags> for Vec<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &FinanceTags, rng: &mut R) -> Self {
            const POOL: &[&str] = &[
                "client",
                "internal",
                "recurring",
                "one-off",
                "travel",
                "software",
                "hardware",
                "Q1",
                "Q2",
                "Q3",
                "Q4",
                "rush",
            ];
            let n = rng.random_range(1..=4usize);
            let mut chosen: Vec<&&str> = POOL.choose_multiple(rng, n).collect();
            chosen.sort();
            chosen.dedup();
            chosen.into_iter().map(|s| s.to_string()).collect()
        }
    }
}
