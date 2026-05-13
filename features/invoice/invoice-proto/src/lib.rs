//! `invoice-proto` — wire contract for the `invoice` feature.
//!
//! Four top-level entities:
//!
//! - `Invoice`     — header (number, client, status, totals, balance)
//! - `InvoiceLine` — line item belonging to an invoice
//! - `Client`      — billable customer (billing address, default rate)
//! - `Payment`     — a payment recorded against an invoice
//!
//! Each is a separate `architect::Entity` with its own Repo trait —
//! the scaffolder treats them uniformly (one LoroMap per entity type,
//! UUID-keyed). Domain operations like marking paid or recording a
//! payment live in `InvoiceService`.
//!
//! ── Tax / total contract ─────────────────────────────────────────────
//!
//! `subtotal     = Σ line.amount_cents`
//! `discounted   = max(0, subtotal - discount_cents)`
//!
//! When `tax_inclusive = false`:
//!   `tax   = discounted * tax_rate_bps / 10_000`
//!   `total = discounted + tax`
//!
//! When `tax_inclusive = true` (subtotal already contains tax):
//!   `tax   = subtotal * tax_rate_bps / (10_000 + tax_rate_bps)`
//!   `total = discounted`
//!
//! `balance       = total - Σ payments.amount_cents` — cached on
//! `Invoice.balance_cents` and recomputed on every payment write.
//!
//! Per-line `tax_rate_bps` override is reserved as a FUTURE field —
//! v1 calc helpers ignore it and use the invoice-level rate.

pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

// ── Status / method constants ────────────────────────────────────────

pub const INVOICE_STATUSES: &[&str] = &[
    "draft",
    "sent",
    "viewed",
    "partial",
    "paid",
    "overdue",
    "cancelled",
];

pub const PAYMENT_METHODS: &[&str] = &["stripe", "cash", "check", "bank-transfer", "other"];

pub const RECURRING_INVOICE_STATUSES: &[&str] = &["active", "paused", "ended"];
pub const RECURRING_INVOICE_FREQUENCIES: &[&str] =
    &["weekly", "biweekly", "monthly", "quarterly", "yearly"];

/// Advance `d` by one recurrence step of `freq`. Unknown frequencies
/// fall through to a monthly step.
pub fn next_date_after(d: DateTime<Utc>, freq: &str) -> DateTime<Utc> {
    use chrono::Duration;
    match freq {
        "weekly" => d + Duration::weeks(1),
        "biweekly" => d + Duration::weeks(2),
        "quarterly" => add_months(d, 3),
        "yearly" => add_months(d, 12),
        _ => add_months(d, 1),
    }
}

fn add_months(d: DateTime<Utc>, months: u32) -> DateTime<Utc> {
    use chrono::{Datelike, TimeZone, Timelike};
    let total = d.year() as i64 * 12 + (d.month() as i64 - 1) + months as i64;
    let year = total.div_euclid(12) as i32;
    let month = (total.rem_euclid(12) + 1) as u32;
    let day = d.day().min(last_day_of_month(year, month));
    Utc.with_ymd_and_hms(year, month, day, d.hour(), d.minute(), d.second())
        .single()
        .unwrap_or(d)
}

fn last_day_of_month(year: i32, month: u32) -> u32 {
    use chrono::{Datelike, NaiveDate};
    let (ny, nm) = if month == 12 {
        (year + 1, 1)
    } else {
        (year, month + 1)
    };
    let first_next = NaiveDate::from_ymd_opt(ny, nm, 1).unwrap();
    let last = first_next.pred_opt().unwrap();
    last.day()
}

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

    /// Flat discount applied before tax (cents). Default 0.
    #[cfg_attr(feature = "fake", dummy(faker = "0i64..10_000"))]
    pub discount_cents: i64,

    /// Invoice-level tax rate in basis points (2500 = 25.00 %).
    #[cfg_attr(feature = "fake", dummy(faker = "0i32..3000"))]
    pub tax_rate_bps: i32,

    /// If true, totals already include tax (tax extracted from
    /// subtotal rather than added to it).
    pub tax_inclusive: bool,

    #[cfg_attr(feature = "fake", dummy(faker = "0i64..200_000"))]
    pub tax_cents: i64,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "5_000i64..2_200_000"))]
    pub total_cents: i64,

    /// `total_cents - Σ payments.amount_cents`. Cached; the
    /// `record_payment` flow recomputes this on every payment write.
    #[architect(sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "0i64..2_200_000"))]
    pub balance_cents: i64,

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

    /// Source project, set when this line was derived from time
    /// entries on a project. None for ad-hoc / standalone lines.
    #[architect(filterable)]
    pub project_id: Option<Uuid>,

    /// First source TimeEntry — used for back-navigation from an
    /// invoice line to "show me the time that generated this".
    #[architect(filterable)]
    pub time_entry_id: Option<Uuid>,

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

    /// Per-line tax rate override (basis points). `None` means
    /// "use the invoice-level rate." FUTURE — not used by v1 calc
    /// helpers; reserved so the field is on the wire for migrations.
    pub tax_rate_bps: Option<i32>,

    #[architect(sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "0i64..20"))]
    pub sort_index: i64,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── Client ────────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "clients", repo)]
pub struct Client {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ClientName"))]
    pub name: String,

    #[architect(filterable)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::internet::en::SafeEmail()")
    )]
    pub email: Option<String>,

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::phone_number::en::PhoneNumber()")
    )]
    pub phone: Option<String>,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::StreetLine"))]
    pub billing_address_line1: Option<String>,

    pub billing_address_line2: Option<String>,

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::address::en::CityName()")
    )]
    pub billing_city: Option<String>,

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::address::en::StateAbbr()")
    )]
    pub billing_region: Option<String>,

    #[cfg_attr(feature = "fake", dummy(faker = "fake::faker::address::en::ZipCode()"))]
    pub billing_postal_code: Option<String>,

    /// ISO 3166-1 alpha-2 (e.g. `US`, `GB`, `DE`).
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::CountryCode"))]
    pub billing_country: Option<String>,

    /// ISO 4217 (default `"USD"`).
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::Currency"))]
    pub currency: String,

    #[architect(sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "5_000i64..40_000"))]
    pub default_rate_cents: Option<i64>,

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

// ── Payment ───────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "payments", repo)]
pub struct Payment {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub invoice_id: Uuid,

    #[architect(sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "1_000i64..500_000"))]
    pub amount_cents: i64,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub paid_at: DateTime<Utc>,

    /// One of [`PAYMENT_METHODS`]: stripe | cash | check | bank-transfer | other.
    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::PaymentMethod"))]
    pub method: String,

    /// Check number, Stripe txn id, etc.
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::PaymentReference"))]
    pub reference: Option<String>,

    #[architect(fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Sentence(3..8)")
    )]
    pub notes: Option<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── RecurringInvoice ──────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "recurring_invoices", repo)]
pub struct RecurringInvoice {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub client_id: Uuid,

    /// "active" | "paused" | "ended" — see [`RECURRING_INVOICE_STATUSES`].
    #[architect(filterable)]
    pub status: String,

    /// "weekly" | "biweekly" | "monthly" | "quarterly" | "yearly" —
    /// see [`RECURRING_INVOICE_FREQUENCIES`].
    #[architect(filterable)]
    pub frequency: String,

    #[architect(sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::FutureDateTime"))]
    pub next_issue_date: DateTime<Utc>,

    /// `None` = forever; otherwise generation stops once
    /// `next_issue_date > end_date`.
    pub end_date: Option<DateTime<Utc>>,

    pub last_generated_at: Option<DateTime<Utc>>,

    /// Number of invoices issued so far from this template.
    pub generated_count: i64,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::Currency"))]
    pub currency: String,

    /// Template totals — copied to each generated invoice.
    #[cfg_attr(feature = "fake", dummy(faker = "5_000i64..2_000_000"))]
    pub subtotal_cents: i64,

    #[cfg_attr(feature = "fake", dummy(faker = "0i32..3000"))]
    pub tax_rate_bps: i32,

    pub tax_inclusive: bool,

    #[cfg_attr(feature = "fake", dummy(faker = "0i64..10_000"))]
    pub discount_cents: i64,

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

// ── RecurringInvoiceLine ──────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "recurring_invoice_lines", repo)]
pub struct RecurringInvoiceLine {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable)]
    pub recurring_invoice_id: Uuid,

    #[architect(filterable)]
    pub project_id: Option<Uuid>,

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

/// Wire-shape input for recording a payment against an invoice.
/// Carried alongside the `Payment` entity because services accept
/// validated input rather than already-stamped DB rows.
#[derive(Debug, Clone, PartialEq, ::facet::Facet)]
pub struct PaymentInput {
    pub amount_cents: i64,
    pub paid_at: DateTime<Utc>,
    pub method: String,
    pub reference: Option<String>,
    pub notes: Option<String>,
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait InvoiceService {
    /// Mark an invoice as paid at the given timestamp.
    async fn mark_paid(
        &self,
        invoice_id: Uuid,
        paid_at: DateTime<Utc>,
    ) -> Result<(), InvoiceServiceError>;

    /// Record a payment against an invoice. Creates a `Payment`,
    /// recomputes `Invoice.balance_cents` (= total - Σ payments),
    /// and patches the invoice row.
    async fn record_payment(
        &self,
        invoice_id: Uuid,
        payment: PaymentInput,
    ) -> Result<Payment, InvoiceServiceError>;

    /// Generate a new `Invoice` (plus lines) from a `RecurringInvoice`
    /// template. Advances `next_issue_date` by one period, increments
    /// `generated_count`, and stamps `last_generated_at`. Returns the
    /// freshly created invoice. Stops (returns `InvalidInput`) when
    /// `next_issue_date > end_date`.
    async fn generate_from_recurring(
        &self,
        recurring_id: Uuid,
    ) -> Result<Invoice, InvoiceServiceError>;
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
            pick(rng, crate::INVOICE_STATUSES)
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

    pub struct ClientName;
    impl Dummy<ClientName> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ClientName, rng: &mut R) -> Self {
            const VALUES: &[&str] = &[
                "Acme Studios",
                "Northwind Records",
                "Pearl & Co.",
                "Cascade Audio",
                "Tidewater Films",
                "Bluebird Labs",
                "Foundry Mastering",
                "Sundial Music Group",
                "Halcyon Post",
                "Riverstone Media",
                "Cobalt Productions",
                "Sable Sound",
                "Wavecrest LLC",
                "Magnolia Pictures",
                "Granite Strategies",
            ];
            pick(rng, VALUES)
        }
    }

    pub struct CountryCode;
    impl Dummy<CountryCode> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &CountryCode, rng: &mut R) -> Self {
            const VALUES: &[&str] = &[
                "US", "US", "US", "GB", "GB", "DE", "FR", "CA", "AU", "NL", "SE", "JP", "ES", "IT",
                "BR",
            ];
            pick(rng, VALUES)
        }
    }

    pub struct StreetLine;
    impl Dummy<StreetLine> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &StreetLine, rng: &mut R) -> Self {
            const NAMES: &[&str] = &[
                "Market St",
                "Pine Ave",
                "Cedar Ln",
                "Maple Rd",
                "Lakeshore Dr",
                "Birch Way",
                "Sunset Blvd",
                "Elm St",
                "Harbor Rd",
                "Riverside Dr",
            ];
            let n: u32 = rng.random_range(10..9999);
            format!("{} {}", n, *NAMES.choose(rng).unwrap())
        }
    }

    pub struct PaymentMethod;
    impl Dummy<PaymentMethod> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &PaymentMethod, rng: &mut R) -> Self {
            pick(rng, crate::PAYMENT_METHODS)
        }
    }

    pub struct PaymentReference;
    impl Dummy<PaymentReference> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &PaymentReference, rng: &mut R) -> Self {
            const PREFIXES: &[&str] = &["ch_", "pi_", "txn_", "chk#", "ref-"];
            let pre = *PREFIXES.choose(rng).unwrap();
            let n: u64 = rng.random_range(100_000..9_999_999);
            format!("{}{}", pre, n)
        }
    }
}
