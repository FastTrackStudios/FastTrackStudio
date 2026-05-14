//! `finance-proto` — wire contract for the `finance` feature.
//!
//! Three top-level entities:
//!
//! - `Revenue`         — money in (from client work, royalties, licensing)
//! - `Expense`         — money out (rent, gear, subscriptions, travel)
//! - `FinancialAsset`  — capital holdings (real estate, stocks, crypto, gear)
//!
//! Each is a separate `architect::Entity` with its own Repo trait.
//! Domain operations like recording a payment against an invoice
//! live in `FinanceService`. `FinancialAsset` reuses `FinanceServiceError`
//! for v1 — no dedicated service is defined since plain repo CRUD covers it.

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
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::company::en::CompanyName()")
    )]
    pub source: String,

    #[architect(filterable)]
    pub client_id: Option<Uuid>,

    #[architect(filterable)]
    pub invoice_id: Option<Uuid>,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "50_000i64..5_000_000"))]
    pub amount_cents: i64,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::Currency"))]
    pub currency: String,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub received_at: DateTime<Utc>,

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Sentence(3..10)")
    )]
    pub notes: Option<String>,

    #[architect(json)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::FinanceTags"))]
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
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ExpenseCategory"))]
    pub category: String,

    #[architect(filterable)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::company::en::CompanyName()")
    )]
    pub vendor: Option<String>,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "500i64..150_000"))]
    pub amount_cents: i64,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::Currency"))]
    pub currency: String,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub spent_at: DateTime<Utc>,

    #[architect(filterable)]
    pub project_id: Option<Uuid>,

    #[architect(filterable)]
    pub tax_deductible: bool,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ReceiptUrl"))]
    pub receipt_url: Option<String>,

    #[architect(fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Sentence(3..10)")
    )]
    pub notes: Option<String>,

    #[architect(json)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::FinanceTags"))]
    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── FinancialAsset ────────────────────────────────────────────────────

/// Canonical kinds for `FinancialAsset.kind`. Anything outside this list
/// still round-trips through the CRDT — the constant just drives the UI
/// combobox + faker.
pub const FINANCIAL_ASSET_KINDS: &[&str] = &[
    "real-estate",
    "stock",
    "bond",
    "crypto",
    "vehicle",
    "equipment",
    "cash",
    "other",
];

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "financial_assets", repo)]
pub struct FinancialAsset {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::AssetName"))]
    pub name: String,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::AssetKind"))]
    pub kind: String,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::Ticker"))]
    pub symbol: Option<String>,

    #[architect(sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::PriceCents"))]
    pub purchase_price_cents: Option<i64>,

    #[architect(sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::CurrentValueCents"))]
    pub current_value_cents: Option<i64>,

    /// Divisible-asset quantity, in thousandths (so 1.500 shares == 1500).
    /// Mirrors inventory's int-with-fixed-scale convention.
    #[cfg_attr(feature = "fake", dummy(faker = "1_000i64..50_000_000"))]
    pub quantity_thousandths: Option<i64>,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::Currency"))]
    pub currency: String,

    #[architect(sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub purchase_date: Option<DateTime<Utc>>,

    /// `None` = still held. `Some(_)` = realised position; UI strikes
    /// through the card and badges it Neutral.
    pub sold_date: Option<DateTime<Utc>>,

    #[architect(sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::MonthlyIncomeCents"))]
    pub monthly_income_cents: Option<i64>,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::Account"))]
    pub account: Option<String>,

    /// Free-form owner bucket for v1. Could become a `Person` FK later.
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::Owner"))]
    pub owner: Option<String>,

    #[architect(fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Sentence(3..10)")
    )]
    pub notes: Option<String>,

    #[architect(json)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::AssetTags"))]
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

#[cfg(feature = "fake")]
pub mod fakers {
    use chrono::{DateTime, Duration, Utc};
    use fake::Dummy;
    use fake::rand::{Rng, seq::IndexedRandom};

    fn pick<R: Rng + ?Sized>(rng: &mut R, values: &[&str]) -> String {
        (*values.choose(rng).unwrap()).to_string()
    }

    pub struct Currency;
    impl Dummy<Currency> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &Currency, rng: &mut R) -> Self {
            const VALUES: &[&str] = &[
                "USD", "USD", "USD", "USD", "EUR", "EUR", "EUR", "GBP", "GBP", "CAD", "AUD", "JPY",
            ];
            pick(rng, VALUES)
        }
    }

    pub struct ExpenseCategory;
    impl Dummy<ExpenseCategory> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ExpenseCategory, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "software",
                    "hardware",
                    "travel",
                    "meals",
                    "rent",
                    "utilities",
                    "supplies",
                    "marketing",
                    "education",
                    "subscriptions",
                    "professional-services",
                    "office",
                ],
            )
        }
    }

    pub struct RecentDateTime;
    impl Dummy<RecentDateTime> for DateTime<Utc> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &RecentDateTime, rng: &mut R) -> Self {
            Utc::now() - Duration::days(rng.random_range(0..365))
        }
    }

    pub struct ReceiptUrl;
    impl Dummy<ReceiptUrl> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ReceiptUrl, rng: &mut R) -> Self {
            const HOSTS: &[&str] = &[
                "receipts.expensify.com",
                "files.dropbox.com",
                "drive.google.com",
                "storage.example.com",
            ];
            let host = HOSTS.choose(rng).unwrap();
            let id: u64 = rng.random_range(100_000..9_999_999);
            format!("https://{}/receipts/{}.pdf", host, id)
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
                "reimbursable",
                "Q1",
                "Q2",
                "Q3",
                "Q4",
            ];
            let n = rng.random_range(1..=4usize);
            let chosen: Vec<&&str> = POOL.choose_multiple(rng, n).collect();
            chosen.into_iter().map(|s| s.to_string()).collect()
        }
    }

    // ── FinancialAsset fakers ────────────────────────────────────────

    pub struct AssetName;
    impl Dummy<AssetName> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &AssetName, rng: &mut R) -> Self {
            const VALUES: &[&str] = &[
                "Maple Ave Property",
                "Downtown Studio Loft",
                "Lakeside Cabin",
                "VTI Holdings",
                "VXUS Holdings",
                "Apple Inc",
                "NVIDIA Inc",
                "Tesla Inc",
                "Treasury Bond Ladder",
                "BTC Cold Storage",
                "ETH Staking Wallet",
                "Studio A Mixer",
                "Hasselblad H6D",
                "Tesla Model Y",
                "Sprinter Van",
                "Emergency Fund",
                "Roth IRA Bucket",
                "Series I Bonds",
            ];
            pick(rng, VALUES)
        }
    }

    pub struct AssetKind;
    impl Dummy<AssetKind> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &AssetKind, rng: &mut R) -> Self {
            pick(rng, super::FINANCIAL_ASSET_KINDS)
        }
    }

    pub struct Ticker;
    impl Dummy<Ticker> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &Ticker, rng: &mut R) -> Self {
            const VALUES: &[&str] = &[
                "VTI", "VXUS", "AAPL", "NVDA", "TSLA", "MSFT", "AMZN", "GOOG", "BTC", "ETH", "SOL",
                "LTC",
            ];
            pick(rng, VALUES)
        }
    }

    /// Cents in the 1k–2M range — covers gear up through small properties.
    pub struct PriceCents;
    impl Dummy<PriceCents> for i64 {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &PriceCents, rng: &mut R) -> Self {
            rng.random_range(100_000i64..200_000_000)
        }
    }

    /// Slightly wider band than `PriceCents` so deltas land both sides of zero.
    pub struct CurrentValueCents;
    impl Dummy<CurrentValueCents> for i64 {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &CurrentValueCents, rng: &mut R) -> Self {
            rng.random_range(80_000i64..250_000_000)
        }
    }

    pub struct MonthlyIncomeCents;
    impl Dummy<MonthlyIncomeCents> for i64 {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &MonthlyIncomeCents, rng: &mut R) -> Self {
            rng.random_range(0i64..500_000)
        }
    }

    pub struct Account;
    impl Dummy<Account> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &Account, rng: &mut R) -> Self {
            const VALUES: &[&str] = &[
                "Schwab Brokerage",
                "Fidelity IRA",
                "Vanguard Roth",
                "Coinbase",
                "Ledger Cold",
                "Studio LLC",
                "Personal Checking",
                "Property Holdings LLC",
            ];
            pick(rng, VALUES)
        }
    }

    pub struct Owner;
    impl Dummy<Owner> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &Owner, rng: &mut R) -> Self {
            const VALUES: &[&str] = &["self", "joint", "Studio LLC", "Trust"];
            pick(rng, VALUES)
        }
    }

    pub struct AssetTags;
    impl Dummy<AssetTags> for Vec<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &AssetTags, rng: &mut R) -> Self {
            const POOL: &[&str] = &[
                "long-term",
                "short-term",
                "income",
                "growth",
                "speculative",
                "tax-advantaged",
                "illiquid",
                "core",
            ];
            let n = rng.random_range(1..=3usize);
            let chosen: Vec<&&str> = POOL.choose_multiple(rng, n).collect();
            chosen.into_iter().map(|s| s.to_string()).collect()
        }
    }
}
