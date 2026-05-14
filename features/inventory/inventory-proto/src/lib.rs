//! `inventory-proto` — wire contract for the `inventory` feature.
//!
//! Gear / hardware / software cataloging system modeled on Snipe-IT,
//! Homebox, and InvenTree. Pantry/food types moved to the `cookbook`
//! feature in a prior pass.
//!
//! Two top-level entities:
//!
//! - `InventoryItem`  — a piece of gear / hardware / software license
//! - `CheckoutEvent`  — a record of an item being checked out / in

pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

// ── Status / Category enum-ish constants ─────────────────────────────

pub const INVENTORY_STATUSES: &[&str] = &[
    "active",
    "in-repair",
    "retired",
    "sold",
    "lost",
    "checked-out",
];

pub const INVENTORY_CATEGORIES: &[&str] = &[
    "audio-interface",
    "microphone",
    "outboard",
    "computer",
    "instrument",
    "cable",
    "software-license",
    "vehicle",
    "other",
];

// ── InventoryItem ────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "inventory_items", repo)]
pub struct InventoryItem {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::InventoryName"))]
    pub name: String,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::Category"))]
    pub category: Option<String>,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::Status"))]
    pub status: String,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::Manufacturer"))]
    pub manufacturer: Option<String>,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ModelNumber"))]
    pub model: Option<String>,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::SerialNumber"))]
    pub serial_number: Option<String>,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::QrCode"))]
    pub qr_code: Option<String>,

    pub owner_id: Option<Uuid>,

    pub location_id: Option<Uuid>,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::SubLocation"))]
    pub sub_location: Option<String>,

    #[architect(sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "5_000i64..500_000"))]
    pub value_cents: Option<i64>,

    #[architect(sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::WarrantyDate"))]
    pub warranty_until: Option<DateTime<Utc>>,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::Vendor"))]
    pub vendor: Option<String>,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::PurchaseOrder"))]
    pub purchase_order: Option<String>,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::PastDateTimeOpt"))]
    pub acquired_at: Option<DateTime<Utc>>,

    pub photo_url: Option<String>,

    #[architect(fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Sentence(3..10)")
    )]
    pub notes: Option<String>,

    #[architect(json)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::InventoryTags"))]
    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── CheckoutEvent ────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "checkout_events", repo)]
pub struct CheckoutEvent {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub item_id: Uuid,

    pub person_id: Option<Uuid>,

    #[architect(sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::PastDateTime"))]
    pub checked_out_at: DateTime<Utc>,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::FutureDateTime"))]
    pub due_at: Option<DateTime<Utc>>,

    pub returned_at: Option<DateTime<Utc>>,

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Sentence(3..8)")
    )]
    pub note: Option<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── InventoryService ─────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum InventoryServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait InventoryService {
    /// Check an item out to a person. Creates a `CheckoutEvent` and
    /// updates the item's `status` to `"checked-out"` and `owner_id`.
    async fn checkout(
        &self,
        item_id: Uuid,
        person_id: Uuid,
        due_at: Option<DateTime<Utc>>,
        note: Option<String>,
    ) -> Result<CheckoutEvent, InventoryServiceError>;

    /// Check an item back in. Closes the most recent open `CheckoutEvent`
    /// for the item by setting `returned_at`, and flips the item's
    /// status back to `"active"`.
    async fn checkin(
        &self,
        item_id: Uuid,
        note: Option<String>,
    ) -> Result<CheckoutEvent, InventoryServiceError>;
}

// ── Fakers ───────────────────────────────────────────────────────────

#[cfg(feature = "fake")]
pub mod fakers {
    use chrono::{DateTime, Duration, Utc};
    use fake::Dummy;
    use fake::rand::Rng;
    use fake::rand::seq::IndexedRandom;

    fn pick<R: Rng + ?Sized>(rng: &mut R, values: &[&str]) -> String {
        (*values.choose(rng).unwrap()).to_string()
    }

    pub struct InventoryName;
    impl Dummy<InventoryName> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &InventoryName, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "Apollo x8p",
                    "SM7B",
                    "1176 Compressor",
                    "MacBook Pro 16\"",
                    "Mac Studio M2 Ultra",
                    "Neumann U87",
                    "AKG C414",
                    "Pro Tools License",
                    "Ableton Live 12",
                    "API 512c Preamp",
                    "Shure SM57",
                    "Universal Audio LA-2A",
                    "Genelec 8351B",
                    "Yamaha NS-10M",
                    "Sennheiser HD650",
                    "Focusrite Scarlett 18i20",
                    "Roland TR-8S",
                    "Moog Subsequent 37",
                    "Fender Stratocaster",
                    "Gibson Les Paul",
                ],
            )
        }
    }

    pub struct Category;
    impl Dummy<Category> for Option<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &Category, rng: &mut R) -> Self {
            Some(pick(rng, crate::INVENTORY_CATEGORIES))
        }
    }

    pub struct Status;
    impl Dummy<Status> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &Status, rng: &mut R) -> Self {
            const POOL: &[&str] = &[
                "active",
                "active",
                "active",
                "active",
                "checked-out",
                "in-repair",
                "retired",
            ];
            pick(rng, POOL)
        }
    }

    pub struct Manufacturer;
    impl Dummy<Manufacturer> for Option<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &Manufacturer, rng: &mut R) -> Self {
            Some(pick(
                rng,
                &[
                    "Universal Audio",
                    "Shure",
                    "Neumann",
                    "AKG",
                    "Apple",
                    "Avid",
                    "Ableton",
                    "API",
                    "Sennheiser",
                    "Focusrite",
                    "Roland",
                    "Moog",
                    "Fender",
                    "Gibson",
                    "Yamaha",
                    "Genelec",
                ],
            ))
        }
    }

    pub struct ModelNumber;
    impl Dummy<ModelNumber> for Option<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ModelNumber, rng: &mut R) -> Self {
            let prefix = pick(rng, &["MK", "PRO", "X", "V", "DX", "RX"]);
            let n: u32 = rng.random_range(100..9999);
            Some(format!("{}-{}", prefix, n))
        }
    }

    pub struct SerialNumber;
    impl Dummy<SerialNumber> for Option<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &SerialNumber, rng: &mut R) -> Self {
            let n: u64 = rng.random_range(100_000..9_999_999);
            Some(format!("SN-{:07}", n))
        }
    }

    pub struct QrCode;
    impl Dummy<QrCode> for Option<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &QrCode, rng: &mut R) -> Self {
            let n: u32 = rng.random_range(10_000..99_999);
            Some(format!("ASSET-{}", n))
        }
    }

    pub struct SubLocation;
    impl Dummy<SubLocation> for Option<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &SubLocation, rng: &mut R) -> Self {
            Some(pick(
                rng,
                &[
                    "Rack 1", "Rack 2", "Rack 3", "Locker 1", "Locker 2", "Locker 3", "Shelf A",
                    "Shelf B", "Drawer 1", "Drawer 2", "On Desk", "In Case",
                ],
            ))
        }
    }

    pub struct Vendor;
    impl Dummy<Vendor> for Option<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &Vendor, rng: &mut R) -> Self {
            Some(pick(
                rng,
                &[
                    "Sweetwater",
                    "B&H Photo",
                    "Vintage King",
                    "Reverb",
                    "Guitar Center",
                    "Apple Store",
                    "Amazon",
                    "Thomann",
                    "Perfect Circuit",
                ],
            ))
        }
    }

    pub struct PurchaseOrder;
    impl Dummy<PurchaseOrder> for Option<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &PurchaseOrder, rng: &mut R) -> Self {
            let n: u32 = rng.random_range(1_000..99_999);
            Some(format!("PO-{}", n))
        }
    }

    pub struct PastDateTime;
    impl Dummy<PastDateTime> for DateTime<Utc> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &PastDateTime, rng: &mut R) -> Self {
            Utc::now() - Duration::days(rng.random_range(1..1825))
        }
    }

    pub struct PastDateTimeOpt;
    impl Dummy<PastDateTimeOpt> for Option<DateTime<Utc>> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &PastDateTimeOpt, rng: &mut R) -> Self {
            Some(Utc::now() - Duration::days(rng.random_range(1..1825)))
        }
    }

    pub struct FutureDateTime;
    impl Dummy<FutureDateTime> for Option<DateTime<Utc>> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &FutureDateTime, rng: &mut R) -> Self {
            Some(Utc::now() + Duration::days(rng.random_range(1..30)))
        }
    }

    pub struct WarrantyDate;
    impl Dummy<WarrantyDate> for Option<DateTime<Utc>> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &WarrantyDate, rng: &mut R) -> Self {
            let days: i64 = rng.random_range(-365..1095);
            Some(Utc::now() + Duration::days(days))
        }
    }

    pub struct InventoryTags;
    impl Dummy<InventoryTags> for Vec<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &InventoryTags, rng: &mut R) -> Self {
            const POOL: &[&str] = &[
                "studio-a",
                "studio-b",
                "mobile",
                "high-value",
                "fragile",
                "rental",
                "vintage",
                "new",
                "backup",
            ];
            let n = rng.random_range(0..=3usize);
            POOL.choose_multiple(rng, n)
                .map(|s| s.to_string())
                .collect()
        }
    }
}
