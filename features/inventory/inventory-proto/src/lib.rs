//! `inventory-proto` — wire contract for the `inventory` feature.
//!
//! Three top-level entities:
//!
//! - `FoodProduct`      — a recurring ingredient / pantry SKU
//! - `PantryItem`       — what's actually in your pantry right now
//! - `ShoppingListItem` — an item on the shopping list

pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

// ── FoodProduct ───────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "food_products", repo)]
pub struct FoodProduct {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    pub name: String,

    #[architect(filterable)]
    pub brand: Option<String>,

    #[architect(filterable)]
    pub category: Option<String>,

    #[architect(filterable)]
    pub barcode: Option<String>,

    pub default_unit: Option<String>,

    pub default_qty_thousandths: Option<i64>,

    pub notes: Option<String>,

    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── PantryItem ────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "pantry_items", repo)]
pub struct PantryItem {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub product_id: Option<Uuid>,

    #[architect(filterable, fulltext)]
    pub name: String,

    #[architect(sortable)]
    pub qty_thousandths: i64,

    #[architect(filterable)]
    pub unit: String,

    #[architect(filterable)]
    pub location: Option<String>,

    #[architect(filterable, sortable)]
    pub expires_at: Option<DateTime<Utc>>,

    #[architect(filterable)]
    pub opened_at: Option<DateTime<Utc>>,

    pub notes: Option<String>,

    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── ShoppingListItem ──────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "shopping_list_items", repo)]
pub struct ShoppingListItem {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub product_id: Option<Uuid>,

    #[architect(filterable, fulltext)]
    pub name: String,

    pub qty_thousandths: i64,

    pub unit: String,

    #[architect(filterable)]
    pub purchased: bool,

    #[architect(filterable)]
    pub purchased_at: Option<DateTime<Utc>>,

    #[architect(sortable)]
    pub sort_index: i64,

    pub notes: Option<String>,

    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── InventoryService ──────────────────────────────────────────────────

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
    /// Mark a shopping list item as purchased.
    async fn mark_purchased(&self, item_id: Uuid) -> Result<(), InventoryServiceError>;
}
