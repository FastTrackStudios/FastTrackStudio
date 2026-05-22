//! `PantryService` — wire surface for the pantry. Same
//! shape as `CookbookService` / `InventoryService`, plus
//! pantry-specific convenience mutators (`consume`,
//! `restock`, `open`, `set_expiry`).

use facet::Facet;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::model::{PantryItem, PantryItemDraft};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet, Error)]
#[repr(u8)]
pub enum PantryError {
    #[error("not found: {0}")]
    NotFound(String),
    #[error("already exists: {0}")]
    AlreadyExists(String),
    #[error("bad request: {0}")]
    BadRequest(String),
    #[error("insufficient stock: have {have} {unit}, need {need} {unit}")]
    InsufficientStock { have: f64, need: f64, unit: String },
    #[error("lookup: {0}")]
    Lookup(String),
    #[error("io: {0}")]
    Io(String),
}

#[architect::rpc]
pub trait PantryService {
    fn list(&self) -> Result<Vec<PantryItem>, PantryError>;

    fn get(&self, id: &str) -> Result<PantryItem, PantryError>;

    fn create(&self, item: PantryItem) -> Result<PantryItem, PantryError>;

    fn update(&self, item: PantryItem) -> Result<PantryItem, PantryError>;

    fn rename(&self, id: &str, new_path: &str) -> Result<PantryItem, PantryError>;

    fn delete(&self, id: &str) -> Result<(), PantryError>;

    /// Deduct `amount` from this item's `qty`. Errors with
    /// [`PantryError::InsufficientStock`] when the deduction
    /// would underflow. Used by mealplan's "I cooked this"
    /// flow.
    fn consume(&self, id: &str, amount: f64) -> Result<PantryItem, PantryError>;

    /// Add `amount` to this item's `qty`. Used by the
    /// "restock from grocery run" flow.
    fn restock(&self, id: &str, amount: f64) -> Result<PantryItem, PantryError>;

    /// Mark the package as opened and stamp today's date
    /// onto `opened_date`. Idempotent — calling on an
    /// already-opened item is a no-op.
    fn open(&self, id: &str) -> Result<PantryItem, PantryError>;

    /// Find a pantry row that already lists `barcode` in
    /// its `barcodes` field. `NotFound` if no local match
    /// — the caller can fall back to
    /// [`Self::resolve_barcode`] for an external lookup.
    fn find_by_barcode(&self, barcode: &str) -> Result<PantryItem, PantryError>;

    /// Resolve a barcode against the local vault first,
    /// then OpenFoodFacts as a fallback. The result tells
    /// the caller whether it's a known item (`Local`), an
    /// off-the-shelf draft ready to confirm (`Draft`), or
    /// nothing was found (`NotFound`).
    fn resolve_barcode(&self, barcode: &str) -> Result<BarcodeResolution, PantryError>;
}

/// Outcome of [`PantryService::resolve_barcode`].
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
#[repr(u8)]
pub enum BarcodeResolution {
    /// Barcode matches a pantry item already in the vault.
    Local(PantryItem),
    /// Not in the vault, but an external source returned a
    /// draft the caller can edit + persist via
    /// [`PantryService::create`] (use
    /// [`PantryItemDraft::into_item`]).
    Draft(PantryItemDraft),
    /// Neither local vault nor external lookup found the
    /// barcode.
    NotFound,
}
