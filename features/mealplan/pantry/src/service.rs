//! `PantryService` — wire surface for the pantry. Same
//! shape as `CookbookService` / `InventoryService`, plus
//! pantry-specific convenience mutators (`consume`,
//! `restock`, `open`, `set_expiry`).

use facet::Facet;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::model::PantryItem;

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
}
