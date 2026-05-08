//! `FoodProduct` — branded, barcode-keyed product.
//!
//! Specific instances of a [`crate::food::Food`]: "Trader Joe's Extra
//! Virgin Olive Oil 500ml" links to the generic "olive oil" Food. Open
//! Food Facts ingestion creates these; the Pantry and FoodLog beads
//! reference them when the user scans / logs a specific item.

pub mod model;

pub use model::{
    ActiveModel as FoodProductActiveModel, Column as FoodProductColumn,
    Entity as FoodProductEntityRef, FoodProductApi, Model as FoodProduct,
};

pub use model::*;
