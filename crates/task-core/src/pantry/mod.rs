//! `PantryItem` — stocked food (generic Food and/or branded FoodProduct)
//! at an organization, optionally placed in a storage Location.
//!
//! Storage Locations are shared with the Location entity; pantry-storage
//! locations are tagged via `Location::venue_type = "pantry-storage"` so
//! the same table holds both venues and storage spots.

pub mod model;

pub use model::{
    ActiveModel as PantryItemActiveModel, Column as PantryItemColumn,
    Entity as PantryItemEntityRef, Model as PantryItem, PantryItemApi,
};

pub use model::*;
