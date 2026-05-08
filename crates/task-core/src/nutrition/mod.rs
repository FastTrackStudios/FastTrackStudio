//! `NutritionFacts` — JSON-shape nutrition payload.
//!
//! Lives inside `JsonObject` columns on `Food` and `FoodProduct`. Not a
//! SeaORM entity. Open Food Facts ingestion populates the `FoodProduct`
//! version; the Food carries default values for the generic ingredient.

pub mod model;

pub use model::NutritionFacts;
