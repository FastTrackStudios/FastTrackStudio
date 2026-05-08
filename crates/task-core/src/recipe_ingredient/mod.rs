//! `RecipeIngredient` — child rows of `Recipe`, ordered by `sequence`.
//!
//! Mirrors the Mealie ingredient row: optional `quantity`/`unit`, free-text
//! `food`, optional `note`. `is_section = true` rows are rendered as
//! section headers ("For the sauce:") rather than ingredients.

pub mod model;

pub use model::Model as RecipeIngredient;
pub use model::*;
