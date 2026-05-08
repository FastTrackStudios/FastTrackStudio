//! `Cookbook` — named collection of recipes.
//!
//! Many-to-many with `Recipe` via [`crate::cookbook_recipe`]. We use a
//! surrogate `id: Uuid` PK on the join table with a unique index over
//! `(cookbook_id, recipe_id)` rather than a composite PK — the crudcrate
//! `EntityToModels` derive doesn't currently emit ergonomic Api shapes
//! for composite-key entities.

pub mod model;

pub use model::Model as Cookbook;
pub use model::*;
