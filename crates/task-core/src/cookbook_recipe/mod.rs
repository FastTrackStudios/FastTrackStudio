//! `CookbookRecipe` — many-to-many join between Cookbook and Recipe.
//!
//! Uses a surrogate `id: Uuid` PK with a unique index on
//! `(cookbook_id, recipe_id)` (see entity migration). `sequence` orders
//! recipes within a cookbook.

pub mod model;

pub use model::{
    ActiveModel, Column, CookbookRecipeApi, CookbookRecipeApiCreate, CookbookRecipeApiList,
    CookbookRecipeApiUpdate, Entity, Model, Model as CookbookRecipe,
};
