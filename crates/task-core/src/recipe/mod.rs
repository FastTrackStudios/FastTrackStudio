//! Recipe entity — Mealie-style recipe used by the cooking workflow.
//!
//! A `Recipe` belongs (loosely) to an organization and carries the
//! human metadata for one cooking artifact: prep/cook times, servings,
//! source URL, rating, last-made date. Polymorphic
//! `properties: JsonObject` (cuisine, dietary tags, …) matches the
//! Obsidian-frontmatter shape used elsewhere. Ingredient and step rows
//! live in dedicated child tables (`recipe_ingredients`, `recipe_steps`)
//! ordered by `sequence`.
//!
//! Slug derivation rules:
//! - Generated from `name` via [`slug::slugify`] when none is given.
//! - Unique within `(organization, slug)` — collision handling appends
//!   `-2`, `-3`, … via the [`CookingService`](crate::service::CookingService)
//!   layer (the entity itself only enforces a non-empty string).

pub mod model;

pub use model::{
    ActiveModel, Column, Entity, Model, Model as Recipe, RecipeApi, RecipeApiCreate, RecipeApiList,
    RecipeApiUpdate, RecipeIngredientSpec, RecipeStepSpec,
};
