//! `cookbook` — typed view of recipe pages in a
//! `vault::Vault`.
//!
//! Recipes are markdown files with YAML frontmatter
//! (`type: recipe`) living under `<vault>/Wiki/Cookbook/` so
//! they ride on the wiki feature: bodies link freely into
//! curated concept pages (`[[saute]]`, `[[mise en place]]`),
//! and the wiki graph picks recipes up as just-another-page.
//! This crate only owns the typed surface — ingredients,
//! steps, nutrition. Narrative lives on the page body.
//!
//! Surface:
//! - [`Recipe`] / [`Ingredient`] / [`Nutrition`] / [`Course`]
//! - [`parse_page`] / [`looks_like_recipe`]
//! - [`serialize_recipe`] / [`write_recipe`]
//! - [`scan_vault`]
//! - [`CookbookService`] — `#[architect::rpc]` trait
//! - [`Store`] — `Arc<Mutex<vault::Vault>>`-backed impl that
//!   can share its vault snapshot with locations / inventory /
//!   pantry / mealplan via `Store::from_shared`.

#![cfg(not(target_arch = "wasm32"))]

pub mod model;
pub mod parse;
pub mod scan;
pub mod service;
pub mod store;
pub mod write;

pub use model::{Course, Ingredient, Nutrition, Recipe};
pub use parse::{ParseError, looks_like_recipe, parse_page};
pub use scan::scan_vault;
pub use service::{CookbookError, CookbookService};
pub use store::Store;
pub use write::{WriteError, default_recipe_path, serialize_recipe, write_recipe};
