//! `CookbookService` — wire surface for browsing + mutating
//! recipes, decorated with `#[architect::rpc]`.
//!
//! Same shape as `LocationsService` / `InventoryService`: sync
//! trait, vox async client emitted by the macro. Agent
//! integrations (e.g. "find me a 30-minute vegan dinner using
//! what's in the pantry") drive this surface remotely.

use facet::Facet;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::model::Recipe;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet, Error)]
#[repr(u8)]
pub enum CookbookError {
    #[error("not found: {0}")]
    NotFound(String),
    #[error("already exists: {0}")]
    AlreadyExists(String),
    #[error("bad request: {0}")]
    BadRequest(String),
    #[error("io: {0}")]
    Io(String),
}

#[architect::rpc]
pub trait CookbookService {
    /// Every recipe currently in the vault.
    fn list(&self) -> Result<Vec<Recipe>, CookbookError>;

    /// One recipe by id.
    fn get(&self, id: &str) -> Result<Recipe, CookbookError>;

    /// Create a new recipe. Backend assigns `recipe.path`
    /// (default `Wiki/Cookbook/<slug>.md`) and `recipe.id` if
    /// either is empty / nil.
    fn create(&self, recipe: Recipe) -> Result<Recipe, CookbookError>;

    /// Replace the recipe whose `id` matches. Path is
    /// preserved; rename via [`Self::rename`].
    fn update(&self, recipe: Recipe) -> Result<Recipe, CookbookError>;

    /// Move the backing file to a new vault-relative path.
    fn rename(&self, id: &str, new_path: &str) -> Result<Recipe, CookbookError>;

    /// Remove the backing file.
    fn delete(&self, id: &str) -> Result<(), CookbookError>;
}
