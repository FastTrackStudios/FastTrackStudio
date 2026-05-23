//! `CookbookService` — wire surface for browsing + mutating
//! cooklang `.cook` files.
//!
//! Identity is the vault-relative `path`. There is no UUID;
//! cooklang files don't carry one and the cookbook layer
//! doesn't synthesize one. Meals, agents, and the wiki all
//! reference recipes by path.

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
    /// Every `.cook` file currently under `<vault>/Wiki/Cookbook/`.
    fn list(&self) -> Result<Vec<Recipe>, CookbookError>;

    /// One recipe by vault-relative path.
    fn get(&self, path: &str) -> Result<Recipe, CookbookError>;

    /// Create a new recipe. Caller supplies `path` + `source`
    /// in [`Recipe`]; backend fills out parsed fields and
    /// stamps `date_modified`.
    fn create(&self, recipe: Recipe) -> Result<Recipe, CookbookError>;

    /// Replace the file at `recipe.path` with `recipe.source`.
    /// Backend re-parses and returns the refreshed view.
    fn update(&self, recipe: Recipe) -> Result<Recipe, CookbookError>;

    /// Move the backing file (and sibling step/title images)
    /// to a new vault-relative path.
    fn rename(&self, old_path: &str, new_path: &str) -> Result<Recipe, CookbookError>;

    /// Delete the backing file (and sibling images).
    fn delete(&self, path: &str) -> Result<(), CookbookError>;
}
