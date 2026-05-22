//! Walk a `vault::Vault` and collect every page that looks
//! like a recipe.

use vault::Vault;

use crate::model::Recipe;
use crate::parse::{looks_like_recipe, parse_page};

pub fn scan_vault(vault: &Vault) -> Vec<Recipe> {
    vault
        .pages
        .iter()
        .filter(|p| looks_like_recipe(p))
        .filter_map(|p| match parse_page(p) {
            Ok(r) => Some(r),
            Err(e) => {
                tracing::warn!(path = %p.rel_path, ?e, "recipe parse failed");
                None
            }
        })
        .collect()
}
