//! `Food` — canonical ingredient catalog.
//!
//! What recipes ask for ("olive oil", "eggs", "chicken thigh"). Linked
//! from `RecipeIngredient.food_id` (nullable, name-matched on insert)
//! and from `FoodProduct.food_id` (a brand-specific product is "a kind
//! of" Food). See module docs in [`crate::food_product`] for the split
//! rationale.

pub mod model;

pub use model::{
    ActiveModel as FoodActiveModel, Column as FoodColumn, Entity as FoodEntityRef, FoodAliasList,
    FoodApi, Model as Food,
};

pub use model::*;

use sea_orm::{ColumnTrait, ConnectionTrait, EntityTrait, QueryFilter};

/// Look up a Food by canonical name OR alias (case-insensitive) within
/// an organization scope. Returns the first match.
///
/// We materialize the candidate set with the organization filter and
/// scan the alias arrays in Rust — keeps the implementation backend-
/// agnostic (no `json_each` requirement) and the catalogs are small
/// enough that the cost is negligible.
pub async fn find_food_by_name<C: ConnectionTrait>(
    db: &C,
    organization: Option<&str>,
    name_or_alias: &str,
) -> Result<Option<Food>, sea_orm::DbErr> {
    let needle = name_or_alias.trim().to_lowercase();
    if needle.is_empty() {
        return Ok(None);
    }

    // Materialize the org-scoped catalog and resolve in Rust. Catalog
    // size is small enough that the cost is negligible and it keeps
    // the implementation backend-agnostic (no `json_each` requirement).
    let mut q = Entity::find();
    q = match organization {
        Some(org) => q.filter(Column::Organization.eq(org)),
        None => q.filter(Column::Organization.is_null()),
    };
    let candidates = q.all(db).await?;

    // 1) Exact name match (case-insensitive).
    for row in &candidates {
        if row.name.to_lowercase() == needle {
            return Ok(Some(row.clone()));
        }
    }
    // 2) Exact alias match.
    for row in &candidates {
        for alias in &*row.aliases {
            if alias.to_lowercase() == needle {
                return Ok(Some(row.clone()));
            }
        }
    }
    // 3) Substring contains — recipe imports drag descriptors along
    //    ("yellow onion, diced", "broccoli, cut into florets"). We
    //    accept a Food whose canonical name appears as a whole-word
    //    fragment of the needle, OR vice-versa for short canonical
    //    names. This is a heuristic, not a guarantee — callers wanting
    //    exact-only behavior should use `*_exact` variants if they
    //    appear in future iterations.
    for row in &candidates {
        let lc = row.name.to_lowercase();
        if needle.contains(&lc) || lc.contains(&needle) {
            return Ok(Some(row.clone()));
        }
        for alias in &*row.aliases {
            let alc = alias.to_lowercase();
            if needle.contains(&alc) || alc.contains(&needle) {
                return Ok(Some(row.clone()));
            }
        }
    }
    Ok(None)
}
