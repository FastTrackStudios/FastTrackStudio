//! `Glossary` — workflow-agnostic term catalog plus `[[wikilink]]`
//! resolver for markdown bodies.
//!
//! Cooking is the first user (recipe steps reference `[[simmer]]`,
//! `[[deglaze]]`, ...) but the entity carries a `category` tag so
//! audio-production, fitness, etc. can co-exist without their terms
//! cross-resolving into one another. See
//! [`wikilinks::find_wikilinks`] for the parser and the [`Model`] doc
//! for the storage shape.

pub mod model;
pub mod wikilinks;

pub use model::{
    ActiveModel as GlossaryActiveModel, Column as GlossaryColumn, Entity as GlossaryEntity,
    GlossaryAliasList, GlossaryRelatedList, GlossaryTerm, GlossaryTermApi, Model, slugify,
};

pub use model::*;

pub use wikilinks::{
    ResolvedWikilink, WikilinkSpan, find_wikilinks, render_for_terminal, resolve_wikilinks,
};

use sea_orm::{ColumnTrait, ConnectionTrait, EntityTrait, QueryFilter};
use uuid::Uuid;

/// Look up a term by slug or any alias (case-insensitive) within an
/// organization scope. When `category` is `Some`, only terms in that
/// category are considered. Returns the first match.
///
/// Mirror of [`crate::food::find_food_by_name`]: catalog is small, we
/// materialize the candidate set and resolve in Rust to stay
/// backend-agnostic (no `json_each` requirement).
pub async fn find_term_by_slug_or_alias<C: ConnectionTrait>(
    db: &C,
    organization: Option<&str>,
    category: Option<&str>,
    slug_or_alias: &str,
) -> Result<Option<GlossaryTerm>, sea_orm::DbErr> {
    let needle_raw = slug_or_alias.trim();
    if needle_raw.is_empty() {
        return Ok(None);
    }
    let needle_lc = needle_raw.to_lowercase();
    let needle_slug = slugify(needle_raw);

    let mut q = GlossaryEntity::find();
    q = match organization {
        Some(org) => q.filter(model::Column::Organization.eq(org)),
        None => q.filter(model::Column::Organization.is_null()),
    };
    if let Some(cat) = category {
        q = q.filter(model::Column::Category.eq(cat));
    }
    let candidates = q.all(db).await?;

    // 1) Exact slug match.
    for row in &candidates {
        if row.slug == needle_slug || row.slug.to_lowercase() == needle_lc {
            return Ok(Some(row.clone()));
        }
    }
    // 2) Exact name (case-insensitive).
    for row in &candidates {
        if row.name.to_lowercase() == needle_lc {
            return Ok(Some(row.clone()));
        }
    }
    // 3) Alias match.
    for row in &candidates {
        for alias in &*row.aliases {
            if alias.to_lowercase() == needle_lc {
                return Ok(Some(row.clone()));
            }
        }
    }
    Ok(None)
}

/// Build a `slug -> id` map for a set of slugs by querying `glossary_terms`
/// with the optional org and category scope. Looks up by slug first,
/// then falls back to an alias scan for any slugs that didn't match.
pub async fn build_slug_index<C: ConnectionTrait>(
    db: &C,
    organization: Option<&str>,
    category: Option<&str>,
    slugs: &[String],
) -> Result<std::collections::HashMap<String, (Uuid, GlossaryTerm)>, sea_orm::DbErr> {
    let mut out = std::collections::HashMap::new();
    if slugs.is_empty() {
        return Ok(out);
    }
    let mut q = GlossaryEntity::find();
    q = match organization {
        Some(org) => q.filter(model::Column::Organization.eq(org)),
        None => q.filter(model::Column::Organization.is_null()),
    };
    if let Some(cat) = category {
        q = q.filter(model::Column::Category.eq(cat));
    }
    let candidates = q.all(db).await?;

    let needles: std::collections::HashSet<&str> = slugs.iter().map(String::as_str).collect();

    // First pass: slug match.
    for row in &candidates {
        if needles.contains(row.slug.as_str()) {
            out.insert(row.slug.clone(), (row.id, row.clone()));
        }
    }
    // Second pass: alias-based fallback for any unresolved needle.
    for needle in &needles {
        if out.contains_key(*needle) {
            continue;
        }
        for row in &candidates {
            if row
                .aliases
                .iter()
                .any(|a| a.to_lowercase() == needle.to_lowercase())
            {
                out.insert((*needle).to_string(), (row.id, row.clone()));
                break;
            }
        }
    }
    Ok(out)
}
