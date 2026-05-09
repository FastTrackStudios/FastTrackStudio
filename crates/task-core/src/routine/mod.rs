//! `Routine` — workout template (PPL day, 5k Easy Run, mobility flow).
//!
//! A `Routine` belongs to an organization and carries the human-readable
//! programming notes for one workout template. Exercise rows live in a
//! dedicated child table (`routine_exercises`) ordered by `position`,
//! optionally grouped by `group_label` for supersets / circuits.

pub mod model;

pub use model::{
    ActiveModel as RoutineActiveModel, Column as RoutineColumn, Entity as RoutineEntityRef,
    Model as Routine, RoutineApi, RoutineTagList,
};

pub use model::*;

use sea_orm::{ColumnTrait, ConnectionTrait, EntityTrait, QueryFilter};

/// Look up a Routine by slug or canonical name (case-insensitive)
/// within an organization scope. Returns the first match.
pub async fn find_routine_by_slug_or_name<C: ConnectionTrait>(
    db: &C,
    organization: Option<&str>,
    needle: &str,
) -> Result<Option<Routine>, sea_orm::DbErr> {
    let needle = needle.trim().to_lowercase();
    if needle.is_empty() {
        return Ok(None);
    }

    let mut q = Entity::find();
    q = match organization {
        Some(org) => q.filter(Column::Organization.eq(org)),
        None => q.filter(Column::Organization.is_null()),
    };
    let candidates = q.all(db).await?;

    for row in &candidates {
        if row.slug.to_lowercase() == needle {
            return Ok(Some(row.clone()));
        }
    }
    for row in &candidates {
        if row.name.to_lowercase() == needle {
            return Ok(Some(row.clone()));
        }
    }
    Ok(None)
}
