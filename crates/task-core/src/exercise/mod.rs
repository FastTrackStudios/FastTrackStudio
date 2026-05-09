//! `Exercise` — canonical fitness movement catalog.
//!
//! What routines reference ("Bench Press", "Back Squat", "5k Easy Run").
//! Linked from `RoutineExercise.exercise_id` (nullable, name-matched on
//! lookup). Movement-specific fields (modality, equipment, muscle
//! groups) drive UI and validation downstream.

pub mod model;

pub use model::{
    ActiveModel as ExerciseActiveModel, Column as ExerciseColumn, Entity as ExerciseEntityRef,
    ExerciseAliasList, ExerciseApi, ExerciseModality, ExerciseMuscleList, Model as Exercise,
};

pub use model::*;

use sea_orm::{ColumnTrait, ConnectionTrait, EntityTrait, QueryFilter};

/// Look up an Exercise by slug, canonical name, or alias
/// (case-insensitive) within an organization scope. Returns the first
/// match.
///
/// Resolution order: slug → name → alias.
pub async fn find_exercise_by_slug_or_alias<C: ConnectionTrait>(
    db: &C,
    organization: Option<&str>,
    needle: &str,
) -> Result<Option<Exercise>, sea_orm::DbErr> {
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

    // 1) Exact slug match.
    for row in &candidates {
        if row.slug.to_lowercase() == needle {
            return Ok(Some(row.clone()));
        }
    }
    // 2) Exact name match.
    for row in &candidates {
        if row.name.to_lowercase() == needle {
            return Ok(Some(row.clone()));
        }
    }
    // 3) Exact alias match.
    for row in &candidates {
        for alias in &*row.aliases {
            if alias.to_lowercase() == needle {
                return Ok(Some(row.clone()));
            }
        }
    }
    Ok(None)
}
