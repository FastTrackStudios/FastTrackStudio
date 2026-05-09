//! `SetLog` entity — one row per logged or planned set.

use chrono::{DateTime, Utc};
use crudcrate::EntityToModels;
use facet::Facet;
use sea_orm::entity::prelude::*;
use serde::{Deserialize, Serialize};

#[derive(
    Clone,
    Debug,
    Default,
    PartialEq,
    Facet,
    DeriveEntityModel,
    EntityToModels,
    Serialize,
    Deserialize,
)]
#[sea_orm(table_name = "set_logs")]
#[crudcrate(
    api_struct = "SetLogApi",
    generate_vox_service,
    name_singular = "set_log",
    name_plural = "set_logs"
)]
pub struct Model {
    #[facet(default)]
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[crudcrate(filterable)]
    pub workout_session_id: Uuid,

    /// Soft FK to exercises.id. Nullable so free-form custom sets
    /// (display_name only) work — same shape as RoutineExercise.
    #[crudcrate(filterable)]
    pub exercise_id: Option<Uuid>,

    /// Snapshot of Exercise.name (or routine_exercise.display_name when
    /// the entry is custom). Lets the row stand alone even if the
    /// exercise is later renamed.
    pub exercise_name_snapshot: String,

    /// Optional pointer back to the routine_exercise row this set was
    /// logged against. Lets the UI show planned-vs-actual side-by-side.
    /// Nullable for ad-hoc sets unrelated to a routine row.
    #[crudcrate(filterable)]
    pub routine_exercise_id: Option<Uuid>,

    /// Position within the workout — 0-indexed in the order the set
    /// was logged. Multiple sets of the same exercise get sequential
    /// positions; the CLI uses this for stable ordering.
    pub position: i32,

    /// Set index within this exercise (0 = first set, 1 = second, ...)
    /// Computed at insert time as the count of existing SetLog rows
    /// for (workout_session_id, exercise_name_snapshot) so the UI can
    /// say "set 2 of 4".
    pub set_index: i32,

    // ── Strength fields ──
    pub reps: Option<u32>,
    pub weight_kg: Option<f64>,

    // ── Cardio / mobility fields ──
    pub duration_seconds: Option<u32>,
    pub distance_meters: Option<f64>,
    pub avg_hr: Option<u32>,
    pub pace_seconds_per_km: Option<u32>,

    /// RPE for this specific set, 1.0..=10.0. Optional.
    pub rpe: Option<f32>,

    /// Free-form note for this set ("plates loaded uneven", "form
    /// breakdown last 2", "ran out of breath").
    pub notes: Option<String>,

    /// When was this set marked complete? `None` = pending (the
    /// checkbox is empty). `Some` = done. The CLI's primary action
    /// is flipping this from None → Some.
    #[crudcrate(filterable)]
    pub completed_at: Option<DateTime<Utc>>,

    #[crudcrate(exclude(list))]
    #[facet(skip)]
    #[facet(default)]
    #[sea_orm(column_type = "Json")]
    pub properties: crate::property::JsonObject,

    #[crudcrate(exclude(create), on_create = chrono::Utc::now())]
    pub created_at: DateTime<Utc>,
    #[crudcrate(exclude(create), exclude(update), on_create = chrono::Utc::now())]
    pub updated_at: DateTime<Utc>,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {
    fn new() -> Self {
        Self {
            properties: sea_orm::ActiveValue::Set(crate::property::JsonObject::default()),
            ..<Self as sea_orm::ActiveModelTrait>::default()
        }
    }
}

pub type SetLog = Model;
