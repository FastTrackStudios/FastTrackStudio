//! `RoutineExercise` entity — child row of a Routine template.

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
#[sea_orm(table_name = "routine_exercises")]
#[crudcrate(
    api_struct = "RoutineExerciseApi",
    generate_vox_service,
    name_singular = "routine_exercise",
    name_plural = "routine_exercises"
)]
pub struct Model {
    #[facet(default)]
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    /// Soft FK to routines.id.
    #[crudcrate(filterable)]
    pub routine_id: Uuid,

    /// Soft FK to exercises.id. Nullable to allow free-form text-only
    /// entries ("rest 5 min between rounds") that aren't an exercise.
    #[crudcrate(filterable)]
    pub exercise_id: Option<Uuid>,

    /// Display name snapshot — denormalized so the routine reads
    /// cleanly even when the linked exercise is renamed. Set from
    /// Exercise.name on insert when exercise_id is provided.
    pub display_name: String,

    /// Position within the routine, 0-indexed.
    pub position: i32,

    /// Optional grouping label for supersets / circuits ("A1", "A2",
    /// "B1", "B2"). Same group_label = perform back-to-back.
    pub group_label: Option<String>,

    // ─── Strength targets (apply when linked Exercise.modality = Strength) ───
    pub target_sets: Option<u32>,
    pub target_reps: Option<u32>,
    /// In kilograms. UI translates if user prefers lbs.
    pub target_weight_kg: Option<f64>,
    /// Rest between sets in seconds. UI hint only — no enforcement.
    pub target_rest_seconds: Option<u32>,
    /// Target RPE (rate of perceived exertion), 1.0..=10.0.
    pub target_rpe: Option<f32>,
    /// Tempo string e.g. "3-1-1-0" (eccentric-pause-concentric-pause).
    pub tempo: Option<String>,

    // ─── Cardio targets (apply when modality = Cardio) ───
    pub target_duration_seconds: Option<u32>,
    pub target_distance_meters: Option<f64>,
    /// Target average HR (bpm).
    pub target_avg_hr: Option<u32>,
    /// Target pace as seconds per kilometer.
    pub target_pace_seconds_per_km: Option<u32>,

    /// Free-form note ("warm-up only", "drop set on last", "alternate
    /// arms"). Markdown-friendly.
    pub notes: Option<String>,

    #[crudcrate(exclude(list))]
    #[facet(skip)]
    #[facet(default)]
    #[sea_orm(column_type = "Json")]
    pub properties: crate::property::JsonObject,

    #[crudcrate(exclude(create), on_create = chrono::Utc::now())]
    pub created_at: chrono::DateTime<chrono::Utc>,
    #[crudcrate(exclude(create), exclude(update), on_create = chrono::Utc::now())]
    pub updated_at: chrono::DateTime<chrono::Utc>,
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

pub type RoutineExercise = Model;
