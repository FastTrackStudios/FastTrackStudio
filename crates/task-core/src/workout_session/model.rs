//! `WorkoutSession` entity — a thin container for a logged workout.

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
#[sea_orm(table_name = "workout_sessions")]
#[crudcrate(
    api_struct = "WorkoutSessionApi",
    generate_vox_service,
    name_singular = "workout_session",
    name_plural = "workout_sessions"
)]
pub struct Model {
    #[facet(default)]
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    /// Soft FK to routines.id. Nullable so ad-hoc sessions ("just lift
    /// whatever I feel like") aren't required to start from a routine.
    #[crudcrate(filterable)]
    pub routine_id: Option<Uuid>,

    /// Snapshot of the routine name at session start so the session
    /// reads cleanly even if the routine is renamed/deleted.
    pub routine_name_snapshot: String,

    #[crudcrate(filterable)]
    pub status: WorkoutSessionStatus,

    pub started_at: DateTime<Utc>,
    pub completed_at: Option<DateTime<Utc>>,

    /// Free-form notes. ("Felt strong on bench, lower back tight on
    /// deadlifts. Cut the planks short.")
    pub notes: String,

    /// Subjective overall RPE 1.0..=10.0. Populated on complete.
    pub overall_rpe: Option<f32>,

    /// Bodyweight at session start in kg. Populated optionally to
    /// support relative-strength tracking later.
    pub bodyweight_kg: Option<f64>,

    #[crudcrate(filterable)]
    pub organization: Option<String>,

    pub created_by: Option<String>,

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
            notes: sea_orm::ActiveValue::Set(String::new()),
            properties: sea_orm::ActiveValue::Set(crate::property::JsonObject::default()),
            ..<Self as sea_orm::ActiveModelTrait>::default()
        }
    }
}

/// Lifecycle of a [`WorkoutSession`].
#[derive(
    Clone,
    Copy,
    Debug,
    PartialEq,
    Eq,
    Default,
    Serialize,
    Deserialize,
    DeriveActiveEnum,
    EnumIter,
    utoipa::ToSchema,
    facet::Facet,
)]
#[sea_orm(rs_type = "String", db_type = "String(StringLen::N(16))")]
#[repr(u8)]
pub enum WorkoutSessionStatus {
    #[default]
    #[sea_orm(string_value = "active")]
    Active,
    #[sea_orm(string_value = "completed")]
    Completed,
    #[sea_orm(string_value = "abandoned")]
    Abandoned,
}

impl WorkoutSessionStatus {
    #[must_use]
    pub fn as_str(&self) -> &'static str {
        match self {
            WorkoutSessionStatus::Active => "active",
            WorkoutSessionStatus::Completed => "completed",
            WorkoutSessionStatus::Abandoned => "abandoned",
        }
    }

    #[must_use]
    pub fn parse(s: &str) -> Option<Self> {
        match s.to_ascii_lowercase().as_str() {
            "active" => Some(WorkoutSessionStatus::Active),
            "completed" | "complete" | "done" => Some(WorkoutSessionStatus::Completed),
            "abandoned" | "abandon" => Some(WorkoutSessionStatus::Abandoned),
            _ => None,
        }
    }
}

pub type WorkoutSession = Model;
