//! `fitness-proto` — wire contract for the fitness-tracking feature.
//!
//! Five top-level entities:
//!
//! - `Exercise`        — a reusable movement / activity definition
//! - `Routine`         — an ordered workout template
//! - `WorkoutSession`  — an actual session performed by the user
//! - `SetLog`          — one set within a session (reps / weight / time / distance)
//! - `BodyMeasurement` — periodic body check-in (weight, body fat, etc.)

pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

// ── Exercise ──────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "exercises", repo)]
pub struct Exercise {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    pub name: String,

    #[architect(filterable)]
    pub category: Option<String>,

    pub muscle_groups: Vec<String>,

    #[architect(filterable)]
    pub equipment: Option<String>,

    #[architect(fulltext)]
    pub instructions: Option<String>,

    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── Routine ───────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "routines", repo)]
pub struct Routine {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    pub name: String,

    #[architect(fulltext)]
    pub description: Option<String>,

    pub target_duration_minutes: Option<u32>,

    #[architect(filterable)]
    pub difficulty: Option<String>,

    pub exercise_ids: Vec<String>,

    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── WorkoutSession ────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "workout_sessions", repo)]
pub struct WorkoutSession {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub routine_id: Option<Uuid>,

    #[architect(filterable, sortable, fulltext)]
    pub name: String,

    #[architect(filterable, sortable)]
    pub started_at: DateTime<Utc>,

    #[architect(filterable, sortable)]
    pub ended_at: Option<DateTime<Utc>>,

    #[architect(fulltext)]
    pub notes: Option<String>,

    #[architect(filterable)]
    pub mood: Option<String>,

    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── SetLog ────────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "set_logs", repo)]
pub struct SetLog {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable)]
    pub session_id: Uuid,

    #[architect(filterable)]
    pub exercise_id: Uuid,

    #[architect(sortable)]
    pub set_number: u32,

    pub reps: Option<u32>,

    #[architect(sortable)]
    pub weight_grams: Option<i64>,

    pub duration_seconds: Option<u32>,

    pub distance_meters: Option<u32>,

    pub rpe: Option<u32>,

    pub notes: Option<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── BodyMeasurement ───────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "body_measurements", repo)]
pub struct BodyMeasurement {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable)]
    pub taken_at: DateTime<Utc>,

    #[architect(sortable)]
    pub weight_grams: Option<i64>,

    pub body_fat_pct_x10: Option<u32>,

    pub waist_mm: Option<u32>,

    pub chest_mm: Option<u32>,

    pub arm_mm: Option<u32>,

    pub thigh_mm: Option<u32>,

    pub notes: Option<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── FitnessService ────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum FitnessServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait FitnessService {
    /// Mark a session complete by setting `ended_at` to now.
    async fn complete_session(&self, session_id: Uuid) -> Result<(), FitnessServiceError>;
}
