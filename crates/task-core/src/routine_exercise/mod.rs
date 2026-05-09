//! `RoutineExercise` — child of [`crate::routine::Routine`].
//!
//! Holds one entry on a routine's ordered exercise list. Soft FKs to
//! `routines.id` (parent) and optionally `exercises.id` (canonical).
//! Free-form rows (no exercise_id, just display_name) are allowed for
//! "rest 5 min" / "drink water" markers.

pub mod model;

pub use model::{
    ActiveModel as RoutineExerciseActiveModel, Column as RoutineExerciseColumn,
    Entity as RoutineExerciseEntityRef, Model as RoutineExercise, RoutineExerciseApi,
};

pub use model::*;
