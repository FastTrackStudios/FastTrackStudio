//! `SetLog` — one row per logged set within a [`crate::workout_session::WorkoutSession`].
//!
//! `completed_at = None` represents a planned set (the empty checkbox
//! waiting to be ticked off). `completed_at = Some(_)` is a finished
//! set. The CLI's primary action is flipping that field.

pub mod model;

pub use model::*;
