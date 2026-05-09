//! `WorkoutSession` — thin container for a logged workout.
//!
//! Pairs with [`crate::set_log::SetLog`] (one row per set). The session
//! itself is a dumb container: `Active` while sets are being checked
//! off, then flipped to `Completed` or `Abandoned`. The interactive
//! UX comes from the checkbox CLI on top of `SetLog`, not from any
//! server-side step machine.

pub mod model;

pub use model::*;
