//! Walk the vault for routines + workout sessions.

use chrono::NaiveDate;
use uuid::Uuid;
use vault::Vault;

use crate::model::{Routine, WorkoutSession};
use crate::parse::{looks_like_routine, looks_like_session, parse_routine, parse_session};

pub fn scan_routines(vault: &Vault) -> Vec<Routine> {
    vault
        .pages
        .iter()
        .filter(|p| looks_like_routine(p))
        .filter_map(|p| match parse_routine(p) {
            Ok(r) => Some(r),
            Err(e) => {
                tracing::warn!(path = %p.rel_path, ?e, "routine parse failed");
                None
            }
        })
        .collect()
}

pub fn scan_sessions(vault: &Vault) -> Vec<WorkoutSession> {
    vault
        .pages
        .iter()
        .filter(|p| looks_like_session(p))
        .filter_map(|p| match parse_session(p) {
            Ok(s) => Some(s),
            Err(e) => {
                tracing::warn!(path = %p.rel_path, ?e, "session parse failed");
                None
            }
        })
        .collect()
}

/// Sessions in `[start, end)`. Useful for weekly volume
/// summaries.
pub fn sessions_between(vault: &Vault, start: NaiveDate, end: NaiveDate) -> Vec<WorkoutSession> {
    scan_sessions(vault)
        .into_iter()
        .filter(|s| s.date >= start && s.date < end)
        .collect()
}

/// Sessions that logged at least one set of `exercise_id`.
/// Drives the "show me bench-press progression" view —
/// caller charts the resulting [`crate::model::LoggedSet`]
/// max weights over time.
pub fn sessions_for_exercise(vault: &Vault, exercise_id: Uuid) -> Vec<WorkoutSession> {
    scan_sessions(vault)
        .into_iter()
        .filter(|s| {
            s.logged_sets
                .iter()
                .any(|set| set.exercise_id == exercise_id)
        })
        .collect()
}
