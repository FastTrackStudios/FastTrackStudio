//! Pure-Rust state helpers for the JSON columns on a
//! [`CookingSession`](super::Model): `mise_en_place_state` is a
//! `Vec<bool>` indexed by ingredient sequence, `step_states` is a
//! `Vec<StepState>` indexed by step sequence.
//!
//! These types live separately from the entity so unit tests can cover
//! pause/resume math without spinning up a database. They serialize
//! transparently into [`crate::property::JsonObject`] columns.

use chrono::{DateTime, Duration, Utc};
use serde::{Deserialize, Serialize};

use crate::property::JsonObject;

/// Per-step timer state. Tracks pause/resume cycles via
/// `pause_offset_seconds` so resumed timers compute remaining time
/// correctly across multiple pause/resume cycles.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct StepState {
    /// When the user first hit "start". `None` until the timer is
    /// started.
    pub started_at: Option<DateTime<Utc>>,
    /// When a currently-active timer was paused. `Some` only while
    /// paused.
    pub paused_at: Option<DateTime<Utc>>,
    /// Accumulated paused duration in whole seconds — `pause`/`resume`
    /// cycles add `(now - paused_at)` here on resume so multiple
    /// pause/resume rounds compose correctly.
    pub pause_offset_seconds: u32,
    /// When the user explicitly hit "complete".
    pub completed_at: Option<DateTime<Utc>>,
}

impl StepState {
    /// True when the timer has been started and not yet completed.
    /// Includes paused timers (they're started, just frozen).
    #[must_use]
    pub fn is_running(&self) -> bool {
        self.started_at.is_some() && self.completed_at.is_none()
    }

    /// True when the timer is currently paused.
    #[must_use]
    pub fn is_paused(&self) -> bool {
        self.is_running() && self.paused_at.is_some()
    }

    /// True when the timer has been completed.
    #[must_use]
    pub fn is_completed(&self) -> bool {
        self.completed_at.is_some()
    }

    /// Wall-clock time spent on this step so far, excluding paused
    /// intervals. Returns `None` if the step hasn't started yet.
    ///
    /// When the timer is currently paused the elapsed time freezes at
    /// the value computed at the moment of pausing. When completed it
    /// reflects total work time at completion.
    #[must_use]
    pub fn elapsed(&self, now: DateTime<Utc>) -> Option<Duration> {
        let started = self.started_at?;
        // Anchor "now" to completion or pause when those are set so the
        // returned elapsed is stable.
        let anchor = match (self.completed_at, self.paused_at) {
            (Some(done), _) => done,
            (None, Some(paused)) => paused,
            _ => now,
        };
        let raw = anchor - started;
        let pause_offset = Duration::seconds(i64::from(self.pause_offset_seconds));
        let net = raw - pause_offset;
        Some(if net < Duration::zero() {
            Duration::zero()
        } else {
            net
        })
    }

    /// Seconds remaining for a step that has a `duration_minutes` hint.
    /// Returns `None` when the step hasn't started yet. Negative values
    /// indicate the step has overrun its expected duration.
    #[must_use]
    pub fn timer_remaining(&self, now: DateTime<Utc>, duration_minutes: u32) -> Option<Duration> {
        let elapsed = self.elapsed(now)?;
        let expected = Duration::seconds(i64::from(duration_minutes) * 60);
        Some(expected - elapsed)
    }
}

// ── JSON serialization helpers ──────────────────────────────────────

/// Decode the `mise_en_place_state` JsonObject as a `Vec<bool>`. Returns
/// an empty vec when the column was created from `Default::default()`
/// (the default is `{}`, not `[]`, so we treat any non-array shape as
/// "no state yet").
#[must_use]
pub fn mise_en_place_from_json(value: &JsonObject) -> Vec<bool> {
    match value.as_value() {
        serde_json::Value::Array(arr) => arr.iter().map(|v| v.as_bool().unwrap_or(false)).collect(),
        _ => Vec::new(),
    }
}

/// Encode a `Vec<bool>` mise-en-place state as a JsonObject.
#[must_use]
pub fn mise_en_place_to_json(checks: &[bool]) -> JsonObject {
    JsonObject::from_value(serde_json::Value::Array(
        checks
            .iter()
            .copied()
            .map(serde_json::Value::Bool)
            .collect(),
    ))
}

/// Decode the `step_states` JsonObject as a `Vec<StepState>`. Empty vec
/// when not yet initialized.
#[must_use]
pub fn step_states_from_json(value: &JsonObject) -> Vec<StepState> {
    match value.as_value() {
        serde_json::Value::Array(_) => {
            serde_json::from_value(value.as_value().clone()).unwrap_or_default()
        }
        _ => Vec::new(),
    }
}

/// Encode `Vec<StepState>` as a JsonObject.
pub fn step_states_to_json(states: &[StepState]) -> JsonObject {
    JsonObject::from_value(
        serde_json::to_value(states).unwrap_or(serde_json::Value::Array(Vec::new())),
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::TimeZone;

    fn dt(secs: i64) -> DateTime<Utc> {
        Utc.timestamp_opt(1_700_000_000 + secs, 0).single().unwrap()
    }

    #[test]
    fn default_step_state_not_running() {
        let s = StepState::default();
        assert!(!s.is_running());
        assert!(!s.is_paused());
        assert!(!s.is_completed());
        assert_eq!(s.elapsed(dt(0)), None);
    }

    #[test]
    fn started_only_elapsed_grows_with_now() {
        let s = StepState {
            started_at: Some(dt(0)),
            ..Default::default()
        };
        assert!(s.is_running());
        assert!(!s.is_paused());
        assert_eq!(s.elapsed(dt(30)).unwrap().num_seconds(), 30);
        assert_eq!(s.elapsed(dt(120)).unwrap().num_seconds(), 120);
    }

    #[test]
    fn paused_freezes_elapsed_at_pause_moment() {
        let s = StepState {
            started_at: Some(dt(0)),
            paused_at: Some(dt(45)),
            ..Default::default()
        };
        assert!(s.is_paused());
        // even if `now` advances, elapsed stays at the pause moment.
        assert_eq!(s.elapsed(dt(45)).unwrap().num_seconds(), 45);
        assert_eq!(s.elapsed(dt(500)).unwrap().num_seconds(), 45);
    }

    #[test]
    fn resume_subtracts_paused_offset() {
        // started 0, paused 30..60 (30s offset), resumed, now at 100.
        // raw = 100 - 0 = 100, pause_offset = 30 → elapsed = 70.
        let s = StepState {
            started_at: Some(dt(0)),
            pause_offset_seconds: 30,
            ..Default::default()
        };
        assert_eq!(s.elapsed(dt(100)).unwrap().num_seconds(), 70);
    }

    #[test]
    fn multiple_pause_resume_cycles_compose() {
        // pause 30..50 (20s), then 70..90 (20s) → total offset 40s.
        // started 0, now 120 → raw 120 - offset 40 = 80.
        let s = StepState {
            started_at: Some(dt(0)),
            pause_offset_seconds: 40,
            ..Default::default()
        };
        assert_eq!(s.elapsed(dt(120)).unwrap().num_seconds(), 80);
    }

    #[test]
    fn pause_currently_active_with_prior_offset() {
        // Already accumulated 20s offset, started at 0, now paused at 80.
        // Anchor = paused_at (80). Raw = 80 - 0 = 80. Net = 80 - 20 = 60.
        let s = StepState {
            started_at: Some(dt(0)),
            paused_at: Some(dt(80)),
            pause_offset_seconds: 20,
            ..Default::default()
        };
        // elapsed stable at 60s regardless of `now`.
        assert_eq!(s.elapsed(dt(80)).unwrap().num_seconds(), 60);
        assert_eq!(s.elapsed(dt(500)).unwrap().num_seconds(), 60);
    }

    #[test]
    fn completed_freezes_elapsed_at_completion() {
        let s = StepState {
            started_at: Some(dt(0)),
            completed_at: Some(dt(200)),
            pause_offset_seconds: 50,
            ..Default::default()
        };
        assert!(s.is_completed());
        assert!(!s.is_running());
        // 200 - 0 - 50 = 150
        assert_eq!(s.elapsed(dt(9999)).unwrap().num_seconds(), 150);
    }

    #[test]
    fn negative_net_clamps_to_zero() {
        // Pause offset > raw — shouldn't happen in practice but must
        // never go negative.
        let s = StepState {
            started_at: Some(dt(0)),
            pause_offset_seconds: 200,
            ..Default::default()
        };
        assert_eq!(s.elapsed(dt(10)).unwrap().num_seconds(), 0);
    }

    #[test]
    fn timer_remaining_returns_none_before_start() {
        let s = StepState::default();
        assert_eq!(s.timer_remaining(dt(0), 5), None);
    }

    #[test]
    fn timer_remaining_under_duration() {
        let s = StepState {
            started_at: Some(dt(0)),
            ..Default::default()
        };
        // 5 minute timer, 60s elapsed → 240s remaining
        assert_eq!(s.timer_remaining(dt(60), 5).unwrap().num_seconds(), 240);
    }

    #[test]
    fn timer_remaining_overrun_negative() {
        let s = StepState {
            started_at: Some(dt(0)),
            ..Default::default()
        };
        // 1 minute timer, 90s elapsed → -30s remaining (overrun).
        assert_eq!(s.timer_remaining(dt(90), 1).unwrap().num_seconds(), -30);
    }

    #[test]
    fn timer_remaining_excludes_paused_time() {
        let s = StepState {
            started_at: Some(dt(0)),
            pause_offset_seconds: 60,
            ..Default::default()
        };
        // 5 min timer, raw 120s elapsed but 60s paused → net 60s.
        // Remaining = 300 - 60 = 240.
        assert_eq!(s.timer_remaining(dt(120), 5).unwrap().num_seconds(), 240);
    }

    #[test]
    fn round_trip_step_states_json() {
        let states = vec![
            StepState {
                started_at: Some(dt(10)),
                pause_offset_seconds: 5,
                ..Default::default()
            },
            StepState::default(),
            StepState {
                started_at: Some(dt(0)),
                completed_at: Some(dt(60)),
                ..Default::default()
            },
        ];
        let json = step_states_to_json(&states);
        let back = step_states_from_json(&json);
        assert_eq!(states, back);
    }

    #[test]
    fn round_trip_mise_en_place_json() {
        let checks = vec![true, false, true, true, false];
        let json = mise_en_place_to_json(&checks);
        let back = mise_en_place_from_json(&json);
        assert_eq!(checks, back);
    }

    #[test]
    fn empty_default_jsonobject_decodes_to_empty_vec() {
        let empty = JsonObject::default();
        assert_eq!(mise_en_place_from_json(&empty), Vec::<bool>::new());
        assert_eq!(step_states_from_json(&empty), Vec::<StepState>::new());
    }

    #[test]
    fn is_paused_only_when_running() {
        // Completed step — paused_at clear. Should not report paused.
        let s = StepState {
            started_at: Some(dt(0)),
            completed_at: Some(dt(60)),
            paused_at: None,
            pause_offset_seconds: 0,
        };
        assert!(!s.is_running());
        assert!(!s.is_paused());
    }
}
