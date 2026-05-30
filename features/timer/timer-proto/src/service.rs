//! Slim domain trait for atomic / lifecycle-aware ops.
//!
//! CRUD on the entities (Client, WorkSession, Tag, Rate
//! tables) is the architect-emitted per-entity Repo trait.
//! This trait carries the operations CRUD can't enforce:
//!
//! - **`start_timer`** atomically asserts the "one open
//!   session per user" invariant.
//! - **`stop_timer`** closes the currently-open session
//!   for a user, snapshots the rate cascade into
//!   `rate_cents` + `currency`, and emits the closed row.
//! - **`active_timer`** is the cheap "is anything running
//!   right now?" lookup the UI polls.
//! - **`switch_timer`** is a compound that stops the open
//!   session (if any) and starts a new one in one
//!   transaction — the common case for a user moving from
//!   task to task.
//! - **`resolve_rate`** runs the cascade and returns the
//!   rate cents + currency a session would close at, for
//!   the UI to render before stop.
//!
//! Owned-String params throughout — same convention as
//! `agent-proto`'s service traits, dictated by what
//! `#[architect::rpc]`'s async dispatcher emits cleanly.

use chrono::{DateTime, Utc};
use uuid::Uuid;

use crate::error::TimerError;
use crate::session::{WorkSession, WorkSessionFilter};

#[architect::rpc]
pub trait TimerService {
    /// Start a new work session for the calling user. Fails
    /// with [`TimerError::AlreadyRunning`] if the user
    /// already has an open session. The new row's
    /// `billable` is seeded from the project's
    /// `billableDefault` (or `false` when no project link).
    async fn start_timer(&self, req: StartTimerRequest) -> Result<WorkSession, TimerError>;

    /// Stop the calling user's open session. Snapshots
    /// `rate_cents` + `currency` via the rate cascade, sets
    /// `end_time = now`, returns the closed row. Fails with
    /// [`TimerError::NoActiveTimer`] when nothing is open.
    async fn stop_timer(&self, user_id: Uuid) -> Result<WorkSession, TimerError>;

    /// Read the calling user's currently-open session.
    /// Returns `Ok(None)` (not an error) when nothing is
    /// running — this is the cheap UI poll.
    async fn active_timer(&self, user_id: Uuid) -> Result<Option<WorkSession>, TimerError>;

    /// Stop-then-start in one transaction. Equivalent to
    /// `stop_timer` followed by `start_timer`, but atomic so
    /// a UI button-press doesn't briefly show two open
    /// sessions (or none) under contention. Returns
    /// `(closed_or_none, started)`.
    async fn switch_timer(
        &self,
        req: StartTimerRequest,
    ) -> Result<(Option<WorkSession>, WorkSession), TimerError>;

    /// Manually retro-log a session (start + end in the
    /// past). Skips the active-timer invariant; the open
    /// session stays open. The frontend uses this for
    /// "I forgot to start the timer" entries.
    async fn log_session(&self, req: LogSessionRequest) -> Result<WorkSession, TimerError>;

    /// Resolve the rate cascade for the given
    /// `(user_id, project_id)`. Returns the cents/hour and
    /// the ISO 4217 currency that would be snapshotted if a
    /// session closed right now. Cents = 0 when no rate is
    /// configured at any level.
    async fn resolve_rate(
        &self,
        user_id: Uuid,
        project_id: Option<Uuid>,
    ) -> Result<RateResolution, TimerError>;

    /// List sessions matching `filter` (by user / project / date range
    /// / billable / open). Powers the timer page's history + totals.
    async fn list_sessions(
        &self,
        filter: WorkSessionFilter,
    ) -> Result<Vec<WorkSession>, TimerError>;
}

/// Args for [`TimerService::start_timer`].
#[derive(::facet::Facet, serde::Serialize, serde::Deserialize, Clone, Debug, PartialEq)]
#[repr(C)]
pub struct StartTimerRequest {
    pub user_id: Uuid,
    pub org_id: Uuid,
    /// Project the session belongs to. `None` = uncategorized
    /// (still tracked, just doesn't roll up).
    pub project_id: Option<Uuid>,
    /// Vault path cache. Empty = no project link.
    pub project_path: String,
    pub task_note_path: String,
    pub description: String,
}

/// Args for [`TimerService::log_session`].
#[derive(::facet::Facet, serde::Serialize, serde::Deserialize, Clone, Debug, PartialEq)]
#[repr(C)]
pub struct LogSessionRequest {
    pub user_id: Uuid,
    pub org_id: Uuid,
    pub project_id: Option<Uuid>,
    pub project_path: String,
    pub task_note_path: String,
    pub description: String,
    pub start_time: DateTime<Utc>,
    pub end_time: DateTime<Utc>,
    /// If `Some(false)`, log as non-billable regardless of
    /// project default. `None` inherits.
    pub billable_override: Option<bool>,
}

/// Result of [`TimerService::resolve_rate`].
#[derive(::facet::Facet, serde::Serialize, serde::Deserialize, Clone, Debug, PartialEq, Eq)]
#[repr(C)]
pub struct RateResolution {
    pub hourly_cents: i64,
    pub currency: String,
    /// Which level produced the rate. Useful for the UI
    /// to render "Using project member rate" / "Using org
    /// default" indicators.
    pub source: RateSource,
}

/// Where the resolved rate came from. Mirrors the cascade
/// in [`crate::rate`].
#[derive(
    ::facet::Facet, serde::Serialize, serde::Deserialize, Clone, Copy, Debug, PartialEq, Eq,
)]
#[repr(u8)]
pub enum RateSource {
    /// No billable rate at any level. Session goes
    /// non-billable.
    None = 0,
    /// `ProjectMemberRate` row matched.
    ProjectMember = 1,
    /// Project markdown's `defaultRateCents` field.
    ProjectDefault = 2,
    /// `OrgMemberRate` row matched.
    OrgMember = 3,
}
