//! `timer-proto` — wire contract for the time-tracking feature.
//!
//! `TimeEntry` is a stop/start window of tracked time, optionally
//! linked to a `Task` and rolled up onto an `Invoice`. Each entry is a
//! top-level CRDT row keyed by uuid — concurrent additions from
//! different peers merge cleanly with no conflict resolution work.

pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "time_entries", repo)]
pub struct TimeEntry {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    /// Optional FK to the `Task` this entry belongs to. Top-level so
    /// concurrent peers can add entries without merging into a nested
    /// list inside the task.
    #[architect(filterable)]
    pub task_id: Option<Uuid>,

    /// Who tracked the time. Free-form for now — promote to FK once
    /// the `person` feature settles.
    #[architect(filterable)]
    pub user: Option<String>,

    /// When the timer started. Sortable for "show me today's entries".
    #[architect(filterable, sortable)]
    pub start_time: DateTime<Utc>,

    /// `None` while the timer is still running.
    #[architect(filterable, sortable)]
    pub end_time: Option<DateTime<Utc>>,

    /// Free-text note about what was done in this window.
    #[architect(fulltext)]
    pub description: Option<String>,

    /// Whether this entry should be invoiced.
    #[architect(filterable)]
    pub billable: bool,

    /// Per-entry hourly rate override, in cents. Falls back to
    /// project / member / org default when None.
    pub billable_rate_cents: Option<u32>,

    /// Free-form tags for grouping / reporting.
    pub tags: Vec<String>,

    /// Set once this entry is rolled up into an invoice. Non-None
    /// means "exclude from future invoice generation runs."
    #[architect(filterable)]
    pub invoiced_at: Option<DateTime<Utc>>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum TimerServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait TimerService {
    /// Stop the currently-running entry for `user` (the one with
    /// `end_time = None`) and return the closed entry.
    async fn stop_running(&self, user: String) -> Result<TimeEntry, TimerServiceError>;

    /// Mark a contiguous range of entries as invoiced (set
    /// `invoiced_at`). Returns the affected entry count.
    async fn mark_invoiced(
        &self,
        entry_ids: Vec<Uuid>,
        invoiced_at: DateTime<Utc>,
    ) -> Result<u32, TimerServiceError>;
}
