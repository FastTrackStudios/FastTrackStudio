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

    /// Optional FK to a `project_proto::Project`. Set when the work
    /// rolls up to a project (and on through to its client).
    #[architect(filterable)]
    pub project_id: Option<Uuid>,

    /// Denormalised client id for fast client-scoped filtering.
    /// Written from `Project.client_id` at create time when a
    /// `project_id` is supplied, or set directly for project-less work.
    #[architect(filterable)]
    pub client_id: Option<Uuid>,

    /// Who tracked the time. Free-form for now — promote to FK once
    /// the `person` feature settles.
    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "fake::faker::name::en::Name()"))]
    pub user: Option<String>,

    /// When the timer started. Sortable for "show me today's entries".
    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub start_time: DateTime<Utc>,

    /// `None` while the timer is still running.
    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub end_time: Option<DateTime<Utc>>,

    /// Free-text note about what was done in this window.
    #[architect(fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::TimerDescription"))]
    pub description: Option<String>,

    /// Whether this entry should be invoiced.
    #[architect(filterable)]
    pub billable: bool,

    /// `false` = the timer was started/stopped live; `true` = the
    /// entry was typed into a manual-entry dialog after the fact.
    /// Used for reporting on logged vs. tracked time.
    #[architect(filterable)]
    pub manual: bool,

    /// Per-entry hourly rate override, in cents. Falls back to
    /// project / member / org default when None.
    #[cfg_attr(feature = "fake", dummy(faker = "5000u32..30000"))]
    pub billable_rate_cents: Option<u32>,

    /// Free-form tags for grouping / reporting.
    #[architect(json)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::TimerTags"))]
    pub tags: Vec<String>,

    /// Set once this entry is rolled up into an invoice. Non-None
    /// means "exclude from future invoice generation runs."
    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
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

#[cfg(feature = "fake")]
pub mod fakers {
    use chrono::{DateTime, Duration, Utc};
    use fake::Dummy;
    use fake::rand::{Rng, seq::IndexedRandom};

    pub struct RecentDateTime;
    impl Dummy<RecentDateTime> for DateTime<Utc> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &RecentDateTime, rng: &mut R) -> Self {
            Utc::now() - Duration::days(rng.random_range(0..180))
        }
    }

    pub struct TimerDescription;
    impl Dummy<TimerDescription> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &TimerDescription, rng: &mut R) -> Self {
            const VALUES: &[&str] = &[
                "Code review",
                "Pair programming session",
                "Bug investigation",
                "Sprint planning meeting",
                "Client call",
                "Refactoring auth module",
                "Documentation update",
                "Deploy to staging",
                "Design review",
                "Mixing session",
                "Mastering pass",
                "Email triage",
                "Architecture brainstorm",
                "Test writing",
            ];
            (*VALUES.choose(rng).unwrap()).to_string()
        }
    }

    pub struct TimerTags;
    impl Dummy<TimerTags> for Vec<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &TimerTags, rng: &mut R) -> Self {
            const POOL: &[&str] = &[
                "billable", "internal", "meeting", "coding", "design", "review", "ops", "client",
            ];
            let n = rng.random_range(1..=3usize);
            POOL.choose_multiple(rng, n)
                .map(|s| s.to_string())
                .collect()
        }
    }
}
