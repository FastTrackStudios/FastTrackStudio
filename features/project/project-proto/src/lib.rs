//! `project-proto` — wire contract for the project-management feature.
//!
//! Four top-level entities:
//!
//! - `Project`   — the container ("my audio-production project")
//! - `Task`      — work item belonging to a project; can nest via `parent_id`
//! - `Cycle`     — time-boxed iteration ("sprint 5", "Q1 release")
//! - `Milestone` — dated goal within a project
//!
//! Each is a separate `architect::Entity` with its own Repo trait —
//! the scaffolder treats them uniformly (one LoroMap per entity type,
//! UUID-keyed). Hierarchy + ordering (drag-reorder tasks between
//! cycles, nest subtasks) live in `ProjectService` and use LoroTree +
//! LoroMovableList sub-containers; the per-row Repo trait stays
//! conflict-free on field edits.

pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

// ── Project ───────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "projects", repo)]
pub struct Project {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    pub name: String,

    #[architect(fulltext)]
    pub description: Option<String>,

    #[architect(filterable, sortable)]
    pub status: String,

    #[architect(filterable)]
    pub project_type: Option<String>,

    pub color: Option<String>,

    #[architect(filterable)]
    pub owner: Option<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── Task ──────────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "tasks", repo)]
pub struct Task {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub project_id: Uuid,

    #[architect(filterable)]
    pub parent_id: Option<Uuid>,

    #[architect(filterable)]
    pub cycle_id: Option<Uuid>,

    #[architect(filterable, sortable, fulltext)]
    pub title: String,

    #[architect(fulltext)]
    pub description: Option<String>,

    #[architect(filterable, sortable)]
    pub status: String,

    #[architect(filterable, sortable)]
    pub priority: String,

    #[architect(filterable)]
    pub assignee: Option<String>,

    pub estimate_minutes: Option<i64>,

    #[architect(filterable, sortable)]
    pub due_date: Option<DateTime<Utc>>,

    pub tags: Vec<String>,

    #[architect(sortable)]
    pub sort_index: i64,

    #[architect(filterable)]
    pub completed_at: Option<DateTime<Utc>>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── Cycle ─────────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "cycles", repo)]
pub struct Cycle {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub project_id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    pub name: String,

    #[architect(filterable, sortable)]
    pub start_date: DateTime<Utc>,

    #[architect(filterable, sortable)]
    pub end_date: DateTime<Utc>,

    #[architect(filterable)]
    pub status: String,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── Milestone ─────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "milestones", repo)]
pub struct Milestone {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub project_id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    pub name: String,

    #[architect(fulltext)]
    pub description: Option<String>,

    #[architect(filterable, sortable)]
    pub target_date: Option<DateTime<Utc>>,

    #[architect(filterable)]
    pub completed_at: Option<DateTime<Utc>>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── ProjectService ────────────────────────────────────────────────────
//
// Domain operations that don't map onto plain Repo CRUD. Hand-written;
// architect leaves this alone. Implementations in project-crdt drive
// LoroTree (hierarchy) and LoroMovableList (ordering) sub-containers
// for the moves Loro's docs call out as needing dedicated CRDT
// containers to stay conflict-free.

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum ProjectServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait ProjectService {
    /// Move `task_id` to a new position within its sibling list
    /// (same parent + same cycle).
    async fn reorder_task(&self, task_id: Uuid, new_index: u32) -> Result<(), ProjectServiceError>;

    /// Move `task_id` under a new parent.
    async fn reparent_task(
        &self,
        task_id: Uuid,
        new_parent_id: Option<Uuid>,
    ) -> Result<(), ProjectServiceError>;

    /// Mark a task done, optionally cascading to subtasks.
    async fn complete_task(
        &self,
        task_id: Uuid,
        cascade_subtasks: bool,
    ) -> Result<u32, ProjectServiceError>;
}
