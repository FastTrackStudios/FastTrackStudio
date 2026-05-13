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
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ProjectName"))]
    pub name: String,

    #[architect(fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Paragraph(1..3)")
    )]
    pub description: Option<String>,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ProjectStatus"))]
    pub status: String,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ProjectType"))]
    pub project_type: Option<String>,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ColorName"))]
    pub color: Option<String>,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "fake::faker::name::en::Name()"))]
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
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::TaskTitle"))]
    pub title: String,

    #[architect(fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Paragraph(1..3)")
    )]
    pub description: Option<String>,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::TaskStatus"))]
    pub status: String,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::Priority"))]
    pub priority: String,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "fake::faker::name::en::Name()"))]
    pub assignee: Option<String>,

    #[cfg_attr(feature = "fake", dummy(faker = "15i64..480"))]
    pub estimate_minutes: Option<i64>,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::FutureDateTime"))]
    pub due_date: Option<DateTime<Utc>>,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ProjectTags"))]
    pub tags: Vec<String>,

    #[architect(sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "0i64..100"))]
    pub sort_index: i64,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
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
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::CycleName"))]
    pub name: String,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub start_date: DateTime<Utc>,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::FutureDateTime"))]
    pub end_date: DateTime<Utc>,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::CycleStatus"))]
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
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::MilestoneName"))]
    pub name: String,

    #[architect(fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Paragraph(1..3)")
    )]
    pub description: Option<String>,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::FutureDateTime"))]
    pub target_date: Option<DateTime<Utc>>,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
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

#[cfg(feature = "fake")]
pub mod fakers {
    use chrono::{DateTime, Duration, Utc};
    use fake::Dummy;
    use fake::rand::{Rng, seq::IndexedRandom};

    fn pick<R: Rng + ?Sized>(rng: &mut R, values: &[&str]) -> String {
        (*values.choose(rng).unwrap()).to_string()
    }

    pub struct ProjectName;
    impl Dummy<ProjectName> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ProjectName, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "Q4 Launch",
                    "Redesign Marketing Site",
                    "Migrate Auth Service",
                    "Mobile App v2",
                    "Customer Onboarding Revamp",
                    "Analytics Pipeline",
                    "Payments Integration",
                    "Performance Audit",
                    "Album Production",
                    "Sound Design Library",
                    "Studio Build-out",
                    "API Gateway Rollout",
                    "Documentation Refresh",
                    "Holiday Campaign",
                ],
            )
        }
    }

    pub struct ProjectStatus;
    impl Dummy<ProjectStatus> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ProjectStatus, rng: &mut R) -> Self {
            pick(
                rng,
                &["planning", "active", "on-hold", "completed", "archived"],
            )
        }
    }

    pub struct ProjectType;
    impl Dummy<ProjectType> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ProjectType, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "engineering",
                    "design",
                    "marketing",
                    "research",
                    "operations",
                    "audio",
                    "client-work",
                    "internal",
                ],
            )
        }
    }

    pub struct ColorName;
    impl Dummy<ColorName> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ColorName, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "#ef4444", "#f97316", "#eab308", "#22c55e", "#06b6d4", "#3b82f6", "#8b5cf6",
                    "#ec4899", "#64748b",
                ],
            )
        }
    }

    pub struct TaskTitle;
    impl Dummy<TaskTitle> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &TaskTitle, rng: &mut R) -> Self {
            const VERBS: &[&str] = &[
                "Implement",
                "Refactor",
                "Fix",
                "Investigate",
                "Design",
                "Test",
                "Document",
                "Deploy",
                "Migrate",
                "Review",
                "Optimize",
                "Audit",
            ];
            const NOUNS: &[&str] = &[
                "login flow",
                "dashboard layout",
                "payment integration",
                "API rate limiter",
                "onboarding tutorial",
                "search results page",
                "notification system",
                "settings panel",
                "user permissions",
                "data export",
                "session handling",
                "error reporting",
                "cache invalidation",
                "background job queue",
                "webhook delivery",
            ];
            let v = VERBS.choose(rng).unwrap();
            let n = NOUNS.choose(rng).unwrap();
            format!("{} {}", v, n)
        }
    }

    pub struct TaskStatus;
    impl Dummy<TaskStatus> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &TaskStatus, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "todo",
                    "in-progress",
                    "in-review",
                    "blocked",
                    "done",
                    "cancelled",
                ],
            )
        }
    }

    pub struct Priority;
    impl Dummy<Priority> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &Priority, rng: &mut R) -> Self {
            pick(rng, &["low", "medium", "high", "urgent"])
        }
    }

    pub struct CycleName;
    impl Dummy<CycleName> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &CycleName, rng: &mut R) -> Self {
            let kind: &str = ["Sprint", "Cycle", "Iteration"].choose(rng).unwrap();
            let n: u32 = rng.random_range(1..40);
            format!("{} {}", kind, n)
        }
    }

    pub struct CycleStatus;
    impl Dummy<CycleStatus> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &CycleStatus, rng: &mut R) -> Self {
            pick(rng, &["upcoming", "active", "completed"])
        }
    }

    pub struct MilestoneName;
    impl Dummy<MilestoneName> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &MilestoneName, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "Alpha Release",
                    "Beta Release",
                    "Public Launch",
                    "Internal Demo",
                    "Investor Review",
                    "Feature Freeze",
                    "Code Freeze",
                    "Soft Launch",
                    "v1.0",
                    "v2.0",
                ],
            )
        }
    }

    pub struct RecentDateTime;
    impl Dummy<RecentDateTime> for DateTime<Utc> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &RecentDateTime, rng: &mut R) -> Self {
            Utc::now() - Duration::days(rng.random_range(0..365))
        }
    }

    pub struct FutureDateTime;
    impl Dummy<FutureDateTime> for DateTime<Utc> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &FutureDateTime, rng: &mut R) -> Self {
            Utc::now() + Duration::days(rng.random_range(0..90))
        }
    }

    pub struct ProjectTags;
    impl Dummy<ProjectTags> for Vec<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ProjectTags, rng: &mut R) -> Self {
            const POOL: &[&str] = &[
                "urgent",
                "blocked",
                "backend",
                "frontend",
                "design",
                "bug",
                "feature",
                "tech-debt",
                "good-first-issue",
                "infra",
                "docs",
                "perf",
            ];
            let n = rng.random_range(1..=4usize);
            POOL.choose_multiple(rng, n)
                .map(|s| s.to_string())
                .collect()
        }
    }
}
