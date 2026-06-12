//! `TaskInfo` data model.
//!
//! Mirrors `callumalpass/tasknotes`'s `TaskInfo` interface
//! (`src/types.ts:453`) so existing `TaskNotes` vaults round-trip.
//! Field names are the same — `due` not `dueDate`, `scheduled`
//! not `start`, `timeEntries` not `time_entries`. Configurable
//! field-mapping (à la `TaskNotes`' `FieldMapper`) is a future
//! addition; v1 fixes the names to `TaskNotes` defaults.

use chrono::{DateTime, NaiveDate, Utc};
use facet::Facet;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// `Vec<String>` newtype — JSON column. Used for several
/// list fields (tags, contexts, projects, etc.).
#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(
    architect::JsonField, Debug, Clone, Default, PartialEq, Eq, Facet, Serialize, Deserialize,
)]
#[repr(transparent)]
#[serde(transparent)]
pub struct StringList(pub Vec<String>);

impl StringList {
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl From<Vec<String>> for StringList {
    fn from(v: Vec<String>) -> Self {
        Self(v)
    }
}

impl FromIterator<String> for StringList {
    fn from_iter<I: IntoIterator<Item = String>>(iter: I) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl std::ops::Deref for StringList {
    type Target = Vec<String>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for StringList {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// `Vec<TimeEntry>` newtype — JSON column. Time entries live
/// inline in the task page. If per-entry queries get hot,
/// promote `TimeEntry` to its own entity later.
#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(architect::JsonField, Debug, Clone, Default, PartialEq, Facet, Serialize, Deserialize)]
#[repr(transparent)]
#[serde(transparent)]
pub struct TimeEntries(pub Vec<TimeEntry>);

impl TimeEntries {
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl From<Vec<TimeEntry>> for TimeEntries {
    fn from(v: Vec<TimeEntry>) -> Self {
        Self(v)
    }
}

impl FromIterator<TimeEntry> for TimeEntries {
    fn from_iter<I: IntoIterator<Item = TimeEntry>>(iter: I) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl std::ops::Deref for TimeEntries {
    type Target = Vec<TimeEntry>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// One task. Most fields are optional — the discriminator is
/// usually just `tags: [..., task]` or `type: task` on the
/// page. `id` is added on first read for any task page that
/// doesn't yet have one (parser backfill via
/// `Uuid::new_v5(NAMESPACE_URL, path)` so the same path
/// always resolves to the same id across machines until the
/// next save writes it to frontmatter).
#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(architect::Entity, Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
#[architect(table_name = "tasks", repo)]
pub struct TaskInfo {
    /// Stable identity. Generated on first write; never
    /// re-derived from the path. Downstream features (timer,
    /// agent-dispatch) reference tasks by this UUID, so
    /// renaming the markdown file doesn't orphan rows.
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    /// Vault-relative path of the markdown file backing this
    /// task (e.g. `tasks/buy-milk.md`). Populated by the
    /// scanner; not serialized into frontmatter. Persisted as
    /// a DB column under server so reverse lookups work.
    #[serde(skip)]
    #[architect(filterable, sortable)]
    pub path: String,

    #[architect(filterable, sortable, fulltext)]
    pub title: String,

    /// `"open"` / `"in-progress"` / `"done"` etc. Free-form so
    /// custom statuses (e.g. `"waiting"`, `"blocked"`) round-trip.
    /// Parsing prefers the [`Status`] enum but stores the raw
    /// string so unknown values survive.
    #[architect(filterable, sortable)]
    pub status: String,

    /// `"none"` / `"low"` / `"normal"` / `"high"` / `"critical"`
    /// — free-form like `status`.
    #[serde(default = "default_priority")]
    #[architect(filterable, sortable)]
    pub priority: String,

    /// Due date (YYYY-MM-DD or full ISO timestamp).
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub due: Option<String>,

    /// When you plan to work on it (YYYY-MM-DD or ISO).
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub scheduled: Option<String>,

    /// Frontmatter `tags:` array. `"task"` is the conventional
    /// discriminator; everything else is user-defined.
    #[serde(skip_serializing_if = "StringList::is_empty", default)]
    #[architect(json)]
    pub tags: StringList,

    /// GTD-style contexts (e.g. `"@shopping"`, `"@dev"`).
    #[serde(skip_serializing_if = "StringList::is_empty", default)]
    #[architect(json)]
    pub contexts: StringList,

    /// Project wikilinks (e.g. `"[[Website Redesign]]"`).
    /// Stored verbatim — the wikilink-to-page resolution lives in
    /// `vault-obsidian::links`.
    #[serde(skip_serializing_if = "StringList::is_empty", default)]
    #[architect(json)]
    pub projects: StringList,

    /// Owning project (stable UUID). Authoritative pointer
    /// — the `projects:` wikilink array above stays as a
    /// human-readable hint, but downstream code (timer
    /// project-defaults, kanban grouping, agent dispatch
    /// scoping) reads `project_id` first. `None` when the
    /// task is not yet associated to a project.
    #[serde(skip_serializing_if = "Option::is_none", default, rename = "projectId")]
    #[architect(filterable)]
    pub project_id: Option<Uuid>,

    /// Milestone this task rolls up to. Optional. A milestone
    /// is project-scoped, so `milestone_id` implies
    /// `project_id` (the backend doesn't currently enforce
    /// it; the CLI's `set-milestone` does). See the
    /// `milestone` crate for the rollup contract.
    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "milestoneId"
    )]
    #[architect(filterable)]
    pub milestone_id: Option<Uuid>,

    /// Estimated work in minutes.
    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "timeEstimate"
    )]
    pub time_estimate: Option<u32>,

    /// One row per work session. Append-only; reading + summing
    /// is the consumer's job.
    #[serde(
        skip_serializing_if = "TimeEntries::is_empty",
        default,
        rename = "timeEntries"
    )]
    #[architect(json)]
    pub time_entries: TimeEntries,

    /// RFC 5545 RRULE (e.g. `"FREQ=WEEKLY;BYDAY=MO"`).
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub recurrence: Option<String>,

    /// `"scheduled"` (fixed) or `"completion"` (flexible). Only
    /// meaningful when `recurrence` is set.
    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "recurrence_anchor"
    )]
    pub recurrence_anchor: Option<String>,

    /// YYYY-MM-DD dates when each recurrence instance completed.
    #[serde(
        skip_serializing_if = "StringList::is_empty",
        default,
        rename = "complete_instances"
    )]
    #[architect(json)]
    pub complete_instances: StringList,

    /// YYYY-MM-DD when this task moved to `status: done`.
    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "completedDate"
    )]
    pub completed_date: Option<NaiveDate>,

    /// `agent-dispatch`: which agent profile (codex, hermes, …)
    /// is allowed to claim this task when it's dispatched to an
    /// agent queue. Empty = any. Setting this on a task note
    /// makes it *eligible* for agent dispatch — the actual
    /// dispatch is an explicit user action (or the recurring-
    /// task cron in `agent-dispatch::schedule_recurring`).
    #[serde(
        skip_serializing_if = "String::is_empty",
        default,
        rename = "agentProfile"
    )]
    pub agent_profile: String,

    /// `agent-dispatch`: stable ids of agent tasks dispatched
    /// from this task note. Each entry is a UUID; the
    /// corresponding row lives in
    /// `agent_tasks` on the server. `agent-dispatch::dispatch`
    /// appends to this list when it creates a card, and
    /// `complete_agent_task` doesn't remove from it — the list
    /// is an audit trail of every dispatch over the task's
    /// lifetime.
    #[serde(
        skip_serializing_if = "StringList::is_empty",
        default,
        rename = "dispatchedAgentTasks"
    )]
    #[architect(json)]
    pub dispatched_agent_tasks: StringList,

    /// File-created ISO timestamp. Re-derived from `file.ctime`
    /// when missing — kept in the frontmatter so it round-trips
    /// across machines (mtime / ctime is per-filesystem).
    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "dateCreated"
    )]
    pub date_created: Option<DateTime<Utc>>,

    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "dateModified"
    )]
    pub date_modified: Option<DateTime<Utc>>,

    /// Body text (the markdown after the frontmatter close
    /// fence). Populated by `parse_page`; written verbatim by
    /// `serialize_task`.
    #[serde(skip)]
    pub details: String,

    /// Linear-style workflow attributes. `None` for tasks that
    /// aren't part of a tracked workflow (the default — keeps
    /// existing TaskNotes-shape vaults round-tripping). Set
    /// when the task is linked to a workspace / cycle / project,
    /// or claimed by an agent.
    ///
    /// Serialized under the `workflow:` frontmatter key as a
    /// nested mapping so the addition stays scoped (rather than
    /// scattering ~8 new top-level keys into every task page).
    #[serde(skip_serializing_if = "Option::is_none", default)]
    #[architect(json)]
    pub workflow: Option<WorkflowAttrs>,
}

fn default_priority() -> String {
    "normal".to_string()
}

/// Single time-tracking session. `endTime` is `None` while the
/// timer is running.
#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct TimeEntry {
    #[serde(rename = "startTime")]
    pub start_time: DateTime<Utc>,
    #[serde(skip_serializing_if = "Option::is_none", default, rename = "endTime")]
    pub end_time: Option<DateTime<Utc>>,
}

/// Built-in status values. Parsing accepts any string;
/// these are the recognized canonical forms.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Status {
    Open,
    InProgress,
    Done,
    Cancelled,
    Waiting,
}

impl Status {
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Open => "open",
            Self::InProgress => "in-progress",
            Self::Done => "done",
            Self::Cancelled => "cancelled",
            Self::Waiting => "waiting",
        }
    }

    /// Parse the canonical status set. Returns `None` for
    /// unknown statuses — callers keep the raw string.
    #[allow(clippy::should_implement_trait)]
    #[must_use]
    pub fn from_str(s: &str) -> Option<Self> {
        match s.trim().to_ascii_lowercase().as_str() {
            "open" | "todo" | "none" => Some(Self::Open),
            "in-progress" | "in_progress" | "doing" => Some(Self::InProgress),
            "done" | "completed" | "complete" => Some(Self::Done),
            "cancelled" | "canceled" => Some(Self::Cancelled),
            "waiting" | "blocked" => Some(Self::Waiting),
            _ => None,
        }
    }

    #[must_use]
    pub fn is_done(self) -> bool {
        matches!(self, Self::Done)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Priority {
    None,
    Low,
    Normal,
    High,
    Critical,
}

impl Priority {
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::None => "none",
            Self::Low => "low",
            Self::Normal => "normal",
            Self::High => "high",
            Self::Critical => "critical",
        }
    }

    #[allow(clippy::should_implement_trait)]
    #[must_use]
    pub fn from_str(s: &str) -> Option<Self> {
        match s.trim().to_ascii_lowercase().as_str() {
            "none" | "" => Some(Self::None),
            "low" => Some(Self::Low),
            "normal" | "medium" | "med" => Some(Self::Normal),
            "high" => Some(Self::High),
            "critical" | "urgent" => Some(Self::Critical),
            _ => None,
        }
    }
}

// ---- Linear-style workflow attributes ----------------------------------

/// T-shirt or numeric estimate. Stored alongside other workflow
/// attributes inside [`WorkflowAttrs`]; the size enum gives you
/// stable bucketed reporting (XS/S/M/L/XL), the points variant
/// supports teams that prefer fibonacci-style numbers.
#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Facet)]
#[serde(rename_all = "snake_case", tag = "size")]
#[repr(u8)]
pub enum Estimate {
    XS,
    S,
    M,
    L,
    XL,
    Points { value: u8 },
}

/// `Vec<Uuid>` newtype for storing related-task / blocker lists
/// in a JSON column. Same pattern as [`StringList`].
#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(
    architect::JsonField, Debug, Clone, Default, PartialEq, Eq, Facet, Serialize, Deserialize,
)]
#[repr(transparent)]
#[serde(transparent)]
pub struct UuidList(pub Vec<Uuid>);

impl UuidList {
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl From<Vec<Uuid>> for UuidList {
    fn from(v: Vec<Uuid>) -> Self {
        Self(v)
    }
}

impl std::ops::Deref for UuidList {
    type Target = Vec<Uuid>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for UuidList {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Typed relation kinds (Plane / Linear parity). Direction is
/// always *source → target* read as "`source` `<kind>`s
/// `target`": `blocks` = source blocks target; `duplicate` =
/// source duplicates target (target is canonical); `implements`
/// = source implements target (target is the spec / PRD);
/// `relates` = soft symmetric link.
#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Facet)]
#[serde(rename_all = "lowercase")]
#[repr(u8)]
pub enum RelationKind {
    Blocks,
    Duplicate,
    Implements,
    Relates,
}

impl RelationKind {
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Blocks => "blocks",
            Self::Duplicate => "duplicate",
            Self::Implements => "implements",
            Self::Relates => "relates",
        }
    }

    /// Parse a kind slug (alias-tolerant: `duplicates`,
    /// `relates-to`, `implement`, …).
    #[allow(clippy::should_implement_trait)]
    #[must_use]
    pub fn from_str(s: &str) -> Option<Self> {
        Some(match s.trim().to_ascii_lowercase().replace('_', "-").as_str() {
            "blocks" | "block" => Self::Blocks,
            "duplicate" | "duplicates" | "dup" => Self::Duplicate,
            "implements" | "implement" => Self::Implements,
            "relates" | "relates-to" | "related" => Self::Relates,
            _ => return None,
        })
    }
}

/// One typed edge from the carrying task to `target`. See
/// [`RelationKind`] for direction semantics.
#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Facet)]
pub struct Relation {
    pub kind: RelationKind,
    pub target: Uuid,
}

/// `Vec<Relation>` newtype — JSON column. Same pattern as
/// [`UuidList`].
#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(
    architect::JsonField, Debug, Clone, Default, PartialEq, Eq, Facet, Serialize, Deserialize,
)]
#[repr(transparent)]
#[serde(transparent)]
pub struct RelationList(pub Vec<Relation>);

impl RelationList {
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl From<Vec<Relation>> for RelationList {
    fn from(v: Vec<Relation>) -> Self {
        Self(v)
    }
}

impl std::ops::Deref for RelationList {
    type Target = Vec<Relation>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for RelationList {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// `Vec<AgentRef>` newtype — JSON column. Lets a task be
/// owned by N actors (human + agent + agent triple is common
/// for human-supervised agent work).
#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(architect::JsonField, Debug, Clone, Default, PartialEq, Facet, Serialize, Deserialize)]
#[repr(transparent)]
#[serde(transparent)]
pub struct AgentRefList(pub Vec<workflows_proto::AgentRef>);

impl AgentRefList {
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl From<Vec<workflows_proto::AgentRef>> for AgentRefList {
    fn from(v: Vec<workflows_proto::AgentRef>) -> Self {
        Self(v)
    }
}

impl std::ops::Deref for AgentRefList {
    type Target = Vec<workflows_proto::AgentRef>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Linear-style work attributes. Nested under a single
/// `workflow:` frontmatter key on `TaskInfo` so the surface
/// stays scoped (and so TaskNotes-shape vaults without this
/// key keep round-tripping).
///
/// Every field is optional + skip-if-empty so an empty
/// `WorkflowAttrs` serializes to `{}` (or is omitted entirely
/// when `TaskInfo::workflow` is `None`).
#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(architect::JsonField, Debug, Clone, Default, PartialEq, Facet, Serialize, Deserialize)]
pub struct WorkflowAttrs {
    // NOTE: no `workspace` here. The grouping role workspace
    // played folds into the org-level Project — tasks group via
    // `TaskInfo::project_id`, repos bind to a Project via
    // RepoBinding, and the triage queue is just
    // `tasks in project where status = triage`. There is no
    // separate Workspace entity.
    /// Active cycle / sprint. `None` = un-scoped / backlog.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub cycle: Option<Uuid>,

    /// Owning workstream — the parent-with-swarm construct
    /// (`workstream::Workstream`) this task rolls up into.
    /// `None` = unattached (the default; absent in frontmatter
    /// and on the wire, so existing pages keep round-tripping).
    /// Orthogonal to `parent` (structural sub-task link): a
    /// subtask can carry both.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub workstream: Option<Uuid>,

    // NOTE: project membership is NOT here — it lives on
    // `TaskInfo::project_id`, which already points at the
    // org-level Project (= the Linear-sense Project, now that
    // we collapsed the two). Don't reintroduce a `project`
    // field here; it would duplicate `project_id`.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub estimate: Option<Estimate>,

    /// Parent task this is a subtask of. The triage workflow
    /// breaks one issue into N agent-sized subtasks, each with
    /// `parent` set to the issue's task id. A parent is
    /// considered done when every subtask closes. Distinct from
    /// `blockers`: parent is structural ("part of"), blockers
    /// are ordering ("can't start until").
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub parent: Option<Uuid>,

    /// Who owns this task. For subtasks under parallel-agent
    /// work this is the *claim*: an empty list means unclaimed
    /// and available; a single agent means claimed. Human +
    /// agent + agent triple is the supervised pattern.
    #[serde(skip_serializing_if = "AgentRefList::is_empty", default)]
    pub assignees: AgentRefList,

    /// Hard dependencies — this task is blocked until each
    /// listed task closes. Sub-issue / parent relations are
    /// modelled the same way (parent task blocks children's
    /// completion implicitly via the workflow rules).
    ///
    /// **Legacy encoding** alongside [`Self::relations`]: an
    /// entry `b` here is equivalent to `b` carrying
    /// `Relation { kind: Blocks, target: <this task> }`. The
    /// graph resolver (`crate::relations`) merges both views;
    /// keep writing whichever is convenient.
    #[serde(skip_serializing_if = "UuidList::is_empty", default)]
    pub blockers: UuidList,

    /// Soft links (e.g. "see also"). No completion enforcement.
    ///
    /// **Legacy encoding** alongside [`Self::relations`]: an
    /// entry here is equivalent to a
    /// `Relation { kind: Relates, target }` on this task. The
    /// graph resolver (`crate::relations`) merges both views.
    #[serde(skip_serializing_if = "UuidList::is_empty", default)]
    pub relates_to: UuidList,

    /// Typed relations to other tasks — `blocks` / `duplicate`
    /// / `implements` / `relates`, direction *this task →
    /// target* (see [`RelationKind`]). Coexists with the legacy
    /// [`Self::blockers`] / [`Self::relates_to`] lists: those
    /// keep working, and `crate::relations` merges both
    /// encodings into one edge set for queries / rollups.
    #[serde(skip_serializing_if = "RelationList::is_empty", default)]
    pub relations: RelationList,

    /// Currently-active `WorkSession` id from `workflows-proto`,
    /// if work is in progress. Cleared on Finish/Cancel.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub session: Option<Uuid>,
}
