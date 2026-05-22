//! Agent tasks — dispatchable units of work owned by an agent.
//!
//! Modeled on hermes-webui's queue (`api/kanban_bridge.py`) but
//! with visualization-neutral naming: an `AgentTask` is a row in
//! a `Queue`, with a fixed `AgentTaskStatus` lifecycle. Kanban is
//! one possible rendering (see `view-kanban`); timeline, agenda,
//! and graph views are future renderings of the same data.
//!
//! The dispatcher owns the `Running` status — clients cannot set
//! it directly. Clients call `claim` (typically the server) and
//! the impl atomically flips status to `Running` if the agent
//! task is free. This matches Hermes' contract and prevents two
//! workers double-claiming the same unit of work.
//!
//! See `plans/agent-dispatch.md` for the full design.

use chrono::{DateTime, Utc};
use facet::Facet;

/// Status lifecycle for an agent task. Fixed across the system —
/// queues don't define their own column sets; the view layer can
/// hide statuses or group them however it wants.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Facet)]
#[repr(u8)]
pub enum AgentTaskStatus {
    /// New, unreviewed. Default for freshly dispatched tasks.
    Triage = 0,
    /// Reviewed + queued, ready for a worker to claim.
    Ready = 1,
    /// A worker has claimed it and is executing.
    /// **Dispatcher-owned** — not settable from the client API.
    Running = 2,
    /// Paused on a dependency or external input.
    Blocked = 3,
    /// Finished successfully. Awaits archive sweep.
    Done = 4,
    /// Soft-deleted. Doesn't show in default queue reads.
    Archived = 5,
}

impl AgentTaskStatus {
    /// Stable slug used in storage + the wire protocol.
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Triage => "triage",
            Self::Ready => "ready",
            Self::Running => "running",
            Self::Blocked => "blocked",
            Self::Done => "done",
            Self::Archived => "archived",
        }
    }

    /// Reverse of [`Self::as_str`].
    #[allow(clippy::should_implement_trait)]
    #[must_use]
    pub fn from_str(s: &str) -> Option<Self> {
        Some(match s {
            "triage" => Self::Triage,
            "ready" => Self::Ready,
            "running" => Self::Running,
            "blocked" => Self::Blocked,
            "done" => Self::Done,
            "archived" => Self::Archived,
            _ => return None,
        })
    }

    /// `true` if a client UI is allowed to transition INTO this
    /// status directly. `Running` returns `false` because the
    /// dispatcher owns the claim.
    #[must_use]
    pub fn client_settable(self) -> bool {
        !matches!(self, Self::Running)
    }
}

/// A queue is a grouping of agent tasks — typically scoped to a
/// project. Each queue has its own claim/archive policy and event
/// stream; queues do not share state.
#[derive(Debug, Clone, PartialEq, Eq, Facet)]
#[repr(C)]
pub struct Queue {
    /// Stable slug. `"default"` is the canonical fallback.
    pub id: String,
    pub label: String,
    /// Known worker handles seen claiming tasks on this queue.
    /// Backends update this opportunistically.
    pub assignees: Vec<String>,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}

/// A snapshot of a queue — agent tasks plus a watermark so
/// subscribers can detect drift.
#[derive(Debug, Clone, PartialEq, Facet)]
#[repr(C)]
pub struct QueueSnapshot {
    pub queue: Queue,
    pub tasks: Vec<AgentTask>,
    /// Latest `queue_events.id` observed when this snapshot
    /// was built. Subscribers pass it back on the event tail
    /// to resume without gaps.
    pub latest_event_id: u64,
    /// Whether the queue is read-only for the caller.
    pub read_only: bool,
}

/// One dispatchable unit of agent work.
#[derive(Debug, Clone, PartialEq, Facet)]
#[repr(C)]
pub struct AgentTask {
    pub id: String,
    pub queue_id: String,
    pub title: String,
    /// Markdown body — instructions to the agent.
    pub description: String,
    pub status: AgentTaskStatus,
    /// 0 = highest, 4 = lowest. Matches Hermes' P0..P4.
    pub priority: u8,
    pub labels: Vec<String>,
    /// Vault-relative path of the source task note, if this
    /// agent task was dispatched from one. Empty otherwise.
    pub source_task: String,
    /// Vault-relative path of the project note this agent task
    /// belongs to. Empty if standalone.
    pub project: String,
    /// Which backend should claim this. Empty = any. Backends
    /// self-select by matching against their configured profile.
    pub agent_profile: String,
    /// Worker handle currently holding the claim. Empty = free.
    pub claimed_by: String,
    /// When the claim expires. Dispatchers reclaim past this.
    pub claim_expires_at: Option<DateTime<Utc>>,
    /// PID of the worker process, when known. 0 = unknown.
    pub worker_pid: u32,
    /// Transcript / output blob attached at completion.
    /// Empty until `complete` is called.
    pub result_blob: String,
    /// Optional session this agent task is being worked in.
    /// Empty when no session is linked.
    pub linked_session_id: String,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
    pub archived_at: Option<DateTime<Utc>>,
}

/// Kind of edge between two agent tasks.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Facet)]
#[repr(u8)]
pub enum AgentTaskLinkKind {
    /// The `from` task blocks the `to` task — `to` can't move
    /// to Done until `from` is Done.
    Blocks = 0,
    /// Loose relationship — surfaced in the UI but not enforced.
    Related = 1,
    /// `to` was created as a follow-up to `from`.
    SpawnedFrom = 2,
}

#[derive(Debug, Clone, PartialEq, Eq, Facet)]
#[repr(C)]
pub struct AgentTaskLink {
    pub from_task: String,
    pub to_task: String,
    pub kind: AgentTaskLinkKind,
    pub created_at: DateTime<Utc>,
}

/// One comment on an agent task. Append-only; edits land as new
/// comments.
#[derive(Debug, Clone, PartialEq, Eq, Facet)]
#[repr(C)]
pub struct AgentTaskComment {
    pub id: String,
    pub agent_task_id: String,
    /// `"user"` for human comments, or the agent profile name
    /// for agent-emitted comments.
    pub author: String,
    pub body: String,
    pub created_at: DateTime<Utc>,
}

/// Filter applied when reading a queue snapshot.
#[derive(Debug, Clone, PartialEq, Eq, Facet)]
#[repr(C)]
pub struct QueueFilter {
    /// Restrict to a specific assignee. Empty = all.
    pub assignee: String,
    /// Include archived agent tasks.
    pub include_archived: bool,
    /// `"only_mine"` shortcut — caller's handle (resolved
    /// by the backend). Empty = no filter.
    pub only_handle: String,
    /// Restrict to agent tasks linked to a session.
    pub linked_session_id: String,
    /// Restrict to a specific agent profile. Empty = all.
    pub agent_profile: String,
}
