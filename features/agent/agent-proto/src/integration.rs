//! Public API every agent-integration plugin implements.
//!
//! Phase A scaffolding: no live HTTP, no UI. Concrete plugins
//! (`agent-hermes`, `agent-claude-code`, `agent-mock`) live in
//! sibling crates and register themselves with the
//! `IntegrationRegistry` at server boot.
//!
//! Server-only. Wasm clients should never instantiate `EventSink` or
//! `IntegrationRegistry` — they only ever see the wire types this
//! crate exports.

use async_trait::async_trait;
use chrono::{DateTime, Utc};
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use uuid::Uuid;

use crate::AgentRun;

// ── Capabilities ──────────────────────────────────────────────────────

#[derive(Default, Clone, Copy, Debug, PartialEq, Eq)]
pub struct IntegrationCaps {
    pub can_cancel: bool,
    pub can_stream_logs: bool,
    pub can_resume: bool,
    pub two_way_sync: bool,
}

// ── Strongly-typed status ─────────────────────────────────────────────

/// Strongly-typed mirror of `AgentRun.status`. Plugins return this in
/// `RunSnapshot`; the EventSink translates it to the wire-string
/// when patching the doc.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AgentRunStatus {
    Queued,
    Running,
    Blocked,
    Succeeded,
    Failed,
    Cancelled,
    Archived,
}

impl AgentRunStatus {
    /// Lowercase string used on the wire / in the CRDT.
    pub fn as_str(&self) -> &'static str {
        match self {
            AgentRunStatus::Queued => "queued",
            AgentRunStatus::Running => "running",
            AgentRunStatus::Blocked => "blocked",
            AgentRunStatus::Succeeded => "succeeded",
            AgentRunStatus::Failed => "failed",
            AgentRunStatus::Cancelled => "cancelled",
            AgentRunStatus::Archived => "archived",
        }
    }

    /// Inverse of `as_str`. Unknown / unrecognised statuses fall back
    /// to `Queued` rather than failing — older runs in the doc may
    /// carry strings this enum doesn't know.
    pub fn parse(s: &str) -> Self {
        match s {
            "running" => AgentRunStatus::Running,
            "blocked" => AgentRunStatus::Blocked,
            "succeeded" | "completed" | "done" => AgentRunStatus::Succeeded,
            "failed" | "error" => AgentRunStatus::Failed,
            "cancelled" | "canceled" => AgentRunStatus::Cancelled,
            "archived" => AgentRunStatus::Archived,
            _ => AgentRunStatus::Queued,
        }
    }
}

// ── DTOs the trait deals in ───────────────────────────────────────────

/// Small redacted Task projection. `agent-proto` deliberately avoids
/// a hard dep on `project-proto` to keep the proto crate graph
/// acyclic; the caller (server) builds this from the real Task at
/// dispatch time.
#[derive(Clone, Debug)]
pub struct TaskSummary {
    pub id: Uuid,
    pub project_id: Option<Uuid>,
    pub title: String,
    pub description: Option<String>,
    pub priority: Option<String>,
    pub tags: Vec<String>,
    /// Pre-computed via `project_proto::branch_slug(...)`. The plugin
    /// can use this to name PR branches; it's stable across dispatch
    /// retries.
    pub branch_name: Option<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum WorkspaceKind {
    Scratch,
    Worktree,
    Dir,
}

#[derive(Clone, Debug)]
pub struct WorkspaceHint {
    pub kind: WorkspaceKind,
    pub path: Option<String>,
    pub git_repo_owner: Option<String>,
    pub git_repo_name: Option<String>,
    pub git_base_branch: Option<String>,
}

#[derive(Clone, Debug)]
pub struct DispatchRequest {
    pub task: TaskSummary,
    pub agent_kind: String,
    pub prompt: String,
    pub workspace_hint: Option<WorkspaceHint>,
    /// Caller-chosen `AgentRun.id`. The plugin pins its returned
    /// `DispatchAck.run.id` to this so the server can reconcile
    /// retried dispatches without duplicating runs.
    pub idempotency_key: Uuid,
}

#[derive(Clone, Debug)]
pub struct DispatchAck {
    pub external_id: String,
    pub external_url: Option<String>,
    /// status=Queued, `integration` + `external_id` set, `id` ==
    /// `DispatchRequest.idempotency_key`.
    pub run: AgentRun,
}

#[derive(Clone, Debug)]
pub struct CommitRefDto {
    pub sha: String,
    pub message: String,
    pub url: Option<String>,
    pub authored_at: DateTime<Utc>,
}

#[derive(Clone, Debug)]
pub struct RunSnapshot {
    pub status: AgentRunStatus,
    pub started_at: Option<DateTime<Utc>>,
    pub completed_at: Option<DateTime<Utc>>,
    pub result_summary: Option<String>,
    pub error: Option<String>,
    pub tokens_used: Option<u32>,
    pub cost_cents: Option<u32>,
    pub branch_name: Option<String>,
    pub pr_urls: Vec<String>,
    pub commit_refs: Vec<CommitRefDto>,
}

#[derive(Clone, Debug)]
pub struct AgentLogLineDto {
    pub at: DateTime<Utc>,
    /// One of `crate::AGENT_LOG_LEVELS`.
    pub level: String,
    pub source: String,
    pub text: String,
    pub external_event_id: Option<i64>,
}

// ── Errors ────────────────────────────────────────────────────────────

#[derive(thiserror::Error, Debug)]
pub enum IntegrationError {
    #[error("auth: {0}")]
    Auth(String),
    /// Network errors are stringified at the call boundary so this
    /// crate stays dep-light (no `reqwest`).
    #[error("network: {0}")]
    Network(String),
    #[error("not found")]
    NotFound,
    #[error("conflict: {0}")]
    Conflict(String),
    #[error("bad request: {0}")]
    BadRequest(String),
    #[error("upstream {status}: {body}")]
    Upstream { status: u16, body: String },
    #[error("decode: {0}")]
    Decode(String),
    #[error("disabled")]
    Disabled,
}

// ── Event sink (server-side) ──────────────────────────────────────────

/// Plugin-facing abstraction over "write into the doc". The server
/// constructs an `EventSink` that wraps its `AgentRunRepoLoro` +
/// `AgentLogLineRepoLoro` + a callback into `project-proto`'s repo
/// for `maybe_mirror_task_status`. Plugins use it without depending
/// on a specific Repo type.
#[derive(Clone)]
pub struct EventSink {
    pub inner: Arc<dyn EventSinkImpl>,
}

#[async_trait]
pub trait EventSinkImpl: Send + Sync + 'static {
    /// Apply a snapshot to an in-flight run. Idempotent — the sink
    /// merges fields, doesn't replace.
    async fn patch_run(&self, run_id: Uuid, snapshot: RunSnapshot) -> Result<(), IntegrationError>;

    /// Append a log line. Implementations dedupe on
    /// `external_event_id` to avoid double-writes during resume.
    async fn append_log(&self, run_id: Uuid, line: AgentLogLineDto)
    -> Result<(), IntegrationError>;

    /// Best-effort task status mirror. Called when an inbound event
    /// indicates the user's task status should change (e.g. Done on
    /// success). Sink implementations are free to no-op if the user
    /// has opted out.
    async fn maybe_mirror_task_status(
        &self,
        task_id: Uuid,
        new_status: &str,
    ) -> Result<(), IntegrationError>;
}

// ── Shutdown signal ───────────────────────────────────────────────────

#[derive(Clone, Default)]
pub struct ShutdownSignal {
    pub flag: Arc<AtomicBool>,
}

impl ShutdownSignal {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn is_set(&self) -> bool {
        self.flag.load(Ordering::SeqCst)
    }
    pub fn set(&self) {
        self.flag.store(true, Ordering::SeqCst);
    }
}

// ── The trait ─────────────────────────────────────────────────────────

#[async_trait]
pub trait AgentIntegration: Send + Sync + 'static {
    /// Stable plugin key. Used both as the `IntegrationRegistry`
    /// index and the value stored in `AgentRun.integration`.
    fn name(&self) -> &'static str;

    fn capabilities(&self) -> IntegrationCaps;

    async fn dispatch(&self, req: DispatchRequest) -> Result<DispatchAck, IntegrationError>;

    async fn cancel(&self, external_id: &str) -> Result<(), IntegrationError>;

    async fn fetch_run(&self, external_id: &str) -> Result<RunSnapshot, IntegrationError>;

    async fn fetch_logs(
        &self,
        external_id: &str,
        after: Option<DateTime<Utc>>,
    ) -> Result<Vec<AgentLogLineDto>, IntegrationError>;

    /// Long-running loop; the registry spawns this once at boot.
    /// Plugins push inbound state changes through `sink` which
    /// writes to the Loro doc. Exits cleanly when `shutdown` is
    /// set.
    async fn run_event_loop(
        &self,
        sink: EventSink,
        shutdown: ShutdownSignal,
    ) -> Result<(), IntegrationError>;
}

// ── Registry ──────────────────────────────────────────────────────────

#[derive(Default, Clone)]
pub struct IntegrationRegistry {
    plugins: HashMap<&'static str, Arc<dyn AgentIntegration>>,
}

impl IntegrationRegistry {
    pub fn new() -> Self {
        Self {
            plugins: HashMap::new(),
        }
    }

    /// Keyed by `p.name()`. Last write wins — registering a plugin
    /// with the same name twice replaces the previous one (handy in
    /// tests).
    pub fn register(&mut self, p: Arc<dyn AgentIntegration>) {
        self.plugins.insert(p.name(), p);
    }

    pub fn get(&self, name: &str) -> Option<Arc<dyn AgentIntegration>> {
        self.plugins.get(name).cloned()
    }

    pub fn names(&self) -> Vec<&'static str> {
        self.plugins.keys().copied().collect()
    }

    pub fn all(&self) -> Vec<Arc<dyn AgentIntegration>> {
        self.plugins.values().cloned().collect()
    }
}
