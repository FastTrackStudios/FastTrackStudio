pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

pub mod chat_model;
pub mod integration;

pub use chat_model::{
    ChatModel, ChatModelRegistry, ChatStreamChunk, ChatStreamRequest, ChatTurn, ToolSchema,
};

/// Canonical log levels emitted on `AgentLogLine`. Plugins SHOULD
/// pick from this set; unknown values are stored verbatim and may
/// render as "info" in the UI.
pub const AGENT_LOG_LEVELS: &[&str] = &["info", "tool", "stdout", "stderr", "error"];

/// Git provider keys recognised by `GitRepoConnection.provider`.
/// Unknown values are accepted but routes default to the no-op
/// branch in webhook handlers.
pub const GIT_PROVIDERS: &[&str] = &["github", "gitlab", "forgejo"];

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "agent_runs", repo)]
pub struct AgentRun {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::AgentRunName"))]
    pub name: String,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::AgentKind"))]
    pub kind: String,

    #[architect(fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Paragraph(1..3)")
    )]
    pub prompt: String,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::AgentStatus"))]
    pub status: String,

    #[architect(filterable)]
    pub task_id: Option<Uuid>,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub started_at: Option<DateTime<Utc>>,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub completed_at: Option<DateTime<Utc>>,

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Paragraph(1..3)")
    )]
    pub result: Option<String>,

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Sentence(3..10)")
    )]
    pub error_message: Option<String>,

    #[cfg_attr(feature = "fake", dummy(faker = "500u32..200_000"))]
    pub tokens_used: Option<u32>,

    #[cfg_attr(feature = "fake", dummy(faker = "1u32..500"))]
    pub cost_cents: Option<u32>,

    #[architect(json)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::AgentTags"))]
    pub tags: Vec<String>,

    // ── MVP additions per features/agent/spec/agent.md ──────────────
    /// Run lineage: parent for reruns / forks / continuations.
    #[architect(filterable)]
    pub parent_run_id: Option<Uuid>,

    /// Filesystem path the agent operates in (CWD). None for web-only
    /// agents that don't bind to a worktree.
    pub worktree_path: Option<String>,

    /// Optional git remote binding for PR creation et al.
    #[architect(filterable)]
    pub git_repo_connection_id: Option<Uuid>,

    /// When the run was spawned from a conversation message.
    #[architect(filterable)]
    pub spawned_from_message_id: Option<Uuid>,

    // ── Token + cost tracking ──────────────────────────────────────
    #[cfg_attr(feature = "fake", dummy(faker = "500u32..200_000"))]
    pub input_tokens: Option<u32>,
    #[cfg_attr(feature = "fake", dummy(faker = "500u32..200_000"))]
    pub output_tokens: Option<u32>,
    #[cfg_attr(feature = "fake", dummy(faker = "0u32..50_000"))]
    pub cache_read_tokens: Option<u32>,
    #[cfg_attr(feature = "fake", dummy(faker = "0u32..50_000"))]
    pub cache_creation_tokens: Option<u32>,

    /// Cost estimate in cents (i64 — signed for refunds / credit).
    /// `cost_cents` above is the legacy u32; this is the new field
    /// the service maintains going forward.
    #[architect(sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "1i64..50_000"))]
    pub cost_cents_estimate: Option<i64>,

    // ── Denormalized counters ──────────────────────────────────────
    #[cfg_attr(feature = "fake", dummy(faker = "0u32..200"))]
    pub tool_call_count: u32,
    #[cfg_attr(feature = "fake", dummy(faker = "0u32..200"))]
    pub assistant_message_count: u32,

    // ── Resource limits ────────────────────────────────────────────
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::MaybeTokenLimit"))]
    pub max_tokens: Option<u64>,
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::MaybeToolLimit"))]
    pub max_tool_calls: Option<u32>,
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::MaybeWallSeconds"))]
    pub max_wall_seconds: Option<u32>,

    // ── Integration plugin extensions ────────────────────────────────
    //
    // Added in the Hermes-agent arc so AgentRun can carry pointers
    // back to the external system that executed it (Hermes,
    // claude-code, mock). All optional so existing seeds and
    // local-only runs still work unchanged.
    /// Plugin name — keys the IntegrationRegistry. Examples: "hermes",
    /// "claude-code", "mock". `None` for runs that don't go through
    /// a plugin (purely local LLM call, etc.).
    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::MaybeIntegration"))]
    pub integration: Option<String>,

    /// The plugin's external task id (e.g. Hermes task id). Pair with
    /// `integration` to look up provenance in the upstream system.
    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::MaybeExternalId"))]
    pub external_id: Option<String>,

    /// Direct link the UI can open ("Open in Hermes" button). May be
    /// `None` even when `external_id` is set — some plugins don't
    /// expose a stable URL.
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::MaybeExternalUrl"))]
    pub external_url: Option<String>,

    /// Resume cursor for the upstream event loop. The plugin advances
    /// this each time it processes a new event so a restart can pick
    /// up where it left off without re-emitting duplicates.
    pub log_cursor: Option<i64>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── AgentLogLine ──────────────────────────────────────────────────────
//
// Logs are separated from `AgentRun` because they're too chatty for
// the main doc: a single Hermes run can produce thousands of lines,
// and storing them inline would balloon every `AgentRun.update()`
// payload. Each line is its own Entity, keyed by `run_id`.

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "agent_log_lines", repo)]
pub struct AgentLogLine {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    /// Owning `AgentRun.id`. Indexed for chronological scans.
    #[architect(filterable)]
    pub run_id: Uuid,

    #[architect(sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub at: DateTime<Utc>,

    /// One of `AGENT_LOG_LEVELS`. Stored as a string for forward-
    /// compat — unknown values render as "info" in the UI.
    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::LogLevel"))]
    pub level: String,

    /// Producer identifier: "stdout" / "hermes.tool" / "model" / etc.
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::LogSource"))]
    pub source: String,

    #[architect(fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Sentence(3..15)")
    )]
    pub text: String,

    /// Upstream event id (e.g. Hermes `task_events.id`). Used by the
    /// plugin to dedupe on resume.
    pub external_event_id: Option<i64>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── GitRepoConnection ─────────────────────────────────────────────────
//
// Server-only: secrets must be sealed at rest; only the server
// decrypts. Don't ship the raw struct to wasm clients.
//
// v1 caveat: `webhook_secret_hash` here is treated as the already-
// sealed value. The actual sealing layer (sealed-box / KMS) is the
// next phase — this crate just stores whatever the server hands it.

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "git_repo_connections", repo)]
pub struct GitRepoConnection {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    /// One of `GIT_PROVIDERS`.
    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::GitProvider"))]
    pub provider: String,

    #[architect(filterable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::GitOwner"))]
    pub owner: String,

    #[architect(filterable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::GitRepoName"))]
    pub repo: String,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::GitBranch"))]
    pub default_branch: String,

    /// Optional org/project scope. `None` means the connection is
    /// available to any project in the org.
    #[architect(filterable)]
    pub project_id: Option<Uuid>,

    /// Sealed-box hash of the webhook secret. The raw secret is
    /// never stored — only the verifying hash. v1 stub: this crate
    /// holds whatever the server writes; sealing happens in the
    /// next phase.
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::SecretHash"))]
    pub webhook_secret_hash: String,

    /// URL-safe random suffix (e.g. "gh-7f3a") appended to a
    /// well-known prefix to make the webhook URL non-guessable
    /// without leaking the project_id.
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::WebhookPath"))]
    pub webhook_path: String,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub last_event_at: Option<DateTime<Utc>>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── AgentConversation ─────────────────────────────────────────────────
//
// Phase A: schema-only. The conversation hangs `Message` rows off
// itself via `Message.agent_conversation_id` (XOR with `channel_id`,
// enforced at the service layer). Branching is supported via
// `parent_conversation_id` + `branch_from_message_id` — when a user
// rewinds and resubmits, we fork a child conversation and replay up
// to the branch point.

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "agent_conversations", repo)]
pub struct AgentConversation {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "crate::fakers::AgentConversationTitle")
    )]
    pub title: String,

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Sentence(5..15)")
    )]
    pub system_prompt: Option<String>,

    /// Model id from `default_model_catalog()`. Filterable so the
    /// UI can scope by model.
    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ConversationModel"))]
    pub default_model: String,

    /// 700 = 0.7. See `ChatStreamRequest::temperature_milli`.
    #[cfg_attr(feature = "fake", dummy(faker = "0i32..1500"))]
    pub temperature_milli: i32,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::MaybeMaxTokens"))]
    pub max_tokens: Option<i32>,

    /// Names of tools available to this conversation. Resolved
    /// against the server-side tool registry when dispatching.
    #[architect(json)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::AgentToolSet"))]
    pub tool_set: Vec<String>,

    /// Optional pointer to the `AgentRun` that's currently driving
    /// this conversation. `None` for chat-only sessions that never
    /// dispatched to an external integration.
    #[architect(filterable)]
    pub agent_run_id: Option<Uuid>,

    #[architect(filterable)]
    pub project_id: Option<Uuid>,

    /// For branching. `None` for the root; `Some` when this is a
    /// child fork. The fork point is `branch_from_message_id`.
    #[architect(filterable)]
    pub parent_conversation_id: Option<Uuid>,

    pub branch_from_message_id: Option<Uuid>,

    #[architect(filterable)]
    pub archived: bool,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── ModelInfo + catalog ───────────────────────────────────────────────
//
// Embedded value type (NOT an Entity) — describes a single LLM the
// server can route to. Returned by `ChatModel::models()` and exposed
// to the UI via the registry.

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(::facet::Facet, Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct ModelInfo {
    /// Stable identifier (e.g. "claude-opus-4-7", "gemma3:4b").
    pub id: String,
    /// Provider key (e.g. "anthropic", "ollama", "mock"). Matches a
    /// `ChatModel::provider()`.
    pub provider: String,
    /// Human-readable label.
    pub display: String,
    /// `true` if the model supports extended thinking / reasoning
    /// blocks. Drives whether `stream_reasoning` is honoured.
    pub reasoning: bool,
    /// Context window in tokens.
    pub context_tokens: u32,
    /// Cost per million input tokens, in millicents
    /// (i.e. 15_000 = $15.00 / Mtok). `0` for free/local models.
    pub input_cost_milli: u32,
    pub output_cost_milli: u32,
}

/// Hand-authored v1 model catalog. Numbers are plausible-but-static —
/// real per-provider pricing lives in the plugin crates and supersedes
/// this list once the registry boots.
pub fn default_model_catalog() -> Vec<ModelInfo> {
    vec![
        ModelInfo {
            id: "mock".into(),
            provider: "mock".into(),
            display: "Mock (deterministic)".into(),
            reasoning: false,
            context_tokens: 32_000,
            input_cost_milli: 0,
            output_cost_milli: 0,
        },
        ModelInfo {
            id: "gemma3:4b".into(),
            provider: "ollama".into(),
            display: "Gemma 3 (4B, local)".into(),
            reasoning: false,
            context_tokens: 128_000,
            input_cost_milli: 0,
            output_cost_milli: 0,
        },
        ModelInfo {
            id: "qwen2.5-coder:32b".into(),
            provider: "ollama".into(),
            display: "Qwen2.5 Coder (32B, local)".into(),
            reasoning: false,
            context_tokens: 128_000,
            input_cost_milli: 0,
            output_cost_milli: 0,
        },
        ModelInfo {
            id: "claude-opus-4-7".into(),
            provider: "anthropic".into(),
            display: "Claude Opus 4.7".into(),
            reasoning: true,
            context_tokens: 200_000,
            input_cost_milli: 15_000,
            output_cost_milli: 75_000,
        },
        ModelInfo {
            id: "claude-sonnet-4-6".into(),
            provider: "anthropic".into(),
            display: "Claude Sonnet 4.6".into(),
            reasoning: true,
            context_tokens: 200_000,
            input_cost_milli: 3_000,
            output_cost_milli: 15_000,
        },
        ModelInfo {
            id: "claude-haiku-4-5".into(),
            provider: "anthropic".into(),
            display: "Claude Haiku 4.5".into(),
            reasoning: false,
            context_tokens: 200_000,
            input_cost_milli: 800,
            output_cost_milli: 4_000,
        },
        ModelInfo {
            id: "gpt-4o".into(),
            provider: "openai".into(),
            display: "GPT-4o".into(),
            reasoning: false,
            context_tokens: 128_000,
            input_cost_milli: 2_500,
            output_cost_milli: 10_000,
        },
        ModelInfo {
            id: "gpt-4o-mini".into(),
            provider: "openai".into(),
            display: "GPT-4o mini".into(),
            reasoning: false,
            context_tokens: 128_000,
            input_cost_milli: 150,
            output_cost_milli: 600,
        },
    ]
}

// ── ToolCall ──────────────────────────────────────────────────────────
//
// Structured tool-call records, separate from `AgentLogLine`. The log
// stream gets one row per event (including a tool-shaped one for
// visibility); the `ToolCall` entity is the sortable + filterable
// view the UI uses for "show me every Bash call this run", "approve
// this Edit", etc. Per spec rule `agent.tool-call.entity`.

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "agent_tool_calls", repo)]
pub struct ToolCall {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub run_id: Uuid,

    /// Monotonic per-run order. Server-assigned on insert.
    #[architect(sortable)]
    pub seq: i64,

    /// Tool name: `Bash`, `Read`, `Edit`, `Write`, `NotebookEdit`,
    /// `WebFetch`, etc. Free-text; UI maps known names to icons.
    #[architect(filterable, sortable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ToolName"))]
    pub name: String,

    /// JSON-encoded tool arguments. For file-edit tools includes
    /// `{path, before, after}` so the UI can render an inline diff
    /// without re-reading the file (r[agent.tool-call.file-edit-diff]).
    #[architect(fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ToolArgsJson"))]
    pub args_json: String,

    /// JSON-encoded tool result. None while pending / running.
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::MaybeToolResultJson"))]
    pub result_json: Option<String>,

    /// `pending`, `approved`, `denied`, `running`, `ok`, `error`.
    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ToolCallStatus"))]
    pub status: String,

    #[architect(sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub started_at: Option<DateTime<Utc>>,

    #[architect(sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub completed_at: Option<DateTime<Utc>>,

    /// When true, the run pauses on this tool until the user calls
    /// `AgentService.approve_tool` or `.deny_tool`.
    #[architect(filterable)]
    pub approval_required: bool,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── ConversationMessage ───────────────────────────────────────────────
//
// One message inside an `AgentConversation`. Per spec rule
// `agent.conversation.message`. Body field is intended to back a
// `LoroText` container at the CRDT layer; this proto holds the plain
// `String` view so wasm clients can serialize/deserialize without a
// Loro dep.

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "agent_conversation_messages", repo)]
pub struct ConversationMessage {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub conversation_id: Uuid,

    /// Monotonic per-conversation order. Server-assigned on insert.
    #[architect(sortable)]
    pub seq: i64,

    /// `user`, `assistant`, `tool`, `system`.
    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::MessageRole"))]
    pub role: String,

    /// Message text. Backed by a `LoroText` container in the CRDT
    /// layer (r[agent.crdt.conversation-message-text]) for multi-peer
    /// concurrent edits while a human composes.
    #[architect(fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Paragraph(1..3)")
    )]
    pub body: String,

    /// Set when `role=tool` — links to the originating `ToolCall.id`.
    #[architect(filterable)]
    pub tool_call_id: Option<Uuid>,

    /// Set when `role=assistant` — which model produced this message.
    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ConversationModel"))]
    pub model_id: Option<String>,

    /// True while the assistant is still streaming tokens into this
    /// row. UI concatenates streaming rows by `id` until this flips
    /// to false (r[agent.log-line.assistant-streaming]).
    pub streaming: bool,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── Run status state machine ──────────────────────────────────────────
//
// Per spec r[agent.run.status-state-machine]. Validates transitions
// at the service layer; the repo accepts any string so historical
// data and provider-native states keep round-tripping.

/// Canonical statuses an AgentRun can hold. Stored as a string in
/// `AgentRun.status`; this enum is the validator's view.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RunStatus {
    Queued,
    Starting,
    Running,
    Paused,
    AwaitingInput,
    Completed,
    Failed,
    Cancelled,
    TimedOut,
}

impl RunStatus {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Queued => "queued",
            Self::Starting => "starting",
            Self::Running => "running",
            Self::Paused => "paused",
            Self::AwaitingInput => "awaiting-input",
            Self::Completed => "completed",
            Self::Failed => "failed",
            Self::Cancelled => "cancelled",
            Self::TimedOut => "timed-out",
        }
    }

    pub fn parse(s: &str) -> Option<Self> {
        match s {
            "queued" => Some(Self::Queued),
            "starting" => Some(Self::Starting),
            "running" => Some(Self::Running),
            "paused" => Some(Self::Paused),
            "awaiting-input" => Some(Self::AwaitingInput),
            "completed" => Some(Self::Completed),
            "failed" => Some(Self::Failed),
            "cancelled" => Some(Self::Cancelled),
            "timed-out" => Some(Self::TimedOut),
            _ => None,
        }
    }

    pub fn is_terminal(self) -> bool {
        matches!(
            self,
            Self::Completed | Self::Failed | Self::Cancelled | Self::TimedOut
        )
    }
}

/// Return Ok(()) when `from -> to` is a legal transition per the spec
/// state machine. Returns `InvalidInput` with a descriptive message
/// otherwise.
pub fn validate_status_transition(from: RunStatus, to: RunStatus) -> Result<(), AgentServiceError> {
    use RunStatus::*;
    let ok = match (from, to) {
        // Terminal states are absorbing.
        (Completed | Failed | Cancelled | TimedOut, _) => false,
        // Forward queue progression.
        (Queued, Starting | Cancelled) => true,
        (Starting, Running | Failed | Cancelled) => true,
        // From running: user pause, agent block, or terminal.
        (Running, Paused | AwaitingInput | Completed | Failed | Cancelled | TimedOut) => true,
        // From paused or awaiting-input: only resume or cancel.
        (Paused | AwaitingInput, Running | Cancelled | Failed | TimedOut) => true,
        // Same-state writes are no-ops (allow idempotent retries).
        (a, b) if a == b => true,
        _ => false,
    };
    if ok {
        Ok(())
    } else {
        Err(AgentServiceError::InvalidInput(format!(
            "illegal AgentRun status transition: {} -> {}",
            from.as_str(),
            to.as_str()
        )))
    }
}

// ── Live update events ────────────────────────────────────────────────
//
// Per spec r[agent.live-update.events]. Three event kinds carried
// over a vox subscription keyed on `run_id` or `workspace_id`.

#[derive(::facet::Facet, Clone, Debug, PartialEq)]
#[repr(C)]
pub enum AgentEvent {
    /// Run transitioned to a new status. Subscribers refetch the run
    /// if they need other fields.
    RunStateChanged { run_id: Uuid, new_status: String },
    /// A log line was appended. Carries the line's id + sequence so
    /// subscribers can drive a virtualized list without an extra
    /// fetch round-trip; full body comes via the standard repo `get`.
    LogAppended {
        run_id: Uuid,
        log_id: Uuid,
        seq: i64,
    },
    /// A tool call changed status (pending → approved → running → ok
    /// / error etc.). Subscribers refetch the row to render the new
    /// state.
    ToolCallChanged {
        run_id: Uuid,
        tool_call_id: Uuid,
        new_status: String,
    },
}

// Provider backend trait: see `agent_proto::integration::AgentIntegration`.
// Hermes is the canonical backend — model selection (claude-code, codex,
// gemini, custom) happens inside Hermes itself, so the Task side talks
// only to Hermes through `AgentIntegration` (dispatch / cancel /
// fetch_run / fetch_logs / run_event_loop). Older adapter sketches
// (claude-code / codex direct subprocess) are deferred — Hermes covers
// them via its model registry.

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum AgentServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait AgentService {
    /// Start a new run. Service selects the adapter by `kind`, creates
    /// the `AgentRun` row, and kicks off the adapter. Returns the
    /// newly-created run.
    async fn start_run(
        &self,
        prompt: String,
        kind: String,
        worktree_path: Option<String>,
    ) -> Result<AgentRun, AgentServiceError>;

    /// Cancel a run. The adapter is signalled gracefully; on timeout
    /// the service forces a hard kill and marks the run cancelled.
    async fn cancel(&self, run_id: Uuid) -> Result<(), AgentServiceError>;

    /// Approve a tool call that's blocking on user consent.
    async fn approve_tool(&self, tool_call_id: Uuid) -> Result<(), AgentServiceError>;

    /// Deny a tool call. The agent stays in `awaiting-input` until it
    /// proposes an alternative or the user cancels the run.
    async fn deny_tool(
        &self,
        tool_call_id: Uuid,
        reason: Option<String>,
    ) -> Result<(), AgentServiceError>;
}

#[cfg(feature = "fake")]
pub mod fakers {
    use chrono::{DateTime, Duration, Utc};
    use fake::Dummy;
    use fake::rand::{Rng, seq::IndexedRandom};

    fn pick<R: Rng + ?Sized>(rng: &mut R, values: &[&str]) -> String {
        (*values.choose(rng).unwrap()).to_string()
    }

    pub struct AgentRunName;
    impl Dummy<AgentRunName> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &AgentRunName, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "Summarize meeting notes",
                    "Triage inbox",
                    "Draft release notes",
                    "Generate test cases",
                    "Refactor module",
                    "Translate documentation",
                    "Classify support tickets",
                    "Plan project milestones",
                    "Write commit message",
                    "Code review pass",
                ],
            )
        }
    }

    pub struct AgentKind;
    impl Dummy<AgentKind> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &AgentKind, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "chat",
                    "completion",
                    "tool-use",
                    "embedding",
                    "summarization",
                    "classification",
                    "code-gen",
                ],
            )
        }
    }

    pub struct AgentStatus;
    impl Dummy<AgentStatus> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &AgentStatus, rng: &mut R) -> Self {
            pick(
                rng,
                &["queued", "running", "completed", "failed", "cancelled"],
            )
        }
    }

    pub struct RecentDateTime;
    impl Dummy<RecentDateTime> for DateTime<Utc> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &RecentDateTime, rng: &mut R) -> Self {
            Utc::now() - Duration::days(rng.random_range(0..90))
        }
    }

    pub struct MaybeIntegration;
    impl Dummy<MaybeIntegration> for Option<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &MaybeIntegration, rng: &mut R) -> Self {
            if rng.random_bool(0.5) {
                Some(pick(rng, &["hermes", "claude-code", "mock"]))
            } else {
                None
            }
        }
    }

    pub struct MaybeExternalId;
    impl Dummy<MaybeExternalId> for Option<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &MaybeExternalId, rng: &mut R) -> Self {
            if rng.random_bool(0.5) {
                Some(format!("hermes-{}", rng.random_range(1000..99999u32)))
            } else {
                None
            }
        }
    }

    pub struct MaybeExternalUrl;
    impl Dummy<MaybeExternalUrl> for Option<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &MaybeExternalUrl, rng: &mut R) -> Self {
            if rng.random_bool(0.3) {
                Some(format!(
                    "https://hermes.example.com/tasks/{}",
                    rng.random_range(1000..99999u32)
                ))
            } else {
                None
            }
        }
    }

    pub struct LogLevel;
    impl Dummy<LogLevel> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &LogLevel, rng: &mut R) -> Self {
            pick(rng, super::AGENT_LOG_LEVELS)
        }
    }

    pub struct LogSource;
    impl Dummy<LogSource> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &LogSource, rng: &mut R) -> Self {
            pick(
                rng,
                &["stdout", "stderr", "model", "hermes.tool", "hermes.runner"],
            )
        }
    }

    pub struct GitProvider;
    impl Dummy<GitProvider> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &GitProvider, rng: &mut R) -> Self {
            pick(rng, super::GIT_PROVIDERS)
        }
    }

    pub struct GitOwner;
    impl Dummy<GitOwner> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &GitOwner, rng: &mut R) -> Self {
            pick(rng, &["cody", "Codys-Wright", "FastTrackStudios", "team"])
        }
    }

    pub struct GitRepoName;
    impl Dummy<GitRepoName> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &GitRepoName, rng: &mut R) -> Self {
            pick(rng, &["Task", "starcommand", "architect", "vox", "scratch"])
        }
    }

    pub struct GitBranch;
    impl Dummy<GitBranch> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &GitBranch, rng: &mut R) -> Self {
            pick(rng, &["main", "master", "develop"])
        }
    }

    pub struct SecretHash;
    impl Dummy<SecretHash> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &SecretHash, rng: &mut R) -> Self {
            (0..64)
                .map(|_| {
                    let n = rng.random_range(0..16u8);
                    char::from_digit(n as u32, 16).unwrap()
                })
                .collect()
        }
    }

    pub struct WebhookPath;
    impl Dummy<WebhookPath> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &WebhookPath, rng: &mut R) -> Self {
            let kind: &str = ["gh", "gl", "fj"].choose(rng).unwrap();
            let suffix: String = (0..4)
                .map(|_| {
                    let n = rng.random_range(0..16u8);
                    char::from_digit(n as u32, 16).unwrap()
                })
                .collect();
            format!("{kind}-{suffix}")
        }
    }

    pub struct AgentConversationTitle;
    impl Dummy<AgentConversationTitle> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &AgentConversationTitle, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "Refactor the agent feature",
                    "Plan the Phase A migration",
                    "Help me write release notes",
                    "Debug the CRDT codec",
                    "Architect dry-run",
                    "Brainstorm a new feature",
                    "Code review of the chat PR",
                    "Triage today's bugs",
                ],
            )
        }
    }

    pub struct ConversationModel;
    impl Dummy<ConversationModel> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ConversationModel, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "mock",
                    "claude-opus-4-7",
                    "claude-sonnet-4-6",
                    "gpt-4o-mini",
                    "gemma3:4b",
                ],
            )
        }
    }

    pub struct MaybeMaxTokens;
    impl Dummy<MaybeMaxTokens> for Option<i32> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &MaybeMaxTokens, rng: &mut R) -> Self {
            if rng.random_bool(0.5) {
                Some(rng.random_range(512..16_384i32))
            } else {
                None
            }
        }
    }

    // ── Fakers for MVP additions (P1) ────────────────────────────────

    pub struct MaybeTokenLimit;
    impl Dummy<MaybeTokenLimit> for Option<u64> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &MaybeTokenLimit, rng: &mut R) -> Self {
            if rng.random_bool(0.3) {
                Some(rng.random_range(10_000u64..2_000_000))
            } else {
                None
            }
        }
    }

    pub struct MaybeToolLimit;
    impl Dummy<MaybeToolLimit> for Option<u32> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &MaybeToolLimit, rng: &mut R) -> Self {
            if rng.random_bool(0.3) {
                Some(rng.random_range(10u32..500))
            } else {
                None
            }
        }
    }

    pub struct MaybeWallSeconds;
    impl Dummy<MaybeWallSeconds> for Option<u32> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &MaybeWallSeconds, rng: &mut R) -> Self {
            if rng.random_bool(0.3) {
                Some(rng.random_range(60u32..7200))
            } else {
                None
            }
        }
    }

    pub struct ToolName;
    impl Dummy<ToolName> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ToolName, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "Bash",
                    "Read",
                    "Edit",
                    "Write",
                    "Grep",
                    "Glob",
                    "WebFetch",
                    "WebSearch",
                    "NotebookEdit",
                ],
            )
        }
    }

    pub struct ToolArgsJson;
    impl Dummy<ToolArgsJson> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ToolArgsJson, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    r#"{"command":"ls -la"}"#,
                    r#"{"file_path":"src/lib.rs"}"#,
                    r#"{"path":"src/main.rs","before":"old","after":"new"}"#,
                    r#"{"pattern":"TODO","path":"src/"}"#,
                ],
            )
        }
    }

    pub struct MaybeToolResultJson;
    impl Dummy<MaybeToolResultJson> for Option<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &MaybeToolResultJson, rng: &mut R) -> Self {
            if rng.random_bool(0.7) {
                Some(pick(
                    rng,
                    &[
                        r#"{"stdout":"file.rs\nmod.rs\n","exit_code":0}"#,
                        r#"{"hash":"abc123","written":42}"#,
                        r#"{"matches":["file.rs:10","mod.rs:5"]}"#,
                    ],
                ))
            } else {
                None
            }
        }
    }

    pub struct ToolCallStatus;
    impl Dummy<ToolCallStatus> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ToolCallStatus, rng: &mut R) -> Self {
            pick(
                rng,
                &["pending", "approved", "denied", "running", "ok", "error"],
            )
        }
    }

    pub struct MessageRole;
    impl Dummy<MessageRole> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &MessageRole, rng: &mut R) -> Self {
            pick(rng, &["user", "assistant", "tool", "system"])
        }
    }

    pub struct AgentToolSet;
    impl Dummy<AgentToolSet> for Vec<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &AgentToolSet, rng: &mut R) -> Self {
            const POOL: &[&str] = &[
                "shell",
                "edit",
                "grep",
                "read",
                "write",
                "web_fetch",
                "task_search",
            ];
            let n = rng.random_range(0..=3usize);
            POOL.choose_multiple(rng, n)
                .map(|s| s.to_string())
                .collect()
        }
    }

    pub struct AgentTags;
    impl Dummy<AgentTags> for Vec<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &AgentTags, rng: &mut R) -> Self {
            const POOL: &[&str] = &[
                "gpt-4",
                "gpt-5",
                "claude",
                "experimental",
                "production",
                "background",
                "user-initiated",
                "scheduled",
            ];
            let n = rng.random_range(1..=3usize);
            POOL.choose_multiple(rng, n)
                .map(|s| s.to_string())
                .collect()
        }
    }
}
