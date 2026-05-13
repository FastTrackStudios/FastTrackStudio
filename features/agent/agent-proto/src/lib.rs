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

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::AgentTags"))]
    pub tags: Vec<String>,

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
    async fn cancel(&self, run_id: Uuid) -> Result<(), AgentServiceError>;
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
