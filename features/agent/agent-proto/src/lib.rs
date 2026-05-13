pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

pub mod integration;

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
