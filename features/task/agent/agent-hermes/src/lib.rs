//! Hermes agent backend — an in-process HTTP client for a
//! [hermes-agent](https://github.com/NousResearch/hermes-agent)
//! gateway.
//!
//! Where [`agent-codex`] spawns `codex app-server` processes over
//! stdio, Hermes is a long-running **gateway** we talk to over its
//! OpenAI-compatible API. One gateway serves every session, and the
//! agent's self-improving state (memory, skills) follows the
//! conversation rather than the connection: the [`responses`]
//! transport chains each turn onto the previous response id, and the
//! [`stream`] fallback scopes with the `X-Hermes-Session-Key` header.
//!
//! The backend keeps the Task-side conversation history itself
//! (`Vec<Message>` per session) and replays it as the `messages`
//! array each turn — the gateway's session memory is additive
//! (skills, learned context), not a substitute for the visible
//! transcript.
//!
//! Config comes from `TASK_HERMES_*` env vars (see
//! [`HermesConfig::from_env`]); the backend is only constructed
//! when `TASK_HERMES_URL` is set.

#![cfg(not(target_arch = "wasm32"))]

mod discovery;
mod responses;
mod routines;
mod service;
mod stream;

use std::collections::HashMap;
use std::sync::Arc;
use std::sync::atomic::AtomicBool;

use agent_proto::event::AgentEvent;
use agent_proto::message::Message;
use agent_proto::session::Session;
use tokio::sync::{Mutex, broadcast};

pub const BACKEND_ID: &str = "hermes";

/// Connection settings for one hermes-agent gateway.
#[derive(Debug, Clone)]
pub struct HermesConfig {
    /// API base including the version segment, e.g.
    /// `http://127.0.0.1:8642/v1`.
    pub base_url: String,
    /// `API_SERVER_KEY` bearer token; empty = no auth header.
    pub api_key: String,
    /// Default model id sent when the turn carries no override.
    /// The gateway resolves it against its configured provider —
    /// `"hermes"` selects the gateway's default profile/model.
    pub model: String,
}

impl HermesConfig {
    /// Read the gateway settings from the environment:
    ///
    /// - `TASK_HERMES_URL` — API base (required; absence = backend
    ///   disabled). With or without the trailing `/v1`.
    /// - `TASK_HERMES_API_KEY` — bearer token (optional).
    /// - `TASK_HERMES_MODEL` — default model (default `hermes`).
    #[must_use]
    pub fn from_env() -> Option<Self> {
        let raw = std::env::var("TASK_HERMES_URL").ok()?;
        let raw = raw.trim().trim_end_matches('/');
        if raw.is_empty() {
            return None;
        }
        let base_url = if raw.ends_with("/v1") {
            raw.to_string()
        } else {
            format!("{raw}/v1")
        };
        Some(Self {
            base_url,
            api_key: std::env::var("TASK_HERMES_API_KEY").unwrap_or_default(),
            model: std::env::var("TASK_HERMES_MODEL").unwrap_or_else(|_| "hermes".to_string()),
        })
    }
}

/// Per-session bookkeeping.
pub(crate) struct SessionRow {
    pub(crate) session: Session,
    /// Translated `AgentEvent`s for this session — UIs subscribe,
    /// `dispatch_turn`'s worker publishes.
    pub(crate) events_tx: broadcast::Sender<AgentEvent>,
    /// Full transcript (user + assistant), chronological. Replayed
    /// as the `messages` array on each turn.
    pub(crate) messages: Vec<Message>,
    /// Set by `cancel_turn`; the streaming worker checks it
    /// between chunks and stops cleanly.
    pub(crate) cancel: Arc<AtomicBool>,
    /// Response id of the last completed turn — chains the next one
    /// onto the same gateway-side session (see [`responses`]).
    pub(crate) last_response_id: String,
}

/// Handle to the Hermes backend. Clone-friendly — state lives
/// behind `Arc` so workers spawned from trait methods share the
/// session map.
#[derive(Clone, architect::HasDispatcher)]
pub struct HermesBackend {
    pub(crate) inner: Arc<HermesInner>,
}

pub(crate) struct HermesInner {
    pub(crate) config: HermesConfig,
    pub(crate) http: reqwest::Client,
    /// Runtime handle captured at construction — the sync Discovery
    /// methods run on blocking dispatcher threads and drive their
    /// HTTP hops through it.
    pub(crate) runtime: tokio::runtime::Handle,
    pub(crate) sessions: Mutex<HashMap<String, SessionRow>>,
    /// Latched once a gateway 404s `/v1/responses` — pre-Responses
    /// deployments shouldn't pay a failed round-trip per turn.
    pub(crate) legacy_transport: AtomicBool,
}

impl HermesBackend {
    #[must_use]
    pub fn new(config: HermesConfig) -> Self {
        Self {
            inner: Arc::new(HermesInner {
                config,
                http: reqwest::Client::new(),
                runtime: tokio::runtime::Handle::current(),
                sessions: Mutex::new(HashMap::new()),
                legacy_transport: AtomicBool::new(false),
            }),
        }
    }

    /// Construct from `TASK_HERMES_*` env vars; `None` when the
    /// gateway URL isn't configured.
    #[must_use]
    pub fn from_env() -> Option<Self> {
        HermesConfig::from_env().map(Self::new)
    }

    #[must_use]
    pub fn config(&self) -> &HermesConfig {
        &self.inner.config
    }

    /// Async session-existence probe — for callers already inside
    /// the runtime (the sync trait methods use `blocking_lock` and
    /// must not be called from async context).
    pub async fn has_session(&self, session_id: &str) -> bool {
        self.inner.sessions.lock().await.contains_key(session_id)
    }
}
