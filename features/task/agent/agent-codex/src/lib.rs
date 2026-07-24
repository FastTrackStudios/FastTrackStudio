//! `agent-codex` — Codex backend for the agent feature.
//!
//! Vendors `CodexMonitor`'s tokio Rust control plane for
//! `codex app-server` under [`vendor`] (~3100 LOC, mostly
//! verbatim) and exposes a wrapper that implements
//! `agent_proto::Agents` on top of it.
//!
//! ## Surface
//!
//! - `CodexBackend` — top-level handle, cheaply clonable.
//!   Owns a `BroadcastSink` (vendor `EventSink`), a session
//!   registry keyed by `session_id`, and per-session
//!   broadcast channels with translated
//!   `agent_proto::AgentEvent`s.
//! - `CodexBackend::subscribe_raw()` — raw event firehose
//!   for debug / advanced consumers.
//! - `CodexBackend::chat()` — single-turn convenience used
//!   by the CLI demo and by `agent-wiki`'s ingest bridge.
//! - `impl Agents for CodexBackend` — proto-shaped surface.
//!   Codex-relevant methods (sessions, `dispatch_turn`,
//!   `subscribe_session`) are real; non-Codex methods
//!   (profiles, kanban, projects, ...) return
//!   `AgentError::Unsupported`.

#[path = "../vendor/mod.rs"]
mod vendor;

mod chat;
mod service;
mod translate;

use std::collections::HashMap;
use std::sync::Arc;

use tokio::sync::{Mutex, broadcast};

use agent_proto::event::AgentEvent;
use agent_proto::session::Session;

pub use chat::{ChatHandle, ChatOpts};
pub use vendor::events::AppServerEvent;

/// Sink that fans `AppServerEvent`s out to a broadcast
/// channel. Implements the vendored `EventSink` trait;
/// `CodexBackend` translates each event into an
/// `agent_proto::AgentEvent` for the trait's subscription
/// channels.
#[derive(Clone)]
pub struct BroadcastSink {
    tx: broadcast::Sender<AppServerEvent>,
}

impl BroadcastSink {
    #[must_use]
    pub fn new(capacity: usize) -> Self {
        let (tx, _rx) = broadcast::channel(capacity);
        Self { tx }
    }

    #[must_use]
    pub fn subscribe(&self) -> broadcast::Receiver<AppServerEvent> {
        self.tx.subscribe()
    }
}

impl vendor::events::EventSink for BroadcastSink {
    fn emit_app_server_event(&self, event: AppServerEvent) {
        let _ = self.tx.send(event);
    }
}

/// Per-session bookkeeping carried by [`CodexBackend`].
pub(crate) struct SessionRow {
    pub(crate) session: Session,
    /// Broadcast channel of translated `AgentEvent`s for
    /// this session. Subscribers (UIs, CLIs, agent-wiki
    /// bridges) hit this; `dispatch_turn` populates it.
    pub(crate) events_tx: broadcast::Sender<AgentEvent>,
    /// Accumulated assistant text keyed by message id —
    /// `list_messages` rebuilds `Message`s from this.
    pub(crate) accumulated: HashMap<String, String>,
}

/// Top-level handle to the Codex backend. Clone-friendly:
/// the inner state lives behind `Arc`, so workers spawned
/// from trait methods share the same session map.
#[derive(Clone, architect::HasDispatcher)]
pub struct CodexBackend {
    inner: Arc<CodexInner>,
}

pub(crate) struct CodexInner {
    pub(crate) sink: BroadcastSink,
    pub(crate) sessions: Mutex<HashMap<String, SessionRow>>,
}

impl CodexBackend {
    #[must_use]
    pub fn new() -> Self {
        Self {
            inner: Arc::new(CodexInner {
                sink: BroadcastSink::new(1024),
                sessions: Mutex::new(HashMap::new()),
            }),
        }
    }

    /// Subscribe to the raw Codex event stream — every
    /// `AppServerEvent` regardless of workspace. UIs prefer
    /// `subscribe_session` (in the trait) for filtered
    /// streams.
    /// Async session-existence probe — for callers already inside
    /// the runtime (the sync trait methods use `blocking_lock` and
    /// must not be called from async context).
    pub async fn has_session(&self, session_id: &str) -> bool {
        self.inner.sessions.lock().await.contains_key(session_id)
    }

    #[must_use]
    pub fn subscribe_raw(&self) -> broadcast::Receiver<AppServerEvent> {
        self.inner.sink.subscribe()
    }

    /// Clone of the sink so callers can spawn workspace
    /// sessions with it.
    #[must_use]
    pub fn sink(&self) -> BroadcastSink {
        self.inner.sink.clone()
    }
}

impl Default for CodexBackend {
    fn default() -> Self {
        Self::new()
    }
}
