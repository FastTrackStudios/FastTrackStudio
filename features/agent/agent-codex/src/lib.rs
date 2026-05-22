//! `agent-codex` — Codex backend for the agent feature.
//!
//! Vendors CodexMonitor's tokio Rust control plane for
//! `codex app-server` under [`vendor`] (~3100 LOC, mostly
//! verbatim) and exposes a thin wrapper that implements
//! `agent_proto::AgentService` on top of it.
//!
//! ## Current slice (this commit)
//!
//! - Vendored modules build clean.
//! - `EventSink` is implemented over a
//!   `tokio::sync::broadcast` channel so multiple
//!   subscribers (UIs, CLIs, the wiki bridge) can tap the
//!   same Codex event stream.
//! - `CodexBackend` struct owns the workspace-session
//!   registry. `AgentService` impl is queued for the next
//!   commit — the trait surface needs translation from
//!   Codex's `WorkspaceInfo` / `ThreadSummary` /
//!   `ConversationItem` shapes into our types.
//!
//! ## Roadmap
//!
//! 1. (this) — vendor + skeleton builds.
//! 2. Translate JSON-RPC payloads → `AgentEvent`
//!    (`message_delta`, `tool_started`, `tool_finished`,
//!    `approval_requested`, etc.).
//! 3. Implement read-side `AgentService` methods
//!    (`list_sessions`, `read_session`, `list_messages`,
//!    `list_tool_calls`).
//! 4. Implement `dispatch_turn` over `vendor::app_server::WorkspaceSession::send_request`.
//! 5. `import_external_session` from on-disk Codex logs
//!    (separate path; no daemon required).

#[path = "../vendor/mod.rs"]
mod vendor;

use std::sync::Arc;
use tokio::sync::{Mutex, broadcast};

// Re-exports of the vendored types our public surface needs.
// `vendor::app_server::WorkspaceSession` is `pub(crate)` upstream; we surface it
// through `CodexBackend` methods rather than re-exporting.
pub use vendor::events::AppServerEvent;

/// Sink that fans `AppServerEvent`s out to a broadcast
/// channel. Consumers subscribe to a `Receiver<AppServerEvent>`;
/// the [`CodexBackend`] then translates each event into an
/// `agent_proto::AgentEvent` for the trait's subscription
/// channels.
#[derive(Clone)]
pub struct BroadcastSink {
    tx: broadcast::Sender<AppServerEvent>,
}

impl BroadcastSink {
    pub fn new(capacity: usize) -> Self {
        let (tx, _rx) = broadcast::channel(capacity);
        Self { tx }
    }

    pub fn subscribe(&self) -> broadcast::Receiver<AppServerEvent> {
        self.tx.subscribe()
    }
}

impl vendor::events::EventSink for BroadcastSink {
    fn emit_app_server_event(&self, event: AppServerEvent) {
        // Drop the result — there may be no subscribers yet
        // and that's fine.
        let _ = self.tx.send(event);
    }
}

/// Top-level handle to the Codex backend. Owns a registry
/// of per-workspace [`vendor::app_server::WorkspaceSession`] handles + the
/// broadcast sink they all push to.
pub struct CodexBackend {
    sink: BroadcastSink,
    #[allow(dead_code)] // wired by `register` in the next slice
    sessions: Arc<Mutex<Vec<Arc<vendor::app_server::WorkspaceSession>>>>,
}

impl CodexBackend {
    pub fn new() -> Self {
        Self {
            sink: BroadcastSink::new(1024),
            sessions: Arc::new(Mutex::new(Vec::new())),
        }
    }

    /// Subscribe to the raw Codex event stream. Most
    /// callers will prefer the `agent_proto::AgentService`
    /// `subscribe_session` surface once it's wired; this is
    /// the low-level firehose.
    pub fn subscribe_raw(&self) -> broadcast::Receiver<AppServerEvent> {
        self.sink.subscribe()
    }

    /// Hand out a clone of the sink so callers can spawn
    /// workspace sessions with it.
    pub fn sink(&self) -> BroadcastSink {
        self.sink.clone()
    }

    /// Track a freshly-spawned vendor `WorkspaceSession`.
    /// Crate-internal — the public API runs through
    /// `agent_proto::AgentService` once the wrapper is wired
    /// (next slice).
    #[allow(dead_code)]
    pub(crate) async fn register(&self, session: Arc<vendor::app_server::WorkspaceSession>) {
        self.sessions.lock().await.push(session);
    }

    /// Snapshot of the live session registry. Crate-internal.
    #[allow(dead_code)]
    pub(crate) async fn sessions(&self) -> Vec<Arc<vendor::app_server::WorkspaceSession>> {
        self.sessions.lock().await.clone()
    }
}

impl Default for CodexBackend {
    fn default() -> Self {
        Self::new()
    }
}
