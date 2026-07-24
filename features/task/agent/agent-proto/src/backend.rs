//! `AgentBackend` — a configured connection to one agent
//! runtime. Multiple backends can coexist (e.g. one Hermes
//! in-process, one Codex CLI monitor, one Claude CLI bridge).
//! Each backend registers a stable `id` that profiles +
//! sessions reference.

use chrono::{DateTime, Utc};
use facet::Facet;

/// One configured agent runtime. The `kind` field tells the
/// service whether the backend dispatches in-process,
/// monitors an external CLI, or talks HTTP.
#[derive(Debug, Clone, PartialEq, Facet)]
#[repr(C)]
pub struct AgentBackend {
    /// Stable id — `"hermes"`, `"codex"`, `"claude-cli"`,
    /// `"pi"`, `"openai-api"`. Lowercase + hyphens; used in
    /// path components.
    pub id: String,
    /// Human-facing label.
    pub label: String,
    /// What kind of backend this is.
    pub kind: BackendKind,
    /// Backend-specific configuration JSON (model defaults,
    /// API endpoints, watch paths, etc.). Opaque to the
    /// trait surface.
    pub config_json: String,
    /// When the backend was registered.
    pub registered_at: DateTime<Utc>,
    /// Last time the backend successfully answered a
    /// `health` ping. Empty when never.
    pub last_seen: Option<DateTime<Utc>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Facet)]
#[repr(C)]
pub enum BackendKind {
    /// Embeds an agent runtime — Hermes Rust SDK, `OpenAI`
    /// SDK, custom Rust agent. Owns the conversation lock,
    /// streams events directly.
    InProcess,
    /// Watches an external CLI's on-disk logs (Codex,
    /// Claude CLI). Read-only by default; `dispatch_turn`
    /// is `Unsupported` unless the CLI exposes an IPC
    /// channel.
    ExternalMonitor,
    /// Spawns a CLI per turn and parses its output stream.
    /// Bridges between in-process and external-monitor:
    /// dispatch works, but no persistent process.
    CliBridge,
    /// Talks to a remote HTTP / SSE / WebSocket endpoint
    /// (a hosted Hermes, a peer Task server, etc.).
    Http,
}

/// Per-backend health snapshot.
#[derive(Debug, Clone, PartialEq, Facet)]
#[repr(C)]
pub struct BackendHealth {
    pub backend_id: String,
    pub reachable: bool,
    /// Latency of the most recent ping, in milliseconds.
    /// `0` when the backend is local (nothing to probe).
    pub last_ping_ms: u32,
    /// Backend version string, if reported.
    pub version: String,
    /// Backend-specific status (e.g. `"model: claude-opus
    /// reasoning: medium"`), or the error text when unreachable.
    pub status_text: String,
    /// Lifecycle state the backend reports for itself
    /// (`"running"`, `"degraded"`, `"local"`, `"unconfigured"`).
    /// Empty when the backend doesn't report one.
    pub state: String,
    /// Agent runs currently in flight inside the backend.
    pub active_agents: u32,
    /// Connected chat platforms / adapters (`"api_server"`,
    /// `"discord"`, …) — Hermes gateways serve several at once.
    pub platforms: Vec<String>,
    /// The backend's configured default model.
    pub model: String,
    /// When the snapshot was taken.
    pub at: DateTime<Utc>,
}
