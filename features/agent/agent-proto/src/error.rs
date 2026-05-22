//! Trait-boundary error type for `AgentService`.

use facet::Facet;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq, Facet, Error)]
#[repr(C)]
pub enum AgentError {
    #[error("backend not found: {0}")]
    BackendNotFound(String),

    #[error("profile not found: {0}")]
    ProfileNotFound(String),

    #[error("project not found: {0}")]
    ProjectNotFound(String),

    #[error("session not found: {0}")]
    SessionNotFound(String),

    #[error("message not found: {0}")]
    MessageNotFound(String),

    #[error("kanban board not found: {0}")]
    BoardNotFound(String),

    #[error("card not found: {0}")]
    CardNotFound(String),

    /// A turn is already in flight on this session — the
    /// caller must wait or cancel.
    #[error("session busy: {0}")]
    SessionBusy(String),

    /// State-machine violation (e.g. answering an `Approval`
    /// for an already-resolved request).
    #[error("illegal state: {0}")]
    IllegalState(String),

    /// Backend told us it doesn't support a method.
    /// External-monitor backends typically `Unsupported`
    /// `dispatch_turn`; in-process backends typically
    /// support everything.
    #[error("unsupported by backend {backend}: {operation}")]
    Unsupported { backend: String, operation: String },

    #[error("timeout: {0}")]
    Timeout(String),

    #[error("rate limited: {0}")]
    RateLimited(String),

    #[error("auth: {0}")]
    Auth(String),

    #[error("io: {0}")]
    Io(String),

    #[error("backend: {0}")]
    Backend(String),
}
