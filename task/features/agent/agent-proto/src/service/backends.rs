//! Backend registry — the "which agent runtimes are wired
//! up" surface. Usually owned by whoever runs the server,
//! not by individual backends.

use crate::backend::{AgentBackend, BackendHealth, BackendKind};
use crate::error::AgentError;

#[architect::rpc]
pub trait Backends {
    fn upsert_backend(&self, backend: AgentBackend) -> Result<AgentBackend, AgentError>;
    fn remove_backend(&self, backend_id: &str) -> Result<(), AgentError>;
    fn list_backends(&self) -> Result<Vec<AgentBackend>, AgentError>;
    fn backend_health(&self, backend_id: &str) -> Result<BackendHealth, AgentError>;
    fn backends_by_kind(&self, kind: BackendKind) -> Result<Vec<AgentBackend>, AgentError>;
}
