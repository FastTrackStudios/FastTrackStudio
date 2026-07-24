//! Discovery — what a backend can do, for UIs to render pickers
//! and panels from live data instead of hardcoded lists.
//!
//! Backends answer from their own source of truth: the Hermes
//! backend proxies its gateway's `/v1/models` + `/v1/skills` +
//! `/v1/capabilities`; Codex reports its static model set. A
//! router merges across backends, tagging each row with its
//! `backend_id`.

use crate::error::AgentError;
use facet::Facet;

/// One selectable model, as reported by a backend.
#[derive(Debug, Clone, PartialEq, Eq, Facet)]
#[repr(C)]
pub struct ModelInfo {
    /// Backend that serves it (`"hermes"`, `"codex"`).
    pub backend_id: String,
    /// Id to pass as `DispatchTurn.model_override`.
    pub id: String,
    /// Display label; falls back to `id` when empty.
    pub label: String,
    /// True for the backend's default model.
    pub is_default: bool,
}

/// One agent skill (Hermes's self-improving skill library).
#[derive(Debug, Clone, PartialEq, Eq, Facet)]
#[repr(C)]
pub struct SkillInfo {
    pub backend_id: String,
    pub name: String,
    pub description: String,
    /// Whether the skill is currently enabled/loadable.
    pub enabled: bool,
}

/// Feature flags a backend reports (shape mirrors Hermes's
/// `/v1/capabilities`, flattened to labeled booleans so the UI can
/// list them without knowing the backend).
#[derive(Debug, Clone, PartialEq, Eq, Facet)]
#[repr(C)]
pub struct CapabilityFlag {
    pub backend_id: String,
    pub name: String,
    pub enabled: bool,
}

#[architect::rpc]
pub trait Discovery {
    /// Models across all configured backends (or one, when
    /// `backend_id` is non-empty).
    fn list_models(&self, backend_id: &str) -> Result<Vec<ModelInfo>, AgentError>;

    /// Skills across all configured backends. Backends without a
    /// skill system return an empty list.
    fn list_skills(&self, backend_id: &str) -> Result<Vec<SkillInfo>, AgentError>;

    /// Capability flags across all configured backends.
    fn list_capabilities(&self, backend_id: &str) -> Result<Vec<CapabilityFlag>, AgentError>;
}
