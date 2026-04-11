// r[impl integration.framework]
//! Integration configuration framework.
//!
//! Integrations are declared as TOML files at:
//!   `.config/task/integrations/<name>.toml`
//!
//! Each file describes a named bundle of status definitions, project/task
//! templates, and area/context conventions for a specific domain.

use std::path::Path;

use serde::{Deserialize, Serialize};

// r[impl integration.framework]
/// A named integration bundle loaded from a TOML config file.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Integration {
    pub name: String,
    /// Custom status definitions for this domain.
    #[serde(default)]
    pub statuses: Vec<StatusDef>,
    /// Named project templates with pre-defined task scaffolding.
    #[serde(default)]
    pub project_templates: Vec<ProjectTemplate>,
    /// Standalone task templates.
    #[serde(default)]
    pub task_templates: Vec<TaskTemplate>,
    /// Suggested area WikiLink names for this domain.
    #[serde(default)]
    pub area_conventions: Vec<String>,
    /// Suggested context names for this domain.
    #[serde(default)]
    pub context_conventions: Vec<String>,
}

/// A single status definition for a domain integration.
// r[impl integration.status-set]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StatusDef {
    pub name: String,
    /// Whether completing a task with this status marks it as done.
    #[serde(default)]
    pub is_completion: bool,
    /// Optional display colour (CSS hex or named colour).
    pub color: Option<String>,
}

/// A named project template that scaffolds tasks on creation.
// r[impl integration.project-template]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProjectTemplate {
    pub name: String,
    pub description: Option<String>,
    /// Tasks to create automatically when a project is instantiated.
    #[serde(default)]
    pub tasks: Vec<TaskTemplate>,
}

/// A task template with optional `{{variable}}` placeholders in text fields.
// r[impl integration.task-template]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskTemplate {
    pub title: String,
    /// Status name string (may be an integration-specific status).
    pub status: Option<String>,
    /// Priority string: none / low / normal / high / urgent.
    pub priority: Option<String>,
    #[serde(default)]
    pub contexts: Vec<String>,
    #[serde(default)]
    pub tags: Vec<String>,
    /// RFC 5545 RRULE string.
    pub recurrence: Option<String>,
    pub time_estimate_minutes: Option<u32>,
    pub body: Option<String>,
}

impl Integration {
    /// Look up a status definition by name (case-insensitive).
    pub fn status(&self, name: &str) -> Option<&StatusDef> {
        self.statuses
            .iter()
            .find(|s| s.name.eq_ignore_ascii_case(name))
    }

    /// Return whether `status_name` is a completion status for this integration.
    pub fn is_completion_status(&self, name: &str) -> bool {
        self.status(name).map(|s| s.is_completion).unwrap_or(false)
    }
}

// r[impl integration.framework]
/// Load a single integration from a TOML file.
pub fn load_integration(path: &Path) -> eyre::Result<Integration> {
    let content = std::fs::read_to_string(path)
        .map_err(|e| eyre::eyre!("Failed to read {}: {e}", path.display()))?;
    let integration: Integration = toml::from_str(&content)
        .map_err(|e| eyre::eyre!("Failed to parse {}: {e}", path.display()))?;
    Ok(integration)
}

// r[impl integration.framework]
/// Load all integrations from `<config_dir>/integrations/*.toml`.
/// Files that fail to parse are silently skipped.
pub fn list_integrations(config_dir: &Path) -> Vec<Integration> {
    let dir = config_dir.join("integrations");
    let entries = match std::fs::read_dir(&dir) {
        Ok(e) => e,
        Err(_) => return vec![],
    };
    entries
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().and_then(|s| s.to_str()) == Some("toml"))
        .filter_map(|e| load_integration(&e.path()).ok())
        .collect()
}
