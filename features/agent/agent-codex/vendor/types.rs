//! Minimal type shim for the vendored modules. CodexMonitor's
//! `types.rs` is 1418 lines of UI config; the vendored
//! `app_server.rs` + `args.rs` only touch a tiny subset
//! (`entry.id`, `entry.path`, and `AppSettings.codex_args`).
//! This shim covers exactly that surface so the vendored
//! code compiles without dragging in unrelated config.

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub(super) struct WorkspaceEntry {
    pub(super) id: String,
    pub(super) name: String,
    pub(super) path: String,
    #[serde(default)]
    pub(super) kind: WorkspaceKind,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(super) parent_id: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(super) worktree: Option<WorktreeInfo>,
    #[serde(default)]
    pub(super) settings: WorkspaceSettings,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(super) enum WorkspaceKind {
    Main,
    Worktree,
}

impl Default for WorkspaceKind {
    fn default() -> Self {
        WorkspaceKind::Main
    }
}

impl WorkspaceKind {
    #[allow(dead_code)]
    pub(super) fn is_worktree(&self) -> bool {
        matches!(self, WorkspaceKind::Worktree)
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub(super) struct WorktreeInfo {
    pub(super) branch: String,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub(super) struct WorkspaceSettings {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(super) launch_script: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(super) git_root: Option<String>,
}

/// Only the field used by the vendored `args.rs`. Other
/// CodexMonitor settings (theme, fonts, shortcuts, dictation)
/// are intentionally absent.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub(super) struct AppSettings {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub(super) codex_args: Option<String>,
}
