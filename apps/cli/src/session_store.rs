//! Persistent CLI session file.
//!
//! Written by `task auth login`, read by every subcommand
//! that needs an authenticated `user_id` / `org_id`. Lives at
//! `$XDG_DATA_HOME/task/session.json` (override via
//! `TASK_SESSION_FILE`). The file is JSON, not TOML, so a
//! human inspecting it sees the same shape the server's auth
//! tables use.

use std::path::PathBuf;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CliSession {
    pub token: String,
    pub user_id: uuid::Uuid,
    pub email: String,
    /// Active org. Populated either at login (from the
    /// session's `active_organization_id`) or via
    /// `task auth org use <id>`. `None` = no org selected
    /// yet; callers should refuse to do org-scoped work
    /// until one is picked.
    pub org_id: Option<uuid::Uuid>,
}

/// Resolve the session path. `$TASK_SESSION_FILE` wins; else
/// `$XDG_DATA_HOME/task/session.json` with the standard
/// `$HOME/.local/share` fallback. Creates parent dirs.
pub fn session_path() -> eyre::Result<PathBuf> {
    if let Ok(explicit) = std::env::var("TASK_SESSION_FILE") {
        if !explicit.is_empty() {
            let p = PathBuf::from(explicit);
            if let Some(parent) = p.parent() {
                std::fs::create_dir_all(parent)
                    .map_err(|e| eyre::eyre!("create {}: {e}", parent.display()))?;
            }
            return Ok(p);
        }
    }
    let base = match std::env::var("XDG_DATA_HOME") {
        Ok(v) if !v.is_empty() => PathBuf::from(v),
        _ => {
            let home = std::env::var("HOME")
                .map_err(|_| eyre::eyre!("neither XDG_DATA_HOME nor HOME is set"))?;
            PathBuf::from(home).join(".local").join("share")
        }
    };
    let dir = base.join("task");
    std::fs::create_dir_all(&dir).map_err(|e| eyre::eyre!("create {}: {e}", dir.display()))?;
    Ok(dir.join("session.json"))
}

pub fn load() -> eyre::Result<Option<CliSession>> {
    let path = session_path()?;
    if !path.exists() {
        return Ok(None);
    }
    let raw =
        std::fs::read_to_string(&path).map_err(|e| eyre::eyre!("read {}: {e}", path.display()))?;
    let sess: CliSession =
        serde_json::from_str(&raw).map_err(|e| eyre::eyre!("parse {}: {e}", path.display()))?;
    Ok(Some(sess))
}

pub fn save(sess: &CliSession) -> eyre::Result<()> {
    let path = session_path()?;
    let raw =
        serde_json::to_string_pretty(sess).map_err(|e| eyre::eyre!("serialize session: {e}"))?;
    std::fs::write(&path, raw).map_err(|e| eyre::eyre!("write {}: {e}", path.display()))?;
    Ok(())
}

pub fn clear() -> eyre::Result<()> {
    let path = session_path()?;
    if path.exists() {
        std::fs::remove_file(&path).map_err(|e| eyre::eyre!("remove {}: {e}", path.display()))?;
    }
    Ok(())
}

/// Local-sqlite auth db path. Mirrors `task-server`'s
/// `default_auth_db_path` — CLI + server share one
/// `auth.sqlite` so an org created server-side is visible to
/// `task auth login`.
pub fn default_auth_db_path() -> eyre::Result<PathBuf> {
    let base = match std::env::var("XDG_DATA_HOME") {
        Ok(v) if !v.is_empty() => PathBuf::from(v),
        _ => {
            let home = std::env::var("HOME")
                .map_err(|_| eyre::eyre!("neither XDG_DATA_HOME nor HOME is set"))?;
            PathBuf::from(home).join(".local").join("share")
        }
    };
    let dir = base.join("task-server");
    std::fs::create_dir_all(&dir).map_err(|e| eyre::eyre!("create {}: {e}", dir.display()))?;
    Ok(dir.join("auth.sqlite"))
}

/// Auth secret. Must match `task-server`'s
/// `DEFAULT_AUTH_SECRET` since both processes hash + verify
/// session tokens against the same value.
pub const DEFAULT_AUTH_SECRET: &str = "task-server-auth-dev-secret-32+!";
