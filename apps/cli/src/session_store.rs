//! Persistent CLI session file.
//!
//! Written by `task auth login`, read by every subcommand
//! that needs an authenticated `user_id` / `org_id`. Lives at
//! `$XDG_DATA_HOME/task/session.json` (override via
//! `TASK_SESSION_FILE`).
//!
//! ## Shape — multi-server
//!
//! `home` is the identity-anchor org (the user's personal
//! org). `active` is the org subsequent commands run against
//! (timer, finance, auth db lookups). `servers` is a map
//! keyed by org slug holding per-server credentials. This
//! shape is forward-compatible with Phase 3 federation —
//! linked remote orgs land in the same map under their slug.
//!
//! ## Back-compat
//!
//! [`load`] silently upgrades the pre-PR-2 single-org shape
//! `{token, user_id, email, org_id}` to the new shape under
//! the slug `"default"`. The upgrade rewrites the file on
//! first successful save so the legacy form is only ever
//! tolerated once.

use std::collections::BTreeMap;
use std::path::PathBuf;

use serde::{Deserialize, Serialize};

/// Per-org server entry. One row per (server-the-CLI-has-
/// signed-into × org-it-targets). For Phase 2 the only entry
/// is the local `default` (or named) org; Phase 3 federation
/// adds remote URL entries keyed by slug.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ServerEntry {
    /// Where this org lives. `"local"` for an org served by
    /// the local `task-server` process / opened directly via
    /// SQLite by the CLI. Remote orgs use their federation
    /// URL.
    pub url: String,
    /// Authenticated user id (from architect-auth) within
    /// this org.
    pub user_id: uuid::Uuid,
    /// Email captured at sign-in. Cached for `task auth
    /// whoami`; not used for routing.
    pub email: String,
    /// Bearer token for the active architect-auth session.
    /// Encrypted-at-rest is a Phase 3 concern.
    pub token: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CliSession {
    /// Identity anchor — the user's personal/home org. Empty
    /// until first login from a home server.
    #[serde(default)]
    pub home: String,
    /// Currently-active org. Subsequent commands operate
    /// against this slug's resolver paths.
    pub active: String,
    /// Slug → server entry. Populated by `task auth login`
    /// per signed-into org.
    pub servers: BTreeMap<String, ServerEntry>,
}

/// Pre-PR-2 single-org shape. Kept as a deserialize-only
/// shim so [`load`] can upgrade legacy session files in
/// place. Don't reference outside this module. `org_id` here
/// was the architect-auth membership id — different concept
/// from the on-disk org slug; we drop it on upgrade and let
/// `task auth org use` re-set it.
#[derive(Debug, Deserialize)]
#[allow(dead_code)]
struct LegacySession {
    token: String,
    user_id: uuid::Uuid,
    email: String,
    org_id: Option<uuid::Uuid>,
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
    // Try new shape first.
    if let Ok(sess) = serde_json::from_str::<CliSession>(&raw) {
        return Ok(Some(sess));
    }
    // Fall back to legacy shape and upgrade.
    let legacy: LegacySession = serde_json::from_str(&raw).map_err(|e| {
        eyre::eyre!(
            "parse {} (neither new nor legacy shape): {e}",
            path.display()
        )
    })?;
    let slug = "default".to_owned();
    let mut servers = BTreeMap::new();
    servers.insert(
        slug.clone(),
        ServerEntry {
            url: "local".into(),
            user_id: legacy.user_id,
            email: legacy.email,
            token: legacy.token,
        },
    );
    let sess = CliSession {
        home: slug.clone(),
        active: slug,
        servers,
    };
    // Persist the upgraded shape so the legacy form is only
    // ever tolerated once.
    save(&sess)?;
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

impl CliSession {
    /// Lookup helper. `None` if the active slug has no entry
    /// (e.g. a stale `active` after the entry was removed).
    #[must_use]
    pub fn active_server(&self) -> Option<&ServerEntry> {
        self.servers.get(&self.active)
    }
}

/// Auth secret. Must match `task-server`'s
/// `DEFAULT_AUTH_SECRET` since both processes hash + verify
/// session tokens against the same value.
pub const DEFAULT_AUTH_SECRET: &str = "task-server-auth-dev-secret-32+!";
