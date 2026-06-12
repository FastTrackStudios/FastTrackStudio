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
//! ## Storage
//!
//! Bearer tokens are persisted through architect-auth's
//! client kit ([`FileTokenStore`]) — atomic temp-file +
//! rename writes, `0600` on unix — one `StoredSession` JSON
//! per org slug under a `…-tokens/` directory next to the
//! session file. `session.json` itself is reduced to the
//! non-secret routing document (`home` / `active` / slug →
//! url map).
//!
//! ## Back-compat
//!
//! [`load`] silently upgrades both older on-disk shapes — the
//! pre-PR-2 single-org `{token, user_id, email, org_id}` and
//! the combined multi-server document that embedded tokens
//! directly in `session.json` — to the split layout. The
//! upgrade rewrites the files on first load so the legacy
//! forms are only ever tolerated once.

use std::collections::BTreeMap;
use std::path::PathBuf;

use architect_auth::client::{FileTokenStore, StoredSession, TokenStore as _};
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
    /// Stored via [`FileTokenStore`] (atomic write, `0600`),
    /// never in the routing document.
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

/// What `session.json` holds on disk: the non-secret routing
/// state. Tokens (plus the cached user id / email) live in
/// per-slug [`FileTokenStore`] files. Don't reference outside
/// this module.
#[derive(Debug, Serialize, Deserialize)]
struct RoutingDoc {
    #[serde(default)]
    home: String,
    active: String,
    /// Slug → server url. The rest of the entry is rebuilt
    /// from the slug's token file.
    servers: BTreeMap<String, String>,
}

/// Pre-split multi-server shape: tokens embedded directly in
/// `session.json`. Deserialize-only shim so [`load`] can
/// upgrade in place. Don't reference outside this module.
#[derive(Debug, Deserialize)]
struct EmbeddedSession {
    #[serde(default)]
    home: String,
    active: String,
    servers: BTreeMap<String, ServerEntry>,
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

/// Directory holding the per-slug token files, derived from
/// the session path so the `TASK_SESSION_FILE` override moves
/// both together: `session.json` → `session-tokens/`.
fn tokens_dir() -> eyre::Result<PathBuf> {
    let path = session_path()?;
    let stem = path.file_stem().map_or_else(
        || "session".to_owned(),
        |s| s.to_string_lossy().into_owned(),
    );
    Ok(path.with_file_name(format!("{stem}-tokens")))
}

/// The [`FileTokenStore`] for one org slug.
fn token_store(slug: &str) -> eyre::Result<FileTokenStore> {
    Ok(FileTokenStore::new(
        tokens_dir()?.join(format!("{slug}.json")),
    ))
}

fn entry_to_stored(entry: &ServerEntry) -> StoredSession {
    StoredSession::new(entry.token.clone())
        .with_user_id(entry.user_id.to_string())
        .with_email(entry.email.clone())
}

fn entry_from_stored(slug: &str, url: String, stored: StoredSession) -> eyre::Result<ServerEntry> {
    let user_id = stored
        .user_id
        .as_deref()
        .ok_or_else(|| eyre::eyre!("token file for `{slug}` has no user_id"))?;
    let user_id = user_id
        .parse::<uuid::Uuid>()
        .map_err(|e| eyre::eyre!("token file for `{slug}`: bad user_id `{user_id}`: {e}"))?;
    Ok(ServerEntry {
        url,
        user_id,
        email: stored.email.unwrap_or_default(),
        token: stored.token,
    })
}

pub fn load() -> eyre::Result<Option<CliSession>> {
    let path = session_path()?;
    if !path.exists() {
        return Ok(None);
    }
    let raw =
        std::fs::read_to_string(&path).map_err(|e| eyre::eyre!("read {}: {e}", path.display()))?;
    // Current shape: routing doc + per-slug token files.
    if let Ok(doc) = serde_json::from_str::<RoutingDoc>(&raw) {
        let mut servers = BTreeMap::new();
        for (slug, url) in doc.servers {
            // A missing token file means that slug was signed
            // out out-of-band — drop the entry rather than
            // failing every command.
            let Some(stored) = token_store(&slug)?
                .load()
                .map_err(|e| eyre::eyre!("load token for `{slug}`: {e}"))?
            else {
                continue;
            };
            servers.insert(slug.clone(), entry_from_stored(&slug, url, stored)?);
        }
        return Ok(Some(CliSession {
            home: doc.home,
            active: doc.active,
            servers,
        }));
    }
    // Pre-split multi-server shape (tokens embedded in
    // session.json): upgrade to the split layout.
    if let Ok(embedded) = serde_json::from_str::<EmbeddedSession>(&raw) {
        let sess = CliSession {
            home: embedded.home,
            active: embedded.active,
            servers: embedded.servers,
        };
        save(&sess)?;
        return Ok(Some(sess));
    }
    // Fall back to legacy single-org shape and upgrade.
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
    let dir = tokens_dir()?;
    std::fs::create_dir_all(&dir).map_err(|e| eyre::eyre!("create {}: {e}", dir.display()))?;
    // Tokens first — one FileTokenStore (atomic write, 0600)
    // per slug.
    for (slug, entry) in &sess.servers {
        token_store(slug)?
            .save(&entry_to_stored(entry))
            .map_err(|e| eyre::eyre!("save token for `{slug}`: {e}"))?;
    }
    // Prune token files for slugs no longer in the session
    // (logout removes the entry then calls `save`).
    for dirent in std::fs::read_dir(&dir).map_err(|e| eyre::eyre!("read {}: {e}", dir.display()))? {
        let dirent = dirent.map_err(|e| eyre::eyre!("read {}: {e}", dir.display()))?;
        let name = dirent.file_name();
        let Some(slug) = name
            .to_string_lossy()
            .strip_suffix(".json")
            .map(str::to_owned)
        else {
            continue;
        };
        if !sess.servers.contains_key(&slug) {
            token_store(&slug)?
                .clear()
                .map_err(|e| eyre::eyre!("clear token for `{slug}`: {e}"))?;
        }
    }
    // Then the non-secret routing doc, atomically (temp +
    // rename) so a crash never leaves a truncated file.
    let doc = RoutingDoc {
        home: sess.home.clone(),
        active: sess.active.clone(),
        servers: sess
            .servers
            .iter()
            .map(|(slug, entry)| (slug.clone(), entry.url.clone()))
            .collect(),
    };
    let raw =
        serde_json::to_string_pretty(&doc).map_err(|e| eyre::eyre!("serialize session: {e}"))?;
    let tmp = path.with_extension("json.tmp");
    std::fs::write(&tmp, raw).map_err(|e| eyre::eyre!("write {}: {e}", tmp.display()))?;
    std::fs::rename(&tmp, &path)
        .map_err(|e| eyre::eyre!("rename {} -> {}: {e}", tmp.display(), path.display()))?;
    Ok(())
}

pub fn clear() -> eyre::Result<()> {
    let path = session_path()?;
    if path.exists() {
        std::fs::remove_file(&path).map_err(|e| eyre::eyre!("remove {}: {e}", path.display()))?;
    }
    let dir = tokens_dir()?;
    match std::fs::remove_dir_all(&dir) {
        Ok(()) => {}
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
        Err(e) => return Err(eyre::eyre!("remove {}: {e}", dir.display())),
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
