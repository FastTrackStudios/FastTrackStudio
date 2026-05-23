//! [`DataRoot`] + [`OrgRoot`] — the layout resolver.
//!
//! ```text
//! <data_root>/                        # `DataRoot`
//! ├── server-key.ed25519              # cross-org blob signing keypair
//! └── orgs/
//!     ├── codywright/                 # `OrgRoot` (one per org)
//!     │   ├── org.toml                # `OrgManifest`
//!     │   ├── auth.sqlite
//!     │   ├── identity.sqlite         # only when `is_home = true`
//!     │   ├── timer.sqlite
//!     │   ├── finance.sqlite
//!     │   ├── vault/
//!     │   └── attachments/
//!     ├── fasttrackstudios/
//!     └── ...
//! ```
//!
//! Phase 1 wires these resolvers but doesn't yet move
//! existing data — the server + CLI keep working against
//! their current `$XDG_DATA_HOME/task-server/*.sqlite` paths
//! until PR 2 migrates them through `OrgRoot`. See
//! `plans/federated-task-platform.md` Phase 1.

use std::path::{Path, PathBuf};

use crate::manifest::{OrgManifest, ParseError};

#[derive(Debug, thiserror::Error)]
pub enum RootError {
    #[error("resolve data root: {0}")]
    Resolve(String),
    #[error("create {path}: {source}")]
    Create {
        path: String,
        #[source]
        source: std::io::Error,
    },
    #[error("scan {path}: {source}")]
    Scan {
        path: String,
        #[source]
        source: std::io::Error,
    },
    #[error("org `{slug}` already exists at {path}")]
    AlreadyExists { slug: String, path: String },
    #[error("invalid slug `{slug}`: {reason}")]
    InvalidSlug { slug: String, reason: &'static str },
    #[error(transparent)]
    Manifest(#[from] ParseError),
}

/// Top-level data root. One per task-server process. Holds
/// `orgs/` plus any cross-org artifacts (server keypair,
/// future federation discovery cache).
#[derive(Debug, Clone)]
pub struct DataRoot {
    path: PathBuf,
}

impl DataRoot {
    /// Wrap an explicit path. Caller is responsible for
    /// ensuring it exists (or calling [`Self::ensure`]).
    #[must_use]
    pub fn new(path: PathBuf) -> Self {
        Self { path }
    }

    /// Resolve the canonical default:
    /// `$TASK_DATA_ROOT` → `$XDG_DATA_HOME/task-server` →
    /// `$HOME/.local/share/task-server`. Picks an env-var
    /// override path first so tests (and per-org server
    /// instances) can point at temp dirs.
    pub fn from_env() -> Result<Self, RootError> {
        if let Ok(explicit) = std::env::var("TASK_DATA_ROOT") {
            if !explicit.is_empty() {
                return Ok(Self::new(PathBuf::from(explicit)));
            }
        }
        let base = match std::env::var("XDG_DATA_HOME") {
            Ok(v) if !v.is_empty() => PathBuf::from(v),
            _ => {
                let home = std::env::var("HOME").map_err(|_| {
                    RootError::Resolve(
                        "neither TASK_DATA_ROOT, XDG_DATA_HOME nor HOME is set".into(),
                    )
                })?;
                PathBuf::from(home).join(".local").join("share")
            }
        };
        Ok(Self::new(base.join("task-server")))
    }

    /// Path to the root itself (`<data_root>`).
    #[must_use]
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// `<data_root>/orgs/`.
    #[must_use]
    pub fn orgs_dir(&self) -> PathBuf {
        self.path.join("orgs")
    }

    /// `<data_root>/server-key.ed25519` — the blob signing
    /// keypair, shared across hosted orgs.
    #[must_use]
    pub fn server_keypair_path(&self) -> PathBuf {
        self.path.join("server-key.ed25519")
    }

    /// Create `<data_root>/` + `orgs/` if missing. Idempotent.
    pub fn ensure(&self) -> Result<(), RootError> {
        for p in [&self.path, &self.orgs_dir()] {
            std::fs::create_dir_all(p).map_err(|source| RootError::Create {
                path: p.display().to_string(),
                source,
            })?;
        }
        Ok(())
    }

    /// Resolver for a single org by slug. Does **not** check
    /// that the org exists on disk — use [`OrgRoot::manifest`]
    /// or [`Self::load_org`] for that.
    #[must_use]
    pub fn org(&self, slug: impl Into<String>) -> OrgRoot {
        let slug = slug.into();
        OrgRoot {
            path: self.orgs_dir().join(&slug),
            slug,
        }
    }

    /// Scaffold a fresh org dir. Refuses to overwrite an
    /// existing one — that's a federation-breaking change a
    /// human should confirm.
    pub fn init_org(
        &self,
        slug: &str,
        display_name: &str,
        is_home: bool,
    ) -> Result<OrgRoot, RootError> {
        validate_slug(slug)?;
        self.ensure()?;
        let org = self.org(slug);
        if org.path().exists() {
            return Err(RootError::AlreadyExists {
                slug: slug.to_owned(),
                path: org.path().display().to_string(),
            });
        }
        std::fs::create_dir_all(org.path()).map_err(|source| RootError::Create {
            path: org.path().display().to_string(),
            source,
        })?;
        let manifest = OrgManifest::new(slug, display_name, is_home);
        manifest.write_to_dir(org.path())?;
        // Sub-dirs the downstream features will use. Created
        // up-front so a fresh `OrgRoot` is immediately usable
        // — no "first write creates the dir" surprises.
        for sub in ["vault", "attachments"] {
            let p = org.path().join(sub);
            std::fs::create_dir_all(&p).map_err(|source| RootError::Create {
                path: p.display().to_string(),
                source,
            })?;
        }
        Ok(org)
    }

    /// Load an existing org by slug. Returns
    /// `RootError::Manifest(ParseError::Read{..})` when the
    /// org doesn't exist on disk.
    pub fn load_org(&self, slug: &str) -> Result<(OrgRoot, OrgManifest), RootError> {
        let org = self.org(slug);
        let manifest = OrgManifest::load_from_dir(org.path())?;
        Ok((org, manifest))
    }

    /// Enumerate every org dir under `orgs/` that has a
    /// loadable `org.toml`. Dirs without a manifest are
    /// silently skipped — they may be partial scaffolds or
    /// unrelated files.
    pub fn scan_orgs(&self) -> Result<Vec<(OrgRoot, OrgManifest)>, RootError> {
        let dir = self.orgs_dir();
        if !dir.exists() {
            return Ok(Vec::new());
        }
        let entries = std::fs::read_dir(&dir).map_err(|source| RootError::Scan {
            path: dir.display().to_string(),
            source,
        })?;
        let mut out = Vec::new();
        for entry in entries.flatten() {
            if !entry.file_type().is_ok_and(|t| t.is_dir()) {
                continue;
            }
            let Some(slug) = entry.file_name().to_str().map(str::to_owned) else {
                continue;
            };
            let org = self.org(&slug);
            match OrgManifest::load_from_dir(org.path()) {
                Ok(m) => out.push((org, m)),
                Err(_) => continue,
            }
        }
        out.sort_by(|a, b| a.0.slug().cmp(b.0.slug()));
        Ok(out)
    }
}

/// One org's on-disk root. Pure path resolver — no I/O until
/// you call a method that actually touches the file system.
#[derive(Debug, Clone)]
pub struct OrgRoot {
    slug: String,
    path: PathBuf,
}

impl OrgRoot {
    #[must_use]
    pub fn slug(&self) -> &str {
        &self.slug
    }

    #[must_use]
    pub fn path(&self) -> &Path {
        &self.path
    }

    #[must_use]
    pub fn manifest_path(&self) -> PathBuf {
        self.path.join("org.toml")
    }

    #[must_use]
    pub fn auth_db(&self) -> PathBuf {
        self.path.join("auth.sqlite")
    }

    #[must_use]
    pub fn identity_db(&self) -> PathBuf {
        self.path.join("identity.sqlite")
    }

    #[must_use]
    pub fn timer_db(&self) -> PathBuf {
        self.path.join("timer.sqlite")
    }

    #[must_use]
    pub fn finance_db(&self) -> PathBuf {
        self.path.join("finance.sqlite")
    }

    #[must_use]
    pub fn vault_dir(&self) -> PathBuf {
        self.path.join("vault")
    }

    #[must_use]
    pub fn attachments_dir(&self) -> PathBuf {
        self.path.join("attachments")
    }

    pub fn manifest(&self) -> Result<OrgManifest, ParseError> {
        OrgManifest::load_from_dir(&self.path)
    }
}

/// Slug rules: lowercase ASCII, digits, `-`. Non-empty,
/// max 64 chars. Matches the `/org/<slug>/...` URL contract;
/// also keeps directory names portable across filesystems.
fn validate_slug(slug: &str) -> Result<(), RootError> {
    if slug.is_empty() {
        return Err(RootError::InvalidSlug {
            slug: slug.to_owned(),
            reason: "must not be empty",
        });
    }
    if slug.len() > 64 {
        return Err(RootError::InvalidSlug {
            slug: slug.to_owned(),
            reason: "must be ≤ 64 chars",
        });
    }
    for c in slug.chars() {
        if !(c.is_ascii_lowercase() || c.is_ascii_digit() || c == '-') {
            return Err(RootError::InvalidSlug {
                slug: slug.to_owned(),
                reason: "only [a-z0-9-] allowed",
            });
        }
    }
    if slug.starts_with('-') || slug.ends_with('-') {
        return Err(RootError::InvalidSlug {
            slug: slug.to_owned(),
            reason: "must not start or end with `-`",
        });
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn init_scan_load_cycle() {
        let tmp = tempfile::tempdir().unwrap();
        let root = DataRoot::new(tmp.path().to_owned());
        root.init_org("codywright", "Cody Wright", true).unwrap();
        root.init_org("fasttrackstudios", "FastTrackStudios", false)
            .unwrap();
        let scanned = root.scan_orgs().unwrap();
        assert_eq!(scanned.len(), 2);
        assert_eq!(scanned[0].0.slug(), "codywright");
        assert_eq!(scanned[1].0.slug(), "fasttrackstudios");
        assert!(scanned[0].1.is_home);
        let (_, m) = root.load_org("fasttrackstudios").unwrap();
        assert_eq!(m.display_name, "FastTrackStudios");
    }

    #[test]
    fn init_twice_rejected() {
        let tmp = tempfile::tempdir().unwrap();
        let root = DataRoot::new(tmp.path().to_owned());
        root.init_org("dup", "x", false).unwrap();
        let err = root.init_org("dup", "y", false).unwrap_err();
        assert!(matches!(err, RootError::AlreadyExists { .. }));
    }

    #[test]
    fn invalid_slugs_rejected() {
        let tmp = tempfile::tempdir().unwrap();
        let root = DataRoot::new(tmp.path().to_owned());
        for bad in [
            "",
            "UPPER",
            "with space",
            "starts-",
            "-ends",
            "tooo".repeat(20).as_str(),
        ] {
            assert!(
                root.init_org(bad, "x", false).is_err(),
                "expected reject: {bad:?}"
            );
        }
    }

    #[test]
    fn paths_compose_correctly() {
        let tmp = tempfile::tempdir().unwrap();
        let root = DataRoot::new(tmp.path().to_owned());
        let org = root.init_org("cody", "Cody", false).unwrap();
        assert_eq!(org.auth_db().file_name().unwrap(), "auth.sqlite");
        assert_eq!(org.timer_db().file_name().unwrap(), "timer.sqlite");
        assert!(org.vault_dir().exists());
        assert!(org.attachments_dir().exists());
    }
}
