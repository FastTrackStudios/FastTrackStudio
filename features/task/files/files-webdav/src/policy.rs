//! Per-root WebDAV policy — the "a per-root policy can hide a root from
//! WebDAV" half of issue #274.
//!
//! Persisted as JSON beside the Files registry
//! (`<data_dir>/webdav-policy.json`, next to `roots.json`), for the same
//! reason the registry lives there: the WebDAV bridge is a *view* of an
//! org's roots, so its policy belongs with that org's Files data rather
//! than in a server-global config.
//!
//! **Why a file and not an RPC verb.** Hiding a root is an
//! operator/owner-tier decision on a compat surface, and
//! [`files_proto::FilesService`] is a shared wire contract that
//! concurrent tickets are also extending — adding a method here would
//! be a cross-ticket collision for a knob with exactly one production
//! caller. The file is the surface: an operator edits it, and the next
//! request picks it up ([`WebdavPolicy::reload_if_changed`] watches its
//! mtime), no restart. When the Vault-entity integration lands and a
//! root carries real per-root settings, this collapses into that.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use std::time::SystemTime;

use uuid::Uuid;

/// On-disk shape of `webdav-policy.json`. One key today; a struct
/// (rather than a bare array) so later per-root knobs — read-only,
/// per-root principal mapping — are additive rather than a format
/// break.
#[derive(Debug, Default, serde::Serialize, serde::Deserialize)]
struct PolicyFile {
    /// Roots that must not appear on, or be reachable through, the
    /// WebDAV bridge. Ids not matching any registered root are kept
    /// verbatim (a root may be re-registered later).
    #[serde(default)]
    hidden: BTreeSet<Uuid>,
}

#[derive(Debug)]
struct Cached {
    file: PolicyFile,
    /// mtime the cache was read at, or `None` when the file did not
    /// exist — either way a *change* invalidates.
    stamp: Option<SystemTime>,
}

/// Which of an org's File Roots the WebDAV bridge may expose.
#[derive(Debug)]
pub struct WebdavPolicy {
    path: PathBuf,
    cached: Mutex<Cached>,
}

impl WebdavPolicy {
    /// Open (or default-initialize) the policy for a Files data
    /// directory. A missing file means "nothing hidden" and is not
    /// written out — the empty policy has no on-disk representation to
    /// maintain.
    pub fn open(data_dir: impl Into<PathBuf>) -> std::io::Result<Self> {
        let path = data_dir.into().join("webdav-policy.json");
        let policy = Self {
            path,
            cached: Mutex::new(Cached {
                file: PolicyFile::default(),
                // Force the first `reload_if_changed` to actually read:
                // a real stamp is only ever `Some`, and `None` here is
                // indistinguishable from "file absent", which is the
                // state we want to re-check.
                stamp: None,
            }),
        };
        policy.reload_if_changed();
        Ok(policy)
    }

    fn stamp(&self) -> Option<SystemTime> {
        std::fs::metadata(&self.path)
            .ok()
            .and_then(|m| m.modified().ok())
    }

    /// Re-read the policy file when its mtime moved since the last read
    /// — how an operator's edit reaches a running server without a
    /// restart. A malformed file is *ignored* (logged, previous policy
    /// kept): a typo must not silently un-hide a root that was
    /// deliberately hidden.
    ///
    /// The file read happens *outside* the lock. The steady state is
    /// "nothing changed", where this costs one `stat` and no lock at
    /// all; only a genuine change takes the lock, and never while
    /// blocked on the filesystem.
    fn reload_if_changed(&self) {
        let stamp = self.stamp();
        {
            let cached = self.cached.lock().expect("webdav policy lock poisoned");
            if stamp == cached.stamp && stamp.is_some() {
                return;
            }
        }
        let file = match std::fs::read(&self.path) {
            Ok(bytes) => match serde_json::from_slice::<PolicyFile>(&bytes) {
                Ok(parsed) => parsed,
                Err(e) => {
                    tracing::warn!(
                        target: "files_webdav::policy",
                        path = %self.path.display(),
                        error = %e,
                        "webdav policy file is malformed — keeping the previous policy",
                    );
                    return;
                }
            },
            // Absent (or unreadable) = the empty policy.
            Err(_) => PolicyFile::default(),
        };
        let mut cached = self.cached.lock().expect("webdav policy lock poisoned");
        cached.file = file;
        cached.stamp = stamp;
    }

    fn persist(&self, file: &PolicyFile) -> std::io::Result<()> {
        if let Some(parent) = self.path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        let bytes = serde_json::to_vec_pretty(file)?;
        std::fs::write(&self.path, bytes)
    }

    /// Every hidden root id, as one snapshot. This is the shape callers
    /// want: filtering a root list needs *one* policy read, not one per
    /// root — a mount with twenty roots would otherwise `stat` the
    /// policy file twenty times per request.
    #[must_use]
    pub fn hidden_set(&self) -> BTreeSet<Uuid> {
        self.reload_if_changed();
        self.cached
            .lock()
            .expect("webdav policy lock poisoned")
            .file
            .hidden
            .clone()
    }

    /// May the bridge expose `root_id`? Picks up an operator's edit to
    /// the policy file first.
    #[must_use]
    pub fn is_visible(&self, root_id: Uuid) -> bool {
        !self.hidden_set().contains(&root_id)
    }

    /// Hide (or un-hide) a root from the WebDAV bridge. Idempotent.
    pub fn set_hidden(&self, root_id: Uuid, hidden: bool) -> std::io::Result<()> {
        self.reload_if_changed();
        let mut cached = self.cached.lock().expect("webdav policy lock poisoned");
        let changed = if hidden {
            cached.file.hidden.insert(root_id)
        } else {
            cached.file.hidden.remove(&root_id)
        };
        if !changed {
            return Ok(());
        }
        self.persist(&cached.file)?;
        cached.stamp = self.stamp();
        Ok(())
    }

    /// Every root id this policy hides, sorted.
    #[must_use]
    pub fn hidden(&self) -> Vec<Uuid> {
        self.hidden_set().into_iter().collect()
    }

    /// The policy file this instance reads and writes — the operator's
    /// editing surface.
    #[must_use]
    pub fn path(&self) -> &Path {
        &self.path
    }
}
