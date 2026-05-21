//! `VaultSync` backend — the canonical filesystem implementation
//! of [`vault_proto::VaultSync`].
//!
//! One [`Backend`] instance serves one-or-more vault roots, each
//! addressed by an opaque `vault_id`. A desktop app typically has
//! one backend with one vault registered; a server can register
//! many. Both shapes go through the same wire trait + the same
//! `architect::serve` mount point, so a remote client doesn't
//! know which backend it's talking to.
//!
//! Conflict policy: **last-writer-wins with `IfMatch`**, mirroring
//! the proto:
//! - [`IfMatch::CreateOnly`] — fail if the file exists.
//! - [`IfMatch::Sha`]        — fail unless the server's current
//!   sha matches.
//! - [`IfMatch::Force`]      — unconditional. Only safe on the
//!   first push of a brand-new vault.
//!
//! Live events: every successful PUT / DELETE publishes a
//! [`VaultEvent`] on the per-vault `broadcast::Sender`. A
//! subscribing client receives every committed change; if the
//! channel laps (capacity 256), the server sends
//! [`VaultEvent::Resync`] so the client knows to re-pull the
//! manifest.
//!
//! Disk-side externalities (file changes from outside the
//! backend) are *not* forwarded here — for that, a higher-level
//! integration plumbs [`crate::watcher`] events into the same
//! channel. The pure-RPC subscriber only sees changes the
//! backend itself applied.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use architect::HasDispatcher;
use architect::dispatch::TokioBlockingDispatcher;
use architect::vox;
use sha2::{Digest, Sha256};
use tokio::sync::{RwLock, broadcast};
use vault_proto::{
    FileBytes, IfMatch, Manifest, ManifestEntry, PutAck, VaultEvent, VaultSync, VaultSyncError,
};

/// How the backend resolves `vault_id` → on-disk path.
#[derive(Debug, Clone)]
enum Layout {
    /// Each `vault_id` maps to an explicit absolute path.
    /// Unknown ids return [`VaultSyncError::NotFound`]. Right
    /// for a desktop app that opens a finite, user-chosen set
    /// of vaults.
    Explicit(HashMap<String, PathBuf>),
    /// All vaults live as subdirectories under one parent
    /// directory. Any `vault_id` is accepted; the directory is
    /// created on the first write. Right for a multi-tenant
    /// server hosting many client vaults.
    UnderParent(PathBuf),
}

/// Filesystem-backed `VaultSync` implementation. Cheap to
/// `Clone` — internals are `Arc`d.
///
/// Two layouts (pick one per backend instance):
/// - [`Backend::single`] / [`Backend::with_roots`]: explicit
///   `vault_id → path` registry. Unknown ids fail.
/// - [`Backend::under_parent`]: open-ended, one subdir per
///   vault under a shared parent. Unknown ids auto-create.
#[derive(Clone)]
pub struct Backend {
    layout: Layout,
    /// Coarse global write lock. Reads bypass it; writes
    /// `lock` → `read-sha` → `write` → `unlock`. Refine to a
    /// per-vault `RwLock` if write contention shows up.
    write_lock: Arc<std::sync::Mutex<()>>,
    /// Per-vault broadcast sender, lazily created on first
    /// `subscribe`. Capacity 256: rapid bursts coalesce
    /// client-side via [`VaultEvent::Resync`].
    channels: Arc<RwLock<HashMap<String, broadcast::Sender<VaultEvent>>>>,
}

impl Backend {
    /// Build a backend serving a single vault under `root` as
    /// `vault_id`. The directory is created if missing.
    pub fn single(vault_id: impl Into<String>, root: PathBuf) -> std::io::Result<Self> {
        std::fs::create_dir_all(&root)?;
        let mut roots = HashMap::with_capacity(1);
        roots.insert(vault_id.into(), root);
        Ok(Self::with_roots(roots))
    }

    /// Build a backend from a pre-built `vault_id → root` map.
    /// Caller is responsible for creating the directories.
    pub fn with_roots(roots: HashMap<String, PathBuf>) -> Self {
        Self::from_layout(Layout::Explicit(roots))
    }

    /// Build a multi-tenant backend where every `vault_id`
    /// resolves to `{parent}/{vault_id}/`. Subdirs are created
    /// on demand on the first write. `parent` itself is
    /// created up front.
    pub fn under_parent(parent: PathBuf) -> std::io::Result<Self> {
        std::fs::create_dir_all(&parent)?;
        Ok(Self::from_layout(Layout::UnderParent(parent)))
    }

    fn from_layout(layout: Layout) -> Self {
        Self {
            layout,
            write_lock: Arc::new(std::sync::Mutex::new(())),
            channels: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Resolve `vault_id` to an absolute root path. Returns
    /// [`VaultSyncError::NotFound`] only in `Explicit` mode
    /// when the id is unregistered; `UnderParent` always
    /// succeeds.
    fn root(&self, vault_id: &str) -> Result<PathBuf, VaultSyncError> {
        match &self.layout {
            Layout::Explicit(map) => map.get(vault_id).cloned().ok_or(VaultSyncError::NotFound),
            Layout::UnderParent(parent) => Ok(parent.join(vault_id)),
        }
    }

    /// Whether `vault_id` would resolve to a path this backend
    /// serves. `Explicit`: present in the registry.
    /// `UnderParent`: always true.
    fn knows(&self, vault_id: &str) -> bool {
        match &self.layout {
            Layout::Explicit(map) => map.contains_key(vault_id),
            Layout::UnderParent(_) => true,
        }
    }

    fn file_path(&self, vault_id: &str, rel: &str) -> Result<PathBuf, VaultSyncError> {
        // Refuse anything that could traverse out of the vault
        // dir. Reject `..`, absolute paths, and Windows drive
        // letters defensively.
        if rel.is_empty()
            || rel.starts_with('/')
            || rel.starts_with('\\')
            || rel.contains("..")
            || rel.contains(':')
        {
            return Err(VaultSyncError::BadPath);
        }
        let root = self.root(vault_id)?;
        Ok(root.join(rel))
    }

    /// Get-or-create the per-vault broadcast sender. Async —
    /// uses tokio's `RwLock` so it interoperates with the async
    /// `subscribe` impl.
    pub async fn channel(&self, vault_id: &str) -> broadcast::Sender<VaultEvent> {
        if let Some(tx) = self.channels.read().await.get(vault_id) {
            return tx.clone();
        }
        let mut chans = self.channels.write().await;
        if let Some(tx) = chans.get(vault_id) {
            return tx.clone();
        }
        let (tx, _rx) = broadcast::channel::<VaultEvent>(256);
        chans.insert(vault_id.to_string(), tx.clone());
        tx
    }

    /// Sync sibling of [`Self::channel`] for use inside the
    /// sync trait methods. `TokioBlockingDispatcher` already
    /// runs us on a blocking-pool thread, so the
    /// `blocking_read`/`blocking_write` variants are safe.
    fn channel_blocking(&self, vault_id: &str) -> broadcast::Sender<VaultEvent> {
        if let Some(tx) = self.channels.blocking_read().get(vault_id) {
            return tx.clone();
        }
        let mut chans = self.channels.blocking_write();
        if let Some(tx) = chans.get(vault_id) {
            return tx.clone();
        }
        let (tx, _rx) = broadcast::channel::<VaultEvent>(256);
        chans.insert(vault_id.to_string(), tx.clone());
        tx
    }
}

impl HasDispatcher for Backend {
    type Dispatcher = TokioBlockingDispatcher;
    fn dispatcher(&self) -> Self::Dispatcher {
        TokioBlockingDispatcher
    }
}

impl VaultSync for Backend {
    fn manifest(&self, vault_id: &str) -> Result<Manifest, VaultSyncError> {
        let dir = self.root(vault_id)?;
        if !dir.exists() {
            return Ok(Manifest {
                vault_id: vault_id.to_string(),
                files: Vec::new(),
            });
        }
        let mut entries = Vec::new();
        collect(&dir, &dir, &mut entries)?;
        Ok(Manifest {
            vault_id: vault_id.to_string(),
            files: entries,
        })
    }

    fn get_file(&self, vault_id: &str, path: &str) -> Result<FileBytes, VaultSyncError> {
        let abs = self.file_path(vault_id, path)?;
        if !abs.exists() {
            return Err(VaultSyncError::NotFound);
        }
        let bytes = std::fs::read(&abs).map_err(io_err)?;
        Ok(FileBytes(bytes))
    }

    fn put_file(
        &self,
        vault_id: &str,
        path: &str,
        bytes: Vec<u8>,
        if_match: IfMatch,
    ) -> Result<PutAck, VaultSyncError> {
        let abs = self.file_path(vault_id, path)?;
        let _g = self
            .write_lock
            .lock()
            .expect("vault::sync write_lock poisoned");
        let existing_sha = if abs.exists() {
            let bytes = std::fs::read(&abs).map_err(io_err)?;
            Some(sha256_hex(&bytes))
        } else {
            None
        };
        match (&if_match, existing_sha.as_deref()) {
            (IfMatch::CreateOnly, Some(_)) => return Err(conflict(&abs, existing_sha.as_deref())),
            (IfMatch::Sha(want), Some(have)) if want != have => {
                return Err(conflict(&abs, existing_sha.as_deref()));
            }
            _ => {}
        }
        if let Some(parent) = abs.parent() {
            std::fs::create_dir_all(parent).map_err(io_err)?;
        }
        // Atomic write via temp+rename so concurrent reads never
        // see a half-written body.
        let tmp = abs.with_extension(format!(
            "{}.tmp.{}",
            abs.extension().and_then(|s| s.to_str()).unwrap_or(""),
            std::process::id()
        ));
        std::fs::write(&tmp, &bytes).map_err(io_err)?;
        std::fs::rename(&tmp, &abs).map_err(io_err)?;
        let new_sha = sha256_hex(&bytes);
        let mtime_ms = std::fs::metadata(&abs)
            .ok()
            .and_then(|m| m.modified().ok())
            .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
            .map(|d| d.as_millis() as i64)
            .unwrap_or(0);
        drop(_g);
        let tx = self.channel_blocking(vault_id);
        let _ = tx.send(VaultEvent::Put {
            path: path.to_string(),
            sha256: new_sha.clone(),
            mtime_ms,
            size: bytes.len() as u64,
        });
        Ok(PutAck {
            sha256: new_sha,
            mtime_ms,
        })
    }

    fn delete_file(
        &self,
        vault_id: &str,
        path: &str,
        if_match: IfMatch,
    ) -> Result<(), VaultSyncError> {
        let abs = self.file_path(vault_id, path)?;
        let _g = self
            .write_lock
            .lock()
            .expect("vault::sync write_lock poisoned");
        if !abs.exists() {
            // Idempotent: missing path = success, no broadcast.
            return Ok(());
        }
        if let IfMatch::Sha(want) = &if_match {
            let bytes = std::fs::read(&abs).map_err(io_err)?;
            let have = sha256_hex(&bytes);
            if *want != have {
                return Err(VaultSyncError::Conflict {
                    server_sha: have,
                    server_bytes: bytes,
                });
            }
        }
        std::fs::remove_file(&abs).map_err(io_err)?;
        drop(_g);
        let tx = self.channel_blocking(vault_id);
        let _ = tx.send(VaultEvent::Delete {
            path: path.to_string(),
        });
        Ok(())
    }

    async fn subscribe(&self, vault_id: String, tx: vox::Tx<VaultEvent>) {
        // Validate up front so misconfigured callers fail fast.
        if !self.knows(&vault_id) {
            let _ = tx.close(Default::default()).await;
            return;
        }
        let sender = self.channel(&vault_id).await;
        let mut rx = sender.subscribe();
        loop {
            match rx.recv().await {
                Ok(evt) => {
                    if tx.send(evt).await.is_err() {
                        return;
                    }
                }
                Err(broadcast::error::RecvError::Closed) => return,
                Err(broadcast::error::RecvError::Lagged(_)) => {
                    if tx.send(VaultEvent::Resync).await.is_err() {
                        return;
                    }
                }
            }
        }
    }
}

fn collect(root: &Path, dir: &Path, out: &mut Vec<ManifestEntry>) -> Result<(), VaultSyncError> {
    for entry in std::fs::read_dir(dir).map_err(io_err)? {
        let entry = entry.map_err(io_err)?;
        let path = entry.path();
        if path.is_dir() {
            collect(root, &path, out)?;
            continue;
        }
        let rel = path
            .strip_prefix(root)
            .map_err(|_| VaultSyncError::Internal("strip prefix".into()))?
            .to_string_lossy()
            .replace('\\', "/");
        let bytes = std::fs::read(&path).map_err(io_err)?;
        let sha256 = sha256_hex(&bytes);
        let mtime_ms = std::fs::metadata(&path)
            .map_err(io_err)?
            .modified()
            .ok()
            .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
            .map(|d| d.as_millis() as i64)
            .unwrap_or(0);
        out.push(ManifestEntry {
            path: rel,
            sha256,
            mtime_ms,
            size: bytes.len() as u64,
        });
    }
    Ok(())
}

fn conflict(abs: &Path, existing_sha: Option<&str>) -> VaultSyncError {
    let bytes = std::fs::read(abs).unwrap_or_default();
    VaultSyncError::Conflict {
        server_sha: existing_sha.unwrap_or("").to_string(),
        server_bytes: bytes,
    }
}

fn io_err(e: std::io::Error) -> VaultSyncError {
    VaultSyncError::Io(e.to_string())
}

fn sha256_hex(bytes: &[u8]) -> String {
    let mut h = Sha256::new();
    h.update(bytes);
    let out = h.finalize();
    let mut hex = String::with_capacity(out.len() * 2);
    for b in out {
        use std::fmt::Write;
        let _ = write!(hex, "{b:02x}");
    }
    hex
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_backend() -> (tempfile::TempDir, Backend) {
        let tmp = tempfile::tempdir().unwrap();
        let backend = Backend::single("v1", tmp.path().to_path_buf()).unwrap();
        (tmp, backend)
    }

    #[test]
    fn manifest_empty_vault() {
        let (_tmp, b) = make_backend();
        let m = b.manifest("v1").unwrap();
        assert_eq!(m.vault_id, "v1");
        assert!(m.files.is_empty());
    }

    #[test]
    fn unknown_vault_id_returns_not_found() {
        let (_tmp, b) = make_backend();
        assert!(matches!(b.manifest("nope"), Err(VaultSyncError::NotFound)));
    }

    #[test]
    fn put_then_get_round_trips() {
        let (_tmp, b) = make_backend();
        b.put_file("v1", "hello.md", b"body".to_vec(), IfMatch::CreateOnly)
            .unwrap();
        let got = b.get_file("v1", "hello.md").unwrap();
        assert_eq!(got.0, b"body");
    }

    #[test]
    fn if_match_create_only_refuses_existing_file() {
        let (_tmp, b) = make_backend();
        b.put_file("v1", "x.md", b"first".to_vec(), IfMatch::CreateOnly)
            .unwrap();
        let err = b
            .put_file("v1", "x.md", b"second".to_vec(), IfMatch::CreateOnly)
            .unwrap_err();
        assert!(matches!(err, VaultSyncError::Conflict { .. }));
    }

    #[test]
    fn if_match_sha_mismatch_returns_conflict() {
        let (_tmp, b) = make_backend();
        b.put_file("v1", "x.md", b"v1".to_vec(), IfMatch::CreateOnly)
            .unwrap();
        let err = b
            .put_file(
                "v1",
                "x.md",
                b"v2".to_vec(),
                IfMatch::Sha("deadbeef".into()),
            )
            .unwrap_err();
        match err {
            VaultSyncError::Conflict { server_bytes, .. } => assert_eq!(server_bytes, b"v1"),
            other => panic!("expected Conflict, got {other:?}"),
        }
    }

    #[test]
    fn rejects_traversal_attempts() {
        let (_tmp, b) = make_backend();
        let err = b
            .put_file("v1", "../escape.md", b"x".to_vec(), IfMatch::Force)
            .unwrap_err();
        assert!(matches!(err, VaultSyncError::BadPath));
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn put_broadcasts_event() {
        let (_tmp, b) = make_backend();
        let tx = b.channel("v1").await;
        let mut rx = tx.subscribe();
        let backend = b.clone();
        tokio::task::spawn_blocking(move || {
            backend
                .put_file("v1", "hi.md", b"x".to_vec(), IfMatch::CreateOnly)
                .unwrap();
        })
        .await
        .unwrap();
        let evt = tokio::time::timeout(std::time::Duration::from_secs(1), rx.recv())
            .await
            .expect("event timeout")
            .expect("recv ok");
        match evt {
            VaultEvent::Put { path, size, .. } => {
                assert_eq!(path, "hi.md");
                assert_eq!(size, 1);
            }
            other => panic!("expected Put, got {other:?}"),
        }
    }

    #[test]
    fn under_parent_auto_creates_vault_dirs() {
        let tmp = tempfile::tempdir().unwrap();
        let b = Backend::under_parent(tmp.path().to_path_buf()).unwrap();
        // Any vault_id works; the subdir materializes on first
        // write.
        b.put_file("fresh-vault", "n.md", b"x".to_vec(), IfMatch::CreateOnly)
            .unwrap();
        assert!(tmp.path().join("fresh-vault").join("n.md").exists());
        // Manifest of an unwritten id is empty, not an error.
        assert!(b.manifest("never-touched").unwrap().files.is_empty());
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn multi_vault_isolation() {
        let tmp_a = tempfile::tempdir().unwrap();
        let tmp_b = tempfile::tempdir().unwrap();
        let mut roots = HashMap::new();
        roots.insert("a".into(), tmp_a.path().to_path_buf());
        roots.insert("b".into(), tmp_b.path().to_path_buf());
        let backend = Backend::with_roots(roots);
        let b1 = backend.clone();
        tokio::task::spawn_blocking(move || {
            b1.put_file("a", "note.md", b"hello-a".to_vec(), IfMatch::CreateOnly)
                .unwrap();
        })
        .await
        .unwrap();
        assert_eq!(backend.manifest("b").unwrap().files.len(), 0);
        assert_eq!(backend.manifest("a").unwrap().files.len(), 1);
    }
}
