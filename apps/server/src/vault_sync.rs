//! File-replication sync for the vault layer — architect::rpc-shaped.
//!
//! Phase 2 of the sync architecture (see
//! `plans/sync-architecture.md`): per-file get / put / delete with
//! sha256-based conflict detection. No structural awareness of
//! markdown content — that lives in `vault` + the editor. The
//! server is a thin store keyed by `(vault_id, rel_path)` → blob
//! + sha256 + mtime.
//!
//! Conflict policy: **last-writer-wins with `IfMatch`**.
//! [`IfMatch::CreateOnly`] = "create only, fail on existing";
//! [`IfMatch::Sha`] = "update only if server's current sha
//! matches"; [`IfMatch::Force`] = "overwrite unconditionally"
//! (only safe for the first push of a brand-new vault).
//!
//! Modeled on
//! [`vrtmrz/obsidian-livesync`](https://github.com/vrtmrz/obsidian-livesync)'s
//! HTTP shape but storage is the filesystem (one folder per
//! vault under `root`) rather than CouchDB.
//!
//! This module is the *server-side* implementation of
//! [`vault_sync_proto::VaultSync`] — the canonical sync trait
//! decorated with `#[architect::rpc]`. Native + wasm clients
//! consume the architect-emitted `VaultSyncClient` against the
//! same `/vox` route every other architect service uses; there
//! is no separate REST surface.
//!
//! `VaultSyncState` implements `HasDispatcher` returning
//! [`TokioBlockingDispatcher`] so each sync method runs inside
//! `tokio::task::spawn_blocking` when invoked over RPC — the
//! `std::fs` calls won't stall the async executor.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use architect::HasDispatcher;
use architect::dispatch::TokioBlockingDispatcher;
use sha2::{Digest, Sha256};
use tokio::sync::{RwLock, broadcast};
use vault_sync_proto::{
    FileBytes, IfMatch, Manifest, ManifestEntry, PutAck, VaultEvent, VaultSync, VaultSyncError,
};

/// Server-side filesystem state. One process can serve many
/// vaults — `root` is the parent directory under which each
/// vault lives at `{root}/{vault_id}/`. Cheap to `Clone` —
/// `Arc`s inside. Broadcasts hand off through a tokio mpsc so
/// the sync impl can stay sync while the async `subscribe`
/// stays async.
#[derive(Clone)]
pub struct VaultSyncState {
    pub root: PathBuf,
    /// Single coarse lock — serializes writes across vaults.
    /// Read paths don't take it; conflicts use sha-then-write
    /// inside the lock. The mutex is `std::sync` (not tokio's)
    /// so it can be held inside the sync trait methods.
    write_lock: Arc<std::sync::Mutex<()>>,
    /// Per-vault broadcast channels. PUT / DELETE handlers
    /// publish a [`VaultEvent`] here; `subscribe` subscribes
    /// per connection. Capacity 256 — bursts of rapid edits
    /// get coalesced client-side.
    channels: Arc<RwLock<HashMap<String, broadcast::Sender<VaultEvent>>>>,
}

impl VaultSyncState {
    pub fn new(root: PathBuf) -> std::io::Result<Self> {
        std::fs::create_dir_all(&root)?;
        Ok(Self {
            root,
            write_lock: Arc::new(std::sync::Mutex::new(())),
            channels: Arc::new(RwLock::new(HashMap::new())),
        })
    }

    /// Lazily creates the per-vault broadcast sender. Sync path —
    /// uses `std::sync::Mutex` under the hood by way of
    /// `tokio::sync::RwLock::blocking_*` so it works in either
    /// trait method shape.
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

    /// Sync sibling of [`Self::channel`] — used by the sync
    /// trait methods so they don't need an executor. We can't
    /// hold an async RwLock guard across a sync boundary, so
    /// use the blocking_* variants that block the current
    /// thread; `TokioBlockingDispatcher` already runs us on a
    /// blocking-pool thread.
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

    fn vault_dir(&self, vault_id: &str) -> PathBuf {
        self.root.join(vault_id)
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
        Ok(self.vault_dir(vault_id).join(rel))
    }
}

impl HasDispatcher for VaultSyncState {
    type Dispatcher = TokioBlockingDispatcher;
    fn dispatcher(&self) -> Self::Dispatcher {
        TokioBlockingDispatcher
    }
}

impl VaultSync for VaultSyncState {
    fn manifest(&self, vault_id: &str) -> Result<Manifest, VaultSyncError> {
        let dir = self.vault_dir(vault_id);
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
            .expect("vault_sync write_lock poisoned");
        let existing_sha = if abs.exists() {
            let bytes = std::fs::read(&abs).map_err(io_err)?;
            Some(sha256_hex(&bytes))
        } else {
            None
        };
        // IfMatch semantics:
        //   CreateOnly → fail if file exists.
        //   Sha(hex)   → fail unless the server's current sha matches.
        //   Force      → unconditional write (client overwriting blindly;
        //                only safe on first push of a brand-new vault).
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
            .expect("vault_sync write_lock poisoned");
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
                    // Subscriber missed events because we hit the
                    // channel cap. Send a hint so the client knows
                    // to re-pull the manifest.
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

    /// `channel_blocking` uses `RwLock::blocking_*`, which panics if
    /// called from a thread that's driving a tokio runtime. Tests use
    /// the multi-thread flavor so the blocking calls land on a worker
    /// thread that isn't currently running an executor.
    fn make_state() -> (tempfile::TempDir, VaultSyncState) {
        let tmp = tempfile::tempdir().unwrap();
        let state = VaultSyncState::new(tmp.path().to_path_buf()).unwrap();
        (tmp, state)
    }

    #[test]
    fn manifest_empty_vault() {
        let (_tmp, state) = make_state();
        let m = state.manifest("v1").unwrap();
        assert_eq!(m.vault_id, "v1");
        assert!(m.files.is_empty());
    }

    #[test]
    fn put_then_get_round_trips() {
        let (_tmp, state) = make_state();
        state
            .put_file("v1", "hello.md", b"body".to_vec(), IfMatch::CreateOnly)
            .unwrap();
        let got = state.get_file("v1", "hello.md").unwrap();
        assert_eq!(got.0, b"body");
    }

    #[test]
    fn if_match_create_only_refuses_existing_file() {
        let (_tmp, state) = make_state();
        state
            .put_file("v1", "x.md", b"first".to_vec(), IfMatch::CreateOnly)
            .unwrap();
        let err = state
            .put_file("v1", "x.md", b"second".to_vec(), IfMatch::CreateOnly)
            .unwrap_err();
        assert!(matches!(err, VaultSyncError::Conflict { .. }));
    }

    #[test]
    fn if_match_sha_mismatch_returns_conflict() {
        let (_tmp, state) = make_state();
        state
            .put_file("v1", "x.md", b"v1".to_vec(), IfMatch::CreateOnly)
            .unwrap();
        let err = state
            .put_file(
                "v1",
                "x.md",
                b"v2".to_vec(),
                IfMatch::Sha("deadbeef".into()),
            )
            .unwrap_err();
        match err {
            VaultSyncError::Conflict { server_bytes, .. } => {
                assert_eq!(server_bytes, b"v1");
            }
            other => panic!("expected Conflict, got {other:?}"),
        }
    }

    #[test]
    fn rejects_traversal_attempts() {
        let (_tmp, state) = make_state();
        let err = state
            .put_file("v1", "../escape.md", b"x".to_vec(), IfMatch::Force)
            .unwrap_err();
        assert!(matches!(err, VaultSyncError::BadPath));
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn put_broadcasts_event() {
        let (_tmp, state) = make_state();
        let tx = state.channel("v1").await;
        let mut rx = tx.subscribe();
        let st = state.clone();
        tokio::task::spawn_blocking(move || {
            st.put_file("v1", "hello.md", b"hi".to_vec(), IfMatch::CreateOnly)
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
                assert_eq!(path, "hello.md");
                assert_eq!(size, 2);
            }
            other => panic!("expected Put, got {other:?}"),
        }
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn delete_broadcasts_event() {
        let (_tmp, state) = make_state();
        let st1 = state.clone();
        tokio::task::spawn_blocking(move || {
            st1.put_file("v1", "gone.md", b"x".to_vec(), IfMatch::CreateOnly)
                .unwrap();
        })
        .await
        .unwrap();
        let tx = state.channel("v1").await;
        let mut rx = tx.subscribe();
        let st2 = state.clone();
        tokio::task::spawn_blocking(move || {
            st2.delete_file("v1", "gone.md", IfMatch::Force).unwrap();
        })
        .await
        .unwrap();
        let evt = tokio::time::timeout(std::time::Duration::from_secs(1), rx.recv())
            .await
            .expect("event timeout")
            .expect("recv ok");
        match evt {
            VaultEvent::Delete { path } => assert_eq!(path, "gone.md"),
            other => panic!("expected Delete, got {other:?}"),
        }
    }
}
