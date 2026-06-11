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
//! backend — vim/obsidian/git pulls/etc.) are picked up by
//! attaching a watcher via [`Backend::start_watcher`]: the
//! returned [`WatcherHandle`] keeps a [`crate::watcher`] alive
//! and forwards every FS change into the same broadcast channel
//! that powers `subscribe`. Drop the handle to detach. Caller
//! is expected to start one watcher per registered vault; the
//! backend itself doesn't auto-spawn.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use architect::vox;
use sha2::{Digest, Sha256};
use tokio::sync::{RwLock, broadcast};
use vault_proto::{
    FileBytes, FolderIndex, IfMatch, Manifest, ManifestEntry, PageMeta, PutAck, VaultEvent,
    VaultSync, VaultSyncError,
};

use crate::vault::Vault;
use crate::watcher::{self, WatchError};
use editor_state::markdown::{FrontMatter, PropValue, parse_frontmatter};

/// Debounce window for the FS watcher attached by
/// [`Backend::start_watcher`]. Coalesces editor swap-file dances
/// + git-pull bursts into one event per touched path.
const WATCHER_DEBOUNCE: Duration = Duration::from_millis(500);

/// Handle for a per-vault filesystem watcher attached via
/// [`Backend::start_watcher`]. Keeps the watcher + the
/// forwarding thread alive; drop to stop receiving external
/// disk events.
pub struct WatcherHandle {
    _guard: watcher::WatcherGuard,
}

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
#[derive(Clone, architect::HasDispatcher)]
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
    #[must_use]
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

    /// Attach a debounced filesystem watcher to `vault_id`.
    /// External edits (vim, Obsidian, `git pull`, …) under the
    /// vault root are translated into the same
    /// [`vault_proto::VaultEvent`]s that PUT/DELETE wire calls
    /// emit, and pushed onto the broadcast channel `subscribe`
    /// listens to.
    ///
    /// The forwarder runs on a dedicated OS thread. Dropping the
    /// returned [`WatcherHandle`] closes the underlying
    /// debouncer; the thread exits naturally on the next loop
    /// iteration as the sender side hangs up.
    ///
    /// Caveats:
    /// - Subscribers may observe duplicates after their own
    ///   writes (the broadcast emits once on commit, the
    ///   watcher emits again on the disk event). The duplicate
    ///   carries the same `sha256`, so clients can dedupe on
    ///   that.
    /// - Only `Explicit` / `UnderParent`-registered `vault_ids`
    ///   resolve. The watcher fails up front for unknown ids.
    /// - For `UnderParent` layouts the root dir must already
    ///   exist (which it always does after the first write); to
    ///   pre-attach before any write, create the subdir first.
    pub async fn start_watcher(&self, vault_id: &str) -> Result<WatcherHandle, WatchError> {
        let root = self
            .root(vault_id)
            .map_err(|_| WatchError::Notify(format!("unknown vault id `{vault_id}`")))?;
        let tx = self.channel(vault_id).await;
        let (rx, guard) = watcher::watch(root.clone(), WATCHER_DEBOUNCE)?;

        let thread_name = format!("vault-sync-watcher:{vault_id}");
        std::thread::Builder::new()
            .name(thread_name)
            .spawn(move || forward_watcher_events(root, rx, tx))
            .map_err(|e| WatchError::Notify(format!("spawn watcher thread: {e}")))?;

        Ok(WatcherHandle { _guard: guard })
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
        let g = self
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
            .map_or(0, |d| d.as_millis() as i64);
        drop(g);
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
        let g = self
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
        drop(g);
        let tx = self.channel_blocking(vault_id);
        let _ = tx.send(VaultEvent::Delete {
            path: path.to_string(),
        });
        Ok(())
    }

    fn folder_index(&self, vault_id: &str) -> Result<FolderIndex, VaultSyncError> {
        let dir = self.root(vault_id)?;
        if !dir.exists() {
            return Ok(FolderIndex {
                vault_id: vault_id.to_string(),
                pages: Vec::new(),
            });
        }
        // `Vault::open` walks every `.md` page and hands back the
        // raw bytes + derived basename, so we parse frontmatter
        // once here rather than re-walking the tree.
        let vault = Vault::open(&dir).map_err(|e| VaultSyncError::Internal(e.to_string()))?;
        let pages = vault
            .pages
            .iter()
            .map(|p| {
                let fm = parse_frontmatter(&p.raw);
                let get = |key: &str| fm.as_ref().and_then(|f| fm_text(f, key));
                PageMeta {
                    path: p.rel_path.clone(),
                    basename: p.basename.clone(),
                    title: get("title").unwrap_or_else(|| p.basename.clone()),
                    page_type: get("type").unwrap_or_default(),
                    folder: get("folder")
                        .map(|s| strip_wikilink(&s))
                        .unwrap_or_default(),
                    // `raw` is the file's verbatim UTF-8 bytes, so this
                    // matches the manifest's per-file hash.
                    sha256: sha256_hex(p.raw.as_bytes()),
                }
            })
            .collect();
        Ok(FolderIndex {
            vault_id: vault_id.to_string(),
            pages,
        })
    }

    fn set_folder(
        &self,
        vault_id: &str,
        path: &str,
        parent: Option<String>,
        if_match: IfMatch,
    ) -> Result<PutAck, VaultSyncError> {
        let abs = self.file_path(vault_id, path)?;
        let g = self
            .write_lock
            .lock()
            .expect("vault::sync write_lock poisoned");
        if !abs.exists() {
            return Err(VaultSyncError::NotFound);
        }
        let bytes = std::fs::read(&abs).map_err(io_err)?;
        let existing_sha = sha256_hex(&bytes);
        if let IfMatch::Sha(want) = &if_match {
            if *want != existing_sha {
                return Err(VaultSyncError::Conflict {
                    server_sha: existing_sha,
                    server_bytes: bytes,
                });
            }
        }
        let content = String::from_utf8(bytes)
            .map_err(|_| VaultSyncError::Internal("page is not valid UTF-8".into()))?;
        let mtime_now = || {
            std::fs::metadata(&abs)
                .ok()
                .and_then(|m| m.modified().ok())
                .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
                .map_or(0, |d| d.as_millis() as i64)
        };
        let Some(new_content) = apply_folder(&content, parent.as_deref()) else {
            // Already in the requested state (e.g. clearing an
            // absent `folder`): no write, no broadcast.
            return Ok(PutAck {
                sha256: existing_sha,
                mtime_ms: mtime_now(),
            });
        };
        let new_bytes = new_content.into_bytes();
        let tmp = abs.with_extension(format!(
            "{}.tmp.{}",
            abs.extension().and_then(|s| s.to_str()).unwrap_or(""),
            std::process::id()
        ));
        std::fs::write(&tmp, &new_bytes).map_err(io_err)?;
        std::fs::rename(&tmp, &abs).map_err(io_err)?;
        let new_sha = sha256_hex(&new_bytes);
        let mtime_ms = mtime_now();
        drop(g);
        let tx = self.channel_blocking(vault_id);
        let _ = tx.send(VaultEvent::Put {
            path: path.to_string(),
            sha256: new_sha.clone(),
            mtime_ms,
            size: new_bytes.len() as u64,
        });
        Ok(PutAck {
            sha256: new_sha,
            mtime_ms,
        })
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
            .map_or(0, |d| d.as_millis() as i64);
        let now = chrono::Utc::now();
        out.push(ManifestEntry {
            path: rel,
            sha256,
            mtime_ms,
            size: bytes.len() as u64,
            created_at: now,
            updated_at: now,
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

/// Read a scalar frontmatter property as a string. Text and Date
/// round-trip as-is; other kinds (bool/number/list) aren't
/// meaningful for the folder/title/type keys we read.
fn fm_text(fm: &FrontMatter, key: &str) -> Option<String> {
    fm.props
        .iter()
        .find(|p| p.key.eq_ignore_ascii_case(key))
        .and_then(|p| match &p.value {
            PropValue::Text(s) | PropValue::Date(s) => Some(s.clone()),
            _ => None,
        })
}

/// Reduce a `folder` value to a bare parent basename. Handles the
/// Obsidian wikilink form `[[Name|alias]]#heading` as well as a
/// plain string.
fn strip_wikilink(value: &str) -> String {
    let t = value.trim();
    let inner = t
        .strip_prefix("[[")
        .and_then(|x| x.strip_suffix("]]"))
        .unwrap_or(t);
    inner
        .split(['|', '#'])
        .next()
        .unwrap_or(inner)
        .trim()
        .to_string()
}

/// Splice a note's `folder` frontmatter to point at `parent`
/// (`None` clears it). Returns the rewritten content, or `None`
/// when no change is needed. Preserves key order + every other
/// property by editing only the `folder` line's byte range.
fn apply_folder(content: &str, parent: Option<&str>) -> Option<String> {
    let fm = parse_frontmatter(content);
    let existing = fm.as_ref().and_then(|f| {
        f.props
            .iter()
            .find(|p| p.key.eq_ignore_ascii_case("folder"))
    });
    match (parent, existing) {
        // Re-point an existing `folder:` line.
        (Some(p), Some(prop)) => {
            let mut s = String::with_capacity(content.len() + p.len());
            s.push_str(&content[..prop.range.start]);
            s.push_str(&format!("folder: \"[[{p}]]\"\n"));
            s.push_str(&content[prop.range.end..]);
            Some(s)
        }
        // Drop an existing `folder:` line (move to root).
        (None, Some(prop)) => {
            let mut s = String::with_capacity(content.len());
            s.push_str(&content[..prop.range.start]);
            s.push_str(&content[prop.range.end..]);
            Some(s)
        }
        // Insert into an existing frontmatter block, just before
        // the closing `---`.
        (Some(p), None) if fm.is_some() => {
            let at = fm.as_ref().unwrap().closer.start;
            let mut s = String::with_capacity(content.len() + p.len() + 16);
            s.push_str(&content[..at]);
            s.push_str(&format!("folder: \"[[{p}]]\"\n"));
            s.push_str(&content[at..]);
            Some(s)
        }
        // No frontmatter at all — prepend a minimal block.
        (Some(p), None) => Some(format!("---\nfolder: \"[[{p}]]\"\n---\n{content}")),
        // Clearing an absent `folder:` — already at root.
        (None, None) => None,
    }
}

fn io_err(e: std::io::Error) -> VaultSyncError {
    VaultSyncError::Io(e.to_string())
}

/// Read mtime in unix-ms, defaulting to 0 on any error. Matches
/// the `0` fallback used elsewhere in the backend so wire shapes
/// stay consistent.
fn mtime_ms(abs: &Path) -> i64 {
    std::fs::metadata(abs)
        .ok()
        .and_then(|m| m.modified().ok())
        .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
        .map_or(0, |d| d.as_millis() as i64)
}

/// Pump `watcher` events into the broadcast channel. Runs on a
/// dedicated OS thread spawned by [`Backend::start_watcher`];
/// exits when the watcher guard drops (closing the sender).
fn forward_watcher_events(
    root: PathBuf,
    rx: std::sync::mpsc::Receiver<watcher::VaultEvent>,
    tx: broadcast::Sender<VaultEvent>,
) {
    while let Ok(evt) = rx.recv() {
        let abs = match evt {
            watcher::VaultEvent::Changed { abs_path } => abs_path,
            watcher::VaultEvent::Removed { abs_path } => abs_path,
        };
        let Ok(rel_path) = abs.strip_prefix(&root) else {
            continue;
        };
        let rel = rel_path.to_string_lossy().replace('\\', "/");
        if rel.is_empty() {
            continue;
        }
        // `Changed` events from the debouncer cover both
        // create+modify AND delete. Reload-or-classify by
        // existence — cheaper than disambiguating the raw
        // `notify::EventKind`.
        let payload = if abs.exists() {
            match std::fs::read(&abs) {
                Ok(bytes) => VaultEvent::Put {
                    path: rel,
                    sha256: sha256_hex(&bytes),
                    mtime_ms: mtime_ms(&abs),
                    size: bytes.len() as u64,
                },
                Err(e) => {
                    tracing::warn!(?abs, ?e, "watcher: failed to read changed file");
                    continue;
                }
            }
        } else {
            VaultEvent::Delete { path: rel }
        };
        if tx.send(payload).is_err() {
            // No active subscribers — keep pumping; new
            // subscribers attach later via the same channel.
            continue;
        }
    }
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
    async fn watcher_forwards_external_writes() {
        let (tmp, b) = make_backend();
        let tx = b.channel("v1").await;
        let mut rx = tx.subscribe();
        // Hold the watcher alive across the write+recv.
        let _watch = b.start_watcher("v1").await.expect("start watcher");
        // notify-debouncer-mini ignores events that fire before
        // the watcher has fully attached on some platforms; a
        // brief settle prevents flakes.
        tokio::time::sleep(std::time::Duration::from_millis(200)).await;

        // Write a file directly to disk — bypasses the backend
        // entirely, simulating an external editor.
        std::fs::write(tmp.path().join("ext.md"), b"hi from vim").unwrap();

        // The debounce window is 500ms; wait up to 3s for the
        // event to land.
        let evt = tokio::time::timeout(std::time::Duration::from_secs(3), rx.recv())
            .await
            .expect("watcher event timeout")
            .expect("rx recv ok");
        match evt {
            VaultEvent::Put { path, size, .. } => {
                assert_eq!(path, "ext.md");
                assert_eq!(size, 11);
            }
            other => panic!("expected Put, got {other:?}"),
        }
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

    fn meta<'a>(idx: &'a vault_proto::FolderIndex, base: &str) -> &'a PageMeta {
        idx.pages
            .iter()
            .find(|p| p.basename == base)
            .unwrap_or_else(|| panic!("no page `{base}` in index"))
    }

    #[test]
    fn folder_index_parses_frontmatter_and_resolves_parent() {
        let (_tmp, b) = make_backend();
        // Root folder note (no `folder`), a child note pointing at
        // it via a wikilink, and a plain note with no frontmatter.
        b.put_file(
            "v1",
            "Wisdom/Wisdom.md",
            b"---\ntitle: Wisdom\ntype: folder\n---\n# Wisdom\n".to_vec(),
            IfMatch::CreateOnly,
        )
        .unwrap();
        b.put_file(
            "v1",
            "Wisdom/Plans.md",
            b"---\ntitle: Plans rot\nfolder: \"[[Wisdom]]\"\n---\nbody\n".to_vec(),
            IfMatch::CreateOnly,
        )
        .unwrap();
        b.put_file(
            "v1",
            "loose.md",
            b"no frontmatter here\n".to_vec(),
            IfMatch::CreateOnly,
        )
        .unwrap();

        let idx = b.folder_index("v1").unwrap();
        assert_eq!(idx.pages.len(), 3);

        let root = meta(&idx, "Wisdom");
        assert_eq!(root.title, "Wisdom");
        assert_eq!(root.page_type, "folder");
        assert_eq!(root.folder, "", "root folder note has no parent");

        let child = meta(&idx, "Plans");
        assert_eq!(child.title, "Plans rot");
        assert_eq!(
            child.folder, "Wisdom",
            "wikilink resolved to parent basename"
        );

        let loose = meta(&idx, "loose");
        assert_eq!(loose.title, "loose", "title falls back to basename");
        assert_eq!(loose.folder, "");
    }

    #[test]
    fn set_folder_inserts_replaces_and_clears_preserving_order() {
        let (_tmp, b) = make_backend();
        b.put_file(
            "v1",
            "n.md",
            b"---\ntitle: N\ntags: [a]\n---\nbody\n".to_vec(),
            IfMatch::CreateOnly,
        )
        .unwrap();

        // Insert into an existing block (before the closer), other
        // keys + order preserved.
        b.set_folder("v1", "n.md", Some("Home".into()), IfMatch::Force)
            .unwrap();
        let after_insert = String::from_utf8(b.get_file("v1", "n.md").unwrap().0).unwrap();
        assert_eq!(
            after_insert,
            "---\ntitle: N\ntags: [a]\nfolder: \"[[Home]]\"\n---\nbody\n"
        );

        // Re-point the existing folder line.
        b.set_folder("v1", "n.md", Some("Work".into()), IfMatch::Force)
            .unwrap();
        let after_repoint = String::from_utf8(b.get_file("v1", "n.md").unwrap().0).unwrap();
        assert!(after_repoint.contains("folder: \"[[Work]]\""));
        assert!(!after_repoint.contains("Home"));

        // Clear it (move to root) — line removed, rest intact.
        b.set_folder("v1", "n.md", None, IfMatch::Force).unwrap();
        let after_clear = String::from_utf8(b.get_file("v1", "n.md").unwrap().0).unwrap();
        assert_eq!(after_clear, "---\ntitle: N\ntags: [a]\n---\nbody\n");
    }

    #[test]
    fn set_folder_creates_block_when_no_frontmatter() {
        let (_tmp, b) = make_backend();
        b.put_file(
            "v1",
            "bare.md",
            b"just text\n".to_vec(),
            IfMatch::CreateOnly,
        )
        .unwrap();
        b.set_folder("v1", "bare.md", Some("Inbox".into()), IfMatch::Force)
            .unwrap();
        let out = String::from_utf8(b.get_file("v1", "bare.md").unwrap().0).unwrap();
        assert_eq!(out, "---\nfolder: \"[[Inbox]]\"\n---\njust text\n");
    }
}
