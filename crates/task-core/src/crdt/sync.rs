//! CRDT ↔ file sync engine.
//!
//! Bridges between the in-memory Loro state and `.md` files on disk. Field
//! writes and body edits are broadcast to WebSocket peers, debounced to files,
//! and scanned for concurrent-edit conflicts before LWW eats the loser.
//!
//! ```text
//! .md file changes (editor, Nextcloud, Obsidian)
//!   │
//!   ▼ file watcher (notify)
//!   │
//!   ├─→ Parse .md → rebuild CrdtDocument → broadcast Refresh
//!   │
//!   └─→ Update SQLite index
//!
//! Remote op (WebSocket)
//!   │
//!   ▼ snapshot_fields() BEFORE import
//!   ├─→ import(update)
//!   ├─→ for each metadata field:
//!   │     if value changed AND last_editor is remote AND we had written it
//!   │     → emit ConflictEvent (sync -> SQLite record_conflict)
//!   ├─→ Broadcast to other clients
//!   ├─→ Debounced write back to .md
//!   └─→ Update SQLite index
//! ```

use std::collections::HashMap;
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use loro::PeerID;
use tokio::sync::{RwLock, broadcast, mpsc};

use crate::service::VaultError;
use crate::task::Task;
use crate::vault::Vault;

use super::document::{CONFLICT_FIELDS, CrdtDocument, DocumentSnapshot};

/// An operation broadcast to connected clients.
#[derive(Debug, Clone)]
#[allow(clippy::large_enum_variant)]
pub enum SyncOp {
    /// A metadata field changed on a task.
    FieldChanged {
        file_path: String,
        field: String,
        value: String,
        /// Peer that applied the change, if known.
        peer: Option<PeerID>,
    },
    /// Raw Loro update bytes — body edits and anything else.
    DocUpdate { file_path: String, update: Vec<u8> },
    /// A new task was created.
    TaskCreated { file_path: String, task: Task },
    /// A task was deleted.
    TaskDeleted { file_path: String },
    /// Full refresh — file was rewritten out-of-band.
    Refresh,
}

/// A concurrent-edit conflict detected when importing remote updates.
///
/// The sync engine emits one of these per field where the local replica had a
/// pending write that got overwritten by the incoming import. The outer layer
/// (server / indexer) is responsible for persisting the conflict via
/// [`TaskIndex::record_conflict`](crate::index::TaskIndex::record_conflict).
#[derive(Debug, Clone)]
pub struct ConflictEvent {
    pub file_path: String,
    pub field: String,
    /// The value we held locally before the import (now overwritten).
    pub losing_value: Option<String>,
    /// The value after the import — what the map now resolves to.
    pub winning_value: Option<String>,
    /// Peer that wrote the losing value (the local replica's perspective).
    pub losing_peer: Option<PeerID>,
    /// Peer that wrote the winning value (the remote's perspective).
    pub winning_peer: Option<PeerID>,
}

/// Live state: all open CRDT documents plus broadcast channels.
///
/// Documents load on-demand from `.md` files and stay in memory for fast
/// reads and real-time operations.
pub struct CrdtSyncEngine {
    root: PathBuf,
    local_peer: PeerID,
    documents: Arc<RwLock<HashMap<String, CrdtDocument>>>,
    broadcast_tx: broadcast::Sender<SyncOp>,
    conflict_tx: broadcast::Sender<ConflictEvent>,
    write_tx: mpsc::Sender<String>,
}

impl CrdtSyncEngine {
    /// Create a new engine. `local_peer` must be stable per device — a random
    /// u64 is fine as long as it doesn't collide with any other peer writing
    /// to the same vault.
    pub fn new(root: &Path, local_peer: PeerID) -> Self {
        let (broadcast_tx, _) = broadcast::channel(256);
        let (conflict_tx, _) = broadcast::channel(64);
        let (write_tx, write_rx) = mpsc::channel(64);

        let engine = Self {
            root: root.to_path_buf(),
            local_peer,
            documents: Arc::new(RwLock::new(HashMap::new())),
            broadcast_tx,
            conflict_tx,
            write_tx,
        };

        let docs = engine.documents.clone();
        let root = engine.root.clone();
        tokio::spawn(debounced_writer(root, docs, write_rx));

        engine
    }

    /// Subscribe to sync operations (WebSocket broadcast).
    pub fn subscribe(&self) -> broadcast::Receiver<SyncOp> {
        self.broadcast_tx.subscribe()
    }

    /// Subscribe to conflict events (server indexer wires this to SQLite).
    pub fn subscribe_conflicts(&self) -> broadcast::Receiver<ConflictEvent> {
        self.conflict_tx.subscribe()
    }

    pub fn local_peer(&self) -> PeerID {
        self.local_peer
    }

    /// Fetch or load a CRDT document for a relative file path.
    pub async fn get_or_load(&self, rel_path: &str) -> Option<Task> {
        {
            let docs = self.documents.read().await;
            if let Some(doc) = docs.get(rel_path) {
                return Some(doc.to_task());
            }
        }

        let abs_path = self.root.join(rel_path);
        let content = std::fs::read_to_string(&abs_path).ok()?;
        let task = Vault::parse_task_from_md(&content)?;
        let doc = CrdtDocument::from_task(&task, rel_path);
        doc.set_peer_id(self.local_peer).ok()?;
        let out = doc.to_task();

        let mut docs = self.documents.write().await;
        docs.insert(rel_path.to_string(), doc);

        Some(out)
    }

    /// Handle an external file change (watcher-driven). Rebuilds the doc from
    /// disk — the file is the source of truth when it's edited out-of-band.
    pub async fn on_file_changed(&self, rel_path: &str) -> Result<(), VaultError> {
        let abs_path = self.root.join(rel_path);

        if !abs_path.exists() {
            self.documents.write().await.remove(rel_path);
            let _ = self.broadcast_tx.send(SyncOp::TaskDeleted {
                file_path: rel_path.to_string(),
            });
            return Ok(());
        }

        let content =
            std::fs::read_to_string(&abs_path).map_err(|e| VaultError::IoError(e.to_string()))?;
        let Some(task) = Vault::parse_task_from_md(&content) else {
            return Ok(());
        };

        let new_doc = CrdtDocument::from_task(&task, rel_path);
        let _ = new_doc.set_peer_id(self.local_peer);

        let mut docs = self.documents.write().await;
        let was_new = !docs.contains_key(rel_path);
        docs.insert(rel_path.to_string(), new_doc);

        if was_new {
            let _ = self.broadcast_tx.send(SyncOp::TaskCreated {
                file_path: rel_path.to_string(),
                task,
            });
        } else {
            let _ = self.broadcast_tx.send(SyncOp::Refresh);
        }
        Ok(())
    }

    /// Apply a local field change and broadcast.
    pub async fn apply_field_change(
        &self,
        rel_path: &str,
        field: &str,
        value: &str,
    ) -> Result<(), VaultError> {
        self.get_or_load(rel_path).await;

        let docs = self.documents.read().await;
        if let Some(doc) = docs.get(rel_path) {
            doc.set_field(field, value);
            doc.commit();

            let _ = self.broadcast_tx.send(SyncOp::FieldChanged {
                file_path: rel_path.to_string(),
                field: field.to_string(),
                value: value.to_string(),
                peer: Some(self.local_peer),
            });
            let _ = self.write_tx.send(rel_path.to_string()).await;
        }
        Ok(())
    }

    /// Replace the markdown body for a document and broadcast the resulting
    /// Loro update. This is the server-side entry point for clients that edit
    /// prose without constructing their own Loro update bytes.
    pub async fn apply_body_change(&self, rel_path: &str, body: &str) -> Result<(), VaultError> {
        self.get_or_load(rel_path).await;

        let docs = self.documents.read().await;
        if let Some(doc) = docs.get(rel_path) {
            let before = doc.version_vector();
            doc.set_body(body);
            doc.commit();
            let update = doc.export_updates_since(&before)?;

            let _ = self.broadcast_tx.send(SyncOp::DocUpdate {
                file_path: rel_path.to_string(),
                update,
            });
            let _ = self.write_tx.send(rel_path.to_string()).await;
        }
        Ok(())
    }

    /// Apply a remote Loro update. Detects concurrent writes: for every
    /// conflict-relevant field we held a value for, check whether the import
    /// overwrote it with a value written by a different peer. Each collision
    /// emits a [`ConflictEvent`].
    pub async fn apply_remote_update(
        &self,
        rel_path: &str,
        update: &[u8],
    ) -> Result<(), VaultError> {
        self.get_or_load(rel_path).await;

        let docs = self.documents.read().await;
        let Some(doc) = docs.get(rel_path) else {
            return Ok(());
        };

        let before = doc.snapshot_fields();
        doc.import(update)?;

        let conflicts = detect_field_conflicts(doc, &before, self.local_peer);
        for c in &conflicts {
            let _ = self.conflict_tx.send(ConflictEvent {
                file_path: rel_path.to_string(),
                field: c.field.clone(),
                losing_value: c.losing_value.clone(),
                winning_value: c.winning_value.clone(),
                losing_peer: c.losing_peer,
                winning_peer: c.winning_peer,
            });
        }

        let _ = self.broadcast_tx.send(SyncOp::DocUpdate {
            file_path: rel_path.to_string(),
            update: update.to_vec(),
        });
        let _ = self.write_tx.send(rel_path.to_string()).await;
        Ok(())
    }

    /// Snapshot of currently-loaded tasks.
    pub async fn loaded_tasks(&self) -> Vec<Task> {
        let docs = self.documents.read().await;
        docs.values().map(|d| d.to_task()).collect()
    }

    pub async fn loaded_count(&self) -> usize {
        self.documents.read().await.len()
    }

    /// Return the relative paths for every currently loaded CRDT document.
    pub async fn loaded_paths(&self) -> Vec<String> {
        self.documents.read().await.keys().cloned().collect()
    }

    /// Rescan the vault for markdown files and push out-of-band file edits into
    /// the CRDT broadcast stream. This is the bridge from local editors,
    /// Nextcloud/WebDAV sync, and service-layer file writes into realtime.
    pub async fn rescan_vault(&self) -> Result<(), VaultError> {
        let mut seen = HashSet::new();

        for entry in walkdir::WalkDir::new(&self.root)
            .follow_links(false)
            .into_iter()
            .filter_map(|entry| entry.ok())
            .filter(|entry| entry.path().extension().and_then(|s| s.to_str()) == Some("md"))
        {
            let path = entry.path();
            let Ok(rel) = path.strip_prefix(&self.root) else {
                continue;
            };
            let rel = rel.to_string_lossy().replace('\\', "/");
            seen.insert(rel.clone());
            self.on_file_changed(&rel).await?;
        }

        for rel in self.loaded_paths().await {
            if !seen.contains(&rel) {
                self.on_file_changed(&rel).await?;
            }
        }

        Ok(())
    }

    /// Export a full Loro snapshot for a document, loading it from disk if
    /// needed. Clients use this as the initial sync payload.
    pub async fn export_snapshot(&self, rel_path: &str) -> Result<Option<Vec<u8>>, VaultError> {
        self.get_or_load(rel_path).await;

        let docs = self.documents.read().await;
        match docs.get(rel_path) {
            Some(doc) => doc.export_snapshot().map(Some),
            None => Ok(None),
        }
    }

    /// Return the current server view of a task document.
    pub async fn task(&self, rel_path: &str) -> Option<Task> {
        self.get_or_load(rel_path).await
    }
}

/// Decide which fields changed in a way that represents a concurrent overwrite
/// of a locally-authored write.
///
/// Rule: a field is a conflict iff
/// 1. the value after the import differs from the value before, AND
/// 2. the post-import `last_editor` is not the local peer, AND
/// 3. the pre-import `last_editor` **was** the local peer (we had a pending
///    local write that the remote just clobbered).
///
/// This is the "Option A" send-time detection — imperfect (misses A->B->A
/// chains where A's original losing write came from another remote) but
/// catches every case where the local replica actually lost a write it
/// authored.
pub fn detect_field_conflicts(
    doc: &CrdtDocument,
    before: &DocumentSnapshot,
    local_peer: PeerID,
) -> Vec<DetectedConflict> {
    let mut out = Vec::new();
    for field in CONFLICT_FIELDS {
        let before_value = before.fields.get(*field).cloned().flatten();
        let before_editor = before.field_editors.get(*field).and_then(|e| *e);
        let after_value = doc.get_field(field);
        let after_editor = doc.last_editor(field);

        if after_value == before_value {
            continue;
        }
        if after_editor == Some(local_peer) {
            continue;
        }
        if before_editor != Some(local_peer) {
            // We didn't author the losing value, so nothing of ours was
            // clobbered. It's a normal remote write, not a conflict.
            continue;
        }

        out.push(DetectedConflict {
            field: (*field).to_string(),
            losing_value: before_value,
            winning_value: after_value,
            losing_peer: before_editor,
            winning_peer: after_editor,
        });
    }
    out
}

#[derive(Debug, Clone)]
pub struct DetectedConflict {
    pub field: String,
    pub losing_value: Option<String>,
    pub winning_value: Option<String>,
    pub losing_peer: Option<PeerID>,
    pub winning_peer: Option<PeerID>,
}

// ── debounced writer ────────────────────────────────────────────────────────

async fn debounced_writer(
    root: PathBuf,
    documents: Arc<RwLock<HashMap<String, CrdtDocument>>>,
    mut rx: mpsc::Receiver<String>,
) {
    let mut pending: HashMap<String, tokio::time::Instant> = HashMap::new();
    let debounce = Duration::from_millis(500);

    loop {
        let timeout = if pending.is_empty() {
            Duration::from_secs(60)
        } else {
            Duration::from_millis(100)
        };

        match tokio::time::timeout(timeout, rx.recv()).await {
            Ok(Some(path)) => {
                pending.insert(path, tokio::time::Instant::now());
            }
            Ok(None) => break,
            Err(_) => {}
        }

        let now = tokio::time::Instant::now();
        let ready: Vec<String> = pending
            .iter()
            .filter(|(_, instant)| now.duration_since(**instant) >= debounce)
            .map(|(path, _)| path.clone())
            .collect();

        for path in ready {
            pending.remove(&path);
            let docs = documents.read().await;
            if let Some(doc) = docs.get(&path) {
                match doc.to_md() {
                    Ok(content) => {
                        let abs_path = root.join(&path);
                        if let Err(e) = Vault::atomic_write(&abs_path, &content) {
                            tracing::warn!(path = %path, error = %e, "failed to write CRDT to file");
                        }
                    }
                    Err(e) => {
                        tracing::warn!(path = %path, error = %e, "failed to render CRDT to markdown");
                    }
                }
            }
        }
    }
}

// ── tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::task::{Priority, Status};

    fn doc_with(peer: PeerID, title: &str) -> CrdtDocument {
        let t = Task {
            id: uuid::Uuid::parse_str("00000000-0000-4000-8000-000000000402").unwrap(),
            title: title.into(),
            status: Status::Open,
            priority: Priority::Normal,
            ..Default::default()
        };
        let doc = CrdtDocument::from_task(&t, "t-1.md");
        doc.set_peer_id(peer).unwrap();
        doc.commit();
        doc
    }

    /// Two peers concurrently write the same field. After bidirectional
    /// import, the loser's write shows up as a detected conflict on the
    /// replica that lost.
    #[test]
    fn concurrent_same_field_produces_conflict() {
        let a = doc_with(1, "Task");
        let snap = a.export_snapshot().unwrap();

        let b = CrdtDocument::from_snapshot(&snap, "t-1.md").unwrap();
        b.set_peer_id(2).unwrap();

        // Each peer writes a different value to `status`.
        a.set_field("status", "Done");
        a.commit();
        b.set_field("status", "Cancelled");
        b.commit();

        // Snapshot A's state before it receives B's update.
        let before_a = a.snapshot_fields();
        let b_update = b.export_updates_since(&a.version_vector()).unwrap();
        a.import(&b_update).unwrap();

        let conflicts = detect_field_conflicts(&a, &before_a, 1);

        // Exactly one conflict, on `status`. Which value wins is Loro's LWW
        // call; the detector's job is to preserve both.
        let status_conflict = conflicts.iter().find(|c| c.field == "status");
        assert!(
            status_conflict.is_some(),
            "expected a status conflict, got {conflicts:?}"
        );
        let c = status_conflict.unwrap();
        assert_eq!(c.losing_value.as_deref(), Some("Done"));
        assert!(matches!(
            c.winning_value.as_deref(),
            Some("Done") | Some("Cancelled")
        ));
        assert_eq!(c.losing_peer, Some(1));
    }

    /// Disjoint field writes merge without generating conflicts.
    #[test]
    fn disjoint_writes_produce_no_conflicts() {
        let a = doc_with(1, "Task");
        let snap = a.export_snapshot().unwrap();
        let b = CrdtDocument::from_snapshot(&snap, "t-1.md").unwrap();
        b.set_peer_id(2).unwrap();

        a.set_field("status", "Done");
        a.commit();
        b.set_field("assignee", "amy");
        b.commit();

        let before_a = a.snapshot_fields();
        let b_update = b.export_updates_since(&a.version_vector()).unwrap();
        a.import(&b_update).unwrap();

        let conflicts = detect_field_conflicts(&a, &before_a, 1);
        assert!(
            conflicts.is_empty(),
            "disjoint writes must not conflict, got {conflicts:?}"
        );
        // Final state: both fields present.
        assert_eq!(a.get_field("status").as_deref(), Some("Done"));
        assert_eq!(a.get_field("assignee").as_deref(), Some("amy"));
    }
}
