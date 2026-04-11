//! CRDT ↔ file sync engine.
//!
//! Bridges between the in-memory CRDT state and the `.md` files on disk.
//!
//! ```text
//! .md file changes (edit, Nextcloud sync, Obsidian)
//!   │
//!   ▼ file watcher (notify)
//!   │
//!   ├─→ Parse .md → update CrdtDocument → broadcast to WebSocket clients
//!   │
//!   └─→ Update SQLite index
//!
//! CRDT operation (from WebSocket client)
//!   │
//!   ▼ apply to in-memory CrdtDocument
//!   │
//!   ├─→ Broadcast to other connected clients
//!   │
//!   ├─→ Debounced write back to .md file
//!   │
//!   └─→ Update SQLite index
//! ```

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use tokio::sync::{RwLock, broadcast, mpsc};

use crate::service::VaultError;
use crate::task::Task;
use crate::vault::Vault;
use super::document::CrdtDocument;

/// An operation that can be broadcast to connected clients.
#[derive(Debug, Clone)]
pub enum SyncOp {
    /// A task's metadata field changed.
    FieldChanged {
        file_path: String,
        field: String,
        value: String,
    },
    /// A task's body text changed (Yrs update bytes).
    BodyChanged {
        file_path: String,
        update: Vec<u8>,
    },
    /// A new task was created.
    TaskCreated {
        file_path: String,
        task: Task,
    },
    /// A task was deleted.
    TaskDeleted {
        file_path: String,
    },
    /// Full refresh needed (bulk change, rebuild).
    Refresh,
}

/// Manages the live CRDT state for all open documents.
///
/// Documents are loaded on-demand when a file is accessed, and kept
/// in memory for fast reads and real-time operations.
pub struct CrdtSyncEngine {
    /// Root directory of the vault.
    root: PathBuf,
    /// In-memory CRDT documents keyed by relative file path.
    documents: Arc<RwLock<HashMap<String, CrdtDocument>>>,
    /// Broadcast channel for sync operations.
    broadcast_tx: broadcast::Sender<SyncOp>,
    /// Channel for debounced file writes.
    write_tx: mpsc::Sender<String>,
}

impl CrdtSyncEngine {
    /// Create a new sync engine for a vault root directory.
    pub fn new(root: &Path) -> Self {
        let (broadcast_tx, _) = broadcast::channel(256);
        let (write_tx, write_rx) = mpsc::channel(64);

        let engine = Self {
            root: root.to_path_buf(),
            documents: Arc::new(RwLock::new(HashMap::new())),
            broadcast_tx,
            write_tx,
        };

        // Spawn the debounced writer
        let docs = engine.documents.clone();
        let root = engine.root.clone();
        tokio::spawn(debounced_writer(root, docs, write_rx));

        engine
    }

    /// Subscribe to sync operations (for WebSocket broadcast).
    pub fn subscribe(&self) -> broadcast::Receiver<SyncOp> {
        self.broadcast_tx.subscribe()
    }

    /// Get or load a CRDT document for a file path.
    pub async fn get_or_load(&self, rel_path: &str) -> Option<Task> {
        // Check if already loaded
        {
            let docs = self.documents.read().await;
            if let Some(doc) = docs.get(rel_path) {
                return Some(doc.to_task());
            }
        }

        // Load from disk
        let abs_path = self.root.join(rel_path);
        let content = std::fs::read_to_string(&abs_path).ok()?;
        let task = Vault::parse_task_from_md(&content)?;

        let doc = CrdtDocument::from_task(&task, rel_path);
        let result = doc.to_task();

        let mut docs = self.documents.write().await;
        docs.insert(rel_path.to_string(), doc);

        Some(result)
    }

    /// Handle a file change from the file watcher.
    pub async fn on_file_changed(&self, rel_path: &str) -> Result<(), VaultError> {
        let abs_path = self.root.join(rel_path);

        if !abs_path.exists() {
            // File was deleted
            let mut docs = self.documents.write().await;
            docs.remove(rel_path);
            let _ = self.broadcast_tx.send(SyncOp::TaskDeleted {
                file_path: rel_path.to_string(),
            });
            return Ok(());
        }

        let content = std::fs::read_to_string(&abs_path)
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        let Some(task) = Vault::parse_task_from_md(&content) else {
            return Ok(()); // Not a valid task file
        };

        let new_doc = CrdtDocument::from_task(&task, rel_path);
        let mut docs = self.documents.write().await;

        if let Some(existing) = docs.get(rel_path) {
            // Compare and detect changes
            let old_task = existing.to_task();
            if old_task.status != task.status {
                let _ = self.broadcast_tx.send(SyncOp::FieldChanged {
                    file_path: rel_path.to_string(),
                    field: "status".to_string(),
                    value: format!("{:?}", task.status),
                });
            }
            if old_task.assignee != task.assignee {
                let _ = self.broadcast_tx.send(SyncOp::FieldChanged {
                    file_path: rel_path.to_string(),
                    field: "assignee".to_string(),
                    value: task.assignee.clone().unwrap_or_default(),
                });
            }
            if old_task.title != task.title {
                let _ = self.broadcast_tx.send(SyncOp::FieldChanged {
                    file_path: rel_path.to_string(),
                    field: "title".to_string(),
                    value: task.title.clone(),
                });
            }
        } else {
            // New document
            let _ = self.broadcast_tx.send(SyncOp::TaskCreated {
                file_path: rel_path.to_string(),
                task: task.clone(),
            });
        }

        docs.insert(rel_path.to_string(), new_doc);
        Ok(())
    }

    /// Apply a field change from a remote client.
    pub async fn apply_field_change(
        &self,
        rel_path: &str,
        field: &str,
        value: &str,
    ) -> Result<(), VaultError> {
        // Ensure document is loaded
        self.get_or_load(rel_path).await;

        let mut docs = self.documents.write().await;
        if let Some(doc) = docs.get_mut(rel_path) {
            doc.set_field(field, value);

            // Broadcast to other clients
            let _ = self.broadcast_tx.send(SyncOp::FieldChanged {
                file_path: rel_path.to_string(),
                field: field.to_string(),
                value: value.to_string(),
            });

            // Queue debounced file write
            let _ = self.write_tx.send(rel_path.to_string()).await;
        }

        Ok(())
    }

    /// Apply a body text update from a remote client.
    pub async fn apply_body_update(
        &self,
        rel_path: &str,
        update: &[u8],
    ) -> Result<(), VaultError> {
        self.get_or_load(rel_path).await;

        let docs = self.documents.read().await;
        if let Some(doc) = docs.get(rel_path) {
            doc.apply_body_update(update)?;

            let _ = self.broadcast_tx.send(SyncOp::BodyChanged {
                file_path: rel_path.to_string(),
                update: update.to_vec(),
            });

            let _ = self.write_tx.send(rel_path.to_string()).await;
        }

        Ok(())
    }

    /// Get all currently loaded documents as tasks.
    pub async fn loaded_tasks(&self) -> Vec<Task> {
        let docs = self.documents.read().await;
        docs.values().map(|d| d.to_task()).collect()
    }

    /// Get the count of loaded documents.
    pub async fn loaded_count(&self) -> usize {
        self.documents.read().await.len()
    }
}

/// Background task that debounces file writes.
///
/// Collects file paths from the channel and writes them back to disk
/// after a 500ms quiet period (no more changes to that file).
async fn debounced_writer(
    root: PathBuf,
    documents: Arc<RwLock<HashMap<String, CrdtDocument>>>,
    mut rx: mpsc::Receiver<String>,
) {
    let mut pending: HashMap<String, tokio::time::Instant> = HashMap::new();
    let debounce = Duration::from_millis(500);

    loop {
        // Wait for a write request or check pending writes
        let timeout = if pending.is_empty() {
            Duration::from_secs(60) // Idle timeout
        } else {
            Duration::from_millis(100) // Check interval
        };

        match tokio::time::timeout(timeout, rx.recv()).await {
            Ok(Some(path)) => {
                // New write request — reset the debounce timer
                pending.insert(path, tokio::time::Instant::now());
            }
            Ok(None) => break, // Channel closed
            Err(_) => {} // Timeout — check pending
        }

        // Flush any pending writes past the debounce period
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
                            tracing::warn!(path = %path, error = %e, "Failed to write CRDT to file");
                        } else {
                            tracing::debug!(path = %path, "CRDT state written to file");
                        }
                    }
                    Err(e) => {
                        tracing::warn!(path = %path, error = %e, "Failed to render CRDT to markdown");
                    }
                }
            }
        }
    }
}
