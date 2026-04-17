//! Offline operation queue.
//!
//! When the client is disconnected from the server, CRDT operations
//! are stored locally. On reconnect, they're replayed against the
//! server's state and merged automatically (CRDTs guarantee convergence).
//!
//! The queue is persisted to a file so operations survive app restarts.

use std::path::Path;

use crate::service::VaultError;

/// A queued operation waiting to be synced.
#[derive(Debug, Clone)]
pub struct QueuedOp {
    /// When the operation was created.
    pub timestamp: String,
    /// Which file this operation applies to.
    pub file_path: String,
    /// The operation type.
    pub op: QueuedOpType,
}

#[derive(Debug, Clone)]
pub enum QueuedOpType {
    /// Metadata field change.
    FieldChange { field: String, value: String },
    /// Body text update (Yrs binary).
    BodyUpdate { update: Vec<u8> },
    /// Full task save (Automerge binary).
    MetadataSnapshot { data: Vec<u8> },
}

/// Manages the offline operation queue.
pub struct OfflineQueue {
    /// Queued operations not yet synced.
    ops: Vec<QueuedOp>,
    /// Path to the queue persistence file.
    persist_path: Option<std::path::PathBuf>,
}

impl OfflineQueue {
    /// Create a new in-memory queue (no persistence).
    pub fn new() -> Self {
        Self {
            ops: Vec::new(),
            persist_path: None,
        }
    }

    /// Create a queue with file persistence.
    pub fn with_persistence(path: &Path) -> Self {
        let mut queue = Self {
            ops: Vec::new(),
            persist_path: Some(path.to_path_buf()),
        };
        queue.load();
        queue
    }

    /// Enqueue an operation.
    pub fn push(&mut self, op: QueuedOp) {
        self.ops.push(op);
        self.save();
    }

    /// Get the count of pending operations.
    pub fn len(&self) -> usize {
        self.ops.len()
    }

    /// Check if the queue is empty.
    pub fn is_empty(&self) -> bool {
        self.ops.is_empty()
    }

    /// Drain all pending operations for replay.
    pub fn drain(&mut self) -> Vec<QueuedOp> {
        let ops = std::mem::take(&mut self.ops);
        self.save();
        ops
    }

    /// Replay all queued operations against the sync engine.
    pub async fn replay(
        &mut self,
        engine: &super::sync::CrdtSyncEngine,
    ) -> Result<usize, VaultError> {
        let ops = self.drain();
        let count = ops.len();

        for op in ops {
            match op.op {
                QueuedOpType::FieldChange { field, value } => {
                    engine
                        .apply_field_change(&op.file_path, &field, &value)
                        .await?;
                }
                QueuedOpType::BodyUpdate { update } => {
                    engine.apply_body_update(&op.file_path, &update).await?;
                }
                QueuedOpType::MetadataSnapshot { .. } => {
                    // Full snapshot — load and merge
                    // TODO: implement full Automerge doc merge
                }
            }
        }

        Ok(count)
    }

    /// Save queue to disk (if persistence is configured).
    fn save(&self) {
        let Some(ref path) = self.persist_path else {
            return;
        };
        // Simple format: one JSON line per op
        let lines: Vec<String> = self
            .ops
            .iter()
            .map(|op| {
                match &op.op {
                    QueuedOpType::FieldChange { field, value } => {
                        format!(
                            r#"{{"ts":"{}","path":"{}","type":"field","field":"{}","value":"{}"}}"#,
                            op.timestamp, op.file_path, field, value
                        )
                    }
                    QueuedOpType::BodyUpdate { update } => {
                        let b64 = base64_encode(update);
                        format!(
                            r#"{{"ts":"{}","path":"{}","type":"body","update":"{}"}}"#,
                            op.timestamp, op.file_path, b64
                        )
                    }
                    QueuedOpType::MetadataSnapshot { data } => {
                        let b64 = base64_encode(data);
                        format!(
                            r#"{{"ts":"{}","path":"{}","type":"snapshot","data":"{}"}}"#,
                            op.timestamp, op.file_path, b64
                        )
                    }
                }
            })
            .collect();

        let content = lines.join("\n");
        let _ = std::fs::write(path, content);
    }

    /// Load queue from disk.
    fn load(&mut self) {
        let Some(ref path) = self.persist_path else {
            return;
        };
        let content = match std::fs::read_to_string(path) {
            Ok(c) => c,
            Err(_) => return,
        };

        for line in content.lines() {
            if line.trim().is_empty() {
                continue;
            }
            // Simple JSON parsing (field extraction)
            let ts = extract_json_field(line, "ts").unwrap_or_default();
            let file_path = extract_json_field(line, "path").unwrap_or_default();
            let op_type = extract_json_field(line, "type").unwrap_or_default();

            let op = match op_type.as_str() {
                "field" => {
                    let field = extract_json_field(line, "field").unwrap_or_default();
                    let value = extract_json_field(line, "value").unwrap_or_default();
                    QueuedOpType::FieldChange { field, value }
                }
                "body" => {
                    let b64 = extract_json_field(line, "update").unwrap_or_default();
                    QueuedOpType::BodyUpdate {
                        update: base64_decode(&b64),
                    }
                }
                "snapshot" => {
                    let b64 = extract_json_field(line, "data").unwrap_or_default();
                    QueuedOpType::MetadataSnapshot {
                        data: base64_decode(&b64),
                    }
                }
                _ => continue,
            };

            self.ops.push(QueuedOp {
                timestamp: ts,
                file_path,
                op,
            });
        }
    }
}

fn extract_json_field(json: &str, key: &str) -> Option<String> {
    let pattern = format!(r#""{}":""#, key);
    let start = json.find(&pattern)? + pattern.len();
    let rest = &json[start..];
    let end = rest.find('"')?;
    Some(rest[..end].to_string())
}

fn base64_encode(data: &[u8]) -> String {
    // Simple base64 encode (no external dep)
    const CHARS: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let mut result = String::new();
    for chunk in data.chunks(3) {
        let b0 = chunk[0] as u32;
        let b1 = chunk.get(1).copied().unwrap_or(0) as u32;
        let b2 = chunk.get(2).copied().unwrap_or(0) as u32;
        let n = (b0 << 16) | (b1 << 8) | b2;
        result.push(CHARS[((n >> 18) & 63) as usize] as char);
        result.push(CHARS[((n >> 12) & 63) as usize] as char);
        if chunk.len() > 1 {
            result.push(CHARS[((n >> 6) & 63) as usize] as char);
        } else {
            result.push('=');
        }
        if chunk.len() > 2 {
            result.push(CHARS[(n & 63) as usize] as char);
        } else {
            result.push('=');
        }
    }
    result
}

fn base64_decode(s: &str) -> Vec<u8> {
    let s = s.trim_end_matches('=');
    let mut result = Vec::new();
    let chars: Vec<u8> = s
        .bytes()
        .map(|b| match b {
            b'A'..=b'Z' => b - b'A',
            b'a'..=b'z' => b - b'a' + 26,
            b'0'..=b'9' => b - b'0' + 52,
            b'+' => 62,
            b'/' => 63,
            _ => 0,
        })
        .collect();

    for chunk in chars.chunks(4) {
        let n = (chunk[0] as u32) << 18
            | chunk.get(1).copied().unwrap_or(0) as u32 * 4096
            | chunk.get(2).copied().unwrap_or(0) as u32 * 64
            | chunk.get(3).copied().unwrap_or(0) as u32;
        result.push((n >> 16) as u8);
        if chunk.len() > 2 {
            result.push((n >> 8) as u8);
        }
        if chunk.len() > 3 {
            result.push(n as u8);
        }
    }
    result
}
