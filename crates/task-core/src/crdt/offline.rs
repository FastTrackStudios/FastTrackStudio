//! Offline operation queue.
//!
//! When the client is disconnected, CRDT operations are staged locally. On
//! reconnect, they replay against the server's state. Loro guarantees
//! convergence — out-of-order imports are fine.
//!
//! The queue persists to a flat file so it survives restarts.

use std::path::Path;

use crate::service::VaultError;

#[derive(Debug, Clone)]
pub struct QueuedOp {
    pub timestamp: String,
    pub file_path: String,
    pub op: QueuedOpType,
}

/// Operation kinds the queue persists.
#[derive(Debug, Clone)]
pub enum QueuedOpType {
    /// Metadata field assignment.
    FieldChange { field: String, value: String },
    /// Raw Loro update bytes (body edits, list ops, etc.).
    LoroUpdate { update: Vec<u8> },
    /// Loro full snapshot (for cold-start / big catchup).
    Snapshot { data: Vec<u8> },
}

/// A file-backed queue of pending operations.
pub struct OfflineQueue {
    ops: Vec<QueuedOp>,
    persist_path: Option<std::path::PathBuf>,
}

impl OfflineQueue {
    pub fn new() -> Self {
        Self {
            ops: Vec::new(),
            persist_path: None,
        }
    }

    pub fn with_persistence(path: &Path) -> Self {
        let mut queue = Self {
            ops: Vec::new(),
            persist_path: Some(path.to_path_buf()),
        };
        queue.load();
        queue
    }

    pub fn push(&mut self, op: QueuedOp) {
        self.ops.push(op);
        self.save();
    }

    pub fn len(&self) -> usize {
        self.ops.len()
    }

    pub fn is_empty(&self) -> bool {
        self.ops.is_empty()
    }

    pub fn drain(&mut self) -> Vec<QueuedOp> {
        let ops = std::mem::take(&mut self.ops);
        self.save();
        ops
    }

    /// Replay every queued op against the sync engine.
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
                QueuedOpType::LoroUpdate { update } => {
                    engine.apply_remote_update(&op.file_path, &update).await?;
                }
                QueuedOpType::Snapshot { data } => {
                    // Snapshots replay via the same import path — Loro
                    // auto-detects snapshot vs updates.
                    engine.apply_remote_update(&op.file_path, &data).await?;
                }
            }
        }

        Ok(count)
    }

    fn save(&self) {
        let Some(ref path) = self.persist_path else {
            return;
        };
        let lines: Vec<String> = self
            .ops
            .iter()
            .map(|op| match &op.op {
                QueuedOpType::FieldChange { field, value } => format!(
                    r#"{{"ts":"{}","path":"{}","type":"field","field":"{}","value":"{}"}}"#,
                    op.timestamp, op.file_path, field, value
                ),
                QueuedOpType::LoroUpdate { update } => format!(
                    r#"{{"ts":"{}","path":"{}","type":"update","update":"{}"}}"#,
                    op.timestamp,
                    op.file_path,
                    base64_encode(update)
                ),
                QueuedOpType::Snapshot { data } => format!(
                    r#"{{"ts":"{}","path":"{}","type":"snapshot","data":"{}"}}"#,
                    op.timestamp,
                    op.file_path,
                    base64_encode(data)
                ),
            })
            .collect();

        let _ = std::fs::write(path, lines.join("\n"));
    }

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
            let ts = extract_json_field(line, "ts").unwrap_or_default();
            let file_path = extract_json_field(line, "path").unwrap_or_default();
            let op_type = extract_json_field(line, "type").unwrap_or_default();

            let op = match op_type.as_str() {
                "field" => {
                    let field = extract_json_field(line, "field").unwrap_or_default();
                    let value = extract_json_field(line, "value").unwrap_or_default();
                    QueuedOpType::FieldChange { field, value }
                }
                "update" => QueuedOpType::LoroUpdate {
                    update: base64_decode(&extract_json_field(line, "update").unwrap_or_default()),
                },
                "snapshot" => QueuedOpType::Snapshot {
                    data: base64_decode(&extract_json_field(line, "data").unwrap_or_default()),
                },
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

impl Default for OfflineQueue {
    fn default() -> Self {
        Self::new()
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
        let n = ((chunk[0] as u32) << 18)
            | ((chunk.get(1).copied().unwrap_or(0) as u32) * 4096)
            | ((chunk.get(2).copied().unwrap_or(0) as u32) * 64)
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn queue_round_trip() {
        let mut q = OfflineQueue::new();
        q.push(QueuedOp {
            timestamp: "2026-04-17T00:00:00Z".into(),
            file_path: "t-1.md".into(),
            op: QueuedOpType::FieldChange {
                field: "status".into(),
                value: "Done".into(),
            },
        });
        q.push(QueuedOp {
            timestamp: "2026-04-17T00:00:01Z".into(),
            file_path: "t-1.md".into(),
            op: QueuedOpType::LoroUpdate {
                update: vec![1, 2, 3, 4],
            },
        });
        assert_eq!(q.len(), 2);
        let drained = q.drain();
        assert_eq!(drained.len(), 2);
        assert_eq!(q.len(), 0);
    }
}
