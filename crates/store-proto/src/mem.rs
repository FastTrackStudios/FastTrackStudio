//! In-memory backend impl. Implements both `KvStore` and
//! `LogStore`. Used by tests + the default demo route until a
//! native disk backend is mounted.

use std::sync::Mutex;

use crate::error::StoreError;
use crate::kv_store::KvStore;
use crate::log_store::LogStore;
use crate::types::LogEntry;

#[derive(Default)]
pub struct MemStore {
    inner: Mutex<Inner>,
}

#[derive(Default)]
struct Inner {
    /// `(namespace, key) -> bytes`
    kv: std::collections::BTreeMap<(String, String), Vec<u8>>,
    /// `channel -> (next_seq, entries)`
    logs: std::collections::BTreeMap<String, (u64, Vec<LogEntry>)>,
}

impl MemStore {
    pub fn new() -> Self {
        Self::default()
    }
}

impl KvStore for MemStore {
    fn get(&self, namespace: &str, key: &str) -> Result<Option<Vec<u8>>, StoreError> {
        let inner = self.inner.lock().map_err(poisoned)?;
        Ok(inner.kv.get(&(namespace.into(), key.into())).cloned())
    }

    fn put(&self, namespace: &str, key: &str, value: Vec<u8>) -> Result<(), StoreError> {
        let mut inner = self.inner.lock().map_err(poisoned)?;
        inner.kv.insert((namespace.into(), key.into()), value);
        Ok(())
    }

    fn delete(&self, namespace: &str, key: &str) -> Result<(), StoreError> {
        let mut inner = self.inner.lock().map_err(poisoned)?;
        inner.kv.remove(&(namespace.into(), key.into()));
        Ok(())
    }

    fn list_keys(&self, namespace: &str) -> Result<Vec<String>, StoreError> {
        let inner = self.inner.lock().map_err(poisoned)?;
        Ok(inner
            .kv
            .keys()
            .filter(|(ns, _)| ns == namespace)
            .map(|(_, k)| k.clone())
            .collect())
    }
}

impl LogStore for MemStore {
    fn append(&self, channel: &str, data: Vec<u8>) -> Result<u64, StoreError> {
        let mut inner = self.inner.lock().map_err(poisoned)?;
        let entry_seq;
        {
            let (next, entries) = inner.logs.entry(channel.into()).or_insert((1, Vec::new()));
            let seq = *next;
            *next += 1;
            entry_seq = seq;
            entries.push(LogEntry {
                seq,
                appended_utc: now_rfc3339(),
                data,
            });
        }
        Ok(entry_seq)
    }

    fn read(&self, channel: &str, from_seq: u64, limit: u32) -> Result<Vec<LogEntry>, StoreError> {
        let inner = self.inner.lock().map_err(poisoned)?;
        let Some((_, entries)) = inner.logs.get(channel) else {
            return Ok(Vec::new());
        };
        Ok(entries
            .iter()
            .filter(|e| e.seq > from_seq)
            .take(limit as usize)
            .cloned()
            .collect())
    }

    fn truncate(&self, channel: &str, up_to_seq: u64) -> Result<(), StoreError> {
        let mut inner = self.inner.lock().map_err(poisoned)?;
        if let Some((_, entries)) = inner.logs.get_mut(channel) {
            entries.retain(|e| e.seq > up_to_seq);
        }
        Ok(())
    }
}

fn poisoned<T>(_: std::sync::PoisonError<T>) -> StoreError {
    StoreError::Backend {
        message: "store mutex poisoned".into(),
    }
}

/// Time provider. Pinned to a placeholder string on wasm-no-clock
/// builds; native + wasmbind builds get real timestamps. Kept here
/// so callers don't pull chrono just to write a log entry.
fn now_rfc3339() -> String {
    // We don't pull chrono in this crate to stay wasm-clean
    // without feature gates; this lightweight stamp is enough for
    // the in-memory backend. Native disk backends (store-json /
    // store-sqlite) substitute a real timestamp.
    let ns_since_epoch: u128 = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    format!("epoch-ns:{ns_since_epoch}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn kv_put_get_delete() {
        let s = MemStore::new();
        assert!(s.get("ns", "k").unwrap().is_none());
        s.put("ns", "k", b"hello".to_vec()).unwrap();
        assert_eq!(
            s.get("ns", "k").unwrap().as_deref(),
            Some(b"hello".as_ref())
        );
        s.delete("ns", "k").unwrap();
        assert!(s.get("ns", "k").unwrap().is_none());
    }

    #[test]
    fn list_keys_filters_by_namespace() {
        let s = MemStore::new();
        s.put("a", "k1", vec![]).unwrap();
        s.put("a", "k2", vec![]).unwrap();
        s.put("b", "k1", vec![]).unwrap();
        let mut a = s.list_keys("a").unwrap();
        a.sort();
        assert_eq!(a, vec!["k1".to_string(), "k2".to_string()]);
        assert_eq!(s.list_keys("b").unwrap(), vec!["k1".to_string()]);
    }

    #[test]
    fn log_append_returns_monotonic_seq() {
        let s = MemStore::new();
        let s1 = s.append("audit", b"a".to_vec()).unwrap();
        let s2 = s.append("audit", b"b".to_vec()).unwrap();
        assert!(s2 > s1);
        let entries = s.read("audit", 0, 100).unwrap();
        assert_eq!(entries.len(), 2);
        assert_eq!(entries[0].data, b"a");
    }

    #[test]
    fn log_read_skips_already_seen() {
        let s = MemStore::new();
        let s1 = s.append("c", b"a".to_vec()).unwrap();
        s.append("c", b"b".to_vec()).unwrap();
        let rest = s.read("c", s1, 100).unwrap();
        assert_eq!(rest.len(), 1);
        assert_eq!(rest[0].data, b"b");
    }

    #[test]
    fn log_truncate_drops_acknowledged() {
        let s = MemStore::new();
        s.append("c", b"a".to_vec()).unwrap();
        let s2 = s.append("c", b"b".to_vec()).unwrap();
        s.append("c", b"c".to_vec()).unwrap();
        s.truncate("c", s2).unwrap();
        let remaining = s.read("c", 0, 100).unwrap();
        assert_eq!(remaining.len(), 1);
        assert_eq!(remaining[0].data, b"c");
    }
}
