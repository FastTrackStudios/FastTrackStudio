//! SQLite-backed [`Persistence`] for native (non-wasm) builds.
//!
//! ## Schema (v1)
//!
//! ```sql
//! CREATE TABLE snapshots (
//!   doc_id     TEXT PRIMARY KEY,
//!   bytes      BLOB NOT NULL,
//!   updated_at TEXT NOT NULL
//! );
//! CREATE TABLE updates (
//!   id          INTEGER PRIMARY KEY AUTOINCREMENT,
//!   doc_id      TEXT NOT NULL,
//!   bytes       BLOB NOT NULL,
//!   appended_at TEXT NOT NULL
//! );
//! CREATE INDEX idx_updates_doc ON updates(doc_id);
//! ```
//!
//! Mirrors the IDB shape on the wasm side so the offline
//! protocol stays consistent: snapshot per doc, append-only
//! update log, `compact` swaps in a fresh snapshot + clears
//! the matching updates.
//!
//! ## Threading
//!
//! `rusqlite::Connection` is `!Sync`, so we wrap it in a
//! `tokio::sync::Mutex` and call through `spawn_blocking` to
//! avoid blocking the async runtime. That's the standard
//! pattern for using sync drivers under tokio without pulling
//! in `sqlx`.

use async_trait::async_trait;
use crdt::{PersistError, Persistence};
use rusqlite::{Connection, params};
use std::path::Path;
use std::sync::Arc;
use tokio::sync::Mutex;
use uuid::Uuid;

#[derive(Clone)]
pub struct SqlitePersistence {
    inner: Arc<Mutex<Connection>>,
}

impl SqlitePersistence {
    /// Open (or create) a SQLite DB at `path`. Runs schema
    /// migrations idempotently — safe to call on every startup.
    pub async fn open(path: impl AsRef<Path>) -> Result<Self, PersistError> {
        let path = path.as_ref().to_owned();
        let conn = tokio::task::spawn_blocking(move || -> Result<Connection, rusqlite::Error> {
            let conn = Connection::open(&path)?;
            // WAL gives concurrent readers + a single writer
            // and survives `kill -9` better than the default.
            // Pragma must run on a fresh connection before any
            // schema work.
            conn.pragma_update(None, "journal_mode", "WAL")?;
            conn.pragma_update(None, "foreign_keys", "ON")?;
            conn.execute_batch(SCHEMA_V1)?;
            Ok(conn)
        })
        .await
        .map_err(|e| PersistError::Backend(format!("spawn open: {e}")))?
        .map_err(|e| PersistError::Backend(format!("open db: {e}")))?;
        Ok(Self {
            inner: Arc::new(Mutex::new(conn)),
        })
    }

    /// In-memory DB — for tests, ephemeral workflows, and
    /// integration scenarios where you don't want a file.
    pub async fn in_memory() -> Result<Self, PersistError> {
        let conn = tokio::task::spawn_blocking(|| -> Result<Connection, rusqlite::Error> {
            let conn = Connection::open_in_memory()?;
            conn.execute_batch(SCHEMA_V1)?;
            Ok(conn)
        })
        .await
        .map_err(|e| PersistError::Backend(format!("spawn open: {e}")))?
        .map_err(|e| PersistError::Backend(format!("open db: {e}")))?;
        Ok(Self {
            inner: Arc::new(Mutex::new(conn)),
        })
    }

    async fn with_conn<F, R>(&self, f: F) -> Result<R, PersistError>
    where
        F: FnOnce(&mut Connection) -> Result<R, rusqlite::Error> + Send + 'static,
        R: Send + 'static,
    {
        let conn = self.inner.clone();
        tokio::task::spawn_blocking(move || {
            // Hold the lock for the duration of the closure —
            // SQLite serializes writes anyway, and this avoids
            // re-acquiring per statement.
            let mut guard = conn.blocking_lock();
            f(&mut guard)
        })
        .await
        .map_err(|e| PersistError::Backend(format!("spawn: {e}")))?
        .map_err(|e| PersistError::Backend(format!("sqlite: {e}")))
    }
}

#[async_trait]
impl Persistence for SqlitePersistence {
    async fn load_snapshot(&self, doc_id: Uuid) -> Result<Option<Vec<u8>>, PersistError> {
        self.with_conn(move |c| {
            let mut stmt = c.prepare_cached("SELECT bytes FROM snapshots WHERE doc_id = ?1")?;
            let mut rows = stmt.query(params![doc_id.to_string()])?;
            if let Some(row) = rows.next()? {
                let bytes: Vec<u8> = row.get(0)?;
                Ok(Some(bytes))
            } else {
                Ok(None)
            }
        })
        .await
    }

    async fn write_snapshot(&self, doc_id: Uuid, bytes: &[u8]) -> Result<(), PersistError> {
        let bytes = bytes.to_vec();
        self.with_conn(move |c| {
            let now = chrono::Utc::now().to_rfc3339();
            c.execute(
                "INSERT INTO snapshots (doc_id, bytes, updated_at) VALUES (?1, ?2, ?3)
                 ON CONFLICT(doc_id) DO UPDATE SET bytes = excluded.bytes, updated_at = excluded.updated_at",
                params![doc_id.to_string(), bytes, now],
            )?;
            Ok(())
        })
        .await
    }

    async fn append_update(&self, doc_id: Uuid, bytes: &[u8]) -> Result<(), PersistError> {
        let bytes = bytes.to_vec();
        self.with_conn(move |c| {
            let now = chrono::Utc::now().to_rfc3339();
            c.execute(
                "INSERT INTO updates (doc_id, bytes, appended_at) VALUES (?1, ?2, ?3)",
                params![doc_id.to_string(), bytes, now],
            )?;
            Ok(())
        })
        .await
    }

    async fn load_updates(&self, doc_id: Uuid) -> Result<Vec<Vec<u8>>, PersistError> {
        self.with_conn(move |c| {
            let mut stmt =
                c.prepare_cached("SELECT bytes FROM updates WHERE doc_id = ?1 ORDER BY id ASC")?;
            let rows =
                stmt.query_map(params![doc_id.to_string()], |row| row.get::<_, Vec<u8>>(0))?;
            let mut out = Vec::new();
            for r in rows {
                out.push(r?);
            }
            Ok(out)
        })
        .await
    }

    async fn compact(&self, doc_id: Uuid, snapshot: &[u8]) -> Result<(), PersistError> {
        let snapshot = snapshot.to_vec();
        self.with_conn(move |c| {
            let now = chrono::Utc::now().to_rfc3339();
            let tx = c.transaction()?;
            tx.execute(
                "INSERT INTO snapshots (doc_id, bytes, updated_at) VALUES (?1, ?2, ?3)
                 ON CONFLICT(doc_id) DO UPDATE SET bytes = excluded.bytes, updated_at = excluded.updated_at",
                params![doc_id.to_string(), snapshot, now],
            )?;
            tx.execute(
                "DELETE FROM updates WHERE doc_id = ?1",
                params![doc_id.to_string()],
            )?;
            tx.commit()?;
            Ok(())
        })
        .await
    }
}

const SCHEMA_V1: &str = r#"
CREATE TABLE IF NOT EXISTS snapshots (
    doc_id     TEXT PRIMARY KEY,
    bytes      BLOB NOT NULL,
    updated_at TEXT NOT NULL
);
CREATE TABLE IF NOT EXISTS updates (
    id          INTEGER PRIMARY KEY AUTOINCREMENT,
    doc_id      TEXT NOT NULL,
    bytes       BLOB NOT NULL,
    appended_at TEXT NOT NULL
);
CREATE INDEX IF NOT EXISTS idx_updates_doc ON updates(doc_id);
"#;

#[cfg(test)]
mod tests {
    use super::*;

    async fn fresh() -> SqlitePersistence {
        SqlitePersistence::in_memory().await.expect("in-memory db")
    }

    #[tokio::test]
    async fn snapshot_roundtrip() {
        let p = fresh().await;
        let id = Uuid::new_v4();
        assert!(p.load_snapshot(id).await.unwrap().is_none());
        p.write_snapshot(id, b"hello world").await.unwrap();
        let got = p.load_snapshot(id).await.unwrap().unwrap();
        assert_eq!(got, b"hello world");
    }

    #[tokio::test]
    async fn snapshot_upsert_replaces() {
        let p = fresh().await;
        let id = Uuid::new_v4();
        p.write_snapshot(id, b"v1").await.unwrap();
        p.write_snapshot(id, b"v2").await.unwrap();
        let got = p.load_snapshot(id).await.unwrap().unwrap();
        assert_eq!(got, b"v2");
    }

    #[tokio::test]
    async fn updates_append_and_load_in_order() {
        let p = fresh().await;
        let id = Uuid::new_v4();
        for chunk in [b"a", b"b", b"c"] {
            p.append_update(id, chunk).await.unwrap();
        }
        let loaded = p.load_updates(id).await.unwrap();
        assert_eq!(loaded, vec![b"a".to_vec(), b"b".to_vec(), b"c".to_vec()]);
    }

    #[tokio::test]
    async fn updates_isolated_per_doc() {
        let p = fresh().await;
        let a = Uuid::new_v4();
        let b = Uuid::new_v4();
        p.append_update(a, b"alpha").await.unwrap();
        p.append_update(b, b"beta").await.unwrap();
        assert_eq!(p.load_updates(a).await.unwrap(), vec![b"alpha".to_vec()]);
        assert_eq!(p.load_updates(b).await.unwrap(), vec![b"beta".to_vec()]);
    }

    #[tokio::test]
    async fn compact_clears_updates_for_doc_only() {
        let p = fresh().await;
        let a = Uuid::new_v4();
        let b = Uuid::new_v4();
        p.append_update(a, b"a1").await.unwrap();
        p.append_update(a, b"a2").await.unwrap();
        p.append_update(b, b"b1").await.unwrap();
        p.compact(a, b"snap-a").await.unwrap();
        assert!(p.load_updates(a).await.unwrap().is_empty());
        assert_eq!(p.load_updates(b).await.unwrap(), vec![b"b1".to_vec()]);
        assert_eq!(p.load_snapshot(a).await.unwrap().unwrap(), b"snap-a");
    }

    #[tokio::test]
    async fn schema_idempotent_on_reopen() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("vault.sqlite");
        {
            let p = SqlitePersistence::open(&path).await.unwrap();
            let id = Uuid::new_v4();
            p.write_snapshot(id, b"persisted").await.unwrap();
        }
        let p = SqlitePersistence::open(&path).await.unwrap();
        // Schema migrations re-running shouldn't break existing data.
        // (We can't read it back without the original UUID; the
        // point is that `open` doesn't error on existing tables.)
        let _ = p;
    }
}
