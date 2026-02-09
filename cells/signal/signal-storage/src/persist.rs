//! Key-value persistence abstraction.
//!
//! Provides a [`Persistable`] trait for saving and loading arbitrary data by
//! string key. Backends implement the trait to provide different storage
//! strategies (SQLite, file, in-memory for tests).
//!
//! Convenience functions [`save_value`] and [`load_value`] handle Facet
//! serialization/deserialization so callers work with typed values.

use async_trait::async_trait;

use crate::error::{StorageError, StorageResult};

// ─────────────────────────────────────────────────────────────────────────────
// Persistable Trait
// ─────────────────────────────────────────────────────────────────────────────

/// Trait for persisting arbitrary typed data by string key.
///
/// Backends (SQLite, file, memory) implement this to provide
/// save/load semantics without coupling to a specific storage engine.
/// All values are stored as JSON strings — the caller handles serialization.
#[async_trait]
pub trait Persistable: Send + Sync {
    /// Save a JSON string under the given key, overwriting any previous value.
    async fn save(&self, key: &str, json: &str) -> StorageResult<()>;

    /// Load the JSON string stored under the given key.
    /// Returns `None` if the key doesn't exist.
    async fn load(&self, key: &str) -> StorageResult<Option<String>>;

    /// Delete the value stored under the given key.
    /// Returns `true` if a value was deleted, `false` if no value existed.
    async fn delete(&self, key: &str) -> StorageResult<bool>;

    /// List all keys, optionally filtered by prefix.
    async fn list_keys(&self, prefix: Option<&str>) -> StorageResult<Vec<String>>;
}

// ─────────────────────────────────────────────────────────────────────────────
// Convenience Functions
// ─────────────────────────────────────────────────────────────────────────────

/// Save a Facet-serializable value under the given key.
pub async fn save_value<'a, T: for<'b> facet::Facet<'b>>(
    store: &'a dyn Persistable,
    key: &'a str,
    value: &T,
) -> StorageResult<()> {
    let json = facet_json::to_string(value)
        .map_err(|e| StorageError::Config(format!("failed to serialize key '{key}': {e}")))?;
    store.save(key, &json).await
}

/// Load a Facet-deserializable value from the given key.
///
/// Returns `None` if the key doesn't exist. Returns an error if the
/// stored JSON fails to deserialize into `T`.
pub async fn load_value<'a, T: for<'b> facet::Facet<'b>>(
    store: &'a dyn Persistable,
    key: &'a str,
) -> StorageResult<Option<T>> {
    match store.load(key).await? {
        Some(json) => {
            let value: T = facet_json::from_str(&json).map_err(|e| {
                StorageError::Config(format!("failed to deserialize key '{key}': {e}"))
            })?;
            Ok(Some(value))
        }
        None => Ok(None),
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// MemoryBackend
// ─────────────────────────────────────────────────────────────────────────────

/// In-memory implementation of [`Persistable`] for testing.
pub struct MemoryBackend {
    data: std::sync::Mutex<std::collections::HashMap<String, String>>,
}

impl MemoryBackend {
    /// Create a new empty in-memory store.
    pub fn new() -> Self {
        Self {
            data: std::sync::Mutex::new(std::collections::HashMap::new()),
        }
    }
}

impl Default for MemoryBackend {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl Persistable for MemoryBackend {
    async fn save(&self, key: &str, json: &str) -> StorageResult<()> {
        self.data
            .lock()
            .unwrap()
            .insert(key.to_string(), json.to_string());
        Ok(())
    }

    async fn load(&self, key: &str) -> StorageResult<Option<String>> {
        Ok(self.data.lock().unwrap().get(key).cloned())
    }

    async fn delete(&self, key: &str) -> StorageResult<bool> {
        Ok(self.data.lock().unwrap().remove(key).is_some())
    }

    async fn list_keys(&self, prefix: Option<&str>) -> StorageResult<Vec<String>> {
        let data = self.data.lock().unwrap();
        let keys = match prefix {
            Some(p) => data.keys().filter(|k| k.starts_with(p)).cloned().collect(),
            None => data.keys().cloned().collect(),
        };
        Ok(keys)
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// SqliteBackend
// ─────────────────────────────────────────────────────────────────────────────

use sqlx::sqlite::{SqliteConnectOptions, SqlitePool, SqlitePoolOptions};
use sqlx::Row;

use crate::local_config::LocalConfig;

/// SQLite-backed implementation of [`Persistable`].
///
/// Uses a simple `kv_store` table in the existing signal SQLite database.
/// The table is created automatically on first use via [`ensure_table`](Self::ensure_table).
pub struct SqliteBackend {
    pool: SqlitePool,
}

impl SqliteBackend {
    /// Create from an existing connection pool.
    pub fn new(pool: SqlitePool) -> Self {
        Self { pool }
    }

    /// Connect using a [`LocalConfig`] and ensure the KV table exists.
    pub async fn from_config(config: &LocalConfig) -> StorageResult<Self> {
        let dir = config.database_dir();
        std::fs::create_dir_all(dir).map_err(|e| {
            StorageError::Config(format!(
                "failed to create database directory {}: {e}",
                dir.display()
            ))
        })?;

        let options = SqliteConnectOptions::new()
            .filename(&config.database_path)
            .create_if_missing(true)
            .journal_mode(sqlx::sqlite::SqliteJournalMode::Wal)
            .busy_timeout(std::time::Duration::from_millis(config.busy_timeout_ms));

        let pool = SqlitePoolOptions::new()
            .max_connections(1)
            .connect_with(options)
            .await
            .map_err(|e| {
                StorageError::ConnectionUnavailable(format!("failed to open SQLite: {e}"))
            })?;

        let backend = Self::new(pool);
        backend.ensure_table().await?;
        Ok(backend)
    }

    /// Connect using the platform-default database path.
    pub async fn from_default_path() -> StorageResult<Self> {
        let config = LocalConfig::default_path()?;
        Self::from_config(&config).await
    }

    /// Create the `kv_store` table if it doesn't already exist.
    pub async fn ensure_table(&self) -> StorageResult<()> {
        sqlx::query(
            "CREATE TABLE IF NOT EXISTS kv_store (
                key TEXT PRIMARY KEY NOT NULL,
                value TEXT NOT NULL,
                updated_at TEXT NOT NULL DEFAULT (datetime('now'))
            )",
        )
        .execute(&self.pool)
        .await
        .map_err(|e| {
            StorageError::ConnectionUnavailable(format!("failed to create kv_store table: {e}"))
        })?;

        Ok(())
    }
}

#[async_trait]
impl Persistable for SqliteBackend {
    async fn save(&self, key: &str, json: &str) -> StorageResult<()> {
        sqlx::query(
            "INSERT INTO kv_store (key, value, updated_at)
             VALUES (?1, ?2, datetime('now'))
             ON CONFLICT(key) DO UPDATE SET
                value = excluded.value,
                updated_at = excluded.updated_at",
        )
        .bind(key)
        .bind(json)
        .execute(&self.pool)
        .await?;

        Ok(())
    }

    async fn load(&self, key: &str) -> StorageResult<Option<String>> {
        let row = sqlx::query("SELECT value FROM kv_store WHERE key = ?1")
            .bind(key)
            .fetch_optional(&self.pool)
            .await?;

        Ok(row.map(|r| r.get::<String, _>("value")))
    }

    async fn delete(&self, key: &str) -> StorageResult<bool> {
        let result = sqlx::query("DELETE FROM kv_store WHERE key = ?1")
            .bind(key)
            .execute(&self.pool)
            .await?;

        Ok(result.rows_affected() > 0)
    }

    async fn list_keys(&self, prefix: Option<&str>) -> StorageResult<Vec<String>> {
        let rows = match prefix {
            Some(p) => {
                let pattern = format!("{p}%");
                sqlx::query("SELECT key FROM kv_store WHERE key LIKE ?1 ORDER BY key")
                    .bind(pattern)
                    .fetch_all(&self.pool)
                    .await?
            }
            None => {
                sqlx::query("SELECT key FROM kv_store ORDER BY key")
                    .fetch_all(&self.pool)
                    .await?
            }
        };

        Ok(rows.iter().map(|r| r.get::<String, _>("key")).collect())
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    // -- MemoryBackend Tests

    #[tokio::test]
    async fn memory_save_load_roundtrip() -> Result<()> {
        let store = MemoryBackend::new();
        store.save("test:key", r#"{"hello":"world"}"#).await?;
        let loaded = store.load("test:key").await?;
        assert_eq!(loaded.as_deref(), Some(r#"{"hello":"world"}"#));
        Ok(())
    }

    #[tokio::test]
    async fn memory_load_missing_returns_none() -> Result<()> {
        let store = MemoryBackend::new();
        let loaded = store.load("nonexistent").await?;
        assert!(loaded.is_none());
        Ok(())
    }

    #[tokio::test]
    async fn memory_save_overwrites() -> Result<()> {
        let store = MemoryBackend::new();
        store.save("key", "v1").await?;
        store.save("key", "v2").await?;
        let loaded = store.load("key").await?;
        assert_eq!(loaded.as_deref(), Some("v2"));
        Ok(())
    }

    #[tokio::test]
    async fn memory_delete() -> Result<()> {
        let store = MemoryBackend::new();
        store.save("key", "value").await?;
        let deleted = store.delete("key").await?;
        assert!(deleted);
        let loaded = store.load("key").await?;
        assert!(loaded.is_none());

        // Delete nonexistent returns false
        let deleted = store.delete("nonexistent").await?;
        assert!(!deleted);
        Ok(())
    }

    #[tokio::test]
    async fn memory_list_keys() -> Result<()> {
        let store = MemoryBackend::new();
        store.save("alpha", "1").await?;
        store.save("beta", "2").await?;
        store.save("gamma", "3").await?;

        let mut keys = store.list_keys(None).await?;
        keys.sort();
        assert_eq!(keys, vec!["alpha", "beta", "gamma"]);
        Ok(())
    }

    #[tokio::test]
    async fn memory_list_keys_with_prefix() -> Result<()> {
        let store = MemoryBackend::new();
        store.save("rig:graph", "1").await?;
        store.save("rig:snapshots", "2").await?;
        store.save("config:theme", "3").await?;

        let mut keys = store.list_keys(Some("rig:")).await?;
        keys.sort();
        assert_eq!(keys, vec!["rig:graph", "rig:snapshots"]);
        Ok(())
    }

    // -- Typed save_value / load_value Tests

    #[derive(Debug, Clone, PartialEq, facet::Facet)]
    struct TestData {
        name: String,
        value: f64,
    }

    #[tokio::test]
    async fn save_value_load_value_roundtrip() -> Result<()> {
        let store = MemoryBackend::new();
        let data = TestData {
            name: "test".to_string(),
            value: 42.5,
        };

        save_value(&store, "test:data", &data).await?;
        let loaded: Option<TestData> = load_value(&store, "test:data").await?;

        assert_eq!(loaded, Some(data));
        Ok(())
    }

    #[tokio::test]
    async fn load_value_missing_returns_none() -> Result<()> {
        let store = MemoryBackend::new();
        let loaded: Option<TestData> = load_value(&store, "nonexistent").await?;
        assert!(loaded.is_none());
        Ok(())
    }

    // -- SqliteBackend Tests

    /// Create an in-memory SQLite backend for testing.
    async fn test_sqlite() -> SqliteBackend {
        let options = SqliteConnectOptions::new()
            .filename(":memory:")
            .create_if_missing(true)
            .journal_mode(sqlx::sqlite::SqliteJournalMode::Wal);

        let pool = SqlitePoolOptions::new()
            .max_connections(1)
            .connect_with(options)
            .await
            .expect("failed to create in-memory SQLite pool");

        let backend = SqliteBackend::new(pool);
        backend
            .ensure_table()
            .await
            .expect("failed to create kv_store table");
        backend
    }

    #[tokio::test]
    async fn sqlite_save_load_roundtrip() -> Result<()> {
        let store = test_sqlite().await;
        store.save("test:key", r#"{"hello":"world"}"#).await?;
        let loaded = store.load("test:key").await?;
        assert_eq!(loaded.as_deref(), Some(r#"{"hello":"world"}"#));
        Ok(())
    }

    #[tokio::test]
    async fn sqlite_save_overwrites() -> Result<()> {
        let store = test_sqlite().await;
        store.save("key", "v1").await?;
        store.save("key", "v2").await?;
        let loaded = store.load("key").await?;
        assert_eq!(loaded.as_deref(), Some("v2"));
        Ok(())
    }

    #[tokio::test]
    async fn sqlite_delete() -> Result<()> {
        let store = test_sqlite().await;
        store.save("key", "value").await?;
        let deleted = store.delete("key").await?;
        assert!(deleted);
        let loaded = store.load("key").await?;
        assert!(loaded.is_none());

        let deleted = store.delete("nonexistent").await?;
        assert!(!deleted);
        Ok(())
    }

    #[tokio::test]
    async fn sqlite_list_keys() -> Result<()> {
        let store = test_sqlite().await;
        store.save("rig:graph", "1").await?;
        store.save("rig:snapshots", "2").await?;
        store.save("config:theme", "3").await?;

        let all_keys = store.list_keys(None).await?;
        assert_eq!(all_keys.len(), 3);

        let rig_keys = store.list_keys(Some("rig:")).await?;
        assert_eq!(rig_keys, vec!["rig:graph", "rig:snapshots"]);
        Ok(())
    }

    #[tokio::test]
    async fn sqlite_typed_roundtrip() -> Result<()> {
        let store = test_sqlite().await;
        let data = TestData {
            name: "sqlite_test".to_string(),
            value: 99.9,
        };

        save_value(&store, "typed:data", &data).await?;
        let loaded: Option<TestData> = load_value(&store, "typed:data").await?;
        assert_eq!(loaded, Some(data));
        Ok(())
    }
}
