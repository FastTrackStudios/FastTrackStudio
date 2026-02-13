//! LocalStorage — SQLite backend for fast local persistence.
//!
//! Uses sqlx with SQLite for async local database access.
//! Queries are built with sea-query using `SqliteQueryBuilder`.
//! Includes FTS5 full-text search for preset names and descriptions.

use async_trait::async_trait;
use chrono::Utc;
use sea_query::{Expr, Query, SqliteQueryBuilder};
use sea_query_binder::SqlxBinder;
use sqlx::sqlite::{SqliteConnectOptions, SqlitePool, SqlitePoolOptions};
use sqlx::Row;
use uuid::Uuid;

use crate::error::{StorageError, StorageResult};
use crate::local_config::LocalConfig;
use crate::schema::{self, Presets, Snapshots};
use crate::service::{PresetFilter, PresetRow, PresetSummary, SnapshotRow, StorageService};

// region: --- FTS5 Schema

/// Raw SQL for creating the FTS5 virtual table that indexes preset names and descriptions.
///
/// Uses `content=presets` to make it a content-sync table — it reads data from the
/// presets table rather than storing its own copy. The `content_rowid=rowid` tells
/// FTS5 which column maps to the internal rowid.
const FTS5_CREATE: &str = r#"
CREATE VIRTUAL TABLE IF NOT EXISTS presets_fts USING fts5(
    name,
    description,
    content=presets,
    content_rowid=rowid
)
"#;

/// Triggers to keep the FTS5 index in sync with the presets table.
/// SQLite FTS5 content-sync tables require manual trigger-based synchronization.
const FTS5_TRIGGERS: &[&str] = &[
    // After INSERT: add the new row to the FTS index
    r#"
    CREATE TRIGGER IF NOT EXISTS presets_fts_insert AFTER INSERT ON presets BEGIN
        INSERT INTO presets_fts(rowid, name, description)
        VALUES (new.rowid, new.name, new.description);
    END
    "#,
    // After UPDATE: remove old entry, add updated entry
    r#"
    CREATE TRIGGER IF NOT EXISTS presets_fts_update AFTER UPDATE ON presets BEGIN
        INSERT INTO presets_fts(presets_fts, rowid, name, description)
        VALUES ('delete', old.rowid, old.name, old.description);
        INSERT INTO presets_fts(rowid, name, description)
        VALUES (new.rowid, new.name, new.description);
    END
    "#,
    // After DELETE: remove the entry from the FTS index
    r#"
    CREATE TRIGGER IF NOT EXISTS presets_fts_delete AFTER DELETE ON presets BEGIN
        INSERT INTO presets_fts(presets_fts, rowid, name, description)
        VALUES ('delete', old.rowid, old.name, old.description);
    END
    "#,
];

// endregion: --- FTS5 Schema

/// LocalStorage — SQLite-backed storage for fast local persistence.
///
/// Holds a sqlx SQLite connection pool and configuration.
/// Includes FTS5 full-text search support for preset discovery.
pub struct LocalStorage {
    pool: SqlitePool,
    #[allow(dead_code)]
    config: LocalConfig,
}

impl LocalStorage {
    /// Connect to (or create) the local SQLite database.
    ///
    /// This creates the connection pool but does NOT run migrations.
    /// Call [`StorageService::run_migrations`] after construction.
    pub async fn connect(config: LocalConfig) -> StorageResult<Self> {
        // Ensure the parent directory exists
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
            .max_connections(1) // SQLite works best with a single writer
            .connect_with(options)
            .await
            .map_err(|e| {
                StorageError::ConnectionUnavailable(format!(
                    "failed to open SQLite database: {e}"
                ))
            })?;

        // Enable foreign keys (off by default in SQLite)
        sqlx::query("PRAGMA foreign_keys = ON")
            .execute(&pool)
            .await?;

        Ok(Self { pool, config })
    }

    /// Create a LocalStorage from an existing pool (useful for testing).
    pub fn from_pool(pool: SqlitePool, config: LocalConfig) -> Self {
        Self { pool, config }
    }

    /// Get a reference to the underlying connection pool.
    pub fn pool(&self) -> &SqlitePool {
        &self.pool
    }
}

// region: --- Row Extraction Helpers

/// Extract a `PresetRow` from a SQLite row.
///
/// JSON columns are stored as TEXT in SQLite and parsed back to `serde_json::Value`.
fn extract_preset_row(row: &sqlx::sqlite::SqliteRow) -> PresetRow {
    let category_str: String = row.get("category");
    let tags_str: String = row.get("tags");
    let data_str: String = row.get("data");
    let id_str: String = row.get("id");
    let author_str: Option<String> = row.get("author_id");

    PresetRow {
        id: id_str.parse().unwrap_or_default(),
        name: row.get("name"),
        description: row.get("description"),
        author_id: author_str.and_then(|s| s.parse().ok()),
        category: serde_json::from_str(&category_str).unwrap_or_default(),
        tags: serde_json::from_str(&tags_str).unwrap_or_default(),
        rating: row.get("rating"),
        data: serde_json::from_str(&data_str).unwrap_or_default(),
        is_public: row.get("is_public"),
        is_deleted: row.get("is_deleted"),
        version: row.get("version"),
        created_at: row.get("created_at"),
        updated_at: row.get("updated_at"),
    }
}

/// Extract a `PresetSummary` from a SQLite row.
fn extract_preset_summary(row: &sqlx::sqlite::SqliteRow) -> PresetSummary {
    let category_str: String = row.get("category");
    let tags_str: String = row.get("tags");
    let id_str: String = row.get("id");

    PresetSummary {
        id: id_str.parse().unwrap_or_default(),
        name: row.get("name"),
        description: row.get("description"),
        category: serde_json::from_str(&category_str).unwrap_or_default(),
        tags: serde_json::from_str(&tags_str).unwrap_or_default(),
        rating: row.get("rating"),
        is_public: row.get("is_public"),
        version: row.get("version"),
        created_at: row.get("created_at"),
        updated_at: row.get("updated_at"),
    }
}

/// Extract a `SnapshotRow` from a SQLite row.
fn extract_snapshot_row(row: &sqlx::sqlite::SqliteRow) -> SnapshotRow {
    let data_str: String = row.get("data");
    let id_str: String = row.get("id");
    let preset_id_str: String = row.get("preset_id");

    SnapshotRow {
        id: id_str.parse().unwrap_or_default(),
        preset_id: preset_id_str.parse().unwrap_or_default(),
        name: row.get("name"),
        data: serde_json::from_str(&data_str).unwrap_or_default(),
        created_at: row.get("created_at"),
        updated_at: row.get("updated_at"),
    }
}

// endregion: --- Row Extraction Helpers

#[async_trait]
impl StorageService for LocalStorage {
    // ── Presets ───────────────────────────────────────────────────────────

    async fn save_preset(&self, preset: &PresetRow) -> StorageResult<Uuid> {
        let now = Utc::now();
        let (sql, values) = Query::insert()
            .into_table(Presets::Table)
            .columns([
                Presets::Id,
                Presets::Name,
                Presets::Description,
                Presets::AuthorId,
                Presets::Category,
                Presets::Tags,
                Presets::Rating,
                Presets::Data,
                Presets::IsPublic,
                Presets::IsDeleted,
                Presets::Version,
                Presets::CreatedAt,
                Presets::UpdatedAt,
            ])
            .values_panic([
                preset.id.to_string().into(),
                preset.name.clone().into(),
                preset.description.clone().into(),
                preset.author_id.map(|u| u.to_string()).into(),
                serde_json::to_string(&preset.category)
                    .unwrap_or_default()
                    .into(),
                serde_json::to_string(&preset.tags)
                    .unwrap_or_default()
                    .into(),
                (preset.rating as i32).into(),
                serde_json::to_string(&preset.data)
                    .unwrap_or_default()
                    .into(),
                preset.is_public.into(),
                preset.is_deleted.into(),
                preset.version.into(),
                now.into(),
                now.into(),
            ])
            .on_conflict(
                sea_query::OnConflict::column(Presets::Id)
                    .update_columns([
                        Presets::Name,
                        Presets::Description,
                        Presets::AuthorId,
                        Presets::Category,
                        Presets::Tags,
                        Presets::Rating,
                        Presets::Data,
                        Presets::IsPublic,
                        Presets::IsDeleted,
                        Presets::Version,
                        Presets::UpdatedAt,
                    ])
                    .to_owned(),
            )
            .build_sqlx(SqliteQueryBuilder);

        sqlx::query_with(&sql, values)
            .execute(&self.pool)
            .await?;

        Ok(preset.id)
    }

    async fn get_preset(&self, id: Uuid) -> StorageResult<Option<PresetRow>> {
        let (sql, values) = Query::select()
            .columns([
                Presets::Id,
                Presets::Name,
                Presets::Description,
                Presets::AuthorId,
                Presets::Category,
                Presets::Tags,
                Presets::Rating,
                Presets::Data,
                Presets::IsPublic,
                Presets::IsDeleted,
                Presets::Version,
                Presets::CreatedAt,
                Presets::UpdatedAt,
            ])
            .from(Presets::Table)
            .and_where(Expr::col(Presets::Id).eq(id.to_string()))
            .build_sqlx(SqliteQueryBuilder);

        let row = sqlx::query_with(&sql, values)
            .fetch_optional(&self.pool)
            .await?;

        Ok(row.as_ref().map(extract_preset_row))
    }

    async fn list_presets(&self, filter: &PresetFilter) -> StorageResult<Vec<PresetSummary>> {
        // When there's a search term, use FTS5 for fast full-text matching.
        // FTS5 MATCH is much faster than LIKE '%query%' on large datasets.
        if let Some(ref search) = filter.search {
            return self.list_presets_fts(search, filter).await;
        }

        let mut query = Query::select();
        query
            .columns([
                Presets::Id,
                Presets::Name,
                Presets::Description,
                Presets::Category,
                Presets::Tags,
                Presets::Rating,
                Presets::IsPublic,
                Presets::Version,
                Presets::CreatedAt,
                Presets::UpdatedAt,
            ])
            .from(Presets::Table);

        if filter.exclude_deleted {
            query.and_where(Expr::col(Presets::IsDeleted).eq(false));
        }

        if let Some(is_public) = filter.is_public {
            query.and_where(Expr::col(Presets::IsPublic).eq(is_public));
        }

        query.order_by(Presets::UpdatedAt, sea_query::Order::Desc);

        if let Some(limit) = filter.limit {
            query.limit(limit);
        }
        if let Some(offset) = filter.offset {
            query.offset(offset);
        }

        let (sql, values) = query.build_sqlx(SqliteQueryBuilder);

        let rows = sqlx::query_with(&sql, values)
            .fetch_all(&self.pool)
            .await?;

        Ok(rows.iter().map(extract_preset_summary).collect())
    }

    async fn delete_preset(&self, id: Uuid) -> StorageResult<()> {
        let (sql, values) = Query::update()
            .table(Presets::Table)
            .values([(Presets::IsDeleted, true.into())])
            .and_where(Expr::col(Presets::Id).eq(id.to_string()))
            .build_sqlx(SqliteQueryBuilder);

        let result = sqlx::query_with(&sql, values)
            .execute(&self.pool)
            .await?;

        if result.rows_affected() == 0 {
            return Err(StorageError::NotFound {
                entity: "preset",
                id,
            });
        }

        Ok(())
    }

    // ── Snapshots ────────────────────────────────────────────────────────

    async fn save_snapshot(&self, snapshot: &SnapshotRow) -> StorageResult<Uuid> {
        let now = Utc::now();
        let (sql, values) = Query::insert()
            .into_table(Snapshots::Table)
            .columns([
                Snapshots::Id,
                Snapshots::PresetId,
                Snapshots::Name,
                Snapshots::Data,
                Snapshots::CreatedAt,
                Snapshots::UpdatedAt,
            ])
            .values_panic([
                snapshot.id.to_string().into(),
                snapshot.preset_id.to_string().into(),
                snapshot.name.clone().into(),
                serde_json::to_string(&snapshot.data)
                    .unwrap_or_default()
                    .into(),
                now.into(),
                now.into(),
            ])
            .on_conflict(
                sea_query::OnConflict::column(Snapshots::Id)
                    .update_columns([
                        Snapshots::Name,
                        Snapshots::Data,
                        Snapshots::UpdatedAt,
                    ])
                    .to_owned(),
            )
            .build_sqlx(SqliteQueryBuilder);

        sqlx::query_with(&sql, values)
            .execute(&self.pool)
            .await?;

        Ok(snapshot.id)
    }

    async fn get_snapshot(&self, id: Uuid) -> StorageResult<Option<SnapshotRow>> {
        let (sql, values) = Query::select()
            .columns([
                Snapshots::Id,
                Snapshots::PresetId,
                Snapshots::Name,
                Snapshots::Data,
                Snapshots::CreatedAt,
                Snapshots::UpdatedAt,
            ])
            .from(Snapshots::Table)
            .and_where(Expr::col(Snapshots::Id).eq(id.to_string()))
            .build_sqlx(SqliteQueryBuilder);

        let row = sqlx::query_with(&sql, values)
            .fetch_optional(&self.pool)
            .await?;

        Ok(row.as_ref().map(extract_snapshot_row))
    }

    async fn list_snapshots(&self, preset_id: Uuid) -> StorageResult<Vec<SnapshotRow>> {
        let (sql, values) = Query::select()
            .columns([
                Snapshots::Id,
                Snapshots::PresetId,
                Snapshots::Name,
                Snapshots::Data,
                Snapshots::CreatedAt,
                Snapshots::UpdatedAt,
            ])
            .from(Snapshots::Table)
            .and_where(Expr::col(Snapshots::PresetId).eq(preset_id.to_string()))
            .order_by(Snapshots::CreatedAt, sea_query::Order::Asc)
            .build_sqlx(SqliteQueryBuilder);

        let rows = sqlx::query_with(&sql, values)
            .fetch_all(&self.pool)
            .await?;

        Ok(rows.iter().map(extract_snapshot_row).collect())
    }

    // ── Lifecycle ────────────────────────────────────────────────────────

    async fn run_migrations(&self) -> StorageResult<()> {
        // Create core tables using sea-query (backend-agnostic schema definitions)
        let tables = [
            schema::create_presets_table().to_string(SqliteQueryBuilder),
            schema::create_snapshots_table().to_string(SqliteQueryBuilder),
            schema::create_rig_configs_table().to_string(SqliteQueryBuilder),
            schema::create_module_chunks_table().to_string(SqliteQueryBuilder),
            schema::create_sync_metadata_table().to_string(SqliteQueryBuilder),
        ];

        for sql in &tables {
            sqlx::query(sql).execute(&self.pool).await?;
        }

        // Create indexes (ignore "already exists" errors)
        let all_indexes = [
            schema::create_presets_indexes(),
            schema::create_snapshots_indexes(),
            schema::create_sync_metadata_indexes(),
        ];

        for indexes in &all_indexes {
            for idx in indexes {
                let sql = idx.to_string(SqliteQueryBuilder);
                match sqlx::query(&sql).execute(&self.pool).await {
                    Ok(_) => {}
                    Err(e) => {
                        let msg = e.to_string();
                        if !msg.contains("already exists") {
                            return Err(StorageError::Database(e));
                        }
                    }
                }
            }
        }

        // Create FTS5 virtual table for full-text search on presets
        sqlx::query(FTS5_CREATE).execute(&self.pool).await?;

        // Create content-sync triggers
        for trigger_sql in FTS5_TRIGGERS {
            sqlx::query(trigger_sql).execute(&self.pool).await?;
        }

        tracing::info!("local storage migrations complete");
        Ok(())
    }

    async fn health_check(&self) -> StorageResult<()> {
        sqlx::query("SELECT 1")
            .execute(&self.pool)
            .await
            .map_err(|e| {
                StorageError::ConnectionUnavailable(format!("health check failed: {e}"))
            })?;
        Ok(())
    }
}

// region: --- FTS5 Search

impl LocalStorage {
    /// List presets using FTS5 full-text search.
    ///
    /// Joins the FTS5 virtual table with the presets table to combine
    /// full-text ranking with the standard filter predicates.
    async fn list_presets_fts(
        &self,
        search: &str,
        filter: &PresetFilter,
    ) -> StorageResult<Vec<PresetSummary>> {
        // Build the FTS query with parameterized search term.
        // FTS5 MATCH syntax supports implicit AND: "blues guitar" matches both words.
        let mut sql = String::from(
            r#"SELECT p."id", p."name", p."description", p."category", p."tags",
                      p."rating", p."is_public", p."version", p."created_at", p."updated_at"
               FROM "presets" p
               INNER JOIN "presets_fts" fts ON p.rowid = fts.rowid
               WHERE presets_fts MATCH ?1"#,
        );

        let mut param_idx = 2u32;

        if filter.exclude_deleted {
            sql.push_str(&format!(" AND p.\"is_deleted\" = ?{param_idx}"));
            param_idx += 1;
        }

        if filter.is_public.is_some() {
            sql.push_str(&format!(" AND p.\"is_public\" = ?{param_idx}"));
            param_idx += 1;
        }

        sql.push_str(" ORDER BY rank");

        if let Some(limit) = filter.limit {
            sql.push_str(&format!(" LIMIT {limit}"));
        }
        if let Some(offset) = filter.offset {
            sql.push_str(&format!(" OFFSET {offset}"));
        }

        let mut query = sqlx::query(&sql).bind(search);

        // Bind filter parameters in the same order they appear in the SQL
        let _ = param_idx; // suppress unused warning — param_idx tracks position
        if filter.exclude_deleted {
            query = query.bind(false);
        }
        if let Some(is_public) = filter.is_public {
            query = query.bind(is_public);
        }

        let rows = query.fetch_all(&self.pool).await?;

        Ok(rows.iter().map(extract_preset_summary).collect())
    }
}

// endregion: --- FTS5 Search

impl std::fmt::Debug for LocalStorage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LocalStorage")
            .field("database_path", &self.config.database_path)
            .finish_non_exhaustive()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::service::PresetFilter;

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    /// Create an in-memory SQLite LocalStorage for testing.
    async fn test_storage() -> LocalStorage {
        let config = LocalConfig::new(":memory:");
        // For in-memory databases, we connect differently
        let options = SqliteConnectOptions::new()
            .filename(":memory:")
            .create_if_missing(true)
            .journal_mode(sqlx::sqlite::SqliteJournalMode::Wal);

        let pool = SqlitePoolOptions::new()
            .max_connections(1)
            .connect_with(options)
            .await
            .expect("failed to create in-memory SQLite pool");

        sqlx::query("PRAGMA foreign_keys = ON")
            .execute(&pool)
            .await
            .expect("failed to enable foreign keys");

        let storage = LocalStorage::from_pool(pool, config);
        storage
            .run_migrations()
            .await
            .expect("migrations failed");
        storage
    }

    fn sample_preset(id: Uuid) -> PresetRow {
        PresetRow {
            id,
            name: "Blues Lead".to_string(),
            description: Some("Warm bluesy lead tone".to_string()),
            author_id: Some(Uuid::new_v4()),
            category: serde_json::json!({"base_tone": "Lead", "genre": "Blues"}),
            tags: serde_json::json!(["warm", "blues", "lead"]),
            rating: 4,
            data: serde_json::json!({"blocks": [], "snapshots": []}),
            is_public: false,
            is_deleted: false,
            version: 1,
            created_at: Utc::now(),
            updated_at: Utc::now(),
        }
    }

    // -- Lifecycle Tests

    #[tokio::test]
    async fn migrations_run_idempotently() -> Result<()> {
        // -- Setup & Fixtures
        let storage = test_storage().await;

        // -- Exec: run migrations a second time
        storage.run_migrations().await?;

        // -- Check
        storage.health_check().await?;
        Ok(())
    }

    #[tokio::test]
    async fn health_check_succeeds() -> Result<()> {
        let storage = test_storage().await;
        storage.health_check().await?;
        Ok(())
    }

    // -- Preset CRUD Tests

    #[tokio::test]
    async fn save_and_get_preset() -> Result<()> {
        // -- Setup & Fixtures
        let storage = test_storage().await;
        let id = Uuid::new_v4();
        let preset = sample_preset(id);

        // -- Exec
        let returned_id = storage.save_preset(&preset).await?;

        // -- Check
        assert_eq!(returned_id, id);
        let fetched = storage.get_preset(id).await?.expect("preset should exist");
        assert_eq!(fetched.name, "Blues Lead");
        assert_eq!(fetched.rating, 4);
        assert_eq!(fetched.is_public, false);
        Ok(())
    }

    #[tokio::test]
    async fn save_preset_upsert_updates_existing() -> Result<()> {
        // -- Setup & Fixtures
        let storage = test_storage().await;
        let id = Uuid::new_v4();
        let mut preset = sample_preset(id);
        storage.save_preset(&preset).await?;

        // -- Exec: update the name
        preset.name = "Updated Blues Lead".to_string();
        preset.version = 2;
        storage.save_preset(&preset).await?;

        // -- Check
        let fetched = storage.get_preset(id).await?.expect("preset should exist");
        assert_eq!(fetched.name, "Updated Blues Lead");
        assert_eq!(fetched.version, 2);
        Ok(())
    }

    #[tokio::test]
    async fn get_nonexistent_preset_returns_none() -> Result<()> {
        let storage = test_storage().await;
        let result = storage.get_preset(Uuid::new_v4()).await?;
        assert!(result.is_none());
        Ok(())
    }

    #[tokio::test]
    async fn delete_preset_soft_deletes() -> Result<()> {
        // -- Setup & Fixtures
        let storage = test_storage().await;
        let id = Uuid::new_v4();
        storage.save_preset(&sample_preset(id)).await?;

        // -- Exec
        storage.delete_preset(id).await?;

        // -- Check: preset still exists but is_deleted = true
        let fetched = storage.get_preset(id).await?.expect("preset row should still exist");
        assert!(fetched.is_deleted);
        Ok(())
    }

    #[tokio::test]
    async fn delete_nonexistent_preset_returns_not_found() {
        let storage = test_storage().await;
        let result = storage.delete_preset(Uuid::new_v4()).await;
        assert!(matches!(result, Err(StorageError::NotFound { .. })));
    }

    // -- List Tests

    #[tokio::test]
    async fn list_presets_excludes_deleted_by_default() -> Result<()> {
        // -- Setup & Fixtures
        let storage = test_storage().await;
        let id1 = Uuid::new_v4();
        let id2 = Uuid::new_v4();
        storage.save_preset(&sample_preset(id1)).await?;
        let mut deleted = sample_preset(id2);
        deleted.name = "Deleted Preset".to_string();
        deleted.is_deleted = true;
        storage.save_preset(&deleted).await?;

        // -- Exec
        let filter = PresetFilter::new();
        let results = storage.list_presets(&filter).await?;

        // -- Check
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].id, id1);
        Ok(())
    }

    #[tokio::test]
    async fn list_presets_with_limit_and_offset() -> Result<()> {
        // -- Setup & Fixtures
        let storage = test_storage().await;
        for _ in 0..5 {
            storage.save_preset(&sample_preset(Uuid::new_v4())).await?;
        }

        // -- Exec
        let filter = PresetFilter {
            limit: Some(2),
            offset: Some(1),
            exclude_deleted: true,
            ..Default::default()
        };
        let results = storage.list_presets(&filter).await?;

        // -- Check
        assert_eq!(results.len(), 2);
        Ok(())
    }

    // -- Snapshot Tests

    #[tokio::test]
    async fn save_and_get_snapshot() -> Result<()> {
        // -- Setup & Fixtures
        let storage = test_storage().await;
        let preset_id = Uuid::new_v4();
        storage.save_preset(&sample_preset(preset_id)).await?;

        let snap_id = Uuid::new_v4();
        let snapshot = SnapshotRow {
            id: snap_id,
            preset_id,
            name: "Clean Variation".to_string(),
            data: serde_json::json!({"volume": 0.8}),
            created_at: Utc::now(),
            updated_at: Utc::now(),
        };

        // -- Exec
        storage.save_snapshot(&snapshot).await?;

        // -- Check
        let fetched = storage.get_snapshot(snap_id).await?.expect("snapshot should exist");
        assert_eq!(fetched.name, "Clean Variation");
        assert_eq!(fetched.preset_id, preset_id);
        Ok(())
    }

    #[tokio::test]
    async fn list_snapshots_for_preset() -> Result<()> {
        // -- Setup & Fixtures
        let storage = test_storage().await;
        let preset_id = Uuid::new_v4();
        storage.save_preset(&sample_preset(preset_id)).await?;

        for i in 0..3 {
            let snap = SnapshotRow {
                id: Uuid::new_v4(),
                preset_id,
                name: format!("Snap {i}"),
                data: serde_json::json!({}),
                created_at: Utc::now(),
                updated_at: Utc::now(),
            };
            storage.save_snapshot(&snap).await?;
        }

        // -- Exec
        let snapshots = storage.list_snapshots(preset_id).await?;

        // -- Check
        assert_eq!(snapshots.len(), 3);
        Ok(())
    }

    // -- FTS5 Search Tests

    #[tokio::test]
    async fn fts_search_finds_by_name() -> Result<()> {
        // -- Setup & Fixtures
        let storage = test_storage().await;
        let id = Uuid::new_v4();
        let mut preset = sample_preset(id);
        preset.name = "Crunchy Rock Lead".to_string();
        storage.save_preset(&preset).await?;

        let id2 = Uuid::new_v4();
        let mut preset2 = sample_preset(id2);
        preset2.name = "Clean Jazz".to_string();
        preset2.description = Some("Smooth jazz tone".to_string());
        storage.save_preset(&preset2).await?;

        // -- Exec: search for "rock"
        let filter = PresetFilter {
            search: Some("rock".to_string()),
            exclude_deleted: true,
            ..Default::default()
        };
        let results = storage.list_presets(&filter).await?;

        // -- Check
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].name, "Crunchy Rock Lead");
        Ok(())
    }

    #[tokio::test]
    async fn fts_search_finds_by_description() -> Result<()> {
        // -- Setup & Fixtures
        let storage = test_storage().await;
        let id = Uuid::new_v4();
        let mut preset = sample_preset(id);
        preset.name = "Tone A".to_string();
        preset.description = Some("A heavy metal crunch distortion".to_string());
        storage.save_preset(&preset).await?;

        // -- Exec
        let filter = PresetFilter {
            search: Some("distortion".to_string()),
            exclude_deleted: true,
            ..Default::default()
        };
        let results = storage.list_presets(&filter).await?;

        // -- Check
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].id, id);
        Ok(())
    }

    #[tokio::test]
    async fn fts_search_respects_deleted_filter() -> Result<()> {
        // -- Setup & Fixtures
        let storage = test_storage().await;
        let id = Uuid::new_v4();
        let mut preset = sample_preset(id);
        preset.name = "Vintage Overdrive".to_string();
        preset.is_deleted = true;
        storage.save_preset(&preset).await?;

        // -- Exec
        let filter = PresetFilter {
            search: Some("vintage".to_string()),
            exclude_deleted: true,
            ..Default::default()
        };
        let results = storage.list_presets(&filter).await?;

        // -- Check: deleted preset should not appear
        assert_eq!(results.len(), 0);
        Ok(())
    }
}
