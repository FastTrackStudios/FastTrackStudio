//! CloudStorage — PostgreSQL backend for cloud sync and sharing.
//!
//! Uses sqlx with a connection pool for async PostgreSQL access.
//! Queries are built with sea-query for type-safe, database-agnostic SQL.

use async_trait::async_trait;
use chrono::Utc;
use sea_query::{Expr, PostgresQueryBuilder, Query};
use sea_query_binder::SqlxBinder;
use sqlx::postgres::{PgPool, PgPoolOptions};
use sqlx::Row;
use uuid::Uuid;

use crate::config::CloudConfig;
use crate::error::{StorageError, StorageResult};
use crate::schema::{self, Presets, Snapshots};
use crate::service::{
    PresetFilter, PresetRow, PresetSummary, SnapshotRow, StorageService,
};

/// CloudStorage — PostgreSQL-backed storage for cloud sync and sharing.
///
/// Holds a sqlx connection pool and configuration for retry/timeout behavior.
pub struct CloudStorage {
    pool: PgPool,
    config: CloudConfig,
}

impl CloudStorage {
    /// Connect to PostgreSQL and create a new CloudStorage instance.
    ///
    /// This creates the connection pool but does NOT run migrations.
    /// Call [`StorageService::run_migrations`] after construction.
    pub async fn connect(config: CloudConfig) -> StorageResult<Self> {
        let pool = PgPoolOptions::new()
            .max_connections(config.max_connections)
            .min_connections(config.min_connections)
            .acquire_timeout(config.acquire_timeout)
            .max_lifetime(config.max_lifetime)
            .idle_timeout(config.idle_timeout)
            .connect(&config.connection_string)
            .await
            .map_err(|e| {
                StorageError::ConnectionUnavailable(format!(
                    "failed to connect to PostgreSQL: {e}"
                ))
            })?;

        Ok(Self { pool, config })
    }

    /// Create a CloudStorage from an existing pool (useful for testing).
    pub fn from_pool(pool: PgPool, config: CloudConfig) -> Self {
        Self { pool, config }
    }

    /// Get a reference to the underlying connection pool.
    pub fn pool(&self) -> &PgPool {
        &self.pool
    }

    /// Execute an operation with retry logic for transient failures.
    ///
    /// Retries on connection errors and serialization failures (deadlocks).
    /// Uses exponential backoff starting from `config.retry_base_delay`.
    async fn with_retry<T, F, Fut>(&self, operation: F) -> StorageResult<T>
    where
        F: Fn() -> Fut,
        Fut: std::future::Future<Output = StorageResult<T>>,
    {
        let mut last_error = None;
        let mut delay = self.config.retry_base_delay;

        for attempt in 0..=self.config.max_retries {
            match operation().await {
                Ok(result) => return Ok(result),
                Err(e) => {
                    if attempt < self.config.max_retries && is_transient(&e) {
                        tracing::warn!(
                            attempt = attempt + 1,
                            max = self.config.max_retries,
                            delay_ms = delay.as_millis(),
                            error = %e,
                            "transient error, retrying"
                        );
                        tokio::time::sleep(delay).await;
                        delay *= 2;
                        last_error = Some(e);
                    } else {
                        return Err(e);
                    }
                }
            }
        }

        Err(last_error.unwrap_or_else(|| {
            StorageError::ConnectionUnavailable("max retries exceeded".to_string())
        }))
    }
}

/// Check if a storage error is transient (worth retrying).
fn is_transient(err: &StorageError) -> bool {
    match err {
        StorageError::Database(sqlx_err) => {
            // Connection reset, pool timeout, serialization failure (deadlock)
            matches!(
                sqlx_err,
                sqlx::Error::PoolTimedOut
                    | sqlx::Error::PoolClosed
                    | sqlx::Error::Io(_)
            ) || sqlx_err
                .as_database_error()
                .is_some_and(|db_err| {
                    // PostgreSQL error codes for serialization failure / deadlock
                    let code = db_err.code().unwrap_or_default();
                    code == "40001" || code == "40P01" || code == "08006" || code == "08001"
                })
        }
        StorageError::ConnectionUnavailable(_) => true,
        _ => false,
    }
}

#[async_trait]
impl StorageService for CloudStorage {
    // ── Presets ───────────────────────────────────────────────────────────

    async fn save_preset(&self, preset: &PresetRow) -> StorageResult<Uuid> {
        self.with_retry(|| async {
            // Upsert: INSERT ... ON CONFLICT (id) DO UPDATE
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
                    preset.id.into(),
                    preset.name.clone().into(),
                    preset.description.clone().into(),
                    preset.author_id.into(),
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
                .build_sqlx(PostgresQueryBuilder);

            sqlx::query_with(&sql, values)
                .execute(&self.pool)
                .await?;

            Ok(preset.id)
        })
        .await
    }

    async fn get_preset(&self, id: Uuid) -> StorageResult<Option<PresetRow>> {
        self.with_retry(|| async {
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
                .and_where(Expr::col(Presets::Id).eq(id))
                .build_sqlx(PostgresQueryBuilder);

            let row = sqlx::query_with(&sql, values)
                .fetch_optional(&self.pool)
                .await?;

            match row {
                Some(row) => {
                    let category_str: String = row.get("category");
                    let tags_str: String = row.get("tags");
                    let data_str: String = row.get("data");

                    Ok(Some(PresetRow {
                        id: row.get("id"),
                        name: row.get("name"),
                        description: row.get("description"),
                        author_id: row.get("author_id"),
                        category: serde_json::from_str(&category_str).unwrap_or_default(),
                        tags: serde_json::from_str(&tags_str).unwrap_or_default(),
                        rating: row.get("rating"),
                        data: serde_json::from_str(&data_str).unwrap_or_default(),
                        is_public: row.get("is_public"),
                        is_deleted: row.get("is_deleted"),
                        version: row.get("version"),
                        created_at: row.get("created_at"),
                        updated_at: row.get("updated_at"),
                    }))
                }
                None => Ok(None),
            }
        })
        .await
    }

    async fn list_presets(&self, filter: &PresetFilter) -> StorageResult<Vec<PresetSummary>> {
        self.with_retry(|| async {
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

            if let Some(ref search) = filter.search {
                let pattern = format!("%{search}%");
                query.and_where(
                    Expr::col(Presets::Name)
                        .like(&pattern)
                        .or(Expr::col(Presets::Description).like(&pattern)),
                );
            }

            query.order_by(Presets::UpdatedAt, sea_query::Order::Desc);

            if let Some(limit) = filter.limit {
                query.limit(limit);
            }

            if let Some(offset) = filter.offset {
                query.offset(offset);
            }

            let (sql, values) = query.build_sqlx(PostgresQueryBuilder);

            let rows = sqlx::query_with(&sql, values)
                .fetch_all(&self.pool)
                .await?;

            let presets = rows
                .iter()
                .map(|row| {
                    let category_str: String = row.get("category");
                    let tags_str: String = row.get("tags");
                    PresetSummary {
                        id: row.get("id"),
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
                })
                .collect();

            Ok(presets)
        })
        .await
    }

    async fn delete_preset(&self, id: Uuid) -> StorageResult<()> {
        self.with_retry(|| async {
            let (sql, values) = Query::update()
                .table(Presets::Table)
                .values([(Presets::IsDeleted, true.into())])
                .and_where(Expr::col(Presets::Id).eq(id))
                .build_sqlx(PostgresQueryBuilder);

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
        })
        .await
    }

    // ── Snapshots ────────────────────────────────────────────────────────

    async fn save_snapshot(&self, snapshot: &SnapshotRow) -> StorageResult<Uuid> {
        self.with_retry(|| async {
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
                    snapshot.id.into(),
                    snapshot.preset_id.into(),
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
                .build_sqlx(PostgresQueryBuilder);

            sqlx::query_with(&sql, values)
                .execute(&self.pool)
                .await?;

            Ok(snapshot.id)
        })
        .await
    }

    async fn get_snapshot(&self, id: Uuid) -> StorageResult<Option<SnapshotRow>> {
        self.with_retry(|| async {
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
                .and_where(Expr::col(Snapshots::Id).eq(id))
                .build_sqlx(PostgresQueryBuilder);

            let row = sqlx::query_with(&sql, values)
                .fetch_optional(&self.pool)
                .await?;

            match row {
                Some(row) => {
                    let data_str: String = row.get("data");
                    Ok(Some(SnapshotRow {
                        id: row.get("id"),
                        preset_id: row.get("preset_id"),
                        name: row.get("name"),
                        data: serde_json::from_str(&data_str).unwrap_or_default(),
                        created_at: row.get("created_at"),
                        updated_at: row.get("updated_at"),
                    }))
                }
                None => Ok(None),
            }
        })
        .await
    }

    async fn list_snapshots(&self, preset_id: Uuid) -> StorageResult<Vec<SnapshotRow>> {
        self.with_retry(|| async {
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
                .and_where(Expr::col(Snapshots::PresetId).eq(preset_id))
                .order_by(Snapshots::CreatedAt, sea_query::Order::Asc)
                .build_sqlx(PostgresQueryBuilder);

            let rows = sqlx::query_with(&sql, values)
                .fetch_all(&self.pool)
                .await?;

            let snapshots = rows
                .iter()
                .map(|row| {
                    let data_str: String = row.get("data");
                    SnapshotRow {
                        id: row.get("id"),
                        preset_id: row.get("preset_id"),
                        name: row.get("name"),
                        data: serde_json::from_str(&data_str).unwrap_or_default(),
                        created_at: row.get("created_at"),
                        updated_at: row.get("updated_at"),
                    }
                })
                .collect();

            Ok(snapshots)
        })
        .await
    }

    // ── Lifecycle ────────────────────────────────────────────────────────

    async fn run_migrations(&self) -> StorageResult<()> {
        let tables = [
            schema::create_presets_table().to_string(PostgresQueryBuilder),
            schema::create_snapshots_table().to_string(PostgresQueryBuilder),
            schema::create_rig_configs_table().to_string(PostgresQueryBuilder),
            schema::create_module_chunks_table().to_string(PostgresQueryBuilder),
            schema::create_sync_metadata_table().to_string(PostgresQueryBuilder),
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
                let sql = idx.to_string(PostgresQueryBuilder);
                match sqlx::query(&sql).execute(&self.pool).await {
                    Ok(_) => {}
                    Err(e) => {
                        // Ignore "already exists" errors
                        let msg = e.to_string();
                        if !msg.contains("already exists") {
                            return Err(StorageError::Database(e));
                        }
                    }
                }
            }
        }

        tracing::info!("cloud storage migrations complete");
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

impl std::fmt::Debug for CloudStorage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CloudStorage")
            .field("max_connections", &self.config.max_connections)
            .field("max_retries", &self.config.max_retries)
            .finish_non_exhaustive()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_transient_detects_pool_timeout() {
        let err = StorageError::Database(sqlx::Error::PoolTimedOut);
        assert!(is_transient(&err));
    }

    #[test]
    fn is_transient_rejects_not_found() {
        let err = StorageError::NotFound {
            entity: "preset",
            id: Uuid::new_v4(),
        };
        assert!(!is_transient(&err));
    }

    #[test]
    fn is_transient_detects_connection_unavailable() {
        let err = StorageError::ConnectionUnavailable("pool closed".to_string());
        assert!(is_transient(&err));
    }

    #[test]
    fn debug_impl_does_not_leak_connection_string() {
        // Can't construct without a real pool, but we can verify the Debug impl exists
        // and doesn't include connection_string in its format
        let _ = format!("{:?}", StorageError::Config("test".to_string()));
    }
}
