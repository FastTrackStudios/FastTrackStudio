//! StorageService trait — the polymorphism boundary for storage backends.

use async_trait::async_trait;
use chrono::{DateTime, Utc};
use uuid::Uuid;

use crate::error::StorageResult;

/// Summary of a preset for list views (avoids loading full JSON data).
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct PresetSummary {
    pub id: Uuid,
    pub name: String,
    pub description: Option<String>,
    pub category: serde_json::Value,
    pub tags: serde_json::Value,
    pub rating: i16,
    pub is_public: bool,
    pub version: i64,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}

/// Full preset row including the JSON data blob.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct PresetRow {
    pub id: Uuid,
    pub name: String,
    pub description: Option<String>,
    pub author_id: Option<Uuid>,
    pub category: serde_json::Value,
    pub tags: serde_json::Value,
    pub rating: i16,
    pub data: serde_json::Value,
    pub is_public: bool,
    pub is_deleted: bool,
    pub version: i64,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}

/// Filter criteria for listing presets.
#[derive(Debug, Clone, Default)]
pub struct PresetFilter {
    /// Text search on name and description.
    pub search: Option<String>,
    /// Only public presets.
    pub is_public: Option<bool>,
    /// Only non-deleted presets (default: true).
    pub exclude_deleted: bool,
    /// Maximum number of results.
    pub limit: Option<u64>,
    /// Offset for pagination.
    pub offset: Option<u64>,
}

impl PresetFilter {
    pub fn new() -> Self {
        Self {
            exclude_deleted: true,
            ..Default::default()
        }
    }
}

/// Snapshot row from the database.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct SnapshotRow {
    pub id: Uuid,
    pub preset_id: Uuid,
    pub name: String,
    pub data: serde_json::Value,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}

/// Abstract storage backend trait.
///
/// Implemented by both `LocalStorage` (SQLite) and `CloudStorage` (PostgreSQL).
/// All methods use raw UUIDs and JSON values to decouple storage from domain types.
#[async_trait]
pub trait StorageService: Send + Sync {
    // ── Presets ───────────────────────────────────────────────────────────

    /// Save a preset (insert or update).
    async fn save_preset(&self, preset: &PresetRow) -> StorageResult<Uuid>;

    /// Get a single preset by ID.
    async fn get_preset(&self, id: Uuid) -> StorageResult<Option<PresetRow>>;

    /// List presets with optional filtering.
    async fn list_presets(&self, filter: &PresetFilter) -> StorageResult<Vec<PresetSummary>>;

    /// Soft-delete a preset.
    async fn delete_preset(&self, id: Uuid) -> StorageResult<()>;

    // ── Snapshots ────────────────────────────────────────────────────────

    /// Save a snapshot (insert or update).
    async fn save_snapshot(&self, snapshot: &SnapshotRow) -> StorageResult<Uuid>;

    /// Get a snapshot by ID.
    async fn get_snapshot(&self, id: Uuid) -> StorageResult<Option<SnapshotRow>>;

    /// List snapshots for a preset.
    async fn list_snapshots(&self, preset_id: Uuid) -> StorageResult<Vec<SnapshotRow>>;

    // ── Lifecycle ────────────────────────────────────────────────────────

    /// Run database migrations to ensure schema is up to date.
    async fn run_migrations(&self) -> StorageResult<()>;

    /// Check if the storage backend is healthy and reachable.
    async fn health_check(&self) -> StorageResult<()>;
}
