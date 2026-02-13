//! Sync engine — manages local-to-cloud synchronization with conflict resolution.
//!
//! Provides:
//! - [`SyncStatus`] — per-entity sync state (Synced, Pending, Conflict, LocalOnly)
//! - [`ConflictStrategy`] — resolution policies (LastWriteWins, Merge, AskUser, KeepBoth)
//! - [`SyncMetadata`] — tracking record for an entity's sync state
//! - [`SyncEngine`] — orchestrator for delta sync, conflict detection, and resolution
//!
//! The engine operates on the `sync_metadata` table (created by migration 000006)
//! via the SeaORM entity in [`crate::entities::sync_metadata`].

use chrono::{DateTime, Utc};
use sea_orm::{
    ActiveModelTrait, ColumnTrait, DatabaseConnection, EntityTrait, Order, QueryFilter, QueryOrder,
    Set,
};
use uuid::Uuid;

use crate::entities::sync_metadata::{self, Entity as SyncMetadataEntity};
use crate::error::{StorageError, StorageResult};

// ─────────────────────────────────────────────────────────────────────────────
// SyncStatus
// ─────────────────────────────────────────────────────────────────────────────

/// Synchronization status for a tracked entity.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SyncStatus {
    /// Entity is in sync between local and remote.
    Synced,
    /// Entity has local changes not yet pushed to remote.
    Pending,
    /// Entity was modified on both local and remote — needs resolution.
    Conflict,
    /// Entity exists only locally (never synced).
    LocalOnly,
}

impl SyncStatus {
    /// Convert to the string representation stored in the database.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Synced => "synced",
            Self::Pending => "pending",
            Self::Conflict => "conflict",
            Self::LocalOnly => "local_only",
        }
    }

    /// Parse from the string representation stored in the database.
    pub fn from_str(s: &str) -> Self {
        match s {
            "synced" => Self::Synced,
            "pending" => Self::Pending,
            "conflict" => Self::Conflict,
            "local_only" => Self::LocalOnly,
            _ => Self::LocalOnly,
        }
    }
}

impl std::fmt::Display for SyncStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// ConflictStrategy
// ─────────────────────────────────────────────────────────────────────────────

/// Strategy for resolving sync conflicts.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConflictStrategy {
    /// Most recent write wins — use the entity with the later modification time.
    LastWriteWins,
    /// Attempt a field-level merge of local and remote changes.
    Merge,
    /// Surface the conflict to the user for manual resolution.
    AskUser,
    /// Keep both versions — create a duplicate with a disambiguated name.
    KeepBoth,
}

// ─────────────────────────────────────────────────────────────────────────────
// SyncMetadata
// ─────────────────────────────────────────────────────────────────────────────

/// In-memory representation of sync tracking metadata for an entity.
///
/// Maps to the `sync_metadata` database table but provides typed access
/// to [`SyncStatus`] rather than raw strings.
#[derive(Debug, Clone, PartialEq)]
pub struct SyncMetadata {
    /// Primary key of the tracking record.
    pub id: Uuid,
    /// The type of entity being tracked (e.g., "preset", "snapshot", "profile").
    pub entity_type: String,
    /// The UUID of the tracked entity.
    pub entity_id: Uuid,
    /// When this entity was last successfully synced.
    pub last_synced_at: DateTime<Utc>,
    /// Current sync status.
    pub status: SyncStatus,
    /// Monotonically increasing local version counter.
    pub local_version: i64,
    /// Last known remote version (0 if never synced).
    pub remote_version: i64,
}

impl From<sync_metadata::Model> for SyncMetadata {
    fn from(m: sync_metadata::Model) -> Self {
        Self {
            id: m.id,
            entity_type: m.entity_type,
            entity_id: m.entity_id,
            last_synced_at: m.last_sync_at.with_timezone(&Utc),
            status: SyncStatus::from_str(&m.sync_status),
            local_version: m.local_version,
            remote_version: m.remote_version,
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// ConflictRecord
// ─────────────────────────────────────────────────────────────────────────────

/// A detected conflict between local and remote versions of an entity.
#[derive(Debug, Clone, PartialEq)]
pub struct ConflictRecord {
    /// The sync metadata for the conflicting entity.
    pub metadata: SyncMetadata,
    /// The remote version that conflicts with the local version.
    pub remote_version: i64,
}

// ─────────────────────────────────────────────────────────────────────────────
// SyncEngine
// ─────────────────────────────────────────────────────────────────────────────

/// Engine managing local-to-cloud synchronization.
///
/// Tracks per-entity sync state in the `sync_metadata` table and provides
/// methods for marking changes, detecting conflicts, and resolving them.
///
/// # Example
///
/// ```ignore
/// let engine = SyncEngine::new(db, ConflictStrategy::LastWriteWins);
///
/// // Mark a preset as locally modified
/// engine.mark_changed("preset", preset_id).await?;
///
/// // Get all entities that need syncing
/// let pending = engine.get_pending_changes().await?;
///
/// // After successful push to remote
/// engine.mark_synced("preset", preset_id, 42).await?;
/// ```
pub struct SyncEngine {
    db: DatabaseConnection,
    conflict_strategy: ConflictStrategy,
}

impl SyncEngine {
    /// Create a new sync engine with the given database connection and conflict strategy.
    pub fn new(db: DatabaseConnection, conflict_strategy: ConflictStrategy) -> Self {
        Self {
            db,
            conflict_strategy,
        }
    }

    /// Get the current conflict resolution strategy.
    pub fn conflict_strategy(&self) -> ConflictStrategy {
        self.conflict_strategy
    }

    /// Set the conflict resolution strategy.
    pub fn set_conflict_strategy(&mut self, strategy: ConflictStrategy) {
        self.conflict_strategy = strategy;
    }

    // ── Tracking ─────────────────────────────────────────────────────────

    /// Mark an entity as locally modified.
    ///
    /// If the entity is not yet tracked, creates a new tracking record with
    /// status `LocalOnly`. If already tracked, increments the local version
    /// and sets status to `Pending`.
    pub async fn mark_changed(
        &self,
        entity_type: &str,
        entity_id: Uuid,
    ) -> StorageResult<SyncMetadata> {
        let now = Utc::now();
        let existing = self.get_metadata(entity_type, entity_id).await?;

        match existing {
            Some(meta) => {
                // Update existing tracking record
                let model = SyncMetadataEntity::find_by_id(meta.id)
                    .one(&self.db)
                    .await?
                    .ok_or(StorageError::NotFound {
                        entity: "sync_metadata",
                        id: meta.id,
                    })?;

                let new_status = match meta.status {
                    SyncStatus::Synced => SyncStatus::Pending,
                    SyncStatus::Conflict => SyncStatus::Conflict, // preserve conflict
                    other => other,
                };

                let mut active: sync_metadata::ActiveModel = model.into();
                active.local_version = Set(meta.local_version + 1);
                active.sync_status = Set(new_status.as_str().to_string());
                active.last_sync_at = Set(now.into());
                let updated = active.update(&self.db).await?;
                Ok(SyncMetadata::from(updated))
            }
            None => {
                // Create new tracking record
                let id = Uuid::new_v4();
                let active = sync_metadata::ActiveModel {
                    id: Set(id),
                    entity_type: Set(entity_type.to_string()),
                    entity_id: Set(entity_id),
                    last_sync_at: Set(now.into()),
                    sync_status: Set(SyncStatus::LocalOnly.as_str().to_string()),
                    local_version: Set(1),
                    remote_version: Set(0),
                };
                let inserted = active.insert(&self.db).await?;
                Ok(SyncMetadata::from(inserted))
            }
        }
    }

    /// Get all entities that have pending local changes (status = Pending or LocalOnly).
    pub async fn get_pending_changes(&self) -> StorageResult<Vec<SyncMetadata>> {
        let models = SyncMetadataEntity::find()
            .filter(
                sync_metadata::Column::SyncStatus
                    .is_in([SyncStatus::Pending.as_str(), SyncStatus::LocalOnly.as_str()]),
            )
            .order_by(sync_metadata::Column::LastSyncAt, Order::Asc)
            .all(&self.db)
            .await?;

        Ok(models.into_iter().map(SyncMetadata::from).collect())
    }

    /// Get all entities with unresolved conflicts (status = Conflict).
    pub async fn get_conflicts(&self) -> StorageResult<Vec<SyncMetadata>> {
        let models = SyncMetadataEntity::find()
            .filter(sync_metadata::Column::SyncStatus.eq(SyncStatus::Conflict.as_str()))
            .order_by(sync_metadata::Column::LastSyncAt, Order::Asc)
            .all(&self.db)
            .await?;

        Ok(models.into_iter().map(SyncMetadata::from).collect())
    }

    /// Get sync metadata for a specific entity.
    pub async fn get_metadata(
        &self,
        entity_type: &str,
        entity_id: Uuid,
    ) -> StorageResult<Option<SyncMetadata>> {
        let model = SyncMetadataEntity::find()
            .filter(sync_metadata::Column::EntityType.eq(entity_type))
            .filter(sync_metadata::Column::EntityId.eq(entity_id))
            .one(&self.db)
            .await?;

        Ok(model.map(SyncMetadata::from))
    }

    /// Get sync metadata for all entities of a given type.
    pub async fn get_metadata_by_type(
        &self,
        entity_type: &str,
    ) -> StorageResult<Vec<SyncMetadata>> {
        let models = SyncMetadataEntity::find()
            .filter(sync_metadata::Column::EntityType.eq(entity_type))
            .order_by(sync_metadata::Column::LastSyncAt, Order::Desc)
            .all(&self.db)
            .await?;

        Ok(models.into_iter().map(SyncMetadata::from).collect())
    }

    // ── Sync Lifecycle ───────────────────────────────────────────────────

    /// Mark an entity as successfully synced with the given remote version.
    ///
    /// Sets status to `Synced` and updates the remote version tracker.
    pub async fn mark_synced(
        &self,
        entity_type: &str,
        entity_id: Uuid,
        remote_version: i64,
    ) -> StorageResult<SyncMetadata> {
        let meta = self
            .get_metadata(entity_type, entity_id)
            .await?
            .ok_or_else(|| {
                StorageError::BusinessRule(format!(
                    "no sync metadata for {entity_type}/{entity_id}"
                ))
            })?;

        let model = SyncMetadataEntity::find_by_id(meta.id)
            .one(&self.db)
            .await?
            .ok_or(StorageError::NotFound {
                entity: "sync_metadata",
                id: meta.id,
            })?;

        let mut active: sync_metadata::ActiveModel = model.into();
        active.sync_status = Set(SyncStatus::Synced.as_str().to_string());
        active.remote_version = Set(remote_version);
        active.last_sync_at = Set(Utc::now().into());
        let updated = active.update(&self.db).await?;
        Ok(SyncMetadata::from(updated))
    }

    // ── Conflict Detection ───────────────────────────────────────────────

    /// Detect conflicts between local state and incoming remote changes.
    ///
    /// A conflict exists when:
    /// - The entity has local changes (Pending status)
    /// - The remote version is newer than the last known remote version
    ///
    /// Entities with conflicts are marked with `Conflict` status.
    /// Returns the list of newly detected conflicts.
    pub async fn detect_conflicts(
        &self,
        remote_changes: &[(String, Uuid, i64)],
    ) -> StorageResult<Vec<ConflictRecord>> {
        let mut conflicts = Vec::new();

        for (entity_type, entity_id, remote_ver) in remote_changes {
            let meta = self.get_metadata(entity_type, *entity_id).await?;

            if let Some(meta) = meta {
                // Conflict: entity was modified locally AND remotely since last sync
                let locally_modified =
                    matches!(meta.status, SyncStatus::Pending | SyncStatus::LocalOnly);
                let remotely_modified = *remote_ver > meta.remote_version;

                if locally_modified && remotely_modified {
                    // Mark as conflict in the database
                    let model = SyncMetadataEntity::find_by_id(meta.id)
                        .one(&self.db)
                        .await?
                        .ok_or(StorageError::NotFound {
                            entity: "sync_metadata",
                            id: meta.id,
                        })?;

                    let mut active: sync_metadata::ActiveModel = model.into();
                    active.sync_status = Set(SyncStatus::Conflict.as_str().to_string());
                    active.update(&self.db).await?;

                    conflicts.push(ConflictRecord {
                        metadata: SyncMetadata {
                            status: SyncStatus::Conflict,
                            ..meta
                        },
                        remote_version: *remote_ver,
                    });
                }
            }
        }

        Ok(conflicts)
    }

    /// Resolve a conflict on an entity using the given strategy.
    ///
    /// - `LastWriteWins`: Accepts whichever version is newer. If local_version > remote_version,
    ///   keeps local (marks Pending for push). Otherwise accepts remote (marks Synced).
    /// - `KeepBoth`: Marks the local version as Pending for push under a new identity.
    ///   The caller is responsible for creating the duplicate entity.
    /// - `Merge`: Marks as Synced — caller must perform the actual merge before calling this.
    /// - `AskUser`: Returns an error; conflicts with this strategy must be resolved
    ///   by calling `resolve_conflict` again with a concrete strategy.
    pub async fn resolve_conflict(
        &self,
        entity_type: &str,
        entity_id: Uuid,
        strategy: ConflictStrategy,
    ) -> StorageResult<SyncMetadata> {
        let meta = self
            .get_metadata(entity_type, entity_id)
            .await?
            .ok_or_else(|| {
                StorageError::BusinessRule(format!(
                    "no sync metadata for {entity_type}/{entity_id}"
                ))
            })?;

        if meta.status != SyncStatus::Conflict {
            return Err(StorageError::BusinessRule(format!(
                "entity {entity_type}/{entity_id} is not in conflict (status: {})",
                meta.status
            )));
        }

        let model = SyncMetadataEntity::find_by_id(meta.id)
            .one(&self.db)
            .await?
            .ok_or(StorageError::NotFound {
                entity: "sync_metadata",
                id: meta.id,
            })?;

        match strategy {
            ConflictStrategy::LastWriteWins => {
                // Higher local version means local wins — mark pending to push
                // Otherwise remote wins — mark synced
                let mut active: sync_metadata::ActiveModel = model.into();
                if meta.local_version > meta.remote_version {
                    active.sync_status = Set(SyncStatus::Pending.as_str().to_string());
                } else {
                    active.sync_status = Set(SyncStatus::Synced.as_str().to_string());
                }
                active.last_sync_at = Set(Utc::now().into());
                let updated = active.update(&self.db).await?;
                Ok(SyncMetadata::from(updated))
            }
            ConflictStrategy::Merge => {
                // Caller has already merged — mark as synced
                let mut active: sync_metadata::ActiveModel = model.into();
                active.sync_status = Set(SyncStatus::Synced.as_str().to_string());
                active.last_sync_at = Set(Utc::now().into());
                let updated = active.update(&self.db).await?;
                Ok(SyncMetadata::from(updated))
            }
            ConflictStrategy::KeepBoth => {
                // Keep local as pending (will be pushed as a new entity by caller)
                let mut active: sync_metadata::ActiveModel = model.into();
                active.sync_status = Set(SyncStatus::Pending.as_str().to_string());
                active.last_sync_at = Set(Utc::now().into());
                let updated = active.update(&self.db).await?;
                Ok(SyncMetadata::from(updated))
            }
            ConflictStrategy::AskUser => Err(StorageError::BusinessRule(
                "AskUser strategy requires manual resolution — call resolve_conflict \
                     with a concrete strategy (LastWriteWins, Merge, or KeepBoth)"
                    .to_string(),
            )),
        }
    }

    /// Resolve a conflict using the engine's default conflict strategy.
    pub async fn resolve_conflict_default(
        &self,
        entity_type: &str,
        entity_id: Uuid,
    ) -> StorageResult<SyncMetadata> {
        self.resolve_conflict(entity_type, entity_id, self.conflict_strategy)
            .await
    }

    // ── Bulk Operations ──────────────────────────────────────────────────

    /// Get a summary of sync state across all tracked entities.
    pub async fn sync_summary(&self) -> StorageResult<SyncSummary> {
        let all = SyncMetadataEntity::find().all(&self.db).await?;

        let mut summary = SyncSummary::default();
        for model in &all {
            match SyncStatus::from_str(&model.sync_status) {
                SyncStatus::Synced => summary.synced += 1,
                SyncStatus::Pending => summary.pending += 1,
                SyncStatus::Conflict => summary.conflicts += 1,
                SyncStatus::LocalOnly => summary.local_only += 1,
            }
        }
        summary.total = all.len() as u64;

        Ok(summary)
    }

    /// Delete sync tracking metadata for an entity.
    ///
    /// Use this when an entity is permanently deleted from local storage.
    pub async fn remove_tracking(&self, entity_type: &str, entity_id: Uuid) -> StorageResult<bool> {
        let result = SyncMetadataEntity::delete_many()
            .filter(sync_metadata::Column::EntityType.eq(entity_type))
            .filter(sync_metadata::Column::EntityId.eq(entity_id))
            .exec(&self.db)
            .await?;

        Ok(result.rows_affected > 0)
    }

    /// Get all entities that have been modified since the given timestamp.
    ///
    /// Useful for delta sync — only transfer entities changed since last sync cycle.
    pub async fn get_changes_since(
        &self,
        since: DateTime<Utc>,
    ) -> StorageResult<Vec<SyncMetadata>> {
        let models = SyncMetadataEntity::find()
            .filter(sync_metadata::Column::LastSyncAt.gt(since))
            .filter(
                sync_metadata::Column::SyncStatus
                    .is_in([SyncStatus::Pending.as_str(), SyncStatus::LocalOnly.as_str()]),
            )
            .order_by(sync_metadata::Column::LastSyncAt, Order::Asc)
            .all(&self.db)
            .await?;

        Ok(models.into_iter().map(SyncMetadata::from).collect())
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// SyncSummary
// ─────────────────────────────────────────────────────────────────────────────

/// Aggregate counts of sync states across all tracked entities.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct SyncSummary {
    pub total: u64,
    pub synced: u64,
    pub pending: u64,
    pub conflicts: u64,
    pub local_only: u64,
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use sea_orm::Database;
    use sea_orm_migration::MigratorTrait;

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    async fn setup_db() -> Result<DatabaseConnection> {
        let db = Database::connect("sqlite::memory:").await?;
        crate::Migrator::up(&db, None).await?;
        Ok(db)
    }

    // -- SyncStatus

    #[test]
    fn sync_status_round_trip() {
        for status in [
            SyncStatus::Synced,
            SyncStatus::Pending,
            SyncStatus::Conflict,
            SyncStatus::LocalOnly,
        ] {
            let s = status.as_str();
            let parsed = SyncStatus::from_str(s);
            assert_eq!(parsed, status, "round-trip failed for {s}");
        }
    }

    #[test]
    fn sync_status_unknown_defaults_to_local_only() {
        assert_eq!(SyncStatus::from_str("garbage"), SyncStatus::LocalOnly);
    }

    // -- SyncEngine: mark_changed

    #[tokio::test]
    async fn mark_changed_creates_tracking_record() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let engine = SyncEngine::new(db, ConflictStrategy::LastWriteWins);
        let entity_id = Uuid::new_v4();

        // -- Exec
        let meta = engine.mark_changed("preset", entity_id).await?;

        // -- Check
        assert_eq!(meta.entity_type, "preset");
        assert_eq!(meta.entity_id, entity_id);
        assert_eq!(meta.status, SyncStatus::LocalOnly);
        assert_eq!(meta.local_version, 1);
        assert_eq!(meta.remote_version, 0);
        Ok(())
    }

    #[tokio::test]
    async fn mark_changed_increments_version() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let engine = SyncEngine::new(db, ConflictStrategy::LastWriteWins);
        let entity_id = Uuid::new_v4();

        // -- Exec
        engine.mark_changed("preset", entity_id).await?;
        let meta = engine.mark_changed("preset", entity_id).await?;

        // -- Check
        assert_eq!(meta.local_version, 2);
        assert_eq!(meta.status, SyncStatus::LocalOnly); // stays LocalOnly until synced
        Ok(())
    }

    #[tokio::test]
    async fn mark_changed_on_synced_entity_sets_pending() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let engine = SyncEngine::new(db, ConflictStrategy::LastWriteWins);
        let entity_id = Uuid::new_v4();

        engine.mark_changed("preset", entity_id).await?;
        engine.mark_synced("preset", entity_id, 1).await?;

        // -- Exec
        let meta = engine.mark_changed("preset", entity_id).await?;

        // -- Check
        assert_eq!(meta.status, SyncStatus::Pending);
        assert_eq!(meta.local_version, 2);
        Ok(())
    }

    // -- SyncEngine: get_pending_changes

    #[tokio::test]
    async fn get_pending_changes_returns_pending_and_local_only() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let engine = SyncEngine::new(db, ConflictStrategy::LastWriteWins);

        let id1 = Uuid::new_v4();
        let id2 = Uuid::new_v4();
        let id3 = Uuid::new_v4();

        engine.mark_changed("preset", id1).await?; // LocalOnly
        engine.mark_changed("preset", id2).await?; // will become Synced
        engine.mark_changed("preset", id3).await?; // LocalOnly

        engine.mark_synced("preset", id2, 1).await?; // now Synced

        // -- Exec
        let pending = engine.get_pending_changes().await?;

        // -- Check: should include id1 (LocalOnly) and id3 (LocalOnly), not id2 (Synced)
        assert_eq!(pending.len(), 2);
        let ids: Vec<Uuid> = pending.iter().map(|m| m.entity_id).collect();
        assert!(ids.contains(&id1));
        assert!(ids.contains(&id3));
        Ok(())
    }

    // -- SyncEngine: mark_synced

    #[tokio::test]
    async fn mark_synced_updates_status_and_remote_version() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let engine = SyncEngine::new(db, ConflictStrategy::LastWriteWins);
        let entity_id = Uuid::new_v4();

        engine.mark_changed("preset", entity_id).await?;

        // -- Exec
        let meta = engine.mark_synced("preset", entity_id, 42).await?;

        // -- Check
        assert_eq!(meta.status, SyncStatus::Synced);
        assert_eq!(meta.remote_version, 42);
        Ok(())
    }

    #[tokio::test]
    async fn mark_synced_untracked_entity_returns_error() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let engine = SyncEngine::new(db, ConflictStrategy::LastWriteWins);

        // -- Exec
        let result = engine.mark_synced("preset", Uuid::new_v4(), 1).await;

        // -- Check
        assert!(result.is_err());
        Ok(())
    }

    // -- SyncEngine: detect_conflicts

    #[tokio::test]
    async fn detect_conflicts_finds_concurrent_modifications() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let engine = SyncEngine::new(db, ConflictStrategy::LastWriteWins);
        let entity_id = Uuid::new_v4();

        // Entity was synced at remote version 5
        engine.mark_changed("preset", entity_id).await?;
        engine.mark_synced("preset", entity_id, 5).await?;

        // Local modification (now Pending)
        engine.mark_changed("preset", entity_id).await?;

        // Remote also changed to version 7
        let remote_changes = vec![("preset".to_string(), entity_id, 7i64)];

        // -- Exec
        let conflicts = engine.detect_conflicts(&remote_changes).await?;

        // -- Check
        assert_eq!(conflicts.len(), 1);
        assert_eq!(conflicts[0].metadata.entity_id, entity_id);
        assert_eq!(conflicts[0].remote_version, 7);

        // Entity should now be marked as Conflict in the database
        let meta = engine.get_metadata("preset", entity_id).await?.unwrap();
        assert_eq!(meta.status, SyncStatus::Conflict);
        Ok(())
    }

    #[tokio::test]
    async fn detect_conflicts_ignores_synced_entities() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let engine = SyncEngine::new(db, ConflictStrategy::LastWriteWins);
        let entity_id = Uuid::new_v4();

        // Entity synced and not locally modified
        engine.mark_changed("preset", entity_id).await?;
        engine.mark_synced("preset", entity_id, 5).await?;

        // Remote changed to version 7 — but no local changes, so no conflict
        let remote_changes = vec![("preset".to_string(), entity_id, 7i64)];

        // -- Exec
        let conflicts = engine.detect_conflicts(&remote_changes).await?;

        // -- Check: no conflict since entity is synced
        assert!(conflicts.is_empty());
        Ok(())
    }

    #[tokio::test]
    async fn detect_conflicts_ignores_unknown_entities() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let engine = SyncEngine::new(db, ConflictStrategy::LastWriteWins);

        // Remote reports change for entity we don't track locally
        let remote_changes = vec![("preset".to_string(), Uuid::new_v4(), 3i64)];

        // -- Exec
        let conflicts = engine.detect_conflicts(&remote_changes).await?;

        // -- Check: unknown entity is not a conflict
        assert!(conflicts.is_empty());
        Ok(())
    }

    // -- SyncEngine: resolve_conflict

    #[tokio::test]
    async fn resolve_conflict_last_write_wins_local() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let engine = SyncEngine::new(db, ConflictStrategy::LastWriteWins);
        let entity_id = Uuid::new_v4();

        // Create a conflict scenario: local version > remote version
        engine.mark_changed("preset", entity_id).await?;
        engine.mark_synced("preset", entity_id, 1).await?;
        // Local changes twice (version 2, 3)
        engine.mark_changed("preset", entity_id).await?;
        engine.mark_changed("preset", entity_id).await?;
        // Remote also changed
        let remote_changes = vec![("preset".to_string(), entity_id, 2i64)];
        engine.detect_conflicts(&remote_changes).await?;

        // -- Exec: resolve with LastWriteWins
        let meta = engine
            .resolve_conflict("preset", entity_id, ConflictStrategy::LastWriteWins)
            .await?;

        // -- Check: local version (3) > remote version (2), so local wins -> Pending
        assert_eq!(meta.status, SyncStatus::Pending);
        Ok(())
    }

    #[tokio::test]
    async fn resolve_conflict_last_write_wins_remote() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let engine = SyncEngine::new(db, ConflictStrategy::LastWriteWins);
        let entity_id = Uuid::new_v4();

        // Create a conflict: remote version > local version
        engine.mark_changed("preset", entity_id).await?;
        engine.mark_synced("preset", entity_id, 5).await?;
        engine.mark_changed("preset", entity_id).await?; // local_version = 2
                                                         // Remote advanced significantly
        let remote_changes = vec![("preset".to_string(), entity_id, 10i64)];
        engine.detect_conflicts(&remote_changes).await?;

        // -- Exec
        let meta = engine
            .resolve_conflict("preset", entity_id, ConflictStrategy::LastWriteWins)
            .await?;

        // -- Check: local_version (2) <= remote_version (5), remote wins -> Synced
        assert_eq!(meta.status, SyncStatus::Synced);
        Ok(())
    }

    #[tokio::test]
    async fn resolve_conflict_merge_marks_synced() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let engine = SyncEngine::new(db, ConflictStrategy::Merge);
        let entity_id = Uuid::new_v4();

        engine.mark_changed("preset", entity_id).await?;
        engine.mark_synced("preset", entity_id, 1).await?;
        engine.mark_changed("preset", entity_id).await?;
        let remote_changes = vec![("preset".to_string(), entity_id, 2i64)];
        engine.detect_conflicts(&remote_changes).await?;

        // -- Exec
        let meta = engine
            .resolve_conflict("preset", entity_id, ConflictStrategy::Merge)
            .await?;

        // -- Check
        assert_eq!(meta.status, SyncStatus::Synced);
        Ok(())
    }

    #[tokio::test]
    async fn resolve_conflict_keep_both_marks_pending() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let engine = SyncEngine::new(db, ConflictStrategy::KeepBoth);
        let entity_id = Uuid::new_v4();

        engine.mark_changed("preset", entity_id).await?;
        engine.mark_synced("preset", entity_id, 1).await?;
        engine.mark_changed("preset", entity_id).await?;
        let remote_changes = vec![("preset".to_string(), entity_id, 2i64)];
        engine.detect_conflicts(&remote_changes).await?;

        // -- Exec
        let meta = engine
            .resolve_conflict("preset", entity_id, ConflictStrategy::KeepBoth)
            .await?;

        // -- Check: local copy will be pushed as new entity
        assert_eq!(meta.status, SyncStatus::Pending);
        Ok(())
    }

    #[tokio::test]
    async fn resolve_conflict_ask_user_returns_error() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let engine = SyncEngine::new(db, ConflictStrategy::AskUser);
        let entity_id = Uuid::new_v4();

        engine.mark_changed("preset", entity_id).await?;
        engine.mark_synced("preset", entity_id, 1).await?;
        engine.mark_changed("preset", entity_id).await?;
        let remote_changes = vec![("preset".to_string(), entity_id, 2i64)];
        engine.detect_conflicts(&remote_changes).await?;

        // -- Exec
        let result = engine
            .resolve_conflict("preset", entity_id, ConflictStrategy::AskUser)
            .await;

        // -- Check: AskUser requires manual resolution
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("manual resolution"));
        Ok(())
    }

    #[tokio::test]
    async fn resolve_conflict_on_non_conflicting_entity_returns_error() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let engine = SyncEngine::new(db, ConflictStrategy::LastWriteWins);
        let entity_id = Uuid::new_v4();

        engine.mark_changed("preset", entity_id).await?; // status = LocalOnly

        // -- Exec
        let result = engine
            .resolve_conflict("preset", entity_id, ConflictStrategy::LastWriteWins)
            .await;

        // -- Check
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("not in conflict"));
        Ok(())
    }

    // -- SyncEngine: sync_summary

    #[tokio::test]
    async fn sync_summary_counts_all_states() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let engine = SyncEngine::new(db, ConflictStrategy::LastWriteWins);

        let id1 = Uuid::new_v4();
        let id2 = Uuid::new_v4();
        let id3 = Uuid::new_v4();

        engine.mark_changed("preset", id1).await?; // LocalOnly
        engine.mark_changed("preset", id2).await?; // will become Synced
        engine.mark_synced("preset", id2, 1).await?;
        engine.mark_changed("preset", id3).await?; // will become Pending then Conflict
        engine.mark_synced("preset", id3, 1).await?;
        engine.mark_changed("preset", id3).await?; // Pending
        let remote_changes = vec![("preset".to_string(), id3, 2i64)];
        engine.detect_conflicts(&remote_changes).await?;

        // -- Exec
        let summary = engine.sync_summary().await?;

        // -- Check
        assert_eq!(summary.total, 3);
        assert_eq!(summary.local_only, 1); // id1
        assert_eq!(summary.synced, 1); // id2
        assert_eq!(summary.conflicts, 1); // id3
        assert_eq!(summary.pending, 0);
        Ok(())
    }

    // -- SyncEngine: remove_tracking

    #[tokio::test]
    async fn remove_tracking_deletes_metadata() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let engine = SyncEngine::new(db, ConflictStrategy::LastWriteWins);
        let entity_id = Uuid::new_v4();

        engine.mark_changed("preset", entity_id).await?;

        // -- Exec
        let deleted = engine.remove_tracking("preset", entity_id).await?;

        // -- Check
        assert!(deleted);
        let meta = engine.get_metadata("preset", entity_id).await?;
        assert!(meta.is_none());
        Ok(())
    }

    #[tokio::test]
    async fn remove_tracking_returns_false_for_untracked() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let engine = SyncEngine::new(db, ConflictStrategy::LastWriteWins);

        // -- Exec
        let deleted = engine.remove_tracking("preset", Uuid::new_v4()).await?;

        // -- Check
        assert!(!deleted);
        Ok(())
    }

    // -- SyncEngine: get_changes_since

    #[tokio::test]
    async fn get_changes_since_filters_by_timestamp() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let engine = SyncEngine::new(db, ConflictStrategy::LastWriteWins);

        let before = Utc::now();

        let id1 = Uuid::new_v4();
        let id2 = Uuid::new_v4();

        engine.mark_changed("preset", id1).await?;
        engine.mark_changed("preset", id2).await?;

        // -- Exec: get changes since before we made any
        // (since the mark_changed timestamps are >= `before`, they should appear)
        let changes = engine
            .get_changes_since(before - chrono::Duration::seconds(1))
            .await?;

        // -- Check
        assert_eq!(changes.len(), 2);
        Ok(())
    }

    // -- SyncEngine: get_metadata_by_type

    #[tokio::test]
    async fn get_metadata_by_type_filters_correctly() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let engine = SyncEngine::new(db, ConflictStrategy::LastWriteWins);

        engine.mark_changed("preset", Uuid::new_v4()).await?;
        engine.mark_changed("preset", Uuid::new_v4()).await?;
        engine.mark_changed("snapshot", Uuid::new_v4()).await?;

        // -- Exec
        let presets = engine.get_metadata_by_type("preset").await?;
        let snapshots = engine.get_metadata_by_type("snapshot").await?;

        // -- Check
        assert_eq!(presets.len(), 2);
        assert_eq!(snapshots.len(), 1);
        Ok(())
    }

    // -- SyncEngine: resolve_conflict_default

    #[tokio::test]
    async fn resolve_conflict_default_uses_engine_strategy() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let engine = SyncEngine::new(db, ConflictStrategy::Merge);
        let entity_id = Uuid::new_v4();

        engine.mark_changed("preset", entity_id).await?;
        engine.mark_synced("preset", entity_id, 1).await?;
        engine.mark_changed("preset", entity_id).await?;
        let remote_changes = vec![("preset".to_string(), entity_id, 2i64)];
        engine.detect_conflicts(&remote_changes).await?;

        // -- Exec: uses the engine's default (Merge)
        let meta = engine.resolve_conflict_default("preset", entity_id).await?;

        // -- Check: Merge resolves to Synced
        assert_eq!(meta.status, SyncStatus::Synced);
        Ok(())
    }

    // -- SyncEngine: get_conflicts

    #[tokio::test]
    async fn get_conflicts_returns_only_conflicting_entities() -> Result<()> {
        // -- Setup & Fixtures
        let db = setup_db().await?;
        let engine = SyncEngine::new(db, ConflictStrategy::LastWriteWins);

        let id1 = Uuid::new_v4();
        let id2 = Uuid::new_v4();

        engine.mark_changed("preset", id1).await?; // LocalOnly
        engine.mark_changed("preset", id2).await?; // will conflict
        engine.mark_synced("preset", id2, 1).await?;
        engine.mark_changed("preset", id2).await?;
        let remote_changes = vec![("preset".to_string(), id2, 2i64)];
        engine.detect_conflicts(&remote_changes).await?;

        // -- Exec
        let conflicts = engine.get_conflicts().await?;

        // -- Check
        assert_eq!(conflicts.len(), 1);
        assert_eq!(conflicts[0].entity_id, id2);
        Ok(())
    }
}
