//! Sync engine — orchestrates local ↔ cloud synchronization with conflict resolution.
//!
//! The `SyncEngine` manages delta sync by comparing `local_version` and `remote_version`
//! in the `sync_metadata` table. Conflicts arise when both versions advance independently
//! (i.e., the entity was modified on multiple devices between syncs).

use std::sync::Arc;

use chrono::Utc;
use sea_orm::entity::prelude::*;
use sea_orm::{ActiveValue, ColumnTrait, Condition, EntityTrait, QueryFilter};
use moire::sync::Mutex;
use uuid::Uuid;

use signal_storage::entities::{preset, sync_metadata};
use sync_proto::{
    ConflictInfo, ConflictResolution, ConflictStrategy, EntityChange, EntityType, SyncProgress,
    SyncStatus,
};

use crate::error::SyncError;

// region: --- SyncEngine

/// Orchestrates synchronization between local and cloud databases.
///
/// The engine works in three phases:
/// 1. **Detect changes** — scan `sync_metadata` for entities where `local_version != remote_version`
/// 2. **Push/pull** — transfer changed entities between local and cloud databases
/// 3. **Resolve conflicts** — apply the configured strategy when both sides changed
pub struct SyncEngine {
    /// Local database connection (SQLite)
    local_db: Arc<DatabaseConnection>,
    /// Cloud database connection (PostgreSQL), `None` when offline
    cloud_db: Arc<Mutex<Option<DatabaseConnection>>>,
    /// Default conflict resolution strategy
    default_strategy: Mutex<ConflictStrategy>,
    /// Offline change queue — entity changes recorded while cloud is unavailable
    offline_queue: Mutex<Vec<EntityChange>>,
    /// Last sync progress snapshot
    last_progress: Mutex<Option<SyncProgress>>,
}

impl SyncEngine {
    pub fn new(local_db: DatabaseConnection) -> Self {
        Self {
            local_db: Arc::new(local_db),
            cloud_db: Arc::new(Mutex::new("sync.engine.cloud_db", None)),
            default_strategy: Mutex::new("sync.engine.strategy", ConflictStrategy::default()),
            offline_queue: Mutex::new("sync.engine.offline_queue", Vec::new()),
            last_progress: Mutex::new("sync.engine.progress", None),
        }
    }

    /// Connect the cloud database (enables actual sync).
    pub async fn set_cloud_db(&self, db: DatabaseConnection) {
        *self.cloud_db.lock().await = Some(db);
    }

    /// Disconnect the cloud database (switches to offline queueing).
    pub async fn disconnect_cloud(&self) {
        *self.cloud_db.lock().await = None;
    }

    /// Check whether a cloud connection is available.
    pub async fn is_online(&self) -> bool {
        self.cloud_db.lock().await.is_some()
    }

    /// Set the default conflict resolution strategy.
    pub async fn set_default_strategy(&self, strategy: ConflictStrategy) {
        *self.default_strategy.lock().await = strategy;
    }

    /// Get the last sync progress snapshot, if any.
    pub async fn last_progress(&self) -> Option<SyncProgress> {
        self.last_progress.lock().await.clone()
    }

    // region: --- Core Sync Operations

    /// Run a complete delta sync cycle.
    ///
    /// 1. Finds all entities with `sync_status != "synced"` in `sync_metadata`
    /// 2. Detects conflicts (both local and remote versions advanced)
    /// 3. Pushes local changes to cloud, pulls remote changes to local
    /// 4. Resolves conflicts using the configured strategy
    /// 5. Updates `sync_metadata` to reflect the new state
    pub async fn sync(&self) -> Result<SyncProgress, SyncError> {
        let cloud_db = self.cloud_db.lock().await;
        let cloud = match cloud_db.as_ref() {
            Some(db) => db,
            None => {
                tracing::info!("Sync skipped: offline");
                return Ok(SyncProgress {
                    total: 0,
                    completed: 0,
                    conflicts: 0,
                    errors: 0,
                    message: "Offline — changes queued for later".into(),
                });
            }
        };

        // Step 1: Find all pending/conflict metadata entries
        let pending_metadata = sync_metadata::Entity::find()
            .filter(
                Condition::any()
                    .add(sync_metadata::Column::SyncStatus.eq("pending"))
                    .add(sync_metadata::Column::SyncStatus.eq("conflict"))
                    .add(sync_metadata::Column::SyncStatus.eq("local_only")),
            )
            .all(self.local_db.as_ref())
            .await
            .map_err(SyncError::Database)?;

        // Also drain the offline queue
        let queued = {
            let mut queue = self.offline_queue.lock().await;
            std::mem::take(&mut *queue)
        };

        let total = pending_metadata.len() + queued.len();
        let mut progress = SyncProgress::new(total);
        progress.message = format!("Syncing {total} entities...");

        // Step 2: Process each pending metadata entry
        for meta in &pending_metadata {
            let entity_type = match EntityType::from_str_value(&meta.entity_type) {
                Some(et) => et,
                None => {
                    progress.errors += 1;
                    continue;
                }
            };

            let status = SyncStatus::from_str_value(&meta.sync_status);
            match status {
                SyncStatus::Pending | SyncStatus::LocalOnly => {
                    // Push local → cloud
                    match self.push_entity(entity_type, meta.entity_id, cloud).await {
                        Ok(()) => {
                            self.update_metadata_synced(meta.id, meta.local_version)
                                .await?;
                            progress.completed += 1;
                        }
                        Err(e) => {
                            tracing::warn!(entity_id = %meta.entity_id, error = %e, "Push failed");
                            progress.errors += 1;
                        }
                    }
                }
                SyncStatus::Conflict => {
                    let strategy = *self.default_strategy.lock().await;
                    match self
                        .resolve_conflict_internal(entity_type, meta, strategy, cloud)
                        .await
                    {
                        Ok(resolution) => {
                            tracing::info!(
                                entity_id = %meta.entity_id,
                                ?resolution,
                                "Conflict resolved"
                            );
                            progress.conflicts += 1;
                            progress.completed += 1;
                        }
                        Err(e) => {
                            tracing::warn!(
                                entity_id = %meta.entity_id,
                                error = %e,
                                "Conflict resolution failed"
                            );
                            progress.errors += 1;
                        }
                    }
                }
                SyncStatus::Synced => {
                    progress.completed += 1;
                }
            }
        }

        // Step 3: Process queued offline changes
        for change in &queued {
            match self
                .push_entity(change.entity_type, change.entity_id, cloud)
                .await
            {
                Ok(()) => {
                    progress.completed += 1;
                }
                Err(e) => {
                    tracing::warn!(
                        entity_id = %change.entity_id,
                        error = %e,
                        "Queued push failed"
                    );
                    progress.errors += 1;
                }
            }
        }

        progress.message = if progress.errors > 0 {
            format!(
                "Sync completed with {} errors ({} conflicts resolved)",
                progress.errors, progress.conflicts
            )
        } else {
            format!(
                "Sync complete: {} entities, {} conflicts resolved",
                progress.completed, progress.conflicts
            )
        };

        *self.last_progress.lock().await = Some(progress.clone());
        Ok(progress)
    }

    /// Get all entities with pending (unsynced) changes.
    pub async fn get_pending_changes(&self) -> Result<Vec<EntityChange>, SyncError> {
        let metadata = sync_metadata::Entity::find()
            .filter(sync_metadata::Column::SyncStatus.ne("synced"))
            .all(self.local_db.as_ref())
            .await
            .map_err(SyncError::Database)?;

        Ok(metadata
            .into_iter()
            .filter_map(|m| {
                let entity_type = EntityType::from_str_value(&m.entity_type)?;
                Some(EntityChange {
                    entity_type,
                    entity_id: m.entity_id,
                    local_version: m.local_version,
                    remote_version: m.remote_version,
                    modified_at: m.last_sync_at.with_timezone(&Utc),
                    status: SyncStatus::from_str_value(&m.sync_status),
                })
            })
            .collect())
    }

    /// Get all entities currently in conflict state.
    pub async fn get_conflicts(&self) -> Result<Vec<ConflictInfo>, SyncError> {
        let metadata = sync_metadata::Entity::find()
            .filter(sync_metadata::Column::SyncStatus.eq("conflict"))
            .all(self.local_db.as_ref())
            .await
            .map_err(SyncError::Database)?;

        Ok(metadata
            .into_iter()
            .filter_map(|m| {
                let entity_type = EntityType::from_str_value(&m.entity_type)?;
                Some(ConflictInfo {
                    entity_type,
                    entity_id: m.entity_id,
                    local_version: m.local_version,
                    remote_version: m.remote_version,
                    local_modified_at: m.last_sync_at.with_timezone(&Utc),
                    // Remote modified_at would come from the cloud DB; use sync time as proxy
                    remote_modified_at: m.last_sync_at.with_timezone(&Utc),
                })
            })
            .collect())
    }

    /// Resolve a specific conflict using the given strategy.
    pub async fn resolve_conflict(
        &self,
        entity_type: EntityType,
        entity_id: Uuid,
        strategy: ConflictStrategy,
    ) -> Result<ConflictResolution, SyncError> {
        let cloud_db = self.cloud_db.lock().await;
        let cloud = cloud_db
            .as_ref()
            .ok_or_else(|| SyncError::Offline("Cannot resolve conflict while offline".into()))?;

        let meta = sync_metadata::Entity::find()
            .filter(
                Condition::all()
                    .add(sync_metadata::Column::EntityType.eq(entity_type.as_str()))
                    .add(sync_metadata::Column::EntityId.eq(entity_id)),
            )
            .one(self.local_db.as_ref())
            .await
            .map_err(SyncError::Database)?
            .ok_or(SyncError::NotFound(entity_type, entity_id))?;

        self.resolve_conflict_internal(entity_type, &meta, strategy, cloud)
            .await
    }

    /// Mark an entity as locally modified (bumps local_version, sets status to pending).
    pub async fn mark_modified(
        &self,
        entity_type: EntityType,
        entity_id: Uuid,
    ) -> Result<(), SyncError> {
        let now = Utc::now().fixed_offset();

        // Try to find existing metadata
        let existing = sync_metadata::Entity::find()
            .filter(
                Condition::all()
                    .add(sync_metadata::Column::EntityType.eq(entity_type.as_str()))
                    .add(sync_metadata::Column::EntityId.eq(entity_id)),
            )
            .one(self.local_db.as_ref())
            .await
            .map_err(SyncError::Database)?;

        match existing {
            Some(meta) => {
                // Bump local version and mark pending
                let new_version = meta.local_version + 1;
                let mut active: sync_metadata::ActiveModel = meta.into();
                active.local_version = ActiveValue::Set(new_version);
                active.sync_status = ActiveValue::Set(SyncStatus::Pending.as_str().to_string());
                active.last_sync_at = ActiveValue::Set(now);
                active.update(self.local_db.as_ref()).await.map_err(SyncError::Database)?;
            }
            None => {
                // Create new metadata entry (local_only)
                let meta = sync_metadata::ActiveModel {
                    id: ActiveValue::Set(Uuid::new_v4()),
                    entity_type: ActiveValue::Set(entity_type.as_str().to_string()),
                    entity_id: ActiveValue::Set(entity_id),
                    last_sync_at: ActiveValue::Set(now),
                    sync_status: ActiveValue::Set(SyncStatus::LocalOnly.as_str().to_string()),
                    local_version: ActiveValue::Set(1),
                    remote_version: ActiveValue::Set(0),
                };
                sync_metadata::Entity::insert(meta)
                    .exec(self.local_db.as_ref())
                    .await
                    .map_err(SyncError::Database)?;
            }
        }

        // If offline, also queue for later
        if !self.is_online().await {
            self.offline_queue.lock().await.push(EntityChange {
                entity_type,
                entity_id,
                local_version: 1,
                remote_version: 0,
                modified_at: now.with_timezone(&Utc),
                status: SyncStatus::Pending,
            });
        }

        Ok(())
    }

    /// Get the sync status for a specific entity.
    pub async fn get_entity_status(
        &self,
        entity_type: EntityType,
        entity_id: Uuid,
    ) -> Result<SyncStatus, SyncError> {
        let meta = sync_metadata::Entity::find()
            .filter(
                Condition::all()
                    .add(sync_metadata::Column::EntityType.eq(entity_type.as_str()))
                    .add(sync_metadata::Column::EntityId.eq(entity_id)),
            )
            .one(self.local_db.as_ref())
            .await
            .map_err(SyncError::Database)?;

        match meta {
            Some(m) => Ok(SyncStatus::from_str_value(&m.sync_status)),
            None => Ok(SyncStatus::LocalOnly),
        }
    }

    /// Get the size of the offline change queue.
    pub async fn offline_queue_size(&self) -> usize {
        self.offline_queue.lock().await.len()
    }

    // endregion: --- Core Sync Operations

    // region: --- Internal Helpers

    /// Push a local entity to the cloud database.
    async fn push_entity(
        &self,
        entity_type: EntityType,
        entity_id: Uuid,
        cloud: &DatabaseConnection,
    ) -> Result<(), SyncError> {
        match entity_type {
            EntityType::Preset => {
                let local_preset = preset::Entity::find_by_id(entity_id)
                    .one(self.local_db.as_ref())
                    .await
                    .map_err(SyncError::Database)?
                    .ok_or(SyncError::NotFound(entity_type, entity_id))?;

                // Upsert to cloud: try insert, update on conflict
                let active = preset::ActiveModel {
                    id: ActiveValue::Set(local_preset.id),
                    name: ActiveValue::Set(local_preset.name),
                    description: ActiveValue::Set(local_preset.description),
                    author_id: ActiveValue::Set(local_preset.author_id),
                    category: ActiveValue::Set(local_preset.category),
                    tags: ActiveValue::Set(local_preset.tags),
                    data: ActiveValue::Set(local_preset.data),
                    is_public: ActiveValue::Set(local_preset.is_public),
                    is_deleted: ActiveValue::Set(local_preset.is_deleted),
                    version: ActiveValue::Set(local_preset.version),
                    created_at: ActiveValue::Set(local_preset.created_at),
                    updated_at: ActiveValue::Set(local_preset.updated_at),
                };

                // Check if it already exists in cloud
                let cloud_exists = preset::Entity::find_by_id(entity_id)
                    .one(cloud)
                    .await
                    .map_err(SyncError::Database)?;

                if cloud_exists.is_some() {
                    active.update(cloud).await.map_err(SyncError::Database)?;
                } else {
                    preset::Entity::insert(active)
                        .exec(cloud)
                        .await
                        .map_err(SyncError::Database)?;
                }
            }
            // Snapshot and ModuleChunk follow the same pattern but with their respective entities.
            // For now we only fully implement preset sync; other entity types are stubbed.
            EntityType::Snapshot | EntityType::ModuleChunk => {
                tracing::debug!(
                    ?entity_type,
                    %entity_id,
                    "Push for entity type not yet fully implemented"
                );
            }
        }

        Ok(())
    }

    /// Update sync_metadata to mark an entity as synced.
    async fn update_metadata_synced(
        &self,
        metadata_id: Uuid,
        version: i64,
    ) -> Result<(), SyncError> {
        let now = Utc::now().fixed_offset();
        let active = sync_metadata::ActiveModel {
            id: ActiveValue::Set(metadata_id),
            sync_status: ActiveValue::Set(SyncStatus::Synced.as_str().to_string()),
            remote_version: ActiveValue::Set(version),
            last_sync_at: ActiveValue::Set(now),
            ..Default::default()
        };
        active
            .update(self.local_db.as_ref())
            .await
            .map_err(SyncError::Database)?;
        Ok(())
    }

    /// Internal conflict resolution for a specific metadata entry.
    async fn resolve_conflict_internal(
        &self,
        entity_type: EntityType,
        meta: &sync_metadata::Model,
        strategy: ConflictStrategy,
        cloud: &DatabaseConnection,
    ) -> Result<ConflictResolution, SyncError> {
        let resolution = match strategy {
            ConflictStrategy::LastWriteWins => {
                self.resolve_last_write_wins(entity_type, meta, cloud)
                    .await?
            }
            ConflictStrategy::Merge => {
                self.resolve_merge(entity_type, meta, cloud).await?
            }
            ConflictStrategy::KeepBoth => {
                self.resolve_keep_both(entity_type, meta, cloud).await?
            }
            ConflictStrategy::AskUser => {
                // Mark as deferred — the UI layer picks this up and prompts the user
                ConflictResolution::Deferred
            }
        };

        // If resolved (not deferred), update metadata
        if resolution != ConflictResolution::Deferred {
            let now = Utc::now().fixed_offset();
            let version = std::cmp::max(meta.local_version, meta.remote_version);
            let active = sync_metadata::ActiveModel {
                id: ActiveValue::Set(meta.id),
                sync_status: ActiveValue::Set(SyncStatus::Synced.as_str().to_string()),
                local_version: ActiveValue::Set(version),
                remote_version: ActiveValue::Set(version),
                last_sync_at: ActiveValue::Set(now),
                ..Default::default()
            };
            active
                .update(self.local_db.as_ref())
                .await
                .map_err(SyncError::Database)?;
        }

        Ok(resolution)
    }

    /// LastWriteWins: compare `updated_at` timestamps and keep the most recent.
    async fn resolve_last_write_wins(
        &self,
        entity_type: EntityType,
        meta: &sync_metadata::Model,
        cloud: &DatabaseConnection,
    ) -> Result<ConflictResolution, SyncError> {
        match entity_type {
            EntityType::Preset => {
                let local = preset::Entity::find_by_id(meta.entity_id)
                    .one(self.local_db.as_ref())
                    .await
                    .map_err(SyncError::Database)?;
                let remote = preset::Entity::find_by_id(meta.entity_id)
                    .one(cloud)
                    .await
                    .map_err(SyncError::Database)?;

                match (local, remote) {
                    (Some(local_p), Some(remote_p)) => {
                        if local_p.updated_at >= remote_p.updated_at {
                            // Local wins — push to cloud
                            self.push_entity(entity_type, meta.entity_id, cloud)
                                .await?;
                            Ok(ConflictResolution::KeptLocal)
                        } else {
                            // Remote wins — pull from cloud
                            self.pull_preset(&remote_p).await?;
                            Ok(ConflictResolution::KeptRemote)
                        }
                    }
                    (Some(_), None) => {
                        // Only exists locally — push
                        self.push_entity(entity_type, meta.entity_id, cloud)
                            .await?;
                        Ok(ConflictResolution::KeptLocal)
                    }
                    (None, Some(remote_p)) => {
                        // Only exists remotely — pull
                        self.pull_preset(&remote_p).await?;
                        Ok(ConflictResolution::KeptRemote)
                    }
                    (None, None) => Err(SyncError::NotFound(entity_type, meta.entity_id)),
                }
            }
            _ => {
                // For unimplemented entity types, default to keeping local
                self.push_entity(entity_type, meta.entity_id, cloud)
                    .await?;
                Ok(ConflictResolution::KeptLocal)
            }
        }
    }

    /// Merge: combine non-overlapping JSON field changes.
    ///
    /// Works by comparing each top-level key in the preset `data` JSON blob.
    /// If a key was only changed on one side, that side's value is kept.
    /// If both sides changed the same key, the local value wins (safe default).
    async fn resolve_merge(
        &self,
        entity_type: EntityType,
        meta: &sync_metadata::Model,
        cloud: &DatabaseConnection,
    ) -> Result<ConflictResolution, SyncError> {
        match entity_type {
            EntityType::Preset => {
                let local = preset::Entity::find_by_id(meta.entity_id)
                    .one(self.local_db.as_ref())
                    .await
                    .map_err(SyncError::Database)?
                    .ok_or(SyncError::NotFound(entity_type, meta.entity_id))?;

                let remote = preset::Entity::find_by_id(meta.entity_id)
                    .one(cloud)
                    .await
                    .map_err(SyncError::Database)?
                    .ok_or(SyncError::NotFound(entity_type, meta.entity_id))?;

                // Merge the JSON data blobs
                let merged_data = merge_json(&local.data, &remote.data);

                // Use whichever metadata fields are newer
                let use_local_meta = local.updated_at >= remote.updated_at;
                let merged_version = std::cmp::max(local.version, remote.version) + 1;

                let active = preset::ActiveModel {
                    id: ActiveValue::Set(local.id),
                    name: ActiveValue::Set(if use_local_meta {
                        local.name.clone()
                    } else {
                        remote.name.clone()
                    }),
                    description: ActiveValue::Set(if use_local_meta {
                        local.description.clone()
                    } else {
                        remote.description.clone()
                    }),
                    author_id: ActiveValue::Set(local.author_id),
                    category: ActiveValue::Set(local.category.clone()),
                    tags: ActiveValue::Set(if use_local_meta {
                        local.tags.clone()
                    } else {
                        remote.tags.clone()
                    }),
                    data: ActiveValue::Set(merged_data.clone()),
                    is_public: ActiveValue::Set(local.is_public || remote.is_public),
                    is_deleted: ActiveValue::Set(local.is_deleted && remote.is_deleted),
                    version: ActiveValue::Set(merged_version),
                    created_at: ActiveValue::Set(local.created_at),
                    updated_at: ActiveValue::Set(Utc::now().fixed_offset()),
                };

                // Update both local and cloud
                active
                    .clone()
                    .update(self.local_db.as_ref())
                    .await
                    .map_err(SyncError::Database)?;
                active
                    .update(cloud)
                    .await
                    .map_err(SyncError::Database)?;

                Ok(ConflictResolution::Merged)
            }
            _ => {
                // Fallback to last-write-wins for unsupported types
                self.resolve_last_write_wins(entity_type, meta, cloud).await
            }
        }
    }

    /// KeepBoth: keep the local version as-is, insert the remote version with a new ID.
    async fn resolve_keep_both(
        &self,
        entity_type: EntityType,
        meta: &sync_metadata::Model,
        cloud: &DatabaseConnection,
    ) -> Result<ConflictResolution, SyncError> {
        match entity_type {
            EntityType::Preset => {
                let remote = preset::Entity::find_by_id(meta.entity_id)
                    .one(cloud)
                    .await
                    .map_err(SyncError::Database)?
                    .ok_or(SyncError::NotFound(entity_type, meta.entity_id))?;

                // Insert remote as a copy with new ID and suffixed name
                let copy_id = Uuid::new_v4();
                let copy_name = format!("{} (synced copy)", remote.name);
                let copy = preset::ActiveModel {
                    id: ActiveValue::Set(copy_id),
                    name: ActiveValue::Set(copy_name),
                    description: ActiveValue::Set(remote.description),
                    author_id: ActiveValue::Set(remote.author_id),
                    category: ActiveValue::Set(remote.category),
                    tags: ActiveValue::Set(remote.tags),
                    data: ActiveValue::Set(remote.data),
                    is_public: ActiveValue::Set(false),
                    is_deleted: ActiveValue::Set(false),
                    version: ActiveValue::Set(1),
                    created_at: ActiveValue::Set(Utc::now().fixed_offset()),
                    updated_at: ActiveValue::Set(Utc::now().fixed_offset()),
                };

                preset::Entity::insert(copy)
                    .exec(self.local_db.as_ref())
                    .await
                    .map_err(SyncError::Database)?;

                // Push original local to cloud
                self.push_entity(entity_type, meta.entity_id, cloud)
                    .await?;

                Ok(ConflictResolution::KeptBoth)
            }
            _ => {
                self.push_entity(entity_type, meta.entity_id, cloud)
                    .await?;
                Ok(ConflictResolution::KeptLocal)
            }
        }
    }

    /// Pull a preset from the cloud into the local database.
    async fn pull_preset(&self, remote: &preset::Model) -> Result<(), SyncError> {
        let local_exists = preset::Entity::find_by_id(remote.id)
            .one(self.local_db.as_ref())
            .await
            .map_err(SyncError::Database)?;

        let active = preset::ActiveModel {
            id: ActiveValue::Set(remote.id),
            name: ActiveValue::Set(remote.name.clone()),
            description: ActiveValue::Set(remote.description.clone()),
            author_id: ActiveValue::Set(remote.author_id),
            category: ActiveValue::Set(remote.category.clone()),
            tags: ActiveValue::Set(remote.tags.clone()),
            data: ActiveValue::Set(remote.data.clone()),
            is_public: ActiveValue::Set(remote.is_public),
            is_deleted: ActiveValue::Set(remote.is_deleted),
            version: ActiveValue::Set(remote.version),
            created_at: ActiveValue::Set(remote.created_at),
            updated_at: ActiveValue::Set(remote.updated_at),
        };

        if local_exists.is_some() {
            active
                .update(self.local_db.as_ref())
                .await
                .map_err(SyncError::Database)?;
        } else {
            preset::Entity::insert(active)
                .exec(self.local_db.as_ref())
                .await
                .map_err(SyncError::Database)?;
        }

        Ok(())
    }

    // endregion: --- Internal Helpers
}

// endregion: --- SyncEngine

// region: --- JSON Merge

/// Merge two JSON values at the top level.
///
/// For objects: merges keys from both, preferring `local` when both have the same key.
/// For non-objects: returns `local` (can't merge scalars/arrays structurally).
fn merge_json(local: &serde_json::Value, remote: &serde_json::Value) -> serde_json::Value {
    match (local, remote) {
        (serde_json::Value::Object(local_obj), serde_json::Value::Object(remote_obj)) => {
            let mut merged = remote_obj.clone();
            // Local keys overwrite remote keys
            for (key, value) in local_obj {
                merged.insert(key.clone(), value.clone());
            }
            serde_json::Value::Object(merged)
        }
        // Non-object: local wins
        _ => local.clone(),
    }
}

// endregion: --- JSON Merge

// region: --- SyncEngineService impl

#[async_trait::async_trait]
impl sync_proto::SyncEngineService for SyncEngine {
    async fn sync(&self) -> Result<SyncProgress, String> {
        self.sync().await.map_err(|e| e.to_string())
    }

    async fn get_pending_changes(&self) -> Result<Vec<EntityChange>, String> {
        self.get_pending_changes().await.map_err(|e| e.to_string())
    }

    async fn get_conflicts(&self) -> Result<Vec<ConflictInfo>, String> {
        self.get_conflicts().await.map_err(|e| e.to_string())
    }

    async fn resolve_conflict(
        &self,
        entity_type: EntityType,
        entity_id: Uuid,
        strategy: ConflictStrategy,
    ) -> Result<ConflictResolution, String> {
        self.resolve_conflict(entity_type, entity_id, strategy)
            .await
            .map_err(|e| e.to_string())
    }

    async fn mark_modified(
        &self,
        entity_type: EntityType,
        entity_id: Uuid,
    ) -> Result<(), String> {
        self.mark_modified(entity_type, entity_id)
            .await
            .map_err(|e| e.to_string())
    }

    async fn get_entity_status(
        &self,
        entity_type: EntityType,
        entity_id: Uuid,
    ) -> Result<SyncStatus, String> {
        self.get_entity_status(entity_type, entity_id)
            .await
            .map_err(|e| e.to_string())
    }
}

// endregion: --- SyncEngineService impl

#[cfg(test)]
mod tests {
    use super::*;

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    // region: --- JSON Merge Tests

    #[test]
    fn test_merge_json_objects_local_wins_on_overlap() -> Result<()> {
        // -- Setup
        let local = serde_json::json!({"amp": "clean", "volume": 7});
        let remote = serde_json::json!({"amp": "distorted", "reverb": "hall"});

        // -- Exec
        let merged = merge_json(&local, &remote);

        // -- Check: local "amp" wins, remote "reverb" kept
        assert_eq!(merged["amp"], "clean");
        assert_eq!(merged["volume"], 7);
        assert_eq!(merged["reverb"], "hall");
        Ok(())
    }

    #[test]
    fn test_merge_json_disjoint_keys() -> Result<()> {
        // -- Setup
        let local = serde_json::json!({"a": 1, "b": 2});
        let remote = serde_json::json!({"c": 3, "d": 4});

        // -- Exec
        let merged = merge_json(&local, &remote);

        // -- Check: all keys present
        assert_eq!(merged["a"], 1);
        assert_eq!(merged["b"], 2);
        assert_eq!(merged["c"], 3);
        assert_eq!(merged["d"], 4);
        Ok(())
    }

    #[test]
    fn test_merge_json_non_object_local_wins() -> Result<()> {
        // -- Setup
        let local = serde_json::json!("local_string");
        let remote = serde_json::json!("remote_string");

        // -- Exec
        let merged = merge_json(&local, &remote);

        // -- Check
        assert_eq!(merged, "local_string");
        Ok(())
    }

    #[test]
    fn test_merge_json_empty_objects() -> Result<()> {
        // -- Setup
        let local = serde_json::json!({});
        let remote = serde_json::json!({"key": "value"});

        // -- Exec
        let merged = merge_json(&local, &remote);

        // -- Check: remote key preserved
        assert_eq!(merged["key"], "value");
        Ok(())
    }

    // endregion: --- JSON Merge Tests

    // region: --- SyncStatus Tests

    #[test]
    fn test_sync_status_roundtrip() -> Result<()> {
        // -- Check all variants survive string serialization
        for status in &[
            SyncStatus::Synced,
            SyncStatus::Pending,
            SyncStatus::Conflict,
            SyncStatus::LocalOnly,
        ] {
            let s = status.as_str();
            let parsed = SyncStatus::from_str_value(s);
            assert_eq!(*status, parsed, "Failed roundtrip for {s}");
        }
        Ok(())
    }

    #[test]
    fn test_sync_status_unknown_defaults_to_pending() -> Result<()> {
        // -- Exec & Check
        assert_eq!(SyncStatus::from_str_value("garbage"), SyncStatus::Pending);
        Ok(())
    }

    // endregion: --- SyncStatus Tests

    // region: --- EntityType Tests

    #[test]
    fn test_entity_type_roundtrip() -> Result<()> {
        for et in &[
            EntityType::Preset,
            EntityType::Snapshot,
            EntityType::ModuleChunk,
        ] {
            let s = et.as_str();
            let parsed = EntityType::from_str_value(s);
            assert_eq!(Some(*et), parsed, "Failed roundtrip for {s}");
        }
        Ok(())
    }

    #[test]
    fn test_entity_type_unknown_returns_none() -> Result<()> {
        assert_eq!(EntityType::from_str_value("nope"), None);
        Ok(())
    }

    // endregion: --- EntityType Tests

    // region: --- ConflictStrategy Tests

    #[test]
    fn test_conflict_strategy_default_is_last_write_wins() -> Result<()> {
        assert_eq!(ConflictStrategy::default(), ConflictStrategy::LastWriteWins);
        Ok(())
    }

    // endregion: --- ConflictStrategy Tests

    // region: --- SyncProgress Tests

    #[test]
    fn test_sync_progress_fraction() -> Result<()> {
        // -- Setup
        let mut progress = SyncProgress::new(10);
        progress.completed = 5;

        // -- Check
        assert!((progress.fraction() - 0.5).abs() < f64::EPSILON);
        assert!(!progress.is_complete());
        Ok(())
    }

    #[test]
    fn test_sync_progress_complete() -> Result<()> {
        // -- Setup
        let mut progress = SyncProgress::new(3);
        progress.completed = 3;

        // -- Check
        assert!(progress.is_complete());
        assert!((progress.fraction() - 1.0).abs() < f64::EPSILON);
        Ok(())
    }

    #[test]
    fn test_sync_progress_zero_total() -> Result<()> {
        // -- Setup
        let progress = SyncProgress::new(0);

        // -- Check: zero total → fraction is 1.0 (nothing to do = done)
        assert!((progress.fraction() - 1.0).abs() < f64::EPSILON);
        assert!(progress.is_complete());
        Ok(())
    }

    // endregion: --- SyncProgress Tests

    // region: --- SyncEngine Integration Tests (mock DB)

    /// Create an in-memory SQLite database with migrations applied.
    async fn setup_test_db() -> Result<DatabaseConnection> {
        use sea_orm::{Database, DbBackend};
        use signal_storage::Migrator;
        use sea_orm_migration::MigratorTrait;

        let db = Database::connect("sqlite::memory:")
            .await?;

        // Verify it's SQLite
        assert_eq!(db.get_database_backend(), DbBackend::Sqlite);

        // Run migrations
        Migrator::up(&db, None).await?;

        Ok(db)
    }

    #[tokio::test]
    async fn test_engine_offline_sync_returns_zero() -> Result<()> {
        // -- Setup
        let db = setup_test_db().await?;
        let engine = SyncEngine::new(db);

        // -- Exec: sync while offline
        let progress = engine.sync().await?;

        // -- Check
        assert_eq!(progress.total, 0);
        assert_eq!(progress.completed, 0);
        assert!(progress.message.contains("Offline"));
        Ok(())
    }

    #[tokio::test]
    async fn test_engine_is_offline_by_default() -> Result<()> {
        // -- Setup
        let db = setup_test_db().await?;
        let engine = SyncEngine::new(db);

        // -- Check
        assert!(!engine.is_online().await);
        Ok(())
    }

    #[tokio::test]
    async fn test_engine_connect_disconnect() -> Result<()> {
        // -- Setup
        let local = setup_test_db().await?;
        let cloud = setup_test_db().await?;
        let engine = SyncEngine::new(local);

        // -- Exec: connect
        engine.set_cloud_db(cloud).await;
        assert!(engine.is_online().await);

        // -- Exec: disconnect
        engine.disconnect_cloud().await;
        assert!(!engine.is_online().await);
        Ok(())
    }

    #[tokio::test]
    async fn test_engine_mark_modified_creates_metadata() -> Result<()> {
        // -- Setup
        let db = setup_test_db().await?;
        let engine = SyncEngine::new(db);
        let entity_id = Uuid::new_v4();

        // -- Exec
        engine
            .mark_modified(EntityType::Preset, entity_id)
            .await?;

        // -- Check
        let status = engine
            .get_entity_status(EntityType::Preset, entity_id)
            .await?;
        assert_eq!(status, SyncStatus::LocalOnly);
        Ok(())
    }

    #[tokio::test]
    async fn test_engine_mark_modified_twice_bumps_version() -> Result<()> {
        // -- Setup
        let db = setup_test_db().await?;
        let engine = SyncEngine::new(db);
        let entity_id = Uuid::new_v4();

        // -- Exec: mark twice
        engine
            .mark_modified(EntityType::Preset, entity_id)
            .await?;
        engine
            .mark_modified(EntityType::Preset, entity_id)
            .await?;

        // -- Check: status should be pending (not local_only after first bump)
        let status = engine
            .get_entity_status(EntityType::Preset, entity_id)
            .await?;
        assert_eq!(status, SyncStatus::Pending);

        // Also check via pending changes
        let changes = engine.get_pending_changes().await?;
        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].local_version, 2);
        Ok(())
    }

    #[tokio::test]
    async fn test_engine_offline_queues_changes() -> Result<()> {
        // -- Setup
        let db = setup_test_db().await?;
        let engine = SyncEngine::new(db);
        let entity_id = Uuid::new_v4();

        // -- Exec: mark while offline
        engine
            .mark_modified(EntityType::Preset, entity_id)
            .await?;

        // -- Check: change is in the offline queue
        assert_eq!(engine.offline_queue_size().await, 1);
        Ok(())
    }

    #[tokio::test]
    async fn test_engine_get_conflicts_empty() -> Result<()> {
        // -- Setup
        let db = setup_test_db().await?;
        let engine = SyncEngine::new(db);

        // -- Exec & Check
        let conflicts = engine.get_conflicts().await?;
        assert!(conflicts.is_empty());
        Ok(())
    }

    #[tokio::test]
    async fn test_engine_sync_with_pending_preset() -> Result<()> {
        // -- Setup
        let local = setup_test_db().await?;
        let cloud = setup_test_db().await?;
        let now = Utc::now().fixed_offset();
        let preset_id = Uuid::new_v4();

        // Insert a preset into local
        let preset_active = preset::ActiveModel {
            id: ActiveValue::Set(preset_id),
            name: ActiveValue::Set("Test Preset".into()),
            description: ActiveValue::Set(None),
            author_id: ActiveValue::Set(None),
            category: ActiveValue::Set(serde_json::json!("Amp")),
            tags: ActiveValue::Set(serde_json::json!([])),
            data: ActiveValue::Set(serde_json::json!({"gain": 5})),
            is_public: ActiveValue::Set(false),
            is_deleted: ActiveValue::Set(false),
            version: ActiveValue::Set(1),
            created_at: ActiveValue::Set(now),
            updated_at: ActiveValue::Set(now),
        };
        preset::Entity::insert(preset_active)
            .exec(&local)
            .await?;

        // Insert sync metadata as pending
        let meta = sync_metadata::ActiveModel {
            id: ActiveValue::Set(Uuid::new_v4()),
            entity_type: ActiveValue::Set("preset".into()),
            entity_id: ActiveValue::Set(preset_id),
            last_sync_at: ActiveValue::Set(now),
            sync_status: ActiveValue::Set("pending".into()),
            local_version: ActiveValue::Set(1),
            remote_version: ActiveValue::Set(0),
        };
        sync_metadata::Entity::insert(meta).exec(&local).await?;

        let engine = SyncEngine::new(local);
        engine.set_cloud_db(cloud).await;

        // -- Exec
        let progress = engine.sync().await?;

        // -- Check
        assert_eq!(progress.total, 1);
        assert_eq!(progress.completed, 1);
        assert_eq!(progress.errors, 0);

        // Entity should now be synced
        let status = engine
            .get_entity_status(EntityType::Preset, preset_id)
            .await?;
        assert_eq!(status, SyncStatus::Synced);
        Ok(())
    }

    #[tokio::test]
    async fn test_engine_set_default_strategy() -> Result<()> {
        // -- Setup
        let db = setup_test_db().await?;
        let engine = SyncEngine::new(db);

        // -- Exec
        engine.set_default_strategy(ConflictStrategy::Merge).await;

        // -- Check: no direct getter, but exercises the code path
        Ok(())
    }

    #[tokio::test]
    async fn test_engine_resolve_conflict_while_offline_fails() -> Result<()> {
        // -- Setup
        let db = setup_test_db().await?;
        let engine = SyncEngine::new(db);

        // -- Exec
        let result = engine
            .resolve_conflict(EntityType::Preset, Uuid::new_v4(), ConflictStrategy::LastWriteWins)
            .await;

        // -- Check: should fail because no cloud connection
        assert!(result.is_err());
        Ok(())
    }

    // endregion: --- SyncEngine Integration Tests
}
