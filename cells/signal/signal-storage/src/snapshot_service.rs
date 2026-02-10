//! Snapshot service — CRUD operations for DAW parameter and state chunk snapshots.
//!
//! Provides persistence for Snapshooter-style parameter captures and
//! Track Snapshot-style state chunk captures, keyed by track GUID.

use chrono::Utc;
use sea_orm::*;
use uuid::Uuid;

use crate::entities::{daw_parameter_snapshot, daw_state_chunk_snapshot};
use crate::error::StorageResult;

// ─────────────────────────────────────────────────────────────────────────────
// DTOs
// ─────────────────────────────────────────────────────────────────────────────

/// Summary of a snapshot (for list views — avoids loading full data).
#[derive(Debug, Clone)]
pub struct SnapshotSummary {
    pub id: Uuid,
    pub name: String,
    pub created_at: chrono::DateTime<Utc>,
}

// ─────────────────────────────────────────────────────────────────────────────
// Parameter Snapshot CRUD
// ─────────────────────────────────────────────────────────────────────────────

/// Save a parameter snapshot to the database.
///
/// `data` should be the JSON-serialized `DawParameterSnapshot`.
pub async fn save_parameter_snapshot(
    db: &DatabaseConnection,
    track_guid: &str,
    name: &str,
    data: serde_json::Value,
) -> StorageResult<Uuid> {
    let id = Uuid::new_v4();
    let now = Utc::now().fixed_offset();

    daw_parameter_snapshot::ActiveModel {
        id: Set(id),
        track_guid: Set(track_guid.to_string()),
        name: Set(name.to_string()),
        data: Set(data),
        created_at: Set(now),
    }
    .insert(db)
    .await?;

    Ok(id)
}

/// Load a parameter snapshot by ID.
pub async fn load_parameter_snapshot(
    db: &DatabaseConnection,
    id: Uuid,
) -> StorageResult<Option<daw_parameter_snapshot::Model>> {
    Ok(daw_parameter_snapshot::Entity::find_by_id(id)
        .one(db)
        .await?)
}

/// List parameter snapshots for a track (id, name, created_at only).
pub async fn list_parameter_snapshots(
    db: &DatabaseConnection,
    track_guid: &str,
) -> StorageResult<Vec<SnapshotSummary>> {
    let models = daw_parameter_snapshot::Entity::find()
        .filter(daw_parameter_snapshot::Column::TrackGuid.eq(track_guid))
        .order_by_asc(daw_parameter_snapshot::Column::CreatedAt)
        .all(db)
        .await?;

    Ok(models
        .into_iter()
        .map(|m| SnapshotSummary {
            id: m.id,
            name: m.name,
            created_at: m.created_at.into(),
        })
        .collect())
}

/// Delete a parameter snapshot by ID.
pub async fn delete_parameter_snapshot(db: &DatabaseConnection, id: Uuid) -> StorageResult<bool> {
    let result = daw_parameter_snapshot::Entity::delete_by_id(id)
        .exec(db)
        .await?;
    Ok(result.rows_affected > 0)
}

// ─────────────────────────────────────────────────────────────────────────────
// State Chunk Snapshot CRUD
// ─────────────────────────────────────────────────────────────────────────────

/// Save a state chunk snapshot to the database.
///
/// `data` should be the JSON-serialized `DawStateChunkSnapshot`.
pub async fn save_state_chunk_snapshot(
    db: &DatabaseConnection,
    track_guid: &str,
    name: &str,
    data: serde_json::Value,
) -> StorageResult<Uuid> {
    let id = Uuid::new_v4();
    let now = Utc::now().fixed_offset();

    daw_state_chunk_snapshot::ActiveModel {
        id: Set(id),
        track_guid: Set(track_guid.to_string()),
        name: Set(name.to_string()),
        data: Set(data),
        created_at: Set(now),
    }
    .insert(db)
    .await?;

    Ok(id)
}

/// Load a state chunk snapshot by ID.
pub async fn load_state_chunk_snapshot(
    db: &DatabaseConnection,
    id: Uuid,
) -> StorageResult<Option<daw_state_chunk_snapshot::Model>> {
    Ok(daw_state_chunk_snapshot::Entity::find_by_id(id)
        .one(db)
        .await?)
}

/// List state chunk snapshots for a track (id, name, created_at only).
pub async fn list_state_chunk_snapshots(
    db: &DatabaseConnection,
    track_guid: &str,
) -> StorageResult<Vec<SnapshotSummary>> {
    let models = daw_state_chunk_snapshot::Entity::find()
        .filter(daw_state_chunk_snapshot::Column::TrackGuid.eq(track_guid))
        .order_by_asc(daw_state_chunk_snapshot::Column::CreatedAt)
        .all(db)
        .await?;

    Ok(models
        .into_iter()
        .map(|m| SnapshotSummary {
            id: m.id,
            name: m.name,
            created_at: m.created_at.into(),
        })
        .collect())
}

/// Delete a state chunk snapshot by ID.
pub async fn delete_state_chunk_snapshot(db: &DatabaseConnection, id: Uuid) -> StorageResult<bool> {
    let result = daw_state_chunk_snapshot::Entity::delete_by_id(id)
        .exec(db)
        .await?;
    Ok(result.rows_affected > 0)
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::migration::Migrator;
    use sea_orm_migration::MigratorTrait;

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    async fn setup_db() -> Result<DatabaseConnection> {
        let db = Database::connect("sqlite::memory:").await?;
        Migrator::up(&db, None).await?;
        Ok(db)
    }

    // -- Parameter snapshots

    #[tokio::test]
    async fn save_and_load_parameter_snapshot() -> Result<()> {
        let db = setup_db().await?;
        let data = serde_json::json!({
            "name": "Clean",
            "fx_states": [{
                "fx_guid": "{ABC-123}",
                "fx_index": 0,
                "plugin_name": "ReaEQ",
                "enabled": true,
                "parameters": [
                    {"param_index": 0, "param_name": "Band 1 Gain", "value": 0.5}
                ]
            }]
        });

        let id = save_parameter_snapshot(&db, "track-guid-1", "Clean", data.clone()).await?;
        let loaded = load_parameter_snapshot(&db, id).await?;

        assert!(loaded.is_some());
        let model = loaded.unwrap();
        assert_eq!(model.name, "Clean");
        assert_eq!(model.track_guid, "track-guid-1");
        assert_eq!(model.data, data);
        Ok(())
    }

    #[tokio::test]
    async fn list_parameter_snapshots_filters_by_track() -> Result<()> {
        let db = setup_db().await?;
        let data = serde_json::json!({"name": "test"});

        save_parameter_snapshot(&db, "track-1", "Snap A", data.clone()).await?;
        save_parameter_snapshot(&db, "track-1", "Snap B", data.clone()).await?;
        save_parameter_snapshot(&db, "track-2", "Snap C", data.clone()).await?;

        let list = list_parameter_snapshots(&db, "track-1").await?;
        assert_eq!(list.len(), 2);
        assert_eq!(list[0].name, "Snap A");
        assert_eq!(list[1].name, "Snap B");

        let list2 = list_parameter_snapshots(&db, "track-2").await?;
        assert_eq!(list2.len(), 1);
        Ok(())
    }

    #[tokio::test]
    async fn delete_parameter_snapshot_removes_it() -> Result<()> {
        let db = setup_db().await?;
        let data = serde_json::json!({"name": "test"});
        let id = save_parameter_snapshot(&db, "track-1", "Temp", data).await?;

        let deleted = delete_parameter_snapshot(&db, id).await?;
        assert!(deleted);

        let loaded = load_parameter_snapshot(&db, id).await?;
        assert!(loaded.is_none());
        Ok(())
    }

    #[tokio::test]
    async fn delete_parameter_snapshot_returns_false_for_missing() -> Result<()> {
        let db = setup_db().await?;
        let deleted = delete_parameter_snapshot(&db, Uuid::new_v4()).await?;
        assert!(!deleted);
        Ok(())
    }

    // -- State chunk snapshots

    #[tokio::test]
    async fn save_and_load_state_chunk_snapshot() -> Result<()> {
        let db = setup_db().await?;
        let data = serde_json::json!({
            "name": "Full State",
            "chunks": [{
                "fx_guid": "{ABC-123}",
                "fx_index": 0,
                "plugin_name": "NeuralAmpModeler",
                "encoded_chunk": "base64encodeddata=="
            }]
        });

        let id = save_state_chunk_snapshot(&db, "track-guid-1", "Full State", data.clone()).await?;
        let loaded = load_state_chunk_snapshot(&db, id).await?;

        assert!(loaded.is_some());
        let model = loaded.unwrap();
        assert_eq!(model.name, "Full State");
        assert_eq!(model.data, data);
        Ok(())
    }

    #[tokio::test]
    async fn list_state_chunk_snapshots_filters_by_track() -> Result<()> {
        let db = setup_db().await?;
        let data = serde_json::json!({"name": "test"});

        save_state_chunk_snapshot(&db, "track-1", "Preset A", data.clone()).await?;
        save_state_chunk_snapshot(&db, "track-1", "Preset B", data.clone()).await?;
        save_state_chunk_snapshot(&db, "track-2", "Preset C", data.clone()).await?;

        let list = list_state_chunk_snapshots(&db, "track-1").await?;
        assert_eq!(list.len(), 2);

        let list2 = list_state_chunk_snapshots(&db, "track-2").await?;
        assert_eq!(list2.len(), 1);
        Ok(())
    }

    #[tokio::test]
    async fn delete_state_chunk_snapshot_removes_it() -> Result<()> {
        let db = setup_db().await?;
        let data = serde_json::json!({"name": "test"});
        let id = save_state_chunk_snapshot(&db, "track-1", "Temp", data).await?;

        let deleted = delete_state_chunk_snapshot(&db, id).await?;
        assert!(deleted);

        let loaded = load_state_chunk_snapshot(&db, id).await?;
        assert!(loaded.is_none());
        Ok(())
    }
}
