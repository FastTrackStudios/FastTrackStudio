//! Block repository — CRUD operations for block presets and block snapshots.
//!
//! Block presets represent individual DSP units (EQ, Drive, Amp, etc.) with
//! their plugin identity and metadata. Block snapshots capture parameter
//! variations within a preset.

use chrono::Utc;
use sea_orm::*;
use uuid::Uuid;

use crate::entities::{block_preset, block_snapshot};
use crate::error::{StorageError, StorageResult};

// ─────────────────────────────────────────────────────────────────────────────
// Block Preset CRUD
// ─────────────────────────────────────────────────────────────────────────────

/// Create a new block preset.
pub async fn create_block_preset(
    db: &DatabaseConnection,
    name: &str,
    block_type: &str,
    plugin_id: Option<serde_json::Value>,
    plugin_preset_name: Option<&str>,
    description: Option<&str>,
    tags: serde_json::Value,
) -> StorageResult<Uuid> {
    let id = Uuid::new_v4();
    let now = Utc::now().fixed_offset();

    block_preset::ActiveModel {
        id: Set(id),
        name: Set(name.to_string()),
        block_type: Set(block_type.to_string()),
        plugin_id: Set(plugin_id),
        plugin_preset_name: Set(plugin_preset_name.map(String::from)),
        description: Set(description.map(String::from)),
        tags: Set(tags),
        is_template: Set(false),
        is_deleted: Set(false),
        created_at: Set(now),
        updated_at: Set(now),
    }
    .insert(db)
    .await?;

    Ok(id)
}

/// Get a block preset by ID.
pub async fn get_block_preset(
    db: &DatabaseConnection,
    id: Uuid,
) -> StorageResult<Option<block_preset::Model>> {
    Ok(block_preset::Entity::find_by_id(id).one(db).await?)
}

/// List block presets, optionally filtered by block type.
/// Excludes soft-deleted presets.
pub async fn list_block_presets(
    db: &DatabaseConnection,
    block_type_filter: Option<&str>,
) -> StorageResult<Vec<block_preset::Model>> {
    let mut query = block_preset::Entity::find().filter(block_preset::Column::IsDeleted.eq(false));

    if let Some(bt) = block_type_filter {
        query = query.filter(block_preset::Column::BlockType.eq(bt));
    }

    query = query.order_by_asc(block_preset::Column::Name);

    Ok(query.all(db).await?)
}

/// Update a block preset's mutable fields.
pub async fn update_block_preset(
    db: &DatabaseConnection,
    id: Uuid,
    name: Option<&str>,
    description: Option<Option<&str>>,
    plugin_id: Option<Option<serde_json::Value>>,
    plugin_preset_name: Option<Option<&str>>,
    tags: Option<serde_json::Value>,
) -> StorageResult<()> {
    let existing =
        block_preset::Entity::find_by_id(id)
            .one(db)
            .await?
            .ok_or(StorageError::NotFound {
                entity: "block_preset",
                id,
            })?;

    let mut active: block_preset::ActiveModel = existing.into();
    let now = Utc::now().fixed_offset();

    if let Some(n) = name {
        active.name = Set(n.to_string());
    }
    if let Some(d) = description {
        active.description = Set(d.map(String::from));
    }
    if let Some(p) = plugin_id {
        active.plugin_id = Set(p);
    }
    if let Some(p) = plugin_preset_name {
        active.plugin_preset_name = Set(p.map(String::from));
    }
    if let Some(t) = tags {
        active.tags = Set(t);
    }
    active.updated_at = Set(now);

    active.update(db).await?;
    Ok(())
}

/// Soft-delete a block preset (sets is_deleted = true).
pub async fn delete_block_preset(db: &DatabaseConnection, id: Uuid) -> StorageResult<()> {
    let existing =
        block_preset::Entity::find_by_id(id)
            .one(db)
            .await?
            .ok_or(StorageError::NotFound {
                entity: "block_preset",
                id,
            })?;

    let mut active: block_preset::ActiveModel = existing.into();
    active.is_deleted = Set(true);
    active.updated_at = Set(Utc::now().fixed_offset());
    active.update(db).await?;
    Ok(())
}

// ─────────────────────────────────────────────────────────────────────────────
// Block Snapshot CRUD
// ─────────────────────────────────────────────────────────────────────────────

/// Create a new block snapshot for a block preset.
pub async fn create_block_snapshot(
    db: &DatabaseConnection,
    block_preset_id: Uuid,
    name: &str,
    parameters: serde_json::Value,
    daw_chunk_data: Option<&str>,
    is_default: bool,
) -> StorageResult<Uuid> {
    let id = Uuid::new_v4();
    let now = Utc::now().fixed_offset();

    block_snapshot::ActiveModel {
        id: Set(id),
        block_preset_id: Set(block_preset_id),
        name: Set(name.to_string()),
        parameters: Set(parameters),
        daw_chunk_data: Set(daw_chunk_data.map(String::from)),
        is_default: Set(is_default),
        created_at: Set(now),
        updated_at: Set(now),
    }
    .insert(db)
    .await?;

    Ok(id)
}

/// Get a block snapshot by ID.
pub async fn get_block_snapshot(
    db: &DatabaseConnection,
    id: Uuid,
) -> StorageResult<Option<block_snapshot::Model>> {
    Ok(block_snapshot::Entity::find_by_id(id).one(db).await?)
}

/// List block snapshots for a block preset.
pub async fn list_block_snapshots(
    db: &DatabaseConnection,
    block_preset_id: Uuid,
) -> StorageResult<Vec<block_snapshot::Model>> {
    Ok(block_snapshot::Entity::find()
        .filter(block_snapshot::Column::BlockPresetId.eq(block_preset_id))
        .order_by_asc(block_snapshot::Column::CreatedAt)
        .all(db)
        .await?)
}

/// Hard-delete a block snapshot.
pub async fn delete_block_snapshot(db: &DatabaseConnection, id: Uuid) -> StorageResult<bool> {
    let result = block_snapshot::Entity::delete_by_id(id).exec(db).await?;
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

    // -- Block Preset Tests

    #[tokio::test]
    async fn create_and_get_block_preset() -> Result<()> {
        let db = setup_db().await?;
        let plugin = serde_json::json!({"format": "Vst3", "uid": "com.fab.proq4", "display_name": "Pro-Q 4"});

        let id = create_block_preset(
            &db,
            "Main EQ",
            "Eq",
            Some(plugin),
            Some("Default"),
            None,
            serde_json::json!([]),
        )
        .await?;

        let preset = get_block_preset(&db, id).await?.expect("should exist");
        assert_eq!(preset.name, "Main EQ");
        assert_eq!(preset.block_type, "Eq");
        assert!(!preset.is_deleted);
        Ok(())
    }

    #[tokio::test]
    async fn list_block_presets_filters_by_type() -> Result<()> {
        let db = setup_db().await?;
        let tags = serde_json::json!([]);

        create_block_preset(&db, "EQ 1", "Eq", None, None, None, tags.clone()).await?;
        create_block_preset(&db, "EQ 2", "Eq", None, None, None, tags.clone()).await?;
        create_block_preset(&db, "Drive 1", "Drive", None, None, None, tags.clone()).await?;

        let eq_presets = list_block_presets(&db, Some("Eq")).await?;
        assert_eq!(eq_presets.len(), 2);

        let drive_presets = list_block_presets(&db, Some("Drive")).await?;
        assert_eq!(drive_presets.len(), 1);

        let all_presets = list_block_presets(&db, None).await?;
        assert_eq!(all_presets.len(), 3);
        Ok(())
    }

    #[tokio::test]
    async fn list_block_presets_excludes_deleted() -> Result<()> {
        let db = setup_db().await?;
        let tags = serde_json::json!([]);

        let id = create_block_preset(&db, "Deleted", "Eq", None, None, None, tags.clone()).await?;
        create_block_preset(&db, "Active", "Eq", None, None, None, tags).await?;

        delete_block_preset(&db, id).await?;

        let presets = list_block_presets(&db, None).await?;
        assert_eq!(presets.len(), 1);
        assert_eq!(presets[0].name, "Active");
        Ok(())
    }

    #[tokio::test]
    async fn update_block_preset_fields() -> Result<()> {
        let db = setup_db().await?;
        let tags = serde_json::json!([]);

        let id = create_block_preset(&db, "Old Name", "Eq", None, None, None, tags).await?;

        update_block_preset(
            &db,
            id,
            Some("New Name"),
            Some(Some("Updated description")),
            None,
            None,
            None,
        )
        .await?;

        let preset = get_block_preset(&db, id).await?.expect("should exist");
        assert_eq!(preset.name, "New Name");
        assert_eq!(preset.description.as_deref(), Some("Updated description"));
        Ok(())
    }

    #[tokio::test]
    async fn soft_delete_block_preset() -> Result<()> {
        let db = setup_db().await?;
        let tags = serde_json::json!([]);

        let id = create_block_preset(&db, "ToDelete", "Eq", None, None, None, tags).await?;
        delete_block_preset(&db, id).await?;

        let preset = get_block_preset(&db, id).await?.expect("row still exists");
        assert!(preset.is_deleted);
        Ok(())
    }

    // -- Block Snapshot Tests

    #[tokio::test]
    async fn create_and_list_block_snapshots() -> Result<()> {
        let db = setup_db().await?;
        let tags = serde_json::json!([]);
        let preset_id = create_block_preset(&db, "EQ", "Eq", None, None, None, tags).await?;

        let params = serde_json::json!({"gain": 0.5, "freq": 1000});
        create_block_snapshot(&db, preset_id, "Clean", params.clone(), None, true).await?;
        create_block_snapshot(&db, preset_id, "Bright", params, None, false).await?;

        let snapshots = list_block_snapshots(&db, preset_id).await?;
        assert_eq!(snapshots.len(), 2);
        assert_eq!(snapshots[0].name, "Clean");
        assert!(snapshots[0].is_default);
        Ok(())
    }

    #[tokio::test]
    async fn delete_block_snapshot_removes_it() -> Result<()> {
        let db = setup_db().await?;
        let tags = serde_json::json!([]);
        let preset_id = create_block_preset(&db, "EQ", "Eq", None, None, None, tags).await?;

        let params = serde_json::json!({});
        let snap_id = create_block_snapshot(&db, preset_id, "Temp", params, None, false).await?;

        let deleted = delete_block_snapshot(&db, snap_id).await?;
        assert!(deleted);

        let loaded = get_block_snapshot(&db, snap_id).await?;
        assert!(loaded.is_none());
        Ok(())
    }
}
