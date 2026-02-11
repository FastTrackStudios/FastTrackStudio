//! Module repository — CRUD operations for module presets and module snapshots.
//!
//! Module presets represent composable block groups (e.g., Drive Module = Boost + Drive1 + Drive2).
//! Module snapshots capture parameter variations across a module's blocks.

use chrono::Utc;
use sea_orm::*;
use uuid::Uuid;

use crate::entities::{module_preset_entity, module_snapshot};
use crate::error::{StorageError, StorageResult};

// ─────────────────────────────────────────────────────────────────────────────
// Module Preset CRUD
// ─────────────────────────────────────────────────────────────────────────────

/// Create a new module preset.
pub async fn create_module_preset(
    db: &DatabaseConnection,
    name: &str,
    module_type: &str,
    description: Option<&str>,
    blocks: serde_json::Value,
    macros: serde_json::Value,
    tags: serde_json::Value,
) -> StorageResult<Uuid> {
    create_module_preset_with_id(
        db,
        Uuid::new_v4(),
        name,
        module_type,
        description,
        blocks,
        macros,
        tags,
    )
    .await
}

/// Create a module preset with a caller-specified ID.
///
/// Use this when the ID must match a foreign reference (e.g. a `ModulePresetId`
/// in a `Preset.module_assignments` list created by template instantiation).
pub async fn create_module_preset_with_id(
    db: &DatabaseConnection,
    id: Uuid,
    name: &str,
    module_type: &str,
    description: Option<&str>,
    blocks: serde_json::Value,
    macros: serde_json::Value,
    tags: serde_json::Value,
) -> StorageResult<Uuid> {
    let now = Utc::now().fixed_offset();

    module_preset_entity::ActiveModel {
        id: Set(id),
        name: Set(name.to_string()),
        module_type: Set(module_type.to_string()),
        description: Set(description.map(String::from)),
        blocks: Set(blocks),
        macros: Set(macros),
        tags: Set(tags),
        is_deleted: Set(false),
        created_at: Set(now),
        updated_at: Set(now),
    }
    .insert(db)
    .await?;

    Ok(id)
}

/// Get a module preset by ID.
pub async fn get_module_preset(
    db: &DatabaseConnection,
    id: Uuid,
) -> StorageResult<Option<module_preset_entity::Model>> {
    Ok(module_preset_entity::Entity::find_by_id(id).one(db).await?)
}

/// List module presets, optionally filtered by module type.
/// Excludes soft-deleted presets.
pub async fn list_module_presets(
    db: &DatabaseConnection,
    module_type_filter: Option<&str>,
) -> StorageResult<Vec<module_preset_entity::Model>> {
    let mut query = module_preset_entity::Entity::find()
        .filter(module_preset_entity::Column::IsDeleted.eq(false));

    if let Some(mt) = module_type_filter {
        query = query.filter(module_preset_entity::Column::ModuleType.eq(mt));
    }

    query = query.order_by_asc(module_preset_entity::Column::Name);

    Ok(query.all(db).await?)
}

/// Update a module preset's mutable fields.
pub async fn update_module_preset(
    db: &DatabaseConnection,
    id: Uuid,
    name: Option<&str>,
    description: Option<Option<&str>>,
    blocks: Option<serde_json::Value>,
    macros: Option<serde_json::Value>,
    tags: Option<serde_json::Value>,
) -> StorageResult<()> {
    let existing = module_preset_entity::Entity::find_by_id(id)
        .one(db)
        .await?
        .ok_or(StorageError::NotFound {
            entity: "module_preset",
            id,
        })?;

    let mut active: module_preset_entity::ActiveModel = existing.into();
    let now = Utc::now().fixed_offset();

    if let Some(n) = name {
        active.name = Set(n.to_string());
    }
    if let Some(d) = description {
        active.description = Set(d.map(String::from));
    }
    if let Some(b) = blocks {
        active.blocks = Set(b);
    }
    if let Some(m) = macros {
        active.macros = Set(m);
    }
    if let Some(t) = tags {
        active.tags = Set(t);
    }
    active.updated_at = Set(now);

    active.update(db).await?;
    Ok(())
}

/// Soft-delete a module preset (sets is_deleted = true).
pub async fn delete_module_preset(db: &DatabaseConnection, id: Uuid) -> StorageResult<()> {
    let existing = module_preset_entity::Entity::find_by_id(id)
        .one(db)
        .await?
        .ok_or(StorageError::NotFound {
            entity: "module_preset",
            id,
        })?;

    let mut active: module_preset_entity::ActiveModel = existing.into();
    active.is_deleted = Set(true);
    active.updated_at = Set(Utc::now().fixed_offset());
    active.update(db).await?;
    Ok(())
}

// ─────────────────────────────────────────────────────────────────────────────
// Module Snapshot CRUD
// ─────────────────────────────────────────────────────────────────────────────

/// Create a new module snapshot for a module preset.
pub async fn create_module_snapshot(
    db: &DatabaseConnection,
    module_preset_id: Uuid,
    name: &str,
    block_overrides: serde_json::Value,
    is_default: bool,
    tags: serde_json::Value,
) -> StorageResult<Uuid> {
    let id = Uuid::new_v4();
    let now = Utc::now().fixed_offset();

    module_snapshot::ActiveModel {
        id: Set(id),
        module_preset_id: Set(module_preset_id),
        name: Set(name.to_string()),
        block_overrides: Set(block_overrides),
        is_default: Set(is_default),
        tags: Set(tags),
        created_at: Set(now),
        updated_at: Set(now),
    }
    .insert(db)
    .await?;

    Ok(id)
}

/// Get a module snapshot by ID.
pub async fn get_module_snapshot(
    db: &DatabaseConnection,
    id: Uuid,
) -> StorageResult<Option<module_snapshot::Model>> {
    Ok(module_snapshot::Entity::find_by_id(id).one(db).await?)
}

/// List module snapshots for a module preset.
pub async fn list_module_snapshots(
    db: &DatabaseConnection,
    module_preset_id: Uuid,
) -> StorageResult<Vec<module_snapshot::Model>> {
    Ok(module_snapshot::Entity::find()
        .filter(module_snapshot::Column::ModulePresetId.eq(module_preset_id))
        .order_by_asc(module_snapshot::Column::CreatedAt)
        .all(db)
        .await?)
}

/// Hard-delete a module snapshot.
pub async fn delete_module_snapshot(db: &DatabaseConnection, id: Uuid) -> StorageResult<bool> {
    let result = module_snapshot::Entity::delete_by_id(id).exec(db).await?;
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

    // -- Module Preset Tests

    #[tokio::test]
    async fn create_and_get_module_preset() -> Result<()> {
        let db = setup_db().await?;
        let blocks = serde_json::json!([{"name": "Boost", "type": "Drive"}]);
        let macros = serde_json::json!([]);
        let tags = serde_json::json!([]);

        let id = create_module_preset(
            &db,
            "Blues Stack",
            "Drive",
            Some("Classic blues drive tones"),
            blocks,
            macros,
            tags,
        )
        .await?;

        let preset = get_module_preset(&db, id).await?.expect("should exist");
        assert_eq!(preset.name, "Blues Stack");
        assert_eq!(preset.module_type, "Drive");
        assert_eq!(
            preset.description.as_deref(),
            Some("Classic blues drive tones")
        );
        Ok(())
    }

    #[tokio::test]
    async fn list_module_presets_filters_by_type() -> Result<()> {
        let db = setup_db().await?;
        let empty = serde_json::json!([]);

        create_module_preset(
            &db,
            "Drive A",
            "Drive",
            None,
            empty.clone(),
            empty.clone(),
            empty.clone(),
        )
        .await?;
        create_module_preset(
            &db,
            "Drive B",
            "Drive",
            None,
            empty.clone(),
            empty.clone(),
            empty.clone(),
        )
        .await?;
        create_module_preset(
            &db,
            "Amp A",
            "Amp",
            None,
            empty.clone(),
            empty.clone(),
            empty.clone(),
        )
        .await?;

        let drives = list_module_presets(&db, Some("Drive")).await?;
        assert_eq!(drives.len(), 2);

        let amps = list_module_presets(&db, Some("Amp")).await?;
        assert_eq!(amps.len(), 1);

        let all = list_module_presets(&db, None).await?;
        assert_eq!(all.len(), 3);
        Ok(())
    }

    #[tokio::test]
    async fn update_module_preset_fields() -> Result<()> {
        let db = setup_db().await?;
        let empty = serde_json::json!([]);

        let id = create_module_preset(
            &db,
            "Old",
            "Drive",
            None,
            empty.clone(),
            empty.clone(),
            empty,
        )
        .await?;

        update_module_preset(
            &db,
            id,
            Some("Renamed"),
            Some(Some("New desc")),
            None,
            None,
            None,
        )
        .await?;

        let preset = get_module_preset(&db, id).await?.expect("should exist");
        assert_eq!(preset.name, "Renamed");
        assert_eq!(preset.description.as_deref(), Some("New desc"));
        Ok(())
    }

    #[tokio::test]
    async fn soft_delete_module_preset() -> Result<()> {
        let db = setup_db().await?;
        let empty = serde_json::json!([]);

        let id = create_module_preset(
            &db,
            "ToDelete",
            "Eq",
            None,
            empty.clone(),
            empty.clone(),
            empty,
        )
        .await?;
        delete_module_preset(&db, id).await?;

        let preset = get_module_preset(&db, id).await?.expect("row still exists");
        assert!(preset.is_deleted);

        let listed = list_module_presets(&db, None).await?;
        assert!(listed.is_empty());
        Ok(())
    }

    // -- Module Snapshot Tests

    #[tokio::test]
    async fn create_and_list_module_snapshots() -> Result<()> {
        let db = setup_db().await?;
        let empty = serde_json::json!([]);

        let preset_id = create_module_preset(
            &db,
            "Drive",
            "Drive",
            None,
            empty.clone(),
            empty.clone(),
            empty.clone(),
        )
        .await?;

        let overrides = serde_json::json!({"block_1": {"gain": 0.7}});
        create_module_snapshot(
            &db,
            preset_id,
            "Halfman",
            overrides.clone(),
            true,
            empty.clone(),
        )
        .await?;
        create_module_snapshot(&db, preset_id, "Halfman + Teal", overrides, false, empty).await?;

        let snapshots = list_module_snapshots(&db, preset_id).await?;
        assert_eq!(snapshots.len(), 2);
        assert_eq!(snapshots[0].name, "Halfman");
        assert!(snapshots[0].is_default);
        Ok(())
    }

    #[tokio::test]
    async fn delete_module_snapshot_removes_it() -> Result<()> {
        let db = setup_db().await?;
        let empty = serde_json::json!([]);

        let preset_id = create_module_preset(
            &db,
            "Amp",
            "Amp",
            None,
            empty.clone(),
            empty.clone(),
            empty.clone(),
        )
        .await?;
        let snap_id =
            create_module_snapshot(&db, preset_id, "Temp", empty.clone(), false, empty).await?;

        let deleted = delete_module_snapshot(&db, snap_id).await?;
        assert!(deleted);

        let loaded = get_module_snapshot(&db, snap_id).await?;
        assert!(loaded.is_none());
        Ok(())
    }
}
