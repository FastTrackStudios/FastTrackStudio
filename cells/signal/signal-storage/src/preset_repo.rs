//! Preset repository — CRUD operations for rig presets and their snapshots.
//!
//! The existing `presets` table stores the full Preset domain type as a JSON blob
//! in the `data` column. This repo provides typed access with Facet serialization,
//! plus lightweight `PresetInfo` summaries for browser views.

use chrono::Utc;
use sea_orm::*;
use uuid::Uuid;

use crate::entities::{preset, snapshot};
use crate::error::{StorageError, StorageResult};
use crate::facet_bridge;

// ─────────────────────────────────────────────────────────────────────────────
// Preset CRUD
// ─────────────────────────────────────────────────────────────────────────────

/// Create a new preset, storing the full domain type as JSON in the data column.
pub async fn create_preset<T: for<'a> facet::Facet<'a>>(
    db: &DatabaseConnection,
    name: &str,
    description: Option<&str>,
    category: serde_json::Value,
    tags: serde_json::Value,
    data: &T,
) -> StorageResult<Uuid> {
    let id = Uuid::new_v4();
    let now = Utc::now().fixed_offset();
    let data_json = facet_bridge::to_json_value(data)?;

    preset::ActiveModel {
        id: Set(id),
        name: Set(name.to_string()),
        description: Set(description.map(String::from)),
        author_id: Set(None),
        category: Set(category),
        tags: Set(tags),
        data: Set(data_json),
        is_public: Set(false),
        instrument_type: Set("guitar".to_string()),
        is_deleted: Set(false),
        is_favorite: Set(false),
        is_template: Set(false),
        version: Set(1),
        created_at: Set(now),
        updated_at: Set(now),
    }
    .insert(db)
    .await?;

    Ok(id)
}

/// Get a preset row by ID (returns the raw SeaORM model).
pub async fn get_preset_row(
    db: &DatabaseConnection,
    id: Uuid,
) -> StorageResult<Option<preset::Model>> {
    Ok(preset::Entity::find_by_id(id).one(db).await?)
}

/// Get a preset by ID, deserializing the `data` column into the given Facet type.
pub async fn get_preset<T: for<'a> facet::Facet<'a>>(
    db: &DatabaseConnection,
    id: Uuid,
) -> StorageResult<Option<T>> {
    let model = preset::Entity::find_by_id(id).one(db).await?;
    match model {
        Some(m) => {
            let data: T = facet_bridge::from_json_value(&m.data)?;
            Ok(Some(data))
        }
        None => Ok(None),
    }
}

/// List preset summaries (lightweight — no data column).
/// Excludes soft-deleted presets by default.
pub async fn list_presets(
    db: &DatabaseConnection,
    exclude_deleted: bool,
) -> StorageResult<Vec<preset::Model>> {
    let mut query = preset::Entity::find();

    if exclude_deleted {
        query = query.filter(preset::Column::IsDeleted.eq(false));
    }

    query = query.order_by_desc(preset::Column::UpdatedAt);

    Ok(query.all(db).await?)
}

/// List presets filtered by instrument type (e.g. "guitar", "bass").
pub async fn list_presets_by_type(
    db: &DatabaseConnection,
    instrument_type: &str,
) -> StorageResult<Vec<preset::Model>> {
    Ok(preset::Entity::find()
        .filter(preset::Column::IsDeleted.eq(false))
        .filter(preset::Column::InstrumentType.eq(instrument_type))
        .order_by_desc(preset::Column::UpdatedAt)
        .all(db)
        .await?)
}

/// Update the full preset data (the JSON blob in the data column).
pub async fn update_preset_data<T: for<'a> facet::Facet<'a>>(
    db: &DatabaseConnection,
    id: Uuid,
    data: &T,
) -> StorageResult<()> {
    let existing = preset::Entity::find_by_id(id)
        .one(db)
        .await?
        .ok_or(StorageError::NotFound {
            entity: "preset",
            id,
        })?;

    let data_json = facet_bridge::to_json_value(data)?;
    let mut active: preset::ActiveModel = existing.into();
    active.data = Set(data_json);
    active.version = Set(active.version.unwrap() + 1);
    active.updated_at = Set(Utc::now().fixed_offset());
    active.update(db).await?;
    Ok(())
}

/// Update preset metadata (name, description, category, tags) without touching the data blob.
pub async fn update_preset_metadata(
    db: &DatabaseConnection,
    id: Uuid,
    name: Option<&str>,
    description: Option<Option<&str>>,
    category: Option<serde_json::Value>,
    tags: Option<serde_json::Value>,
    is_favorite: Option<bool>,
) -> StorageResult<()> {
    let existing = preset::Entity::find_by_id(id)
        .one(db)
        .await?
        .ok_or(StorageError::NotFound {
            entity: "preset",
            id,
        })?;

    let mut active: preset::ActiveModel = existing.into();
    let now = Utc::now().fixed_offset();

    if let Some(n) = name {
        active.name = Set(n.to_string());
    }
    if let Some(d) = description {
        active.description = Set(d.map(String::from));
    }
    if let Some(c) = category {
        active.category = Set(c);
    }
    if let Some(t) = tags {
        active.tags = Set(t);
    }
    if let Some(f) = is_favorite {
        active.is_favorite = Set(f);
    }
    active.updated_at = Set(now);

    active.update(db).await?;
    Ok(())
}

/// Soft-delete a preset (sets is_deleted = true).
pub async fn delete_preset(db: &DatabaseConnection, id: Uuid) -> StorageResult<()> {
    let existing = preset::Entity::find_by_id(id)
        .one(db)
        .await?
        .ok_or(StorageError::NotFound {
            entity: "preset",
            id,
        })?;

    let mut active: preset::ActiveModel = existing.into();
    active.is_deleted = Set(true);
    active.updated_at = Set(Utc::now().fixed_offset());
    active.update(db).await?;
    Ok(())
}

// ─────────────────────────────────────────────────────────────────────────────
// Snapshot CRUD (preset-level snapshots stored in the `snapshots` table)
// ─────────────────────────────────────────────────────────────────────────────

/// Save a snapshot for a preset, storing the domain type as JSON via Facet.
pub async fn save_preset_snapshot<T: for<'a> facet::Facet<'a>>(
    db: &DatabaseConnection,
    preset_id: Uuid,
    name: &str,
    snapshot_data: &T,
    is_default: bool,
) -> StorageResult<Uuid> {
    let id = Uuid::new_v4();
    let now = Utc::now().fixed_offset();
    let data_json = facet_bridge::to_json_value(snapshot_data)?;

    snapshot::ActiveModel {
        id: Set(id),
        preset_id: Set(preset_id),
        name: Set(name.to_string()),
        data: Set(data_json),
        is_default: Set(is_default),
        created_at: Set(now),
        updated_at: Set(now),
    }
    .insert(db)
    .await?;

    Ok(id)
}

/// Get a snapshot by ID, deserializing into the given Facet type.
pub async fn get_preset_snapshot<T: for<'a> facet::Facet<'a>>(
    db: &DatabaseConnection,
    id: Uuid,
) -> StorageResult<Option<T>> {
    let model = snapshot::Entity::find_by_id(id).one(db).await?;
    match model {
        Some(m) => {
            let data: T = facet_bridge::from_json_value(&m.data)?;
            Ok(Some(data))
        }
        None => Ok(None),
    }
}

/// List snapshots for a preset (returns raw models — caller deserializes as needed).
pub async fn list_preset_snapshots(
    db: &DatabaseConnection,
    preset_id: Uuid,
) -> StorageResult<Vec<snapshot::Model>> {
    Ok(snapshot::Entity::find()
        .filter(snapshot::Column::PresetId.eq(preset_id))
        .order_by_asc(snapshot::Column::CreatedAt)
        .all(db)
        .await?)
}

/// Update a snapshot's data blob.
pub async fn update_preset_snapshot<T: for<'a> facet::Facet<'a>>(
    db: &DatabaseConnection,
    snapshot_id: Uuid,
    snapshot_data: &T,
) -> StorageResult<()> {
    let existing = snapshot::Entity::find_by_id(snapshot_id)
        .one(db)
        .await?
        .ok_or(StorageError::NotFound {
            entity: "snapshot",
            id: snapshot_id,
        })?;

    let data_json = facet_bridge::to_json_value(snapshot_data)?;
    let mut active: snapshot::ActiveModel = existing.into();
    active.data = Set(data_json);
    active.updated_at = Set(Utc::now().fixed_offset());
    active.update(db).await?;
    Ok(())
}

/// Delete a snapshot by ID.
pub async fn delete_preset_snapshot(db: &DatabaseConnection, id: Uuid) -> StorageResult<bool> {
    let result = snapshot::Entity::delete_by_id(id).exec(db).await?;
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

    /// A minimal Facet type for testing serialization roundtrips.
    #[derive(Debug, Clone, PartialEq, facet::Facet)]
    struct TestPresetData {
        name: String,
        gain: f64,
        modules: Vec<String>,
    }

    #[derive(Debug, Clone, PartialEq, facet::Facet)]
    struct TestSnapshotData {
        scene: String,
        volume: f64,
    }

    // -- Preset Tests

    #[tokio::test]
    async fn create_and_get_preset_with_facet() -> Result<()> {
        let db = setup_db().await?;
        let data = TestPresetData {
            name: "Blues Lead".into(),
            gain: 0.7,
            modules: vec!["Drive".into(), "Amp".into()],
        };

        let id = create_preset(
            &db,
            "Blues Lead",
            Some("Warm bluesy lead"),
            serde_json::json!({"base_tone": "Lead"}),
            serde_json::json!(["blues", "lead"]),
            &data,
        )
        .await?;

        let loaded: TestPresetData = get_preset(&db, id).await?.expect("should exist");
        assert_eq!(loaded, data);
        Ok(())
    }

    #[tokio::test]
    async fn list_presets_excludes_deleted() -> Result<()> {
        let db = setup_db().await?;
        let data = TestPresetData {
            name: "Test".into(),
            gain: 0.5,
            modules: vec![],
        };
        let cat = serde_json::json!({});
        let tags = serde_json::json!([]);

        let id1 = create_preset(&db, "Active", None, cat.clone(), tags.clone(), &data).await?;
        let id2 = create_preset(&db, "Deleted", None, cat, tags, &data).await?;
        delete_preset(&db, id2).await?;

        let list = list_presets(&db, true).await?;
        assert_eq!(list.len(), 1);
        assert_eq!(list[0].id, id1);
        Ok(())
    }

    #[tokio::test]
    async fn update_preset_data_increments_version() -> Result<()> {
        let db = setup_db().await?;
        let data = TestPresetData {
            name: "V1".into(),
            gain: 0.5,
            modules: vec![],
        };
        let cat = serde_json::json!({});
        let tags = serde_json::json!([]);

        let id = create_preset(&db, "Test", None, cat, tags, &data).await?;

        let updated_data = TestPresetData {
            name: "V2".into(),
            gain: 0.9,
            modules: vec!["Amp".into()],
        };
        update_preset_data(&db, id, &updated_data).await?;

        let row = get_preset_row(&db, id).await?.expect("should exist");
        assert_eq!(row.version, 2);

        let loaded: TestPresetData = get_preset(&db, id).await?.expect("should exist");
        assert_eq!(loaded, updated_data);
        Ok(())
    }

    #[tokio::test]
    async fn update_preset_metadata_only() -> Result<()> {
        let db = setup_db().await?;
        let data = TestPresetData {
            name: "Test".into(),
            gain: 0.5,
            modules: vec![],
        };
        let cat = serde_json::json!({});
        let tags = serde_json::json!([]);

        let id = create_preset(&db, "Old Name", None, cat, tags, &data).await?;

        update_preset_metadata(
            &db,
            id,
            Some("New Name"),
            Some(Some("Added description")),
            None,
            None,
            Some(true),
        )
        .await?;

        let row = get_preset_row(&db, id).await?.expect("should exist");
        assert_eq!(row.name, "New Name");
        assert_eq!(row.description.as_deref(), Some("Added description"));
        assert!(row.is_favorite);
        Ok(())
    }

    // -- Snapshot Tests

    #[tokio::test]
    async fn save_and_get_preset_snapshot_with_facet() -> Result<()> {
        let db = setup_db().await?;
        let data = TestPresetData {
            name: "P".into(),
            gain: 0.5,
            modules: vec![],
        };
        let cat = serde_json::json!({});
        let tags = serde_json::json!([]);
        let preset_id = create_preset(&db, "P", None, cat, tags, &data).await?;

        let snap_data = TestSnapshotData {
            scene: "Verse".into(),
            volume: 0.8,
        };
        let snap_id = save_preset_snapshot(&db, preset_id, "Verse", &snap_data, false).await?;

        let loaded: TestSnapshotData = get_preset_snapshot(&db, snap_id)
            .await?
            .expect("should exist");
        assert_eq!(loaded, snap_data);
        Ok(())
    }

    #[tokio::test]
    async fn list_preset_snapshots_for_preset() -> Result<()> {
        let db = setup_db().await?;
        let data = TestPresetData {
            name: "P".into(),
            gain: 0.5,
            modules: vec![],
        };
        let cat = serde_json::json!({});
        let tags = serde_json::json!([]);
        let preset_id = create_preset(&db, "P", None, cat, tags, &data).await?;

        let snap = TestSnapshotData {
            scene: "S".into(),
            volume: 0.5,
        };
        save_preset_snapshot(&db, preset_id, "Snap A", &snap, true).await?;
        save_preset_snapshot(&db, preset_id, "Snap B", &snap, false).await?;

        let list = list_preset_snapshots(&db, preset_id).await?;
        assert_eq!(list.len(), 2);
        assert_eq!(list[0].name, "Snap A");
        Ok(())
    }

    #[tokio::test]
    async fn update_preset_snapshot_data() -> Result<()> {
        let db = setup_db().await?;
        let data = TestPresetData {
            name: "P".into(),
            gain: 0.5,
            modules: vec![],
        };
        let cat = serde_json::json!({});
        let tags = serde_json::json!([]);
        let preset_id = create_preset(&db, "P", None, cat, tags, &data).await?;

        let snap = TestSnapshotData {
            scene: "Verse".into(),
            volume: 0.8,
        };
        let snap_id = save_preset_snapshot(&db, preset_id, "Verse", &snap, false).await?;

        let updated = TestSnapshotData {
            scene: "Chorus".into(),
            volume: 1.0,
        };
        update_preset_snapshot(&db, snap_id, &updated).await?;

        let loaded: TestSnapshotData = get_preset_snapshot(&db, snap_id)
            .await?
            .expect("should exist");
        assert_eq!(loaded, updated);
        Ok(())
    }

    #[tokio::test]
    async fn delete_preset_snapshot_removes_it() -> Result<()> {
        let db = setup_db().await?;
        let data = TestPresetData {
            name: "P".into(),
            gain: 0.5,
            modules: vec![],
        };
        let cat = serde_json::json!({});
        let tags = serde_json::json!([]);
        let preset_id = create_preset(&db, "P", None, cat, tags, &data).await?;

        let snap = TestSnapshotData {
            scene: "Temp".into(),
            volume: 0.5,
        };
        let snap_id = save_preset_snapshot(&db, preset_id, "Temp", &snap, false).await?;

        let deleted = delete_preset_snapshot(&db, snap_id).await?;
        assert!(deleted);

        let loaded: Option<TestSnapshotData> = get_preset_snapshot(&db, snap_id).await?;
        assert!(loaded.is_none());
        Ok(())
    }
}
