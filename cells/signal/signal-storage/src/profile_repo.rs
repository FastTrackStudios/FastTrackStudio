//! Profile repository — CRUD operations for profiles and scene templates.
//!
//! Profiles group scene templates associated with a rig. Scene templates
//! define starting points for scenes: which preset, which snapshot, and
//! saved parameter tweaks.

use chrono::Utc;
use sea_orm::*;
use uuid::Uuid;

use crate::entities::{profile, scene_template};
use crate::error::{StorageError, StorageResult};

// ─────────────────────────────────────────────────────────────────────────────
// Profile CRUD
// ─────────────────────────────────────────────────────────────────────────────

/// Create a new profile.
pub async fn create_profile(
    db: &DatabaseConnection,
    name: &str,
    rig_id: Uuid,
    description: Option<&str>,
    tags: serde_json::Value,
    metadata: serde_json::Value,
) -> StorageResult<Uuid> {
    let id = Uuid::new_v4();
    let now = Utc::now().fixed_offset();

    profile::ActiveModel {
        id: Set(id),
        name: Set(name.to_string()),
        rig_id: Set(rig_id),
        description: Set(description.map(String::from)),
        tags: Set(tags),
        metadata: Set(metadata),
        default_scene_template_id: Set(None),
        is_template: Set(false),
        is_deleted: Set(false),
        created_at: Set(now),
        updated_at: Set(now),
    }
    .insert(db)
    .await?;

    Ok(id)
}

/// Get a profile by ID.
pub async fn get_profile(
    db: &DatabaseConnection,
    id: Uuid,
) -> StorageResult<Option<profile::Model>> {
    Ok(profile::Entity::find_by_id(id).one(db).await?)
}

/// List all profiles (excludes soft-deleted).
pub async fn list_profiles(db: &DatabaseConnection) -> StorageResult<Vec<profile::Model>> {
    Ok(profile::Entity::find()
        .filter(profile::Column::IsDeleted.eq(false))
        .order_by_asc(profile::Column::Name)
        .all(db)
        .await?)
}

/// Update a profile's mutable fields.
pub async fn update_profile(
    db: &DatabaseConnection,
    id: Uuid,
    name: Option<&str>,
    description: Option<Option<&str>>,
    tags: Option<serde_json::Value>,
    metadata: Option<serde_json::Value>,
    default_scene_template_id: Option<Option<Uuid>>,
) -> StorageResult<()> {
    let existing =
        profile::Entity::find_by_id(id)
            .one(db)
            .await?
            .ok_or(StorageError::NotFound {
                entity: "profile",
                id,
            })?;

    let mut active: profile::ActiveModel = existing.into();
    let now = Utc::now().fixed_offset();

    if let Some(n) = name {
        active.name = Set(n.to_string());
    }
    if let Some(d) = description {
        active.description = Set(d.map(String::from));
    }
    if let Some(t) = tags {
        active.tags = Set(t);
    }
    if let Some(m) = metadata {
        active.metadata = Set(m);
    }
    if let Some(d) = default_scene_template_id {
        active.default_scene_template_id = Set(d);
    }
    active.updated_at = Set(now);

    active.update(db).await?;
    Ok(())
}

/// Soft-delete a profile.
pub async fn delete_profile(db: &DatabaseConnection, id: Uuid) -> StorageResult<()> {
    let existing =
        profile::Entity::find_by_id(id)
            .one(db)
            .await?
            .ok_or(StorageError::NotFound {
                entity: "profile",
                id,
            })?;

    let mut active: profile::ActiveModel = existing.into();
    active.is_deleted = Set(true);
    active.updated_at = Set(Utc::now().fixed_offset());
    active.update(db).await?;
    Ok(())
}

// ─────────────────────────────────────────────────────────────────────────────
// Scene Template CRUD
// ─────────────────────────────────────────────────────────────────────────────

/// Add a scene template to a profile.
pub async fn add_scene_template(
    db: &DatabaseConnection,
    profile_id: Uuid,
    name: &str,
    preset_id: Uuid,
    snapshot_id: Option<Uuid>,
    module_overrides: serde_json::Value,
    block_overrides: serde_json::Value,
    parameter_state: serde_json::Value,
    sort_order: i32,
    tags: serde_json::Value,
) -> StorageResult<Uuid> {
    let id = Uuid::new_v4();
    let now = Utc::now().fixed_offset();

    scene_template::ActiveModel {
        id: Set(id),
        profile_id: Set(profile_id),
        name: Set(name.to_string()),
        preset_id: Set(preset_id),
        snapshot_id: Set(snapshot_id),
        module_overrides: Set(module_overrides),
        block_overrides: Set(block_overrides),
        parameter_state: Set(parameter_state),
        sort_order: Set(sort_order),
        tags: Set(tags),
        created_at: Set(now),
        updated_at: Set(now),
    }
    .insert(db)
    .await?;

    Ok(id)
}

/// List scene templates for a profile, ordered by sort_order.
pub async fn list_scene_templates(
    db: &DatabaseConnection,
    profile_id: Uuid,
) -> StorageResult<Vec<scene_template::Model>> {
    Ok(scene_template::Entity::find()
        .filter(scene_template::Column::ProfileId.eq(profile_id))
        .order_by_asc(scene_template::Column::SortOrder)
        .all(db)
        .await?)
}

/// Update a scene template's fields.
pub async fn update_scene_template(
    db: &DatabaseConnection,
    id: Uuid,
    name: Option<&str>,
    preset_id: Option<Uuid>,
    snapshot_id: Option<Option<Uuid>>,
    module_overrides: Option<serde_json::Value>,
    block_overrides: Option<serde_json::Value>,
    parameter_state: Option<serde_json::Value>,
    tags: Option<serde_json::Value>,
) -> StorageResult<()> {
    let existing = scene_template::Entity::find_by_id(id)
        .one(db)
        .await?
        .ok_or(StorageError::NotFound {
            entity: "scene_template",
            id,
        })?;

    let mut active: scene_template::ActiveModel = existing.into();
    let now = Utc::now().fixed_offset();

    if let Some(n) = name {
        active.name = Set(n.to_string());
    }
    if let Some(p) = preset_id {
        active.preset_id = Set(p);
    }
    if let Some(s) = snapshot_id {
        active.snapshot_id = Set(s);
    }
    if let Some(m) = module_overrides {
        active.module_overrides = Set(m);
    }
    if let Some(b) = block_overrides {
        active.block_overrides = Set(b);
    }
    if let Some(p) = parameter_state {
        active.parameter_state = Set(p);
    }
    if let Some(t) = tags {
        active.tags = Set(t);
    }
    active.updated_at = Set(now);

    active.update(db).await?;
    Ok(())
}

/// Delete a scene template (hard delete — cascaded from parent profile).
pub async fn delete_scene_template(db: &DatabaseConnection, id: Uuid) -> StorageResult<bool> {
    let result = scene_template::Entity::delete_by_id(id).exec(db).await?;
    Ok(result.rows_affected > 0)
}

/// Reorder scene templates within a profile.
/// Takes a list of template IDs in the desired order and updates their sort_order.
pub async fn reorder_scene_templates(
    db: &DatabaseConnection,
    profile_id: Uuid,
    ordered_ids: &[Uuid],
) -> StorageResult<()> {
    for (index, &template_id) in ordered_ids.iter().enumerate() {
        let existing = scene_template::Entity::find_by_id(template_id)
            .filter(scene_template::Column::ProfileId.eq(profile_id))
            .one(db)
            .await?
            .ok_or(StorageError::NotFound {
                entity: "scene_template",
                id: template_id,
            })?;

        let mut active: scene_template::ActiveModel = existing.into();
        active.sort_order = Set(index as i32);
        active.update(db).await?;
    }
    Ok(())
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

    // -- Profile Tests

    #[tokio::test]
    async fn create_and_get_profile() -> Result<()> {
        let db = setup_db().await?;
        let rig_id = Uuid::new_v4();

        let id = create_profile(
            &db,
            "Sunday Morning",
            rig_id,
            Some("Service profile"),
            serde_json::json!([]),
            serde_json::json!({}),
        )
        .await?;

        let profile = get_profile(&db, id).await?.expect("should exist");
        assert_eq!(profile.name, "Sunday Morning");
        assert_eq!(profile.rig_id, rig_id);
        assert_eq!(profile.description.as_deref(), Some("Service profile"));
        Ok(())
    }

    #[tokio::test]
    async fn list_profiles_excludes_deleted() -> Result<()> {
        let db = setup_db().await?;
        let rig_id = Uuid::new_v4();
        let tags = serde_json::json!([]);
        let meta = serde_json::json!({});

        create_profile(&db, "Active", rig_id, None, tags.clone(), meta.clone()).await?;
        let to_delete = create_profile(&db, "Deleted", rig_id, None, tags, meta).await?;
        delete_profile(&db, to_delete).await?;

        let profiles = list_profiles(&db).await?;
        assert_eq!(profiles.len(), 1);
        assert_eq!(profiles[0].name, "Active");
        Ok(())
    }

    #[tokio::test]
    async fn update_profile_fields() -> Result<()> {
        let db = setup_db().await?;
        let rig_id = Uuid::new_v4();

        let id = create_profile(
            &db,
            "Old",
            rig_id,
            None,
            serde_json::json!([]),
            serde_json::json!({}),
        )
        .await?;

        update_profile(
            &db,
            id,
            Some("Renamed"),
            Some(Some("New desc")),
            None,
            None,
            None,
        )
        .await?;

        let profile = get_profile(&db, id).await?.expect("should exist");
        assert_eq!(profile.name, "Renamed");
        assert_eq!(profile.description.as_deref(), Some("New desc"));
        Ok(())
    }

    // -- Scene Template Tests

    #[tokio::test]
    async fn add_and_list_scene_templates() -> Result<()> {
        let db = setup_db().await?;
        let rig_id = Uuid::new_v4();
        let profile_id = create_profile(
            &db,
            "Test",
            rig_id,
            None,
            serde_json::json!([]),
            serde_json::json!({}),
        )
        .await?;

        let preset_id = Uuid::new_v4();
        let empty = serde_json::json!({});

        add_scene_template(
            &db,
            profile_id,
            "Clean",
            preset_id,
            None,
            empty.clone(),
            empty.clone(),
            empty.clone(),
            0,
            serde_json::json!([]),
        )
        .await?;
        add_scene_template(
            &db,
            profile_id,
            "Lead",
            preset_id,
            None,
            empty.clone(),
            empty.clone(),
            empty.clone(),
            1,
            serde_json::json!([]),
        )
        .await?;

        let templates = list_scene_templates(&db, profile_id).await?;
        assert_eq!(templates.len(), 2);
        assert_eq!(templates[0].name, "Clean");
        assert_eq!(templates[0].sort_order, 0);
        assert_eq!(templates[1].name, "Lead");
        assert_eq!(templates[1].sort_order, 1);
        Ok(())
    }

    #[tokio::test]
    async fn reorder_scene_templates_updates_sort_order() -> Result<()> {
        let db = setup_db().await?;
        let rig_id = Uuid::new_v4();
        let profile_id = create_profile(
            &db,
            "Test",
            rig_id,
            None,
            serde_json::json!([]),
            serde_json::json!({}),
        )
        .await?;

        let preset_id = Uuid::new_v4();
        let empty = serde_json::json!({});

        let t1 = add_scene_template(
            &db,
            profile_id,
            "A",
            preset_id,
            None,
            empty.clone(),
            empty.clone(),
            empty.clone(),
            0,
            serde_json::json!([]),
        )
        .await?;
        let t2 = add_scene_template(
            &db,
            profile_id,
            "B",
            preset_id,
            None,
            empty.clone(),
            empty.clone(),
            empty.clone(),
            1,
            serde_json::json!([]),
        )
        .await?;
        let t3 = add_scene_template(
            &db,
            profile_id,
            "C",
            preset_id,
            None,
            empty.clone(),
            empty.clone(),
            empty.clone(),
            2,
            serde_json::json!([]),
        )
        .await?;

        // Reverse order: C, B, A
        reorder_scene_templates(&db, profile_id, &[t3, t2, t1]).await?;

        let templates = list_scene_templates(&db, profile_id).await?;
        assert_eq!(templates[0].name, "C");
        assert_eq!(templates[1].name, "B");
        assert_eq!(templates[2].name, "A");
        Ok(())
    }

    #[tokio::test]
    async fn delete_scene_template_removes_it() -> Result<()> {
        let db = setup_db().await?;
        let rig_id = Uuid::new_v4();
        let profile_id = create_profile(
            &db,
            "Test",
            rig_id,
            None,
            serde_json::json!([]),
            serde_json::json!({}),
        )
        .await?;

        let empty = serde_json::json!({});
        let t_id = add_scene_template(
            &db,
            profile_id,
            "Temp",
            Uuid::new_v4(),
            None,
            empty.clone(),
            empty.clone(),
            empty.clone(),
            0,
            serde_json::json!([]),
        )
        .await?;

        let deleted = delete_scene_template(&db, t_id).await?;
        assert!(deleted);

        let templates = list_scene_templates(&db, profile_id).await?;
        assert!(templates.is_empty());
        Ok(())
    }
}
