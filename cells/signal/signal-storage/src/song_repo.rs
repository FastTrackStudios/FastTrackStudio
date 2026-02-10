//! Song repository — CRUD operations for performance songs and song scenes.
//!
//! Performance songs contain ordered scenes for live use. Each scene
//! references a preset/snapshot with optional transition and MIDI trigger config.

use chrono::Utc;
use sea_orm::*;
use uuid::Uuid;

use crate::entities::{performance_song, song_scene};
use crate::error::{StorageError, StorageResult};

// ─────────────────────────────────────────────────────────────────────────────
// Song CRUD
// ─────────────────────────────────────────────────────────────────────────────

/// Create a new performance song.
pub async fn create_song(
    db: &DatabaseConnection,
    name: &str,
    artist: Option<&str>,
    auto_advance: bool,
    tags: serde_json::Value,
) -> StorageResult<Uuid> {
    let id = Uuid::new_v4();
    let now = Utc::now().fixed_offset();

    performance_song::ActiveModel {
        id: Set(id),
        name: Set(name.to_string()),
        artist: Set(artist.map(String::from)),
        auto_advance: Set(auto_advance),
        linked_song_id: Set(None),
        module_overrides: Set(serde_json::json!({})),
        tags: Set(tags),
        is_deleted: Set(false),
        created_at: Set(now),
        updated_at: Set(now),
    }
    .insert(db)
    .await?;

    Ok(id)
}

/// Get a song by ID.
pub async fn get_song(
    db: &DatabaseConnection,
    id: Uuid,
) -> StorageResult<Option<performance_song::Model>> {
    Ok(performance_song::Entity::find_by_id(id).one(db).await?)
}

/// List all songs (excludes soft-deleted).
pub async fn list_songs(db: &DatabaseConnection) -> StorageResult<Vec<performance_song::Model>> {
    Ok(performance_song::Entity::find()
        .filter(performance_song::Column::IsDeleted.eq(false))
        .order_by_asc(performance_song::Column::Name)
        .all(db)
        .await?)
}

/// Update a song's mutable fields.
pub async fn update_song(
    db: &DatabaseConnection,
    id: Uuid,
    name: Option<&str>,
    artist: Option<Option<&str>>,
    auto_advance: Option<bool>,
    module_overrides: Option<serde_json::Value>,
    tags: Option<serde_json::Value>,
) -> StorageResult<()> {
    let existing = performance_song::Entity::find_by_id(id)
        .one(db)
        .await?
        .ok_or(StorageError::NotFound {
            entity: "performance_song",
            id,
        })?;

    let mut active: performance_song::ActiveModel = existing.into();
    let now = Utc::now().fixed_offset();

    if let Some(n) = name {
        active.name = Set(n.to_string());
    }
    if let Some(a) = artist {
        active.artist = Set(a.map(String::from));
    }
    if let Some(aa) = auto_advance {
        active.auto_advance = Set(aa);
    }
    if let Some(m) = module_overrides {
        active.module_overrides = Set(m);
    }
    if let Some(t) = tags {
        active.tags = Set(t);
    }
    active.updated_at = Set(now);

    active.update(db).await?;
    Ok(())
}

/// Soft-delete a song.
pub async fn delete_song(db: &DatabaseConnection, id: Uuid) -> StorageResult<()> {
    let existing = performance_song::Entity::find_by_id(id)
        .one(db)
        .await?
        .ok_or(StorageError::NotFound {
            entity: "performance_song",
            id,
        })?;

    let mut active: performance_song::ActiveModel = existing.into();
    active.is_deleted = Set(true);
    active.updated_at = Set(Utc::now().fixed_offset());
    active.update(db).await?;
    Ok(())
}

// ─────────────────────────────────────────────────────────────────────────────
// Song Scene CRUD
// ─────────────────────────────────────────────────────────────────────────────

/// Add a scene to a song.
pub async fn add_song_scene(
    db: &DatabaseConnection,
    song_id: Uuid,
    name: &str,
    preset_id: Uuid,
    snapshot_id: Option<Uuid>,
    transition: serde_json::Value,
    midi_triggers: serde_json::Value,
    module_overrides: serde_json::Value,
    block_overrides: serde_json::Value,
    sort_order: i32,
    tags: serde_json::Value,
) -> StorageResult<Uuid> {
    let id = Uuid::new_v4();
    let now = Utc::now().fixed_offset();

    song_scene::ActiveModel {
        id: Set(id),
        song_id: Set(song_id),
        name: Set(name.to_string()),
        preset_id: Set(preset_id),
        snapshot_id: Set(snapshot_id),
        transition: Set(transition),
        midi_triggers: Set(midi_triggers),
        module_overrides: Set(module_overrides),
        block_overrides: Set(block_overrides),
        sort_order: Set(sort_order),
        tags: Set(tags),
        created_at: Set(now),
        updated_at: Set(now),
    }
    .insert(db)
    .await?;

    Ok(id)
}

/// List scenes for a song, ordered by sort_order.
pub async fn list_song_scenes(
    db: &DatabaseConnection,
    song_id: Uuid,
) -> StorageResult<Vec<song_scene::Model>> {
    Ok(song_scene::Entity::find()
        .filter(song_scene::Column::SongId.eq(song_id))
        .order_by_asc(song_scene::Column::SortOrder)
        .all(db)
        .await?)
}

/// Update a song scene's fields.
pub async fn update_song_scene(
    db: &DatabaseConnection,
    id: Uuid,
    name: Option<&str>,
    preset_id: Option<Uuid>,
    snapshot_id: Option<Option<Uuid>>,
    transition: Option<serde_json::Value>,
    midi_triggers: Option<serde_json::Value>,
    module_overrides: Option<serde_json::Value>,
    block_overrides: Option<serde_json::Value>,
    tags: Option<serde_json::Value>,
) -> StorageResult<()> {
    let existing =
        song_scene::Entity::find_by_id(id)
            .one(db)
            .await?
            .ok_or(StorageError::NotFound {
                entity: "song_scene",
                id,
            })?;

    let mut active: song_scene::ActiveModel = existing.into();
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
    if let Some(t) = transition {
        active.transition = Set(t);
    }
    if let Some(m) = midi_triggers {
        active.midi_triggers = Set(m);
    }
    if let Some(m) = module_overrides {
        active.module_overrides = Set(m);
    }
    if let Some(b) = block_overrides {
        active.block_overrides = Set(b);
    }
    if let Some(t) = tags {
        active.tags = Set(t);
    }
    active.updated_at = Set(now);

    active.update(db).await?;
    Ok(())
}

/// Delete a song scene (hard delete).
pub async fn delete_song_scene(db: &DatabaseConnection, id: Uuid) -> StorageResult<bool> {
    let result = song_scene::Entity::delete_by_id(id).exec(db).await?;
    Ok(result.rows_affected > 0)
}

/// Reorder song scenes within a song.
pub async fn reorder_song_scenes(
    db: &DatabaseConnection,
    song_id: Uuid,
    ordered_ids: &[Uuid],
) -> StorageResult<()> {
    for (index, &scene_id) in ordered_ids.iter().enumerate() {
        let existing = song_scene::Entity::find_by_id(scene_id)
            .filter(song_scene::Column::SongId.eq(song_id))
            .one(db)
            .await?
            .ok_or(StorageError::NotFound {
                entity: "song_scene",
                id: scene_id,
            })?;

        let mut active: song_scene::ActiveModel = existing.into();
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

    // -- Song Tests

    #[tokio::test]
    async fn create_and_get_song() -> Result<()> {
        let db = setup_db().await?;

        let id = create_song(
            &db,
            "Gravity",
            Some("John Mayer"),
            false,
            serde_json::json!(["blues", "live"]),
        )
        .await?;

        let song = get_song(&db, id).await?.expect("should exist");
        assert_eq!(song.name, "Gravity");
        assert_eq!(song.artist.as_deref(), Some("John Mayer"));
        assert!(!song.auto_advance);
        Ok(())
    }

    #[tokio::test]
    async fn list_songs_excludes_deleted() -> Result<()> {
        let db = setup_db().await?;
        let tags = serde_json::json!([]);

        create_song(&db, "Active", None, false, tags.clone()).await?;
        let to_delete = create_song(&db, "Deleted", None, false, tags).await?;
        delete_song(&db, to_delete).await?;

        let songs = list_songs(&db).await?;
        assert_eq!(songs.len(), 1);
        assert_eq!(songs[0].name, "Active");
        Ok(())
    }

    #[tokio::test]
    async fn update_song_fields() -> Result<()> {
        let db = setup_db().await?;

        let id = create_song(&db, "Old", None, false, serde_json::json!([])).await?;

        update_song(
            &db,
            id,
            Some("New Title"),
            Some(Some("Artist Name")),
            Some(true),
            None,
            None,
        )
        .await?;

        let song = get_song(&db, id).await?.expect("should exist");
        assert_eq!(song.name, "New Title");
        assert_eq!(song.artist.as_deref(), Some("Artist Name"));
        assert!(song.auto_advance);
        Ok(())
    }

    // -- Song Scene Tests

    #[tokio::test]
    async fn add_and_list_song_scenes() -> Result<()> {
        let db = setup_db().await?;
        let song_id = create_song(&db, "Test", None, false, serde_json::json!([])).await?;
        let preset_id = Uuid::new_v4();
        let empty = serde_json::json!({});

        add_song_scene(
            &db,
            song_id,
            "Intro",
            preset_id,
            None,
            empty.clone(),
            empty.clone(),
            empty.clone(),
            empty.clone(),
            0,
            serde_json::json!([]),
        )
        .await?;
        add_song_scene(
            &db,
            song_id,
            "Verse",
            preset_id,
            None,
            empty.clone(),
            empty.clone(),
            empty.clone(),
            empty.clone(),
            1,
            serde_json::json!([]),
        )
        .await?;

        let scenes = list_song_scenes(&db, song_id).await?;
        assert_eq!(scenes.len(), 2);
        assert_eq!(scenes[0].name, "Intro");
        assert_eq!(scenes[1].name, "Verse");
        Ok(())
    }

    #[tokio::test]
    async fn reorder_song_scenes_updates_sort_order() -> Result<()> {
        let db = setup_db().await?;
        let song_id = create_song(&db, "Test", None, false, serde_json::json!([])).await?;
        let preset_id = Uuid::new_v4();
        let empty = serde_json::json!({});

        let s1 = add_song_scene(
            &db,
            song_id,
            "Intro",
            preset_id,
            None,
            empty.clone(),
            empty.clone(),
            empty.clone(),
            empty.clone(),
            0,
            serde_json::json!([]),
        )
        .await?;
        let s2 = add_song_scene(
            &db,
            song_id,
            "Verse",
            preset_id,
            None,
            empty.clone(),
            empty.clone(),
            empty.clone(),
            empty.clone(),
            1,
            serde_json::json!([]),
        )
        .await?;
        let s3 = add_song_scene(
            &db,
            song_id,
            "Chorus",
            preset_id,
            None,
            empty.clone(),
            empty.clone(),
            empty.clone(),
            empty.clone(),
            2,
            serde_json::json!([]),
        )
        .await?;

        // Reverse: Chorus, Verse, Intro
        reorder_song_scenes(&db, song_id, &[s3, s2, s1]).await?;

        let scenes = list_song_scenes(&db, song_id).await?;
        assert_eq!(scenes[0].name, "Chorus");
        assert_eq!(scenes[1].name, "Verse");
        assert_eq!(scenes[2].name, "Intro");
        Ok(())
    }

    #[tokio::test]
    async fn delete_song_scene_removes_it() -> Result<()> {
        let db = setup_db().await?;
        let song_id = create_song(&db, "Test", None, false, serde_json::json!([])).await?;
        let empty = serde_json::json!({});

        let scene_id = add_song_scene(
            &db,
            song_id,
            "Temp",
            Uuid::new_v4(),
            None,
            empty.clone(),
            empty.clone(),
            empty.clone(),
            empty.clone(),
            0,
            serde_json::json!([]),
        )
        .await?;

        let deleted = delete_song_scene(&db, scene_id).await?;
        assert!(deleted);

        let scenes = list_song_scenes(&db, song_id).await?;
        assert!(scenes.is_empty());
        Ok(())
    }
}
