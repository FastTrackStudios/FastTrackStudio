//! Setlist repository — CRUD operations for setlists and song ordering.
//!
//! Setlists are ordered collections of performance songs for gig planning.

use chrono::Utc;
use sea_orm::*;
use uuid::Uuid;

use crate::entities::{setlist, setlist_song};
use crate::error::{StorageError, StorageResult};

// ─────────────────────────────────────────────────────────────────────────────
// Setlist CRUD
// ─────────────────────────────────────────────────────────────────────────────

/// Create a new setlist.
pub async fn create_setlist(
    db: &DatabaseConnection,
    name: &str,
    metadata: serde_json::Value,
    tags: serde_json::Value,
) -> StorageResult<Uuid> {
    let id = Uuid::new_v4();
    let now = Utc::now().fixed_offset();

    setlist::ActiveModel {
        id: Set(id),
        name: Set(name.to_string()),
        metadata: Set(metadata),
        tags: Set(tags),
        is_deleted: Set(false),
        created_at: Set(now),
        updated_at: Set(now),
    }
    .insert(db)
    .await?;

    Ok(id)
}

/// Get a setlist by ID.
pub async fn get_setlist(
    db: &DatabaseConnection,
    id: Uuid,
) -> StorageResult<Option<setlist::Model>> {
    Ok(setlist::Entity::find_by_id(id).one(db).await?)
}

/// List all setlists (excludes soft-deleted).
pub async fn list_setlists(db: &DatabaseConnection) -> StorageResult<Vec<setlist::Model>> {
    Ok(setlist::Entity::find()
        .filter(setlist::Column::IsDeleted.eq(false))
        .order_by_asc(setlist::Column::Name)
        .all(db)
        .await?)
}

/// Update a setlist's fields.
pub async fn update_setlist(
    db: &DatabaseConnection,
    id: Uuid,
    name: Option<&str>,
    metadata: Option<serde_json::Value>,
    tags: Option<serde_json::Value>,
) -> StorageResult<()> {
    let existing =
        setlist::Entity::find_by_id(id)
            .one(db)
            .await?
            .ok_or(StorageError::NotFound {
                entity: "setlist",
                id,
            })?;

    let mut active: setlist::ActiveModel = existing.into();
    let now = Utc::now().fixed_offset();

    if let Some(n) = name {
        active.name = Set(n.to_string());
    }
    if let Some(m) = metadata {
        active.metadata = Set(m);
    }
    if let Some(t) = tags {
        active.tags = Set(t);
    }
    active.updated_at = Set(now);

    active.update(db).await?;
    Ok(())
}

/// Soft-delete a setlist.
pub async fn delete_setlist(db: &DatabaseConnection, id: Uuid) -> StorageResult<()> {
    let existing =
        setlist::Entity::find_by_id(id)
            .one(db)
            .await?
            .ok_or(StorageError::NotFound {
                entity: "setlist",
                id,
            })?;

    let mut active: setlist::ActiveModel = existing.into();
    active.is_deleted = Set(true);
    active.updated_at = Set(Utc::now().fixed_offset());
    active.update(db).await?;
    Ok(())
}

// ─────────────────────────────────────────────────────────────────────────────
// Setlist Song Management
// ─────────────────────────────────────────────────────────────────────────────

/// Add a song to a setlist at the given sort position.
pub async fn add_song_to_setlist(
    db: &DatabaseConnection,
    setlist_id: Uuid,
    song_id: Uuid,
    sort_order: i32,
) -> StorageResult<Uuid> {
    let id = Uuid::new_v4();

    setlist_song::ActiveModel {
        id: Set(id),
        setlist_id: Set(setlist_id),
        song_id: Set(song_id),
        sort_order: Set(sort_order),
    }
    .insert(db)
    .await?;

    Ok(id)
}

/// List songs in a setlist, ordered by sort_order.
/// Returns the join table rows — caller can look up the actual songs.
pub async fn list_setlist_songs(
    db: &DatabaseConnection,
    setlist_id: Uuid,
) -> StorageResult<Vec<setlist_song::Model>> {
    Ok(setlist_song::Entity::find()
        .filter(setlist_song::Column::SetlistId.eq(setlist_id))
        .order_by_asc(setlist_song::Column::SortOrder)
        .all(db)
        .await?)
}

/// Remove a song from a setlist (by the join table row ID).
pub async fn remove_song_from_setlist(
    db: &DatabaseConnection,
    setlist_song_id: Uuid,
) -> StorageResult<bool> {
    let result = setlist_song::Entity::delete_by_id(setlist_song_id)
        .exec(db)
        .await?;
    Ok(result.rows_affected > 0)
}

/// Remove a song from a setlist by setlist_id + song_id pair.
pub async fn remove_song_from_setlist_by_ids(
    db: &DatabaseConnection,
    setlist_id: Uuid,
    song_id: Uuid,
) -> StorageResult<bool> {
    let result = setlist_song::Entity::delete_many()
        .filter(setlist_song::Column::SetlistId.eq(setlist_id))
        .filter(setlist_song::Column::SongId.eq(song_id))
        .exec(db)
        .await?;
    Ok(result.rows_affected > 0)
}

/// Reorder songs in a setlist.
/// Takes a list of song IDs in the desired order and updates sort_order.
pub async fn reorder_setlist_songs(
    db: &DatabaseConnection,
    setlist_id: Uuid,
    ordered_song_ids: &[Uuid],
) -> StorageResult<()> {
    for (index, &song_id) in ordered_song_ids.iter().enumerate() {
        // Find the join row for this setlist + song
        let existing = setlist_song::Entity::find()
            .filter(setlist_song::Column::SetlistId.eq(setlist_id))
            .filter(setlist_song::Column::SongId.eq(song_id))
            .one(db)
            .await?
            .ok_or(StorageError::NotFound {
                entity: "setlist_song",
                id: song_id,
            })?;

        let mut active: setlist_song::ActiveModel = existing.into();
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
    use crate::song_repo;
    use sea_orm_migration::MigratorTrait;

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    async fn setup_db() -> Result<DatabaseConnection> {
        let db = Database::connect("sqlite::memory:").await?;
        Migrator::up(&db, None).await?;
        Ok(db)
    }

    // -- Setlist Tests

    #[tokio::test]
    async fn create_and_get_setlist() -> Result<()> {
        let db = setup_db().await?;

        let id = create_setlist(
            &db,
            "Friday Night",
            serde_json::json!({"venue": "The Blue Note"}),
            serde_json::json!(["gig"]),
        )
        .await?;

        let setlist = get_setlist(&db, id).await?.expect("should exist");
        assert_eq!(setlist.name, "Friday Night");
        Ok(())
    }

    #[tokio::test]
    async fn list_setlists_excludes_deleted() -> Result<()> {
        let db = setup_db().await?;
        let meta = serde_json::json!({});
        let tags = serde_json::json!([]);

        create_setlist(&db, "Active", meta.clone(), tags.clone()).await?;
        let to_delete = create_setlist(&db, "Deleted", meta, tags).await?;
        delete_setlist(&db, to_delete).await?;

        let setlists = list_setlists(&db).await?;
        assert_eq!(setlists.len(), 1);
        assert_eq!(setlists[0].name, "Active");
        Ok(())
    }

    // -- Setlist Song Management Tests

    #[tokio::test]
    async fn add_and_list_setlist_songs() -> Result<()> {
        let db = setup_db().await?;
        let setlist_id =
            create_setlist(&db, "Test", serde_json::json!({}), serde_json::json!([])).await?;

        let song_a =
            song_repo::create_song(&db, "Song A", None, false, serde_json::json!([])).await?;
        let song_b =
            song_repo::create_song(&db, "Song B", None, false, serde_json::json!([])).await?;

        add_song_to_setlist(&db, setlist_id, song_a, 0).await?;
        add_song_to_setlist(&db, setlist_id, song_b, 1).await?;

        let songs = list_setlist_songs(&db, setlist_id).await?;
        assert_eq!(songs.len(), 2);
        assert_eq!(songs[0].song_id, song_a);
        assert_eq!(songs[0].sort_order, 0);
        assert_eq!(songs[1].song_id, song_b);
        assert_eq!(songs[1].sort_order, 1);
        Ok(())
    }

    #[tokio::test]
    async fn remove_song_from_setlist_by_ids_removes_it() -> Result<()> {
        let db = setup_db().await?;
        let setlist_id =
            create_setlist(&db, "Test", serde_json::json!({}), serde_json::json!([])).await?;
        let song_id =
            song_repo::create_song(&db, "Song", None, false, serde_json::json!([])).await?;

        add_song_to_setlist(&db, setlist_id, song_id, 0).await?;

        let removed = remove_song_from_setlist_by_ids(&db, setlist_id, song_id).await?;
        assert!(removed);

        let songs = list_setlist_songs(&db, setlist_id).await?;
        assert!(songs.is_empty());
        Ok(())
    }

    #[tokio::test]
    async fn reorder_setlist_songs_updates_sort_order() -> Result<()> {
        let db = setup_db().await?;
        let setlist_id =
            create_setlist(&db, "Test", serde_json::json!({}), serde_json::json!([])).await?;

        let song_a = song_repo::create_song(&db, "A", None, false, serde_json::json!([])).await?;
        let song_b = song_repo::create_song(&db, "B", None, false, serde_json::json!([])).await?;
        let song_c = song_repo::create_song(&db, "C", None, false, serde_json::json!([])).await?;

        add_song_to_setlist(&db, setlist_id, song_a, 0).await?;
        add_song_to_setlist(&db, setlist_id, song_b, 1).await?;
        add_song_to_setlist(&db, setlist_id, song_c, 2).await?;

        // Reorder: C, A, B
        reorder_setlist_songs(&db, setlist_id, &[song_c, song_a, song_b]).await?;

        let songs = list_setlist_songs(&db, setlist_id).await?;
        assert_eq!(songs[0].song_id, song_c);
        assert_eq!(songs[1].song_id, song_a);
        assert_eq!(songs[2].song_id, song_b);
        Ok(())
    }
}
