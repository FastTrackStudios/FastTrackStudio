//! Integration tests for preset tagging, categorization, and favorites.
//!
//! Uses an in-memory SQLite database to exercise the full `TaggingService`
//! against real SeaORM queries.

use chrono::Utc;
use sea_orm::{ActiveModelTrait, ActiveValue, Database, DatabaseConnection, IntoActiveModel};
use sea_orm_migration::MigratorTrait;
use serde_json::json;
use uuid::Uuid;

use signal_storage::entities::preset;
use signal_storage::tagging::{extract_tags, PresetCategory, TaggingService};
use signal_storage::Migrator;

type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

// region: --- Setup & Fixtures

/// Create an in-memory SQLite database with all migrations applied.
async fn setup_db() -> DatabaseConnection {
    let db = Database::connect("sqlite::memory:")
        .await
        .expect("connect to in-memory sqlite");
    Migrator::up(&db, None).await.expect("run migrations");
    db
}

/// Insert a preset with the given name, tags, and category.
async fn insert_preset(
    db: &DatabaseConnection,
    name: &str,
    tags: &[&str],
    category: Option<PresetCategory>,
) -> preset::Model {
    let now = Utc::now().fixed_offset();
    let tags_json = json!(tags);
    let cat_json = category
        .map(|c| c.to_json())
        .unwrap_or_else(|| json!(null));

    let active = preset::ActiveModel {
        id: ActiveValue::Set(Uuid::new_v4()),
        name: ActiveValue::Set(name.to_string()),
        description: ActiveValue::Set(None),
        author_id: ActiveValue::Set(None),
        category: ActiveValue::Set(cat_json),
        tags: ActiveValue::Set(tags_json),
        data: ActiveValue::Set(json!({})),
        is_public: ActiveValue::Set(false),
        is_deleted: ActiveValue::Set(false),
        is_favorite: ActiveValue::Set(false),
        version: ActiveValue::Set(1),
        created_at: ActiveValue::Set(now),
        updated_at: ActiveValue::Set(now),
    };

    active.insert(db).await.expect("insert preset")
}

// endregion: --- Setup & Fixtures

// region: --- Tag CRUD

#[tokio::test]
async fn add_tag_to_empty_preset() -> Result<()> {
    // -- Setup
    let db = setup_db().await;
    let p = insert_preset(&db, "Test Preset", &[], None).await;

    // -- Exec
    let updated = TaggingService::add_tag(&db, p.id, "Blues").await?;

    // -- Check
    let tags = extract_tags(&updated);
    assert_eq!(tags, vec!["Blues"]);
    Ok(())
}

#[tokio::test]
async fn add_tag_preserves_existing() -> Result<()> {
    // -- Setup
    let db = setup_db().await;
    let p = insert_preset(&db, "Test", &["Rock"], None).await;

    // -- Exec
    let updated = TaggingService::add_tag(&db, p.id, "Blues").await?;

    // -- Check
    let tags = extract_tags(&updated);
    assert_eq!(tags.len(), 2);
    assert!(tags.contains(&"Rock".to_string()));
    assert!(tags.contains(&"Blues".to_string()));
    Ok(())
}

#[tokio::test]
async fn add_tag_is_idempotent() -> Result<()> {
    // -- Setup
    let db = setup_db().await;
    let p = insert_preset(&db, "Test", &["Blues"], None).await;

    // -- Exec — case-insensitive duplicate check
    let updated = TaggingService::add_tag(&db, p.id, "blues").await?;

    // -- Check
    let tags = extract_tags(&updated);
    assert_eq!(tags, vec!["Blues"]); // no duplicate
    Ok(())
}

#[tokio::test]
async fn remove_tag() -> Result<()> {
    // -- Setup
    let db = setup_db().await;
    let p = insert_preset(&db, "Test", &["Blues", "Rock", "Jazz"], None).await;

    // -- Exec
    let updated = TaggingService::remove_tag(&db, p.id, "Rock").await?;

    // -- Check
    let tags = extract_tags(&updated);
    assert_eq!(tags, vec!["Blues", "Jazz"]);
    Ok(())
}

#[tokio::test]
async fn remove_tag_case_insensitive() -> Result<()> {
    // -- Setup
    let db = setup_db().await;
    let p = insert_preset(&db, "Test", &["Blues"], None).await;

    // -- Exec
    let updated = TaggingService::remove_tag(&db, p.id, "BLUES").await?;

    // -- Check
    let tags = extract_tags(&updated);
    assert!(tags.is_empty());
    Ok(())
}

#[tokio::test]
async fn remove_nonexistent_tag_is_noop() -> Result<()> {
    // -- Setup
    let db = setup_db().await;
    let p = insert_preset(&db, "Test", &["Rock"], None).await;

    // -- Exec
    let updated = TaggingService::remove_tag(&db, p.id, "NotHere").await?;

    // -- Check
    let tags = extract_tags(&updated);
    assert_eq!(tags, vec!["Rock"]);
    Ok(())
}

#[tokio::test]
async fn get_tags() -> Result<()> {
    // -- Setup
    let db = setup_db().await;
    let p = insert_preset(&db, "Test", &["A", "B", "C"], None).await;

    // -- Exec
    let tags = TaggingService::get_tags(&db, p.id).await?;

    // -- Check
    assert_eq!(tags, vec!["A", "B", "C"]);
    Ok(())
}

#[tokio::test]
async fn set_tags_replaces_all() -> Result<()> {
    // -- Setup
    let db = setup_db().await;
    let p = insert_preset(&db, "Test", &["Old1", "Old2"], None).await;

    // -- Exec
    let new_tags = vec!["New1".to_string(), "New2".to_string(), "New3".to_string()];
    let updated = TaggingService::set_tags(&db, p.id, &new_tags).await?;

    // -- Check
    let tags = extract_tags(&updated);
    assert_eq!(tags, vec!["New1", "New2", "New3"]);
    Ok(())
}

// endregion: --- Tag CRUD

// region: --- Tag queries

#[tokio::test]
async fn list_by_tag() -> Result<()> {
    // -- Setup
    let db = setup_db().await;
    insert_preset(&db, "A Blues", &["Blues", "Rock"], None).await;
    insert_preset(&db, "B Rock Only", &["Rock"], None).await;
    insert_preset(&db, "C Also Blues", &["Blues", "Jazz"], None).await;

    // -- Exec
    let results = TaggingService::list_by_tag(&db, "Blues").await?;

    // -- Check
    assert_eq!(results.len(), 2);
    assert_eq!(results[0].name, "A Blues");
    assert_eq!(results[1].name, "C Also Blues");
    Ok(())
}

#[tokio::test]
async fn list_by_tag_case_insensitive() -> Result<()> {
    // -- Setup
    let db = setup_db().await;
    insert_preset(&db, "Test", &["Blues"], None).await;

    // -- Exec
    let results = TaggingService::list_by_tag(&db, "blues").await?;

    // -- Check
    assert_eq!(results.len(), 1);
    Ok(())
}

#[tokio::test]
async fn list_by_tag_excludes_deleted() -> Result<()> {
    // -- Setup
    let db = setup_db().await;
    let p = insert_preset(&db, "Deleted", &["Blues"], None).await;
    // Soft-delete the preset
    let mut active: preset::ActiveModel = p.into_active_model();
    active.is_deleted = ActiveValue::Set(true);
    active.update(&db).await?;

    // -- Exec
    let results = TaggingService::list_by_tag(&db, "Blues").await?;

    // -- Check
    assert!(results.is_empty());
    Ok(())
}

#[tokio::test]
async fn list_tags_returns_sorted_unique() -> Result<()> {
    // -- Setup
    let db = setup_db().await;
    insert_preset(&db, "P1", &["Blues", "Rock"], None).await;
    insert_preset(&db, "P2", &["Rock", "Jazz"], None).await;
    insert_preset(&db, "P3", &["Blues", "Country"], None).await;

    // -- Exec
    let tags = TaggingService::list_tags(&db).await?;

    // -- Check — sorted alphabetically, deduplicated
    assert_eq!(tags, vec!["Blues", "Country", "Jazz", "Rock"]);
    Ok(())
}

#[tokio::test]
async fn list_tags_empty_when_no_presets() -> Result<()> {
    let db = setup_db().await;
    let tags = TaggingService::list_tags(&db).await?;
    assert!(tags.is_empty());
    Ok(())
}

// endregion: --- Tag queries

// region: --- Autocomplete

#[tokio::test]
async fn autocomplete_tags_by_prefix() -> Result<()> {
    // -- Setup
    let db = setup_db().await;
    insert_preset(&db, "P1", &["Blues", "Bright"], None).await;
    insert_preset(&db, "P2", &["Blues", "Bold"], None).await;
    insert_preset(&db, "P3", &["Rock"], None).await;

    // -- Exec
    let results = TaggingService::autocomplete_tags(&db, "Bl").await?;

    // -- Check
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].0, "Blues");
    assert_eq!(results[0].1, 2); // frequency = 2

    // -- Exec (broader prefix)
    let results = TaggingService::autocomplete_tags(&db, "B").await?;
    assert_eq!(results.len(), 3); // Blues(2), Bold(1), Bright(1)
    assert_eq!(results[0].0, "Blues"); // highest frequency first
    Ok(())
}

#[tokio::test]
async fn autocomplete_empty_prefix_returns_all() -> Result<()> {
    // -- Setup
    let db = setup_db().await;
    insert_preset(&db, "P1", &["Blues", "Rock"], None).await;
    insert_preset(&db, "P2", &["Jazz"], None).await;

    // -- Exec
    let results = TaggingService::autocomplete_tags(&db, "").await?;

    // -- Check — all tags, sorted by frequency
    assert_eq!(results.len(), 3);
    Ok(())
}

// endregion: --- Autocomplete

// region: --- Categories

#[tokio::test]
async fn set_and_get_category() -> Result<()> {
    // -- Setup
    let db = setup_db().await;
    let p = insert_preset(&db, "Test", &[], None).await;

    // -- Exec
    TaggingService::set_category(&db, p.id, PresetCategory::Amp).await?;
    let cat = TaggingService::get_category(&db, p.id).await?;

    // -- Check
    assert_eq!(cat, Some(PresetCategory::Amp));
    Ok(())
}

#[tokio::test]
async fn list_by_category() -> Result<()> {
    // -- Setup
    let db = setup_db().await;
    insert_preset(&db, "Amp1", &[], Some(PresetCategory::Amp)).await;
    insert_preset(&db, "Amp2", &[], Some(PresetCategory::Amp)).await;
    insert_preset(&db, "Effect1", &[], Some(PresetCategory::Effect)).await;

    // -- Exec
    let amps = TaggingService::list_by_category(&db, PresetCategory::Amp).await?;

    // -- Check
    assert_eq!(amps.len(), 2);
    assert_eq!(amps[0].name, "Amp1");
    assert_eq!(amps[1].name, "Amp2");
    Ok(())
}

#[tokio::test]
async fn preset_category_all_variants() {
    assert_eq!(PresetCategory::ALL.len(), 5);
    assert_eq!(PresetCategory::Amp.display_name(), "Amp");
    assert_eq!(PresetCategory::Effect.display_name(), "Effect");
    assert_eq!(PresetCategory::FullRig.display_name(), "Full Rig");
    assert_eq!(PresetCategory::Module.display_name(), "Module");
    assert_eq!(PresetCategory::Snapshot.display_name(), "Snapshot");
}

#[tokio::test]
async fn preset_category_json_roundtrip() {
    for cat in PresetCategory::ALL {
        let json = cat.to_json();
        let parsed = PresetCategory::from_json(&json);
        assert_eq!(parsed, Some(*cat));
    }
}

// endregion: --- Categories

// region: --- Favorites

#[tokio::test]
async fn toggle_favorite() -> Result<()> {
    // -- Setup
    let db = setup_db().await;
    let p = insert_preset(&db, "Test", &[], None).await;
    assert!(!p.is_favorite);

    // -- Exec: toggle on
    let now_fav = TaggingService::toggle_favorite(&db, p.id).await?;
    assert!(now_fav);

    // -- Exec: toggle off
    let now_not = TaggingService::toggle_favorite(&db, p.id).await?;
    assert!(!now_not);
    Ok(())
}

#[tokio::test]
async fn set_favorite() -> Result<()> {
    // -- Setup
    let db = setup_db().await;
    let p = insert_preset(&db, "Test", &[], None).await;

    // -- Exec
    let updated = TaggingService::set_favorite(&db, p.id, true).await?;
    assert!(updated.is_favorite);

    let updated = TaggingService::set_favorite(&db, p.id, false).await?;
    assert!(!updated.is_favorite);
    Ok(())
}

#[tokio::test]
async fn list_favorites() -> Result<()> {
    // -- Setup
    let db = setup_db().await;
    let p1 = insert_preset(&db, "A Fav", &[], None).await;
    insert_preset(&db, "B Not Fav", &[], None).await;
    let p3 = insert_preset(&db, "C Fav", &[], None).await;
    TaggingService::set_favorite(&db, p1.id, true).await?;
    TaggingService::set_favorite(&db, p3.id, true).await?;

    // -- Exec
    let favs = TaggingService::list_favorites(&db).await?;

    // -- Check
    assert_eq!(favs.len(), 2);
    assert_eq!(favs[0].name, "A Fav");
    assert_eq!(favs[1].name, "C Fav");
    Ok(())
}

// endregion: --- Favorites

// region: --- Compound filters

#[tokio::test]
async fn filter_presets_by_tag_and_category() -> Result<()> {
    // -- Setup
    let db = setup_db().await;
    insert_preset(&db, "Blues Amp", &["Blues"], Some(PresetCategory::Amp)).await;
    insert_preset(&db, "Rock Amp", &["Rock"], Some(PresetCategory::Amp)).await;
    insert_preset(&db, "Blues Effect", &["Blues"], Some(PresetCategory::Effect)).await;

    // -- Exec: filter by tag "Blues" + category Amp
    let results = TaggingService::filter_presets(
        &db,
        &["Blues".to_string()],
        Some(PresetCategory::Amp),
        false,
    )
    .await?;

    // -- Check
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].name, "Blues Amp");
    Ok(())
}

#[tokio::test]
async fn filter_presets_favorites_only() -> Result<()> {
    // -- Setup
    let db = setup_db().await;
    let p1 = insert_preset(&db, "A Fav Blues", &["Blues"], None).await;
    insert_preset(&db, "B Not Fav Blues", &["Blues"], None).await;
    TaggingService::set_favorite(&db, p1.id, true).await?;

    // -- Exec
    let results = TaggingService::filter_presets(
        &db,
        &["Blues".to_string()],
        None,
        true,
    )
    .await?;

    // -- Check
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].name, "A Fav Blues");
    Ok(())
}

#[tokio::test]
async fn filter_presets_multiple_tags() -> Result<()> {
    // -- Setup
    let db = setup_db().await;
    insert_preset(&db, "Blues Rock", &["Blues", "Rock"], None).await;
    insert_preset(&db, "Blues Only", &["Blues"], None).await;

    // -- Exec: require both tags
    let results = TaggingService::filter_presets(
        &db,
        &["Blues".to_string(), "Rock".to_string()],
        None,
        false,
    )
    .await?;

    // -- Check
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].name, "Blues Rock");
    Ok(())
}

// endregion: --- Compound filters

// region: --- Error handling

#[tokio::test]
async fn add_tag_nonexistent_preset_returns_not_found() {
    let db = setup_db().await;
    let result = TaggingService::add_tag(&db, Uuid::new_v4(), "Blues").await;
    assert!(result.is_err());
}

#[tokio::test]
async fn remove_tag_nonexistent_preset_returns_not_found() {
    let db = setup_db().await;
    let result = TaggingService::remove_tag(&db, Uuid::new_v4(), "Blues").await;
    assert!(result.is_err());
}

#[tokio::test]
async fn toggle_favorite_nonexistent_preset_returns_not_found() {
    let db = setup_db().await;
    let result = TaggingService::toggle_favorite(&db, Uuid::new_v4()).await;
    assert!(result.is_err());
}

// endregion: --- Error handling
