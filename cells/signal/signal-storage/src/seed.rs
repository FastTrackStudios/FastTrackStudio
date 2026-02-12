//! Database seeding — populates the DB with default data on first launch.
//!
//! Core seed data comes from [`MockRigControlService::with_guitar_defaults()`],
//! ensuring the DB seed and mock service always agree. Dummy block/module
//! presets are seeded separately for testing preset browsers and overrides.

use chrono::Utc;
use sea_orm::{ActiveModelTrait, DatabaseConnection, Set};

use signal::service::MockRigControlService;
use signal::stores::SceneStore as _; // Bring trait methods into scope
use signal_proto::defaults::blocks::dummy;
use signal_proto::defaults::modules::dummy as module_dummy;
use signal_proto::preset::ModulePreset;
use signal_proto::profile::{Patch, Profile};
use signal_proto::scene::{RigScene, ScopedSceneRef};
use signal_proto::song::{Song, SongSection};

use crate::entities::{
    module_preset_entity, module_snapshot, performance_song, preset as preset_entity,
    profile as profile_entity, scene_template, snapshot as snapshot_entity, song_scene,
};
use crate::error::StorageResult;
use crate::facet_bridge;

/// Seed the database with defaults if it's empty.
///
/// Uses incremental seeding: each category is checked independently so
/// new seed data (e.g. dummy block/module presets) gets added even if
/// the DB was created before those features existed.
///
/// Returns `true` if any seeding was performed, `false` if fully up-to-date.
pub async fn seed_if_empty(db: &DatabaseConnection) -> StorageResult<bool> {
    let existing_module_presets = crate::module_repo::list_module_presets(db, None).await?;
    let existing_block_presets = crate::block_repo::list_block_presets(db, None).await?;
    let mut seeded = false;

    // Full seed: no module presets means fresh DB
    if existing_module_presets.is_empty() {
        seed_from_defaults(db).await?;
        seeded = true;
    } else {
        tracing::debug!(
            "Core data already seeded ({} module presets)",
            existing_module_presets.len()
        );

        // Incremental: seed dummy block presets if missing
        if existing_block_presets.is_empty() {
            let dummy_blocks = dummy::all_dummy_blocks();
            seed_dummy_block_presets(db, &dummy_blocks).await?;
            tracing::info!(
                "Incremental seed: added {} dummy block presets",
                dummy_blocks.len()
            );
            seeded = true;
        }

        // Incremental: seed dummy module presets if missing.
        // Check for any module preset whose name starts with "Dummy " as marker.
        let has_dummy_modules = existing_module_presets
            .iter()
            .any(|p| p.name.starts_with("Dummy "));
        if !has_dummy_modules {
            let dummy_modules = module_dummy::all_dummy_module_presets();
            seed_dummy_module_presets(db, &dummy_modules).await?;
            tracing::info!(
                "Incremental seed: added {} dummy module presets",
                dummy_modules.len()
            );
            seeded = true;
        }
    }

    if !seeded {
        tracing::debug!("Database fully seeded, nothing to do");
    }
    Ok(seeded)
}

/// Seed all core data from the mock service defaults.
///
/// This is the single source of truth: `MockRigControlService::with_guitar_defaults()`
/// builds the canonical guitar rig with scenes, profiles, and songs. We extract
/// and persist everything from that data set.
async fn seed_from_defaults(db: &DatabaseConnection) -> StorageResult<()> {
    let service = MockRigControlService::with_guitar_defaults();
    let data = service.data();

    // Seed module presets from the scene store
    let module_presets: Vec<_> = data.store.module_presets().values().cloned().collect();
    seed_module_presets(db, &module_presets).await?;

    // Seed scenes as top-level presets (so the preset browser has content)
    seed_scenes_as_presets(db, &data.store).await?;

    // Seed profiles with their patches
    seed_profiles(db, &data.profiles).await?;

    // Seed songs with their sections
    seed_songs(db, &data.songs).await?;

    // Seed dummy block presets for every block type (for testing overrides)
    let dummy_blocks = dummy::all_dummy_blocks();
    seed_dummy_block_presets(db, &dummy_blocks).await?;

    // Seed dummy module presets for every module type
    let dummy_modules = module_dummy::all_dummy_module_presets();
    seed_dummy_module_presets(db, &dummy_modules).await?;

    tracing::info!(
        "Seeded DB: {} module presets, {} profiles, {} songs, {} dummy block presets, {} dummy module presets",
        module_presets.len(),
        data.profiles.len(),
        data.songs.len(),
        dummy_blocks.len(),
        dummy_modules.len(),
    );
    Ok(())
}

// ── Module Presets ────────────────────────────────────────────────────────

async fn seed_module_presets(
    db: &DatabaseConnection,
    presets: &[ModulePreset],
) -> StorageResult<()> {
    for mp in presets {
        let now = Utc::now().fixed_offset();
        // Serialize the embedded snapshot as the "blocks" column (schema-agnostic JSON)
        let snapshot_json = facet_bridge::to_json_value(&mp.snapshot)?;
        let tags_json = facet_bridge::to_json_value(&mp.metadata.tags)?;

        module_preset_entity::ActiveModel {
            id: Set(mp.id.into()),
            name: Set(mp.metadata.name.clone()),
            module_type: Set(mp.module_type.display_name().to_string()),
            description: Set(mp.metadata.description.clone()),
            blocks: Set(snapshot_json.clone()),
            macros: Set(serde_json::json!([])),
            tags: Set(tags_json),
            is_template: Set(true),
            is_deleted: Set(false),
            created_at: Set(now),
            updated_at: Set(now),
        }
        .insert(db)
        .await?;

        // Create a single module snapshot row for the embedded snapshot
        seed_module_snapshot(db, mp.id.into(), &mp.snapshot, true).await?;
    }
    Ok(())
}

async fn seed_module_snapshot(
    db: &DatabaseConnection,
    module_preset_id: uuid::Uuid,
    snapshot: &signal_proto::snapshot::ModuleSnapshot,
    is_default: bool,
) -> StorageResult<()> {
    let now = Utc::now().fixed_offset();
    let block_refs_json = facet_bridge::to_json_value(&snapshot.block_snapshots)?;
    let tags_json = facet_bridge::to_json_value(&snapshot.tags)?;

    module_snapshot::ActiveModel {
        id: Set(snapshot.id.into()),
        module_preset_id: Set(module_preset_id),
        name: Set(snapshot.name.clone()),
        block_overrides: Set(block_refs_json),
        is_default: Set(is_default),
        tags: Set(tags_json),
        created_at: Set(now),
        updated_at: Set(now),
    }
    .insert(db)
    .await?;
    Ok(())
}

// ── Scenes as Presets ────────────────────────────────────────────────────
//
// The old domain had top-level "Presets". The new domain has hierarchical
// scenes (Layer → Engine → Rig). We serialize each rig scene as a
// preset row so the preset browser still has browsable content.

async fn seed_scenes_as_presets(
    db: &DatabaseConnection,
    store: &signal::stores::InMemorySceneStore,
) -> StorageResult<()> {
    // Seed rig scenes as top-level presets
    for (id, scene) in store.rig_scenes() {
        seed_rig_scene_as_preset(db, scene).await?;

        // Also seed the engine/layer scenes it references as snapshots
        seed_scene_hierarchy_as_snapshots(db, (*id).into(), scene, store).await?;
    }
    Ok(())
}

async fn seed_rig_scene_as_preset(db: &DatabaseConnection, scene: &RigScene) -> StorageResult<()> {
    let now = Utc::now().fixed_offset();
    let data_json = facet_bridge::to_json_value(scene)?;
    let tags_json = facet_bridge::to_json_value(&scene.tags)?;

    preset_entity::ActiveModel {
        id: Set(scene.id.into()),
        name: Set(scene.name.clone()),
        description: Set(None),
        author_id: Set(None),
        category: Set(serde_json::json!("Default")),
        tags: Set(tags_json),
        data: Set(data_json),
        is_public: Set(false),
        is_deleted: Set(false),
        is_favorite: Set(false),
        is_template: Set(true),
        version: Set(1),
        created_at: Set(now),
        updated_at: Set(now),
    }
    .insert(db)
    .await?;
    Ok(())
}

/// Seed engine and layer scenes referenced by a rig scene as "snapshots" in the
/// preset table's snapshot relation. This preserves the hierarchy in the DB.
async fn seed_scene_hierarchy_as_snapshots(
    db: &DatabaseConnection,
    rig_scene_uuid: uuid::Uuid,
    rig_scene: &RigScene,
    store: &signal::stores::InMemorySceneStore,
) -> StorageResult<()> {
    for entry in &rig_scene.engine_scenes {
        if let Some(engine_scene) = store.engine_scene(entry.scene_ref.target_id()) {
            let snap_json = facet_bridge::to_json_value(engine_scene)?;
            let now = Utc::now().fixed_offset();

            snapshot_entity::ActiveModel {
                id: Set(engine_scene.id.into()),
                preset_id: Set(rig_scene_uuid),
                name: Set(engine_scene.name.clone()),
                data: Set(snap_json),
                created_at: Set(now),
                updated_at: Set(now),
            }
            .insert(db)
            .await?;
        }
    }
    Ok(())
}

// ── Profiles + Patches ───────────────────────────────────────────────────

async fn seed_profiles(db: &DatabaseConnection, profiles: &[Profile]) -> StorageResult<()> {
    for p in profiles {
        let now = Utc::now().fixed_offset();
        let tags_json = facet_bridge::to_json_value(&p.tags)?;

        profile_entity::ActiveModel {
            id: Set(p.id.into()),
            name: Set(p.name.clone()),
            rig_id: Set(uuid::Uuid::nil()), // Profiles no longer carry rig_id
            description: Set(None),
            tags: Set(tags_json),
            metadata: Set(serde_json::json!({})),
            default_scene_template_id: Set(None),
            is_template: Set(true),
            is_deleted: Set(false),
            created_at: Set(now),
            updated_at: Set(now),
        }
        .insert(db)
        .await?;

        seed_patches(db, p.id.into(), &p.patches).await?;
    }
    Ok(())
}

async fn seed_patches(
    db: &DatabaseConnection,
    profile_id: uuid::Uuid,
    patches: &[Patch],
) -> StorageResult<()> {
    for (i, patch) in patches.iter().enumerate() {
        let now = Utc::now().fixed_offset();
        let scene_ref_json = facet_bridge::to_json_value(&patch.scene_ref)?;
        let tags_json = facet_bridge::to_json_value(&patch.tags)?;

        // Extract the scene ID from the ScopedSceneRef for the preset_id column
        let preset_id = scene_ref_to_uuid(&patch.scene_ref);

        scene_template::ActiveModel {
            id: Set(patch.id.into()),
            profile_id: Set(profile_id),
            name: Set(patch.name.clone()),
            preset_id: Set(preset_id),
            snapshot_id: Set(None),
            module_overrides: Set(scene_ref_json.clone()),
            block_overrides: Set(serde_json::json!([])),
            parameter_state: Set(serde_json::json!({})),
            sort_order: Set(i as i32),
            tags: Set(tags_json),
            created_at: Set(now),
            updated_at: Set(now),
        }
        .insert(db)
        .await?;
    }
    Ok(())
}

/// Extract the underlying UUID from a ScopedSceneRef.
fn scene_ref_to_uuid(scene_ref: &ScopedSceneRef) -> uuid::Uuid {
    match scene_ref {
        ScopedSceneRef::Layer(r) => (*r.target_id()).into(),
        ScopedSceneRef::Engine(r) => (*r.target_id()).into(),
        ScopedSceneRef::Rig(r) => (*r.target_id()).into(),
        ScopedSceneRef::Rack(r) => (*r.target_id()).into(),
    }
}

// ── Songs + Sections ─────────────────────────────────────────────────────

async fn seed_songs(db: &DatabaseConnection, songs: &[Song]) -> StorageResult<()> {
    for s in songs {
        let now = Utc::now().fixed_offset();
        let tags_json = facet_bridge::to_json_value(&s.tags)?;

        performance_song::ActiveModel {
            id: Set(s.id.into()),
            name: Set(s.name.clone()),
            artist: Set(s.artist.clone()),
            auto_advance: Set(false),
            linked_song_id: Set(None),
            module_overrides: Set(serde_json::json!([])),
            tags: Set(tags_json),
            is_template: Set(true),
            is_deleted: Set(false),
            created_at: Set(now),
            updated_at: Set(now),
        }
        .insert(db)
        .await?;

        seed_song_sections(db, s.id.into(), &s.sections).await?;
    }
    Ok(())
}

async fn seed_song_sections(
    db: &DatabaseConnection,
    song_id: uuid::Uuid,
    sections: &[SongSection],
) -> StorageResult<()> {
    for (i, section) in sections.iter().enumerate() {
        let now = Utc::now().fixed_offset();
        let transition_json = facet_bridge::to_json_value(&section.transition)?;
        let midi_triggers_json = facet_bridge::to_json_value(&section.midi_triggers)?;
        let overrides_json = facet_bridge::to_json_value(&section.overrides)?;
        let tags_json = facet_bridge::to_json_value(&section.tags)?;

        let preset_id = scene_ref_to_uuid(&section.scene_ref);

        song_scene::ActiveModel {
            id: Set(section.id.into()),
            song_id: Set(song_id),
            name: Set(section.name.clone()),
            preset_id: Set(preset_id),
            snapshot_id: Set(None),
            transition: Set(transition_json),
            midi_triggers: Set(midi_triggers_json),
            module_overrides: Set(overrides_json),
            block_overrides: Set(serde_json::json!([])),
            sort_order: Set(i as i32),
            tags: Set(tags_json),
            created_at: Set(now),
            updated_at: Set(now),
        }
        .insert(db)
        .await?;
    }
    Ok(())
}

// ── Dummy Block Presets + Snapshots ──────────────────────────────────────

async fn seed_dummy_block_presets(
    db: &DatabaseConnection,
    blocks: &[signal_proto::block::Block],
) -> StorageResult<()> {
    for block in blocks {
        let plugin_id_json = facet_bridge::to_json_value(&block.plugin_id)?;
        let tags_json = serde_json::json!([]);
        let block_type_str = block.block_type.display_name();

        let preset_id = crate::block_repo::create_block_preset(
            db,
            &block.name,
            block_type_str,
            Some(plugin_id_json),
            None,
            block.description.as_deref(),
            tags_json,
        )
        .await?;

        // Create 4 snapshots per dummy block preset
        for (i, name) in dummy::DUMMY_SNAPSHOT_NAMES.iter().enumerate() {
            let is_default = i == 0;
            let params = serde_json::json!({});
            crate::block_repo::create_block_snapshot(db, preset_id, name, params, None, is_default)
                .await?;
        }
    }
    Ok(())
}

// ── Dummy Module Presets + Snapshots ────────────────────────────────────

async fn seed_dummy_module_presets(
    db: &DatabaseConnection,
    modules: &[module_dummy::DummyModulePreset],
) -> StorageResult<()> {
    for module in modules {
        let module_type_str = module.module_type.display_name();
        let tags_json = serde_json::json!([]);

        let preset_id = crate::module_repo::create_module_preset(
            db,
            &module.name,
            module_type_str,
            Some(&module.description),
            serde_json::json!([]), // empty blocks
            serde_json::json!([]), // empty macros
            tags_json,
        )
        .await?;

        // Create 4 snapshots per dummy module preset
        for (i, name) in module_dummy::DUMMY_SNAPSHOT_NAMES.iter().enumerate() {
            let is_default = i == 0;
            let overrides = serde_json::json!([]);
            let snap_tags = serde_json::json!([]);
            crate::module_repo::create_module_snapshot(
                db, preset_id, name, overrides, is_default, snap_tags,
            )
            .await?;
        }
    }
    Ok(())
}

// ── Tests ────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use sea_orm::Database;
    use sea_orm_migration::MigratorTrait;

    async fn test_db() -> DatabaseConnection {
        let db = Database::connect("sqlite::memory:").await.unwrap();
        crate::Migrator::up(&db, None).await.unwrap();
        db
    }

    #[tokio::test]
    async fn seed_defaults_inserts_all_data() {
        let db = test_db().await;

        let seeded = seed_if_empty(&db).await.unwrap();
        assert!(seeded, "should seed on empty DB");

        // Verify module presets (2 from guitar defaults + 19 dummy)
        let module_presets = crate::module_repo::list_module_presets(&db, None)
            .await
            .unwrap();
        assert!(
            module_presets.len() >= 2,
            "expected at least 2 module presets from defaults, got {}",
            module_presets.len()
        );

        // Verify module snapshots exist for the first module preset
        let first_mp = &module_presets[0];
        let snapshots = crate::module_repo::list_module_snapshots(&db, first_mp.id)
            .await
            .unwrap();
        assert!(
            !snapshots.is_empty(),
            "first module preset should have snapshots"
        );

        // Verify rig scenes seeded as top-level presets
        let presets = crate::preset_repo::list_presets(&db, true).await.unwrap();
        assert!(
            presets.len() >= 2,
            "expected at least 2 rig scene presets, got {}",
            presets.len()
        );

        // Verify preset snapshots (engine scenes)
        let first_preset = &presets[0];
        let preset_snaps = crate::preset_repo::list_preset_snapshots(&db, first_preset.id)
            .await
            .unwrap();
        assert!(
            !preset_snaps.is_empty(),
            "first preset should have engine scene snapshots"
        );

        // Verify profiles
        let profiles = crate::profile_repo::list_profiles(&db).await.unwrap();
        assert_eq!(profiles.len(), 1, "expected 1 profile (Live Show)");

        // Verify patches (scene templates)
        let first_profile = &profiles[0];
        let patches = crate::profile_repo::list_scene_templates(&db, first_profile.id)
            .await
            .unwrap();
        assert_eq!(patches.len(), 2, "Live Show profile should have 2 patches");

        // Verify songs
        let songs = crate::song_repo::list_songs(&db).await.unwrap();
        assert_eq!(songs.len(), 1, "expected 1 song (Amazing Grace)");

        // Verify song sections (song scenes)
        let first_song = &songs[0];
        let sections = crate::song_repo::list_song_scenes(&db, first_song.id)
            .await
            .unwrap();
        assert_eq!(sections.len(), 2, "Amazing Grace should have 2 sections");

        // Verify dummy block presets
        let block_presets = crate::block_repo::list_block_presets(&db, None)
            .await
            .unwrap();
        assert_eq!(
            block_presets.len(),
            33,
            "expected 33 dummy block presets (one per BlockType)"
        );

        // Verify each block preset has 4 snapshots
        for bp in &block_presets {
            let snaps = crate::block_repo::list_block_snapshots(&db, bp.id)
                .await
                .unwrap();
            assert_eq!(
                snaps.len(),
                4,
                "block preset '{}' should have 4 snapshots, got {}",
                bp.name,
                snaps.len()
            );
            assert!(snaps[0].is_default, "first snapshot should be default");
        }

        // Verify dummy module presets (19 module types x 1 dummy each)
        let dummy_module_presets: Vec<_> = module_presets
            .iter()
            .filter(|p| p.name.starts_with("Dummy "))
            .collect();
        assert_eq!(
            dummy_module_presets.len(),
            19,
            "expected 19 dummy module presets (one per ModuleType), got {}",
            dummy_module_presets.len()
        );

        // Verify each dummy module preset has 4 snapshots
        for dmp in &dummy_module_presets {
            let snaps = crate::module_repo::list_module_snapshots(&db, dmp.id)
                .await
                .unwrap();
            assert_eq!(
                snaps.len(),
                4,
                "dummy module preset '{}' should have 4 snapshots, got {}",
                dmp.name,
                snaps.len()
            );
            assert!(
                snaps[0].is_default,
                "first module snapshot should be default"
            );
        }
    }

    #[tokio::test]
    async fn seed_is_idempotent() {
        let db = test_db().await;

        let first = seed_if_empty(&db).await.unwrap();
        assert!(first, "first seed should insert data");

        let second = seed_if_empty(&db).await.unwrap();
        assert!(!second, "second seed should be a no-op");

        // Counts should be unchanged
        let profiles = crate::profile_repo::list_profiles(&db).await.unwrap();
        assert_eq!(profiles.len(), 1);
    }
}
