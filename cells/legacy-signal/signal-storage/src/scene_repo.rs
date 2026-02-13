//! SQLite-backed scene repository — implements [`SceneRepository`] using the KV store.
//!
//! Uses the existing [`Persistable`] abstraction (key-value with Facet serialization)
//! rather than dedicated relational tables. This means zero schema changes — all scene
//! data is stored as JSON blobs in the `kv_store` table with namespaced keys:
//!
//! ```text
//! scene:layer:{id}      → LayerScene
//! scene:engine:{id}     → EngineScene
//! scene:rig:{id}        → RigScene
//! scene:rack:{id}       → RackScene
//! snapshot:module:{id}  → ModuleSnapshot
//! snapshot:block:{id}   → BlockSnapshot
//! preset:module:{id}    → ModulePreset
//! profile:{id}          → Profile
//! song:{id}             → Song
//! ```
//!
//! When query patterns emerge (e.g., "list all scenes for a profile"), dedicated
//! tables can be added. The KV approach provides a working persistence layer now
//! while preserving the same test suite parity as the in-memory implementation.

use async_trait::async_trait;

use signal::id::*;
use signal::preset::ModulePreset;
use signal::profile::Profile;
use signal::repository::{RepositoryError, RepositoryResult, SceneRepository};
use signal::scene::{EngineScene, LayerScene, RackScene, RigScene};
use signal::snapshot::{BlockSnapshot, ModuleSnapshot};
use signal::song::Song;
use signal::stores::InMemorySceneStore;

use crate::persist::{load_value, save_value, Persistable};

// ─── Key conventions ─────────────────────────────────────────────

fn layer_scene_key(id: &LayerSceneId) -> String {
    format!("scene:layer:{}", id.as_uuid())
}

fn engine_scene_key(id: &EngineSceneId) -> String {
    format!("scene:engine:{}", id.as_uuid())
}

fn rig_scene_key(id: &RigSceneId) -> String {
    format!("scene:rig:{}", id.as_uuid())
}

fn rack_scene_key(id: &RackSceneId) -> String {
    format!("scene:rack:{}", id.as_uuid())
}

fn module_snapshot_key(id: &ModuleSnapshotId) -> String {
    format!("snapshot:module:{}", id.as_uuid())
}

fn block_snapshot_key(id: &BlockSnapshotId) -> String {
    format!("snapshot:block:{}", id.as_uuid())
}

fn module_preset_key(id: &ModulePresetId) -> String {
    format!("preset:module:{}", id.as_uuid())
}

fn profile_key(id: &ProfileId) -> String {
    format!("profile:{}", id.as_uuid())
}

fn song_key(id: &SongId) -> String {
    format!("song:{}", id.as_uuid())
}

// ─── SqliteSceneRepository ───────────────────────────────────────

/// SQLite-backed [`SceneRepository`] using the KV persistence layer.
///
/// All domain types are serialized via Facet into JSON and stored in the
/// `kv_store` table. This reuses the existing `SqliteBackend` infrastructure
/// and requires zero schema migrations.
pub struct SqliteSceneRepository<P: Persistable> {
    store: P,
}

impl<P: Persistable> SqliteSceneRepository<P> {
    /// Create a new repository wrapping the given persistence backend.
    pub fn new(store: P) -> Self {
        Self { store }
    }
}

/// Convert a storage error into a repository error.
fn map_storage_err(e: crate::error::StorageError) -> RepositoryError {
    RepositoryError::Storage(e.to_string())
}

#[async_trait]
impl<P: Persistable> SceneRepository for SqliteSceneRepository<P> {
    // ── Scene CRUD ───────────────────────────────────────────────

    async fn save_layer_scene(&self, scene: &LayerScene) -> RepositoryResult<()> {
        save_value(&self.store, &layer_scene_key(&scene.id), scene)
            .await
            .map_err(map_storage_err)
    }

    async fn save_engine_scene(&self, scene: &EngineScene) -> RepositoryResult<()> {
        save_value(&self.store, &engine_scene_key(&scene.id), scene)
            .await
            .map_err(map_storage_err)
    }

    async fn save_rig_scene(&self, scene: &RigScene) -> RepositoryResult<()> {
        save_value(&self.store, &rig_scene_key(&scene.id), scene)
            .await
            .map_err(map_storage_err)
    }

    async fn save_rack_scene(&self, scene: &RackScene) -> RepositoryResult<()> {
        save_value(&self.store, &rack_scene_key(&scene.id), scene)
            .await
            .map_err(map_storage_err)
    }

    async fn get_layer_scene(&self, id: &LayerSceneId) -> RepositoryResult<Option<LayerScene>> {
        load_value(&self.store, &layer_scene_key(id))
            .await
            .map_err(map_storage_err)
    }

    async fn get_engine_scene(&self, id: &EngineSceneId) -> RepositoryResult<Option<EngineScene>> {
        load_value(&self.store, &engine_scene_key(id))
            .await
            .map_err(map_storage_err)
    }

    async fn get_rig_scene(&self, id: &RigSceneId) -> RepositoryResult<Option<RigScene>> {
        load_value(&self.store, &rig_scene_key(id))
            .await
            .map_err(map_storage_err)
    }

    async fn get_rack_scene(&self, id: &RackSceneId) -> RepositoryResult<Option<RackScene>> {
        load_value(&self.store, &rack_scene_key(id))
            .await
            .map_err(map_storage_err)
    }

    // ── Snapshot/Preset CRUD ─────────────────────────────────────

    async fn save_module_preset(&self, preset: &ModulePreset) -> RepositoryResult<()> {
        save_value(&self.store, &module_preset_key(&preset.id), preset)
            .await
            .map_err(map_storage_err)
    }

    async fn save_module_snapshot(&self, snapshot: &ModuleSnapshot) -> RepositoryResult<()> {
        save_value(&self.store, &module_snapshot_key(&snapshot.id), snapshot)
            .await
            .map_err(map_storage_err)
    }

    async fn save_block_snapshot(&self, snapshot: &BlockSnapshot) -> RepositoryResult<()> {
        save_value(&self.store, &block_snapshot_key(&snapshot.id()), snapshot)
            .await
            .map_err(map_storage_err)
    }

    async fn get_module_preset(
        &self,
        id: &ModulePresetId,
    ) -> RepositoryResult<Option<ModulePreset>> {
        load_value(&self.store, &module_preset_key(id))
            .await
            .map_err(map_storage_err)
    }

    async fn get_module_snapshot(
        &self,
        id: &ModuleSnapshotId,
    ) -> RepositoryResult<Option<ModuleSnapshot>> {
        load_value(&self.store, &module_snapshot_key(id))
            .await
            .map_err(map_storage_err)
    }

    async fn get_block_snapshot(
        &self,
        id: &BlockSnapshotId,
    ) -> RepositoryResult<Option<BlockSnapshot>> {
        load_value(&self.store, &block_snapshot_key(id))
            .await
            .map_err(map_storage_err)
    }

    async fn list_module_presets(&self) -> RepositoryResult<Vec<ModulePreset>> {
        let keys = self
            .store
            .list_keys(Some("preset:module:"))
            .await
            .map_err(map_storage_err)?;

        let mut presets = Vec::with_capacity(keys.len());
        for key in &keys {
            if let Some(preset) = load_value::<ModulePreset>(&self.store, key)
                .await
                .map_err(map_storage_err)?
            {
                presets.push(preset);
            }
        }
        Ok(presets)
    }

    // ── Profile/Song CRUD ────────────────────────────────────────

    async fn save_profile(&self, profile: &Profile) -> RepositoryResult<()> {
        save_value(&self.store, &profile_key(&profile.id), profile)
            .await
            .map_err(map_storage_err)
    }

    async fn get_profile(&self, id: &ProfileId) -> RepositoryResult<Option<Profile>> {
        load_value(&self.store, &profile_key(id))
            .await
            .map_err(map_storage_err)
    }

    async fn list_profiles(&self) -> RepositoryResult<Vec<Profile>> {
        let keys = self
            .store
            .list_keys(Some("profile:"))
            .await
            .map_err(map_storage_err)?;

        let mut profiles = Vec::with_capacity(keys.len());
        for key in &keys {
            if let Some(profile) = load_value::<Profile>(&self.store, key)
                .await
                .map_err(map_storage_err)?
            {
                profiles.push(profile);
            }
        }
        Ok(profiles)
    }

    async fn delete_profile(&self, id: &ProfileId) -> RepositoryResult<bool> {
        self.store
            .delete(&profile_key(id))
            .await
            .map_err(map_storage_err)
    }

    async fn save_song(&self, song: &Song) -> RepositoryResult<()> {
        save_value(&self.store, &song_key(&song.id), song)
            .await
            .map_err(map_storage_err)
    }

    async fn get_song(&self, id: &SongId) -> RepositoryResult<Option<Song>> {
        load_value(&self.store, &song_key(id))
            .await
            .map_err(map_storage_err)
    }

    async fn list_songs(&self) -> RepositoryResult<Vec<Song>> {
        let keys = self
            .store
            .list_keys(Some("song:"))
            .await
            .map_err(map_storage_err)?;

        let mut songs = Vec::with_capacity(keys.len());
        for key in &keys {
            if let Some(song) = load_value::<Song>(&self.store, key)
                .await
                .map_err(map_storage_err)?
            {
                songs.push(song);
            }
        }
        Ok(songs)
    }

    async fn delete_song(&self, id: &SongId) -> RepositoryResult<bool> {
        self.store
            .delete(&song_key(id))
            .await
            .map_err(map_storage_err)
    }

    // ── Bulk operations ──────────────────────────────────────────

    async fn load_all_into(&self, target: &mut InMemorySceneStore) -> RepositoryResult<()> {
        // Load all layer scenes
        let keys = self
            .store
            .list_keys(Some("scene:layer:"))
            .await
            .map_err(map_storage_err)?;
        for key in &keys {
            if let Some(scene) = load_value::<LayerScene>(&self.store, key)
                .await
                .map_err(map_storage_err)?
            {
                target.register_layer_scene(scene);
            }
        }

        // Load all engine scenes
        let keys = self
            .store
            .list_keys(Some("scene:engine:"))
            .await
            .map_err(map_storage_err)?;
        for key in &keys {
            if let Some(scene) = load_value::<EngineScene>(&self.store, key)
                .await
                .map_err(map_storage_err)?
            {
                target.register_engine_scene(scene);
            }
        }

        // Load all rig scenes
        let keys = self
            .store
            .list_keys(Some("scene:rig:"))
            .await
            .map_err(map_storage_err)?;
        for key in &keys {
            if let Some(scene) = load_value::<RigScene>(&self.store, key)
                .await
                .map_err(map_storage_err)?
            {
                target.register_rig_scene(scene);
            }
        }

        // Load all rack scenes
        let keys = self
            .store
            .list_keys(Some("scene:rack:"))
            .await
            .map_err(map_storage_err)?;
        for key in &keys {
            if let Some(scene) = load_value::<RackScene>(&self.store, key)
                .await
                .map_err(map_storage_err)?
            {
                target.register_rack_scene(scene);
            }
        }

        // Load all module presets (auto-registers embedded snapshots)
        let keys = self
            .store
            .list_keys(Some("preset:module:"))
            .await
            .map_err(map_storage_err)?;
        for key in &keys {
            if let Some(preset) = load_value::<ModulePreset>(&self.store, key)
                .await
                .map_err(map_storage_err)?
            {
                target.register_module_preset(preset);
            }
        }

        Ok(())
    }
}

// ─── Tests ───────────────────────────────────────────────────────
//
// Runs the same shared test suite as InMemorySceneRepository,
// proving behavioral parity across both backends.

#[cfg(test)]
mod tests {
    use super::*;
    use crate::persist::MemoryBackend;
    use signal::repository::test_suite::*;

    /// Create a SqliteSceneRepository backed by MemoryBackend for tests.
    fn test_repo() -> SqliteSceneRepository<MemoryBackend> {
        SqliteSceneRepository::new(MemoryBackend::new())
    }

    #[tokio::test]
    async fn sqlite_save_and_load_module_preset() {
        test_save_and_load_module_preset(&test_repo()).await;
    }

    #[tokio::test]
    async fn sqlite_save_and_list_module_presets() {
        test_save_and_list_module_presets(&test_repo()).await;
    }

    #[tokio::test]
    async fn sqlite_save_and_load_layer_scene() {
        test_save_and_load_layer_scene(&test_repo()).await;
    }

    #[tokio::test]
    async fn sqlite_save_and_load_profile() {
        test_save_and_load_profile(&test_repo()).await;
    }

    #[tokio::test]
    async fn sqlite_list_profiles() {
        test_list_profiles(&test_repo()).await;
    }

    #[tokio::test]
    async fn sqlite_delete_profile() {
        test_delete_profile(&test_repo()).await;
    }

    #[tokio::test]
    async fn sqlite_update_profile() {
        test_update_profile(&test_repo()).await;
    }

    #[tokio::test]
    async fn sqlite_save_and_load_song() {
        test_save_and_load_song(&test_repo()).await;
    }

    #[tokio::test]
    async fn sqlite_load_all_into() {
        test_load_all_into(&test_repo()).await;
    }
}
