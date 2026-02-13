//! Async scene repository — CRUD persistence for scene data.
//!
//! [`SceneRepository`] complements [`SceneStore`](crate::stores::SceneStore):
//!
//! - **SceneStore** is sync, returns `&T`, used by the resolver hot path.
//! - **SceneRepository** is async, returns owned `T`, used for persistence.
//!
//! On startup: load from repository → populate `InMemorySceneStore` → resolver
//! uses store synchronously. This gives sync hot-path + async persistence +
//! testable with both in-memory and SQLite backends.
//!
//! # Implementations
//!
//! - [`InMemorySceneRepository`] — for tests (wraps `InMemorySceneStore`)
//! - `SqliteSceneRepository` — in `signal-storage` crate, for production

use async_trait::async_trait;
use std::fmt;
use std::sync::RwLock;

use crate::id::*;
use crate::preset::ModulePreset;
use crate::profile::Profile;
use crate::scene::{EngineScene, LayerScene, RackScene, RigScene};
use crate::snapshot::{BlockSnapshot, ModuleSnapshot};
use crate::song::Song;
use crate::stores::{InMemorySceneStore, SceneStore};

// ─── Error type ──────────────────────────────────────────────────

/// Repository operation error.
#[derive(Debug)]
pub enum RepositoryError {
    /// Entity not found.
    NotFound { entity: &'static str, id: String },
    /// Serialization/deserialization failure.
    Serialization(String),
    /// Storage backend error (DB, I/O, etc.).
    Storage(String),
}

impl fmt::Display for RepositoryError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NotFound { entity, id } => write!(f, "{entity} not found: {id}"),
            Self::Serialization(msg) => write!(f, "serialization error: {msg}"),
            Self::Storage(msg) => write!(f, "storage error: {msg}"),
        }
    }
}

impl std::error::Error for RepositoryError {}

pub type RepositoryResult<T> = Result<T, RepositoryError>;

// ─── SceneRepository trait ───────────────────────────────────────

/// Async CRUD repository for scene, snapshot, preset, profile, and song data.
///
/// Both in-memory and SQLite implementations run the same test suite,
/// ensuring behavioral parity across backends.
#[async_trait]
pub trait SceneRepository: Send + Sync {
    // ── Scene CRUD ───────────────────────────────────────────────

    async fn save_layer_scene(&self, scene: &LayerScene) -> RepositoryResult<()>;
    async fn save_engine_scene(&self, scene: &EngineScene) -> RepositoryResult<()>;
    async fn save_rig_scene(&self, scene: &RigScene) -> RepositoryResult<()>;
    async fn save_rack_scene(&self, scene: &RackScene) -> RepositoryResult<()>;

    async fn get_layer_scene(&self, id: &LayerSceneId) -> RepositoryResult<Option<LayerScene>>;
    async fn get_engine_scene(&self, id: &EngineSceneId) -> RepositoryResult<Option<EngineScene>>;
    async fn get_rig_scene(&self, id: &RigSceneId) -> RepositoryResult<Option<RigScene>>;
    async fn get_rack_scene(&self, id: &RackSceneId) -> RepositoryResult<Option<RackScene>>;

    // ── Snapshot/Preset CRUD ─────────────────────────────────────

    async fn save_module_preset(&self, preset: &ModulePreset) -> RepositoryResult<()>;
    async fn save_module_snapshot(&self, snapshot: &ModuleSnapshot) -> RepositoryResult<()>;
    async fn save_block_snapshot(&self, snapshot: &BlockSnapshot) -> RepositoryResult<()>;

    async fn get_module_preset(
        &self,
        id: &ModulePresetId,
    ) -> RepositoryResult<Option<ModulePreset>>;
    async fn get_module_snapshot(
        &self,
        id: &ModuleSnapshotId,
    ) -> RepositoryResult<Option<ModuleSnapshot>>;
    async fn get_block_snapshot(
        &self,
        id: &BlockSnapshotId,
    ) -> RepositoryResult<Option<BlockSnapshot>>;

    async fn list_module_presets(&self) -> RepositoryResult<Vec<ModulePreset>>;

    // ── Profile/Song CRUD ────────────────────────────────────────

    async fn save_profile(&self, profile: &Profile) -> RepositoryResult<()>;
    async fn get_profile(&self, id: &ProfileId) -> RepositoryResult<Option<Profile>>;
    async fn list_profiles(&self) -> RepositoryResult<Vec<Profile>>;
    async fn delete_profile(&self, id: &ProfileId) -> RepositoryResult<bool>;

    async fn save_song(&self, song: &Song) -> RepositoryResult<()>;
    async fn get_song(&self, id: &SongId) -> RepositoryResult<Option<Song>>;
    async fn list_songs(&self) -> RepositoryResult<Vec<Song>>;
    async fn delete_song(&self, id: &SongId) -> RepositoryResult<bool>;

    // ── Bulk operations ──────────────────────────────────────────

    /// Load all scene/snapshot/preset data into an `InMemorySceneStore`.
    ///
    /// Call this at startup to populate the sync store the resolver uses.
    async fn load_all_into(&self, store: &mut InMemorySceneStore) -> RepositoryResult<()>;
}

// ─── InMemorySceneRepository ─────────────────────────────────────

/// In-memory implementation for tests — wraps `InMemorySceneStore` + domain vecs.
///
/// All operations are instant (no I/O). Uses `RwLock` for interior mutability
/// so the trait's `&self` methods can mutate internal state.
pub struct InMemorySceneRepository {
    store: RwLock<InMemorySceneStore>,
    profiles: RwLock<Vec<Profile>>,
    songs: RwLock<Vec<Song>>,
}

impl InMemorySceneRepository {
    pub fn new() -> Self {
        Self {
            store: RwLock::new(InMemorySceneStore::new()),
            profiles: RwLock::new(Vec::new()),
            songs: RwLock::new(Vec::new()),
        }
    }
}

impl Default for InMemorySceneRepository {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl SceneRepository for InMemorySceneRepository {
    // ── Scene CRUD ───────────────────────────────────────────────

    async fn save_layer_scene(&self, scene: &LayerScene) -> RepositoryResult<()> {
        self.store
            .write()
            .unwrap()
            .register_layer_scene(scene.clone());
        Ok(())
    }

    async fn save_engine_scene(&self, scene: &EngineScene) -> RepositoryResult<()> {
        self.store
            .write()
            .unwrap()
            .register_engine_scene(scene.clone());
        Ok(())
    }

    async fn save_rig_scene(&self, scene: &RigScene) -> RepositoryResult<()> {
        self.store
            .write()
            .unwrap()
            .register_rig_scene(scene.clone());
        Ok(())
    }

    async fn save_rack_scene(&self, scene: &RackScene) -> RepositoryResult<()> {
        self.store
            .write()
            .unwrap()
            .register_rack_scene(scene.clone());
        Ok(())
    }

    async fn get_layer_scene(&self, id: &LayerSceneId) -> RepositoryResult<Option<LayerScene>> {
        Ok(self.store.read().unwrap().layer_scene(id).cloned())
    }

    async fn get_engine_scene(&self, id: &EngineSceneId) -> RepositoryResult<Option<EngineScene>> {
        Ok(self.store.read().unwrap().engine_scene(id).cloned())
    }

    async fn get_rig_scene(&self, id: &RigSceneId) -> RepositoryResult<Option<RigScene>> {
        Ok(self.store.read().unwrap().rig_scene(id).cloned())
    }

    async fn get_rack_scene(&self, id: &RackSceneId) -> RepositoryResult<Option<RackScene>> {
        Ok(self.store.read().unwrap().rack_scene(id).cloned())
    }

    // ── Snapshot/Preset CRUD ─────────────────────────────────────

    async fn save_module_preset(&self, preset: &ModulePreset) -> RepositoryResult<()> {
        self.store
            .write()
            .unwrap()
            .register_module_preset(preset.clone());
        Ok(())
    }

    async fn save_module_snapshot(&self, snapshot: &ModuleSnapshot) -> RepositoryResult<()> {
        self.store
            .write()
            .unwrap()
            .register_module_snapshot(snapshot.clone());
        Ok(())
    }

    async fn save_block_snapshot(&self, snapshot: &BlockSnapshot) -> RepositoryResult<()> {
        self.store
            .write()
            .unwrap()
            .register_block_snapshot(snapshot.clone());
        Ok(())
    }

    async fn get_module_preset(
        &self,
        id: &ModulePresetId,
    ) -> RepositoryResult<Option<ModulePreset>> {
        Ok(self.store.read().unwrap().module_preset(id).cloned())
    }

    async fn get_module_snapshot(
        &self,
        id: &ModuleSnapshotId,
    ) -> RepositoryResult<Option<ModuleSnapshot>> {
        Ok(self.store.read().unwrap().module_snapshot(id).cloned())
    }

    async fn get_block_snapshot(
        &self,
        id: &BlockSnapshotId,
    ) -> RepositoryResult<Option<BlockSnapshot>> {
        Ok(self.store.read().unwrap().block_snapshot(id).cloned())
    }

    async fn list_module_presets(&self) -> RepositoryResult<Vec<ModulePreset>> {
        let store = self.store.read().unwrap();
        Ok(store.module_presets().values().cloned().collect())
    }

    // ── Profile/Song CRUD ────────────────────────────────────────

    async fn save_profile(&self, profile: &Profile) -> RepositoryResult<()> {
        let mut profiles = self.profiles.write().unwrap();
        if let Some(existing) = profiles.iter_mut().find(|p| p.id == profile.id) {
            *existing = profile.clone();
        } else {
            profiles.push(profile.clone());
        }
        Ok(())
    }

    async fn get_profile(&self, id: &ProfileId) -> RepositoryResult<Option<Profile>> {
        Ok(self
            .profiles
            .read()
            .unwrap()
            .iter()
            .find(|p| p.id == *id)
            .cloned())
    }

    async fn list_profiles(&self) -> RepositoryResult<Vec<Profile>> {
        Ok(self.profiles.read().unwrap().clone())
    }

    async fn delete_profile(&self, id: &ProfileId) -> RepositoryResult<bool> {
        let mut profiles = self.profiles.write().unwrap();
        let len_before = profiles.len();
        profiles.retain(|p| p.id != *id);
        Ok(profiles.len() < len_before)
    }

    async fn save_song(&self, song: &Song) -> RepositoryResult<()> {
        let mut songs = self.songs.write().unwrap();
        if let Some(existing) = songs.iter_mut().find(|s| s.id == song.id) {
            *existing = song.clone();
        } else {
            songs.push(song.clone());
        }
        Ok(())
    }

    async fn get_song(&self, id: &SongId) -> RepositoryResult<Option<Song>> {
        Ok(self
            .songs
            .read()
            .unwrap()
            .iter()
            .find(|s| s.id == *id)
            .cloned())
    }

    async fn list_songs(&self) -> RepositoryResult<Vec<Song>> {
        Ok(self.songs.read().unwrap().clone())
    }

    async fn delete_song(&self, id: &SongId) -> RepositoryResult<bool> {
        let mut songs = self.songs.write().unwrap();
        let len_before = songs.len();
        songs.retain(|s| s.id != *id);
        Ok(songs.len() < len_before)
    }

    // ── Bulk operations ──────────────────────────────────────────

    async fn load_all_into(&self, target: &mut InMemorySceneStore) -> RepositoryResult<()> {
        let source = self.store.read().unwrap();
        // Clone all data from our internal store into the target
        for (_, scene) in source.layer_scenes() {
            target.register_layer_scene(scene.clone());
        }
        for (_, scene) in source.engine_scenes() {
            target.register_engine_scene(scene.clone());
        }
        for (_, scene) in source.rig_scenes() {
            target.register_rig_scene(scene.clone());
        }
        for (_, scene) in source.rack_scenes() {
            target.register_rack_scene(scene.clone());
        }
        for (_, preset) in source.module_presets() {
            target.register_module_preset(preset.clone());
        }
        Ok(())
    }
}

// ─── Shared test suite ───────────────────────────────────────────
//
// These functions test any SceneRepository implementation identically.
// The `signal` crate tests them with InMemorySceneRepository.
// The `signal-storage` crate tests them with SqliteSceneRepository.

#[cfg(any(test, feature = "test-utils"))]
pub mod test_suite {
    use super::*;
    use crate::category::PresetCategory;
    use crate::module::ModuleType;
    use crate::preset::PresetMetadata;
    use crate::scene::LayerSceneBuilder;
    use crate::snapshot::ModuleSnapshot;

    /// Create a test module preset for a given module type.
    pub fn make_test_module_preset(module_type: ModuleType) -> ModulePreset {
        let snapshot = ModuleSnapshot::new("Test Snapshot", vec![]);
        ModulePreset::new(
            PresetMetadata::new("Test Preset", PresetCategory::default()),
            module_type,
            snapshot,
        )
    }

    /// Create a test profile with one patch.
    pub fn make_test_profile(name: &str) -> Profile {
        use crate::scene::LayerSceneBuilder;
        use crate::version::VersionedRef;

        let layer_scene = LayerSceneBuilder::new("Test Scene")
            .modules(vec![])
            .no_standalone_blocks()
            .build();
        let scene_ref = crate::scene::ScopedSceneRef::Layer(VersionedRef::new(layer_scene.id, 1));
        let patch = crate::profile::Patch::new("Patch 1", scene_ref);

        let mut profile = Profile::new(name);
        profile.add_patch(patch);
        profile
    }

    /// Create a test song with one section.
    pub fn make_test_song(name: &str) -> Song {
        use crate::scene::LayerSceneBuilder;
        use crate::version::VersionedRef;

        let layer_scene = LayerSceneBuilder::new("Test Scene")
            .modules(vec![])
            .no_standalone_blocks()
            .build();
        let scene_ref = crate::scene::ScopedSceneRef::Layer(VersionedRef::new(layer_scene.id, 1));
        let section = crate::song::SongSection::new("Section 1", scene_ref);

        let mut song = Song::new(name);
        song.add_section(section);
        song
    }

    // ── Shared test functions ────────────────────────────────────

    pub async fn test_save_and_load_module_preset(repo: &dyn SceneRepository) {
        let preset = make_test_module_preset(ModuleType::Drive);
        let preset_id = preset.id;

        repo.save_module_preset(&preset).await.unwrap();

        let loaded = repo.get_module_preset(&preset_id).await.unwrap();
        assert!(loaded.is_some());
        let loaded = loaded.unwrap();
        assert_eq!(loaded.id, preset_id);
        assert_eq!(loaded.module_type, ModuleType::Drive);
    }

    pub async fn test_save_and_list_module_presets(repo: &dyn SceneRepository) {
        let drive = make_test_module_preset(ModuleType::Drive);
        let amp = make_test_module_preset(ModuleType::Amp);

        repo.save_module_preset(&drive).await.unwrap();
        repo.save_module_preset(&amp).await.unwrap();

        let all = repo.list_module_presets().await.unwrap();
        assert_eq!(all.len(), 2);
    }

    pub async fn test_save_and_load_layer_scene(repo: &dyn SceneRepository) {
        let scene = LayerSceneBuilder::new("Clean Verse")
            .modules(vec![])
            .no_standalone_blocks()
            .build();
        let scene_id = scene.id;

        repo.save_layer_scene(&scene).await.unwrap();

        let loaded = repo.get_layer_scene(&scene_id).await.unwrap();
        assert!(loaded.is_some());
        assert_eq!(loaded.unwrap().name, "Clean Verse");
    }

    pub async fn test_save_and_load_profile(repo: &dyn SceneRepository) {
        let profile = make_test_profile("Test Profile");
        let profile_id = profile.id;

        repo.save_profile(&profile).await.unwrap();

        let loaded = repo.get_profile(&profile_id).await.unwrap();
        assert!(loaded.is_some());
        let loaded = loaded.unwrap();
        assert_eq!(loaded.name, "Test Profile");
        assert_eq!(loaded.patches.len(), 1);
    }

    pub async fn test_list_profiles(repo: &dyn SceneRepository) {
        let p1 = make_test_profile("Alpha");
        let p2 = make_test_profile("Beta");

        repo.save_profile(&p1).await.unwrap();
        repo.save_profile(&p2).await.unwrap();

        let all = repo.list_profiles().await.unwrap();
        assert_eq!(all.len(), 2);
    }

    pub async fn test_delete_profile(repo: &dyn SceneRepository) {
        let profile = make_test_profile("Deletable");
        let profile_id = profile.id;

        repo.save_profile(&profile).await.unwrap();
        assert!(repo.get_profile(&profile_id).await.unwrap().is_some());

        let deleted = repo.delete_profile(&profile_id).await.unwrap();
        assert!(deleted);
        assert!(repo.get_profile(&profile_id).await.unwrap().is_none());
    }

    pub async fn test_update_profile(repo: &dyn SceneRepository) {
        let mut profile = make_test_profile("Original");
        let profile_id = profile.id;

        repo.save_profile(&profile).await.unwrap();

        profile.name = "Updated".to_string();
        repo.save_profile(&profile).await.unwrap();

        let loaded = repo.get_profile(&profile_id).await.unwrap().unwrap();
        assert_eq!(loaded.name, "Updated");
    }

    pub async fn test_save_and_load_song(repo: &dyn SceneRepository) {
        let song = make_test_song("Test Song");
        let song_id = song.id;

        repo.save_song(&song).await.unwrap();

        let loaded = repo.get_song(&song_id).await.unwrap();
        assert!(loaded.is_some());
        let loaded = loaded.unwrap();
        assert_eq!(loaded.name, "Test Song");
        assert_eq!(loaded.sections.len(), 1);
    }

    pub async fn test_load_all_into(repo: &dyn SceneRepository) {
        let preset = make_test_module_preset(ModuleType::Drive);
        let scene = LayerSceneBuilder::new("Loaded Scene")
            .modules(vec![])
            .no_standalone_blocks()
            .build();

        let preset_id = preset.id;
        let scene_id = scene.id;

        repo.save_module_preset(&preset).await.unwrap();
        repo.save_layer_scene(&scene).await.unwrap();

        let mut store = InMemorySceneStore::new();
        repo.load_all_into(&mut store).await.unwrap();

        assert!(store.module_preset(&preset_id).is_some());
        assert!(store.layer_scene(&scene_id).is_some());
    }
}

// ─── Tests ───────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::test_suite::*;
    use super::*;

    #[tokio::test]
    async fn in_memory_save_and_load_module_preset() {
        test_save_and_load_module_preset(&InMemorySceneRepository::new()).await;
    }

    #[tokio::test]
    async fn in_memory_save_and_list_module_presets() {
        test_save_and_list_module_presets(&InMemorySceneRepository::new()).await;
    }

    #[tokio::test]
    async fn in_memory_save_and_load_layer_scene() {
        test_save_and_load_layer_scene(&InMemorySceneRepository::new()).await;
    }

    #[tokio::test]
    async fn in_memory_save_and_load_profile() {
        test_save_and_load_profile(&InMemorySceneRepository::new()).await;
    }

    #[tokio::test]
    async fn in_memory_list_profiles() {
        test_list_profiles(&InMemorySceneRepository::new()).await;
    }

    #[tokio::test]
    async fn in_memory_delete_profile() {
        test_delete_profile(&InMemorySceneRepository::new()).await;
    }

    #[tokio::test]
    async fn in_memory_update_profile() {
        test_update_profile(&InMemorySceneRepository::new()).await;
    }

    #[tokio::test]
    async fn in_memory_save_and_load_song() {
        test_save_and_load_song(&InMemorySceneRepository::new()).await;
    }

    #[tokio::test]
    async fn in_memory_load_all_into() {
        test_load_all_into(&InMemorySceneRepository::new()).await;
    }
}
