//! Signal Control — ergonomic API facade for rig control.
//!
//! Wraps the ROAM service trait behind simple async methods so consumers
//! never need to think about `roam::Context` or service plumbing.
//!
//! # Usage
//!
//! ```ignore
//! // With SQLite persistence:
//! let ctl = SignalControl::connect_db("~/Music/FastTrackStudio/signal.db").await?;
//!
//! // Or with mock data only (no database):
//! let ctl = SignalControl::mock_guitar();
//!
//! // Simple async calls — no ROAM context needed
//! let presets = ctl.get_available_presets().await;
//! let profile = ctl.get_current_profile().await;
//!
//! // DB-backed CRUD (requires connect_db):
//! let id = ctl.create_block_preset("Main EQ", "Eq", None, None, None).await?;
//! let presets = ctl.list_block_presets(Some("Eq")).await?;
//!
//! // Execute commands
//! ctl.next_song().await;
//! ctl.load_preset(preset_id).await;
//!
//! // Subscribe to events
//! let mut rx = ctl.subscribe().await;
//! while let Ok(Some(event)) = rx.recv().await {
//!     // handle event
//! }
//! ```

pub mod daw_bridge;
pub mod fx_binding;
pub mod morph_engine;
pub mod snapshot_ops;

use std::sync::Arc;
use uuid::Uuid;

// Import the trait so its methods are in scope on Arc<MockRigControlService>
use signal::RigControlService;

// Re-export domain type modules from signal-proto (via signal)
pub use signal::{
    block, category, defaults, director, id, layer, module, module_preset, non_empty, normalized,
    parameter, patch, performance, preset, profile, rig, routing, section, selection, source, tags,
    template,
};

// Re-export service/engine types that consumers need
pub use signal::engine::PreloadPriority;
pub use signal::module::ModuleType;
pub use signal::service::{
    EngineStateInfo, InstanceInfo, PreloadStatusInfo, PresetInfo, PresetSnapshotInfo, ProfileInfo,
    ProfileSceneInfo, RigControlCommand, RigControlEvent, RigInfo, SetlistInfo, SlotErrorInfo,
    SlotStateInfo, SongInfo, SwitchOutcomeInfo, TransitionResultInfo,
};

// Re-export storage entity models for UI consumers
pub use signal_storage::entities::{
    block_preset, block_snapshot, module_preset_entity, module_snapshot, performance_song,
    preset as preset_entity, profile as profile_entity, scene_template, setlist, setlist_song,
    snapshot as snapshot_entity, song_scene,
};
pub use signal_storage::DatabaseConnection;

/// Ergonomic rig control API.
///
/// Wraps any `RigControlService` implementation and provides simple
/// async methods without exposing ROAM internals. Optionally backed by
/// a SQLite database for persistent CRUD operations.
#[derive(Clone)]
pub struct SignalControl {
    service: Arc<signal::MockRigControlService>,
    db: Option<DatabaseConnection>,
}

impl PartialEq for SignalControl {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.service, &other.service)
    }
}

impl SignalControl {
    /// Create from a mock service instance (no database).
    pub fn new(service: signal::MockRigControlService) -> Self {
        Self {
            service: Arc::new(service),
            db: None,
        }
    }

    /// Create with default guitar rig mock data (no database).
    pub fn mock_guitar() -> Self {
        Self::new(signal::MockRigControlService::with_guitar_defaults())
    }

    /// Connect to a SQLite database, run migrations, seed defaults if empty,
    /// and create a `SignalControl` with both mock rig service and persistent storage.
    pub async fn connect_db(db_path: &str) -> eyre::Result<Self> {
        let db = signal_storage::connect_migrate_and_seed(db_path).await?;
        tracing::info!("signal-control: connected to {db_path}");
        Ok(Self {
            service: Arc::new(signal::MockRigControlService::with_guitar_defaults()),
            db: Some(db),
        })
    }

    /// Get a reference to the underlying service (for advanced usage).
    pub fn inner(&self) -> &Arc<signal::MockRigControlService> {
        &self.service
    }

    /// Get a reference to the database connection, if connected.
    pub fn db(&self) -> Option<&DatabaseConnection> {
        self.db.as_ref()
    }

    /// Get the database connection or return an error.
    fn require_db(&self) -> eyre::Result<&DatabaseConnection> {
        self.db
            .as_ref()
            .ok_or_else(|| eyre::eyre!("no database connection — call connect_db() first"))
    }

    // ── Mock Rig Queries ─────────────────────────────────────────────

    pub async fn get_available_profiles(&self) -> Vec<ProfileInfo> {
        self.service.get_available_profiles(&Self::cx()).await
    }

    pub async fn get_current_profile(&self) -> Option<ProfileInfo> {
        self.service.get_current_profile(&Self::cx()).await
    }

    pub async fn get_available_presets(&self) -> Vec<PresetInfo> {
        self.service.get_available_presets(&Self::cx()).await
    }

    pub async fn get_current_preset(&self) -> Option<PresetInfo> {
        self.service.get_current_preset(&Self::cx()).await
    }

    pub async fn get_current_rig(&self) -> Option<RigInfo> {
        self.service.get_current_rig(&Self::cx()).await
    }

    pub async fn get_available_setlists(&self) -> Vec<SetlistInfo> {
        self.service.get_available_setlists(&Self::cx()).await
    }

    pub async fn get_current_setlist(&self) -> Option<SetlistInfo> {
        self.service.get_current_setlist(&Self::cx()).await
    }

    pub async fn get_setlist_songs(&self) -> Vec<SongInfo> {
        self.service.get_setlist_songs(&Self::cx()).await
    }

    pub async fn get_current_song(&self) -> Option<SongInfo> {
        self.service.get_current_song(&Self::cx()).await
    }

    pub async fn get_current_scene(&self) -> Option<ProfileSceneInfo> {
        self.service.get_current_scene(&Self::cx()).await
    }

    /// Get the current preset's modules materialized for UI display.
    ///
    /// Synchronous — reads directly from the mock service's data store.
    pub fn get_current_modules(&self) -> Vec<signal::module::Module> {
        self.service.build_current_modules()
    }

    // ── Mock Rig Commands ────────────────────────────────────────────

    pub async fn execute(&self, cmd: RigControlCommand) {
        self.service.execute(&Self::cx(), cmd).await;
    }

    pub async fn load_profile(&self, profile_id: Uuid) {
        self.execute(RigControlCommand::LoadProfile { profile_id })
            .await;
    }

    pub async fn load_preset(&self, preset_id: Uuid) {
        self.execute(RigControlCommand::LoadPreset { preset_id })
            .await;
    }

    pub async fn load_preset_with_scene(&self, preset_id: Uuid, scene_index: usize) {
        self.execute(RigControlCommand::LoadPresetWithScene {
            preset_id,
            scene_index,
        })
        .await;
    }

    pub async fn next_song(&self) {
        self.execute(RigControlCommand::NextSong).await;
    }

    pub async fn previous_song(&self) {
        self.execute(RigControlCommand::PreviousSong).await;
    }

    pub async fn next_scene(&self) {
        self.execute(RigControlCommand::NextScene).await;
    }

    pub async fn previous_scene(&self) {
        self.execute(RigControlCommand::PreviousScene).await;
    }

    // ── Subscriptions ────────────────────────────────────────────────

    /// Subscribe to rig events. Returns a receiver channel.
    pub async fn subscribe(&self) -> roam::Rx<RigControlEvent> {
        let (tx, rx) = roam::channel::<RigControlEvent>();
        self.service.subscribe(&Self::cx(), tx).await;
        rx
    }

    // ── Block Preset CRUD ────────────────────────────────────────────

    pub async fn create_block_preset(
        &self,
        name: &str,
        block_type: &str,
        plugin_id: Option<serde_json::Value>,
        plugin_preset_name: Option<&str>,
        description: Option<&str>,
    ) -> eyre::Result<Uuid> {
        let db = self.require_db()?;
        Ok(signal_storage::block_repo::create_block_preset(
            db,
            name,
            block_type,
            plugin_id,
            plugin_preset_name,
            description,
            serde_json::json!([]),
        )
        .await?)
    }

    pub async fn get_block_preset(&self, id: Uuid) -> eyre::Result<Option<block_preset::Model>> {
        let db = self.require_db()?;
        Ok(signal_storage::block_repo::get_block_preset(db, id).await?)
    }

    pub async fn list_block_presets(
        &self,
        block_type_filter: Option<&str>,
    ) -> eyre::Result<Vec<block_preset::Model>> {
        let db = self.require_db()?;
        Ok(signal_storage::block_repo::list_block_presets(db, block_type_filter).await?)
    }

    pub async fn update_block_preset(
        &self,
        id: Uuid,
        name: Option<&str>,
        description: Option<Option<&str>>,
        plugin_id: Option<Option<serde_json::Value>>,
        plugin_preset_name: Option<Option<&str>>,
        tags: Option<serde_json::Value>,
    ) -> eyre::Result<()> {
        let db = self.require_db()?;
        Ok(signal_storage::block_repo::update_block_preset(
            db,
            id,
            name,
            description,
            plugin_id,
            plugin_preset_name,
            tags,
        )
        .await?)
    }

    pub async fn delete_block_preset(&self, id: Uuid) -> eyre::Result<()> {
        let db = self.require_db()?;
        Ok(signal_storage::block_repo::delete_block_preset(db, id).await?)
    }

    // ── Block Snapshot CRUD ──────────────────────────────────────────

    pub async fn create_block_snapshot(
        &self,
        block_preset_id: Uuid,
        name: &str,
        parameters: serde_json::Value,
        daw_chunk_data: Option<&str>,
        is_default: bool,
    ) -> eyre::Result<Uuid> {
        let db = self.require_db()?;
        Ok(signal_storage::block_repo::create_block_snapshot(
            db,
            block_preset_id,
            name,
            parameters,
            daw_chunk_data,
            is_default,
        )
        .await?)
    }

    pub async fn get_block_snapshot(
        &self,
        id: Uuid,
    ) -> eyre::Result<Option<block_snapshot::Model>> {
        let db = self.require_db()?;
        Ok(signal_storage::block_repo::get_block_snapshot(db, id).await?)
    }

    pub async fn list_block_snapshots(
        &self,
        block_preset_id: Uuid,
    ) -> eyre::Result<Vec<block_snapshot::Model>> {
        let db = self.require_db()?;
        Ok(signal_storage::block_repo::list_block_snapshots(db, block_preset_id).await?)
    }

    pub async fn delete_block_snapshot(&self, id: Uuid) -> eyre::Result<bool> {
        let db = self.require_db()?;
        Ok(signal_storage::block_repo::delete_block_snapshot(db, id).await?)
    }

    // ── Module Preset CRUD ───────────────────────────────────────────

    pub async fn create_module_preset(
        &self,
        name: &str,
        module_type: &str,
        description: Option<&str>,
        blocks: serde_json::Value,
        macros: serde_json::Value,
    ) -> eyre::Result<Uuid> {
        let db = self.require_db()?;
        Ok(signal_storage::module_repo::create_module_preset(
            db,
            name,
            module_type,
            description,
            blocks,
            macros,
            serde_json::json!([]),
        )
        .await?)
    }

    /// Create a module preset with a specific ID (for linking to Preset.module_assignments).
    pub async fn create_module_preset_with_id(
        &self,
        id: Uuid,
        name: &str,
        module_type: &str,
        description: Option<&str>,
        blocks: serde_json::Value,
        macros: serde_json::Value,
    ) -> eyre::Result<Uuid> {
        let db = self.require_db()?;
        Ok(signal_storage::module_repo::create_module_preset_with_id(
            db,
            id,
            name,
            module_type,
            description,
            blocks,
            macros,
            serde_json::json!([]),
        )
        .await?)
    }

    pub async fn get_module_preset(
        &self,
        id: Uuid,
    ) -> eyre::Result<Option<module_preset_entity::Model>> {
        let db = self.require_db()?;
        Ok(signal_storage::module_repo::get_module_preset(db, id).await?)
    }

    pub async fn list_module_presets(
        &self,
        module_type_filter: Option<&str>,
    ) -> eyre::Result<Vec<module_preset_entity::Model>> {
        let db = self.require_db()?;
        Ok(signal_storage::module_repo::list_module_presets(db, module_type_filter).await?)
    }

    pub async fn update_module_preset(
        &self,
        id: Uuid,
        name: Option<&str>,
        description: Option<Option<&str>>,
        blocks: Option<serde_json::Value>,
        macros: Option<serde_json::Value>,
        tags: Option<serde_json::Value>,
    ) -> eyre::Result<()> {
        let db = self.require_db()?;
        Ok(signal_storage::module_repo::update_module_preset(
            db,
            id,
            name,
            description,
            blocks,
            macros,
            tags,
        )
        .await?)
    }

    pub async fn delete_module_preset(&self, id: Uuid) -> eyre::Result<()> {
        let db = self.require_db()?;
        Ok(signal_storage::module_repo::delete_module_preset(db, id).await?)
    }

    // ── Module Snapshot CRUD ─────────────────────────────────────────

    pub async fn create_module_snapshot(
        &self,
        module_preset_id: Uuid,
        name: &str,
        block_overrides: serde_json::Value,
        is_default: bool,
    ) -> eyre::Result<Uuid> {
        let db = self.require_db()?;
        Ok(signal_storage::module_repo::create_module_snapshot(
            db,
            module_preset_id,
            name,
            block_overrides,
            is_default,
            serde_json::json!([]),
        )
        .await?)
    }

    pub async fn get_module_snapshot(
        &self,
        id: Uuid,
    ) -> eyre::Result<Option<module_snapshot::Model>> {
        let db = self.require_db()?;
        Ok(signal_storage::module_repo::get_module_snapshot(db, id).await?)
    }

    pub async fn list_module_snapshots(
        &self,
        module_preset_id: Uuid,
    ) -> eyre::Result<Vec<module_snapshot::Model>> {
        let db = self.require_db()?;
        Ok(signal_storage::module_repo::list_module_snapshots(db, module_preset_id).await?)
    }

    pub async fn delete_module_snapshot(&self, id: Uuid) -> eyre::Result<bool> {
        let db = self.require_db()?;
        Ok(signal_storage::module_repo::delete_module_snapshot(db, id).await?)
    }

    // ── Rig Preset CRUD (Facet-typed) ────────────────────────────────

    pub async fn create_rig_preset<T: for<'a> facet::Facet<'a>>(
        &self,
        name: &str,
        description: Option<&str>,
        category: serde_json::Value,
        tags: serde_json::Value,
        data: &T,
    ) -> eyre::Result<Uuid> {
        let db = self.require_db()?;
        Ok(
            signal_storage::preset_repo::create_preset(db, name, description, category, tags, data)
                .await?,
        )
    }

    pub async fn get_rig_preset<T: for<'a> facet::Facet<'a>>(
        &self,
        id: Uuid,
    ) -> eyre::Result<Option<T>> {
        let db = self.require_db()?;
        Ok(signal_storage::preset_repo::get_preset(db, id).await?)
    }

    pub async fn get_rig_preset_row(&self, id: Uuid) -> eyre::Result<Option<preset_entity::Model>> {
        let db = self.require_db()?;
        Ok(signal_storage::preset_repo::get_preset_row(db, id).await?)
    }

    pub async fn list_rig_presets(&self) -> eyre::Result<Vec<preset_entity::Model>> {
        let db = self.require_db()?;
        Ok(signal_storage::preset_repo::list_presets(db, true).await?)
    }

    pub async fn update_rig_preset_data<T: for<'a> facet::Facet<'a>>(
        &self,
        id: Uuid,
        data: &T,
    ) -> eyre::Result<()> {
        let db = self.require_db()?;
        Ok(signal_storage::preset_repo::update_preset_data(db, id, data).await?)
    }

    pub async fn update_rig_preset_metadata(
        &self,
        id: Uuid,
        name: Option<&str>,
        description: Option<Option<&str>>,
        category: Option<serde_json::Value>,
        tags: Option<serde_json::Value>,
        is_favorite: Option<bool>,
    ) -> eyre::Result<()> {
        let db = self.require_db()?;
        Ok(signal_storage::preset_repo::update_preset_metadata(
            db,
            id,
            name,
            description,
            category,
            tags,
            is_favorite,
        )
        .await?)
    }

    pub async fn delete_rig_preset(&self, id: Uuid) -> eyre::Result<()> {
        let db = self.require_db()?;
        Ok(signal_storage::preset_repo::delete_preset(db, id).await?)
    }

    // ── Rig Preset Snapshot CRUD (Facet-typed) ───────────────────────

    pub async fn save_rig_preset_snapshot<T: for<'a> facet::Facet<'a>>(
        &self,
        preset_id: Uuid,
        name: &str,
        snapshot_data: &T,
    ) -> eyre::Result<Uuid> {
        let db = self.require_db()?;
        Ok(
            signal_storage::preset_repo::save_preset_snapshot(db, preset_id, name, snapshot_data)
                .await?,
        )
    }

    pub async fn get_rig_preset_snapshot<T: for<'a> facet::Facet<'a>>(
        &self,
        id: Uuid,
    ) -> eyre::Result<Option<T>> {
        let db = self.require_db()?;
        Ok(signal_storage::preset_repo::get_preset_snapshot(db, id).await?)
    }

    pub async fn list_rig_preset_snapshots(
        &self,
        preset_id: Uuid,
    ) -> eyre::Result<Vec<snapshot_entity::Model>> {
        let db = self.require_db()?;
        Ok(signal_storage::preset_repo::list_preset_snapshots(db, preset_id).await?)
    }

    pub async fn delete_rig_preset_snapshot(&self, id: Uuid) -> eyre::Result<bool> {
        let db = self.require_db()?;
        Ok(signal_storage::preset_repo::delete_preset_snapshot(db, id).await?)
    }

    // ── Profile CRUD ─────────────────────────────────────────────────

    pub async fn create_profile(
        &self,
        name: &str,
        rig_id: Uuid,
        description: Option<&str>,
    ) -> eyre::Result<Uuid> {
        let db = self.require_db()?;
        Ok(signal_storage::profile_repo::create_profile(
            db,
            name,
            rig_id,
            description,
            serde_json::json!([]),
            serde_json::json!({}),
        )
        .await?)
    }

    pub async fn get_profile(&self, id: Uuid) -> eyre::Result<Option<profile_entity::Model>> {
        let db = self.require_db()?;
        Ok(signal_storage::profile_repo::get_profile(db, id).await?)
    }

    pub async fn list_profiles(&self) -> eyre::Result<Vec<profile_entity::Model>> {
        let db = self.require_db()?;
        Ok(signal_storage::profile_repo::list_profiles(db).await?)
    }

    pub async fn update_profile(
        &self,
        id: Uuid,
        name: Option<&str>,
        description: Option<Option<&str>>,
        tags: Option<serde_json::Value>,
        metadata: Option<serde_json::Value>,
        default_scene_template_id: Option<Option<Uuid>>,
    ) -> eyre::Result<()> {
        let db = self.require_db()?;
        Ok(signal_storage::profile_repo::update_profile(
            db,
            id,
            name,
            description,
            tags,
            metadata,
            default_scene_template_id,
        )
        .await?)
    }

    pub async fn delete_profile(&self, id: Uuid) -> eyre::Result<()> {
        let db = self.require_db()?;
        Ok(signal_storage::profile_repo::delete_profile(db, id).await?)
    }

    // ── Scene Template CRUD ──────────────────────────────────────────

    pub async fn add_scene_template(
        &self,
        profile_id: Uuid,
        name: &str,
        preset_id: Uuid,
        snapshot_id: Option<Uuid>,
        sort_order: i32,
    ) -> eyre::Result<Uuid> {
        let db = self.require_db()?;
        Ok(signal_storage::profile_repo::add_scene_template(
            db,
            profile_id,
            name,
            preset_id,
            snapshot_id,
            serde_json::json!({}),
            serde_json::json!({}),
            serde_json::json!({}),
            sort_order,
            serde_json::json!([]),
        )
        .await?)
    }

    pub async fn list_scene_templates(
        &self,
        profile_id: Uuid,
    ) -> eyre::Result<Vec<scene_template::Model>> {
        let db = self.require_db()?;
        Ok(signal_storage::profile_repo::list_scene_templates(db, profile_id).await?)
    }

    pub async fn update_scene_template(
        &self,
        id: Uuid,
        name: Option<&str>,
        preset_id: Option<Uuid>,
        snapshot_id: Option<Option<Uuid>>,
    ) -> eyre::Result<()> {
        let db = self.require_db()?;
        Ok(signal_storage::profile_repo::update_scene_template(
            db,
            id,
            name,
            preset_id,
            snapshot_id,
            None, // module_overrides
            None, // block_overrides
            None, // parameter_state
            None, // tags
        )
        .await?)
    }

    pub async fn delete_scene_template(&self, id: Uuid) -> eyre::Result<bool> {
        let db = self.require_db()?;
        Ok(signal_storage::profile_repo::delete_scene_template(db, id).await?)
    }

    pub async fn reorder_scene_templates(
        &self,
        profile_id: Uuid,
        ordered_ids: &[Uuid],
    ) -> eyre::Result<()> {
        let db = self.require_db()?;
        Ok(
            signal_storage::profile_repo::reorder_scene_templates(db, profile_id, ordered_ids)
                .await?,
        )
    }

    // ── Song CRUD ────────────────────────────────────────────────────

    pub async fn create_song(
        &self,
        name: &str,
        artist: Option<&str>,
        auto_advance: bool,
    ) -> eyre::Result<Uuid> {
        let db = self.require_db()?;
        Ok(signal_storage::song_repo::create_song(
            db,
            name,
            artist,
            auto_advance,
            serde_json::json!([]),
        )
        .await?)
    }

    pub async fn get_song(&self, id: Uuid) -> eyre::Result<Option<performance_song::Model>> {
        let db = self.require_db()?;
        Ok(signal_storage::song_repo::get_song(db, id).await?)
    }

    pub async fn list_songs(&self) -> eyre::Result<Vec<performance_song::Model>> {
        let db = self.require_db()?;
        Ok(signal_storage::song_repo::list_songs(db).await?)
    }

    pub async fn update_song(
        &self,
        id: Uuid,
        name: Option<&str>,
        artist: Option<Option<&str>>,
        auto_advance: Option<bool>,
        module_overrides: Option<serde_json::Value>,
        tags: Option<serde_json::Value>,
    ) -> eyre::Result<()> {
        let db = self.require_db()?;
        Ok(signal_storage::song_repo::update_song(
            db,
            id,
            name,
            artist,
            auto_advance,
            module_overrides,
            tags,
        )
        .await?)
    }

    pub async fn delete_song(&self, id: Uuid) -> eyre::Result<()> {
        let db = self.require_db()?;
        Ok(signal_storage::song_repo::delete_song(db, id).await?)
    }

    // ── Song Scene CRUD ──────────────────────────────────────────────

    pub async fn add_song_scene(
        &self,
        song_id: Uuid,
        name: &str,
        preset_id: Uuid,
        snapshot_id: Option<Uuid>,
        sort_order: i32,
    ) -> eyre::Result<Uuid> {
        let db = self.require_db()?;
        Ok(signal_storage::song_repo::add_song_scene(
            db,
            song_id,
            name,
            preset_id,
            snapshot_id,
            serde_json::json!({}),
            serde_json::json!([]),
            serde_json::json!({}),
            serde_json::json!({}),
            sort_order,
            serde_json::json!([]),
        )
        .await?)
    }

    pub async fn list_song_scenes(&self, song_id: Uuid) -> eyre::Result<Vec<song_scene::Model>> {
        let db = self.require_db()?;
        Ok(signal_storage::song_repo::list_song_scenes(db, song_id).await?)
    }

    pub async fn update_song_scene(
        &self,
        id: Uuid,
        name: Option<&str>,
        preset_id: Option<Uuid>,
        snapshot_id: Option<Option<Uuid>>,
    ) -> eyre::Result<()> {
        let db = self.require_db()?;
        Ok(signal_storage::song_repo::update_song_scene(
            db,
            id,
            name,
            preset_id,
            snapshot_id,
            None,
            None,
            None,
            None,
            None,
        )
        .await?)
    }

    pub async fn delete_song_scene(&self, id: Uuid) -> eyre::Result<bool> {
        let db = self.require_db()?;
        Ok(signal_storage::song_repo::delete_song_scene(db, id).await?)
    }

    pub async fn reorder_song_scenes(
        &self,
        song_id: Uuid,
        ordered_ids: &[Uuid],
    ) -> eyre::Result<()> {
        let db = self.require_db()?;
        Ok(signal_storage::song_repo::reorder_song_scenes(db, song_id, ordered_ids).await?)
    }

    // ── Setlist CRUD ─────────────────────────────────────────────────

    pub async fn create_setlist(&self, name: &str) -> eyre::Result<Uuid> {
        let db = self.require_db()?;
        Ok(signal_storage::setlist_repo::create_setlist(
            db,
            name,
            serde_json::json!({}),
            serde_json::json!([]),
        )
        .await?)
    }

    pub async fn get_setlist(&self, id: Uuid) -> eyre::Result<Option<setlist::Model>> {
        let db = self.require_db()?;
        Ok(signal_storage::setlist_repo::get_setlist(db, id).await?)
    }

    pub async fn list_setlists(&self) -> eyre::Result<Vec<setlist::Model>> {
        let db = self.require_db()?;
        Ok(signal_storage::setlist_repo::list_setlists(db).await?)
    }

    pub async fn update_setlist(
        &self,
        id: Uuid,
        name: Option<&str>,
        metadata: Option<serde_json::Value>,
        tags: Option<serde_json::Value>,
    ) -> eyre::Result<()> {
        let db = self.require_db()?;
        Ok(signal_storage::setlist_repo::update_setlist(db, id, name, metadata, tags).await?)
    }

    pub async fn delete_setlist(&self, id: Uuid) -> eyre::Result<()> {
        let db = self.require_db()?;
        Ok(signal_storage::setlist_repo::delete_setlist(db, id).await?)
    }

    // ── Setlist Song Management ──────────────────────────────────────

    pub async fn add_song_to_setlist(
        &self,
        setlist_id: Uuid,
        song_id: Uuid,
        sort_order: i32,
    ) -> eyre::Result<Uuid> {
        let db = self.require_db()?;
        Ok(
            signal_storage::setlist_repo::add_song_to_setlist(db, setlist_id, song_id, sort_order)
                .await?,
        )
    }

    pub async fn list_setlist_songs(
        &self,
        setlist_id: Uuid,
    ) -> eyre::Result<Vec<setlist_song::Model>> {
        let db = self.require_db()?;
        Ok(signal_storage::setlist_repo::list_setlist_songs(db, setlist_id).await?)
    }

    pub async fn remove_song_from_setlist(&self, setlist_song_id: Uuid) -> eyre::Result<bool> {
        let db = self.require_db()?;
        Ok(signal_storage::setlist_repo::remove_song_from_setlist(db, setlist_song_id).await?)
    }

    pub async fn reorder_setlist_songs(
        &self,
        setlist_id: Uuid,
        ordered_song_ids: &[Uuid],
    ) -> eyre::Result<()> {
        let db = self.require_db()?;
        Ok(
            signal_storage::setlist_repo::reorder_setlist_songs(db, setlist_id, ordered_song_ids)
                .await?,
        )
    }

    // ── Internal ─────────────────────────────────────────────────────

    /// Create a default ROAM context for local service calls.
    fn cx() -> roam::Context {
        roam::Context::new(
            Default::default(),
            Default::default(),
            Default::default(),
            Default::default(),
            vec![],
        )
    }
}
