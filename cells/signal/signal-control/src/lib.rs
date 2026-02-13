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
pub mod standalone;

use std::sync::Arc;
use uuid::Uuid;

use signal::id::{ProfileId, RigPresetId};
// Import the trait so its methods are in scope
use signal::RigControlService;

pub use standalone::StandaloneRigControlService;

// Re-export domain type modules from signal-proto (via signal)
pub use signal::{
    active, automation, block, category, container, defaults, director, id, layer, midi, module,
    non_empty, normalized, override_tree, parameter, preset, profile, rack, rating, rig, routing,
    scene, snapshot, song, source, tags, template, version,
};

// Re-export service/engine types that consumers need
pub use signal::engine::PreloadPriority;
pub use signal::module::ModuleType;
pub use signal::service::{
    EngineStateInfo, InstanceInfo, PatchInfo, PreloadStatusInfo, PresetSceneInfo, ProfileInfo,
    RigControlCommand, RigControlEvent, RigInfo, RigPresetInfo, SectionInfo, SetlistInfo,
    SlotErrorInfo, SlotStateInfo, SongInfo, SwitchOutcomeInfo, TransitionResultInfo,
};

// Re-export storage entity models for UI consumers
pub use signal_storage::entities::{
    block_preset, block_snapshot, module_preset_entity, module_snapshot, performance_song,
    preset as preset_entity, profile as profile_entity, scene_template, setlist, setlist_song,
    snapshot as snapshot_entity, song_scene,
};
pub use signal_storage::DatabaseConnection;

// ── Service Backend Dispatch ─────────────────────────────────────────

/// The concrete service backend: either mock (in-memory) or standalone (DB-backed).
enum ServiceImpl {
    Mock(signal::MockRigControlService),
    Standalone(standalone::StandaloneRigControlService),
}

impl ServiceImpl {
    /// Get the database connection, if this backend has one.
    fn db(&self) -> Option<&DatabaseConnection> {
        match self {
            ServiceImpl::Mock(_) => None,
            ServiceImpl::Standalone(s) => Some(s.db()),
        }
    }

    /// Broadcast an event through the active backend's event subscriber.
    fn broadcast_event(&self, event: RigControlEvent) {
        match self {
            ServiceImpl::Mock(s) => s.broadcast_event(event),
            ServiceImpl::Standalone(s) => s.broadcast_event(event),
        }
    }
}

/// Dispatch a trait method call to whichever backend is active.
macro_rules! dispatch {
    ($self:expr, $method:ident $(, $arg:expr)*) => {
        match &*$self.service {
            ServiceImpl::Mock(s) => s.$method(&SignalControl::cx(), $($arg),*).await,
            ServiceImpl::Standalone(s) => s.$method(&SignalControl::cx(), $($arg),*).await,
        }
    };
}

/// Ergonomic rig control API.
///
/// Wraps any `RigControlService` implementation (mock or DB-backed) and
/// provides simple async methods without exposing ROAM internals.
#[derive(Clone)]
pub struct SignalControl {
    service: Arc<ServiceImpl>,
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
            service: Arc::new(ServiceImpl::Mock(service)),
        }
    }

    /// Create from a standalone (DB-backed) service.
    pub fn standalone(service: standalone::StandaloneRigControlService) -> Self {
        Self {
            service: Arc::new(ServiceImpl::Standalone(service)),
        }
    }

    /// Create with default guitar rig mock data (no database).
    pub fn mock_guitar() -> Self {
        Self::new(signal::MockRigControlService::with_guitar_defaults())
    }

    /// Create with bass rig mock data (no database).
    pub fn mock_bass() -> Self {
        Self::new(signal::MockRigControlService::with_bass_defaults())
    }

    /// Create with vocal rig mock data (no database).
    pub fn mock_vocals() -> Self {
        Self::new(signal::MockRigControlService::with_vocal_defaults())
    }

    /// Create with keys rig mock data (no database).
    pub fn mock_keys() -> Self {
        Self::new(signal::MockRigControlService::with_keys_defaults())
    }

    /// Create with synth rig mock data (no database).
    pub fn mock_synth() -> Self {
        Self::new(signal::MockRigControlService::with_synth_defaults())
    }

    /// Create with drums rig mock data (no database).
    pub fn mock_drums() -> Self {
        Self::new(signal::MockRigControlService::with_drums_defaults())
    }

    /// Create with drum replacement rig mock data (no database).
    pub fn mock_drum_replacement() -> Self {
        Self::new(signal::MockRigControlService::with_drum_replacement_defaults())
    }

    /// Create with synth bass rig mock data (no database).
    pub fn mock_synth_bass() -> Self {
        Self::new(signal::MockRigControlService::with_synth_bass_defaults())
    }

    /// Connect to a SQLite database, run migrations, seed defaults,
    /// and create a `SignalControl` with DB-backed standalone service.
    pub async fn connect_db(db_path: &str) -> eyre::Result<Self> {
        Self::connect_db_for_type(db_path, "guitar").await
    }

    /// Connect to a SQLite database for a specific instrument type.
    pub async fn connect_db_for_type(db_path: &str, instrument_type: &str) -> eyre::Result<Self> {
        let db = signal_storage::connect_migrate_and_seed(db_path).await?;
        tracing::info!("signal-control: connected to {db_path} (instrument: {instrument_type})");
        let svc = standalone::StandaloneRigControlService::new(db, instrument_type).await?;
        Ok(Self::standalone(svc))
    }

    /// Get a reference to the database connection, if the backend has one.
    pub fn db(&self) -> Option<&DatabaseConnection> {
        self.service.db()
    }

    /// Get the database connection or return an error.
    fn require_db(&self) -> eyre::Result<&DatabaseConnection> {
        self.service
            .db()
            .ok_or_else(|| eyre::eyre!("no database connection — using mock service"))
    }

    // ── Service Queries (dispatched to active backend) ──────────────

    pub async fn get_available_profiles(&self) -> Vec<ProfileInfo> {
        dispatch!(self, get_available_profiles)
    }

    pub async fn get_current_profile(&self) -> Option<ProfileInfo> {
        dispatch!(self, get_current_profile)
    }

    pub async fn get_current_patch(&self) -> Option<PatchInfo> {
        dispatch!(self, get_current_patch)
    }

    pub async fn get_current_rig(&self) -> Option<RigInfo> {
        dispatch!(self, get_current_rig)
    }

    pub async fn get_available_setlists(&self) -> Vec<SetlistInfo> {
        dispatch!(self, get_available_setlists)
    }

    pub async fn get_current_setlist(&self) -> Option<SetlistInfo> {
        dispatch!(self, get_current_setlist)
    }

    pub async fn get_setlist_songs(&self) -> Vec<SongInfo> {
        dispatch!(self, get_setlist_songs)
    }

    pub async fn get_current_song(&self) -> Option<SongInfo> {
        dispatch!(self, get_current_song)
    }

    pub async fn get_current_section(&self) -> Option<SectionInfo> {
        dispatch!(self, get_current_section)
    }

    pub async fn get_available_presets(&self) -> Vec<RigPresetInfo> {
        dispatch!(self, get_available_presets)
    }

    /// Get the current preset's modules materialized for UI display.
    ///
    /// Only meaningful for mock backends (which have an in-memory rig).
    /// Standalone returns empty — modules come from FX binding.
    pub fn get_current_modules(&self) -> Vec<signal::module::Module> {
        match &*self.service {
            ServiceImpl::Mock(s) => s.build_current_modules(),
            ServiceImpl::Standalone(_) => Vec::new(),
        }
    }

    // ── Service Commands (dispatched to active backend) ──────────────

    pub async fn execute(&self, cmd: RigControlCommand) {
        dispatch!(self, execute, cmd);
    }

    pub async fn load_profile(&self, profile_id: ProfileId) {
        self.execute(RigControlCommand::LoadProfile { profile_id })
            .await;
    }

    pub async fn select_preset(&self, preset_id: RigPresetId) {
        self.execute(RigControlCommand::SelectPreset { preset_id })
            .await;

        // Build modules from DB (standalone backend) or mock rig (mock backend).
        // Broadcast ModulesChanged so the event handler can update UI signals.
        let modules = self.build_preset_modules(preset_id).await;
        self.service
            .broadcast_event(RigControlEvent::ModulesChanged { modules });
    }

    pub async fn load_patch(&self, profile_id: ProfileId, patch_index: usize) {
        self.execute(RigControlCommand::LoadPatch {
            profile_id,
            patch_index,
        })
        .await;
    }

    pub async fn load_song_section(&self, song_index: usize, section_index: usize) {
        self.execute(RigControlCommand::LoadSongSection {
            song_index,
            section_index,
        })
        .await;
    }

    pub async fn next_song(&self) {
        self.execute(RigControlCommand::NextSong).await;
    }

    pub async fn previous_song(&self) {
        self.execute(RigControlCommand::PreviousSong).await;
    }

    pub async fn next_section(&self) {
        self.execute(RigControlCommand::NextSection).await;
    }

    pub async fn previous_section(&self) {
        self.execute(RigControlCommand::PreviousSection).await;
    }

    // ── Module Building ─────────────────────────────────────────────

    /// Build modules for a preset from the DB, or from the mock rig.
    ///
    /// For DB-backed presets: loads the preset row, parses `module_assignments`
    /// from the JSON data blob, loads each `ModulePreset`, and reconstructs
    /// `Module` objects with blocks parsed from JSON.
    ///
    /// For mock backends (no DB): returns the rig's physical modules.
    pub async fn build_preset_modules(
        &self,
        preset_id: RigPresetId,
    ) -> Vec<signal::module::Module> {
        use signal::block::{Block, BlockType, PluginId};
        use signal::module::{Module, ModuleBlock, ModuleType};
        use signal::normalized::Order;

        // Try DB path first
        let row = match self.get_rig_preset_row(preset_id.as_uuid()).await {
            Ok(Some(r)) => Some(r),
            Ok(None) => None,
            Err(_) => None,
        };

        if let Some(row) = row {
            let assignments = match row
                .data
                .get("module_assignments")
                .and_then(|v| v.as_array())
            {
                Some(arr) => arr.clone(),
                None => return Vec::new(),
            };

            let mut modules = Vec::new();

            for assignment in &assignments {
                let enabled = assignment
                    .get("enabled")
                    .and_then(|v| v.as_bool())
                    .unwrap_or(true);
                if !enabled {
                    continue;
                }

                let Some(mp_id_str) = assignment.get("module_preset_id").and_then(|v| {
                    v.as_str()
                        .or_else(|| v.get("uuid").and_then(|u| u.as_str()))
                }) else {
                    continue;
                };
                let Ok(mp_id) = mp_id_str.parse::<Uuid>() else {
                    tracing::warn!("build_preset_modules: invalid module_preset_id: {mp_id_str}");
                    continue;
                };
                let mp_row = match self.get_module_preset(mp_id).await {
                    Ok(Some(row)) => row,
                    Ok(None) => {
                        tracing::warn!(
                            "build_preset_modules: module preset {mp_id} not found in DB"
                        );
                        continue;
                    }
                    Err(e) => {
                        tracing::warn!(
                            "build_preset_modules: failed to load module preset {mp_id}: {e}"
                        );
                        continue;
                    }
                };

                let module_type = mp_row.module_type_parsed().unwrap_or_else(|| {
                    tracing::warn!(
                        "build_preset_modules: unknown module_type '{}' for preset {}, using Custom",
                        mp_row.module_type,
                        mp_row.id,
                    );
                    ModuleType::Custom
                });
                let mut module = Module::new(&mp_row.name, module_type);

                if let Some(gw) = mp_row.macros.get("grid_width").and_then(|v| v.as_u64()) {
                    module.grid_width = Some(gw as usize);
                }
                if let Some(gh) = mp_row.macros.get("grid_height").and_then(|v| v.as_u64()) {
                    module.grid_height = Some(gh as usize);
                }

                if let Some(blocks_arr) = mp_row.blocks.as_array() {
                    for block_json in blocks_arr {
                        let block_obj = block_json.get("block").unwrap_or(block_json);
                        let name = block_obj
                            .get("name")
                            .and_then(|v| v.as_str())
                            .unwrap_or("Unknown")
                            .to_string();
                        let block_type_str = block_obj
                            .get("block_type")
                            .and_then(|v| v.as_str())
                            .unwrap_or("Custom");
                        let block_type =
                            BlockType::from_variant_name(block_type_str).unwrap_or_else(|| {
                                tracing::warn!(
                                    "build_preset_modules: unknown block_type '{block_type_str}' in module '{}', using Custom",
                                    mp_row.name,
                                );
                                BlockType::Custom
                            });
                        let alias = block_obj
                            .get("alias")
                            .and_then(|v| v.as_str())
                            .map(String::from);
                        let description = block_obj
                            .get("description")
                            .and_then(|v| v.as_str())
                            .map(String::from);

                        let mut block =
                            Block::new(name, PluginId::unassigned()).with_block_type(block_type);
                        block.alias = alias;
                        block.description = description;
                        let order = Order::new(module.blocks.len() as u8);
                        let mut mb = ModuleBlock::new(block, order);

                        mb.local_col = block_json
                            .get("local_col")
                            .and_then(|v| v.as_u64())
                            .map(|v| v as usize);
                        mb.local_row = block_json
                            .get("local_row")
                            .and_then(|v| v.as_u64())
                            .map(|v| v as usize);

                        module.add_block(mb);
                    }
                }

                modules.push(module);
            }

            modules
        } else {
            // No DB row — fall back to mock rig's physical modules
            self.get_current_modules()
        }
    }

    // ── Subscriptions ────────────────────────────────────────────────

    /// Subscribe to rig events. Returns a receiver channel.
    pub async fn subscribe(&self) -> roam::Rx<RigControlEvent> {
        let (tx, rx) = roam::channel::<RigControlEvent>();
        dispatch!(self, subscribe, tx);
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

    /// List rig presets filtered by instrument type (e.g. "guitar", "bass").
    pub async fn list_rig_presets_by_type(
        &self,
        instrument_type: &str,
    ) -> eyre::Result<Vec<preset_entity::Model>> {
        let db = self.require_db()?;
        Ok(signal_storage::preset_repo::list_presets_by_type(db, instrument_type).await?)
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
        is_default: bool,
    ) -> eyre::Result<Uuid> {
        let db = self.require_db()?;
        Ok(signal_storage::preset_repo::save_preset_snapshot(
            db,
            preset_id,
            name,
            snapshot_data,
            is_default,
        )
        .await?)
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

    /// List profiles filtered by instrument type (e.g. "guitar", "bass").
    pub async fn list_profiles_by_type(
        &self,
        instrument_type: &str,
    ) -> eyre::Result<Vec<profile_entity::Model>> {
        let db = self.require_db()?;
        Ok(signal_storage::profile_repo::list_profiles_by_type(db, instrument_type).await?)
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

    /// List songs filtered by instrument type (e.g. "guitar", "bass").
    pub async fn list_songs_by_type(
        &self,
        instrument_type: &str,
    ) -> eyre::Result<Vec<performance_song::Model>> {
        let db = self.require_db()?;
        Ok(signal_storage::song_repo::list_songs_by_type(db, instrument_type).await?)
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
        is_default: bool,
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
            is_default,
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
