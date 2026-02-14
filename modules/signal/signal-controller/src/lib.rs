//! Service-driven controller for the signal domain.
//!
//! The controller exposes **collection / variant** terminology:
//!
//! - A **collection** is a named group of variants (Preset, ModulePreset, Layer, Engine, Rig, Profile, Song).
//! - A **variant** is a specific snapshot within a collection (Snapshot, ModuleSnapshot, LayerSnapshot, EngineScene, RigScene, Patch, Section).
//!
//! Each domain level (block, module, layer, engine, preset/rig, profile, song) gets its own method region.

use signal_live::SignalLive;
use signal_proto::{
    engine::{Engine, EngineId, EngineScene, EngineSceneId},
    layer::{Layer, LayerId, LayerSnapshot, LayerSnapshotId},
    profile::{Patch, PatchId, Profile, ProfileId},
    rig::{Rig, RigId, RigScene, RigSceneId},
    song::{Section, SectionId, Song, SongId},
    tagging::{BrowserHit, BrowserIndex, BrowserQuery},
    Block, BlockService, BlockType, BrowserService, EngineService, LayerService, ModulePreset,
    ModulePresetId, ModuleSnapshot, ModuleSnapshotId, Preset, PresetId, PresetService,
    ProfileService, SnapshotId, SongService,
};
use std::sync::Arc;

pub trait ContextFactory: Send + Sync {
    fn make_context(&self) -> roam::Context;
}

pub type SharedContextFactory = Arc<dyn ContextFactory>;

#[derive(Default)]
pub struct DefaultContextFactory;

impl ContextFactory for DefaultContextFactory {
    fn make_context(&self) -> roam::Context {
        roam::Context::new(
            Default::default(),
            Default::default(),
            Default::default(),
            Default::default(),
            vec![],
        )
    }
}

pub struct SignalController<S = SignalLive>
where
    S: BlockService + LayerService + EngineService + PresetService + ProfileService + SongService,
{
    service: Arc<S>,
    context_factory: SharedContextFactory,
}

impl<S> Clone for SignalController<S>
where
    S: BlockService + LayerService + EngineService + PresetService + ProfileService + SongService,
{
    fn clone(&self) -> Self {
        Self {
            service: self.service.clone(),
            context_factory: self.context_factory.clone(),
        }
    }
}

impl<S> PartialEq for SignalController<S>
where
    S: BlockService + LayerService + EngineService + PresetService + ProfileService + SongService,
{
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.service, &other.service)
    }
}

impl<S> Eq for SignalController<S> where
    S: BlockService + LayerService + EngineService + PresetService + ProfileService + SongService
{
}

impl<S> SignalController<S>
where
    S: BlockService + LayerService + EngineService + PresetService + ProfileService + SongService,
{
    pub fn new(service: Arc<S>) -> Self {
        Self::new_with_context(service, Arc::new(DefaultContextFactory))
    }

    pub fn new_with_context(service: Arc<S>, context_factory: SharedContextFactory) -> Self {
        Self {
            service,
            context_factory,
        }
    }

    // region: --- Block operations

    pub async fn get_block(&self, block_type: BlockType) -> Block {
        let cx = self.context_factory.make_context();
        self.service.get_block(&cx, block_type).await
    }

    pub async fn set_block(&self, block_type: BlockType, block: Block) -> Block {
        let cx = self.context_factory.make_context();
        self.service.set_block(&cx, block_type, block).await
    }

    pub async fn get_value(&self, block_type: BlockType) -> f32 {
        self.get_block(block_type)
            .await
            .first_value()
            .unwrap_or(0.0)
    }

    pub async fn set_value(&self, block_type: BlockType, value: f32) -> Block {
        let mut block = self.get_block(block_type).await;
        block.set_first_value(value);
        self.set_block(block_type, block).await
    }

    // endregion: --- Block operations

    // region: --- Collection + variant operations (block-level)

    /// List all collections for a given block type.
    pub async fn list_collections(&self, block_type: BlockType) -> Vec<Preset> {
        let cx = self.context_factory.make_context();
        self.service.list_presets(&cx, block_type).await
    }

    /// Load the default variant from a collection, returning its block state.
    pub async fn load_collection_default(
        &self,
        block_type: BlockType,
        collection_id: impl Into<PresetId>,
    ) -> Option<Block> {
        let cx = self.context_factory.make_context();
        self.service
            .load_preset(&cx, block_type, collection_id.into())
            .await
            .map(|snapshot| snapshot.block())
    }

    /// Load a specific variant from a collection, returning its block state.
    pub async fn load_variant(
        &self,
        block_type: BlockType,
        collection_id: impl Into<PresetId>,
        variant_id: impl Into<SnapshotId>,
    ) -> Option<Block> {
        let cx = self.context_factory.make_context();
        self.service
            .load_preset_snapshot(&cx, block_type, collection_id.into(), variant_id.into())
            .await
            .map(|snapshot| snapshot.block())
    }

    // endregion: --- Collection + variant operations (block-level)

    // region: --- Collection + variant operations (module-level)

    /// List all module-level collections.
    pub async fn list_module_collections(&self) -> Vec<ModulePreset> {
        let cx = self.context_factory.make_context();
        self.service.list_module_presets(&cx).await
    }

    /// Load the default variant from a module collection.
    pub async fn load_module_collection_default(
        &self,
        collection_id: impl Into<ModulePresetId>,
    ) -> Option<ModuleSnapshot> {
        let cx = self.context_factory.make_context();
        self.service
            .load_module_preset(&cx, collection_id.into())
            .await
    }

    /// Load a specific variant from a module collection.
    pub async fn load_module_variant(
        &self,
        collection_id: impl Into<ModulePresetId>,
        variant_id: impl Into<ModuleSnapshotId>,
    ) -> Option<ModuleSnapshot> {
        let cx = self.context_factory.make_context();
        self.service
            .load_module_preset_snapshot(&cx, collection_id.into(), variant_id.into())
            .await
    }

    // endregion: --- Collection + variant operations (module-level)

    // region: --- Layer operations

    /// List all layers.
    pub async fn list_layers(&self) -> Vec<Layer> {
        let cx = self.context_factory.make_context();
        self.service.list_layers(&cx).await
    }

    /// List all layer presets (alias for layer collections).
    ///
    /// Layer presets are the simple authoring surface for single-sound chains.
    pub async fn list_layer_presets(&self) -> Vec<Layer> {
        self.list_layers().await
    }

    /// Load a layer by ID.
    pub async fn load_layer(&self, id: impl Into<LayerId>) -> Option<Layer> {
        let cx = self.context_factory.make_context();
        self.service.load_layer(&cx, id.into()).await
    }

    /// Load a layer preset by ID (alias for `load_layer`).
    pub async fn load_layer_preset(&self, id: impl Into<LayerId>) -> Option<Layer> {
        self.load_layer(id).await
    }

    /// Save (create or update) a layer.
    pub async fn save_layer(&self, layer: Layer) {
        let cx = self.context_factory.make_context();
        self.service.save_layer(&cx, layer).await;
    }

    /// Delete a layer by ID.
    pub async fn delete_layer(&self, id: impl Into<LayerId>) {
        let cx = self.context_factory.make_context();
        self.service.delete_layer(&cx, id.into()).await;
    }

    /// Load a specific variant from a layer.
    pub async fn load_layer_variant(
        &self,
        layer_id: impl Into<LayerId>,
        variant_id: impl Into<LayerSnapshotId>,
    ) -> Option<LayerSnapshot> {
        let cx = self.context_factory.make_context();
        self.service
            .load_layer_variant(&cx, layer_id.into(), variant_id.into())
            .await
    }

    /// Load a specific variant from a layer preset (alias for `load_layer_variant`).
    pub async fn load_layer_preset_variant(
        &self,
        layer_id: impl Into<LayerId>,
        variant_id: impl Into<LayerSnapshotId>,
    ) -> Option<LayerSnapshot> {
        self.load_layer_variant(layer_id, variant_id).await
    }

    // endregion: --- Layer operations

    // region: --- Engine operations

    /// List all engines.
    pub async fn list_engines(&self) -> Vec<Engine> {
        let cx = self.context_factory.make_context();
        self.service.list_engines(&cx).await
    }

    /// Load an engine by ID.
    pub async fn load_engine(&self, id: impl Into<EngineId>) -> Option<Engine> {
        let cx = self.context_factory.make_context();
        self.service.load_engine(&cx, id.into()).await
    }

    /// Save (create or update) an engine.
    pub async fn save_engine(&self, engine: Engine) {
        let cx = self.context_factory.make_context();
        self.service.save_engine(&cx, engine).await;
    }

    /// Delete an engine by ID.
    pub async fn delete_engine(&self, id: impl Into<EngineId>) {
        let cx = self.context_factory.make_context();
        self.service.delete_engine(&cx, id.into()).await;
    }

    /// Load a specific variant (scene) from an engine.
    pub async fn load_engine_variant(
        &self,
        engine_id: impl Into<EngineId>,
        variant_id: impl Into<EngineSceneId>,
    ) -> Option<EngineScene> {
        let cx = self.context_factory.make_context();
        self.service
            .load_engine_variant(&cx, engine_id.into(), variant_id.into())
            .await
    }

    // endregion: --- Engine operations

    // region: --- Preset (rig) operations

    /// List all rig presets.
    pub async fn list_presets_all(&self) -> Vec<Rig> {
        let cx = self.context_factory.make_context();
        self.service.list_presets_all(&cx).await
    }

    /// Load a rig preset by ID.
    pub async fn load_preset_rig(&self, id: impl Into<RigId>) -> Option<Rig> {
        let cx = self.context_factory.make_context();
        self.service.load_preset_rig(&cx, id.into()).await
    }

    /// Save (create or update) a rig preset.
    pub async fn save_preset(&self, rig: Rig) {
        let cx = self.context_factory.make_context();
        self.service.save_preset(&cx, rig).await;
    }

    /// Delete a rig preset by ID.
    pub async fn delete_preset(&self, id: impl Into<RigId>) {
        let cx = self.context_factory.make_context();
        self.service.delete_preset(&cx, id.into()).await;
    }

    /// Load a specific variant (scene) from a rig preset.
    pub async fn load_preset_variant(
        &self,
        rig_id: impl Into<RigId>,
        variant_id: impl Into<RigSceneId>,
    ) -> Option<RigScene> {
        let cx = self.context_factory.make_context();
        self.service
            .load_preset_variant(&cx, rig_id.into(), variant_id.into())
            .await
    }

    // endregion: --- Preset (rig) operations

    // region: --- Profile operations

    /// List all profiles.
    pub async fn list_profiles(&self) -> Vec<Profile> {
        let cx = self.context_factory.make_context();
        self.service.list_profiles(&cx).await
    }

    /// Load a profile by ID.
    pub async fn load_profile(&self, id: impl Into<ProfileId>) -> Option<Profile> {
        let cx = self.context_factory.make_context();
        self.service.load_profile(&cx, id.into()).await
    }

    /// Save (create or update) a profile.
    pub async fn save_profile(&self, profile: Profile) {
        let cx = self.context_factory.make_context();
        self.service.save_profile(&cx, profile).await;
    }

    /// Delete a profile by ID.
    pub async fn delete_profile(&self, id: impl Into<ProfileId>) {
        let cx = self.context_factory.make_context();
        self.service.delete_profile(&cx, id.into()).await;
    }

    /// Load a specific variant (patch) from a profile.
    pub async fn load_profile_variant(
        &self,
        profile_id: impl Into<ProfileId>,
        variant_id: impl Into<PatchId>,
    ) -> Option<Patch> {
        let cx = self.context_factory.make_context();
        self.service
            .load_profile_variant(&cx, profile_id.into(), variant_id.into())
            .await
    }

    // endregion: --- Profile operations

    // region: --- Song operations

    /// List all songs.
    pub async fn list_songs(&self) -> Vec<Song> {
        let cx = self.context_factory.make_context();
        self.service.list_songs(&cx).await
    }

    /// Load a song by ID.
    pub async fn load_song(&self, id: impl Into<SongId>) -> Option<Song> {
        let cx = self.context_factory.make_context();
        self.service.load_song(&cx, id.into()).await
    }

    /// Save (create or update) a song.
    pub async fn save_song(&self, song: Song) {
        let cx = self.context_factory.make_context();
        self.service.save_song(&cx, song).await;
    }

    /// Delete a song by ID.
    pub async fn delete_song(&self, id: impl Into<SongId>) {
        let cx = self.context_factory.make_context();
        self.service.delete_song(&cx, id.into()).await;
    }

    /// Load a specific variant (section) from a song.
    pub async fn load_song_variant(
        &self,
        song_id: impl Into<SongId>,
        variant_id: impl Into<SectionId>,
    ) -> Option<Section> {
        let cx = self.context_factory.make_context();
        self.service
            .load_song_variant(&cx, song_id.into(), variant_id.into())
            .await
    }

    // endregion: --- Song operations

    // region: --- Deprecated shims

    /// Deprecated: use [`list_collections`] instead.
    #[deprecated(note = "use list_collections")]
    pub async fn list_presets(&self, block_type: BlockType) -> Vec<Preset> {
        self.list_collections(block_type).await
    }

    /// Deprecated: use [`load_collection_default`] instead.
    #[deprecated(note = "use load_collection_default")]
    pub async fn load_preset(
        &self,
        block_type: BlockType,
        preset_id: impl Into<PresetId>,
    ) -> Option<Block> {
        self.load_collection_default(block_type, preset_id).await
    }

    /// Deprecated: use [`load_variant`] instead.
    #[deprecated(note = "use load_variant")]
    pub async fn load_preset_snapshot(
        &self,
        block_type: BlockType,
        preset_id: impl Into<PresetId>,
        snapshot_id: impl Into<SnapshotId>,
    ) -> Option<Block> {
        self.load_variant(block_type, preset_id, snapshot_id).await
    }

    /// Deprecated: use [`list_module_collections`] instead.
    #[deprecated(note = "use list_module_collections")]
    pub async fn list_module_presets(&self) -> Vec<ModulePreset> {
        self.list_module_collections().await
    }

    /// Deprecated: use [`load_module_collection_default`] instead.
    #[deprecated(note = "use load_module_collection_default")]
    pub async fn load_module_preset(
        &self,
        preset_id: impl Into<ModulePresetId>,
    ) -> Option<ModuleSnapshot> {
        self.load_module_collection_default(preset_id).await
    }

    /// Deprecated: use [`load_module_variant`] instead.
    #[deprecated(note = "use load_module_variant")]
    pub async fn load_module_preset_snapshot(
        &self,
        preset_id: impl Into<ModulePresetId>,
        snapshot_id: impl Into<ModuleSnapshotId>,
    ) -> Option<ModuleSnapshot> {
        self.load_module_variant(preset_id, snapshot_id).await
    }

    // endregion: --- Deprecated shims
}

impl<S> SignalController<S>
where
    S: BlockService
        + LayerService
        + EngineService
        + PresetService
        + ProfileService
        + SongService
        + BrowserService,
{
    /// Build the current browser index across all signal domain levels.
    pub async fn browser_index(&self) -> BrowserIndex {
        let cx = self.context_factory.make_context();
        self.service.browser_index(&cx).await
    }

    /// Query the semantic browser using structured tags and fallback scoring.
    pub async fn browse(&self, query: BrowserQuery) -> Vec<BrowserHit> {
        let cx = self.context_factory.make_context();
        self.service.browse(&cx, query).await
    }
}
