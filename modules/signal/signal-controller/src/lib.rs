//! Service-driven controller for the signal domain.
//!
//! The controller exposes **collection / variant** terminology:
//!
//! - A **collection** is a named group of variants (Preset, ModulePreset, Layer, Engine, Rig, Profile, Song).
//! - A **variant** is a specific snapshot within a collection (Snapshot, ModuleSnapshot, LayerSnapshot, EngineScene, RigScene, Patch, Section).
//!
//! Each domain level (block, module, layer, engine, preset/rig, profile, song) gets its own method region.

pub mod events;

use events::EventBus;
use signal_live::SignalLive;
use signal_proto::{
    engine::{Engine, EngineId, EngineScene, EngineSceneId},
    layer::{Layer, LayerId, LayerSnapshot, LayerSnapshotId},
    profile::{Patch, PatchId, Profile, ProfileId},
    resolve::{ResolveError, ResolveTarget, ResolvedGraph},
    rig::{Rig, RigId, RigScene, RigSceneId},
    setlist::{Setlist, SetlistEntry, SetlistEntryId, SetlistId},
    song::{Section, SectionId, SectionSource, Song, SongId},
    tagging::{BrowserHit, BrowserIndex, BrowserQuery},
    scene_template::{SceneTemplate, SceneTemplateId},
    traits::Collection,
    Block, BlockService, BlockType, BrowserService, EngineService, LayerService, ModulePreset,
    ModulePresetId, ModuleSnapshot, ModuleSnapshotId, Preset, PresetId, PresetService,
    ProfileService, ResolveService, SceneTemplateService, SetlistService, SnapshotId, SongService,
};
use std::sync::Arc;

pub trait SignalApi:
    BlockService
    + LayerService
    + EngineService
    + PresetService
    + ProfileService
    + SongService
    + SetlistService
    + BrowserService
    + ResolveService
    + SceneTemplateService
{
}

impl<T> SignalApi for T where
    T: BlockService
        + LayerService
        + EngineService
        + PresetService
        + ProfileService
        + SongService
        + SetlistService
        + BrowserService
        + ResolveService
        + SceneTemplateService
{
}

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
    S: SignalApi,
{
    service: Arc<S>,
    context_factory: SharedContextFactory,
    event_bus: Arc<EventBus>,
}

impl<S> Clone for SignalController<S>
where
    S: SignalApi,
{
    fn clone(&self) -> Self {
        Self {
            service: self.service.clone(),
            context_factory: self.context_factory.clone(),
            event_bus: self.event_bus.clone(),
        }
    }
}

impl<S> PartialEq for SignalController<S>
where
    S: SignalApi,
{
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.service, &other.service)
    }
}

impl<S> Eq for SignalController<S> where S: SignalApi {}

impl<S> SignalController<S>
where
    S: SignalApi,
{
    pub fn new(service: Arc<S>) -> Self {
        Self::new_with_context(service, Arc::new(DefaultContextFactory))
    }

    pub fn new_with_context(service: Arc<S>, context_factory: SharedContextFactory) -> Self {
        Self {
            service,
            context_factory,
            event_bus: Arc::new(EventBus::default()),
        }
    }

    // region: --- Event streaming

    /// Subscribe to signal events for reactive UI updates.
    pub fn subscribe(&self) -> tokio::sync::broadcast::Receiver<events::SignalEvent> {
        self.event_bus.subscribe()
    }

    /// Get the event bus (for internal use by methods that emit events).
    pub fn event_bus(&self) -> &EventBus {
        &self.event_bus
    }

    // endregion: --- Event streaming

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

    /// Save (upsert) a block collection.
    pub async fn save_block_collection(&self, preset: Preset) {
        let cx = self.context_factory.make_context();
        self.service.save_block_collection(&cx, preset).await;
    }

    /// Targeted update: load the preset, find the snapshot, replace its block, bump version, save.
    pub async fn update_snapshot_params(
        &self,
        block_type: BlockType,
        preset_id: impl Into<PresetId>,
        snapshot_id: impl Into<SnapshotId>,
        block: Block,
    ) {
        let preset_id = preset_id.into();
        let snapshot_id = snapshot_id.into();
        let presets = self.list_collections(block_type).await;
        if let Some(mut preset) = presets.into_iter().find(|p| *p.id() == preset_id) {
            if let Some(snap) = preset.variants_mut().iter_mut().find(|s| *s.id() == snapshot_id) {
                snap.set_block(block);
                snap.increment_version();
            }
            self.save_block_collection(preset).await;
        }
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

    // region: --- Rig collection operations

    /// List all rig collections.
    pub async fn list_rig_collections(&self) -> Vec<Rig> {
        let cx = self.context_factory.make_context();
        self.service.list_presets_all(&cx).await
    }

    /// Load a rig collection by ID.
    pub async fn load_rig_collection(&self, id: impl Into<RigId>) -> Option<Rig> {
        let cx = self.context_factory.make_context();
        self.service.load_preset_rig(&cx, id.into()).await
    }

    /// Save (create or update) a rig collection.
    pub async fn save_rig_collection(&self, rig: Rig) {
        let cx = self.context_factory.make_context();
        self.service.save_preset(&cx, rig).await;
    }

    /// Delete a rig collection by ID.
    pub async fn delete_rig_collection(&self, id: impl Into<RigId>) {
        let cx = self.context_factory.make_context();
        self.service.delete_preset(&cx, id.into()).await;
    }

    /// Load a specific variant (scene) from a rig collection.
    pub async fn load_rig_variant(
        &self,
        rig_id: impl Into<RigId>,
        variant_id: impl Into<RigSceneId>,
    ) -> Option<RigScene> {
        let cx = self.context_factory.make_context();
        self.service
            .load_preset_variant(&cx, rig_id.into(), variant_id.into())
            .await
    }

    // endregion: --- Rig collection operations

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

    /// Update a song section's source reference (e.g. assign a new preset/scene or patch).
    pub async fn set_section_source(
        &self,
        song_id: impl Into<SongId>,
        section_id: impl Into<SectionId>,
        source: SectionSource,
    ) {
        let song_id = song_id.into();
        let section_id = section_id.into();
        if let Some(mut song) = self.load_song(song_id).await {
            if let Some(section) = song.sections.iter_mut().find(|s| s.id == section_id) {
                section.source = source;
            }
            self.save_song(song).await;
        }
    }

    // endregion: --- Song operations

    // region: --- Profile mutation helpers

    /// Update a profile patch's underlying rig/scene reference.
    pub async fn set_patch_preset(
        &self,
        profile_id: impl Into<ProfileId>,
        patch_id: impl Into<PatchId>,
        rig_id: impl Into<RigId>,
        scene_id: impl Into<RigSceneId>,
    ) {
        let profile_id = profile_id.into();
        let patch_id = patch_id.into();
        let rig_id = rig_id.into();
        let scene_id = scene_id.into();
        if let Some(mut profile) = self.load_profile(profile_id).await {
            if let Some(patch) = profile.patches.iter_mut().find(|p| p.id == patch_id) {
                patch.rig_id = rig_id;
                patch.rig_variant_id = scene_id;
            }
            self.save_profile(profile).await;
        }
    }

    // endregion: --- Profile mutation helpers

    // region: --- Setlist operations

    /// List all setlists.
    pub async fn list_setlists(&self) -> Vec<Setlist> {
        let cx = self.context_factory.make_context();
        self.service.list_setlists(&cx).await
    }

    /// Load a setlist by ID.
    pub async fn load_setlist(&self, id: impl Into<SetlistId>) -> Option<Setlist> {
        let cx = self.context_factory.make_context();
        self.service.load_setlist(&cx, id.into()).await
    }

    /// Save (create or update) a setlist.
    pub async fn save_setlist(&self, setlist: Setlist) {
        let cx = self.context_factory.make_context();
        self.service.save_setlist(&cx, setlist).await;
    }

    /// Delete a setlist by ID.
    pub async fn delete_setlist(&self, id: impl Into<SetlistId>) {
        let cx = self.context_factory.make_context();
        self.service.delete_setlist(&cx, id.into()).await;
    }

    /// Load a specific entry (variant) from a setlist.
    pub async fn load_setlist_entry(
        &self,
        setlist_id: impl Into<SetlistId>,
        entry_id: impl Into<SetlistEntryId>,
    ) -> Option<SetlistEntry> {
        let cx = self.context_factory.make_context();
        self.service
            .load_setlist_entry(&cx, setlist_id.into(), entry_id.into())
            .await
    }

    // endregion: --- Setlist operations

    // region: --- Browser operations

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

    // endregion: --- Browser operations

    // region: --- Resolve operations

    /// Resolve any target (rig scene, profile patch, song section) into an executable graph.
    pub async fn resolve_target(
        &self,
        target: ResolveTarget,
    ) -> Result<ResolvedGraph, ResolveError> {
        let cx = self.context_factory.make_context();
        self.service.resolve_target(&cx, target).await
    }

    // endregion: --- Resolve operations

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

    /// Deprecated: use [`list_rig_collections`] instead.
    #[deprecated(note = "use list_rig_collections")]
    pub async fn list_presets_all(&self) -> Vec<Rig> {
        self.list_rig_collections().await
    }

    /// Deprecated: use [`load_rig_collection`] instead.
    #[deprecated(note = "use load_rig_collection")]
    pub async fn load_preset_rig(&self, id: impl Into<RigId>) -> Option<Rig> {
        self.load_rig_collection(id).await
    }

    /// Deprecated: use [`save_rig_collection`] instead.
    #[deprecated(note = "use save_rig_collection")]
    pub async fn save_preset(&self, rig: Rig) {
        self.save_rig_collection(rig).await
    }

    /// Deprecated: use [`delete_rig_collection`] instead.
    #[deprecated(note = "use delete_rig_collection")]
    pub async fn delete_preset(&self, id: impl Into<RigId>) {
        self.delete_rig_collection(id).await
    }

    /// Deprecated: use [`load_rig_variant`] instead.
    #[deprecated(note = "use load_rig_variant")]
    pub async fn load_preset_variant(
        &self,
        rig_id: impl Into<RigId>,
        variant_id: impl Into<RigSceneId>,
    ) -> Option<RigScene> {
        self.load_rig_variant(rig_id, variant_id).await
    }

    // endregion: --- Deprecated shims

    // endregion: --- Deprecated shims v2

    // region: --- Filtered queries (by tag)

    /// List profiles filtered by a tag (exact match against tags list).
    pub async fn list_profiles_by_tag(&self, tag: &str) -> Vec<signal_proto::profile::Profile> {
        let all = self.list_profiles().await;
        all.into_iter()
            .filter(|p| p.metadata.tags.contains(tag))
            .collect()
    }

    /// List rig collections filtered by a tag (exact match against tags list).
    pub async fn list_rig_collections_by_tag(&self, tag: &str) -> Vec<Rig> {
        let all = self.list_rig_collections().await;
        all.into_iter()
            .filter(|r| r.metadata.tags.contains(tag))
            .collect()
    }

    // endregion: --- Filtered queries

    // region: --- Scene templates

    pub async fn list_scene_templates(&self) -> Vec<SceneTemplate> {
        let cx = self.context_factory.make_context();
        self.service.list_scene_templates(&cx).await
    }

    pub async fn load_scene_template(
        &self,
        id: impl Into<SceneTemplateId>,
    ) -> Option<SceneTemplate> {
        let cx = self.context_factory.make_context();
        self.service.load_scene_template(&cx, id.into()).await
    }

    pub async fn save_scene_template(&self, template: SceneTemplate) {
        let cx = self.context_factory.make_context();
        self.service.save_scene_template(&cx, template).await;
    }

    pub async fn delete_scene_template(&self, id: impl Into<SceneTemplateId>) {
        let cx = self.context_factory.make_context();
        self.service.delete_scene_template(&cx, id.into()).await;
    }

    pub async fn reorder_scene_templates(&self, ordered_ids: Vec<SceneTemplateId>) {
        let cx = self.context_factory.make_context();
        self.service
            .reorder_scene_templates(&cx, ordered_ids)
            .await;
    }

    // endregion: --- Scene templates

    // region: --- Reorder helpers

    /// Reorder rig scene variants. Takes the rig ID and the desired scene order.
    /// The rig is loaded, variants reordered to match, and saved back.
    pub async fn reorder_rig_scenes(
        &self,
        rig_id: impl Into<RigId>,
        ordered_scene_ids: &[RigSceneId],
    ) {
        let rig_id = rig_id.into();
        if let Some(mut rig) = self.load_rig_collection(rig_id.clone()).await {
            let mut reordered = Vec::with_capacity(rig.variants.len());
            for scene_id in ordered_scene_ids {
                if let Some(pos) = rig.variants.iter().position(|v| &v.id == scene_id) {
                    reordered.push(rig.variants.remove(pos));
                }
            }
            // Append any remaining variants not in the order list.
            reordered.append(&mut rig.variants);
            rig.variants = reordered;
            self.save_rig_collection(rig).await;
        }
    }

    /// Reorder profile patches. Takes the profile ID and the desired patch order.
    pub async fn reorder_profile_patches(
        &self,
        profile_id: impl Into<ProfileId>,
        ordered_patch_ids: &[PatchId],
    ) {
        let profile_id = profile_id.into();
        if let Some(mut profile) = self.load_profile(profile_id.clone()).await {
            let mut reordered = Vec::with_capacity(profile.patches.len());
            for patch_id in ordered_patch_ids {
                if let Some(pos) = profile.patches.iter().position(|p| &p.id == patch_id) {
                    reordered.push(profile.patches.remove(pos));
                }
            }
            reordered.append(&mut profile.patches);
            profile.patches = reordered;
            self.save_profile(profile).await;
        }
    }

    /// Reorder song sections. Takes the song ID and the desired section order.
    pub async fn reorder_song_sections(
        &self,
        song_id: impl Into<SongId>,
        ordered_section_ids: &[SectionId],
    ) {
        let song_id = song_id.into();
        if let Some(mut song) = self.load_song(song_id.clone()).await {
            let mut reordered = Vec::with_capacity(song.sections.len());
            for section_id in ordered_section_ids {
                if let Some(pos) = song.sections.iter().position(|s| &s.id == section_id) {
                    reordered.push(song.sections.remove(pos));
                }
            }
            reordered.append(&mut song.sections);
            song.sections = reordered;
            self.save_song(song).await;
        }
    }

    /// Reorder setlist entries. Takes the setlist ID and the desired entry order.
    pub async fn reorder_setlist_entries(
        &self,
        setlist_id: impl Into<SetlistId>,
        ordered_entry_ids: &[SetlistEntryId],
    ) {
        let setlist_id = setlist_id.into();
        if let Some(mut setlist) = self.load_setlist(setlist_id.clone()).await {
            let mut reordered = Vec::with_capacity(setlist.entries.len());
            for entry_id in ordered_entry_ids {
                if let Some(pos) = setlist.entries.iter().position(|e| &e.id == entry_id) {
                    reordered.push(setlist.entries.remove(pos));
                }
            }
            reordered.append(&mut setlist.entries);
            setlist.entries = reordered;
            self.save_setlist(setlist).await;
        }
    }

    // endregion: --- Reorder helpers
}
