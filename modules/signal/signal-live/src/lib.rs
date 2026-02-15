//! Live service implementation for signal2.
//!
//! Maps service traits onto storage repos:
//! - `BlockService` → `BlockRepo` + `ModuleRepo`
//! - `LayerService` → `LayerRepo`
//! - `EngineService` → `EngineRepo`
//! - `PresetService` → `RigRepo`
//! - `ProfileService` → `ProfileRepo`
//! - `SongService` → `SongRepo`
//!
//! # Collection / Variant Mapping
//!
//! This service operates on *collection* and *variant* concepts:
//! - **Block collections** (`Preset`) group related block-parameter variants (`Snapshot`).
//! - **Module collections** (`ModulePreset`) group multi-block composition variants (`ModuleSnapshot`).
//! - **Layer collections** (`Layer`) group processing-lane variants (`LayerSnapshot`).
//! - **Engine collections** (`Engine`) group scene variants (`EngineScene`).
//! - **Rig presets** (`Rig`) group rig scene variants (`RigScene`).
//! - **Profiles** (`Profile`) group patch variants (`Patch`).
//! - **Songs** (`Song`) group section variants (`Section`).
//!
//! When a block variant is loaded (via `load_preset` / `load_preset_snapshot`), the
//! service applies a **side-effect**: the resolved block state is persisted as
//! the current active block.  This deterministic "load = apply" contract ensures
//! the active block always reflects the last loaded variant.

use roam::Context;
use signal_proto::{
    engine::{Engine, EngineId, EngineScene, EngineSceneId},
    layer::{Layer, LayerId, LayerSnapshot, LayerSnapshotId},
    profile::{Patch, PatchId, Profile, ProfileId},
    resolve::{
        LayerSource, ResolveError, ResolveTarget, ResolvedBlock, ResolvedEngine, ResolvedGraph,
        ResolvedLayer, ResolvedModule,
    },
    rig::{Rig, RigId, RigScene, RigSceneId},
    setlist::{Setlist, SetlistEntry, SetlistEntryId, SetlistId},
    song::{Section, SectionId, Song, SongId},
    tagging::{
        infer_tags_from_name, BrowserEntityKind, BrowserEntry, BrowserHit, BrowserIndex,
        BrowserNodeId, BrowserQuery, StructuredTag, TagCategory, TagSet, TagWeights,
    },
    override_policy::{validate_overrides, FreePolicy, ScenePolicy, SnapshotPolicy},
    overrides::{NodeOverrideOp, NodePathSegment},
    Block, BlockParameterOverride, BlockService, BlockType, BrowserService, EngineService,
    LayerService, ModuleBlockSource, ModulePreset, ModulePresetId, ModuleSnapshot,
    ModuleSnapshotId, Preset, PresetId, PresetService, ProfileService, ResolveService,
    SetlistService, Snapshot, SnapshotId, SongService, ALL_BLOCK_TYPES,
};
use signal_storage::{
    BlockRepo, BlockRepoLive, DatabaseConnection, EngineRepo, EngineRepoLive, LayerRepo,
    LayerRepoLive, ModuleRepo, ModuleRepoLive, ProfileRepo, ProfileRepoLive, RigRepo,
    RigRepoLive, SetlistRepo, SetlistRepoLive, SongRepo, SongRepoLive,
};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

// region: --- SignalLive

/// Live service bridging RPC traits to storage repos.
///
/// Generic over all seven repo traits so tests can inject in-memory repos.
/// Default type parameters enable the common case without specifying concrete types.
pub struct SignalLive<
    B = BlockRepoLive,
    M = ModuleRepoLive,
    L = LayerRepoLive,
    E = EngineRepoLive,
    R = RigRepoLive,
    P = ProfileRepoLive,
    So = SongRepoLive,
    Se = SetlistRepoLive,
> where
    B: BlockRepo,
    M: ModuleRepo,
    L: LayerRepo,
    E: EngineRepo,
    R: RigRepo,
    P: ProfileRepo,
    So: SongRepo,
    Se: SetlistRepo,
{
    block_repo: Arc<B>,
    module_repo: Arc<M>,
    layer_repo: Arc<L>,
    engine_repo: Arc<E>,
    rig_repo: Arc<R>,
    profile_repo: Arc<P>,
    song_repo: Arc<So>,
    setlist_repo: Arc<Se>,
}

impl<B, M, L, E, R, P, So, Se> SignalLive<B, M, L, E, R, P, So, Se>
where
    B: BlockRepo,
    M: ModuleRepo,
    L: LayerRepo,
    E: EngineRepo,
    R: RigRepo,
    P: ProfileRepo,
    So: SongRepo,
    Se: SetlistRepo,
{
    pub fn new(
        block_repo: Arc<B>,
        module_repo: Arc<M>,
        layer_repo: Arc<L>,
        engine_repo: Arc<E>,
        rig_repo: Arc<R>,
        profile_repo: Arc<P>,
        song_repo: Arc<So>,
        setlist_repo: Arc<Se>,
    ) -> Self {
        Self {
            block_repo,
            module_repo,
            layer_repo,
            engine_repo,
            rig_repo,
            profile_repo,
            song_repo,
            setlist_repo,
        }
    }
}

impl
    SignalLive<
        BlockRepoLive,
        ModuleRepoLive,
        LayerRepoLive,
        EngineRepoLive,
        RigRepoLive,
        ProfileRepoLive,
        SongRepoLive,
        SetlistRepoLive,
    >
{
    pub fn from_db(db: DatabaseConnection) -> Self {
        Self::new(
            Arc::new(BlockRepoLive::new(db.clone())),
            Arc::new(ModuleRepoLive::new(db.clone())),
            Arc::new(LayerRepoLive::new(db.clone())),
            Arc::new(EngineRepoLive::new(db.clone())),
            Arc::new(RigRepoLive::new(db.clone())),
            Arc::new(ProfileRepoLive::new(db.clone())),
            Arc::new(SongRepoLive::new(db.clone())),
            Arc::new(SetlistRepoLive::new(db)),
        )
    }
}

// endregion: --- SignalLive

// region: --- BlockService impl

impl<B, M, L, E, R, P, So, Se> BlockService for SignalLive<B, M, L, E, R, P, So, Se>
where
    B: BlockRepo,
    M: ModuleRepo,
    L: LayerRepo,
    E: EngineRepo,
    R: RigRepo,
    P: ProfileRepo,
    So: SongRepo,
    Se: SetlistRepo,
{
    /// Load the current active block state for a given block type.
    /// Returns `Block::default()` when no state has been persisted yet.
    async fn get_block(&self, _cx: &Context, block_type: BlockType) -> Block {
        self.block_repo
            .load_block_state(block_type)
            .await
            .ok()
            .flatten()
            .unwrap_or_default()
    }

    /// Persist a new block state and return it.
    async fn set_block(&self, _cx: &Context, block_type: BlockType, block: Block) -> Block {
        let _ = self
            .block_repo
            .save_block_state(block_type, block.clone())
            .await;
        block
    }

    /// List all block collections (presets) for a given block type.
    async fn list_presets(&self, _cx: &Context, block_type: BlockType) -> Vec<Preset> {
        self.block_repo
            .list_block_collections(block_type)
            .await
            .unwrap_or_default()
    }

    /// Load the default variant of a block collection and apply it as the
    /// current active block.
    async fn load_preset(
        &self,
        _cx: &Context,
        block_type: BlockType,
        preset_id: PresetId,
    ) -> Option<Snapshot> {
        let snapshot = self
            .block_repo
            .load_block_default_variant(block_type, &preset_id)
            .await
            .ok()
            .flatten();
        if let Some(snapshot) = snapshot.as_ref() {
            let _ = self
                .block_repo
                .save_block_state(block_type, snapshot.block())
                .await;
        }
        snapshot
    }

    /// Load a specific variant from a block collection and apply it as the
    /// current active block.
    async fn load_preset_snapshot(
        &self,
        _cx: &Context,
        block_type: BlockType,
        preset_id: PresetId,
        snapshot_id: SnapshotId,
    ) -> Option<Snapshot> {
        let snapshot = self
            .block_repo
            .load_block_variant(block_type, &preset_id, &snapshot_id)
            .await
            .ok()
            .flatten();
        if let Some(snapshot) = snapshot.as_ref() {
            let _ = self
                .block_repo
                .save_block_state(block_type, snapshot.block())
                .await;
        }
        snapshot
    }

    /// List all module collections.
    async fn list_module_presets(&self, _cx: &Context) -> Vec<ModulePreset> {
        self.module_repo
            .list_module_collections()
            .await
            .unwrap_or_default()
    }

    /// Load the default variant of a module collection.
    async fn load_module_preset(
        &self,
        _cx: &Context,
        preset_id: ModulePresetId,
    ) -> Option<ModuleSnapshot> {
        self.module_repo
            .load_module_default_variant(&preset_id)
            .await
            .ok()
            .flatten()
    }

    /// Load a specific variant from a module collection.
    async fn load_module_preset_snapshot(
        &self,
        _cx: &Context,
        preset_id: ModulePresetId,
        snapshot_id: ModuleSnapshotId,
    ) -> Option<ModuleSnapshot> {
        self.module_repo
            .load_module_variant(&preset_id, &snapshot_id)
            .await
            .ok()
            .flatten()
    }
}

// endregion: --- BlockService impl

// region: --- LayerService impl

impl<B, M, L, E, R, P, So, Se> LayerService for SignalLive<B, M, L, E, R, P, So, Se>
where
    B: BlockRepo,
    M: ModuleRepo,
    L: LayerRepo,
    E: EngineRepo,
    R: RigRepo,
    P: ProfileRepo,
    So: SongRepo,
    Se: SetlistRepo,
{
    async fn list_layers(&self, _cx: &Context) -> Vec<Layer> {
        self.layer_repo.list_layers().await.unwrap_or_default()
    }

    async fn load_layer(&self, _cx: &Context, id: LayerId) -> Option<Layer> {
        self.layer_repo.load_layer(&id).await.ok().flatten()
    }

    async fn save_layer(&self, _cx: &Context, layer: Layer) -> () {
        for variant in &layer.variants {
            if variant.validate_overrides().is_err() {
                return;
            }
        }
        let _ = self.layer_repo.save_layer(&layer).await;
    }

    async fn delete_layer(&self, _cx: &Context, id: LayerId) -> () {
        let _ = self.layer_repo.delete_layer(&id).await;
    }

    async fn load_layer_variant(
        &self,
        _cx: &Context,
        layer_id: LayerId,
        variant_id: LayerSnapshotId,
    ) -> Option<LayerSnapshot> {
        self.layer_repo
            .load_variant(&layer_id, &variant_id)
            .await
            .ok()
            .flatten()
    }
}

// endregion: --- LayerService impl

// region: --- EngineService impl

impl<B, M, L, E, R, P, So, Se> EngineService for SignalLive<B, M, L, E, R, P, So, Se>
where
    B: BlockRepo,
    M: ModuleRepo,
    L: LayerRepo,
    E: EngineRepo,
    R: RigRepo,
    P: ProfileRepo,
    So: SongRepo,
    Se: SetlistRepo,
{
    async fn list_engines(&self, _cx: &Context) -> Vec<Engine> {
        self.engine_repo.list_engines().await.unwrap_or_default()
    }

    async fn load_engine(&self, _cx: &Context, id: EngineId) -> Option<Engine> {
        self.engine_repo.load_engine(&id).await.ok().flatten()
    }

    async fn save_engine(&self, _cx: &Context, engine: Engine) -> () {
        for variant in &engine.variants {
            if variant.validate_overrides().is_err() {
                return;
            }
        }
        for layer_id in &engine.layer_ids {
            let Some(layer) = self.layer_repo.load_layer(layer_id).await.ok().flatten() else {
                return;
            };
            if !engine.is_layer_type_compatible(layer.engine_type) {
                return;
            }
        }
        let _ = self.engine_repo.save_engine(&engine).await;
    }

    async fn delete_engine(&self, _cx: &Context, id: EngineId) -> () {
        let _ = self.engine_repo.delete_engine(&id).await;
    }

    async fn load_engine_variant(
        &self,
        _cx: &Context,
        engine_id: EngineId,
        variant_id: EngineSceneId,
    ) -> Option<EngineScene> {
        self.engine_repo
            .load_variant(&engine_id, &variant_id)
            .await
            .ok()
            .flatten()
    }
}

// endregion: --- EngineService impl

// region: --- PresetService impl

impl<B, M, L, E, R, P, So, Se> PresetService for SignalLive<B, M, L, E, R, P, So, Se>
where
    B: BlockRepo,
    M: ModuleRepo,
    L: LayerRepo,
    E: EngineRepo,
    R: RigRepo,
    P: ProfileRepo,
    So: SongRepo,
    Se: SetlistRepo,
{
    async fn list_presets_all(&self, _cx: &Context) -> Vec<Rig> {
        self.rig_repo.list_rigs().await.unwrap_or_default()
    }

    async fn load_preset_rig(&self, _cx: &Context, id: RigId) -> Option<Rig> {
        self.rig_repo.load_rig(&id).await.ok().flatten()
    }

    async fn save_preset(&self, _cx: &Context, rig: Rig) -> () {
        for variant in &rig.variants {
            if variant.validate_overrides().is_err() {
                return;
            }
        }
        let _ = self.rig_repo.save_rig(&rig).await;
    }

    async fn delete_preset(&self, _cx: &Context, id: RigId) -> () {
        let _ = self.rig_repo.delete_rig(&id).await;
    }

    async fn load_preset_variant(
        &self,
        _cx: &Context,
        rig_id: RigId,
        variant_id: RigSceneId,
    ) -> Option<RigScene> {
        self.rig_repo
            .load_variant(&rig_id, &variant_id)
            .await
            .ok()
            .flatten()
    }
}

// endregion: --- PresetService impl

// region: --- ProfileService impl

impl<B, M, L, E, R, P, So, Se> ProfileService for SignalLive<B, M, L, E, R, P, So, Se>
where
    B: BlockRepo,
    M: ModuleRepo,
    L: LayerRepo,
    E: EngineRepo,
    R: RigRepo,
    P: ProfileRepo,
    So: SongRepo,
    Se: SetlistRepo,
{
    async fn list_profiles(&self, _cx: &Context) -> Vec<Profile> {
        self.profile_repo.list_profiles().await.unwrap_or_default()
    }

    async fn load_profile(&self, _cx: &Context, id: ProfileId) -> Option<Profile> {
        self.profile_repo.load_profile(&id).await.ok().flatten()
    }

    async fn save_profile(&self, _cx: &Context, profile: Profile) -> () {
        for variant in &profile.patches {
            if variant.validate_overrides().is_err() {
                return;
            }
        }
        let _ = self.profile_repo.save_profile(&profile).await;
    }

    async fn delete_profile(&self, _cx: &Context, id: ProfileId) -> () {
        let _ = self.profile_repo.delete_profile(&id).await;
    }

    async fn load_profile_variant(
        &self,
        _cx: &Context,
        profile_id: ProfileId,
        variant_id: PatchId,
    ) -> Option<Patch> {
        self.profile_repo
            .load_variant(&profile_id, &variant_id)
            .await
            .ok()
            .flatten()
    }
}

// endregion: --- ProfileService impl

// region: --- SongService impl

impl<B, M, L, E, R, P, So, Se> SongService for SignalLive<B, M, L, E, R, P, So, Se>
where
    B: BlockRepo,
    M: ModuleRepo,
    L: LayerRepo,
    E: EngineRepo,
    R: RigRepo,
    P: ProfileRepo,
    So: SongRepo,
    Se: SetlistRepo,
{
    async fn list_songs(&self, _cx: &Context) -> Vec<Song> {
        self.song_repo.list_songs().await.unwrap_or_default()
    }

    async fn load_song(&self, _cx: &Context, id: SongId) -> Option<Song> {
        self.song_repo.load_song(&id).await.ok().flatten()
    }

    async fn save_song(&self, _cx: &Context, song: Song) -> () {
        for variant in &song.sections {
            if variant.validate_overrides().is_err() {
                return;
            }
        }
        let _ = self.song_repo.save_song(&song).await;
    }

    async fn delete_song(&self, _cx: &Context, id: SongId) -> () {
        let _ = self.song_repo.delete_song(&id).await;
    }

    async fn load_song_variant(
        &self,
        _cx: &Context,
        song_id: SongId,
        variant_id: SectionId,
    ) -> Option<Section> {
        self.song_repo
            .load_variant(&song_id, &variant_id)
            .await
            .ok()
            .flatten()
    }
}

// endregion: --- SongService impl

// region: --- SetlistService impl

impl<B, M, L, E, R, P, So, Se> SetlistService for SignalLive<B, M, L, E, R, P, So, Se>
where
    B: BlockRepo,
    M: ModuleRepo,
    L: LayerRepo,
    E: EngineRepo,
    R: RigRepo,
    P: ProfileRepo,
    So: SongRepo,
    Se: SetlistRepo,
{
    async fn list_setlists(&self, _cx: &Context) -> Vec<Setlist> {
        self.setlist_repo.list_setlists().await.unwrap_or_default()
    }

    async fn load_setlist(&self, _cx: &Context, id: SetlistId) -> Option<Setlist> {
        self.setlist_repo.load_setlist(&id).await.ok().flatten()
    }

    async fn save_setlist(&self, _cx: &Context, setlist: Setlist) -> () {
        let _ = self.setlist_repo.save_setlist(&setlist).await;
    }

    async fn delete_setlist(&self, _cx: &Context, id: SetlistId) -> () {
        let _ = self.setlist_repo.delete_setlist(&id).await;
    }

    async fn load_setlist_entry(
        &self,
        _cx: &Context,
        setlist_id: SetlistId,
        entry_id: SetlistEntryId,
    ) -> Option<SetlistEntry> {
        self.setlist_repo
            .load_entry(&setlist_id, &entry_id)
            .await
            .ok()
            .flatten()
    }
}

// endregion: --- SetlistService impl

// region: --- BrowserService impl

fn tags_from_name(name: &str) -> TagSet {
    infer_tags_from_name(name)
}

fn add_domain_tag(tags: &mut TagSet, value: &str) {
    tags.insert(StructuredTag::new(TagCategory::DomainLevel, value));
}

fn add_block_type_tag(tags: &mut TagSet, value: &str) {
    tags.insert(StructuredTag::new(TagCategory::Block, value));
}

fn add_module_type_tag(tags: &mut TagSet, value: &str) {
    tags.insert(StructuredTag::new(TagCategory::Module, value));
}

fn add_engine_type_tag(tags: &mut TagSet, value: &str) {
    tags.insert(StructuredTag::new(TagCategory::EngineType, value));
}

fn build_entry(
    kind: BrowserEntityKind,
    id: impl Into<String>,
    name: impl Into<String>,
    tags: TagSet,
    aliases: Vec<String>,
) -> BrowserEntry {
    BrowserEntry {
        node: BrowserNodeId {
            kind,
            id: id.into(),
        },
        name: name.into(),
        tags,
        aliases,
    }
}

impl<B, M, L, E, R, P, So, Se> BrowserService for SignalLive<B, M, L, E, R, P, So, Se>
where
    B: BlockRepo,
    M: ModuleRepo,
    L: LayerRepo,
    E: EngineRepo,
    R: RigRepo,
    P: ProfileRepo,
    So: SongRepo,
    Se: SetlistRepo,
{
    async fn browser_index(&self, _cx: &Context) -> BrowserIndex {
        let mut index = BrowserIndex::default();

        for block_type in ALL_BLOCK_TYPES {
            let collections = self
                .block_repo
                .list_block_collections(*block_type)
                .await
                .unwrap_or_default();

            for collection in collections {
                let mut ctags = tags_from_name(collection.name());
                ctags.merge(&TagSet::from_tags(&collection.metadata().tags));
                add_domain_tag(&mut ctags, "block_collection");
                add_block_type_tag(&mut ctags, block_type.as_str());

                index.push(build_entry(
                    BrowserEntityKind::BlockCollection,
                    collection.id().to_string(),
                    collection.name().to_string(),
                    ctags.clone(),
                    vec![block_type.display_name().to_string()],
                ));

                for variant in collection.snapshots() {
                    let mut vtags = tags_from_name(variant.name());
                    vtags.merge(&ctags);
                    vtags.merge(&TagSet::from_tags(&variant.metadata().tags));
                    add_domain_tag(&mut vtags, "block_variant");
                    index.push(build_entry(
                        BrowserEntityKind::BlockVariant,
                        variant.id().to_string(),
                        variant.name().to_string(),
                        vtags,
                        vec![collection.name().to_string()],
                    ));
                }
            }
        }

        let module_collections = self
            .module_repo
            .list_module_collections()
            .await
            .unwrap_or_default();
        for collection in module_collections {
            let mut ctags = tags_from_name(collection.name());
            ctags.merge(&TagSet::from_tags(&collection.metadata().tags));
            add_domain_tag(&mut ctags, "module_collection");
            add_module_type_tag(&mut ctags, collection.module_type().as_str());
            index.push(build_entry(
                BrowserEntityKind::ModuleCollection,
                collection.id().to_string(),
                collection.name().to_string(),
                ctags.clone(),
                vec![collection.module_type().display_name().to_string()],
            ));

            for variant in collection.snapshots() {
                let mut vtags = tags_from_name(variant.name());
                vtags.merge(&ctags);
                vtags.merge(&TagSet::from_tags(&variant.metadata().tags));
                add_domain_tag(&mut vtags, "module_variant");
                index.push(build_entry(
                    BrowserEntityKind::ModuleVariant,
                    variant.id().to_string(),
                    variant.name().to_string(),
                    vtags,
                    vec![collection.name().to_string()],
                ));
            }
        }

        let layers = self.layer_repo.list_layers().await.unwrap_or_default();
        for layer in layers {
            let mut ctags = tags_from_name(&layer.name);
            ctags.merge(&TagSet::from_tags(&layer.metadata.tags));
            add_domain_tag(&mut ctags, "layer_collection");
            add_engine_type_tag(&mut ctags, layer.engine_type.as_str());
            index.push(build_entry(
                BrowserEntityKind::LayerCollection,
                layer.id.to_string(),
                layer.name.clone(),
                ctags.clone(),
                vec![layer.engine_type.as_str().to_string()],
            ));

            for variant in &layer.variants {
                let mut vtags = tags_from_name(&variant.name);
                vtags.merge(&ctags);
                vtags.merge(&TagSet::from_tags(&variant.metadata.tags));
                add_domain_tag(&mut vtags, "layer_variant");
                index.push(build_entry(
                    BrowserEntityKind::LayerVariant,
                    variant.id.to_string(),
                    variant.name.clone(),
                    vtags,
                    vec![layer.name.clone()],
                ));
            }
        }

        let engines = self.engine_repo.list_engines().await.unwrap_or_default();
        for engine in engines {
            let mut ctags = tags_from_name(&engine.name);
            ctags.merge(&TagSet::from_tags(&engine.metadata.tags));
            add_domain_tag(&mut ctags, "engine_collection");
            add_engine_type_tag(&mut ctags, engine.engine_type.as_str());
            index.push(build_entry(
                BrowserEntityKind::EngineCollection,
                engine.id.to_string(),
                engine.name.clone(),
                ctags.clone(),
                vec![engine.engine_type.as_str().to_string()],
            ));

            for variant in &engine.variants {
                let mut vtags = tags_from_name(&variant.name);
                vtags.merge(&ctags);
                vtags.merge(&TagSet::from_tags(&variant.metadata.tags));
                add_domain_tag(&mut vtags, "engine_variant");
                index.push(build_entry(
                    BrowserEntityKind::EngineVariant,
                    variant.id.to_string(),
                    variant.name.clone(),
                    vtags,
                    vec![engine.name.clone()],
                ));
            }
        }

        let rigs = self.rig_repo.list_rigs().await.unwrap_or_default();
        for rig in rigs {
            let mut ctags = tags_from_name(&rig.name);
            ctags.merge(&TagSet::from_tags(&rig.metadata.tags));
            add_domain_tag(&mut ctags, "rig_collection");
            if let Some(rig_type) = rig.rig_type {
                ctags.insert(StructuredTag::new(TagCategory::RigType, rig_type.as_str()));
            }
            index.push(build_entry(
                BrowserEntityKind::RigCollection,
                rig.id.to_string(),
                rig.name.clone(),
                ctags.clone(),
                vec![],
            ));

            for variant in &rig.variants {
                let mut vtags = tags_from_name(&variant.name);
                vtags.merge(&ctags);
                vtags.merge(&TagSet::from_tags(&variant.metadata.tags));
                add_domain_tag(&mut vtags, "rig_variant");
                index.push(build_entry(
                    BrowserEntityKind::RigVariant,
                    variant.id.to_string(),
                    variant.name.clone(),
                    vtags,
                    vec![rig.name.clone()],
                ));
            }
        }

        let profiles = self.profile_repo.list_profiles().await.unwrap_or_default();
        for profile in profiles {
            let mut ctags = tags_from_name(&profile.name);
            ctags.merge(&TagSet::from_tags(&profile.metadata.tags));
            add_domain_tag(&mut ctags, "profile_collection");
            index.push(build_entry(
                BrowserEntityKind::ProfileCollection,
                profile.id.to_string(),
                profile.name.clone(),
                ctags.clone(),
                vec![],
            ));

            for variant in &profile.patches {
                let mut vtags = tags_from_name(&variant.name);
                vtags.merge(&ctags);
                vtags.merge(&TagSet::from_tags(&variant.metadata.tags));
                add_domain_tag(&mut vtags, "profile_variant");
                index.push(build_entry(
                    BrowserEntityKind::ProfileVariant,
                    variant.id.to_string(),
                    variant.name.clone(),
                    vtags,
                    vec![profile.name.clone()],
                ));
            }
        }

        let songs = self.song_repo.list_songs().await.unwrap_or_default();
        for song in songs {
            let mut ctags = tags_from_name(&song.name);
            ctags.merge(&TagSet::from_tags(&song.metadata.tags));
            add_domain_tag(&mut ctags, "song_collection");
            if let Some(artist) = &song.artist {
                ctags.insert(StructuredTag::new(TagCategory::Custom, artist));
            }
            index.push(build_entry(
                BrowserEntityKind::SongCollection,
                song.id.to_string(),
                song.name.clone(),
                ctags.clone(),
                song.artist.clone().into_iter().collect(),
            ));

            for variant in &song.sections {
                let mut vtags = tags_from_name(&variant.name);
                vtags.merge(&ctags);
                vtags.merge(&TagSet::from_tags(&variant.metadata.tags));
                add_domain_tag(&mut vtags, "song_variant");
                index.push(build_entry(
                    BrowserEntityKind::SongVariant,
                    variant.id.to_string(),
                    variant.name.clone(),
                    vtags,
                    vec![song.name.clone()],
                ));
            }
        }

        let setlists = self.setlist_repo.list_setlists().await.unwrap_or_default();
        for setlist in setlists {
            let mut ctags = tags_from_name(&setlist.name);
            ctags.merge(&TagSet::from_tags(&setlist.metadata.tags));
            add_domain_tag(&mut ctags, "setlist_collection");
            index.push(build_entry(
                BrowserEntityKind::SetlistCollection,
                setlist.id.to_string(),
                setlist.name.clone(),
                ctags.clone(),
                vec![],
            ));

            for variant in &setlist.entries {
                let mut vtags = tags_from_name(&variant.name);
                vtags.merge(&ctags);
                vtags.merge(&TagSet::from_tags(&variant.metadata.tags));
                add_domain_tag(&mut vtags, "setlist_variant");
                index.push(build_entry(
                    BrowserEntityKind::SetlistVariant,
                    variant.id.to_string(),
                    variant.name.clone(),
                    vtags,
                    vec![setlist.name.clone()],
                ));
            }
        }

        index
    }

    async fn browse(&self, cx: &Context, query: BrowserQuery) -> Vec<BrowserHit> {
        let index: BrowserIndex = BrowserService::browser_index(self, cx).await;
        index.query(&query, &TagWeights::default())
    }
}

// endregion: --- BrowserService impl

// region: --- ResolveService impl

fn apply_block_parameter_overrides(block: &mut Block, overrides: &[BlockParameterOverride]) {
    for ov in overrides {
        if let Some((idx, _)) = block
            .parameters()
            .iter()
            .enumerate()
            .find(|(_, p)| p.id() == ov.parameter_id())
        {
            block.set_parameter_value(idx, ov.value().get());
        }
    }
}

fn merge_override_levels(levels: &[Vec<signal_proto::overrides::Override>]) -> Vec<signal_proto::overrides::Override> {
    // nearest-scope-wins: later levels replace earlier path entries
    let mut by_path: HashMap<String, signal_proto::overrides::Override> = HashMap::new();
    let mut order: Vec<String> = Vec::new();
    for level in levels {
        for ov in level {
            let key = ov.path.as_str();
            if !by_path.contains_key(&key) {
                order.push(key.clone());
            }
            by_path.insert(key, ov.clone());
        }
    }
    order
        .into_iter()
        .filter_map(|k| by_path.remove(&k))
        .collect()
}

fn map_policy_err(scope: &str, err: signal_proto::override_policy::OverridePolicyError) -> ResolveError {
    ResolveError::InvalidReference(format!("{scope} override policy violation: {err:?}"))
}

fn normalize_ref_id(raw: &str) -> String {
    let looks_like_uuid = raw.len() == 36
        && [8, 13, 18, 23].into_iter().all(|i| raw.as_bytes()[i] == b'-')
        && raw
            .bytes()
            .enumerate()
            .all(|(i, b)| [8, 13, 18, 23].contains(&i) || b.is_ascii_hexdigit());
    if looks_like_uuid {
        raw.to_string()
    } else {
        signal_proto::seed_id(raw).to_string()
    }
}

fn id_matches(entity_id: &str, path_or_alias: &str) -> bool {
    entity_id == path_or_alias || entity_id == normalize_ref_id(path_or_alias)
}

fn segment_engine(path: &signal_proto::overrides::NodePath) -> Option<&str> {
    path.segments().iter().find_map(|seg| match seg {
        NodePathSegment::Engine(v) => Some(v.as_str()),
        _ => None,
    })
}

fn segment_layer(path: &signal_proto::overrides::NodePath) -> Option<&str> {
    path.segments().iter().find_map(|seg| match seg {
        NodePathSegment::Layer(v) => Some(v.as_str()),
        _ => None,
    })
}

fn segment_module(path: &signal_proto::overrides::NodePath) -> Option<&str> {
    path.segments().iter().find_map(|seg| match seg {
        NodePathSegment::Module(v) => Some(v.as_str()),
        _ => None,
    })
}

fn segment_block(path: &signal_proto::overrides::NodePath) -> Option<&str> {
    path.segments().iter().find_map(|seg| match seg {
        NodePathSegment::Block(v) => Some(v.as_str()),
        _ => None,
    })
}

fn segment_param(path: &signal_proto::overrides::NodePath) -> Option<&str> {
    path.segments().iter().find_map(|seg| match seg {
        NodePathSegment::Parameter(v) => Some(v.as_str()),
        _ => None,
    })
}

fn apply_effective_set_overrides(graph: &mut ResolvedGraph) {
    for ov in &graph.effective_overrides {
        let NodeOverrideOp::Set(value) = &ov.op else {
            continue;
        };

        let engine_id = segment_engine(&ov.path);
        let layer_id = segment_layer(&ov.path);
        let module_id = segment_module(&ov.path);
        let block_id = segment_block(&ov.path);
        let Some(parameter_id) = segment_param(&ov.path) else {
            continue;
        };

        for engine in &mut graph.engines {
            if let Some(expected) = engine_id {
                if !id_matches(engine.engine_id.as_str(), expected) {
                    continue;
                }
            }
            for layer in &mut engine.layers {
                if let Some(expected) = layer_id {
                    if !id_matches(layer.layer_id.as_str(), expected) {
                        continue;
                    }
                }

                for module in &mut layer.modules {
                    if let Some(expected) = module_id {
                        if !id_matches(module.source_preset_id.as_str(), expected) {
                            continue;
                        }
                    }
                    for rb in &mut module.blocks {
                        if let Some(expected) = block_id {
                            if !id_matches(&rb.node_id, expected) && rb.node_id != expected {
                                continue;
                            }
                        }
                        if let Some((idx, _)) = rb
                            .block
                            .parameters()
                            .iter()
                            .enumerate()
                            .find(|(_, p)| p.id() == parameter_id)
                        {
                            rb.block.set_parameter_value(idx, value.get());
                        }
                    }
                }

                for rb in &mut layer.standalone_blocks {
                    if let Some(expected) = block_id {
                        if !id_matches(&rb.node_id, expected) && rb.node_id != expected {
                            continue;
                        }
                    }
                    if let Some((idx, _)) = rb
                        .block
                        .parameters()
                        .iter()
                        .enumerate()
                        .find(|(_, p)| p.id() == parameter_id)
                    {
                        rb.block.set_parameter_value(idx, value.get());
                    }
                }
            }
        }
    }
}

impl<B, M, L, E, R, P, So, Se> SignalLive<B, M, L, E, R, P, So, Se>
where
    B: BlockRepo,
    M: ModuleRepo,
    L: LayerRepo,
    E: EngineRepo,
    R: RigRepo,
    P: ProfileRepo,
    So: SongRepo,
    Se: SetlistRepo,
{
    async fn resolve_block_ref(
        &self,
        block_type: BlockType,
        preset_id: &PresetId,
        snapshot_id: Option<&SnapshotId>,
        node_id: String,
        label: String,
    ) -> Result<ResolvedBlock, ResolveError> {
        let snap = match snapshot_id {
            Some(variant_id) => self
                .block_repo
                .load_block_variant(block_type, preset_id, variant_id)
                .await
                .map_err(|e| ResolveError::NotFound(format!("block variant load failed: {e}")))?,
            None => self
                .block_repo
                .load_block_default_variant(block_type, preset_id)
                .await
                .map_err(|e| ResolveError::NotFound(format!("block default variant load failed: {e}")))?,
        }
        .ok_or_else(|| {
            ResolveError::InvalidReference(match snapshot_id {
                Some(variant_id) => format!(
                    "missing block variant: type={} preset={} variant={}",
                    block_type.as_str(),
                    preset_id,
                    variant_id
                ),
                None => format!(
                    "missing block default variant: type={} preset={}",
                    block_type.as_str(),
                    preset_id
                ),
            })
        })?;

        Ok(ResolvedBlock {
            node_id,
            label,
            block_type,
            source_preset_id: Some(preset_id.clone()),
            source_variant_id: Some(snap.id().clone()),
            block: snap.block(),
        })
    }

    async fn resolve_standalone_block_ref(
        &self,
        preset_id: &PresetId,
        snapshot_id: Option<&SnapshotId>,
        node_id: String,
        label: String,
    ) -> Result<ResolvedBlock, ResolveError> {
        for block_type in ALL_BLOCK_TYPES {
            let resolved = self
                .resolve_block_ref(
                    *block_type,
                    preset_id,
                    snapshot_id,
                    node_id.clone(),
                    label.clone(),
                )
                .await;
            if let Ok(resolved) = resolved {
                return Ok(resolved);
            }
        }
        Err(ResolveError::InvalidReference(match snapshot_id {
            Some(variant_id) => format!(
                "standalone block variant not found for any block type: preset={} variant={}",
                preset_id, variant_id
            ),
            None => format!(
                "standalone block default variant not found for any block type: preset={}",
                preset_id
            ),
        }))
    }

    async fn resolve_module_snapshot(&self, snapshot: &ModuleSnapshot) -> Result<ResolvedModule, ResolveError> {
        let mut blocks = Vec::new();
        for block in snapshot.module().blocks() {
            let mut resolved = match block.source() {
                ModuleBlockSource::PresetDefault { preset_id, .. } => self
                    .resolve_block_ref(
                        block.block_type(),
                        preset_id,
                        None,
                        block.id().to_string(),
                        block.label().to_string(),
                    )
                    .await?,
                ModuleBlockSource::PresetSnapshot {
                    preset_id,
                    snapshot_id,
                    ..
                } => self
                    .resolve_block_ref(
                        block.block_type(),
                        preset_id,
                        Some(snapshot_id),
                        block.id().to_string(),
                        block.label().to_string(),
                    )
                    .await?,
                ModuleBlockSource::Inline { block: inline } => ResolvedBlock {
                    node_id: block.id().to_string(),
                    label: block.label().to_string(),
                    block_type: block.block_type(),
                    source_preset_id: None,
                    source_variant_id: None,
                    block: inline.clone(),
                },
            };
            apply_block_parameter_overrides(&mut resolved.block, block.overrides());
            blocks.push(resolved);
        }
        Ok(ResolvedModule {
            source_preset_id: ModulePresetId::new(),
            source_variant_id: snapshot.id().clone(),
            blocks,
        })
    }

    async fn resolve_module_ref(
        &self,
        preset_id: &ModulePresetId,
        variant_id: Option<&ModuleSnapshotId>,
    ) -> Result<ResolvedModule, ResolveError> {
        let snapshot = match variant_id {
            Some(variant_id) => self
                .module_repo
                .load_module_variant(preset_id, variant_id)
                .await
                .map_err(|e| ResolveError::NotFound(format!("module variant load failed: {e}")))?,
            None => self
                .module_repo
                .load_module_default_variant(preset_id)
                .await
                .map_err(|e| ResolveError::NotFound(format!("module default variant load failed: {e}")))?,
        }
        .ok_or_else(|| {
            ResolveError::InvalidReference(match variant_id {
                Some(variant_id) => format!(
                    "missing module variant: preset={} variant={}",
                    preset_id, variant_id
                ),
                None => format!("missing module default variant: preset={preset_id}"),
            })
        })?;
        let mut resolved = self.resolve_module_snapshot(&snapshot).await?;
        resolved.source_preset_id = preset_id.clone();
        resolved.source_variant_id = snapshot.id().clone();
        Ok(resolved)
    }

    async fn resolve_layer_tree(
        &self,
        engine_id: &EngineId,
        start_layer_id: LayerId,
        start_variant_id: LayerSnapshotId,
        start_source: LayerSource,
        selection_overrides: &[signal_proto::overrides::Override],
    ) -> Result<Vec<ResolvedLayer>, ResolveError> {
        #[derive(Clone)]
        enum Phase {
            Explore,
            Build,
        }
        #[derive(Clone)]
        struct Frame {
            layer_id: LayerId,
            variant_id: LayerSnapshotId,
            source: LayerSource,
            phase: Phase,
        }

        let mut stack = vec![Frame {
            layer_id: start_layer_id,
            variant_id: start_variant_id,
            source: start_source,
            phase: Phase::Explore,
        }];
        let mut active: HashSet<String> = HashSet::new();
        let mut loaded: HashMap<String, (Layer, LayerSnapshot, LayerSource)> = HashMap::new();
        let mut resolved = Vec::new();

        while let Some(frame) = stack.pop() {
            let key = format!("{}::{}", frame.layer_id, frame.variant_id);
            match frame.phase {
                Phase::Explore => {
                    if active.contains(&key) {
                        return Err(ResolveError::CycleDetected(format!(
                            "layer variant cycle at {key}"
                        )));
                    }
                    active.insert(key.clone());
                    let layer = self
                        .layer_repo
                        .load_layer(&frame.layer_id)
                        .await
                        .map_err(|e| ResolveError::NotFound(format!("layer load failed: {e}")))?
                        .ok_or_else(|| ResolveError::NotFound(format!("layer not found: {}", frame.layer_id)))?;
                    let variant = layer
                        .variant(&frame.variant_id)
                        .cloned()
                        .ok_or_else(|| ResolveError::NotFound(format!(
                            "layer variant not found: {}::{}",
                            frame.layer_id, frame.variant_id
                        )))?;
                    validate_overrides::<SnapshotPolicy>(&variant.overrides)
                        .map_err(|e| map_policy_err("layer snapshot", e))?;
                    loaded.insert(key.clone(), (layer.clone(), variant.clone(), frame.source.clone()));
                    stack.push(Frame {
                        layer_id: frame.layer_id,
                        variant_id: frame.variant_id,
                        source: frame.source,
                        phase: Phase::Build,
                    });
                    for layer_ref in variant.layer_refs.iter().rev() {
                        let child_layer = self
                            .layer_repo
                            .load_layer(&layer_ref.collection_id)
                            .await
                            .map_err(|e| ResolveError::NotFound(format!("layer load failed: {e}")))?
                            .ok_or_else(|| {
                                ResolveError::InvalidReference(format!(
                                    "missing layer ref: {}",
                                    layer_ref.collection_id
                                ))
                            })?;
                        let child_variant_id = layer_ref
                            .variant_id
                            .clone()
                            .unwrap_or_else(|| child_layer.default_variant_id.clone());
                        stack.push(Frame {
                            layer_id: child_layer.id.clone(),
                            variant_id: child_variant_id,
                            source: LayerSource::InlinedInParent,
                            phase: Phase::Explore,
                        });
                    }
                }
                Phase::Build => {
                    active.remove(&key);
                    let (layer, variant, source) = loaded
                        .remove(&key)
                        .ok_or_else(|| ResolveError::InvalidReference(format!("missing loaded frame {key}")))?;

                    let mut module_refs = variant.module_refs.clone();
                    let mut block_refs = variant.block_refs.clone();
                    let mut disabled_module_ids: HashSet<String> = HashSet::new();
                    let mut disabled_block_ids: HashSet<String> = HashSet::new();

                    for ov in selection_overrides {
                        if let Some(seg_engine) = segment_engine(&ov.path) {
                            if !id_matches(engine_id.as_str(), seg_engine) {
                                continue;
                            }
                        }
                        if let Some(seg_layer) = segment_layer(&ov.path) {
                            if !id_matches(layer.id.as_str(), seg_layer) {
                                continue;
                            }
                        } else {
                            continue;
                        }

                        if let Some(seg_module) = segment_module(&ov.path) {
                            if let Some(mr) = module_refs
                                .iter_mut()
                                .find(|mr| id_matches(mr.collection_id.as_str(), seg_module))
                            {
                                match &ov.op {
                                    NodeOverrideOp::ReplaceRef(next) => {
                                        let next_variant = ModuleSnapshotId::from(normalize_ref_id(next));
                                        let exists = self
                                            .module_repo
                                            .load_module_variant(&mr.collection_id, &next_variant)
                                            .await
                                            .map_err(|e| {
                                                ResolveError::NotFound(format!(
                                                    "module variant load failed during replace_ref: {e}"
                                                ))
                                            })?
                                            .is_some();
                                        if !exists {
                                            return Err(ResolveError::InvalidReference(format!(
                                                "replace_ref target module variant not found: module={} variant={} path={}",
                                                mr.collection_id,
                                                next_variant,
                                                ov.path.as_str()
                                            )));
                                        }
                                        mr.variant_id = Some(next_variant);
                                    }
                                    NodeOverrideOp::Enable(false) | NodeOverrideOp::Bypass(true) => {
                                        disabled_module_ids.insert(mr.collection_id.to_string());
                                    }
                                    _ => {}
                                }
                            }
                            continue;
                        }

                        if let Some(seg_block) = segment_block(&ov.path) {
                            if let Some(br) = block_refs
                                .iter_mut()
                                .find(|br| id_matches(br.collection_id.as_str(), seg_block))
                            {
                                match &ov.op {
                                    NodeOverrideOp::ReplaceRef(next) => {
                                        let next_variant = SnapshotId::from(normalize_ref_id(next));
                                        // Validate that the replacement exists for this block collection.
                                        self.resolve_standalone_block_ref(
                                            &br.collection_id,
                                            Some(&next_variant),
                                            br.collection_id.to_string(),
                                            br.collection_id.to_string(),
                                        )
                                        .await?;
                                        br.variant_id = Some(next_variant);
                                    }
                                    NodeOverrideOp::Enable(false) | NodeOverrideOp::Bypass(true) => {
                                        disabled_block_ids.insert(br.collection_id.to_string());
                                    }
                                    _ => {}
                                }
                            }
                        }
                    }

                    let mut modules = Vec::new();
                    for mr in &module_refs {
                        if disabled_module_ids.contains(&mr.collection_id.to_string()) {
                            continue;
                        }
                        match self
                            .resolve_module_ref(&mr.collection_id, mr.variant_id.as_ref())
                            .await
                        {
                            Ok(module) => modules.push(module),
                            Err(ResolveError::InvalidReference(_)) => {
                                // Keep existing seed/runtime behavior: unresolved base refs are skipped.
                                // ReplaceRef targets are still fail-fast validated above.
                            }
                            Err(e) => return Err(e),
                        }
                    }

                    let mut standalone_blocks = Vec::new();
                    for br in &block_refs {
                        if disabled_block_ids.contains(&br.collection_id.to_string()) {
                            continue;
                        }
                        match self
                            .resolve_standalone_block_ref(
                                &br.collection_id,
                                br.variant_id.as_ref(),
                                br.collection_id.to_string(),
                                br.collection_id.to_string(),
                            )
                            .await
                        {
                            Ok(block) => standalone_blocks.push(block),
                            Err(ResolveError::InvalidReference(_)) => {
                                // Keep existing seed/runtime behavior: unresolved base refs are skipped.
                                // ReplaceRef targets are still fail-fast validated above.
                            }
                            Err(e) => return Err(e),
                        }
                    }

                    resolved.push(ResolvedLayer {
                        layer_id: layer.id,
                        layer_variant_id: variant.id,
                        source,
                        modules,
                        standalone_blocks,
                    });
                }
            }
        }

        Ok(resolved)
    }

    async fn resolve_target_to_rig_scene(
        &self,
        target: &ResolveTarget,
    ) -> Result<(RigId, RigSceneId, Vec<signal_proto::overrides::Override>), ResolveError> {
        match target {
            ResolveTarget::RigScene { rig_id, scene_id } => {
                let rig = self
                    .rig_repo
                    .load_rig(rig_id)
                    .await
                    .map_err(|e| ResolveError::NotFound(format!("rig load failed: {e}")))?
                    .ok_or_else(|| ResolveError::NotFound(format!("rig not found: {rig_id}")))?;
                let scene = rig
                    .variant(scene_id)
                    .cloned()
                    .ok_or_else(|| ResolveError::NotFound(format!("rig scene not found: {scene_id}")))?;
                validate_overrides::<ScenePolicy>(&scene.overrides)
                    .map_err(|e| map_policy_err("rig scene", e))?;
                Ok((rig.id.clone(), scene.id.clone(), scene.overrides.clone()))
            }
            ResolveTarget::ProfilePatch {
                profile_id,
                patch_id,
            } => {
                let profile = self
                    .profile_repo
                    .load_profile(profile_id)
                    .await
                    .map_err(|e| ResolveError::NotFound(format!("profile load failed: {e}")))?
                    .ok_or_else(|| ResolveError::NotFound(format!("profile not found: {profile_id}")))?;
                let patch = profile
                    .patch(patch_id)
                    .cloned()
                    .ok_or_else(|| ResolveError::NotFound(format!("patch not found: {patch_id}")))?;
                validate_overrides::<FreePolicy>(&patch.overrides)
                    .map_err(|e| map_policy_err("profile patch", e))?;
                Ok((patch.rig_id, patch.rig_variant_id, patch.overrides))
            }
            ResolveTarget::SongSection {
                song_id,
                section_id,
            } => {
                let song = self
                    .song_repo
                    .load_song(song_id)
                    .await
                    .map_err(|e| ResolveError::NotFound(format!("song load failed: {e}")))?
                    .ok_or_else(|| ResolveError::NotFound(format!("song not found: {song_id}")))?;
                let section = song
                    .section(section_id)
                    .cloned()
                    .ok_or_else(|| ResolveError::NotFound(format!("section not found: {section_id}")))?;
                validate_overrides::<FreePolicy>(&section.overrides)
                    .map_err(|e| map_policy_err("song section", e))?;
                match section.source {
                    signal_proto::song::SectionSource::RigScene { rig_id, scene_id } => {
                        Ok((rig_id, scene_id, section.overrides))
                    }
                    signal_proto::song::SectionSource::Patch { patch_id } => {
                        let profiles = self.profile_repo.list_profiles().await.map_err(|e| {
                            ResolveError::NotFound(format!("profiles load failed: {e}"))
                        })?;
                        let patch = profiles
                            .iter()
                            .find_map(|p| p.patch(&patch_id))
                            .cloned()
                            .ok_or_else(|| {
                                ResolveError::NotFound(format!(
                                    "section patch source not found: {patch_id}"
                                ))
                            })?;
                        validate_overrides::<FreePolicy>(&patch.overrides)
                            .map_err(|e| map_policy_err("section source patch", e))?;
                        let mut ovs = patch.overrides.clone();
                        ovs.extend(section.overrides);
                        Ok((patch.rig_id, patch.rig_variant_id, ovs))
                    }
                }
            }
        }
    }
}

impl<B, M, L, E, R, P, So, Se> ResolveService for SignalLive<B, M, L, E, R, P, So, Se>
where
    B: BlockRepo,
    M: ModuleRepo,
    L: LayerRepo,
    E: EngineRepo,
    R: RigRepo,
    P: ProfileRepo,
    So: SongRepo,
    Se: SetlistRepo,
{
    async fn resolve_target(
        &self,
        _cx: &Context,
        target: ResolveTarget,
    ) -> Result<ResolvedGraph, ResolveError> {
        let (rig_id, rig_scene_id, higher_scope_overrides) =
            self.resolve_target_to_rig_scene(&target).await?;

        let rig = self
            .rig_repo
            .load_rig(&rig_id)
            .await
            .map_err(|e| ResolveError::NotFound(format!("rig load failed: {e}")))?
            .ok_or_else(|| ResolveError::NotFound(format!("rig not found: {rig_id}")))?;
        let rig_scene = rig
            .variant(&rig_scene_id)
            .cloned()
            .ok_or_else(|| ResolveError::NotFound(format!("rig scene not found: {rig_scene_id}")))?;
        validate_overrides::<ScenePolicy>(&rig_scene.overrides)
            .map_err(|e| map_policy_err("rig scene", e))?;

        let mut engines = Vec::new();
        let global_overrides =
            merge_override_levels(&[rig_scene.overrides.clone(), higher_scope_overrides.clone()]);
        let mut level_overrides = vec![global_overrides.clone()];

        for engine_sel in &rig_scene.engine_selections {
            let mut selected_engine_scene_id = engine_sel.variant_id.clone();
            let mut engine_enabled = true;
            let mut engine_replace_path: Option<String> = None;
            for ov in &global_overrides {
                let Some(seg_engine) = segment_engine(&ov.path) else {
                    continue;
                };
                if !id_matches(engine_sel.engine_id.as_str(), seg_engine) {
                    continue;
                }
                if segment_layer(&ov.path).is_none()
                    && segment_module(&ov.path).is_none()
                    && segment_block(&ov.path).is_none()
                    && segment_param(&ov.path).is_none()
                {
                    match &ov.op {
                        NodeOverrideOp::ReplaceRef(next) => {
                            selected_engine_scene_id = EngineSceneId::from(normalize_ref_id(next));
                            engine_replace_path = Some(ov.path.as_str());
                        }
                        NodeOverrideOp::Enable(false) | NodeOverrideOp::Bypass(true) => {
                            engine_enabled = false;
                        }
                        _ => {}
                    }
                }
            }
            if !engine_enabled {
                continue;
            }

            let engine = self
                .engine_repo
                .load_engine(&engine_sel.engine_id)
                .await
                .map_err(|e| ResolveError::NotFound(format!("engine load failed: {e}")))?
                .ok_or_else(|| {
                    ResolveError::InvalidReference(format!(
                        "missing engine ref: {}",
                        engine_sel.engine_id
                    ))
                })?;
            if engine_replace_path.is_some()
                && !engine
                    .variants
                    .iter()
                    .any(|v| v.id == selected_engine_scene_id)
            {
                return Err(ResolveError::InvalidReference(format!(
                    "replace_ref target engine scene not found: engine={} variant={} path={}",
                    engine.id,
                    selected_engine_scene_id,
                    engine_replace_path.unwrap_or_default()
                )));
            }
            let engine_scene = engine
                .variant(&selected_engine_scene_id)
                .cloned()
                .ok_or_else(|| {
                    ResolveError::InvalidReference(format!(
                        "missing engine scene ref: {}::{}",
                        engine_sel.engine_id, selected_engine_scene_id
                    ))
                })?;
            validate_overrides::<ScenePolicy>(&engine_scene.overrides)
                .map_err(|e| map_policy_err("engine scene", e))?;
            let engine_scope_overrides =
                merge_override_levels(&[global_overrides.clone(), engine_scene.overrides.clone()]);
            level_overrides.push(engine_scope_overrides.clone());

            let mut resolved_layers = Vec::new();
            for layer_sel in &engine_scene.layer_selections {
                let mut selected_layer_variant_id = layer_sel.variant_id.clone();
                let mut layer_enabled = true;
                let mut layer_replace_path: Option<String> = None;
                for ov in &engine_scope_overrides {
                    let Some(seg_layer) = segment_layer(&ov.path) else {
                        continue;
                    };
                    if !id_matches(layer_sel.layer_id.as_str(), seg_layer) {
                        continue;
                    }
                    if let Some(seg_engine) = segment_engine(&ov.path) {
                        if !id_matches(engine.id.as_str(), seg_engine) {
                            continue;
                        }
                    }
                    if segment_module(&ov.path).is_none()
                        && segment_block(&ov.path).is_none()
                        && segment_param(&ov.path).is_none()
                    {
                        match &ov.op {
                            NodeOverrideOp::ReplaceRef(next) => {
                                selected_layer_variant_id =
                                    LayerSnapshotId::from(normalize_ref_id(next));
                                layer_replace_path = Some(ov.path.as_str());
                            }
                            NodeOverrideOp::Enable(false) | NodeOverrideOp::Bypass(true) => {
                                layer_enabled = false;
                            }
                            _ => {}
                        }
                    }
                }
                if !layer_enabled {
                    continue;
                }
                let selected_layer_variant = self
                    .layer_repo
                    .load_variant(&layer_sel.layer_id, &selected_layer_variant_id)
                    .await
                    .map_err(|e| ResolveError::NotFound(format!("layer variant load failed: {e}")))?;
                if selected_layer_variant.is_none() {
                    if let Some(path) = layer_replace_path {
                        return Err(ResolveError::InvalidReference(format!(
                            "replace_ref target layer variant not found: layer={} variant={} path={}",
                            layer_sel.layer_id, selected_layer_variant_id, path
                        )));
                    }
                    return Err(ResolveError::InvalidReference(format!(
                        "missing layer variant ref: layer={} variant={}",
                        layer_sel.layer_id, selected_layer_variant_id
                    )));
                }

                let mut layers = self
                    .resolve_layer_tree(
                        &engine.id,
                        layer_sel.layer_id.clone(),
                        selected_layer_variant_id.clone(),
                        LayerSource::LayerPreset {
                            layer_id: layer_sel.layer_id.clone(),
                            variant_id: selected_layer_variant_id.clone(),
                        },
                        &engine_scope_overrides,
                    )
                    .await?;
                resolved_layers.append(&mut layers);

                if let Some(layer) = selected_layer_variant {
                    level_overrides.push(layer.overrides.clone());
                }
            }

            engines.push(ResolvedEngine {
                engine_id: engine.id.clone(),
                engine_scene_id: engine_scene.id.clone(),
                layers: resolved_layers,
            });
        }

        let mut graph = ResolvedGraph {
            target,
            rig_id,
            rig_scene_id,
            engines,
            effective_overrides: merge_override_levels(&level_overrides),
        };
        apply_effective_set_overrides(&mut graph);
        Ok(graph)
    }
}

    // endregion: --- ResolveService impl

#[cfg(test)]
mod tests {
    use super::*;
    use signal_proto::seed_id;
    use signal_storage::{
        runtime_seed_bundle, BlockRepoLive, Database, EngineRepoLive, LayerRepoLive,
        ModuleRepoLive, ProfileRepoLive, RigRepoLive, SetlistRepoLive, SongRepoLive,
    };

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    fn test_context() -> Context {
        Context::new(
            Default::default(),
            Default::default(),
            Default::default(),
            Default::default(),
            vec![],
        )
    }

    async fn seeded_service() -> Result<
        SignalLive<
            BlockRepoLive,
            ModuleRepoLive,
            LayerRepoLive,
            EngineRepoLive,
            RigRepoLive,
            ProfileRepoLive,
            SongRepoLive,
            SetlistRepoLive,
        >,
    > {
        let db = Database::connect("sqlite::memory:").await?;
        let seeds = runtime_seed_bundle();
        let block_repo = BlockRepoLive::new(db.clone());
        block_repo.init_schema().await?;
        block_repo
            .reseed_defaults(&seeds.block_collections)
            .await?;
        let module_repo = ModuleRepoLive::new(db.clone());
        module_repo.init_schema().await?;
        module_repo
            .reseed_defaults(&seeds.module_collections)
            .await?;
        let layer_repo = LayerRepoLive::new(db.clone());
        layer_repo.init_schema().await?;
        for layer in seeds.layers {
            layer_repo.save_layer(&layer).await?;
        }
        let engine_repo = EngineRepoLive::new(db.clone());
        engine_repo.init_schema().await?;
        for engine in seeds.engines {
            engine_repo.save_engine(&engine).await?;
        }
        let rig_repo = RigRepoLive::new(db.clone());
        rig_repo.init_schema().await?;
        for rig in seeds.rigs {
            rig_repo.save_rig(&rig).await?;
        }
        let profile_repo = ProfileRepoLive::new(db.clone());
        profile_repo.init_schema().await?;
        for profile in seeds.profiles {
            profile_repo.save_profile(&profile).await?;
        }
        let song_repo = SongRepoLive::new(db.clone());
        song_repo.init_schema().await?;
        for song in seeds.songs {
            song_repo.save_song(&song).await?;
        }
        let setlist_repo = SetlistRepoLive::new(db);
        setlist_repo.init_schema().await?;
        for setlist in seeds.setlists {
            setlist_repo.save_setlist(&setlist).await?;
        }
        Ok(SignalLive::new(
            Arc::new(block_repo),
            Arc::new(module_repo),
            Arc::new(layer_repo),
            Arc::new(engine_repo),
            Arc::new(rig_repo),
            Arc::new(profile_repo),
            Arc::new(song_repo),
            Arc::new(setlist_repo),
        ))
    }

    // region: --- get_block / set_block

    #[tokio::test]
    async fn test_live_get_block_returns_seeded_state() -> Result<()> {
        // -- Setup & Fixtures
        let svc = seeded_service().await?;
        let cx = test_context();

        // -- Exec
        let block = svc.get_block(&cx, BlockType::Amp).await;

        // -- Check
        assert!(!block.parameters().is_empty());
        Ok(())
    }

    #[tokio::test]
    async fn test_live_get_block_returns_default_for_empty_repo() -> Result<()> {
        // -- Setup & Fixtures
        let db = Database::connect("sqlite::memory:").await?;
        let block_repo = BlockRepoLive::new(db.clone());
        block_repo.init_schema().await?;
        let module_repo = ModuleRepoLive::new(db.clone());
        module_repo.init_schema().await?;
        let layer_repo = LayerRepoLive::new(db.clone());
        layer_repo.init_schema().await?;
        let engine_repo = EngineRepoLive::new(db.clone());
        engine_repo.init_schema().await?;
        let rig_repo = RigRepoLive::new(db.clone());
        rig_repo.init_schema().await?;
        let profile_repo = ProfileRepoLive::new(db.clone());
        profile_repo.init_schema().await?;
        let song_repo = SongRepoLive::new(db.clone());
        song_repo.init_schema().await?;
        let setlist_repo = SetlistRepoLive::new(db);
        setlist_repo.init_schema().await?;
        let svc = SignalLive::new(
            Arc::new(block_repo),
            Arc::new(module_repo),
            Arc::new(layer_repo),
            Arc::new(engine_repo),
            Arc::new(rig_repo),
            Arc::new(profile_repo),
            Arc::new(song_repo),
            Arc::new(setlist_repo),
        );
        let cx = test_context();

        // -- Exec
        let block = svc.get_block(&cx, BlockType::Amp).await;

        // -- Check
        assert_eq!(block, Block::default());
        Ok(())
    }

    #[tokio::test]
    async fn test_live_set_block_persists_and_returns() -> Result<()> {
        // -- Setup & Fixtures
        let svc = seeded_service().await?;
        let cx = test_context();
        let new_block = Block::new(0.1, 0.2, 0.3);

        // -- Exec
        let returned = svc
            .set_block(&cx, BlockType::Drive, new_block.clone())
            .await;

        // -- Check
        assert_eq!(returned, new_block);
        let loaded = svc.get_block(&cx, BlockType::Drive).await;
        assert_eq!(loaded, new_block);
        Ok(())
    }

    // endregion: --- get_block / set_block

    // region: --- Setlist operations

    #[tokio::test]
    async fn test_live_list_setlists_returns_demo_setlist() -> Result<()> {
        let svc = seeded_service().await?;
        let cx = test_context();

        let setlists = svc.list_setlists(&cx).await;

        assert_eq!(setlists.len(), 1);
        assert_eq!(setlists[0].name, "Demo Setlist");
        assert_eq!(setlists[0].entries.len(), 2);
        Ok(())
    }

    #[tokio::test]
    async fn test_live_load_setlist_entry_returns_dummy_song_entry() -> Result<()> {
        let svc = seeded_service().await?;
        let cx = test_context();

        let entry = svc
            .load_setlist_entry(
                &cx,
                signal_proto::setlist::SetlistId::from(seed_id("demo-setlist")),
                signal_proto::setlist::SetlistEntryId::from(seed_id("demo-setlist-dummy-song")),
            )
            .await;

        assert!(entry.is_some());
        let entry = entry.unwrap();
        assert_eq!(entry.name, "Dummy Song");
        assert_eq!(entry.song_id.as_str(), seed_id("dummy-song").to_string());
        Ok(())
    }

    // endregion: --- Setlist operations

    // region: --- Block collections (list / load)

    #[tokio::test]
    async fn test_live_list_collections_returns_seeded_presets() -> Result<()> {
        // -- Setup & Fixtures
        let svc = seeded_service().await?;
        let cx = test_context();

        // -- Exec
        let amp_collections = svc.list_presets(&cx, BlockType::Amp).await;
        let drive_collections = svc.list_presets(&cx, BlockType::Drive).await;

        // -- Check
        assert_eq!(amp_collections.len(), 5);
        assert_eq!(drive_collections.len(), 5);
        Ok(())
    }

    #[tokio::test]
    async fn test_live_list_collections_empty_repo() -> Result<()> {
        // -- Setup & Fixtures
        let db = Database::connect("sqlite::memory:").await?;
        let block_repo = BlockRepoLive::new(db.clone());
        block_repo.init_schema().await?;
        let module_repo = ModuleRepoLive::new(db.clone());
        module_repo.init_schema().await?;
        let layer_repo = LayerRepoLive::new(db.clone());
        layer_repo.init_schema().await?;
        let engine_repo = EngineRepoLive::new(db.clone());
        engine_repo.init_schema().await?;
        let rig_repo = RigRepoLive::new(db.clone());
        rig_repo.init_schema().await?;
        let profile_repo = ProfileRepoLive::new(db.clone());
        profile_repo.init_schema().await?;
        let song_repo = SongRepoLive::new(db.clone());
        song_repo.init_schema().await?;
        let setlist_repo = SetlistRepoLive::new(db);
        setlist_repo.init_schema().await?;
        let svc = SignalLive::new(
            Arc::new(block_repo),
            Arc::new(module_repo),
            Arc::new(layer_repo),
            Arc::new(engine_repo),
            Arc::new(rig_repo),
            Arc::new(profile_repo),
            Arc::new(song_repo),
            Arc::new(setlist_repo),
        );
        let cx = test_context();

        // -- Exec
        let collections = svc.list_presets(&cx, BlockType::Amp).await;

        // -- Check
        assert!(collections.is_empty());
        Ok(())
    }

    #[tokio::test]
    async fn test_live_load_default_variant_applies_block() -> Result<()> {
        // -- Setup & Fixtures
        let svc = seeded_service().await?;
        let cx = test_context();
        let preset_id = PresetId::from_uuid(seed_id("amp-twin"));

        // -- Exec: load the default variant (triggers side-effect)
        let snapshot = svc.load_preset(&cx, BlockType::Amp, preset_id).await;

        // -- Check: variant returned
        assert!(snapshot.is_some());
        let snapshot = snapshot.unwrap();
        assert_eq!(
            snapshot.id(),
            &SnapshotId::from_uuid(seed_id("amp-twin-default"))
        );

        // -- Check: current block was updated to match the loaded variant
        let current = svc.get_block(&cx, BlockType::Amp).await;
        assert_eq!(current, snapshot.block());
        Ok(())
    }

    #[tokio::test]
    async fn test_live_load_specific_variant_applies_block() -> Result<()> {
        // -- Setup & Fixtures
        let svc = seeded_service().await?;
        let cx = test_context();
        let preset_id = PresetId::from_uuid(seed_id("amp-twin"));
        let snapshot_id = SnapshotId::from_uuid(seed_id("amp-twin-surf"));

        // -- Exec
        let snapshot = svc
            .load_preset_snapshot(&cx, BlockType::Amp, preset_id, snapshot_id.clone())
            .await;

        // -- Check: correct variant returned
        assert!(snapshot.is_some());
        let snapshot = snapshot.unwrap();
        assert_eq!(snapshot.id(), &snapshot_id);

        // -- Check: current block updated
        let current = svc.get_block(&cx, BlockType::Amp).await;
        assert_eq!(current, snapshot.block());
        Ok(())
    }

    #[tokio::test]
    async fn test_live_load_nonexistent_collection_returns_none() -> Result<()> {
        // -- Setup & Fixtures
        let svc = seeded_service().await?;
        let cx = test_context();

        // -- Exec
        let result = svc.load_preset(&cx, BlockType::Amp, PresetId::new()).await;

        // -- Check
        assert!(result.is_none());
        Ok(())
    }

    #[tokio::test]
    async fn test_live_load_nonexistent_variant_returns_none() -> Result<()> {
        // -- Setup & Fixtures
        let svc = seeded_service().await?;
        let cx = test_context();

        // -- Exec
        let result = svc
            .load_preset_snapshot(
                &cx,
                BlockType::Amp,
                PresetId::from_uuid(seed_id("amp-twin")),
                SnapshotId::new(),
            )
            .await;

        // -- Check
        assert!(result.is_none());
        Ok(())
    }

    // endregion: --- Block collections (list / load)

    // region: --- Module collections (list / load)

    #[tokio::test]
    async fn test_live_list_module_collections() -> Result<()> {
        // -- Setup & Fixtures
        let svc = seeded_service().await?;
        let cx = test_context();

        // -- Exec
        let module_collections = svc.list_module_presets(&cx).await;

        // -- Check
        assert_eq!(module_collections.len(), 17);
        let mut names: Vec<&str> = module_collections.iter().map(|c| c.name()).collect();
        names.sort();
        assert!(names.contains(&"Drive Duo"));
        assert!(names.contains(&"Full Drive Stack"));
        assert!(names.contains(&"Parallel Time"));
        assert!(names.contains(&"Source"));
        assert!(names.contains(&"Rescue"));
        Ok(())
    }

    #[tokio::test]
    async fn test_live_load_module_default_variant() -> Result<()> {
        // -- Setup & Fixtures
        let svc = seeded_service().await?;
        let cx = test_context();
        let preset_id = ModulePresetId::from_uuid(seed_id("drive-full-stack"));

        // -- Exec
        let snapshot = svc.load_module_preset(&cx, preset_id).await;

        // -- Check
        assert!(snapshot.is_some());
        let snapshot = snapshot.unwrap();
        assert_eq!(
            snapshot.id(),
            &ModuleSnapshotId::from_uuid(seed_id("drive-full-stack-default"))
        );
        assert_eq!(snapshot.module().blocks().len(), 4);
        Ok(())
    }

    #[tokio::test]
    async fn test_live_load_module_specific_variant() -> Result<()> {
        // -- Setup & Fixtures
        let svc = seeded_service().await?;
        let cx = test_context();
        let preset_id = ModulePresetId::from_uuid(seed_id("drive-full-stack"));
        let snapshot_id = ModuleSnapshotId::from_uuid(seed_id("drive-full-stack-push"));

        // -- Exec
        let snapshot = svc
            .load_module_preset_snapshot(&cx, preset_id, snapshot_id.clone())
            .await;

        // -- Check
        assert!(snapshot.is_some());
        let snapshot = snapshot.unwrap();
        assert_eq!(snapshot.id(), &snapshot_id);
        assert_eq!(snapshot.name(), "Push");
        Ok(())
    }

    #[tokio::test]
    async fn test_live_load_nonexistent_module_collection() -> Result<()> {
        // -- Setup & Fixtures
        let svc = seeded_service().await?;
        let cx = test_context();

        // -- Exec
        let result = svc.load_module_preset(&cx, ModulePresetId::new()).await;

        // -- Check
        assert!(result.is_none());
        Ok(())
    }

    // endregion: --- Module collections (list / load)

    // region: --- Resolver determinism

    #[tokio::test]
    async fn test_live_load_variant_then_different_variant_updates_block() -> Result<()> {
        // -- Setup & Fixtures
        let svc = seeded_service().await?;
        let cx = test_context();

        // -- Exec: load "surf" variant
        let surf = svc
            .load_preset_snapshot(
                &cx,
                BlockType::Amp,
                PresetId::from_uuid(seed_id("amp-twin")),
                SnapshotId::from_uuid(seed_id("amp-twin-surf")),
            )
            .await
            .unwrap();

        let block_after_surf = svc.get_block(&cx, BlockType::Amp).await;
        assert_eq!(block_after_surf, surf.block());

        // -- Exec: load "jazz" variant (should overwrite)
        let jazz = svc
            .load_preset_snapshot(
                &cx,
                BlockType::Amp,
                PresetId::from_uuid(seed_id("amp-twin")),
                SnapshotId::from_uuid(seed_id("amp-twin-jazz")),
            )
            .await
            .unwrap();

        // -- Check: current block reflects the most recently loaded variant
        let block_after_jazz = svc.get_block(&cx, BlockType::Amp).await;
        assert_eq!(block_after_jazz, jazz.block());
        assert_ne!(block_after_jazz, surf.block());
        Ok(())
    }

    #[tokio::test]
    async fn test_live_cross_collection_load_updates_correct_block_type() -> Result<()> {
        // -- Setup & Fixtures
        let svc = seeded_service().await?;
        let cx = test_context();

        // -- Exec: load an amp variant
        let amp_before = svc.get_block(&cx, BlockType::Amp).await;
        let _drive = svc
            .load_preset(
                &cx,
                BlockType::Drive,
                PresetId::from_uuid(seed_id("drive-level")),
            )
            .await;

        // -- Check: amp block was not affected by loading a drive variant
        let amp_after = svc.get_block(&cx, BlockType::Amp).await;
        assert_eq!(amp_before, amp_after);
        Ok(())
    }

    // endregion: --- Resolver determinism

    // region: --- Layer service

    #[tokio::test]
    async fn test_live_list_layers_returns_seeded() -> Result<()> {
        let svc = seeded_service().await?;
        let cx = test_context();

        let layers = svc.list_layers(&cx).await;
        assert_eq!(layers.len(), 11);
        assert!(layers.iter().any(|l| l.name == "Keys Core"));
        assert!(layers.iter().any(|l| l.name == "Guitar Main"));
        assert!(layers.iter().any(|l| l.name == "Vocal Main"));
        Ok(())
    }

    #[tokio::test]
    async fn test_live_load_layer_by_id() -> Result<()> {
        let svc = seeded_service().await?;
        let cx = test_context();

        let layer = svc
            .load_layer(&cx, LayerId::from_uuid(seed_id("keys-layer-core")))
            .await;
        assert!(layer.is_some());
        let layer = layer.unwrap();
        assert_eq!(layer.variants.len(), 2);
        Ok(())
    }

    #[tokio::test]
    async fn test_live_load_layer_missing_returns_none() -> Result<()> {
        let svc = seeded_service().await?;
        let cx = test_context();

        let layer = svc.load_layer(&cx, LayerId::new()).await;
        assert!(layer.is_none());
        Ok(())
    }

    #[tokio::test]
    async fn test_live_save_and_delete_layer() -> Result<()> {
        let svc = seeded_service().await?;
        let cx = test_context();

        let variant = LayerSnapshot::new(seed_id("test-v1"), "Test Default");
        let layer = Layer::new(seed_id("test-layer"), "Test Layer", signal_proto::EngineType::Guitar, variant);
        svc.save_layer(&cx, layer).await;

        let loaded = svc
            .load_layer(&cx, LayerId::from_uuid(seed_id("test-layer")))
            .await;
        assert!(loaded.is_some());

        svc.delete_layer(&cx, LayerId::from_uuid(seed_id("test-layer")))
            .await;
        let after_delete = svc
            .load_layer(&cx, LayerId::from_uuid(seed_id("test-layer")))
            .await;
        assert!(after_delete.is_none());
        Ok(())
    }

    #[tokio::test]
    async fn test_live_load_layer_variant() -> Result<()> {
        let svc = seeded_service().await?;
        let cx = test_context();

        let variant = svc
            .load_layer_variant(
                &cx,
                LayerId::from_uuid(seed_id("synth-layer-osc")),
                LayerSnapshotId::from_uuid(seed_id("synth-layer-osc-alt")),
            )
            .await;
        assert!(variant.is_some());
        let variant = variant.unwrap();
        assert_eq!(variant.name, "Alt");
        assert_eq!(variant.block_refs.len(), 3);
        Ok(())
    }

    // endregion: --- Layer service

    // region: --- Engine service

    #[tokio::test]
    async fn test_live_list_engines_seeded() -> Result<()> {
        let svc = seeded_service().await?;
        let cx = test_context();

        let engines = svc.list_engines(&cx).await;
        assert_eq!(engines.len(), 6);
        let synth = engines
            .iter()
            .find(|e| e.name == "Synth Engine")
            .expect("expected seeded synth engine");
        assert_eq!(synth.variants.len(), 2);
        Ok(())
    }

    #[tokio::test]
    async fn test_live_save_load_delete_engine() -> Result<()> {
        use signal_proto::engine::{EngineScene, LayerSelection};

        let svc = seeded_service().await?;
        let cx = test_context();

        let scene = EngineScene::new(seed_id("scene-1"), "Default Scene").with_layer(
            LayerSelection::new(seed_id("keys-layer-core"), seed_id("keys-layer-core-default")),
        );
        let engine = Engine::new(
            seed_id("engine-1"),
            "Keys Engine Test",
            signal_proto::EngineType::Keys,
            vec![LayerId::from_uuid(seed_id("keys-layer-core"))],
            scene,
        );

        svc.save_engine(&cx, engine).await;

        let loaded = svc
            .load_engine(&cx, EngineId::from_uuid(seed_id("engine-1")))
            .await;
        assert!(loaded.is_some());
        let loaded = loaded.unwrap();
        assert_eq!(loaded.name, "Keys Engine Test");
        assert_eq!(loaded.layer_ids.len(), 1);
        assert_eq!(loaded.variants.len(), 1);

        let engines = svc.list_engines(&cx).await;
        assert_eq!(engines.len(), 7); // 6 seeded + 1 just saved

        svc.delete_engine(&cx, EngineId::from_uuid(seed_id("engine-1")))
            .await;
        let after_delete = svc
            .load_engine(&cx, EngineId::from_uuid(seed_id("engine-1")))
            .await;
        assert!(after_delete.is_none());
        Ok(())
    }

    #[tokio::test]
    async fn test_live_load_engine_variant() -> Result<()> {
        use signal_proto::engine::{EngineScene, LayerSelection};

        let svc = seeded_service().await?;
        let cx = test_context();

        let scene = EngineScene::new(seed_id("scene-clean"), "Clean").with_layer(
            LayerSelection::new(seed_id("keys-layer-core"), seed_id("keys-layer-core-default")),
        );
        let mut engine = Engine::new(
            seed_id("engine-2"),
            "Keys Engine 2",
            signal_proto::EngineType::Keys,
            vec![LayerId::from_uuid(seed_id("keys-layer-core"))],
            scene,
        );
        engine.add_variant(
            EngineScene::new(seed_id("scene-heavy"), "Heavy").with_layer(LayerSelection::new(
                seed_id("keys-layer-core"),
                seed_id("keys-layer-core-bright"),
            )),
        );
        svc.save_engine(&cx, engine).await;

        let variant = svc
            .load_engine_variant(
                &cx,
                EngineId::from_uuid(seed_id("engine-2")),
                EngineSceneId::from_uuid(seed_id("scene-heavy")),
            )
            .await;
        assert!(variant.is_some());
        let variant = variant.unwrap();
        assert_eq!(variant.name, "Heavy");
        assert_eq!(variant.layer_selections.len(), 1);
        assert_eq!(
            variant.layer_selections[0].variant_id,
            LayerSnapshotId::from_uuid(seed_id("keys-layer-core-bright"))
        );
        Ok(())
    }

    // endregion: --- Engine service

    // region: --- Preset (rig) service

    #[tokio::test]
    async fn test_live_list_presets_all_seeded() -> Result<()> {
        let svc = seeded_service().await?;
        let cx = test_context();

        let rigs = svc.list_presets_all(&cx).await;
        assert_eq!(rigs.len(), 3);
        assert!(rigs.iter().all(|r| r.name == "MegaRig"));
        let keys_rig = rigs
            .iter()
            .find(|r| r.rig_type.unwrap().as_str() == "keys")
            .expect("expected seeded keys megarig");
        assert_eq!(keys_rig.variants.len(), 4);
        Ok(())
    }

    #[tokio::test]
    async fn test_live_save_load_delete_preset() -> Result<()> {
        use signal_proto::engine::EngineId;
        use signal_proto::rig::{EngineSelection, RigScene};

        let svc = seeded_service().await?;
        let cx = test_context();

        let scene = RigScene::new(seed_id("rs-default"), "Default Scene").with_engine(
            EngineSelection::new(seed_id("engine-1"), seed_id("scene-1")),
        );
        let rig = Rig::new(
            seed_id("rig-1"),
            "Guitar Rig",
            vec![EngineId::from_uuid(seed_id("engine-1"))],
            scene,
        )
        .with_rig_type("guitar");

        svc.save_preset(&cx, rig).await;

        let loaded = svc
            .load_preset_rig(&cx, RigId::from_uuid(seed_id("rig-1")))
            .await;
        assert!(loaded.is_some());
        let loaded = loaded.unwrap();
        assert_eq!(loaded.name, "Guitar Rig");
        assert_eq!(loaded.engine_ids.len(), 1);
        assert_eq!(loaded.variants.len(), 1);
        assert_eq!(loaded.rig_type.unwrap().as_str(), "guitar");

        let rigs = svc.list_presets_all(&cx).await;
        assert_eq!(rigs.len(), 4); // 3 seeded + 1 just saved

        svc.delete_preset(&cx, RigId::from_uuid(seed_id("rig-1")))
            .await;
        let after_delete = svc
            .load_preset_rig(&cx, RigId::from_uuid(seed_id("rig-1")))
            .await;
        assert!(after_delete.is_none());
        Ok(())
    }

    #[tokio::test]
    async fn test_live_load_preset_variant() -> Result<()> {
        use signal_proto::engine::EngineId;
        use signal_proto::rig::{EngineSelection, RigScene};

        let svc = seeded_service().await?;
        let cx = test_context();

        let scene1 = RigScene::new(seed_id("rs-clean"), "Clean").with_engine(EngineSelection::new(
            seed_id("engine-1"),
            seed_id("scene-clean"),
        ));
        let mut rig = Rig::new(
            seed_id("rig-2"),
            "Guitar Rig 2",
            vec![EngineId::from_uuid(seed_id("engine-1"))],
            scene1,
        );
        rig.add_variant(RigScene::new(seed_id("rs-heavy"), "Heavy").with_engine(
            EngineSelection::new(seed_id("engine-1"), seed_id("scene-heavy")),
        ));
        svc.save_preset(&cx, rig).await;

        let variant = svc
            .load_preset_variant(
                &cx,
                RigId::from_uuid(seed_id("rig-2")),
                RigSceneId::from_uuid(seed_id("rs-heavy")),
            )
            .await;
        assert!(variant.is_some());
        let variant = variant.unwrap();
        assert_eq!(variant.name, "Heavy");
        assert_eq!(variant.engine_selections.len(), 1);
        Ok(())
    }

    // endregion: --- Preset (rig) service

    // region: --- Profile service

    #[tokio::test]
    async fn test_live_list_profiles_seeded() -> Result<()> {
        let svc = seeded_service().await?;
        let cx = test_context();

        let profiles = svc.list_profiles(&cx).await;
        assert_eq!(profiles.len(), 1);
        assert_eq!(profiles[0].name, "Keys Feature");
        assert_eq!(profiles[0].patches.len(), 4);
        Ok(())
    }

    #[tokio::test]
    async fn test_live_save_load_delete_profile() -> Result<()> {
        use signal_proto::profile::Patch;

        let svc = seeded_service().await?;
        let cx = test_context();

        let patch = Patch::new(
            seed_id("p-clean"),
            "Clean",
            seed_id("rig-1"),
            seed_id("rs-clean"),
        );
        let mut profile = Profile::new(seed_id("profile-1"), "Worship", patch);
        profile.add_patch(Patch::new(
            seed_id("p-lead"),
            "Lead",
            seed_id("rig-1"),
            seed_id("rs-lead"),
        ));

        svc.save_profile(&cx, profile).await;

        let loaded = svc
            .load_profile(&cx, ProfileId::from_uuid(seed_id("profile-1")))
            .await;
        assert!(loaded.is_some());
        let loaded = loaded.unwrap();
        assert_eq!(loaded.name, "Worship");
        assert_eq!(loaded.patches.len(), 2);

        let profiles = svc.list_profiles(&cx).await;
        assert_eq!(profiles.len(), 2); // 1 seeded + 1 just saved

        svc.delete_profile(&cx, ProfileId::from_uuid(seed_id("profile-1")))
            .await;
        let after_delete = svc
            .load_profile(&cx, ProfileId::from_uuid(seed_id("profile-1")))
            .await;
        assert!(after_delete.is_none());
        Ok(())
    }

    #[tokio::test]
    async fn test_live_load_profile_variant() -> Result<()> {
        use signal_proto::profile::Patch;

        let svc = seeded_service().await?;
        let cx = test_context();

        let patch1 = Patch::new(
            seed_id("p-clean"),
            "Clean",
            seed_id("rig-1"),
            seed_id("rs-clean"),
        );
        let mut profile = Profile::new(seed_id("profile-2"), "Blues", patch1);
        profile.add_patch(Patch::new(
            seed_id("p-crunch"),
            "Crunch",
            seed_id("rig-1"),
            seed_id("rs-crunch"),
        ));
        svc.save_profile(&cx, profile).await;

        let variant = svc
            .load_profile_variant(
                &cx,
                ProfileId::from_uuid(seed_id("profile-2")),
                PatchId::from_uuid(seed_id("p-crunch")),
            )
            .await;
        assert!(variant.is_some());
        let variant = variant.unwrap();
        assert_eq!(variant.name, "Crunch");
        assert_eq!(variant.rig_id, RigId::from_uuid(seed_id("rig-1")));
        assert_eq!(
            variant.rig_variant_id,
            RigSceneId::from_uuid(seed_id("rs-crunch"))
        );
        Ok(())
    }

    // endregion: --- Profile service

    // region: --- Song service

    #[tokio::test]
    async fn test_live_list_songs_seeded() -> Result<()> {
        let svc = seeded_service().await?;
        let cx = test_context();

        let songs = svc.list_songs(&cx).await;
        assert_eq!(songs.len(), 2);
        let feature = songs
            .iter()
            .find(|s| s.name == "Feature-Demo Song")
            .expect("feature song exists");
        assert_eq!(feature.sections.len(), 4);
        assert_eq!(feature.artist.as_deref(), Some("Signal2"));
        assert!(songs.iter().any(|s| s.name == "Dummy Song"));
        Ok(())
    }

    #[tokio::test]
    async fn test_live_save_load_delete_song() -> Result<()> {
        use signal_proto::song::Section;

        let svc = seeded_service().await?;
        let cx = test_context();

        let verse = Section::from_patch(seed_id("sec-verse"), "Verse", seed_id("patch-clean"));
        let chorus = Section::from_patch(seed_id("sec-chorus"), "Chorus", seed_id("patch-lead"));
        let mut song =
            Song::new(seed_id("song-1"), "Amazing Grace", verse).with_artist("Traditional");
        song.add_section(chorus);

        svc.save_song(&cx, song).await;

        let loaded = svc
            .load_song(&cx, SongId::from_uuid(seed_id("song-1")))
            .await;
        assert!(loaded.is_some());
        let loaded = loaded.unwrap();
        assert_eq!(loaded.name, "Amazing Grace");
        assert_eq!(loaded.artist.as_deref(), Some("Traditional"));
        assert_eq!(loaded.sections.len(), 2);

        let songs = svc.list_songs(&cx).await;
        assert_eq!(songs.len(), 3); // 2 seeded + 1 just saved

        svc.delete_song(&cx, SongId::from_uuid(seed_id("song-1")))
            .await;
        let after_delete = svc
            .load_song(&cx, SongId::from_uuid(seed_id("song-1")))
            .await;
        assert!(after_delete.is_none());
        Ok(())
    }

    #[tokio::test]
    async fn test_live_load_song_variant() -> Result<()> {
        use signal_proto::song::{Section, SectionSource};

        let svc = seeded_service().await?;
        let cx = test_context();

        let verse = Section::from_patch(seed_id("sec-verse"), "Verse", seed_id("patch-clean"));
        let bridge = Section::from_rig_scene(
            seed_id("sec-bridge"),
            "Bridge",
            seed_id("rig-1"),
            seed_id("rs-ambient"),
        );
        let mut song = Song::new(seed_id("song-2"), "Instrumental", verse);
        song.add_section(bridge);
        svc.save_song(&cx, song).await;

        let variant = svc
            .load_song_variant(
                &cx,
                SongId::from_uuid(seed_id("song-2")),
                SectionId::from_uuid(seed_id("sec-bridge")),
            )
            .await;
        assert!(variant.is_some());
        let variant = variant.unwrap();
        assert_eq!(variant.name, "Bridge");
        match &variant.source {
            SectionSource::RigScene { rig_id, scene_id } => {
                assert_eq!(*rig_id, RigId::from_uuid(seed_id("rig-1")));
                assert_eq!(*scene_id, RigSceneId::from_uuid(seed_id("rs-ambient")));
            }
            _ => panic!("expected RigScene source"),
        }
        Ok(())
    }

    // endregion: --- Song service

    // region: --- Browser service

    #[tokio::test]
    async fn test_live_browser_index_and_query() -> Result<()> {
        let svc = seeded_service().await?;
        let cx = test_context();

        let index: BrowserIndex = svc.browser_index(&cx).await;
        assert!(!index.entries().is_empty());
        assert!(index
            .entries()
            .iter()
            .any(|e| matches!(e.node.kind, BrowserEntityKind::SetlistCollection)));
        assert!(index
            .entries()
            .iter()
            .any(|e| matches!(e.node.kind, BrowserEntityKind::SetlistVariant)));

        let hits: Vec<BrowserHit> = svc
            .browse(
                &cx,
                BrowserQuery {
                    include: vec!["tone:clean".to_string()],
                    ..BrowserQuery::default()
                },
            )
            .await;
        assert!(!hits.is_empty());
        Ok(())
    }

    #[tokio::test]
    async fn test_live_browser_query_strict_filters() -> Result<()> {
        let svc = seeded_service().await?;
        let cx = test_context();

        let setlist_hits: Vec<BrowserHit> = svc
            .browse(
                &cx,
                BrowserQuery {
                    kinds: vec![BrowserEntityKind::SetlistCollection],
                    text: Some("demo".to_string()),
                    ..BrowserQuery::default()
                },
            )
            .await;
        assert_eq!(setlist_hits.len(), 1);
        assert!(matches!(
            setlist_hits[0].node.kind,
            BrowserEntityKind::SetlistCollection
        ));

        let strict_keys_hits: Vec<BrowserHit> = svc
            .browse(
                &cx,
                BrowserQuery {
                    rig_type: Some(signal_proto::rig::RigType::Keys),
                    strict_rig_type: true,
                    ..BrowserQuery::default()
                },
            )
            .await;
        assert!(!strict_keys_hits.is_empty());
        Ok(())
    }

    // endregion: --- Browser service

    // region: --- Resolver service

    #[tokio::test]
    async fn test_live_resolve_rig_scene_keys_megarig() -> Result<()> {
        let svc = seeded_service().await?;
        let cx = test_context();

        let graph: ResolvedGraph = svc
            .resolve_target(
                &cx,
                ResolveTarget::RigScene {
                    rig_id: RigId::from_uuid(seed_id("keys-megarig")),
                    scene_id: RigSceneId::from_uuid(seed_id("keys-megarig-default")),
                },
            )
            .await
            .expect("resolve rig scene");

        assert_eq!(graph.rig_id.as_str(), seed_id("keys-megarig").to_string());
        assert!(!graph.engines.is_empty());
        assert!(!graph.effective_overrides.is_empty());
        assert!(graph
            .effective_overrides
            .iter()
            .any(|ov| matches!(ov.op, signal_proto::overrides::NodeOverrideOp::Set(_))));
        Ok(())
    }

    #[tokio::test]
    async fn test_live_resolve_song_section_from_patch() -> Result<()> {
        let svc = seeded_service().await?;
        let cx = test_context();

        let graph: ResolvedGraph = svc
            .resolve_target(
                &cx,
                ResolveTarget::SongSection {
                    song_id: SongId::from_uuid(seed_id("feature-demo-song")),
                    section_id: SectionId::from_uuid(seed_id("feature-demo-verse")),
                },
            )
            .await
            .expect("resolve song section");

        assert_eq!(graph.rig_id.as_str(), seed_id("keys-megarig").to_string());
        assert!(!graph.engines.is_empty());
        assert!(!graph.effective_overrides.is_empty());
        Ok(())
    }

    #[tokio::test]
    async fn test_live_resolve_applies_replace_ref_engine_scene() -> Result<()> {
        let svc = seeded_service().await?;
        let cx = test_context();

        let graph: ResolvedGraph = svc
            .resolve_target(
                &cx,
                ResolveTarget::SongSection {
                    song_id: SongId::from_uuid(seed_id("feature-demo-song")),
                    section_id: SectionId::from_uuid(seed_id("feature-demo-intro")),
                },
            )
            .await
            .expect("resolve song intro");

        let synth_engine = graph
            .engines
            .iter()
            .find(|e| e.engine_id.as_str() == seed_id("synth-engine").to_string())
            .expect("synth engine present");
        assert_eq!(
            synth_engine.engine_scene_id.as_str(),
            seed_id("synth-engine-scene-b").to_string()
        );
        Ok(())
    }

    #[tokio::test]
    async fn test_live_resolve_fails_on_missing_replace_ref_module_variant() -> Result<()> {
        use signal_proto::overrides::{NodeOverrideOp, NodePath, Override};
        use signal_proto::song::Section;

        let svc = seeded_service().await?;
        let cx = test_context();

        let bad = Section::from_rig_scene(
            seed_id("bad-replace-ref-section"),
            "Bad ReplaceRef",
            seed_id("keys-megarig"),
            seed_id("keys-megarig-default"),
        )
        .with_override(Override {
            path: NodePath::engine("synth-engine")
                .with_layer("synth-layer-motion")
                .with_module("time-parallel"),
            op: NodeOverrideOp::ReplaceRef("does-not-exist".to_string()),
        });

        let song = Song::new(seed_id("bad-replace-ref-song"), "Bad ReplaceRef Song", bad);
        svc.save_song(&cx, song).await;

        let resolved: core::result::Result<ResolvedGraph, ResolveError> = svc
            .resolve_target(
                &cx,
                ResolveTarget::SongSection {
                    song_id: SongId::from_uuid(seed_id("bad-replace-ref-song")),
                    section_id: SectionId::from_uuid(seed_id("bad-replace-ref-section")),
                },
            )
            .await;

        assert!(resolved.is_err());
        let err = resolved.err().expect("expected resolve error");
        assert!(matches!(err, ResolveError::InvalidReference(_)));
        Ok(())
    }

    #[tokio::test]
    async fn test_live_resolve_fails_on_missing_replace_ref_engine_scene() -> Result<()> {
        use signal_proto::overrides::{NodeOverrideOp, NodePath, Override};
        use signal_proto::song::Section;

        let svc = seeded_service().await?;
        let cx = test_context();

        let bad = Section::from_rig_scene(
            seed_id("bad-replace-ref-engine-section"),
            "Bad Engine ReplaceRef",
            seed_id("keys-megarig"),
            seed_id("keys-megarig-default"),
        )
        .with_override(Override {
            path: NodePath::engine("synth-engine"),
            op: NodeOverrideOp::ReplaceRef("does-not-exist-engine-scene".to_string()),
        });

        let song = Song::new(
            seed_id("bad-replace-ref-engine-song"),
            "Bad Engine ReplaceRef Song",
            bad,
        );
        svc.save_song(&cx, song).await;

        let resolved: core::result::Result<ResolvedGraph, ResolveError> = svc
            .resolve_target(
                &cx,
                ResolveTarget::SongSection {
                    song_id: SongId::from_uuid(seed_id("bad-replace-ref-engine-song")),
                    section_id: SectionId::from_uuid(seed_id("bad-replace-ref-engine-section")),
                },
            )
            .await;

        assert!(resolved.is_err());
        let err = resolved.err().expect("expected resolve error");
        assert!(matches!(err, ResolveError::InvalidReference(_)));
        Ok(())
    }

    #[tokio::test]
    async fn test_live_resolve_fails_on_missing_replace_ref_layer_variant() -> Result<()> {
        use signal_proto::overrides::{NodeOverrideOp, NodePath, Override};
        use signal_proto::song::Section;

        let svc = seeded_service().await?;
        let cx = test_context();

        let bad = Section::from_rig_scene(
            seed_id("bad-replace-ref-layer-section"),
            "Bad Layer ReplaceRef",
            seed_id("keys-megarig"),
            seed_id("keys-megarig-default"),
        )
        .with_override(Override {
            path: NodePath::engine("synth-engine").with_layer("synth-layer-motion"),
            op: NodeOverrideOp::ReplaceRef("does-not-exist-layer-variant".to_string()),
        });

        let song = Song::new(
            seed_id("bad-replace-ref-layer-song"),
            "Bad Layer ReplaceRef Song",
            bad,
        );
        svc.save_song(&cx, song).await;

        let resolved: core::result::Result<ResolvedGraph, ResolveError> = svc
            .resolve_target(
                &cx,
                ResolveTarget::SongSection {
                    song_id: SongId::from_uuid(seed_id("bad-replace-ref-layer-song")),
                    section_id: SectionId::from_uuid(seed_id("bad-replace-ref-layer-section")),
                },
            )
            .await;

        assert!(resolved.is_err());
        let err = resolved.err().expect("expected resolve error");
        assert!(matches!(err, ResolveError::InvalidReference(_)));
        Ok(())
    }

    // endregion: --- Resolver service
}
