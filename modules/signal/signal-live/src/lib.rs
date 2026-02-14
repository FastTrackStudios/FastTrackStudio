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
    rig::{Rig, RigId, RigScene, RigSceneId},
    song::{Section, SectionId, Song, SongId},
    tagging::{
        infer_tags_from_name, BrowserEntityKind, BrowserEntry, BrowserHit, BrowserIndex,
        BrowserNodeId, BrowserQuery, StructuredTag, TagCategory, TagSet, TagWeights,
    },
    Block, BlockService, BlockType, BrowserService, EngineService, LayerService, ModulePreset,
    ModulePresetId, ModuleSnapshot, ModuleSnapshotId, Preset, PresetId, PresetService,
    ProfileService, Snapshot, SnapshotId, SongService, ALL_BLOCK_TYPES,
};
use signal_storage::{
    BlockRepo, BlockRepoLive, DatabaseConnection, EngineRepo, EngineRepoLive, LayerRepo,
    LayerRepoLive, ModuleRepo, ModuleRepoLive, ProfileRepo, ProfileRepoLive, RigRepo, RigRepoLive,
    SongRepo, SongRepoLive,
};
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
> where
    B: BlockRepo,
    M: ModuleRepo,
    L: LayerRepo,
    E: EngineRepo,
    R: RigRepo,
    P: ProfileRepo,
    So: SongRepo,
{
    block_repo: Arc<B>,
    module_repo: Arc<M>,
    layer_repo: Arc<L>,
    engine_repo: Arc<E>,
    rig_repo: Arc<R>,
    profile_repo: Arc<P>,
    song_repo: Arc<So>,
}

impl<B, M, L, E, R, P, So> SignalLive<B, M, L, E, R, P, So>
where
    B: BlockRepo,
    M: ModuleRepo,
    L: LayerRepo,
    E: EngineRepo,
    R: RigRepo,
    P: ProfileRepo,
    So: SongRepo,
{
    pub fn new(
        block_repo: Arc<B>,
        module_repo: Arc<M>,
        layer_repo: Arc<L>,
        engine_repo: Arc<E>,
        rig_repo: Arc<R>,
        profile_repo: Arc<P>,
        song_repo: Arc<So>,
    ) -> Self {
        Self {
            block_repo,
            module_repo,
            layer_repo,
            engine_repo,
            rig_repo,
            profile_repo,
            song_repo,
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
            Arc::new(SongRepoLive::new(db)),
        )
    }
}

// endregion: --- SignalLive

// region: --- BlockService impl

impl<B, M, L, E, R, P, So> BlockService for SignalLive<B, M, L, E, R, P, So>
where
    B: BlockRepo,
    M: ModuleRepo,
    L: LayerRepo,
    E: EngineRepo,
    R: RigRepo,
    P: ProfileRepo,
    So: SongRepo,
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

impl<B, M, L, E, R, P, So> LayerService for SignalLive<B, M, L, E, R, P, So>
where
    B: BlockRepo,
    M: ModuleRepo,
    L: LayerRepo,
    E: EngineRepo,
    R: RigRepo,
    P: ProfileRepo,
    So: SongRepo,
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

impl<B, M, L, E, R, P, So> EngineService for SignalLive<B, M, L, E, R, P, So>
where
    B: BlockRepo,
    M: ModuleRepo,
    L: LayerRepo,
    E: EngineRepo,
    R: RigRepo,
    P: ProfileRepo,
    So: SongRepo,
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

impl<B, M, L, E, R, P, So> PresetService for SignalLive<B, M, L, E, R, P, So>
where
    B: BlockRepo,
    M: ModuleRepo,
    L: LayerRepo,
    E: EngineRepo,
    R: RigRepo,
    P: ProfileRepo,
    So: SongRepo,
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

impl<B, M, L, E, R, P, So> ProfileService for SignalLive<B, M, L, E, R, P, So>
where
    B: BlockRepo,
    M: ModuleRepo,
    L: LayerRepo,
    E: EngineRepo,
    R: RigRepo,
    P: ProfileRepo,
    So: SongRepo,
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

impl<B, M, L, E, R, P, So> SongService for SignalLive<B, M, L, E, R, P, So>
where
    B: BlockRepo,
    M: ModuleRepo,
    L: LayerRepo,
    E: EngineRepo,
    R: RigRepo,
    P: ProfileRepo,
    So: SongRepo,
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

impl<B, M, L, E, R, P, So> BrowserService for SignalLive<B, M, L, E, R, P, So>
where
    B: BlockRepo,
    M: ModuleRepo,
    L: LayerRepo,
    E: EngineRepo,
    R: RigRepo,
    P: ProfileRepo,
    So: SongRepo,
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

        index
    }

    async fn browse(&self, cx: &Context, query: BrowserQuery) -> Vec<BrowserHit> {
        let index: BrowserIndex = BrowserService::browser_index(self, cx).await;
        index.query(&query, &TagWeights::default())
    }
}

// endregion: --- BrowserService impl

#[cfg(test)]
mod tests {
    use super::*;
    use signal_proto::seed_id;
    use signal_storage::{
        runtime_seed_bundle, BlockRepoLive, Database, EngineRepoLive, LayerRepoLive,
        ModuleRepoLive, ProfileRepoLive, RigRepoLive, SongRepoLive,
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
        let song_repo = SongRepoLive::new(db);
        song_repo.init_schema().await?;
        for song in seeds.songs {
            song_repo.save_song(&song).await?;
        }
        Ok(SignalLive::new(
            Arc::new(block_repo),
            Arc::new(module_repo),
            Arc::new(layer_repo),
            Arc::new(engine_repo),
            Arc::new(rig_repo),
            Arc::new(profile_repo),
            Arc::new(song_repo),
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
        let song_repo = SongRepoLive::new(db);
        song_repo.init_schema().await?;
        let svc = SignalLive::new(
            Arc::new(block_repo),
            Arc::new(module_repo),
            Arc::new(layer_repo),
            Arc::new(engine_repo),
            Arc::new(rig_repo),
            Arc::new(profile_repo),
            Arc::new(song_repo),
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
        let song_repo = SongRepoLive::new(db);
        song_repo.init_schema().await?;
        let svc = SignalLive::new(
            Arc::new(block_repo),
            Arc::new(module_repo),
            Arc::new(layer_repo),
            Arc::new(engine_repo),
            Arc::new(rig_repo),
            Arc::new(profile_repo),
            Arc::new(song_repo),
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
        assert_eq!(songs.len(), 1);
        assert_eq!(songs[0].name, "Feature-Demo Song");
        assert_eq!(songs[0].sections.len(), 4);
        assert_eq!(songs[0].artist.as_deref(), Some("Signal2"));
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
        assert_eq!(songs.len(), 2); // 1 seeded + 1 just saved

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

    // endregion: --- Browser service
}
