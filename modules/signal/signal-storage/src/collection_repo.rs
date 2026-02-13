//! Collection repository — data access layer for block & module collections (SeaORM).
//!
//! Collections map to the underlying `Preset` / `ModulePreset` types and
//! Variants map to `Snapshot` / `ModuleSnapshot`. This module provides a
//! unified surface that exposes collection/variant semantics without leaking
//! service-level business logic into the repo.

use sea_orm::*;
use sea_orm::{ConnectionTrait, Schema};
use signal_proto::{
    Block, BlockParameter, BlockParameterOverride, BlockType, Module, ModuleBlock,
    ModuleBlockSource, ModulePreset, ModulePresetId, ModuleSnapshot, ModuleSnapshotId, Preset,
    PresetId, Snapshot, SnapshotId,
};

use crate::entity;
use crate::{Database, DatabaseConnection, StorageError, StorageResult};

// region: --- Trait

/// Data-access trait for collection/variant storage.
///
/// **Collections** are named groups of variants scoped to a domain concept
/// (e.g. "Clean Amp", "Triple Drive Stack").
///
/// **Variants** are individual state snapshots within a collection
/// (e.g. "Bright", "Warm", "Push").
///
/// Block collections are scoped by [`BlockType`]; module collections are global.
#[async_trait::async_trait]
pub trait CollectionRepo: Send + Sync {
    // -- Block state (current live value per BlockType)
    async fn load_block_state(&self, block_type: BlockType) -> StorageResult<Option<Block>>;
    async fn save_block_state(&self, block_type: BlockType, block: Block) -> StorageResult<()>;

    // -- Block collections & variants
    async fn list_block_collections(&self, block_type: BlockType) -> StorageResult<Vec<Preset>>;
    async fn load_block_default_variant(
        &self,
        block_type: BlockType,
        collection_id: &PresetId,
    ) -> StorageResult<Option<Snapshot>>;
    async fn load_block_variant(
        &self,
        block_type: BlockType,
        collection_id: &PresetId,
        variant_id: &SnapshotId,
    ) -> StorageResult<Option<Snapshot>>;

    // -- Module collections & variants
    async fn list_module_collections(&self) -> StorageResult<Vec<ModulePreset>>;
    async fn load_module_default_variant(
        &self,
        collection_id: &ModulePresetId,
    ) -> StorageResult<Option<ModuleSnapshot>>;
    async fn load_module_variant(
        &self,
        collection_id: &ModulePresetId,
        variant_id: &ModuleSnapshotId,
    ) -> StorageResult<Option<ModuleSnapshot>>;
}

// endregion: --- Trait

// region: --- CollectionRepoLive

#[derive(Clone)]
pub struct CollectionRepoLive {
    db: DatabaseConnection,
}

impl CollectionRepoLive {
    pub fn new(db: DatabaseConnection) -> Self {
        Self { db }
    }

    pub async fn connect_sqlite(url: &str) -> StorageResult<Self> {
        let db = Database::connect(url).await?;
        Ok(Self::new(db))
    }

    pub async fn connect_sqlite_in_memory() -> StorageResult<Self> {
        Self::connect_sqlite("sqlite::memory:").await
    }

    pub async fn init_schema(&self) -> StorageResult<()> {
        let backend = self.db.get_database_backend();
        let schema = Schema::new(backend);

        let mut presets = schema.create_table_from_entity(entity::preset::Entity);
        presets.if_not_exists();
        self.db.execute(backend.build(&presets)).await?;

        let mut snapshots = schema.create_table_from_entity(entity::snapshot::Entity);
        snapshots.if_not_exists();
        self.db.execute(backend.build(&snapshots)).await?;

        let mut module_presets = schema.create_table_from_entity(entity::module_preset::Entity);
        module_presets.if_not_exists();
        self.db.execute(backend.build(&module_presets)).await?;

        let mut module_snapshots = schema.create_table_from_entity(entity::module_snapshot::Entity);
        module_snapshots.if_not_exists();
        self.db.execute(backend.build(&module_snapshots)).await?;

        let mut current_block = schema.create_table_from_entity(entity::current_block::Entity);
        current_block.if_not_exists();
        self.db.execute(backend.build(&current_block)).await?;
        Ok(())
    }

    pub async fn reseed_defaults(
        &self,
        block_collections: &[Preset],
        module_collections: &[ModulePreset],
    ) -> StorageResult<()> {
        entity::snapshot::Entity::delete_many()
            .exec(&self.db)
            .await?;
        entity::preset::Entity::delete_many().exec(&self.db).await?;
        entity::module_snapshot::Entity::delete_many()
            .exec(&self.db)
            .await?;
        entity::module_preset::Entity::delete_many()
            .exec(&self.db)
            .await?;
        entity::current_block::Entity::delete_many()
            .exec(&self.db)
            .await?;

        for collection in block_collections {
            entity::preset::Entity::insert(entity::preset::ActiveModel {
                id: Set(collection.id().to_string()),
                block_type: Set(collection.block_type().as_str().to_string()),
                name: Set(collection.name().to_string()),
                default_snapshot_id: Set(collection.default_snapshot().id().to_string()),
            })
            .exec(&self.db)
            .await?;

            for variant in collection.snapshots() {
                entity::snapshot::Entity::insert(entity::snapshot::ActiveModel {
                    id: Set(variant.id().to_string()),
                    preset_id: Set(collection.id().to_string()),
                    name: Set(variant.name().to_string()),
                    state_json: Set(Self::block_to_json(&variant.block())?),
                })
                .exec(&self.db)
                .await?;
            }
        }

        for collection in module_collections {
            entity::module_preset::Entity::insert(entity::module_preset::ActiveModel {
                id: Set(collection.id().to_string()),
                name: Set(collection.name().to_string()),
                default_snapshot_id: Set(collection.default_snapshot().id().to_string()),
            })
            .exec(&self.db)
            .await?;

            for variant in collection.snapshots() {
                entity::module_snapshot::Entity::insert(entity::module_snapshot::ActiveModel {
                    id: Set(variant.id().to_string()),
                    module_preset_id: Set(collection.id().to_string()),
                    name: Set(variant.name().to_string()),
                    state_json: Set(Self::module_to_json(variant.module())?),
                })
                .exec(&self.db)
                .await?;
            }
        }

        for block_type in [BlockType::Amp, BlockType::Drive] {
            let block = block_collections
                .iter()
                .find(|c| c.block_type() == block_type)
                .map(|c| c.default_snapshot().block())
                .unwrap_or_default();
            self.save_block_state(block_type, block).await?;
        }
        Ok(())
    }

    // region: --- JSON helpers

    fn block_to_json(block: &Block) -> StorageResult<String> {
        serde_json::to_string(block)
            .map_err(|e| StorageError::Data(format!("failed to serialize block state: {e}")))
    }

    fn block_from_json(state_json: &str) -> StorageResult<Block> {
        serde_json::from_str::<Block>(state_json)
            .map_err(|e| StorageError::Data(format!("failed to parse block state json: {e}")))
    }

    fn module_to_json(module: &Module) -> StorageResult<String> {
        serde_json::to_string(module)
            .map_err(|e| StorageError::Data(format!("failed to serialize module state: {e}")))
    }

    fn module_from_json(state_json: &str) -> StorageResult<Module> {
        serde_json::from_str::<Module>(state_json)
            .map_err(|e| StorageError::Data(format!("failed to parse module state json: {e}")))
    }

    // endregion: --- JSON helpers

    // region: --- Model converters

    fn snapshot_from_model(model: &entity::snapshot::Model) -> StorageResult<Snapshot> {
        Ok(Snapshot::new(
            model.snapshot_id_branded(),
            model.name.clone(),
            Self::block_from_json(&model.state_json)?,
        ))
    }

    fn module_snapshot_from_model(
        model: &entity::module_snapshot::Model,
    ) -> StorageResult<ModuleSnapshot> {
        Ok(ModuleSnapshot::new(
            model.snapshot_id_branded(),
            model.name.clone(),
            Self::module_from_json(&model.state_json)?,
        ))
    }

    // endregion: --- Model converters

    // region: --- Shared query helpers

    /// Assemble a full `Preset` (collection) from its entity model by loading all variant snapshots.
    async fn assemble_block_collection(
        &self,
        preset_model: &entity::preset::Model,
        block_type: BlockType,
    ) -> StorageResult<Preset> {
        let snapshot_models = entity::snapshot::Entity::find()
            .filter(entity::snapshot::Column::PresetId.eq(preset_model.id.clone()))
            .order_by_asc(entity::snapshot::Column::Id)
            .all(&self.db)
            .await?;

        let mut variants = Vec::with_capacity(snapshot_models.len());
        for model in &snapshot_models {
            variants.push(Self::snapshot_from_model(model)?);
        }

        let default_variant_id = preset_model.default_snapshot_id_branded();
        let default_variant = variants
            .iter()
            .find(|s| s.id() == &default_variant_id)
            .cloned()
            .ok_or_else(|| {
                StorageError::Data(format!(
                    "collection '{}' references missing default variant '{}'",
                    preset_model.id, preset_model.default_snapshot_id
                ))
            })?;

        let additional = variants
            .into_iter()
            .filter(|s| s.id() != &default_variant_id)
            .collect::<Vec<_>>();

        Ok(Preset::new(
            preset_model.preset_id_branded(),
            preset_model.name.clone(),
            block_type,
            default_variant,
            additional,
        ))
    }

    /// Assemble a full `ModulePreset` (collection) from its entity model by loading all variant snapshots.
    async fn assemble_module_collection(
        &self,
        preset_model: &entity::module_preset::Model,
    ) -> StorageResult<ModulePreset> {
        let snapshot_models = entity::module_snapshot::Entity::find()
            .filter(
                entity::module_snapshot::Column::ModulePresetId.eq(preset_model.id.clone()),
            )
            .order_by_asc(entity::module_snapshot::Column::Id)
            .all(&self.db)
            .await?;

        let mut variants = Vec::with_capacity(snapshot_models.len());
        for model in &snapshot_models {
            variants.push(Self::module_snapshot_from_model(model)?);
        }

        let default_variant_id = preset_model.default_snapshot_id_branded();
        let default_variant = variants
            .iter()
            .find(|s| s.id() == &default_variant_id)
            .cloned()
            .ok_or_else(|| {
                StorageError::Data(format!(
                    "module collection '{}' references missing default variant '{}'",
                    preset_model.id, preset_model.default_snapshot_id
                ))
            })?;

        let additional = variants
            .into_iter()
            .filter(|snapshot| snapshot.id() != &default_variant_id)
            .collect::<Vec<_>>();

        Ok(ModulePreset::new(
            preset_model.preset_id_branded(),
            preset_model.name.clone(),
            default_variant,
            additional,
        ))
    }

    // endregion: --- Shared query helpers
}

// endregion: --- CollectionRepoLive

// region: --- Seed data

pub fn default_seed_block_collections() -> Vec<Preset> {
    vec![
        Preset::new(
            "amp-clean".to_string(),
            "Clean",
            BlockType::Amp,
            Snapshot::new(
                "amp-clean-default".to_string(),
                "Default",
                Block::from_parameters(vec![
                    BlockParameter::new("gain", "Gain", 0.42),
                    BlockParameter::new("bass", "Bass", 0.55),
                    BlockParameter::new("mid", "Mid", 0.52),
                    BlockParameter::new("treble", "Treble", 0.63),
                ]),
            ),
            vec![
                Snapshot::new(
                    "amp-clean-bright".to_string(),
                    "Bright",
                    Block::from_parameters(vec![
                        BlockParameter::new("gain", "Gain", 0.48),
                        BlockParameter::new("bass", "Bass", 0.44),
                        BlockParameter::new("mid", "Mid", 0.50),
                        BlockParameter::new("treble", "Treble", 0.78),
                    ]),
                ),
                Snapshot::new(
                    "amp-clean-warm".to_string(),
                    "Warm",
                    Block::from_parameters(vec![
                        BlockParameter::new("gain", "Gain", 0.38),
                        BlockParameter::new("bass", "Bass", 0.69),
                        BlockParameter::new("mid", "Mid", 0.58),
                        BlockParameter::new("treble", "Treble", 0.45),
                    ]),
                ),
            ],
        ),
        Preset::new(
            "amp-lead".to_string(),
            "Lead",
            BlockType::Amp,
            Snapshot::new(
                "amp-lead-default".to_string(),
                "Default",
                Block::from_parameters(vec![
                    BlockParameter::new("gain", "Gain", 0.80),
                    BlockParameter::new("presence", "Presence", 0.61),
                ]),
            ),
            vec![
                Snapshot::new(
                    "amp-lead-tight".to_string(),
                    "Tight",
                    Block::from_parameters(vec![
                        BlockParameter::new("gain", "Gain", 0.86),
                        BlockParameter::new("presence", "Presence", 0.72),
                    ]),
                ),
                Snapshot::new(
                    "amp-lead-smooth".to_string(),
                    "Smooth",
                    Block::from_parameters(vec![
                        BlockParameter::new("gain", "Gain", 0.74),
                        BlockParameter::new("presence", "Presence", 0.50),
                    ]),
                ),
            ],
        ),
        Preset::new(
            "drive-level".to_string(),
            "Level Drive",
            BlockType::Drive,
            Snapshot::new(
                "drive-level-default".to_string(),
                "Default",
                Block::from_parameters(vec![BlockParameter::new("drive", "Drive", 0.36)]),
            ),
            vec![
                Snapshot::new(
                    "drive-level-low".to_string(),
                    "Low",
                    Block::from_parameters(vec![BlockParameter::new("drive", "Drive", 0.18)]),
                ),
                Snapshot::new(
                    "drive-level-high".to_string(),
                    "High",
                    Block::from_parameters(vec![BlockParameter::new("drive", "Drive", 0.84)]),
                ),
            ],
        ),
        Preset::new(
            "drive-shape".to_string(),
            "Shape Drive",
            BlockType::Drive,
            Snapshot::new(
                "drive-shape-default".to_string(),
                "Default",
                Block::from_parameters(vec![
                    BlockParameter::new("drive", "Drive", 0.67),
                    BlockParameter::new("tone", "Tone", 0.54),
                    BlockParameter::new("blend", "Blend", 0.43),
                    BlockParameter::new("level", "Level", 0.58),
                ]),
            ),
            vec![
                Snapshot::new(
                    "drive-shape-cut".to_string(),
                    "Cut",
                    Block::from_parameters(vec![
                        BlockParameter::new("drive", "Drive", 0.70),
                        BlockParameter::new("tone", "Tone", 0.79),
                        BlockParameter::new("blend", "Blend", 0.35),
                        BlockParameter::new("level", "Level", 0.52),
                    ]),
                ),
                Snapshot::new(
                    "drive-shape-fat".to_string(),
                    "Fat",
                    Block::from_parameters(vec![
                        BlockParameter::new("drive", "Drive", 0.62),
                        BlockParameter::new("tone", "Tone", 0.38),
                        BlockParameter::new("blend", "Blend", 0.68),
                        BlockParameter::new("level", "Level", 0.63),
                    ]),
                ),
            ],
        ),
    ]
}

pub fn default_seed_module_collections() -> Vec<ModulePreset> {
    vec![
        ModulePreset::new(
            "drive-triple-stack",
            "Triple Drive Stack",
            ModuleSnapshot::new(
                "drive-triple-stack-default",
                "Default",
                Module::from_blocks(vec![
                    ModuleBlock::new(
                        "slot-a",
                        "Drive A",
                        BlockType::Drive,
                        ModuleBlockSource::PresetDefault {
                            preset_id: PresetId::from("drive-level"),
                        },
                    ),
                    ModuleBlock::new(
                        "slot-b",
                        "Drive B",
                        BlockType::Drive,
                        ModuleBlockSource::PresetSnapshot {
                            preset_id: PresetId::from("drive-shape"),
                            snapshot_id: SnapshotId::from("drive-shape-fat"),
                        },
                    )
                    .with_overrides(vec![
                        BlockParameterOverride::new("tone", 0.52),
                        BlockParameterOverride::new("blend", 0.81),
                    ]),
                    ModuleBlock::new(
                        "slot-c",
                        "Drive C",
                        BlockType::Drive,
                        ModuleBlockSource::Inline {
                            block: Block::from_parameters(vec![
                                BlockParameter::new("drive", "Drive", 0.76),
                                BlockParameter::new("tone", "Tone", 0.46),
                                BlockParameter::new("blend", "Blend", 0.58),
                                BlockParameter::new("level", "Level", 0.64),
                            ]),
                        },
                    ),
                ]),
            ),
            vec![ModuleSnapshot::new(
                "drive-triple-stack-push",
                "Push",
                Module::from_blocks(vec![
                    ModuleBlock::new(
                        "slot-a",
                        "Drive A",
                        BlockType::Drive,
                        ModuleBlockSource::PresetSnapshot {
                            preset_id: PresetId::from("drive-level"),
                            snapshot_id: SnapshotId::from("drive-level-high"),
                        },
                    ),
                    ModuleBlock::new(
                        "slot-b",
                        "Drive B",
                        BlockType::Drive,
                        ModuleBlockSource::PresetSnapshot {
                            preset_id: PresetId::from("drive-shape"),
                            snapshot_id: SnapshotId::from("drive-shape-cut"),
                        },
                    ),
                    ModuleBlock::new(
                        "slot-c",
                        "Drive C",
                        BlockType::Drive,
                        ModuleBlockSource::Inline {
                            block: Block::from_parameters(vec![
                                BlockParameter::new("drive", "Drive", 0.84),
                                BlockParameter::new("tone", "Tone", 0.61),
                                BlockParameter::new("blend", "Blend", 0.63),
                                BlockParameter::new("level", "Level", 0.69),
                            ]),
                        },
                    )
                    .with_overrides(vec![BlockParameterOverride::new("drive", 0.90)]),
                ]),
            )],
        ),
    ]
}

// endregion: --- Seed data

// region: --- Trait impl

#[async_trait::async_trait]
impl CollectionRepo for CollectionRepoLive {
    async fn load_block_state(&self, block_type: BlockType) -> StorageResult<Option<Block>> {
        let model = entity::current_block::Entity::find_by_id(block_type.as_str().to_string())
            .one(&self.db)
            .await?;

        match model {
            Some(model) => Ok(Some(Self::block_from_json(&model.state_json)?)),
            None => Ok(None),
        }
    }

    async fn save_block_state(&self, block_type: BlockType, block: Block) -> StorageResult<()> {
        let existing = entity::current_block::Entity::find_by_id(block_type.as_str().to_string())
            .one(&self.db)
            .await?;
        let state_json = Self::block_to_json(&block)?;

        if let Some(model) = existing {
            let mut active: entity::current_block::ActiveModel = model.into();
            active.state_json = Set(state_json);
            active.update(&self.db).await?;
        } else {
            entity::current_block::Entity::insert(entity::current_block::ActiveModel {
                block_type: Set(block_type.as_str().to_string()),
                state_json: Set(state_json),
            })
            .exec(&self.db)
            .await?;
        }

        Ok(())
    }

    async fn list_block_collections(&self, block_type: BlockType) -> StorageResult<Vec<Preset>> {
        let preset_models = entity::preset::Entity::find()
            .filter(entity::preset::Column::BlockType.eq(block_type.as_str().to_string()))
            .order_by_asc(entity::preset::Column::Id)
            .all(&self.db)
            .await?;

        let mut out = Vec::with_capacity(preset_models.len());
        for preset_model in &preset_models {
            out.push(self.assemble_block_collection(preset_model, block_type).await?);
        }

        Ok(out)
    }

    async fn load_block_default_variant(
        &self,
        block_type: BlockType,
        collection_id: &PresetId,
    ) -> StorageResult<Option<Snapshot>> {
        let preset = entity::preset::Entity::find_by_id(collection_id.to_string())
            .filter(entity::preset::Column::BlockType.eq(block_type.as_str().to_string()))
            .one(&self.db)
            .await?;

        let Some(preset) = preset else {
            return Ok(None);
        };

        self.load_block_variant(
            block_type,
            collection_id,
            &SnapshotId::from(preset.default_snapshot_id),
        )
        .await
    }

    async fn load_block_variant(
        &self,
        block_type: BlockType,
        collection_id: &PresetId,
        variant_id: &SnapshotId,
    ) -> StorageResult<Option<Snapshot>> {
        let preset = entity::preset::Entity::find_by_id(collection_id.to_string())
            .filter(entity::preset::Column::BlockType.eq(block_type.as_str().to_string()))
            .one(&self.db)
            .await?;
        if preset.is_none() {
            return Ok(None);
        }

        let model = entity::snapshot::Entity::find_by_id(variant_id.to_string())
            .filter(entity::snapshot::Column::PresetId.eq(collection_id.to_string()))
            .one(&self.db)
            .await?;

        match model {
            Some(model) => Ok(Some(Self::snapshot_from_model(&model)?)),
            None => Ok(None),
        }
    }

    async fn list_module_collections(&self) -> StorageResult<Vec<ModulePreset>> {
        let preset_models = entity::module_preset::Entity::find()
            .order_by_asc(entity::module_preset::Column::Id)
            .all(&self.db)
            .await?;

        let mut out = Vec::with_capacity(preset_models.len());
        for preset_model in &preset_models {
            out.push(self.assemble_module_collection(preset_model).await?);
        }

        Ok(out)
    }

    async fn load_module_default_variant(
        &self,
        collection_id: &ModulePresetId,
    ) -> StorageResult<Option<ModuleSnapshot>> {
        let preset = entity::module_preset::Entity::find_by_id(collection_id.to_string())
            .one(&self.db)
            .await?;

        let Some(preset) = preset else {
            return Ok(None);
        };

        self.load_module_variant(
            collection_id,
            &ModuleSnapshotId::from(preset.default_snapshot_id),
        )
        .await
    }

    async fn load_module_variant(
        &self,
        collection_id: &ModulePresetId,
        variant_id: &ModuleSnapshotId,
    ) -> StorageResult<Option<ModuleSnapshot>> {
        let preset = entity::module_preset::Entity::find_by_id(collection_id.to_string())
            .one(&self.db)
            .await?;
        if preset.is_none() {
            return Ok(None);
        }

        let model = entity::module_snapshot::Entity::find_by_id(variant_id.to_string())
            .filter(entity::module_snapshot::Column::ModulePresetId.eq(collection_id.to_string()))
            .one(&self.db)
            .await?;

        match model {
            Some(model) => Ok(Some(Self::module_snapshot_from_model(&model)?)),
            None => Ok(None),
        }
    }
}

// endregion: --- Trait impl

// region: --- Tests

#[cfg(test)]
mod tests {
    use super::*;

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    async fn seeded_repo() -> Result<CollectionRepoLive> {
        let repo = CollectionRepoLive::connect_sqlite_in_memory().await?;
        repo.init_schema().await?;
        repo.reseed_defaults(
            &default_seed_block_collections(),
            &default_seed_module_collections(),
        )
        .await?;
        Ok(repo)
    }

    // -- Block state round-trip

    #[tokio::test]
    async fn block_state_round_trip() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;
        let block = Block::from_parameters(vec![
            BlockParameter::new("gain", "Gain", 0.77),
            BlockParameter::new("bass", "Bass", 0.33),
        ]);

        // -- Exec
        repo.save_block_state(BlockType::Amp, block.clone()).await?;
        let loaded = repo.load_block_state(BlockType::Amp).await?;

        // -- Check
        assert_eq!(loaded, Some(block));
        Ok(())
    }

    #[tokio::test]
    async fn block_state_missing_returns_none() -> Result<()> {
        // -- Setup & Fixtures
        let repo = CollectionRepoLive::connect_sqlite_in_memory().await?;
        repo.init_schema().await?;

        // -- Exec
        let loaded = repo.load_block_state(BlockType::Amp).await?;

        // -- Check
        assert_eq!(loaded, None);
        Ok(())
    }

    // -- Block collection listing

    #[tokio::test]
    async fn list_block_collections_filters_by_type() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;

        // -- Exec
        let amp_collections = repo.list_block_collections(BlockType::Amp).await?;
        let drive_collections = repo.list_block_collections(BlockType::Drive).await?;

        // -- Check
        assert_eq!(amp_collections.len(), 2); // Clean, Lead
        assert_eq!(drive_collections.len(), 2); // Level, Shape
        for c in &amp_collections {
            assert_eq!(c.block_type(), BlockType::Amp);
        }
        for c in &drive_collections {
            assert_eq!(c.block_type(), BlockType::Drive);
        }
        Ok(())
    }

    #[tokio::test]
    async fn block_collection_contains_all_variants() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;

        // -- Exec
        let collections = repo.list_block_collections(BlockType::Amp).await?;
        let clean = collections.iter().find(|c| c.name() == "Clean").unwrap();

        // -- Check: default + 2 additional = 3 total
        assert_eq!(clean.snapshots().len(), 3);
        assert_eq!(clean.default_snapshot().name(), "Default");
        Ok(())
    }

    // -- Block variant loading

    #[tokio::test]
    async fn load_block_default_variant_returns_snapshot() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;
        let collection_id = PresetId::from("amp-clean");

        // -- Exec
        let variant = repo
            .load_block_default_variant(BlockType::Amp, &collection_id)
            .await?;

        // -- Check
        let variant = variant.expect("should find default variant");
        assert_eq!(variant.name(), "Default");
        assert_eq!(variant.id(), &SnapshotId::from("amp-clean-default"));
        Ok(())
    }

    #[tokio::test]
    async fn load_block_variant_by_id() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;
        let collection_id = PresetId::from("amp-clean");
        let variant_id = SnapshotId::from("amp-clean-bright");

        // -- Exec
        let variant = repo
            .load_block_variant(BlockType::Amp, &collection_id, &variant_id)
            .await?;

        // -- Check
        let variant = variant.expect("should find variant");
        assert_eq!(variant.name(), "Bright");
        let block = variant.block();
        let params = block.parameters();
        let treble = params.iter().find(|p| p.id() == "treble").unwrap();
        assert!((treble.value().get() - 0.78).abs() < 0.001);
        Ok(())
    }

    #[tokio::test]
    async fn load_block_variant_wrong_type_returns_none() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;
        let collection_id = PresetId::from("amp-clean");

        // -- Exec: amp-clean is Amp, not Drive
        let variant = repo
            .load_block_default_variant(BlockType::Drive, &collection_id)
            .await?;

        // -- Check
        assert!(variant.is_none());
        Ok(())
    }

    #[tokio::test]
    async fn load_block_variant_missing_collection_returns_none() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;
        let collection_id = PresetId::from("nonexistent");

        // -- Exec
        let variant = repo
            .load_block_default_variant(BlockType::Amp, &collection_id)
            .await?;

        // -- Check
        assert!(variant.is_none());
        Ok(())
    }

    #[tokio::test]
    async fn load_block_variant_missing_variant_returns_none() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;
        let collection_id = PresetId::from("amp-clean");
        let variant_id = SnapshotId::from("nonexistent-variant");

        // -- Exec
        let variant = repo
            .load_block_variant(BlockType::Amp, &collection_id, &variant_id)
            .await?;

        // -- Check
        assert!(variant.is_none());
        Ok(())
    }

    // -- Module collection listing

    #[tokio::test]
    async fn list_module_collections_returns_all() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;

        // -- Exec
        let collections = repo.list_module_collections().await?;

        // -- Check
        assert_eq!(collections.len(), 1);
        assert_eq!(collections[0].name(), "Triple Drive Stack");
        Ok(())
    }

    #[tokio::test]
    async fn module_collection_contains_all_variants() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;

        // -- Exec
        let collections = repo.list_module_collections().await?;
        let stack = &collections[0];

        // -- Check: default + 1 additional = 2 total
        assert_eq!(stack.snapshots().len(), 2);
        assert_eq!(stack.default_snapshot().name(), "Default");
        Ok(())
    }

    // -- Module variant loading

    #[tokio::test]
    async fn load_module_default_variant_returns_snapshot() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;
        let collection_id = ModulePresetId::from("drive-triple-stack");

        // -- Exec
        let variant = repo.load_module_default_variant(&collection_id).await?;

        // -- Check
        let variant = variant.expect("should find default variant");
        assert_eq!(variant.name(), "Default");
        assert_eq!(variant.module().blocks().len(), 3);
        Ok(())
    }

    #[tokio::test]
    async fn load_module_variant_by_id() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;
        let collection_id = ModulePresetId::from("drive-triple-stack");
        let variant_id = ModuleSnapshotId::from("drive-triple-stack-push");

        // -- Exec
        let variant = repo
            .load_module_variant(&collection_id, &variant_id)
            .await?;

        // -- Check
        let variant = variant.expect("should find variant");
        assert_eq!(variant.name(), "Push");
        assert_eq!(variant.module().blocks().len(), 3);
        Ok(())
    }

    #[tokio::test]
    async fn load_module_variant_missing_collection_returns_none() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;
        let collection_id = ModulePresetId::from("nonexistent");

        // -- Exec
        let variant = repo.load_module_default_variant(&collection_id).await?;

        // -- Check
        assert!(variant.is_none());
        Ok(())
    }

    // -- Metadata round-trip (verifies JSON serialization preserves all fields)

    #[tokio::test]
    async fn block_metadata_round_trip_preserves_parameter_names() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;

        // -- Exec
        let collections = repo.list_block_collections(BlockType::Drive).await?;
        let shape = collections.iter().find(|c| c.name() == "Shape Drive").unwrap();
        let default = shape.default_snapshot();

        // -- Check: verify parameter metadata survived serialization
        let block = default.block();
        let params = block.parameters();
        assert_eq!(params.len(), 4);
        assert_eq!(params[0].id(), "drive");
        assert_eq!(params[0].name(), "Drive");
        assert!((params[0].value().get() - 0.67).abs() < 0.001);
        assert_eq!(params[1].id(), "tone");
        assert_eq!(params[2].id(), "blend");
        assert_eq!(params[3].id(), "level");
        Ok(())
    }

    // -- Override round-trip (verifies overrides survive module serialization)

    #[tokio::test]
    async fn module_override_round_trip() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;
        let collection_id = ModulePresetId::from("drive-triple-stack");

        // -- Exec
        let variant = repo
            .load_module_default_variant(&collection_id)
            .await?
            .expect("should find default variant");

        // -- Check: slot-b has overrides
        let blocks = variant.module().blocks();
        let slot_b = blocks.iter().find(|b| b.id() == "slot-b").unwrap();
        let overrides = slot_b.overrides();
        assert_eq!(overrides.len(), 2);
        assert_eq!(overrides[0].parameter_id(), "tone");
        assert!((overrides[0].value().get() - 0.52).abs() < 0.001);
        assert_eq!(overrides[1].parameter_id(), "blend");
        assert!((overrides[1].value().get() - 0.81).abs() < 0.001);

        // Check: slot-c is inline with no overrides
        let slot_c = blocks.iter().find(|b| b.id() == "slot-c").unwrap();
        assert!(slot_c.overrides().is_empty());
        assert!(matches!(slot_c.source(), ModuleBlockSource::Inline { .. }));
        Ok(())
    }

    // -- Default normalization (verifies default variant is always the first in list)

    #[tokio::test]
    async fn default_normalization_first_variant_is_default() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;

        // -- Exec
        let collections = repo.list_block_collections(BlockType::Amp).await?;

        // -- Check: for every collection, default_snapshot_id points to a valid variant
        for collection in &collections {
            let default_id = collection.default_snapshot().id().clone();
            assert!(
                collection.snapshots().iter().any(|s| *s.id() == default_id),
                "default variant '{}' not found in collection '{}'",
                default_id,
                collection.name()
            );
            // The first snapshot is always the default
            assert_eq!(collection.snapshots()[0].id(), &default_id);
        }
        Ok(())
    }

    #[tokio::test]
    async fn module_default_normalization() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;

        // -- Exec
        let collections = repo.list_module_collections().await?;

        // -- Check
        for collection in &collections {
            let default_id = collection.default_snapshot().id().clone();
            assert!(
                collection.snapshots().iter().any(|s| *s.id() == default_id),
                "default variant '{}' not found in module collection '{}'",
                default_id,
                collection.name()
            );
            assert_eq!(collection.snapshots()[0].id(), &default_id);
        }
        Ok(())
    }

    // -- Reseed idempotency

    #[tokio::test]
    async fn reseed_is_idempotent() -> Result<()> {
        // -- Setup & Fixtures
        let repo = CollectionRepoLive::connect_sqlite_in_memory().await?;
        repo.init_schema().await?;

        let block_collections = default_seed_block_collections();
        let module_collections = default_seed_module_collections();

        // -- Exec: seed twice
        repo.reseed_defaults(&block_collections, &module_collections)
            .await?;
        repo.reseed_defaults(&block_collections, &module_collections)
            .await?;

        // -- Check: counts are the same as single seed
        let amp = repo.list_block_collections(BlockType::Amp).await?;
        let drive = repo.list_block_collections(BlockType::Drive).await?;
        let modules = repo.list_module_collections().await?;
        assert_eq!(amp.len(), 2);
        assert_eq!(drive.len(), 2);
        assert_eq!(modules.len(), 1);
        Ok(())
    }

    // -- Module block source round-trip

    #[tokio::test]
    async fn module_block_source_types_round_trip() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;
        let collection_id = ModulePresetId::from("drive-triple-stack");

        // -- Exec
        let variant = repo
            .load_module_default_variant(&collection_id)
            .await?
            .expect("should find default variant");

        // -- Check: each slot has the correct source type
        let blocks = variant.module().blocks();
        assert!(matches!(
            blocks[0].source(),
            ModuleBlockSource::PresetDefault { .. }
        ));
        assert!(matches!(
            blocks[1].source(),
            ModuleBlockSource::PresetSnapshot { .. }
        ));
        assert!(matches!(
            blocks[2].source(),
            ModuleBlockSource::Inline { .. }
        ));
        Ok(())
    }

    // -- Block state overwrite

    #[tokio::test]
    async fn save_block_state_overwrites_previous() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;
        let block1 = Block::from_parameters(vec![BlockParameter::new("a", "A", 0.1)]);
        let block2 = Block::from_parameters(vec![BlockParameter::new("b", "B", 0.9)]);

        // -- Exec
        repo.save_block_state(BlockType::Amp, block1).await?;
        repo.save_block_state(BlockType::Amp, block2.clone()).await?;
        let loaded = repo.load_block_state(BlockType::Amp).await?;

        // -- Check
        assert_eq!(loaded, Some(block2));
        Ok(())
    }
}

// endregion: --- Tests
