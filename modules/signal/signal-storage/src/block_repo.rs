//! Block repo — concrete data access layer (SeaORM)
//!
//! All database queries for blocks/presets/snapshots live here.

use sea_orm::*;
use sea_orm::{ConnectionTrait, Schema};
use signal_proto::{
    Block, BlockParameter, BlockParameterOverride, BlockType, Module, ModuleBlock,
    ModuleBlockSource, ModulePreset, ModulePresetId, ModuleSnapshot, ModuleSnapshotId, Preset,
    PresetId, Snapshot, SnapshotId,
};

use crate::entity;
use crate::{Database, DatabaseConnection, StorageError, StorageResult};

#[async_trait::async_trait]
pub trait BlockRepo: Send + Sync {
    async fn load_block(&self, block_type: BlockType) -> StorageResult<Option<Block>>;
    async fn save_block(&self, block_type: BlockType, block: Block) -> StorageResult<()>;
    async fn list_presets(&self, block_type: BlockType) -> StorageResult<Vec<Preset>>;
    async fn load_preset(
        &self,
        block_type: BlockType,
        preset_id: &PresetId,
    ) -> StorageResult<Option<Snapshot>>;
    async fn load_preset_snapshot(
        &self,
        block_type: BlockType,
        preset_id: &PresetId,
        snapshot_id: &SnapshotId,
    ) -> StorageResult<Option<Snapshot>>;
    async fn list_module_presets(&self) -> StorageResult<Vec<ModulePreset>>;
    async fn load_module_preset(
        &self,
        preset_id: &ModulePresetId,
    ) -> StorageResult<Option<ModuleSnapshot>>;
    async fn load_module_preset_snapshot(
        &self,
        preset_id: &ModulePresetId,
        snapshot_id: &ModuleSnapshotId,
    ) -> StorageResult<Option<ModuleSnapshot>>;
}

#[derive(Clone)]
pub struct BlockRepoLive {
    db: DatabaseConnection,
}

impl BlockRepoLive {
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
        block_presets: &[Preset],
        module_presets: &[ModulePreset],
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

        for preset in block_presets {
            entity::preset::Entity::insert(entity::preset::ActiveModel {
                id: Set(preset.id().to_string()),
                block_type: Set(preset.block_type().as_str().to_string()),
                name: Set(preset.name().to_string()),
                default_snapshot_id: Set(preset.default_snapshot().id().to_string()),
            })
            .exec(&self.db)
            .await?;

            for snapshot in preset.snapshots() {
                entity::snapshot::Entity::insert(entity::snapshot::ActiveModel {
                    id: Set(snapshot.id().to_string()),
                    preset_id: Set(preset.id().to_string()),
                    name: Set(snapshot.name().to_string()),
                    state_json: Set(Self::block_to_json(&snapshot.block())?),
                })
                .exec(&self.db)
                .await?;
            }
        }

        for preset in module_presets {
            entity::module_preset::Entity::insert(entity::module_preset::ActiveModel {
                id: Set(preset.id().to_string()),
                name: Set(preset.name().to_string()),
                default_snapshot_id: Set(preset.default_snapshot().id().to_string()),
            })
            .exec(&self.db)
            .await?;

            for snapshot in preset.snapshots() {
                entity::module_snapshot::Entity::insert(entity::module_snapshot::ActiveModel {
                    id: Set(snapshot.id().to_string()),
                    module_preset_id: Set(preset.id().to_string()),
                    name: Set(snapshot.name().to_string()),
                    state_json: Set(Self::module_to_json(snapshot.module())?),
                })
                .exec(&self.db)
                .await?;
            }
        }

        for block_type in [BlockType::Amp, BlockType::Drive] {
            let block = block_presets
                .iter()
                .find(|preset| preset.block_type() == block_type)
                .map(|preset| preset.default_snapshot().block())
                .unwrap_or_default();
            self.save_block(block_type, block).await?;
        }
        Ok(())
    }

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
}

pub fn default_seed_presets() -> Vec<Preset> {
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

pub fn default_seed_module_presets() -> Vec<ModulePreset> {
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

#[async_trait::async_trait]
impl BlockRepo for BlockRepoLive {
    async fn load_block(&self, block_type: BlockType) -> StorageResult<Option<Block>> {
        let model = entity::current_block::Entity::find_by_id(block_type.as_str().to_string())
            .one(&self.db)
            .await?;

        match model {
            Some(model) => Ok(Some(Self::block_from_json(&model.state_json)?)),
            None => Ok(None),
        }
    }

    async fn save_block(&self, block_type: BlockType, block: Block) -> StorageResult<()> {
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

    async fn list_presets(&self, block_type: BlockType) -> StorageResult<Vec<Preset>> {
        let preset_models = entity::preset::Entity::find()
            .filter(entity::preset::Column::BlockType.eq(block_type.as_str().to_string()))
            .order_by_asc(entity::preset::Column::Id)
            .all(&self.db)
            .await?;

        let mut out = Vec::with_capacity(preset_models.len());
        for preset_model in preset_models {
            let snapshot_models = entity::snapshot::Entity::find()
                .filter(entity::snapshot::Column::PresetId.eq(preset_model.id.clone()))
                .order_by_asc(entity::snapshot::Column::Id)
                .all(&self.db)
                .await?;

            let mut snapshots = Vec::with_capacity(snapshot_models.len());
            for model in &snapshot_models {
                snapshots.push(Self::snapshot_from_model(model)?);
            }

            let default_snapshot_id = preset_model.default_snapshot_id_branded();
            let default_snapshot = snapshots
                .iter()
                .find(|s| s.id() == &default_snapshot_id)
                .cloned()
                .ok_or_else(|| {
                    StorageError::Data(format!(
                        "preset '{}' references missing snapshot '{}'",
                        preset_model.id, preset_model.default_snapshot_id
                    ))
                })?;

            let additional = snapshots
                .into_iter()
                .filter(|s| s.id() != &default_snapshot_id)
                .collect::<Vec<_>>();

            out.push(Preset::new(
                preset_model.preset_id_branded(),
                preset_model.name,
                block_type,
                default_snapshot,
                additional,
            ));
        }

        Ok(out)
    }

    async fn load_preset(
        &self,
        block_type: BlockType,
        preset_id: &PresetId,
    ) -> StorageResult<Option<Snapshot>> {
        let preset = entity::preset::Entity::find_by_id(preset_id.to_string())
            .filter(entity::preset::Column::BlockType.eq(block_type.as_str().to_string()))
            .one(&self.db)
            .await?;

        let Some(preset) = preset else {
            return Ok(None);
        };

        self.load_preset_snapshot(
            block_type,
            preset_id,
            &SnapshotId::from(preset.default_snapshot_id),
        )
            .await
    }

    async fn load_preset_snapshot(
        &self,
        block_type: BlockType,
        preset_id: &PresetId,
        snapshot_id: &SnapshotId,
    ) -> StorageResult<Option<Snapshot>> {
        let preset = entity::preset::Entity::find_by_id(preset_id.to_string())
            .filter(entity::preset::Column::BlockType.eq(block_type.as_str().to_string()))
            .one(&self.db)
            .await?;
        if preset.is_none() {
            return Ok(None);
        }

        let model = entity::snapshot::Entity::find_by_id(snapshot_id.to_string())
            .filter(entity::snapshot::Column::PresetId.eq(preset_id.to_string()))
            .one(&self.db)
            .await?;

        match model {
            Some(model) => Ok(Some(Self::snapshot_from_model(&model)?)),
            None => Ok(None),
        }
    }

    async fn list_module_presets(&self) -> StorageResult<Vec<ModulePreset>> {
        let preset_models = entity::module_preset::Entity::find()
            .order_by_asc(entity::module_preset::Column::Id)
            .all(&self.db)
            .await?;

        let mut out = Vec::with_capacity(preset_models.len());
        for preset_model in preset_models {
            let snapshot_models = entity::module_snapshot::Entity::find()
                .filter(
                    entity::module_snapshot::Column::ModulePresetId.eq(preset_model.id.clone()),
                )
                .order_by_asc(entity::module_snapshot::Column::Id)
                .all(&self.db)
                .await?;

            let mut snapshots = Vec::with_capacity(snapshot_models.len());
            for model in &snapshot_models {
                snapshots.push(Self::module_snapshot_from_model(model)?);
            }

            let default_snapshot_id = preset_model.default_snapshot_id_branded();
            let default_snapshot = snapshots
                .iter()
                .find(|s| s.id() == &default_snapshot_id)
                .cloned()
                .ok_or_else(|| {
                    StorageError::Data(format!(
                        "module preset '{}' references missing snapshot '{}'",
                        preset_model.id, preset_model.default_snapshot_id
                    ))
                })?;

            let additional = snapshots
                .into_iter()
                .filter(|snapshot| snapshot.id() != &default_snapshot_id)
                .collect::<Vec<_>>();

            out.push(ModulePreset::new(
                preset_model.preset_id_branded(),
                preset_model.name,
                default_snapshot,
                additional,
            ));
        }

        Ok(out)
    }

    async fn load_module_preset(
        &self,
        preset_id: &ModulePresetId,
    ) -> StorageResult<Option<ModuleSnapshot>> {
        let preset = entity::module_preset::Entity::find_by_id(preset_id.to_string())
            .one(&self.db)
            .await?;

        let Some(preset) = preset else {
            return Ok(None);
        };

        self.load_module_preset_snapshot(
            preset_id,
            &ModuleSnapshotId::from(preset.default_snapshot_id),
        )
        .await
    }

    async fn load_module_preset_snapshot(
        &self,
        preset_id: &ModulePresetId,
        snapshot_id: &ModuleSnapshotId,
    ) -> StorageResult<Option<ModuleSnapshot>> {
        let preset = entity::module_preset::Entity::find_by_id(preset_id.to_string())
            .one(&self.db)
            .await?;
        if preset.is_none() {
            return Ok(None);
        }

        let model = entity::module_snapshot::Entity::find_by_id(snapshot_id.to_string())
            .filter(entity::module_snapshot::Column::ModulePresetId.eq(preset_id.to_string()))
            .one(&self.db)
            .await?;

        match model {
            Some(model) => Ok(Some(Self::module_snapshot_from_model(&model)?)),
            None => Ok(None),
        }
    }
}
