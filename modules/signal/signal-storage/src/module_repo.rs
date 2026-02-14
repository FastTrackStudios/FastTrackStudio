//! Module repository — data access for module collections and variants.

use sea_orm::*;
use sea_orm::{ConnectionTrait, Schema};
use signal_proto::{Module, ModulePreset, ModulePresetId, ModuleSnapshot, ModuleSnapshotId};

use crate::entity;
use crate::{Database, DatabaseConnection, StorageError, StorageResult};

// region: --- Trait

#[async_trait::async_trait]
pub trait ModuleRepo: Send + Sync {
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

// region: --- ModuleRepoLive

#[derive(Clone)]
pub struct ModuleRepoLive {
    db: DatabaseConnection,
}

impl ModuleRepoLive {
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

        let mut module_presets = schema.create_table_from_entity(entity::module_preset::Entity);
        module_presets.if_not_exists();
        self.db.execute(backend.build(&module_presets)).await?;

        let mut module_snapshots = schema.create_table_from_entity(entity::module_snapshot::Entity);
        module_snapshots.if_not_exists();
        self.db.execute(backend.build(&module_snapshots)).await?;

        // Add version column if missing
        self.db
            .execute_unprepared(
                "ALTER TABLE module_snapshots ADD COLUMN version INTEGER NOT NULL DEFAULT 1",
            )
            .await
            .ok();

        // Add module_type column if missing
        self.db
            .execute_unprepared(
                "ALTER TABLE module_presets ADD COLUMN module_type TEXT NOT NULL DEFAULT 'custom'",
            )
            .await
            .ok();

        Ok(())
    }

    pub async fn reseed_defaults(&self, module_collections: &[ModulePreset]) -> StorageResult<()> {
        entity::module_snapshot::Entity::delete_many()
            .exec(&self.db)
            .await?;
        entity::module_preset::Entity::delete_many()
            .exec(&self.db)
            .await?;

        for collection in module_collections {
            entity::module_preset::Entity::insert(entity::module_preset::ActiveModel {
                id: Set(collection.id().to_string()),
                name: Set(collection.name().to_string()),
                module_type: Set(collection.module_type().as_str().to_string()),
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
                    version: Set(variant.version() as i32),
                })
                .exec(&self.db)
                .await?;
            }
        }

        Ok(())
    }

    // region: --- JSON helpers

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

    fn module_snapshot_from_model(
        model: &entity::module_snapshot::Model,
    ) -> StorageResult<ModuleSnapshot> {
        Ok(ModuleSnapshot::with_version(
            model.snapshot_id_branded(),
            model.name.clone(),
            Self::module_from_json(&model.state_json)?,
            model.version as u32,
        ))
    }

    // endregion: --- Model converters

    // region: --- Shared query helpers

    /// Assemble a full `ModulePreset` (collection) from its entity model by loading all variant snapshots.
    async fn assemble_module_collection(
        &self,
        preset_model: &entity::module_preset::Model,
    ) -> StorageResult<ModulePreset> {
        let snapshot_models = entity::module_snapshot::Entity::find()
            .filter(entity::module_snapshot::Column::ModulePresetId.eq(preset_model.id.clone()))
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
            preset_model.module_type_branded(),
            default_variant,
            additional,
        ))
    }

    // endregion: --- Shared query helpers
}

// endregion: --- ModuleRepoLive

// region: --- Trait impl

#[async_trait::async_trait]
impl ModuleRepo for ModuleRepoLive {
    async fn list_module_collections(&self) -> StorageResult<Vec<ModulePreset>> {
        let preset_models = entity::module_preset::Entity::find()
            .order_by_asc(entity::module_preset::Column::Id)
            .all(&self.db)
            .await?;

        let mut out = Vec::with_capacity(preset_models.len());
        for preset_model in preset_models
            .iter()
            .filter(|p| !p.name.starts_with("__phantom__"))
        {
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
    use signal_proto::{
        seed_id, Block, BlockParameter, BlockParameterOverride, BlockType, ModuleBlock,
        ModuleBlockSource, ModuleType, PresetId, SignalChain, SignalNode,
    };

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    async fn seeded_repo() -> Result<ModuleRepoLive> {
        let repo = ModuleRepoLive::connect_sqlite_in_memory().await?;
        repo.init_schema().await?;
        repo.reseed_defaults(&crate::seed_data::default_module_collections())
            .await?;
        Ok(repo)
    }

    // -- Module collection listing

    #[tokio::test]
    async fn list_module_collections_returns_all() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;

        // -- Exec
        let collections = repo.list_module_collections().await?;

        // -- Check
        assert_eq!(collections.len(), 17);
        let mut names: Vec<&str> = collections.iter().map(|c| c.name()).collect();
        names.sort();
        assert!(names.contains(&"Drive Duo"));
        assert!(names.contains(&"Full Drive Stack"));
        assert!(names.contains(&"Parallel Time"));
        assert!(names.contains(&"Source"));
        assert!(names.contains(&"Rescue"));
        Ok(())
    }

    #[tokio::test]
    async fn module_collection_contains_all_variants() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;

        // -- Exec
        let collections = repo.list_module_collections().await?;
        let stack = collections
            .iter()
            .find(|c| c.name() == "Full Drive Stack")
            .unwrap();

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
        let collection_id = ModulePresetId::from_uuid(seed_id("drive-full-stack"));

        // -- Exec
        let variant = repo.load_module_default_variant(&collection_id).await?;

        // -- Check
        let variant = variant.expect("should find default variant");
        assert_eq!(variant.name(), "Default");
        assert_eq!(variant.module().blocks().len(), 4);
        Ok(())
    }

    #[tokio::test]
    async fn load_module_variant_by_id() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;
        let collection_id = ModulePresetId::from_uuid(seed_id("drive-full-stack"));
        let variant_id = ModuleSnapshotId::from_uuid(seed_id("drive-full-stack-push"));

        // -- Exec
        let variant = repo
            .load_module_variant(&collection_id, &variant_id)
            .await?;

        // -- Check
        let variant = variant.expect("should find variant");
        assert_eq!(variant.name(), "Push");
        assert_eq!(variant.module().blocks().len(), 4);
        Ok(())
    }

    #[tokio::test]
    async fn load_module_variant_missing_collection_returns_none() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;
        let collection_id = ModulePresetId::new();

        // -- Exec
        let variant = repo.load_module_default_variant(&collection_id).await?;

        // -- Check
        assert!(variant.is_none());
        Ok(())
    }

    // -- Override round-trip

    #[tokio::test]
    async fn module_override_round_trip() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;
        let collection_id = ModulePresetId::from_uuid(seed_id("drive-full-stack"));

        // -- Exec
        let variant = repo
            .load_module_default_variant(&collection_id)
            .await?
            .expect("should find default variant");

        // -- Check: drive-2 (Klon) has overrides
        let blocks = variant.module().blocks();
        let drive_2 = blocks.iter().find(|b| b.id() == "drive-2").unwrap();
        let overrides = drive_2.overrides();
        assert_eq!(overrides.len(), 2);
        assert_eq!(overrides[0].parameter_id(), "treble");
        assert!((overrides[0].value().get() - 0.55).abs() < 0.001);
        assert_eq!(overrides[1].parameter_id(), "output");
        assert!((overrides[1].value().get() - 0.65).abs() < 0.001);

        // Check: drive-3 (OCD) is a preset snapshot with no overrides
        let drive_3 = blocks.iter().find(|b| b.id() == "drive-3").unwrap();
        assert!(drive_3.overrides().is_empty());
        assert!(matches!(
            drive_3.source(),
            ModuleBlockSource::PresetSnapshot { .. }
        ));
        Ok(())
    }

    // -- Default normalization

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
        let repo = ModuleRepoLive::connect_sqlite_in_memory().await?;
        repo.init_schema().await?;

        let module_collections = crate::seed_data::default_module_collections();

        // -- Exec: seed twice
        repo.reseed_defaults(&module_collections).await?;
        repo.reseed_defaults(&module_collections).await?;

        // -- Check: counts are the same as single seed
        let modules = repo.list_module_collections().await?;
        assert_eq!(modules.len(), 17);
        Ok(())
    }

    // -- Module block source round-trip

    #[tokio::test]
    async fn module_block_source_types_round_trip() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;
        let collection_id = ModulePresetId::from_uuid(seed_id("drive-full-stack"));

        // -- Exec
        let variant = repo
            .load_module_default_variant(&collection_id)
            .await?
            .expect("should find default variant");

        // -- Check: each slot has the correct source type
        // boost (PresetDefault), drive-1 (PresetDefault), drive-2 (PresetSnapshot), drive-3 (PresetSnapshot)
        let blocks = variant.module().blocks();
        assert!(matches!(
            blocks[0].source(),
            ModuleBlockSource::PresetDefault { .. }
        ));
        assert!(matches!(
            blocks[1].source(),
            ModuleBlockSource::PresetDefault { .. }
        ));
        assert!(matches!(
            blocks[2].source(),
            ModuleBlockSource::PresetSnapshot { .. }
        ));
        assert!(matches!(
            blocks[3].source(),
            ModuleBlockSource::PresetSnapshot { .. }
        ));
        Ok(())
    }

    // -- Snapshot version round-trip

    #[tokio::test]
    async fn module_snapshot_version_round_trips_through_db() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;

        // -- Exec
        let collections = repo.list_module_collections().await?;

        // -- Check: all module snapshots should be at version 1
        for collection in &collections {
            for snap in collection.snapshots() {
                assert_eq!(
                    snap.version(),
                    1,
                    "module snapshot '{}' should be at version 1",
                    snap.name()
                );
            }
        }
        Ok(())
    }

    // -- Override on block inside parallel split survives DB round-trip

    #[tokio::test]
    async fn parallel_block_override_round_trip() -> Result<()> {
        // -- Setup & Fixtures: module with overrides on blocks inside a split
        let repo = ModuleRepoLive::connect_sqlite_in_memory().await?;
        repo.init_schema().await?;

        let module = Module::from_chain(SignalChain::new(vec![
            SignalNode::Block(ModuleBlock::new(
                "pre-eq",
                "Pre EQ",
                BlockType::Eq,
                ModuleBlockSource::PresetDefault {
                    preset_id: PresetId::from_uuid(seed_id("eq-reaeq")),
                    saved_at_version: None,
                },
            )),
            SignalNode::Split {
                lanes: vec![
                    SignalChain::serial(vec![ModuleBlock::new(
                        "delay",
                        "Delay",
                        BlockType::Delay,
                        ModuleBlockSource::PresetDefault {
                            preset_id: PresetId::from_uuid(seed_id("delay-timeline")),
                            saved_at_version: None,
                        },
                    )
                    .with_overrides(vec![
                        BlockParameterOverride::new("time", 0.65),
                        BlockParameterOverride::new("feedback", 0.40),
                    ])]),
                    SignalChain::serial(vec![ModuleBlock::new(
                        "reverb",
                        "Reverb",
                        BlockType::Reverb,
                        ModuleBlockSource::PresetDefault {
                            preset_id: PresetId::from_uuid(seed_id("reverb-bigsky")),
                            saved_at_version: None,
                        },
                    )
                    .with_overrides(vec![BlockParameterOverride::new("decay", 0.80)])]),
                ],
            },
            SignalNode::Block(ModuleBlock::new(
                "post-vol",
                "Post Volume",
                BlockType::Volume,
                ModuleBlockSource::Inline {
                    block: Block::from_parameters(vec![BlockParameter::new(
                        "level", "Level", 0.70,
                    )]),
                },
            )),
        ]));

        let collection = ModulePreset::new(
            seed_id("test-parallel-overrides"),
            "Parallel Overrides Test",
            ModuleType::Custom,
            ModuleSnapshot::new(seed_id("par-ov-default"), "Default", module),
            vec![],
        );

        repo.reseed_defaults(&[collection]).await?;

        // -- Exec: load it back from DB
        let loaded = repo
            .load_module_default_variant(&ModulePresetId::from_uuid(seed_id(
                "test-parallel-overrides",
            )))
            .await?
            .expect("should find variant");

        // -- Check: topology preserved
        let chain = loaded.module().chain();
        assert!(!chain.is_serial()); // has a split
        assert_eq!(chain.len(), 3); // pre-eq, split, post-vol

        // -- Check: overrides on blocks inside the split survived
        let blocks = loaded.module().blocks();
        assert_eq!(blocks.len(), 4); // pre-eq, delay, reverb, post-vol

        let delay = blocks.iter().find(|b| b.id() == "delay").unwrap();
        assert_eq!(delay.overrides().len(), 2);
        assert_eq!(delay.overrides()[0].parameter_id(), "time");
        assert!((delay.overrides()[0].value().get() - 0.65).abs() < 0.001);
        assert_eq!(delay.overrides()[1].parameter_id(), "feedback");
        assert!((delay.overrides()[1].value().get() - 0.40).abs() < 0.001);

        let reverb = blocks.iter().find(|b| b.id() == "reverb").unwrap();
        assert_eq!(reverb.overrides().len(), 1);
        assert_eq!(reverb.overrides()[0].parameter_id(), "decay");
        assert!((reverb.overrides()[0].value().get() - 0.80).abs() < 0.001);

        // pre-eq and post-vol have no overrides
        let pre_eq = blocks.iter().find(|b| b.id() == "pre-eq").unwrap();
        assert!(pre_eq.overrides().is_empty());
        let post_vol = blocks.iter().find(|b| b.id() == "post-vol").unwrap();
        assert!(post_vol.overrides().is_empty());
        Ok(())
    }

    // -- Replace a block in a module's signal chain

    #[tokio::test]
    async fn replace_block_in_parallel_module() -> Result<()> {
        // -- Setup & Fixtures: load the "Parallel Time" default variant
        let repo = seeded_repo().await?;
        let original = repo
            .load_module_default_variant(&ModulePresetId::from_uuid(seed_id("time-parallel")))
            .await?
            .expect("should find variant");

        // Verify original has dly-1 in the first split
        let orig_blocks = original.module().blocks();
        assert!(orig_blocks.iter().any(|b| b.id() == "dly-1"));
        assert!(!orig_blocks.iter().any(|b| b.id() == "chorus-new"));

        // -- Exec: clone the module, find dly-1 in split 0, replace it with a chorus
        let mut chain = original.module().chain().clone();
        // The delay split is at index 0 in the top-level nodes
        let split_node = &mut chain.nodes_mut()[0];
        if let SignalNode::Split { lanes } = split_node {
            // Lane 0 has dly-1 — replace it
            lanes[0] = SignalChain::serial(vec![ModuleBlock::new(
                "chorus-new",
                "Chorus",
                BlockType::Chorus,
                ModuleBlockSource::PresetDefault {
                    preset_id: PresetId::from_uuid(seed_id("chorus-js")),
                    saved_at_version: None,
                },
            )]);
        } else {
            panic!("expected split at index 0");
        }

        // Build a new module collection with the modified chain
        let modified_module = Module::from_chain(chain);
        let modified_snapshot = ModuleSnapshot::new(
            seed_id("time-parallel-chorus"),
            "Chorus Variant",
            modified_module,
        );
        let collection = ModulePreset::new(
            seed_id("time-parallel-modified"),
            "Modified Parallel Time",
            ModuleType::Time,
            modified_snapshot,
            vec![],
        );
        repo.reseed_defaults(&[collection]).await?;

        // -- Exec: load it back
        let loaded = repo
            .load_module_default_variant(&ModulePresetId::from_uuid(seed_id(
                "time-parallel-modified",
            )))
            .await?
            .expect("should find modified variant");

        // -- Check: dly-1 is gone, chorus-new is in its place
        let loaded_blocks = loaded.module().blocks();
        assert!(!loaded_blocks.iter().any(|b| b.id() == "dly-1"));
        assert!(loaded_blocks.iter().any(|b| b.id() == "chorus-new"));

        let chorus = loaded_blocks
            .iter()
            .find(|b| b.id() == "chorus-new")
            .unwrap();
        assert_eq!(chorus.block_type(), BlockType::Chorus);

        // dly-2 is still in the other lane of the delay split
        assert!(loaded_blocks.iter().any(|b| b.id() == "dly-2"));
        Ok(())
    }
}

// endregion: --- Tests
