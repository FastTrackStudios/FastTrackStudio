//! Block repository — data access for block state, collections, and variants.

use sea_orm::*;
use sea_orm::{ConnectionTrait, Schema};
use signal_proto::{Block, BlockType, Preset, PresetId, Snapshot, SnapshotId, ALL_BLOCK_TYPES};

use crate::entity;
use crate::{Database, DatabaseConnection, StorageError, StorageResult};

// region: --- Trait

#[async_trait::async_trait]
pub trait BlockRepo: Send + Sync {
    async fn load_block_state(&self, block_type: BlockType) -> StorageResult<Option<Block>>;
    async fn save_block_state(&self, block_type: BlockType, block: Block) -> StorageResult<()>;
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
}

// endregion: --- Trait

// region: --- BlockRepoLive

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

        let mut current_block = schema.create_table_from_entity(entity::current_block::Entity);
        current_block.if_not_exists();
        self.db.execute(backend.build(&current_block)).await?;

        // Add version column if missing (handles existing DBs created before versioning).
        self.db
            .execute_unprepared(
                "ALTER TABLE snapshots ADD COLUMN version INTEGER NOT NULL DEFAULT 1",
            )
            .await
            .ok();

        Ok(())
    }

    pub async fn reseed_defaults(&self, block_collections: &[Preset]) -> StorageResult<()> {
        entity::snapshot::Entity::delete_many()
            .exec(&self.db)
            .await?;
        entity::preset::Entity::delete_many().exec(&self.db).await?;
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
                    state_json: Set(block_to_json(&variant.block())?),
                    version: Set(variant.version() as i32),
                })
                .exec(&self.db)
                .await?;
            }
        }

        for &block_type in ALL_BLOCK_TYPES {
            let block = block_collections
                .iter()
                .find(|c| c.block_type() == block_type)
                .map(|c| c.default_snapshot().block())
                .unwrap_or_default();
            self.save_block_state(block_type, block).await?;
        }

        Ok(())
    }
}

// endregion: --- BlockRepoLive

// region: --- Private helpers

fn block_to_json(block: &Block) -> StorageResult<String> {
    serde_json::to_string(block)
        .map_err(|e| StorageError::Data(format!("failed to serialize block state: {e}")))
}

fn block_from_json(state_json: &str) -> StorageResult<Block> {
    serde_json::from_str::<Block>(state_json)
        .map_err(|e| StorageError::Data(format!("failed to parse block state json: {e}")))
}

fn snapshot_from_model(model: &entity::snapshot::Model) -> StorageResult<Snapshot> {
    Ok(Snapshot::with_version(
        model.snapshot_id_branded(),
        model.name.clone(),
        block_from_json(&model.state_json)?,
        model.version as u32,
    ))
}

// endregion: --- Private helpers

// region: --- Shared query helpers

impl BlockRepoLive {
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
            variants.push(snapshot_from_model(model)?);
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
}

// endregion: --- Shared query helpers

// region: --- Trait impl

#[async_trait::async_trait]
impl BlockRepo for BlockRepoLive {
    async fn load_block_state(&self, block_type: BlockType) -> StorageResult<Option<Block>> {
        let model = entity::current_block::Entity::find_by_id(block_type.as_str().to_string())
            .one(&self.db)
            .await?;

        match model {
            Some(model) => Ok(Some(block_from_json(&model.state_json)?)),
            None => Ok(None),
        }
    }

    async fn save_block_state(&self, block_type: BlockType, block: Block) -> StorageResult<()> {
        let existing = entity::current_block::Entity::find_by_id(block_type.as_str().to_string())
            .one(&self.db)
            .await?;
        let state_json = block_to_json(&block)?;

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
        for preset_model in preset_models
            .iter()
            .filter(|p| !p.name.starts_with("__phantom__"))
        {
            out.push(
                self.assemble_block_collection(preset_model, block_type)
                    .await?,
            );
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
            Some(model) => Ok(Some(snapshot_from_model(&model)?)),
            None => Ok(None),
        }
    }
}

// endregion: --- Trait impl

// region: --- Tests

#[cfg(test)]
mod tests {
    use super::*;
    use signal_proto::{seed_id, BlockParameter};

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    async fn seeded_repo() -> Result<BlockRepoLive> {
        let repo = BlockRepoLive::connect_sqlite_in_memory().await?;
        repo.init_schema().await?;
        repo.reseed_defaults(&crate::seed_data::default_block_collections())
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
        let repo = BlockRepoLive::connect_sqlite_in_memory().await?;
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
        assert_eq!(amp_collections.len(), 5); // Twin, AC30, JCM800, Recto, SLO
        assert_eq!(drive_collections.len(), 5); // TS808, Klon, OCD, Bluesbreaker, Morning Glory
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
        let twin = collections
            .iter()
            .find(|c| c.name() == "Fender Twin Reverb")
            .unwrap();

        // -- Check: default + 4 additional = 5 total
        assert_eq!(twin.snapshots().len(), 5);
        assert_eq!(twin.default_snapshot().name(), "Default");
        Ok(())
    }

    // -- Block variant loading

    #[tokio::test]
    async fn load_block_default_variant_returns_snapshot() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;
        let collection_id = PresetId::from_uuid(seed_id("amp-twin"));

        // -- Exec
        let variant = repo
            .load_block_default_variant(BlockType::Amp, &collection_id)
            .await?;

        // -- Check
        let variant = variant.expect("should find default variant");
        assert_eq!(variant.name(), "Default");
        assert_eq!(
            variant.id(),
            &SnapshotId::from_uuid(seed_id("amp-twin-default"))
        );
        Ok(())
    }

    #[tokio::test]
    async fn load_block_variant_by_id() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;
        let collection_id = PresetId::from_uuid(seed_id("amp-twin"));
        let variant_id = SnapshotId::from_uuid(seed_id("amp-twin-surf"));

        // -- Exec
        let variant = repo
            .load_block_variant(BlockType::Amp, &collection_id, &variant_id)
            .await?;

        // -- Check
        let variant = variant.expect("should find variant");
        assert_eq!(variant.name(), "Surf");
        let block = variant.block();
        let params = block.parameters();
        let reverb = params.iter().find(|p| p.id() == "reverb").unwrap();
        assert!((reverb.value().get() - 0.75).abs() < 0.001);
        Ok(())
    }

    #[tokio::test]
    async fn load_block_variant_wrong_type_returns_none() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;
        let collection_id = PresetId::from_uuid(seed_id("amp-twin"));

        // -- Exec: amp-twin is Amp, not Drive
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
        let collection_id = PresetId::new();

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
        let collection_id = PresetId::from_uuid(seed_id("amp-twin"));
        let variant_id = SnapshotId::new();

        // -- Exec
        let variant = repo
            .load_block_variant(BlockType::Amp, &collection_id, &variant_id)
            .await?;

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
        let ts = collections
            .iter()
            .find(|c| c.name() == "Tubescreamer")
            .unwrap();
        let default = ts.default_snapshot();

        // -- Check: verify parameter metadata survived serialization
        let block = default.block();
        let params = block.parameters();
        assert_eq!(params.len(), 3);
        assert_eq!(params[0].id(), "drive");
        assert_eq!(params[0].name(), "Drive");
        assert!((params[0].value().get() - 0.50).abs() < 0.001);
        assert_eq!(params[1].id(), "tone");
        assert_eq!(params[2].id(), "level");
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

    // -- Block state overwrite

    #[tokio::test]
    async fn save_block_state_overwrites_previous() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;
        let block1 = Block::from_parameters(vec![BlockParameter::new("a", "A", 0.1)]);
        let block2 = Block::from_parameters(vec![BlockParameter::new("b", "B", 0.9)]);

        // -- Exec
        repo.save_block_state(BlockType::Amp, block1).await?;
        repo.save_block_state(BlockType::Amp, block2.clone())
            .await?;
        let loaded = repo.load_block_state(BlockType::Amp).await?;

        // -- Check
        assert_eq!(loaded, Some(block2));
        Ok(())
    }

    // -- Reseed idempotency

    #[tokio::test]
    async fn reseed_is_idempotent() -> Result<()> {
        // -- Setup & Fixtures
        let repo = BlockRepoLive::connect_sqlite_in_memory().await?;
        repo.init_schema().await?;

        let block_collections = crate::seed_data::default_block_collections();

        // -- Exec: seed twice
        repo.reseed_defaults(&block_collections).await?;
        repo.reseed_defaults(&block_collections).await?;

        // -- Check: counts are the same as single seed
        let amp = repo.list_block_collections(BlockType::Amp).await?;
        let drive = repo.list_block_collections(BlockType::Drive).await?;
        assert_eq!(amp.len(), 5);
        assert_eq!(drive.len(), 5);
        Ok(())
    }

    // -- Snapshot version round-trip

    #[tokio::test]
    async fn snapshot_version_round_trips_through_db() -> Result<()> {
        // -- Setup & Fixtures
        let repo = seeded_repo().await?;

        // -- Exec: load an amp collection and check version
        let collections = repo.list_block_collections(BlockType::Amp).await?;
        let twin = collections
            .iter()
            .find(|c| c.name() == "Fender Twin Reverb")
            .expect("should find Twin Reverb");

        // -- Check: seed data starts at version 1
        for snap in twin.snapshots() {
            assert_eq!(
                snap.version(),
                1,
                "seed snapshot '{}' should start at version 1",
                snap.name()
            );
        }
        Ok(())
    }
}

// endregion: --- Tests
