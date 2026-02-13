//! Layer repository — data access for Layer collections and LayerSnapshot variants.

use sea_orm::*;
use signal_proto::layer::{BlockRef, Layer, LayerId, LayerSnapshot, LayerSnapshotId, ModuleRef};
use signal_proto::metadata::Metadata;
use signal_proto::overrides::Override;

use crate::entity;
use crate::{DatabaseConnection, StorageError, StorageResult};

// region: --- Trait

/// Data-access trait for Layer collections.
#[async_trait::async_trait]
pub trait LayerRepo: Send + Sync {
    async fn list_layers(&self) -> StorageResult<Vec<Layer>>;
    async fn load_layer(&self, id: &LayerId) -> StorageResult<Option<Layer>>;
    async fn save_layer(&self, layer: &Layer) -> StorageResult<()>;
    async fn delete_layer(&self, id: &LayerId) -> StorageResult<()>;
    async fn load_variant(
        &self,
        layer_id: &LayerId,
        variant_id: &LayerSnapshotId,
    ) -> StorageResult<Option<LayerSnapshot>>;
}

// endregion: --- Trait

// region: --- LayerRepoLive

#[derive(Clone)]
pub struct LayerRepoLive {
    db: DatabaseConnection,
}

impl LayerRepoLive {
    pub fn new(db: DatabaseConnection) -> Self {
        Self { db }
    }

    pub async fn init_schema(&self) -> StorageResult<()> {
        let backend = self.db.get_database_backend();
        let schema = Schema::new(backend);

        let mut layers = schema.create_table_from_entity(entity::layer::Entity);
        layers.if_not_exists();
        self.db.execute(backend.build(&layers)).await?;

        let mut variants = schema.create_table_from_entity(entity::layer_snapshot::Entity);
        variants.if_not_exists();
        self.db.execute(backend.build(&variants)).await?;

        Ok(())
    }

    // region: --- JSON helpers

    fn variant_state_to_json(variant: &LayerSnapshot) -> StorageResult<String> {
        let state = VariantState {
            module_refs: &variant.module_refs,
            block_refs: &variant.block_refs,
            overrides: &variant.overrides,
            enabled: variant.enabled,
        };
        serde_json::to_string(&state)
            .map_err(|e| StorageError::Data(format!("failed to serialize layer snapshot: {e}")))
    }

    fn variant_state_from_json(json: &str) -> StorageResult<VariantStateOwned> {
        serde_json::from_str(json)
            .map_err(|e| StorageError::Data(format!("failed to parse layer snapshot json: {e}")))
    }

    fn metadata_to_json(metadata: &Metadata) -> StorageResult<String> {
        serde_json::to_string(metadata)
            .map_err(|e| StorageError::Data(format!("failed to serialize metadata: {e}")))
    }

    fn metadata_from_json(json: &str) -> StorageResult<Metadata> {
        serde_json::from_str(json)
            .map_err(|e| StorageError::Data(format!("failed to parse metadata json: {e}")))
    }

    // endregion: --- JSON helpers

    // region: --- Assembly

    fn variant_from_model(model: &entity::layer_snapshot::Model) -> StorageResult<LayerSnapshot> {
        let state = Self::variant_state_from_json(&model.state_json)?;
        let metadata = Self::metadata_from_json(&model.metadata_json)?;
        Ok(LayerSnapshot {
            id: model.variant_id_branded(),
            name: model.name.clone(),
            module_refs: state.module_refs,
            block_refs: state.block_refs,
            overrides: state.overrides,
            enabled: state.enabled,
            metadata,
        })
    }

    async fn assemble_layer(&self, model: &entity::layer::Model) -> StorageResult<Layer> {
        let variant_models = entity::layer_snapshot::Entity::find()
            .filter(entity::layer_snapshot::Column::LayerId.eq(model.id.clone()))
            .order_by_asc(entity::layer_snapshot::Column::Id)
            .all(&self.db)
            .await?;

        let mut variants = Vec::with_capacity(variant_models.len());
        for vm in &variant_models {
            variants.push(Self::variant_from_model(vm)?);
        }

        let metadata = Self::metadata_from_json(&model.metadata_json)?;

        Ok(Layer {
            id: model.layer_id_branded(),
            name: model.name.clone(),
            default_variant_id: model.default_variant_id_branded(),
            variants,
            metadata,
        })
    }

    // endregion: --- Assembly
}

// endregion: --- LayerRepoLive

// region: --- Serialization types

#[derive(serde::Serialize)]
struct VariantState<'a> {
    module_refs: &'a [ModuleRef],
    block_refs: &'a [BlockRef],
    overrides: &'a [Override],
    enabled: bool,
}

#[derive(serde::Deserialize)]
struct VariantStateOwned {
    module_refs: Vec<ModuleRef>,
    block_refs: Vec<BlockRef>,
    overrides: Vec<Override>,
    enabled: bool,
}

// endregion: --- Serialization types

// region: --- Trait impl

#[async_trait::async_trait]
impl LayerRepo for LayerRepoLive {
    async fn list_layers(&self) -> StorageResult<Vec<Layer>> {
        let models = entity::layer::Entity::find()
            .order_by_asc(entity::layer::Column::Id)
            .all(&self.db)
            .await?;

        let mut out = Vec::with_capacity(models.len());
        for model in &models {
            out.push(self.assemble_layer(model).await?);
        }
        Ok(out)
    }

    async fn load_layer(&self, id: &LayerId) -> StorageResult<Option<Layer>> {
        let model = entity::layer::Entity::find_by_id(id.as_str().to_string())
            .one(&self.db)
            .await?;
        match model {
            Some(ref m) => Ok(Some(self.assemble_layer(m).await?)),
            None => Ok(None),
        }
    }

    async fn save_layer(&self, layer: &Layer) -> StorageResult<()> {
        // Delete existing (cascade deletes variants)
        entity::layer::Entity::delete_by_id(layer.id.as_str().to_string())
            .exec(&self.db)
            .await
            .ok();

        entity::layer::Entity::insert(entity::layer::ActiveModel {
            id: Set(layer.id.as_str().to_string()),
            name: Set(layer.name.clone()),
            default_variant_id: Set(layer.default_variant_id.as_str().to_string()),
            metadata_json: Set(Self::metadata_to_json(&layer.metadata)?),
        })
        .exec(&self.db)
        .await?;

        for variant in &layer.variants {
            entity::layer_snapshot::Entity::insert(entity::layer_snapshot::ActiveModel {
                id: Set(variant.id.as_str().to_string()),
                layer_id: Set(layer.id.as_str().to_string()),
                name: Set(variant.name.clone()),
                state_json: Set(Self::variant_state_to_json(variant)?),
                metadata_json: Set(Self::metadata_to_json(&variant.metadata)?),
            })
            .exec(&self.db)
            .await?;
        }

        Ok(())
    }

    async fn delete_layer(&self, id: &LayerId) -> StorageResult<()> {
        entity::layer::Entity::delete_by_id(id.as_str().to_string())
            .exec(&self.db)
            .await?;
        Ok(())
    }

    async fn load_variant(
        &self,
        layer_id: &LayerId,
        variant_id: &LayerSnapshotId,
    ) -> StorageResult<Option<LayerSnapshot>> {
        let model = entity::layer_snapshot::Entity::find_by_id(variant_id.as_str().to_string())
            .filter(entity::layer_snapshot::Column::LayerId.eq(layer_id.as_str().to_string()))
            .one(&self.db)
            .await?;

        match model {
            Some(ref m) => Ok(Some(Self::variant_from_model(m)?)),
            None => Ok(None),
        }
    }
}

// endregion: --- Trait impl

// region: --- Tests

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Database;
    use signal_proto::layer::ModuleRef;

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    async fn test_repo() -> Result<LayerRepoLive> {
        let db = Database::connect("sqlite::memory:").await?;
        let repo = LayerRepoLive::new(db);
        repo.init_schema().await?;
        Ok(repo)
    }

    fn sample_layer() -> Layer {
        let v1 = LayerSnapshot::new("v1", "Clean").with_module(ModuleRef::new("mod-drive"));
        let v2 = LayerSnapshot::new("v2", "Heavy")
            .with_module(ModuleRef::new("mod-drive").with_variant("push"));
        let mut layer = Layer::new("layer-1", "Guitar Layer", v1);
        layer.add_variant(v2);
        layer
    }

    #[tokio::test]
    async fn save_load_round_trip() -> Result<()> {
        let repo = test_repo().await?;
        let layer = sample_layer();

        repo.save_layer(&layer).await?;
        let loaded = repo.load_layer(&LayerId::new("layer-1")).await?;

        let loaded = loaded.expect("should find layer");
        assert_eq!(loaded.name, "Guitar Layer");
        assert_eq!(loaded.variants.len(), 2);
        assert_eq!(loaded.default_variant_id.as_str(), "v1");
        Ok(())
    }

    #[tokio::test]
    async fn list_layers_returns_all() -> Result<()> {
        let repo = test_repo().await?;

        let l1 = Layer::new("l1", "Layer 1", LayerSnapshot::new("v1", "Default"));
        let l2 = Layer::new("l2", "Layer 2", LayerSnapshot::new("v2", "Default"));
        repo.save_layer(&l1).await?;
        repo.save_layer(&l2).await?;

        let layers = repo.list_layers().await?;
        assert_eq!(layers.len(), 2);
        Ok(())
    }

    #[tokio::test]
    async fn load_missing_returns_none() -> Result<()> {
        let repo = test_repo().await?;
        let loaded = repo.load_layer(&LayerId::new("nonexistent")).await?;
        assert!(loaded.is_none());
        Ok(())
    }

    #[tokio::test]
    async fn delete_layer_removes_it() -> Result<()> {
        let repo = test_repo().await?;
        let layer = sample_layer();
        repo.save_layer(&layer).await?;

        repo.delete_layer(&LayerId::new("layer-1")).await?;
        let loaded = repo.load_layer(&LayerId::new("layer-1")).await?;
        assert!(loaded.is_none());
        Ok(())
    }

    #[tokio::test]
    async fn load_variant_by_id() -> Result<()> {
        let repo = test_repo().await?;
        let layer = sample_layer();
        repo.save_layer(&layer).await?;

        let variant = repo
            .load_variant(&LayerId::new("layer-1"), &LayerSnapshotId::new("v2"))
            .await?;
        let variant = variant.expect("should find variant");
        assert_eq!(variant.name, "Heavy");
        assert_eq!(variant.module_refs.len(), 1);
        Ok(())
    }

    #[tokio::test]
    async fn load_variant_missing_returns_none() -> Result<()> {
        let repo = test_repo().await?;
        let layer = sample_layer();
        repo.save_layer(&layer).await?;

        let variant = repo
            .load_variant(
                &LayerId::new("layer-1"),
                &LayerSnapshotId::new("nonexistent"),
            )
            .await?;
        assert!(variant.is_none());
        Ok(())
    }

    #[tokio::test]
    async fn save_overwrites_existing() -> Result<()> {
        let repo = test_repo().await?;

        let v1 = LayerSnapshot::new("v1", "Original");
        let layer = Layer::new("layer-1", "Layer", v1);
        repo.save_layer(&layer).await?;

        let v1 = LayerSnapshot::new("v1", "Updated");
        let layer = Layer::new("layer-1", "Layer Renamed", v1);
        repo.save_layer(&layer).await?;

        let loaded = repo.load_layer(&LayerId::new("layer-1")).await?.unwrap();
        assert_eq!(loaded.name, "Layer Renamed");
        assert_eq!(loaded.variants.len(), 1);
        assert_eq!(loaded.variants[0].name, "Updated");
        Ok(())
    }

    #[tokio::test]
    async fn metadata_round_trip() -> Result<()> {
        let repo = test_repo().await?;

        let v1 = LayerSnapshot::new("v1", "Default").with_metadata(
            Metadata::new()
                .with_tag("guitar")
                .with_description("Clean tone"),
        );
        let layer = Layer::new("layer-1", "Guitar", v1)
            .with_metadata(Metadata::new().with_tag("main").with_notes("Primary layer"));
        repo.save_layer(&layer).await?;

        let loaded = repo.load_layer(&LayerId::new("layer-1")).await?.unwrap();
        assert!(loaded.metadata.tags.contains("main"));
        assert_eq!(loaded.metadata.notes.as_deref(), Some("Primary layer"));

        let v = &loaded.variants[0];
        assert!(v.metadata.tags.contains("guitar"));
        assert_eq!(v.metadata.description.as_deref(), Some("Clean tone"));
        Ok(())
    }
}

// endregion: --- Tests
