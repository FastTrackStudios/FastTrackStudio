//! Live service implementation for signal2.
//!
//! Maps the `BlockService` RPC trait onto the `CollectionRepo` storage layer.
//!
//! # Collection / Variant Mapping
//!
//! This service operates on *collection* and *variant* concepts:
//! - **Block collections** (`Preset`) group related block-parameter variants (`Snapshot`).
//! - **Module collections** (`ModulePreset`) group multi-block composition variants (`ModuleSnapshot`).
//!
//! When a variant is loaded (via `load_preset` / `load_preset_snapshot`), the
//! service applies a **side-effect**: the resolved block state is persisted as
//! the current active block.  This deterministic "load = apply" contract ensures
//! the active block always reflects the last loaded variant.

use roam::Context;
use signal_proto::{
    Block, BlockService, BlockType, ModulePreset, ModulePresetId, ModuleSnapshot, ModuleSnapshotId,
    Preset, PresetId, Snapshot, SnapshotId,
};
use signal_storage::{CollectionRepo, CollectionRepoLive, DatabaseConnection};
use std::sync::Arc;

// region: --- SignalLive

/// Live service that bridges `BlockService` (RPC surface) to `CollectionRepo` (storage).
///
/// Generic over `R: CollectionRepo` so tests can inject an in-memory repo.
/// The default type parameter (`CollectionRepoLive`) enables the common case
/// without specifying the concrete repo type.
pub struct SignalLive<R = CollectionRepoLive>
where
    R: CollectionRepo,
{
    repo: Arc<R>,
}

impl<R> SignalLive<R>
where
    R: CollectionRepo,
{
    pub fn new(repo: Arc<R>) -> Self {
        Self { repo }
    }
}

impl SignalLive<CollectionRepoLive> {
    pub fn from_db(db: DatabaseConnection) -> Self {
        Self::new(Arc::new(CollectionRepoLive::new(db)))
    }
}

// endregion: --- SignalLive

// region: --- BlockService impl

impl<R> BlockService for SignalLive<R>
where
    R: CollectionRepo,
{
    /// Load the current active block state for a given block type.
    /// Returns `Block::default()` when no state has been persisted yet.
    async fn get_block(&self, _cx: &Context, block_type: BlockType) -> Block {
        self.repo
            .load_block_state(block_type)
            .await
            .ok()
            .flatten()
            .unwrap_or_default()
    }

    /// Persist a new block state and return it.
    async fn set_block(&self, _cx: &Context, block_type: BlockType, block: Block) -> Block {
        let _ = self.repo.save_block_state(block_type, block.clone()).await;
        block
    }

    /// List all block collections (presets) for a given block type.
    async fn list_presets(&self, _cx: &Context, block_type: BlockType) -> Vec<Preset> {
        self.repo
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
            .repo
            .load_block_default_variant(block_type, &preset_id)
            .await
            .ok()
            .flatten();
        if let Some(snapshot) = snapshot.as_ref() {
            let _ = self
                .repo
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
            .repo
            .load_block_variant(block_type, &preset_id, &snapshot_id)
            .await
            .ok()
            .flatten();
        if let Some(snapshot) = snapshot.as_ref() {
            let _ = self
                .repo
                .save_block_state(block_type, snapshot.block())
                .await;
        }
        snapshot
    }

    /// List all module collections.
    async fn list_module_presets(&self, _cx: &Context) -> Vec<ModulePreset> {
        self.repo
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
        self.repo
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
        self.repo
            .load_module_variant(&preset_id, &snapshot_id)
            .await
            .ok()
            .flatten()
    }
}

// endregion: --- BlockService impl

#[cfg(test)]
mod tests {
    use super::*;
    use signal_storage::{
        default_seed_block_collections, default_seed_module_collections, CollectionRepoLive,
        Database,
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

    async fn seeded_service() -> Result<SignalLive<CollectionRepoLive>> {
        let db = Database::connect("sqlite::memory:").await?;
        let repo = CollectionRepoLive::new(db);
        repo.init_schema().await?;
        repo.reseed_defaults(
            &default_seed_block_collections(),
            &default_seed_module_collections(),
        )
        .await?;
        Ok(SignalLive::new(Arc::new(repo)))
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
        let repo = CollectionRepoLive::new(db);
        repo.init_schema().await?;
        let svc = SignalLive::new(Arc::new(repo));
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
        assert_eq!(amp_collections.len(), 2);
        assert_eq!(drive_collections.len(), 2);
        Ok(())
    }

    #[tokio::test]
    async fn test_live_list_collections_empty_repo() -> Result<()> {
        // -- Setup & Fixtures
        let db = Database::connect("sqlite::memory:").await?;
        let repo = CollectionRepoLive::new(db);
        repo.init_schema().await?;
        let svc = SignalLive::new(Arc::new(repo));
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
        let preset_id = PresetId::from("amp-clean");

        // -- Exec: load the default variant (triggers side-effect)
        let snapshot = svc.load_preset(&cx, BlockType::Amp, preset_id).await;

        // -- Check: variant returned
        assert!(snapshot.is_some());
        let snapshot = snapshot.unwrap();
        assert_eq!(snapshot.id(), &SnapshotId::from("amp-clean-default"));

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
        let preset_id = PresetId::from("amp-clean");
        let snapshot_id = SnapshotId::from("amp-clean-bright");

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
        let result = svc
            .load_preset(&cx, BlockType::Amp, PresetId::from("does-not-exist"))
            .await;

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
                PresetId::from("amp-clean"),
                SnapshotId::from("does-not-exist"),
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
        assert_eq!(module_collections.len(), 1);
        assert_eq!(module_collections[0].name(), "Triple Drive Stack");
        Ok(())
    }

    #[tokio::test]
    async fn test_live_load_module_default_variant() -> Result<()> {
        // -- Setup & Fixtures
        let svc = seeded_service().await?;
        let cx = test_context();
        let preset_id = ModulePresetId::from("drive-triple-stack");

        // -- Exec
        let snapshot = svc.load_module_preset(&cx, preset_id).await;

        // -- Check
        assert!(snapshot.is_some());
        let snapshot = snapshot.unwrap();
        assert_eq!(
            snapshot.id(),
            &ModuleSnapshotId::from("drive-triple-stack-default")
        );
        assert_eq!(snapshot.module().blocks().len(), 3);
        Ok(())
    }

    #[tokio::test]
    async fn test_live_load_module_specific_variant() -> Result<()> {
        // -- Setup & Fixtures
        let svc = seeded_service().await?;
        let cx = test_context();
        let preset_id = ModulePresetId::from("drive-triple-stack");
        let snapshot_id = ModuleSnapshotId::from("drive-triple-stack-push");

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
        let result = svc
            .load_module_preset(&cx, ModulePresetId::from("does-not-exist"))
            .await;

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

        // -- Exec: load "bright" variant
        let bright = svc
            .load_preset_snapshot(
                &cx,
                BlockType::Amp,
                PresetId::from("amp-clean"),
                SnapshotId::from("amp-clean-bright"),
            )
            .await
            .unwrap();

        let block_after_bright = svc.get_block(&cx, BlockType::Amp).await;
        assert_eq!(block_after_bright, bright.block());

        // -- Exec: load "warm" variant (should overwrite)
        let warm = svc
            .load_preset_snapshot(
                &cx,
                BlockType::Amp,
                PresetId::from("amp-clean"),
                SnapshotId::from("amp-clean-warm"),
            )
            .await
            .unwrap();

        // -- Check: current block reflects the most recently loaded variant
        let block_after_warm = svc.get_block(&cx, BlockType::Amp).await;
        assert_eq!(block_after_warm, warm.block());
        assert_ne!(block_after_warm, bright.block());
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
            .load_preset(&cx, BlockType::Drive, PresetId::from("drive-level"))
            .await;

        // -- Check: amp block was not affected by loading a drive variant
        let amp_after = svc.get_block(&cx, BlockType::Amp).await;
        assert_eq!(amp_before, amp_after);
        Ok(())
    }

    // endregion: --- Resolver determinism
}
