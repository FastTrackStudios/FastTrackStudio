//! Public Signal crate.
//!
//! Re-exports protocol types and controller APIs for consumers.

pub use signal_controller::SignalController;
pub use signal_live::SignalLive;
pub use signal_proto::*;
pub use signal_storage::{
    default_seed_block_collections, default_seed_module_collections, CollectionRepo,
    CollectionRepoLive, Database, DatabaseConnection, DbErr, StorageError, StorageResult,
};
use std::sync::Arc;

pub async fn bootstrap_in_memory_controller_async() -> Result<SignalController, StorageError> {
    let db = Database::connect("sqlite::memory:").await?;
    let repo = CollectionRepoLive::new(db);
    repo.init_schema().await?;
    repo.reseed_defaults(
        &default_seed_block_collections(),
        &default_seed_module_collections(),
    )
    .await?;

    let service = Arc::new(SignalLive::new(Arc::new(repo)));
    Ok(SignalController::new(service))
}

pub fn bootstrap_in_memory_controller() -> SignalController {
    let runtime = tokio::runtime::Runtime::new().expect("failed to build tokio runtime");
    let db = runtime.block_on(async {
        let db = Database::connect("sqlite::memory:")
            .await
            .expect("failed to connect in-memory sqlite");
        let repo = CollectionRepoLive::new(db.clone());
        repo.init_schema()
            .await
            .expect("failed to initialize signal schema");
        repo.reseed_defaults(
            &default_seed_block_collections(),
            &default_seed_module_collections(),
        )
        .await
        .expect("failed to reseed signal collections");
        db
    });
    let service = Arc::new(SignalLive::from_db(db));
    SignalController::new(service)
}
