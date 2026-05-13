//! Facade for the `inventory` feature. Wire types are always
//! re-exported; backends + transport adapters are feature-gated.

pub use inventory_proto::*;

/// CRDT source of truth + SeaORM persistence. Construct one
/// `CrdtDoc` per collaboration boundary and hand a `InventoryRepoLoro`
/// to the vox dispatcher.
#[cfg(feature = "server")]
pub mod server {
    pub use crdt::{CrdtDoc, Persistence};
    pub use inventory_crdt::{InventoryEntity, InventoryRepoLoro};
    pub use inventory_db::{InventoryMigrator, SeaOrmPersistence};
}

#[cfg(feature = "server-axum")]
pub use architect::axum_ws;
