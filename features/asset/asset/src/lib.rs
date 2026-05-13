//! Facade for the `asset` feature. Wire types are always
//! re-exported; backends + transport adapters are feature-gated.

pub use asset_proto::*;

/// CRDT source of truth + SeaORM persistence. Construct one
/// `CrdtDoc` per collaboration boundary and hand a `AssetRepoLoro`
/// to the vox dispatcher.
#[cfg(feature = "server")]
pub mod server {
    pub use asset_crdt::{AssetEntity, AssetRepoLoro};
    pub use asset_db::{AssetMigrator, SeaOrmPersistence};
    pub use crdt::{CrdtDoc, Persistence};
}

#[cfg(feature = "server-axum")]
pub use architect::axum_ws;
