//! Facade for the `location` feature. Wire types are always
//! re-exported; backends + transport adapters are feature-gated.

pub use location_proto::*;

/// CRDT source of truth + SeaORM persistence. Construct one
/// `CrdtDoc` per collaboration boundary and hand a `LocationRepoLoro`
/// to the vox dispatcher.
#[cfg(feature = "server")]
pub mod server {
    pub use crdt::{CrdtDoc, Persistence};
    pub use location_crdt::{LocationEntity, LocationRepoLoro};
    pub use location_db::{LocationMigrator, SeaOrmPersistence};
}

#[cfg(feature = "server-axum")]
pub use architect::axum_ws;
