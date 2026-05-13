//! Facade for the `caldav` feature. Wire types are always
//! re-exported; backends + transport adapters are feature-gated.

pub use caldav_proto::*;

/// CRDT source of truth + SeaORM persistence. Construct one
/// `CrdtDoc` per collaboration boundary and hand a `CaldavRepoLoro`
/// to the vox dispatcher.
#[cfg(feature = "server")]
pub mod server {
    pub use caldav_crdt::{CaldavEntity, CaldavRepoLoro};
    pub use caldav_db::{CaldavMigrator, SeaOrmPersistence};
    pub use crdt::{CrdtDoc, Persistence};
}

#[cfg(feature = "server-axum")]
pub use architect::axum_ws;
