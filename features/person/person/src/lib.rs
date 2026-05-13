//! Facade for the `person` feature. Wire types are always
//! re-exported; backends + transport adapters are feature-gated.

pub use person_proto::*;

/// CRDT source of truth + SeaORM persistence. Construct one
/// `CrdtDoc` per collaboration boundary and hand a `PersonRepoLoro`
/// to the vox dispatcher.
#[cfg(feature = "server")]
pub mod server {
    pub use crdt::{CrdtDoc, Persistence};
    pub use person_crdt::{PersonEntity, PersonRepoLoro};
    pub use person_db::{PersonMigrator, SeaOrmPersistence};
}

#[cfg(feature = "server-axum")]
pub use architect::axum_ws;
