//! Facade for the `conference` feature. Wire types are always
//! re-exported; backends + transport adapters are feature-gated.

pub use conference_proto::*;

/// CRDT source of truth + SeaORM persistence. Construct one
/// `CrdtDoc` per collaboration boundary and hand a `ConferenceRepoLoro`
/// to the vox dispatcher.
#[cfg(feature = "server")]
pub mod server {
    pub use conference_crdt::{ConferenceEntity, ConferenceRepoLoro};
    pub use conference_db::{ConferenceMigrator, SeaOrmPersistence};
    pub use crdt::{CrdtDoc, Persistence};
}

#[cfg(feature = "server-axum")]
pub use architect::axum_ws;
