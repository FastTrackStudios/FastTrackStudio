//! Facade for the `threads` feature. Wire types are always
//! re-exported; backends + transport adapters are feature-gated.

pub use threads_proto::*;

/// CRDT source of truth + SeaORM persistence. Construct one
/// `CrdtDoc` per collaboration boundary and hand a `ThreadsRepoLoro`
/// to the vox dispatcher.
#[cfg(feature = "server")]
pub mod server {
    pub use crdt::{CrdtDoc, Persistence};
    pub use threads_crdt::{ThreadsEntity, ThreadsRepoLoro};
    pub use threads_db::{SeaOrmPersistence, ThreadsMigrator};
}

#[cfg(feature = "server-axum")]
pub use architect::axum_ws;
