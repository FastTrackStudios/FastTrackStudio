//! Facade for the `bar` feature. Wire types are always
//! re-exported; backends + transport adapters are feature-gated.

pub use bar_proto::*;

/// CRDT source of truth + SeaORM persistence. Construct one
/// `CrdtDoc` per collaboration boundary and hand a `BarRepoLoro`
/// to the vox dispatcher.
#[cfg(feature = "server")]
pub mod server {
    pub use bar_crdt::{BarEntity, BarRepoLoro};
    pub use bar_db::{BarMigrator, SeaOrmPersistence};
    pub use crdt::{CrdtDoc, Persistence};
}

#[cfg(feature = "server-axum")]
pub use architect::axum_ws;
