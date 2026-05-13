//! Facade for the `fitness` feature. Wire types are always
//! re-exported; backends + transport adapters are feature-gated.

pub use fitness_proto::*;

/// CRDT source of truth + SeaORM persistence. Construct one
/// `CrdtDoc` per collaboration boundary and hand a `FitnessRepoLoro`
/// to the vox dispatcher.
#[cfg(feature = "server")]
pub mod server {
    pub use crdt::{CrdtDoc, Persistence};
    pub use fitness_crdt::{FitnessEntity, FitnessRepoLoro};
    pub use fitness_db::{FitnessMigrator, SeaOrmPersistence};
}

#[cfg(feature = "server-axum")]
pub use architect::axum_ws;
