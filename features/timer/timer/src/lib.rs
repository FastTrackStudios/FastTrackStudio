//! Facade for the `timer` feature. Wire types are always
//! re-exported; backends + transport adapters are feature-gated.

pub use timer_proto::*;

/// CRDT source of truth + SeaORM persistence. Construct one
/// `CrdtDoc` per collaboration boundary and hand a `TimerRepoLoro`
/// to the vox dispatcher.
#[cfg(feature = "server")]
pub mod server {
    pub use crdt::{CrdtDoc, Persistence};
    pub use timer_crdt::{TimerEntity, TimerRepoLoro};
    pub use timer_db::{SeaOrmPersistence, TimerMigrator};
}

#[cfg(feature = "server-axum")]
pub use architect::axum_ws;
