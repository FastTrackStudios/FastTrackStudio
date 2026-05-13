//! Facade for the `foo` feature. Wire types are always
//! re-exported; backends + transport adapters are feature-gated.

pub use foo_proto::*;

/// CRDT source of truth + SeaORM persistence. Construct one
/// `CrdtDoc` per collaboration boundary and hand a `FooRepoLoro`
/// to the vox dispatcher.
#[cfg(feature = "server")]
pub mod server {
    pub use crdt::{CrdtDoc, Persistence};
    pub use foo_crdt::{FooEntity, FooRepoLoro};
    pub use foo_db::{FooMigrator, SeaOrmPersistence};
}

#[cfg(feature = "server-axum")]
pub use architect::axum_ws;
