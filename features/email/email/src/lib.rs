//! Facade for the `email` feature. Wire types are always
//! re-exported; backends + transport adapters are feature-gated.

pub use email_proto::*;

/// CRDT source of truth + SeaORM persistence. Construct one
/// `CrdtDoc` per collaboration boundary and hand a `EmailRepoLoro`
/// to the vox dispatcher.
#[cfg(feature = "server")]
pub mod server {
    pub use crdt::{CrdtDoc, Persistence};
    pub use email_crdt::{EmailEntity, EmailRepoLoro};
    pub use email_db::{EmailMigrator, SeaOrmPersistence};
}

#[cfg(feature = "server-axum")]
pub use architect::axum_ws;
