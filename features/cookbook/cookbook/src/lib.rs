//! Facade for the `cookbook` feature. Wire types are always
//! re-exported; backends + transport adapters are feature-gated.

pub use cookbook_proto::*;

/// CRDT source of truth + SeaORM persistence. Construct one
/// `CrdtDoc` per collaboration boundary and hand a `CookbookRepoLoro`
/// to the vox dispatcher.
#[cfg(feature = "server")]
pub mod server {
    pub use cookbook_crdt::{CookbookEntity, CookbookRepoLoro};
    pub use cookbook_db::{CookbookMigrator, SeaOrmPersistence};
    pub use crdt::{CrdtDoc, Persistence};
}

#[cfg(feature = "server-axum")]
pub use architect::axum_ws;
