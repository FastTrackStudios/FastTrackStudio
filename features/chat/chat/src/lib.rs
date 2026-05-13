//! Facade for the `chat` feature. Wire types are always
//! re-exported; backends + transport adapters are feature-gated.

pub use chat_proto::*;

/// CRDT source of truth + SeaORM persistence. Construct one
/// `CrdtDoc` per collaboration boundary and hand a `ChatRepoLoro`
/// to the vox dispatcher.
#[cfg(feature = "server")]
pub mod server {
    pub use chat_crdt::{ChatEntity, ChatRepoLoro};
    pub use chat_db::{ChatMigrator, SeaOrmPersistence};
    pub use crdt::{CrdtDoc, Persistence};
}

#[cfg(feature = "server-axum")]
pub use architect::axum_ws;
