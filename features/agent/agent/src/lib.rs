//! Facade for the `agent` feature. Wire types are always
//! re-exported; backends + transport adapters are feature-gated.

pub use agent_proto::*;

/// CRDT source of truth + SeaORM persistence. Construct one
/// `CrdtDoc` per collaboration boundary and hand a `AgentRepoLoro`
/// to the vox dispatcher.
#[cfg(feature = "server")]
pub mod server {
    pub use agent_crdt::{AgentEntity, AgentRepoLoro};
    pub use agent_db::{AgentMigrator, SeaOrmPersistence};
    pub use crdt::{CrdtDoc, Persistence};
}

#[cfg(feature = "server-axum")]
pub use architect::axum_ws;
