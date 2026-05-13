//! Facade for the `project` feature. Wire types are always
//! re-exported; backends + transport adapters are feature-gated.

pub use project_proto::*;

/// CRDT source of truth + SeaORM persistence. Construct one
/// `CrdtDoc` per collaboration boundary and hand a `ProjectRepoLoro`
/// to the vox dispatcher.
#[cfg(feature = "server")]
pub mod server {
    pub use crdt::{CrdtDoc, Persistence};
    pub use project_crdt::{ProjectEntity, ProjectRepoLoro};
    pub use project_db::{ProjectMigrator, SeaOrmPersistence};
}

#[cfg(feature = "server-axum")]
pub use architect::axum_ws;
