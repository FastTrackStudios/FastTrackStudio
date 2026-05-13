//! Facade for the `knowledge` feature. Wire types are always
//! re-exported; backends + transport adapters are feature-gated.

pub use knowledge_proto::*;

/// CRDT source of truth + SeaORM persistence. Construct one
/// `CrdtDoc` per collaboration boundary and hand a
/// `KnowledgeXRepoLoro` to the vox dispatcher.
#[cfg(feature = "server")]
pub mod server {
    pub use crdt::{CrdtDoc, Persistence};
    pub use knowledge_crdt::{
        BaseEntity, BaseRepoLoro, BlockEntity, BlockRepoLoro, FolderEntity, FolderRepoLoro,
        KnowledgeTagEntity, KnowledgeTagRepoLoro, PageEntity, PageRepoLoro, VaultEntity,
        VaultRepoLoro,
    };
    pub use knowledge_db::{KnowledgeMigrator, SeaOrmPersistence};
}

#[cfg(feature = "server-axum")]
pub use architect::axum_ws;
