//! Facade for the `agent` feature. Wire types are always
//! re-exported; backends + transport adapters are feature-gated.

pub use agent_proto::*;

/// CRDT source of truth + SeaORM persistence. Construct one
/// `CrdtDoc` per collaboration boundary and hand the per-entity
/// RepoLoro newtype to the vox dispatcher.
#[cfg(feature = "server")]
pub mod server {
    pub use agent_crdt::{
        AgentConversationEntity, AgentConversationRepoLoro, AgentLogLineEntity,
        AgentLogLineRepoLoro, AgentRunEntity, AgentRunRepoLoro, ConversationMessageEntity,
        ConversationMessageRepoLoro, GitRepoConnectionEntity, GitRepoConnectionRepoLoro,
        ToolCallEntity, ToolCallRepoLoro,
    };
    pub use agent_db::{AgentMigrator, SeaOrmPersistence};
    pub use crdt::{CrdtDoc, Persistence};
}

#[cfg(feature = "server")]
pub mod live_update;
#[cfg(feature = "server")]
pub use live_update::{LiveUpdateBus, RunSubscription, WorkspaceSubscription};

#[cfg(feature = "server")]
pub mod service;
#[cfg(feature = "server")]
pub use service::{AgentServiceImpl, HermesOnlyRouter, PluginRouter};

#[cfg(feature = "server-axum")]
pub use architect::axum_ws;
