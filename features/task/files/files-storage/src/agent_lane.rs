//! The agent lane's backend: the coordinator's side of the storage-agent
//! protocol. An agent — whichever of the three hostings it is — announces
//! here, heartbeats here, reads its outstanding directives here,
//! subscribes to new ones here, and reports outcomes here.
//!
//! The in-server hosting is not a special case of this protocol; it is
//! the same protocol with the round trip elided (see
//! [`StorageCore::register_local_agent`](crate::StorageCore::register_local_agent)),
//! which is exactly why a desktop or standalone agent can be added later
//! without the coordinator learning a second vocabulary.

use std::sync::Arc;

use files_storage_proto::{
    AgentAnnouncement, AgentDirective, AgentInfo, DirectiveOutcome, StorageAgentService,
    StorageError, VolumeHealth,
};
use uuid::Uuid;

use crate::blocking::blocking;
use crate::core::StorageCore;

#[derive(Clone, architect::HasDispatcher)]
pub struct StorageAgentBackend {
    core: Arc<StorageCore>,
}

impl std::fmt::Debug for StorageAgentBackend {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StorageAgentBackend")
            .finish_non_exhaustive()
    }
}

impl StorageAgentBackend {
    #[must_use]
    pub fn new(core: Arc<StorageCore>) -> Self {
        Self { core }
    }

    #[must_use]
    pub fn core(&self) -> &Arc<StorageCore> {
        &self.core
    }
}

impl StorageAgentService for StorageAgentBackend {
    async fn announce(&self, announcement: AgentAnnouncement) -> Result<AgentInfo, StorageError> {
        let core = self.core.clone();
        blocking(move || core.announce(announcement)).await
    }

    async fn heartbeat(
        &self,
        agent_id: Uuid,
        volumes: Vec<VolumeHealth>,
    ) -> Result<AgentInfo, StorageError> {
        let core = self.core.clone();
        blocking(move || core.heartbeat(agent_id, volumes)).await
    }

    async fn pending_directives(
        &self,
        agent_id: Uuid,
    ) -> Result<Vec<AgentDirective>, StorageError> {
        Ok(self.core.pending_directives(agent_id))
    }

    async fn complete_directive(
        &self,
        agent_id: Uuid,
        directive_id: Uuid,
        outcome: DirectiveOutcome,
    ) -> Result<(), StorageError> {
        let core = self.core.clone();
        blocking(move || core.complete_directive(agent_id, directive_id, outcome)).await
    }
}

/// The `#[subscribe]` backend contract for the directive stream. One hub
/// for every agent — directives carry their `agent_id` and each agent
/// keeps its own.
impl files_storage_proto::service::agent::StorageAgentServiceStreamSource for StorageAgentBackend {
    fn directives_hub(&self) -> &architect::PubSub<AgentDirective> {
        self.core.directives_hub()
    }
}
