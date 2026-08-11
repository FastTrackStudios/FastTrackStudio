//! The operator lane's backend. Mounted on the server lane, never on an
//! org router: every method here is a deployment-wide act (registering a
//! location, approving an agent, admitting an org), and the registry it
//! speaks for is shared by every org in the deployment.

use std::sync::Arc;

use files_storage_proto::{
    AgentInfo, GrantSpec, StorageAdminService, StorageError, StorageGrantInfo, StorageLocationInfo,
};
use uuid::Uuid;

use crate::blocking::blocking;
use crate::core::StorageCore;

#[derive(Clone, architect::HasDispatcher)]
pub struct StorageAdminBackend {
    core: Arc<StorageCore>,
}

impl std::fmt::Debug for StorageAdminBackend {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StorageAdminBackend")
            .finish_non_exhaustive()
    }
}

impl StorageAdminBackend {
    #[must_use]
    pub fn new(core: Arc<StorageCore>) -> Self {
        Self { core }
    }

    #[must_use]
    pub fn core(&self) -> &Arc<StorageCore> {
        &self.core
    }
}

impl StorageAdminService for StorageAdminBackend {
    async fn list_agents(&self) -> Result<Vec<AgentInfo>, StorageError> {
        Ok(self.core.list_agents())
    }

    async fn approve_agent(
        &self,
        agent_id: Uuid,
        approved: bool,
    ) -> Result<AgentInfo, StorageError> {
        let core = self.core.clone();
        blocking(move || core.approve_agent(agent_id, approved)).await
    }

    async fn register_location(
        &self,
        agent_id: Uuid,
        volume_key: String,
    ) -> Result<StorageLocationInfo, StorageError> {
        let core = self.core.clone();
        blocking(move || core.register_location(agent_id, &volume_key)).await
    }

    async fn list_locations(&self) -> Result<Vec<StorageLocationInfo>, StorageError> {
        Ok(self.core.list_locations())
    }

    async fn issue_grant(&self, spec: GrantSpec) -> Result<StorageGrantInfo, StorageError> {
        let core = self.core.clone();
        blocking(move || core.issue_grant(spec)).await
    }

    async fn revoke_grant(&self, grant_id: Uuid) -> Result<(), StorageError> {
        let core = self.core.clone();
        blocking(move || core.revoke_grant(grant_id)).await
    }

    async fn list_grants(
        &self,
        org: Option<String>,
    ) -> Result<Vec<StorageGrantInfo>, StorageError> {
        Ok(self.core.list_grants(org.as_deref()))
    }
}
