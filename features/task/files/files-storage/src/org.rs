//! The org lane's backend — one per org, mounted on that org's router.
//!
//! The org's identity is a field, never an argument: a caller on this
//! lane cannot name another org any more than a `files` caller can name a
//! path outside its own area. Everything it can see is filtered through
//! that org's grants.

use std::sync::Arc;

use files_storage_proto::{
    GrantUsage, RootPlacement, StorageError, StorageEvent, StorageGrantInfo, StorageLocationInfo,
    StorageService,
};
use uuid::Uuid;

use crate::blocking::blocking;
use crate::core::StorageCore;

#[derive(Clone, architect::HasDispatcher)]
pub struct StorageBackend {
    core: Arc<StorageCore>,
    org: String,
    /// This org's own hub, cloned from the coordinator's per-org map —
    /// `#[subscribe]` hands out a `&PubSub`, and clones share one
    /// subscriber list, so a per-org hub is what keeps one org's
    /// placements out of another's stream.
    events: architect::PubSub<StorageEvent>,
}

impl std::fmt::Debug for StorageBackend {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StorageBackend")
            .field("org", &self.org)
            .finish_non_exhaustive()
    }
}

impl StorageBackend {
    #[must_use]
    pub fn new(core: Arc<StorageCore>, org: impl Into<String>) -> Self {
        let org = org.into();
        let events = core.org_hub(&org);
        Self { core, org, events }
    }

    #[must_use]
    pub fn org(&self) -> &str {
        &self.org
    }

    #[must_use]
    pub fn core(&self) -> &Arc<StorageCore> {
        &self.core
    }
}

impl StorageService for StorageBackend {
    async fn list_locations(&self) -> Result<Vec<StorageLocationInfo>, StorageError> {
        Ok(self.core.locations_for(&self.org))
    }

    async fn list_grants(&self) -> Result<Vec<StorageGrantInfo>, StorageError> {
        Ok(self.core.list_grants(Some(&self.org)))
    }

    async fn place_root(
        &self,
        root_id: Uuid,
        location_id: Uuid,
        relative_path: String,
    ) -> Result<RootPlacement, StorageError> {
        let core = self.core.clone();
        let org = self.org.clone();
        blocking(move || core.place_root(&org, root_id, location_id, &relative_path)).await
    }

    async fn placement(&self, root_id: Uuid) -> Result<RootPlacement, StorageError> {
        self.core
            .placement(&self.org, root_id)
            .map_err(crate::error::to_storage_error)
    }

    async fn list_placements(&self) -> Result<Vec<RootPlacement>, StorageError> {
        Ok(self.core.list_placements(&self.org))
    }

    async fn add_blob_replica(
        &self,
        root_id: Uuid,
        location_id: Uuid,
    ) -> Result<RootPlacement, StorageError> {
        let core = self.core.clone();
        let org = self.org.clone();
        blocking(move || core.add_blob_replica(&org, root_id, location_id)).await
    }

    async fn refresh_usage(&self, root_id: Uuid) -> Result<RootPlacement, StorageError> {
        let core = self.core.clone();
        let org = self.org.clone();
        blocking(move || core.refresh_usage(&org, root_id)).await
    }

    async fn usage(&self, location_id: Uuid) -> Result<GrantUsage, StorageError> {
        self.core
            .usage(&self.org, location_id)
            .map_err(crate::error::to_storage_error)
    }
}

/// The `#[subscribe]` backend contract: hand the emitted stream host this
/// org's hub. Publishing happens in [`StorageCore`], on every successful
/// mutation.
impl files_storage_proto::service::org::StorageServiceStreamSource for StorageBackend {
    fn events_hub(&self) -> &architect::PubSub<StorageEvent> {
        &self.events
    }
}
