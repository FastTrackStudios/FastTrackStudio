//! SyncStatusService implementation

use sync_proto::{SyncState, SyncStatusService};
use tokio::sync::Mutex;

/// Tracks the current sync state
pub struct SyncStatusServiceImpl {
    state: Mutex<SyncState>,
    last_sync: Mutex<Option<String>>,
}

impl SyncStatusServiceImpl {
    pub fn new() -> Self {
        Self {
            state: Mutex::new(SyncState::default()),
            last_sync: Mutex::new(None),
        }
    }

    /// Update the sync state
    pub async fn set_state(&self, state: SyncState) {
        *self.state.lock().await = state;
    }

    /// Record a successful sync timestamp
    pub async fn set_last_sync(&self, timestamp: String) {
        *self.last_sync.lock().await = Some(timestamp);
    }
}

impl Default for SyncStatusServiceImpl {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait::async_trait]
impl SyncStatusService for SyncStatusServiceImpl {
    async fn get_sync_state(&self) -> SyncState {
        self.state.lock().await.clone()
    }

    async fn get_last_sync_time(&self) -> Option<String> {
        self.last_sync.lock().await.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    #[tokio::test]
    async fn test_sync_status_default_state() -> Result<()> {
        // -- Setup
        let svc = SyncStatusServiceImpl::new();

        // -- Check
        assert_eq!(svc.get_sync_state().await, SyncState::Offline);
        assert!(svc.get_last_sync_time().await.is_none());
        Ok(())
    }

    #[tokio::test]
    async fn test_sync_status_set_state() -> Result<()> {
        // -- Setup
        let svc = SyncStatusServiceImpl::new();

        // -- Exec
        svc.set_state(SyncState::Syncing).await;

        // -- Check
        assert_eq!(svc.get_sync_state().await, SyncState::Syncing);
        Ok(())
    }

    #[tokio::test]
    async fn test_sync_status_set_last_sync() -> Result<()> {
        // -- Setup
        let svc = SyncStatusServiceImpl::new();
        let timestamp = "2026-02-08T12:00:00Z".to_string();

        // -- Exec
        svc.set_last_sync(timestamp.clone()).await;

        // -- Check
        assert_eq!(svc.get_last_sync_time().await, Some(timestamp));
        Ok(())
    }

    #[tokio::test]
    async fn test_sync_status_state_transitions() -> Result<()> {
        // -- Setup
        let svc = SyncStatusServiceImpl::new();

        // -- Exec: Offline → Syncing → Synced → Idle
        svc.set_state(SyncState::Syncing).await;
        assert_eq!(svc.get_sync_state().await, SyncState::Syncing);

        svc.set_state(SyncState::Synced).await;
        assert_eq!(svc.get_sync_state().await, SyncState::Synced);

        svc.set_state(SyncState::Idle).await;
        assert_eq!(svc.get_sync_state().await, SyncState::Idle);
        Ok(())
    }

    #[tokio::test]
    async fn test_sync_status_error_state() -> Result<()> {
        // -- Setup
        let svc = SyncStatusServiceImpl::new();

        // -- Exec
        svc.set_state(SyncState::Error("connection refused".into())).await;

        // -- Check
        assert_eq!(
            svc.get_sync_state().await,
            SyncState::Error("connection refused".into())
        );
        Ok(())
    }
}
