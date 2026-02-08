//! Sync status types and service definition

/// Current sync operation state
#[repr(u8)]
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum SyncState {
    /// No sync in progress, everything up to date
    Idle,
    /// Currently syncing data
    Syncing,
    /// Sync completed successfully
    Synced,
    /// Sync encountered an error
    Error(String),
    /// Not connected / not authenticated
    #[default]
    Offline,
}

/// Service for querying and subscribing to sync status
#[async_trait::async_trait]
pub trait SyncStatusService: Send + Sync {
    /// Get the current sync state
    async fn get_sync_state(&self) -> SyncState;

    /// Get the last sync timestamp as ISO 8601 string, if any
    async fn get_last_sync_time(&self) -> Option<String>;
}
