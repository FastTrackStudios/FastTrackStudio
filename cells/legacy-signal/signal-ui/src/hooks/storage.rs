//! Hook providing a shared [`StorageService`] via global signals.
//!
//! Exposes `use_storage()` which lazily initializes a `SqliteBackend`
//! (using the same default path as `graph_persistence.rs`) and makes it
//! available to any component that needs key-value persistence.
//!
//! Also tracks cloud sync status so UI components can display a badge.

use crate::prelude::*;
use signal_storage::{Persistable, SqliteBackend};
use std::sync::Arc;

// ─────────────────────────────────────────────────────────────────────────────
// Storage Backend Signal
// ─────────────────────────────────────────────────────────────────────────────

/// Shared persistence backend — opened once and reused across all hooks.
///
/// This is the same backend used by `graph_persistence.rs` via `KV_BACKEND`.
/// Components call `use_storage()` to access it.
pub static STORAGE_BACKEND: GlobalSignal<Option<Arc<SqliteBackend>>> = Signal::global(|| None);

// ─────────────────────────────────────────────────────────────────────────────
// Cloud Sync Status
// ─────────────────────────────────────────────────────────────────────────────

/// Cloud synchronization status for the toolbar indicator.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum CloudSyncStatus {
    /// Only saving locally, cloud not configured.
    #[default]
    LocalOnly,
    /// Cloud sync is active and up-to-date.
    CloudSynced,
    /// Cloud sync is in progress.
    CloudSyncing,
    /// Cloud is unavailable (offline).
    CloudOffline,
}

impl CloudSyncStatus {
    /// Human-readable label for display.
    pub const fn label(self) -> &'static str {
        match self {
            Self::LocalOnly => "Local",
            Self::CloudSynced => "Cloud",
            Self::CloudSyncing => "Syncing",
            Self::CloudOffline => "Offline",
        }
    }

    /// Whether the status represents an offline/unavailable state.
    pub const fn is_offline(self) -> bool {
        matches!(self, Self::CloudOffline)
    }

    /// Whether cloud storage is enabled (even if currently offline).
    pub const fn is_cloud_enabled(self) -> bool {
        matches!(
            self,
            Self::CloudSynced | Self::CloudSyncing | Self::CloudOffline
        )
    }
}

/// Global cloud sync status — UI reads this for toolbar badge.
pub static CLOUD_SYNC_STATUS: GlobalSignal<CloudSyncStatus> =
    Signal::global(CloudSyncStatus::default);

/// Whether the user has opted in to cloud storage.
pub static CLOUD_STORAGE_ENABLED: GlobalSignal<bool> = Signal::global(|| false);

// ─────────────────────────────────────────────────────────────────────────────
// Hook
// ─────────────────────────────────────────────────────────────────────────────

/// Hook that provides access to the shared `StorageService` backend.
///
/// On first call, opens a `SqliteBackend` at the default path and stores
/// it in `STORAGE_BACKEND`. Subsequent calls reuse the existing backend.
///
/// Returns an `Option<Arc<SqliteBackend>>` — `None` if the backend hasn't
/// finished initializing yet.
///
/// # Example
///
/// ```ignore
/// let storage = use_storage();
/// if let Some(backend) = storage {
///     save_value(backend.as_ref(), "my:key", &data).await;
/// }
/// ```
pub fn use_storage() -> Option<Arc<SqliteBackend>> {
    // Trigger lazy initialization on first call
    use_effect(move || {
        let existing = STORAGE_BACKEND.read().clone();
        if existing.is_some() {
            return; // Already initialized
        }

        spawn(async move {
            // Double-check inside the async block (another component may have raced)
            if STORAGE_BACKEND.read().is_some() {
                return;
            }

            match SqliteBackend::from_default_path().await {
                Ok(backend) => {
                    let shared = Arc::new(backend);
                    *STORAGE_BACKEND.write() = Some(shared);
                    tracing::info!("Storage backend initialized (SQLite)");
                }
                Err(e) => {
                    tracing::warn!("Failed to open storage backend: {e}");
                }
            }
        });
    });

    STORAGE_BACKEND.read().clone()
}

/// Toggle cloud storage on/off.
///
/// When enabled, sets `CLOUD_SYNC_STATUS` to `CloudOffline` (mocked — real
/// cloud sync is not yet implemented). When disabled, resets to `LocalOnly`.
pub fn toggle_cloud_storage() {
    let enabled = !*CLOUD_STORAGE_ENABLED.read();
    *CLOUD_STORAGE_ENABLED.write() = enabled;

    if enabled {
        // Mock: immediately go to "offline" since cloud backend is not implemented
        *CLOUD_SYNC_STATUS.write() = CloudSyncStatus::CloudOffline;
        tracing::info!("Cloud storage enabled (mocked — currently offline)");
    } else {
        *CLOUD_SYNC_STATUS.write() = CloudSyncStatus::LocalOnly;
        tracing::info!("Cloud storage disabled, using local only");
    }
}
