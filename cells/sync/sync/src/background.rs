//! Background sync scheduler — runs periodic sync on a timer.
//!
//! Spawns a tokio task that syncs every N minutes when online.
//! Gracefully handles offline state by skipping cycles.

use std::sync::Arc;
use std::time::Duration;

use tokio::sync::Notify;
use tokio::task::JoinHandle;

use crate::engine::SyncEngine;

/// Default sync interval: 5 minutes.
const DEFAULT_SYNC_INTERVAL: Duration = Duration::from_secs(5 * 60);

/// Handle to a running background sync task.
///
/// Dropping the handle does NOT cancel the task. Call [`BackgroundSync::stop`]
/// to cleanly shut down the background loop.
pub struct BackgroundSync {
    /// Notify used to signal shutdown to the background task.
    shutdown: Arc<Notify>,
    /// Notify used to trigger an immediate sync cycle.
    trigger: Arc<Notify>,
    /// Join handle for the background task.
    handle: Option<JoinHandle<()>>,
}

impl BackgroundSync {
    /// Start the background sync loop with the default interval (5 minutes).
    pub fn start(engine: Arc<SyncEngine>) -> Self {
        Self::start_with_interval(engine, DEFAULT_SYNC_INTERVAL)
    }

    /// Start the background sync loop with a custom interval.
    pub fn start_with_interval(engine: Arc<SyncEngine>, interval: Duration) -> Self {
        let shutdown = Arc::new(Notify::new());
        let trigger = Arc::new(Notify::new());

        let shutdown_rx = shutdown.clone();
        let trigger_rx = trigger.clone();

        let handle = tokio::spawn(async move {
            tracing::info!(?interval, "Background sync started");

            loop {
                // Wait for either the interval to elapse, a manual trigger, or shutdown
                tokio::select! {
                    () = tokio::time::sleep(interval) => {}
                    () = trigger_rx.notified() => {
                        tracing::debug!("Manual sync triggered");
                    }
                    () = shutdown_rx.notified() => {
                        tracing::info!("Background sync shutting down");
                        break;
                    }
                }

                // Skip if offline
                if !engine.is_online().await {
                    tracing::debug!("Background sync: skipping (offline)");
                    continue;
                }

                match engine.sync().await {
                    Ok(progress) => {
                        tracing::info!(
                            completed = progress.completed,
                            conflicts = progress.conflicts,
                            errors = progress.errors,
                            "Background sync cycle complete"
                        );
                    }
                    Err(e) => {
                        tracing::warn!(error = %e, "Background sync cycle failed");
                    }
                }
            }
        });

        Self {
            shutdown,
            trigger,
            handle: Some(handle),
        }
    }

    /// Trigger an immediate sync cycle (non-blocking).
    pub fn trigger_sync(&self) {
        self.trigger.notify_one();
    }

    /// Stop the background sync loop.
    pub async fn stop(mut self) {
        self.shutdown.notify_one();
        if let Some(handle) = self.handle.take() {
            let _ = handle.await;
        }
    }

    /// Check whether the background task is still running.
    pub fn is_running(&self) -> bool {
        self.handle
            .as_ref()
            .is_some_and(|h| !h.is_finished())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use sea_orm::Database;
    use sea_orm_migration::MigratorTrait;
    use signal_storage::Migrator;

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    async fn setup_engine() -> Result<Arc<SyncEngine>> {
        let db = Database::connect("sqlite::memory:").await?;
        Migrator::up(&db, None).await?;
        Ok(Arc::new(SyncEngine::new(db)))
    }

    #[tokio::test]
    async fn test_background_sync_starts_and_stops() -> Result<()> {
        // -- Setup
        let engine = setup_engine().await?;

        // -- Exec
        let bg = BackgroundSync::start_with_interval(engine, Duration::from_millis(50));
        assert!(bg.is_running());

        // -- Exec: stop
        bg.stop().await;
        Ok(())
    }

    #[tokio::test]
    async fn test_background_sync_manual_trigger() -> Result<()> {
        // -- Setup
        let engine = setup_engine().await?;
        let bg = BackgroundSync::start_with_interval(engine, Duration::from_secs(3600)); // long interval

        // -- Exec: trigger immediate sync
        bg.trigger_sync();

        // Give it a moment to process
        tokio::time::sleep(Duration::from_millis(50)).await;
        assert!(bg.is_running());

        bg.stop().await;
        Ok(())
    }

    #[tokio::test]
    async fn test_background_sync_skips_when_offline() -> Result<()> {
        // -- Setup: engine is offline by default
        let engine = setup_engine().await?;
        let bg = BackgroundSync::start_with_interval(engine, Duration::from_millis(20));

        // -- Exec: let a few cycles pass
        tokio::time::sleep(Duration::from_millis(100)).await;

        // -- Check: still running (didn't crash), no errors
        assert!(bg.is_running());
        bg.stop().await;
        Ok(())
    }
}
