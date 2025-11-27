//! REAPER implementation of SetlistStateProvider
//!
//! Provides setlist state to the stream API.

use std::sync::Arc;
use setlist::{SetlistApi, SetlistStateProvider};

/// REAPER implementation of SetlistStateProvider
pub struct ReaperSetlistStateProvider {
    /// Reference to the setlist service's state
    setlist_state: Arc<std::sync::Mutex<Option<SetlistApi>>>,
}

impl ReaperSetlistStateProvider {
    /// Create a new state provider with a reference to the setlist service state
    pub fn new(setlist_state: Arc<std::sync::Mutex<Option<SetlistApi>>>) -> Self {
        Self { setlist_state }
    }
}

#[async_trait::async_trait]
impl SetlistStateProvider for ReaperSetlistStateProvider {
    async fn get_setlist_api(&self) -> Result<SetlistApi, String> {
        // Always return the latest state immediately - no waiting
        // The state is continuously updated by the timer callback, so we just return
        // whatever is currently available. This ensures transport updates are sent
        // as fast as the polling task runs (~120Hz).
        let guard = self.setlist_state.lock().unwrap();
        match guard.as_ref() {
            Some(api) => Ok(api.clone()),
            None => {
                // Log periodically to avoid spam
                static MISSING_COUNT: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
                let count = MISSING_COUNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                if count % 1000 == 0 {
                    tracing::debug!(
                        missing_count = count,
                        "Setlist state provider: no setlist API state available yet (waiting for first update)"
                    );
                }
                Err("No setlist API state available".to_string())
            }
        }
    }
}

