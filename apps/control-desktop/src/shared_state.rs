//! Shared state between the Dioxus app and HTTP server.

use crate::roam_client::RoamClient;
use std::sync::Arc;

/// Global shared state accessible by both Dioxus app and HTTP server.
pub struct SharedState {
    pub roam_client: Arc<RoamClient>,
    pub server_port: u16,
}

impl SharedState {
    pub fn new() -> Self {
        let port = std::env::var("FTS_CONTROL_PORT")
            .ok()
            .and_then(|p| p.parse().ok())
            .unwrap_or(9251);

        Self {
            roam_client: Arc::new(RoamClient::new()),
            server_port: port,
        }
    }
}

/// Global shared state instance
static SHARED_STATE: std::sync::OnceLock<Arc<SharedState>> = std::sync::OnceLock::new();

/// Initialize the shared state (call once at startup)
pub fn init_shared_state() -> Arc<SharedState> {
    SHARED_STATE
        .get_or_init(|| Arc::new(SharedState::new()))
        .clone()
}

/// Get the shared state (panics if not initialized)
pub fn get_shared_state() -> Arc<SharedState> {
    SHARED_STATE
        .get()
        .expect("SharedState not initialized")
        .clone()
}
