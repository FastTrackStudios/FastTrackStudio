//! IROH Connection Manager
//!
//! Manages a single Iroh connection to the REAPER endpoint and allows
//! subscribing to different streams (ALPNs) on that same connection.

use anyhow::Result;
use iroh::{Endpoint, EndpointId, EndpointAddr};
use peer_2_peer::iroh_connection::read_endpoint_id;
use std::sync::OnceLock;
use tracing::{info, warn};

/// Shared Iroh endpoint (created once, kept alive for the lifetime of the app)
static ENDPOINT: OnceLock<Endpoint> = OnceLock::new();

/// Shared endpoint ID (cached, can be re-read on retries)
static ENDPOINT_ID: OnceLock<std::sync::Mutex<Option<EndpointId>>> = OnceLock::new();

/// Initialize the shared Iroh endpoint
pub async fn init_shared_endpoint() -> Result<()> {
    if ENDPOINT.get().is_none() {
        let endpoint = Endpoint::builder().bind().await
            .map_err(|e| anyhow::anyhow!("Failed to create client endpoint: {}", e))?;
        ENDPOINT.set(endpoint).map_err(|_| anyhow::anyhow!("Endpoint already initialized"))?;
        info!("[IROH Connection Manager] Created shared endpoint: {}", ENDPOINT.get().unwrap().id());
    }
    ENDPOINT_ID.get_or_init(|| std::sync::Mutex::new(None));
    Ok(())
}

/// Get the shared endpoint
pub fn get_shared_endpoint() -> Option<Endpoint> {
    ENDPOINT.get().cloned()
}

/// Get or read the REAPER endpoint ID
pub fn get_reaper_endpoint_id() -> Result<Option<EndpointId>> {
    // Always re-read from file to get the latest endpoint ID
    let endpoint_id = match read_endpoint_id() {
        Ok(Some(id)) => {
            // Update cache
            if let Some(endpoint_id_lock) = ENDPOINT_ID.get() {
                if let Ok(mut guard) = endpoint_id_lock.lock() {
                    if *guard != Some(id) {
                        if let Some(old_id) = *guard {
                            info!("[IROH Connection Manager] Endpoint ID changed: {} -> {}", old_id, id);
                        } else {
                            info!("[IROH Connection Manager] Found REAPER endpoint ID: {}", id);
                        }
                    }
                    *guard = Some(id);
                }
            }
            Some(id)
        }
        Ok(None) => {
            warn!("[IROH Connection Manager] No endpoint ID found, REAPER extension may not be running");
            None
        }
        Err(e) => {
            warn!("[IROH Connection Manager] Failed to read endpoint ID: {}", e);
            None
        }
    };
    Ok(endpoint_id)
}

/// Get the REAPER endpoint address
pub fn get_reaper_endpoint_addr() -> Result<Option<EndpointAddr>> {
    get_reaper_endpoint_id()?
        .map(|id| Ok(EndpointAddr::from(id)))
        .transpose()
}

