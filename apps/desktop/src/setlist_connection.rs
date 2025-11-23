//! Setlist Connection Module
//!
//! Handles connection to REAPER extension and streaming setlist updates.
//! Updates the SETLIST global signal from the setlist crate.
//!
//! Following iroh best practices: creates the endpoint once and keeps it alive,
//! only creating new connections when needed.

use anyhow::Result;
use dioxus::prelude::*;
use iroh::{Endpoint, EndpointId};
use peer_2_peer::iroh_connection::read_endpoint_id;
use std::sync::OnceLock;
use tracing::{error, info, warn};
use setlist::{SetlistStreamApi, SETLIST};

// Cached endpoint (created once, kept alive for the lifetime of the app)
static ENDPOINT: OnceLock<Endpoint> = OnceLock::new();
// Cached endpoint ID (discovered once, reused on retries)
static ENDPOINT_ID: OnceLock<EndpointId> = OnceLock::new();

/// Connect to REAPER extension and start streaming setlist updates with automatic reconnection
#[cfg(not(target_arch = "wasm32"))]
pub async fn connect_to_reaper_setlist() -> Result<()> {
    // Create the endpoint once (iroh endpoints are long-lived)
    let endpoint = Endpoint::builder().bind().await
        .map_err(|e| anyhow::anyhow!("Failed to create client endpoint: {}", e))?;
    ENDPOINT.set(endpoint).map_err(|_| anyhow::anyhow!("Endpoint already initialized"))?;
    
    // Spawn the connection task with retry logic
    spawn(async move {
        connect_with_retry().await;
    });
    
    Ok(())
}

/// Connect with automatic retry logic
/// 
/// Reuses the cached endpoint and only creates new connections when needed.
/// The endpoint stays alive - we just create new connections when the old one fails.
#[cfg(not(target_arch = "wasm32"))]
async fn connect_with_retry() {
    let mut retry_count = 0u32;
    let mut backoff_seconds = 1u64;
    const MAX_BACKOFF_SECONDS: u64 = 30;
    
    loop {
        match try_connect().await {
            Ok(disconnect_rx) => {
                info!("[Setlist Connection] Successfully connected");
                retry_count = 0;
                backoff_seconds = 1;
                
                // Wait for the disconnect signal (connection lost)
                // This will block until the connection actually fails
                let _ = disconnect_rx.await;
                warn!("[Setlist Connection] Connection lost, will retry...");
            }
            Err(e) => {
                retry_count += 1;
                if retry_count % 10 == 0 {
                    warn!("[Setlist Connection] Connection attempt {} failed: {}", retry_count, e);
                }
                tokio::time::sleep(tokio::time::Duration::from_secs(backoff_seconds)).await;
                backoff_seconds = (backoff_seconds * 2).min(MAX_BACKOFF_SECONDS);
            }
        }
    }
}

/// Attempt a single connection to the setlist stream
/// 
/// Reuses the cached endpoint and only creates a new connection.
/// Returns a channel that will receive a message when the connection is lost.
#[cfg(not(target_arch = "wasm32"))]
async fn try_connect() -> Result<tokio::sync::oneshot::Receiver<()>> {
    // Get the cached endpoint (should exist from connect_to_reaper_setlist())
    let endpoint = ENDPOINT.get()
        .ok_or_else(|| anyhow::anyhow!("Endpoint not initialized. Call connect_to_reaper_setlist() first."))?
        .clone();
    
    // Get or read REAPER endpoint ID (cache after first discovery)
    let endpoint_id = if let Some(id) = ENDPOINT_ID.get() {
        *id
    } else {
        let id = match read_endpoint_id() {
            Ok(Some(id)) => id,
            Ok(None) => {
                return Err(anyhow::anyhow!("No endpoint ID found, REAPER extension may not be running"));
            }
            Err(e) => {
                return Err(anyhow::anyhow!("Failed to read endpoint ID: {}", e));
            }
        };
        info!("[Setlist Connection] Found REAPER endpoint ID: {}", id);
        ENDPOINT_ID.set(id).map_err(|_| anyhow::anyhow!("Endpoint ID already set"))?;
        *ENDPOINT_ID.get().unwrap()
    };
    
    let endpoint_addr = iroh::EndpointAddr::from(endpoint_id);
    
    let setlist_api = SetlistStreamApi::connect(endpoint, endpoint_addr)
        .map_err(|e| anyhow::anyhow!("Failed to connect to setlist stream service: {}", e))?;
    
    let connection_timeout = tokio::time::Duration::from_secs(5);
    let mut receiver = match tokio::time::timeout(connection_timeout, setlist_api.subscribe()).await {
        Ok(Ok(receiver)) => receiver,
        Ok(Err(e)) => {
            return Err(anyhow::anyhow!("Failed to subscribe to setlist stream: {}", e));
        }
        Err(_) => {
            return Err(anyhow::anyhow!("Timeout subscribing to setlist stream after {:?}", connection_timeout));
        }
    };
    
    // Create a channel to signal when the connection is lost
    let (tx, rx) = tokio::sync::oneshot::channel();
    
    // Spawn task to receive updates and update the global signal using Dioxus spawn
    // This task will run until the connection is lost
    spawn(async move {
        use std::sync::atomic::{AtomicU64, Ordering};
        use std::time::Instant;
        
        static RECV_COUNT: AtomicU64 = AtomicU64::new(0);
        static UPDATE_COUNT: AtomicU64 = AtomicU64::new(0);
        static LAST_LOG: std::sync::OnceLock<std::sync::Mutex<Instant>> = std::sync::OnceLock::new();
        
        let connection_timeout = tokio::time::Duration::from_secs(5);
        
        loop {
            // Use timeout to detect if connection is dead
            match tokio::time::timeout(connection_timeout, receiver.recv()).await {
                Ok(Ok(Some(msg))) => {
                    RECV_COUNT.fetch_add(1, Ordering::Relaxed);
                    
                    // Update the global SETLIST signal on every message - no rate limiting
                    // This ensures the UI stays in sync with the latest state
                    UPDATE_COUNT.fetch_add(1, Ordering::Relaxed);
                    *SETLIST.write() = Some(msg.setlist_api.clone());
                    
                    // Log performance metrics every 10 seconds
                    let last_log = LAST_LOG.get_or_init(|| std::sync::Mutex::new(Instant::now()));
                    let mut last_log_guard = last_log.lock().unwrap();
                    if last_log_guard.elapsed().as_secs() >= 10 {
                        let recv_rate = RECV_COUNT.swap(0, Ordering::Relaxed) / 10;
                        let update_rate = UPDATE_COUNT.swap(0, Ordering::Relaxed) / 10;
                        info!("[Performance] IRPC receive: {} msg/s, UI updates: {} updates/s", recv_rate, update_rate);
                        *last_log_guard = Instant::now();
                    }
                }
                Ok(Ok(None)) => {
                    warn!("[Setlist Connection] Stream ended");
                    let _ = tx.send(());
                    break;
                }
                Ok(Err(e)) => {
                    error!("[Setlist Connection] Error receiving update: {}", e);
                    let _ = tx.send(());
                    break;
                }
                Err(_) => {
                    // Timeout - connection appears dead
                    warn!("[Setlist Connection] Connection timeout");
                    let _ = tx.send(());
                    break;
                }
            }
        }
    });
    
    Ok(rx)
}
