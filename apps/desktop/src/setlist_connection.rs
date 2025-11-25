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
use setlist::{SetlistStreamApi, SETLIST, ACTIVE_SLIDE_INDEX};

// Cached endpoint (created once, kept alive for the lifetime of the app)
static ENDPOINT: OnceLock<Endpoint> = OnceLock::new();
// Cached endpoint ID (can be cleared and re-read on retries)
static ENDPOINT_ID: OnceLock<std::sync::Mutex<Option<EndpointId>>> = OnceLock::new();
// Cached setlist stream API (for making RPC calls)
static SETLIST_API: OnceLock<std::sync::Mutex<Option<SetlistStreamApi>>> = OnceLock::new();
// Connection status channel for UI updates
static CONNECTION_STATUS: OnceLock<tokio::sync::watch::Sender<bool>> = OnceLock::new();

/// Initialize the setlist API storage
#[cfg(not(target_arch = "wasm32"))]
fn init_setlist_api_storage() {
    SETLIST_API.get_or_init(|| std::sync::Mutex::new(None));
    ENDPOINT_ID.get_or_init(|| std::sync::Mutex::new(None));
}

/// Get a receiver for connection status updates
#[cfg(not(target_arch = "wasm32"))]
pub fn get_connection_status_receiver() -> Option<tokio::sync::watch::Receiver<bool>> {
    CONNECTION_STATUS.get().map(|tx| tx.subscribe())
}

/// Get the current setlist stream API instance
/// Returns None if not connected
#[cfg(not(target_arch = "wasm32"))]
pub fn get_setlist_api() -> Option<SetlistStreamApi> {
    init_setlist_api_storage();
    SETLIST_API.get()?.lock().ok()?.clone()
}

/// Connect to REAPER extension and start streaming setlist updates with automatic reconnection
#[cfg(not(target_arch = "wasm32"))]
pub async fn connect_to_reaper_setlist() -> Result<()> {
    // Initialize the API storage
    init_setlist_api_storage();
    
    // Create connection status channel
    let (tx, _rx) = tokio::sync::watch::channel(false);
    CONNECTION_STATUS.set(tx).map_err(|_| anyhow::anyhow!("Connection status already initialized"))?;
    
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
/// This function runs forever and never stops trying to reconnect.
#[cfg(not(target_arch = "wasm32"))]
async fn connect_with_retry() {
    let mut retry_count = 0u32;
    const RETRY_INTERVAL_SECONDS: u64 = 3; // Retry every 3 seconds
    
    // Update connection status to disconnected initially
    if let Some(status_tx) = CONNECTION_STATUS.get() {
        let _ = status_tx.send(false);
    }
    
    info!("[Setlist Connection] Starting reconnection loop - will continuously retry every {} seconds until connected", RETRY_INTERVAL_SECONDS);
    
    // Infinite retry loop - never stops trying to reconnect
    loop {
        retry_count += 1;
        info!("[Setlist Connection] Attempting to connect (attempt {})...", retry_count);
        
        match try_connect().await {
            Ok(disconnect_rx) => {
                info!("[Setlist Connection] ✅ Successfully connected on attempt {}", retry_count);
                retry_count = 0; // Reset counter on success
                
                // Update connection status to connected
                if let Some(status_tx) = CONNECTION_STATUS.get() {
                    let _ = status_tx.send(true);
                }
                
                // Wait for the disconnect signal (connection lost)
                // This will block until the connection actually fails
                info!("[Setlist Connection] Waiting for connection to be established and receiving updates...");
                let _ = disconnect_rx.await;
                warn!("[Setlist Connection] ❌ Connection lost, will retry immediately...");
                
                // Clear the SETLIST signal when disconnected
                *SETLIST.write() = None;
                *ACTIVE_SLIDE_INDEX.write() = None;
                
                // Clear the API instance
                if let Some(api_storage) = SETLIST_API.get() {
                    if let Ok(mut guard) = api_storage.lock() {
                        *guard = None;
                    }
                }
                
                // Update connection status to disconnected
                if let Some(status_tx) = CONNECTION_STATUS.get() {
                    let _ = status_tx.send(false);
                }
                
                // Immediately retry (no delay) when connection was established but then lost
                // This ensures we actively try to reconnect as soon as possible
                info!("[Setlist Connection] Retrying immediately after connection loss...");
            }
            Err(e) => {
                if retry_count == 1 || retry_count % 10 == 0 {
                    warn!("[Setlist Connection] Connection attempt {} failed: {}", retry_count, e);
                }
                
                // Note: We now always re-read the endpoint ID on each attempt,
                // so we don't need to clear the cache - it will be updated with
                // the latest value from the file on the next attempt
                
                // Clear the SETLIST signal when disconnected
                *SETLIST.write() = None;
                *ACTIVE_SLIDE_INDEX.write() = None;
                
                // Clear the API instance
                if let Some(api_storage) = SETLIST_API.get() {
                    if let Ok(mut guard) = api_storage.lock() {
                        *guard = None;
                    }
                }
                
                // Update connection status to disconnected
                if let Some(status_tx) = CONNECTION_STATUS.get() {
                    let _ = status_tx.send(false);
                }
                
                // Use fixed retry interval for failed connection attempts
                info!("[Setlist Connection] Retrying in {} seconds...", RETRY_INTERVAL_SECONDS);
                tokio::time::sleep(tokio::time::Duration::from_secs(RETRY_INTERVAL_SECONDS)).await;
            }
        }
        
        // Loop continues - we never exit this function
    }
}

/// Attempt a single connection to the setlist stream
/// 
/// Creates a NEW SetlistStreamApi instance on each attempt.
/// The IrohLazyRemoteConnection inside only retries once, so we need to create
/// a fresh connection object for each retry attempt.
/// Returns a channel that will receive a message when the connection is lost.
#[cfg(not(target_arch = "wasm32"))]
async fn try_connect() -> Result<tokio::sync::oneshot::Receiver<()>> {
    // Get the cached endpoint (should exist from connect_to_reaper_setlist())
    let endpoint = ENDPOINT.get()
        .ok_or_else(|| anyhow::anyhow!("Endpoint not initialized. Call connect_to_reaper_setlist() first."))?
        .clone();
    
    // Always re-read the endpoint ID from file on each connection attempt
    // This ensures we get the latest endpoint ID if REAPER restarted
    // (REAPER might get a new endpoint ID when it restarts)
    let endpoint_id = match read_endpoint_id() {
        Ok(Some(id)) => {
            // Update cache with the latest endpoint ID
            if let Some(endpoint_id_lock) = ENDPOINT_ID.get() {
                if let Ok(mut guard) = endpoint_id_lock.lock() {
                    // Only log if it changed
                    if *guard != Some(id) {
                        if let Some(old_id) = *guard {
                            info!("[Setlist Connection] Endpoint ID changed: {} -> {}", old_id, id);
    } else {
                            info!("[Setlist Connection] Found REAPER endpoint ID: {}", id);
                        }
                    }
                    *guard = Some(id);
                }
            }
            id
        }
        Ok(None) => {
            return Err(anyhow::anyhow!("No endpoint ID found, REAPER extension may not be running"));
        }
        Err(e) => {
            return Err(anyhow::anyhow!("Failed to read endpoint ID: {}", e));
        }
    };
    
    let endpoint_addr = iroh::EndpointAddr::from(endpoint_id);
    
    // CRITICAL: Create a NEW SetlistStreamApi instance on each connection attempt
    // The IrohLazyRemoteConnection inside has limited retry logic, so we need
    // to create a fresh connection object for each retry
    // NOTE: SetlistStreamApi::connect() only creates a lazy connection wrapper.
    // The actual connection happens when subscribe() is called.
    let setlist_api = SetlistStreamApi::connect(endpoint.clone(), endpoint_addr)
        .map_err(|e| {
            warn!("[Setlist Connection] Failed to create connection wrapper: {}", e);
            anyhow::anyhow!("Failed to create connection wrapper: {}", e)
        })?;
    
    // The actual connection attempt happens here in subscribe()
    // IrohLazyRemoteConnection will try to connect when subscribe() calls open_bi()
    info!("[Setlist Connection] Attempting to connect and subscribe to stream...");
    let connection_timeout = tokio::time::Duration::from_secs(10);
    let mut receiver = match tokio::time::timeout(connection_timeout, setlist_api.subscribe()).await {
        Ok(Ok(receiver)) => {
            info!("[Setlist Connection] ✅ Successfully connected and subscribed to setlist stream");
            receiver
        }
        Ok(Err(e)) => {
            warn!("[Setlist Connection] ❌ Failed to connect/subscribe to setlist stream: {}", e);
            return Err(anyhow::anyhow!("Failed to connect/subscribe to setlist stream: {}", e));
        }
        Err(_) => {
            warn!("[Setlist Connection] ❌ Connection timeout after {:?} - REAPER extension may not be running or unreachable", connection_timeout);
            return Err(anyhow::anyhow!("Connection timeout after {:?}", connection_timeout));
        }
    };
    
    // Only store the API instance AFTER subscribe() succeeds
    // This ensures we only mark as connected when we can actually receive data
    // NOTE: We create a new SetlistStreamApi on each retry, so we need to update the stored instance
    let setlist_api_for_storage = setlist_api.clone();
    if let Some(api_storage) = SETLIST_API.get() {
        if let Ok(mut guard) = api_storage.lock() {
            *guard = Some(setlist_api_for_storage);
            info!("[Setlist Connection] Stored setlist API instance for RPC calls");
        }
    }
    
    // Create a channel to signal when the connection is lost
    let (tx, rx) = tokio::sync::oneshot::channel();
    
    // Spawn task to receive updates and update the global signal using Dioxus spawn
    // This task will run until the connection is lost, then signal the disconnect
    // The retry loop will create a new receiver and restart this task on reconnect
    spawn(async move {
        use std::sync::atomic::{AtomicU64, Ordering};
        use std::time::Instant;
        
        static RECV_COUNT: AtomicU64 = AtomicU64::new(0);
        static UPDATE_COUNT: AtomicU64 = AtomicU64::new(0);
        static LAST_LOG: std::sync::OnceLock<std::sync::Mutex<Instant>> = std::sync::OnceLock::new();
        
        let connection_timeout = tokio::time::Duration::from_secs(5);
        
        info!("[Setlist Connection] ✅ Started receiving setlist updates - signal will now update");
        let mut message_count = 0u64;
        
        loop {
            // Use timeout to detect if connection is dead
            match tokio::time::timeout(connection_timeout, receiver.recv()).await {
                Ok(Ok(Some(msg))) => {
                    message_count += 1;
                    RECV_COUNT.fetch_add(1, Ordering::Relaxed);
                    
                    // Update the global SETLIST signal on every message - no rate limiting
                    // This ensures the UI stays in sync with the latest state
                    UPDATE_COUNT.fetch_add(1, Ordering::Relaxed);
                    *SETLIST.write() = Some(msg.setlist_api.clone());
                    
                    // Update active slide index signal (for lyrics view)
                    *ACTIVE_SLIDE_INDEX.write() = msg.setlist_api.active_slide_index();
                    
                    // Log first message received
                    if message_count == 1 {
                        info!("[Setlist Connection] ✅ Received first setlist update - signal is now updating");
                    }
                            
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
                    warn!("[Setlist Connection] Stream ended (received {} messages) - will reconnect", message_count);
                    // Clear the SETLIST signal when disconnected
                    *SETLIST.write() = None;
                    *ACTIVE_SLIDE_INDEX.write() = None;
                    // Clear the API instance on error
                    if let Some(api_storage) = SETLIST_API.get() {
                        if let Ok(mut guard) = api_storage.lock() {
                            *guard = None;
                        }
                    }
                    // Signal disconnect - this will trigger reconnection
                    let _ = tx.send(());
                    break;
                }
                Ok(Err(e)) => {
                    error!("[Setlist Connection] Error receiving update (received {} messages): {}", message_count, e);
                    // Clear the SETLIST signal when disconnected
                    *SETLIST.write() = None;
                    *ACTIVE_SLIDE_INDEX.write() = None;
                    // Clear the API instance on error
                    if let Some(api_storage) = SETLIST_API.get() {
                        if let Ok(mut guard) = api_storage.lock() {
                            *guard = None;
                        }
                    }
                    // Signal disconnect - this will trigger reconnection
                    let _ = tx.send(());
                    break;
                }
                Err(_) => {
                    // Timeout - connection appears dead
                    warn!("[Setlist Connection] Connection timeout (received {} messages) - will reconnect", message_count);
                    // Clear the SETLIST signal when disconnected
                    *SETLIST.write() = None;
                    *ACTIVE_SLIDE_INDEX.write() = None;
                    // Clear the API instance on timeout
                    if let Some(api_storage) = SETLIST_API.get() {
                        if let Ok(mut guard) = api_storage.lock() {
                            *guard = None;
                        }
                    }
                    // Signal disconnect - this will trigger reconnection
                    let _ = tx.send(());
                    break;
                }
            }
        }
        
        info!("[Setlist Connection] Update receiver task ended - reconnection loop will create a new one");
    });
    
    Ok(rx)
}
