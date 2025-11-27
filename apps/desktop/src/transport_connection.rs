//! Transport Connection Module
//!
//! Handles connection to REAPER extension and streaming transport updates.
//! Updates global signals for transport state.

use anyhow::Result;
use dioxus::prelude::*;
use std::sync::OnceLock;
use tracing::{debug, error, info, warn};
use daw::transport::reactive::irpc::{TransportApi, TransportUpdateMessage};
use daw::transport::Transport;
use crate::iroh_connection_manager::{init_shared_endpoint, get_shared_endpoint, get_reaper_endpoint_addr};
// Cached transport API (for making RPC calls)
static TRANSPORT_API: OnceLock<std::sync::Mutex<Option<TransportApi>>> = OnceLock::new();
// Connection status channel for UI updates
static CONNECTION_STATUS: OnceLock<tokio::sync::watch::Sender<bool>> = OnceLock::new();

// Global signals for transport state (project_name -> transport)
static TRANSPORT_STATE: OnceLock<GlobalSignal<std::collections::HashMap<String, Transport>>> = OnceLock::new();

/// Initialize the transport API storage and signals
#[cfg(not(target_arch = "wasm32"))]
fn init_transport_storage() {
    TRANSPORT_API.get_or_init(|| std::sync::Mutex::new(None));
    TRANSPORT_STATE.get_or_init(|| Signal::global(|| std::collections::HashMap::new()));
}

/// Get a receiver for connection status updates
#[cfg(not(target_arch = "wasm32"))]
pub fn get_connection_status_receiver() -> Option<tokio::sync::watch::Receiver<bool>> {
    CONNECTION_STATUS.get().map(|tx| tx.subscribe())
}

/// Get the current transport state signal
pub fn get_transport_state() -> Option<&'static GlobalSignal<std::collections::HashMap<String, Transport>>> {
    TRANSPORT_STATE.get()
}

/// Get the current transport API instance
/// Returns None if not connected
#[cfg(not(target_arch = "wasm32"))]
pub fn get_transport_api() -> Option<TransportApi> {
    init_transport_storage();
    TRANSPORT_API.get()?.lock().ok()?.as_ref().map(|api| {
        // TransportApi doesn't implement Clone, so we need to create a new connection
        // For now, return None - callers should handle this
        // TODO: Implement Clone for TransportApi or use Arc
        None
    }).flatten()
}

/// Connect to REAPER extension and start streaming transport updates with automatic reconnection
#[cfg(not(target_arch = "wasm32"))]
pub async fn connect_to_reaper_transport() -> Result<()> {
    // Initialize the API storage
    init_transport_storage();
    
    // Create connection status channel
    let (tx, _rx) = tokio::sync::watch::channel(false);
    CONNECTION_STATUS.set(tx).map_err(|_| anyhow::anyhow!("Connection status already initialized"))?;
    
    // Ensure shared endpoint is initialized
    init_shared_endpoint().await?;
    
    // Spawn the connection task with retry logic
    spawn(async move {
        connect_with_retry().await;
    });
    
    Ok(())
}

/// Connect with automatic retry logic
#[cfg(not(target_arch = "wasm32"))]
async fn connect_with_retry() {
    let mut retry_count = 0u32;
    const RETRY_INTERVAL_SECONDS: u64 = 3;
    
    // Update connection status to disconnected initially
    if let Some(status_tx) = CONNECTION_STATUS.get() {
        let _ = status_tx.send(false);
    }
    
    info!("[Transport Connection] Starting reconnection loop - will continuously retry every {} seconds until connected", RETRY_INTERVAL_SECONDS);
    
    loop {
        retry_count += 1;
        info!("[Transport Connection] Attempting to connect (attempt {})...", retry_count);
        
        match try_connect().await {
            Ok(disconnect_rx) => {
                info!("[Transport Connection] ✅ Successfully connected on attempt {}", retry_count);
                retry_count = 0;
                
                // Update connection status to connected
                if let Some(status_tx) = CONNECTION_STATUS.get() {
                    let _ = status_tx.send(true);
                }
                
                // Wait for the disconnect signal
                info!("[Transport Connection] Waiting for connection to be established and receiving updates...");
                let _ = disconnect_rx.await;
                warn!("[Transport Connection] ❌ Connection lost, will retry immediately...");
                
                // Clear the transport state when disconnected
                if let Some(state) = TRANSPORT_STATE.get() {
                    *state.write() = std::collections::HashMap::new();
                }
                
                // Clear the API instance
                if let Some(api_storage) = TRANSPORT_API.get() {
                    if let Ok(mut guard) = api_storage.lock() {
                        *guard = None;
                    }
                }
                
                // Update connection status to disconnected
                if let Some(status_tx) = CONNECTION_STATUS.get() {
                    let _ = status_tx.send(false);
                }
                
                info!("[Transport Connection] Retrying immediately after connection loss...");
            }
            Err(e) => {
                if retry_count == 1 || retry_count % 10 == 0 {
                    warn!("[Transport Connection] Connection attempt {} failed: {}", retry_count, e);
                }
                
                // Clear the transport state when disconnected
                if let Some(state) = TRANSPORT_STATE.get() {
                    *state.write() = std::collections::HashMap::new();
                }
                
                // Clear the API instance
                if let Some(api_storage) = TRANSPORT_API.get() {
                    if let Ok(mut guard) = api_storage.lock() {
                        *guard = None;
                    }
                }
                
                // Update connection status to disconnected
                if let Some(status_tx) = CONNECTION_STATUS.get() {
                    let _ = status_tx.send(false);
                }
                
                info!("[Transport Connection] Retrying in {} seconds...", RETRY_INTERVAL_SECONDS);
                tokio::time::sleep(tokio::time::Duration::from_secs(RETRY_INTERVAL_SECONDS)).await;
            }
        }
    }
}

/// Attempt a single connection to the transport stream
#[cfg(not(target_arch = "wasm32"))]
async fn try_connect() -> Result<tokio::sync::oneshot::Receiver<()>> {
    // Get the shared endpoint
    let endpoint = get_shared_endpoint()
        .ok_or_else(|| anyhow::anyhow!("Shared endpoint not initialized. Call connect_to_reaper_transport() first."))?;
    
    // Get the REAPER endpoint address
    let endpoint_addr = get_reaper_endpoint_addr()?
        .ok_or_else(|| anyhow::anyhow!("No endpoint ID found, REAPER extension may not be running"))?;
    
    // Create a NEW TransportApi instance on each connection attempt
    let transport_api = TransportApi::connect(endpoint.clone(), endpoint_addr)
        .map_err(|e| {
            warn!("[Transport Connection] Failed to create connection wrapper: {}", e);
            anyhow::anyhow!("Failed to create connection wrapper: {}", e)
        })?;
    
    // Subscribe to transport updates
    info!("[Transport Connection] Attempting to connect and subscribe to transport stream...");
    let connection_timeout = tokio::time::Duration::from_secs(10);
    
    let mut transport_rx = match tokio::time::timeout(
        connection_timeout,
        transport_api.subscribe()
    ).await {
        Ok(Ok(rx)) => {
            info!("[Transport Connection] ✅ Successfully connected and subscribed to transport stream");
            rx
        }
        Ok(Err(e)) => {
            warn!("[Transport Connection] ❌ Failed to connect/subscribe to stream: {}", e);
            return Err(anyhow::anyhow!("Failed to connect/subscribe to stream: {}", e));
        }
        Err(_) => {
            warn!("[Transport Connection] ❌ Connection timeout after {:?} - REAPER extension may not be running or unreachable", connection_timeout);
            return Err(anyhow::anyhow!("Connection timeout after {:?}", connection_timeout));
        }
    };
    
    // Store the API instance AFTER subscribe() succeeds
    // Note: TransportApi doesn't implement Clone, so we store it directly
    if let Some(api_storage) = TRANSPORT_API.get() {
        if let Ok(mut guard) = api_storage.lock() {
            *guard = Some(transport_api);
            info!("[Transport Connection] Stored transport API instance");
        }
    }
    
    // Create a channel to signal when the connection is lost
    let (tx, rx) = tokio::sync::oneshot::channel();
    
    // Spawn task to receive updates and update the global signal
    spawn(async move {
        use std::sync::atomic::{AtomicU64, Ordering};
        
        static RECV_COUNT: AtomicU64 = AtomicU64::new(0);
        
        info!("[Transport Connection] ✅ Started receiving transport updates - signal will now update");
        let mut message_count = 0u64;
        
        loop {
            match transport_rx.recv().await {
                Ok(Some(msg)) => {
                    message_count += 1;
                    RECV_COUNT.fetch_add(1, Ordering::Relaxed);
                    handle_message(msg, &mut message_count, &RECV_COUNT).await;
                }
                Ok(None) => {
                    warn!("[Transport Connection] Transport stream ended (received {} messages) - will reconnect", message_count);
                    break;
                }
                Err(e) => {
                    error!(
                        error = %e,
                        error_debug = ?e,
                        message_count = message_count,
                        "[Transport Connection] Error receiving transport update (received {} messages)",
                        message_count
                    );
                    break;
                }
            }
        }
        
        // Cleanup and signal disconnect
        cleanup_on_disconnect();
        
        // Signal disconnect - this will trigger reconnection
        let _ = tx.send(());
        
        info!("[Transport Connection] Update receiver task ended - reconnection loop will create a new one");
    });
    
    Ok(rx)
}

/// Handle a single transport update message
async fn handle_message(msg: TransportUpdateMessage, message_count: &mut u64, recv_count: &std::sync::atomic::AtomicU64) {
    use std::sync::atomic::{AtomicU64, Ordering};
    use std::time::Instant;
    
    static UPDATE_COUNT: AtomicU64 = AtomicU64::new(0);
    static LAST_LOG: std::sync::OnceLock<std::sync::Mutex<Instant>> = std::sync::OnceLock::new();
    
    // Handle transport updates
    match msg {
        TransportUpdateMessage::TransportChanged(msg) => {
            UPDATE_COUNT.fetch_add(1, Ordering::Relaxed);
            if let Some(state) = TRANSPORT_STATE.get() {
                let mut transport_map = state.write();
                transport_map.insert(msg.project_id.clone(), msg.transport.clone());
            }
            if *message_count == 1 {
                info!("[Transport Connection] ✅ Received first transport update for project: {}", msg.project_id);
            } else {
                debug!("[Transport Connection] Updated transport for project: {}", msg.project_id);
            }
        }
        TransportUpdateMessage::PlayStateChanged(msg) => {
            UPDATE_COUNT.fetch_add(1, Ordering::Relaxed);
            if let Some(state) = TRANSPORT_STATE.get() {
                let mut transport_map = state.write();
                if let Some(transport) = transport_map.get_mut(&msg.project_id) {
                    transport.play_state = msg.play_state;
                }
            }
            debug!("[Transport Connection] Updated play state for project: {} -> {:?}", msg.project_id, msg.play_state);
        }
        TransportUpdateMessage::TempoChanged(msg) => {
            UPDATE_COUNT.fetch_add(1, Ordering::Relaxed);
            if let Some(state) = TRANSPORT_STATE.get() {
                let mut transport_map = state.write();
                if let Some(transport) = transport_map.get_mut(&msg.project_id) {
                    transport.tempo = msg.tempo;
                }
            }
            debug!("[Transport Connection] Updated tempo for project: {} -> {}", msg.project_id, msg.tempo);
        }
        TransportUpdateMessage::PositionChanged(msg) => {
            UPDATE_COUNT.fetch_add(1, Ordering::Relaxed);
            if let Some(state) = TRANSPORT_STATE.get() {
                let mut transport_map = state.write();
                if let Some(transport) = transport_map.get_mut(&msg.project_id) {
                    transport.playhead_position.time = daw::primitives::TimePosition::from_seconds(msg.position);
                }
            }
            debug!("[Transport Connection] Updated position for project: {} -> {:.3}s", msg.project_id, msg.position);
        }
    }
    
    // Log performance metrics every 10 seconds
    let last_log = LAST_LOG.get_or_init(|| std::sync::Mutex::new(Instant::now()));
    let mut last_log_guard = last_log.lock().unwrap();
    if last_log_guard.elapsed().as_secs() >= 10 {
        let recv_rate = recv_count.swap(0, Ordering::Relaxed) / 10;
        let update_rate = UPDATE_COUNT.swap(0, Ordering::Relaxed) / 10;
        info!("[Transport Performance] IRPC receive: {} msg/s, UI updates: {} updates/s", recv_rate, update_rate);
        *last_log_guard = Instant::now();
    }
}

/// Cleanup signals when connection is lost
fn cleanup_on_disconnect() {
    // Clear the transport state when disconnected
    if let Some(state) = TRANSPORT_STATE.get() {
        *state.write() = std::collections::HashMap::new();
    }
    // Clear the API instance
    if let Some(api_storage) = TRANSPORT_API.get() {
        if let Ok(mut guard) = api_storage.lock() {
            *guard = None;
        }
    }
}

