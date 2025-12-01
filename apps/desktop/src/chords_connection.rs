//! Chords Connection Module
//!
//! Handles connection to REAPER extension and streaming chords updates.
//! Updates global signals for chords state.

use anyhow::Result;
use dioxus::prelude::*;
use std::sync::OnceLock;
use tracing::{debug, error, info, warn};
use crate::iroh_connection_manager::{init_shared_endpoint, get_shared_endpoint, get_reaper_endpoint_addr};

// Use chords types from fts crate
use fts::chords::{ChordsApi, ChordsUpdateMessage, ChordsData};

// Cached chords API (for making RPC calls)
static CHORDS_API: OnceLock<std::sync::Mutex<Option<ChordsApi>>> = OnceLock::new();
// Connection status channel for UI updates
static CONNECTION_STATUS: OnceLock<tokio::sync::watch::Sender<bool>> = OnceLock::new();

// Global signals for chords state (project_name -> chords)
static CHORDS_STATE: OnceLock<GlobalSignal<std::collections::HashMap<String, ChordsData>>> = OnceLock::new();

/// Initialize the chords API storage and signals
#[cfg(not(target_arch = "wasm32"))]
fn init_chords_storage() {
    CHORDS_API.get_or_init(|| std::sync::Mutex::new(None));
    CHORDS_STATE.get_or_init(|| Signal::global(|| std::collections::HashMap::new()));
}

/// Get a receiver for connection status updates
#[cfg(not(target_arch = "wasm32"))]
pub fn get_connection_status_receiver() -> Option<tokio::sync::watch::Receiver<bool>> {
    CONNECTION_STATUS.get().map(|tx| tx.subscribe())
}

/// Get the current chords state signal
pub fn get_chords_state() -> Option<&'static GlobalSignal<std::collections::HashMap<String, ChordsData>>> {
    CHORDS_STATE.get()
}

/// Connect to REAPER extension and start streaming chords updates with automatic reconnection
///
/// Note: This shares the endpoint with other connections. The endpoint should be initialized
/// by the first connection module (usually setlist_connection).
#[cfg(not(target_arch = "wasm32"))]
pub async fn connect_to_reaper_chords() -> Result<()> {
    // Initialize the API storage
    init_chords_storage();
    
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
    
    info!("[Chords Connection] Starting reconnection loop - will continuously retry every {} seconds until connected", RETRY_INTERVAL_SECONDS);
    
    loop {
        retry_count += 1;
        info!("[Chords Connection] Attempting to connect (attempt {})...", retry_count);
        
        match try_connect().await {
            Ok(disconnect_rx) => {
                info!("[Chords Connection] ✅ Successfully connected on attempt {}", retry_count);
                retry_count = 0;
                
                // Update connection status to connected
                if let Some(status_tx) = CONNECTION_STATUS.get() {
                    let _ = status_tx.send(true);
                }
                
                // Wait for the disconnect signal
                info!("[Chords Connection] Waiting for connection to be established and receiving updates...");
                let _ = disconnect_rx.await;
                warn!("[Chords Connection] ❌ Connection lost, will retry immediately...");
                
                // Clear the chords state when disconnected
                if let Some(state) = CHORDS_STATE.get() {
                    *state.write() = std::collections::HashMap::new();
                }
                
                // Clear the API instance
                if let Some(api_storage) = CHORDS_API.get() {
                    if let Ok(mut guard) = api_storage.lock() {
                        *guard = None;
                    }
                }
                
                // Update connection status to disconnected
                if let Some(status_tx) = CONNECTION_STATUS.get() {
                    let _ = status_tx.send(false);
                }
                
                info!("[Chords Connection] Retrying immediately after connection loss...");
            }
            Err(e) => {
                if retry_count == 1 || retry_count % 10 == 0 {
                    warn!("[Chords Connection] Connection attempt {} failed: {}", retry_count, e);
                } else {
                    debug!("[Chords Connection] Connection attempt {} failed: {}", retry_count, e);
                }
                
                // Update connection status to disconnected
                if let Some(status_tx) = CONNECTION_STATUS.get() {
                    let _ = status_tx.send(false);
                }
                
                tokio::time::sleep(tokio::time::Duration::from_secs(RETRY_INTERVAL_SECONDS)).await;
            }
        }
    }
}

/// Attempt to connect and start streaming
#[cfg(not(target_arch = "wasm32"))]
async fn try_connect() -> Result<tokio::sync::oneshot::Receiver<()>> {
    let endpoint = get_shared_endpoint()
        .ok_or_else(|| anyhow::anyhow!("Shared endpoint not initialized"))?;
    
    let endpoint_addr = get_reaper_endpoint_addr()?
        .ok_or_else(|| anyhow::anyhow!("REAPER endpoint address not found"))?;
    
    info!("[Chords Connection] Connecting to REAPER at {:?}", endpoint_addr);
    
    // Connect to chords API
    let chords_api = ChordsApi::connect(endpoint.clone(), endpoint_addr.clone())
        .map_err(|e| anyhow::anyhow!("Failed to connect to chords API: {}", e))?;
    
    info!("[Chords Connection] ✅ Connected to REAPER chords API");
    
    // Store the API instance
    if let Some(api_storage) = CHORDS_API.get() {
        if let Ok(mut guard) = api_storage.lock() {
            *guard = Some(chords_api.clone());
        }
    }
    
    // Subscribe to chords updates
    let mut stream = chords_api.subscribe().await
        .map_err(|e| anyhow::anyhow!("Failed to subscribe to chords updates: {}", e))?;
    
    info!("[Chords Connection] ✅ Subscribed to chords updates");
    
    // Create a channel to signal when the connection is lost
    let (disconnect_tx, disconnect_rx) = tokio::sync::oneshot::channel();
    
    // Spawn task to receive updates
    tokio::spawn(async move {
        info!("[Chords Connection] Started receiving chords updates");
        
        while let Some(msg) = stream.recv().await {
            match msg {
                ChordsUpdateMessage::ChordsChanged(changed) => {
                    debug!(
                        project_name = %changed.project_name,
                        chord_count = changed.chords.chords.len(),
                        "[Chords Connection] Received chords update"
                    );
                    
                    if let Some(chords_state) = CHORDS_STATE.get() {
                        chords_state.write().insert(changed.project_name, changed.chords);
                    }
                }
            }
        }
        
        warn!("[Chords Connection] Stream ended, connection lost");
        let _ = disconnect_tx.send(());
    });
    
    Ok(disconnect_rx)
}

