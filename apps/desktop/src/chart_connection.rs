//! Chart Connection Module
//!
//! Handles connection to REAPER extension and streaming chart updates.
//! Updates global signals for chart state.

use anyhow::Result;
use dioxus::prelude::*;
use std::sync::OnceLock;
use tracing::{debug, error, info, warn};
use crate::iroh_connection_manager::{init_shared_endpoint, get_shared_endpoint, get_reaper_endpoint_addr};

// Use chart types from fts crate
use fts::chords::{ChartApi, ChartUpdateMessage};
// Chart is from keyflow - we need to add keyflow as a dependency or re-export it
// For now, we'll use the full path
use keyflow::Chart;

// Cached chart API (for making RPC calls)
static CHART_API: OnceLock<std::sync::Mutex<Option<ChartApi>>> = OnceLock::new();
// Connection status channel for UI updates
static CONNECTION_STATUS: OnceLock<tokio::sync::watch::Sender<bool>> = OnceLock::new();

// Global signals for chart state (project_name -> chart)
static CHART_STATE: OnceLock<GlobalSignal<std::collections::HashMap<String, Chart>>> = OnceLock::new();

/// Initialize the chart API storage and signals
#[cfg(not(target_arch = "wasm32"))]
fn init_chart_storage() {
    CHART_API.get_or_init(|| std::sync::Mutex::new(None));
    CHART_STATE.get_or_init(|| Signal::global(|| std::collections::HashMap::new()));
}

/// Get a receiver for connection status updates
#[cfg(not(target_arch = "wasm32"))]
pub fn get_connection_status_receiver() -> Option<tokio::sync::watch::Receiver<bool>> {
    CONNECTION_STATUS.get().map(|tx| tx.subscribe())
}

/// Get the current chart state signal
pub fn get_chart_state() -> Option<&'static GlobalSignal<std::collections::HashMap<String, Chart>>> {
    CHART_STATE.get()
}

/// Connect to REAPER extension and start streaming chart updates with automatic reconnection
///
/// Note: This shares the endpoint with other connections. The endpoint should be initialized
/// by the first connection module (usually setlist_connection).
#[cfg(not(target_arch = "wasm32"))]
pub async fn connect_to_reaper_chart() -> Result<()> {
    // Initialize the API storage
    init_chart_storage();
    
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
    
    info!("[Chart Connection] Starting reconnection loop - will continuously retry every {} seconds until connected", RETRY_INTERVAL_SECONDS);
    
    loop {
        retry_count += 1;
        info!("[Chart Connection] Attempting to connect (attempt {})...", retry_count);
        
        match try_connect().await {
            Ok(disconnect_rx) => {
                info!("[Chart Connection] ✅ Successfully connected on attempt {}", retry_count);
                retry_count = 0;
                
                // Update connection status to connected
                if let Some(status_tx) = CONNECTION_STATUS.get() {
                    let _ = status_tx.send(true);
                }
                
                // Wait for the disconnect signal
                info!("[Chart Connection] Waiting for connection to be established and receiving updates...");
                let _ = disconnect_rx.await;
                warn!("[Chart Connection] ❌ Connection lost, will retry immediately...");
                
                // Clear the chart state when disconnected
                if let Some(state) = CHART_STATE.get() {
                    *state.write() = std::collections::HashMap::new();
                }
                
                // Clear the API instance
                if let Some(api_storage) = CHART_API.get() {
                    if let Ok(mut guard) = api_storage.lock() {
                        *guard = None;
                    }
                }
                
                // Update connection status to disconnected
                if let Some(status_tx) = CONNECTION_STATUS.get() {
                    let _ = status_tx.send(false);
                }
                
                info!("[Chart Connection] Retrying immediately after connection loss...");
            }
            Err(e) => {
                if retry_count == 1 || retry_count % 10 == 0 {
                    warn!("[Chart Connection] Connection attempt {} failed: {}", retry_count, e);
                } else {
                    debug!("[Chart Connection] Connection attempt {} failed: {}", retry_count, e);
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
    
    info!("[Chart Connection] Connecting to REAPER at {:?}", endpoint_addr);
    
    // Connect to chart API
    let chart_api = ChartApi::connect(endpoint.clone(), endpoint_addr.clone())
        .map_err(|e| anyhow::anyhow!("Failed to connect to chart API: {}", e))?;
    
    info!("[Chart Connection] ✅ Connected to REAPER chart API");
    
    // Store the API instance
    // Note: ChartApi doesn't implement Clone, so we can't store it
    // The API is only used for subscribing, which we do immediately
    
    // Subscribe to chart updates
    let mut stream = chart_api.subscribe().await
        .map_err(|e| anyhow::anyhow!("Failed to subscribe to chart updates: {}", e))?;
    
    info!("[Chart Connection] ✅ Subscribed to chart updates");
    
    // Create a channel to signal when the connection is lost
    let (disconnect_tx, disconnect_rx) = tokio::sync::oneshot::channel();
    
    // Spawn task to receive updates
    tokio::spawn(async move {
        info!("[Chart Connection] Started receiving chart updates");
        
        loop {
            match stream.recv().await {
                Ok(Some(msg)) => {
                    match msg {
                        ChartUpdateMessage::ChartChanged(changed) => {
                    debug!(
                        project_name = %changed.project_name,
                        section_count = changed.chart.sections.len(),
                        "[Chart Connection] Received chart update"
                    );
                    
                            if let Some(chart_state) = CHART_STATE.get() {
                                chart_state.write().insert(changed.project_name, changed.chart);
                            }
                        }
                    }
                }
                Ok(None) => {
                    warn!("[Chart Connection] Stream ended normally");
                    break;
                }
                Err(e) => {
                    warn!("[Chart Connection] Stream error: {}", e);
                    break;
                }
            }
        }
        
        warn!("[Chart Connection] Stream ended, connection lost");
        let _ = disconnect_tx.send(());
    });
    
    Ok(disconnect_rx)
}

