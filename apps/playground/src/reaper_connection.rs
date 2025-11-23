//! REAPER Connection Module
//!
//! Handles connection to REAPER extension and streaming transport updates.
//! Uses the transport crate's abstraction, so it doesn't depend on reaper_extension directly.
//! Includes automatic reconnection logic similar to the desktop app's setlist connection.

use anyhow::Result;
use dioxus::prelude::*;
use iroh::Endpoint;
use peer_2_peer::iroh_connection::read_endpoint_id;
use tracing::{error, info, warn};
use transport::{TransportStreamApi, TransportUpdateMessage, TransportUpdate};

use crate::transport_display::TRANSPORT;

/// Connect to REAPER extension and start streaming transport updates with automatic reconnection
#[cfg(not(target_arch = "wasm32"))]
pub async fn connect_to_reaper() -> Result<()> {
    // Spawn the connection task with retry logic
    spawn(async move {
        connect_with_retry().await;
    });
    
    Ok(())
}

/// Connect with automatic retry logic (similar to desktop app's setlist connection)
#[cfg(not(target_arch = "wasm32"))]
async fn connect_with_retry() {
    let mut retry_count = 0u32;
    let mut backoff_seconds = 1u64;
    const MAX_BACKOFF_SECONDS: u64 = 30;
    
    loop {
        info!("[REAPER Connection] Attempting to connect to transport stream (attempt {})...", retry_count + 1);
        
        match try_connect().await {
            Ok(()) => {
                info!("[REAPER Connection] Successfully connected to transport stream");
                retry_count = 0;
                backoff_seconds = 1;
                
                // Wait a bit before checking if we need to reconnect
                // The receiver task will handle detecting disconnections
                tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            }
            Err(e) => {
                retry_count += 1;
                warn!("[REAPER Connection] Connection attempt {} failed: {}", retry_count, e);
                
                // Exponential backoff with max limit
                info!("[REAPER Connection] Retrying in {} seconds...", backoff_seconds);
                tokio::time::sleep(tokio::time::Duration::from_secs(backoff_seconds)).await;
                
                backoff_seconds = (backoff_seconds * 2).min(MAX_BACKOFF_SECONDS);
            }
        }
    }
}

/// Attempt a single connection to the transport stream
#[cfg(not(target_arch = "wasm32"))]
async fn try_connect() -> Result<()> {
    // Read endpoint ID from file (needed for EndpointAddr)
    let endpoint_id = match read_endpoint_id() {
        Ok(Some(id)) => id,
        Ok(None) => {
            return Err(anyhow::anyhow!("No endpoint ID found, REAPER extension may not be running"));
        }
        Err(e) => {
            return Err(anyhow::anyhow!("Failed to read endpoint ID: {}", e));
        }
    };

    info!("[REAPER Connection] Connecting to transport stream (endpoint ID: {})", endpoint_id);

    // Create client endpoint and connect to the transport stream service
    // Clients don't need to know about REAPER - they just connect via the generic API
    let client_endpoint = Endpoint::builder().bind().await
        .map_err(|e| anyhow::anyhow!("Failed to create client endpoint: {}", e))?;
    
    // Create EndpointAddr from EndpointId
    let endpoint_addr = iroh::EndpointAddr::from(endpoint_id);
    
    // Connect to the transport stream service using the generic API
    let transport_api = TransportStreamApi::connect(client_endpoint, endpoint_addr)
        .map_err(|e| anyhow::anyhow!("Failed to connect to transport stream service: {}", e))?;
    
    info!("[REAPER Connection] Connected! Subscribing to transport stream...");
    
    // Subscribe to get a receiver with timeout
    let connection_timeout = tokio::time::Duration::from_secs(5);
    let mut receiver = match tokio::time::timeout(connection_timeout, transport_api.subscribe()).await {
        Ok(Ok(receiver)) => receiver,
        Ok(Err(e)) => {
            return Err(anyhow::anyhow!("Failed to subscribe to transport stream: {}", e));
        }
        Err(_) => {
            return Err(anyhow::anyhow!("Timeout subscribing to transport stream after {:?}", connection_timeout));
        }
    };
    
    info!("[REAPER Connection] ✅ Subscribed! Got receiver, starting to receive transport updates...");
    
    // Spawn task to receive updates and update the global signal
    // This task will exit when the connection is lost, triggering a reconnection
    spawn(async move {
        info!("[REAPER Connection] 📡 Receiver task started, waiting for messages...");
        let mut message_count = 0u64;
        let mut last_received = tokio::time::Instant::now();
        let connection_timeout = tokio::time::Duration::from_secs(2); // 2 second timeout for health check
        
        loop {
            // Use timeout to detect if connection is dead
            match tokio::time::timeout(connection_timeout, receiver.recv()).await {
                Ok(Ok(Some(msg))) => {
                    last_received = tokio::time::Instant::now();
                    message_count += 1;
                    
                    // Log every 30 messages (once per second at 30Hz) to avoid spam
                    if message_count % 30 == 0 {
                        info!("[REAPER Connection] ✅ Received message #{}!", message_count);
                    }
                    
                    // Convert TransportUpdateMessage to TransportUpdate
                    let update = TransportUpdate::from(msg);
                    
                    // Update the global TRANSPORT signal
                    let transport = update.transport.clone();
                    *TRANSPORT.write() = transport.clone();
                    
                    // Log every 30 messages (once per second at 30Hz)
                    if message_count % 30 == 0 {
                        let position_seconds = transport.playhead_position.time.to_seconds();
                        let tempo_bpm = transport.tempo.bpm;
                        info!(
                            "[REAPER Connection] 📨 Update #{}: {:?} | Position: {:.2}s | Tempo: {:.1} BPM | Rate: {:.2}x",
                            message_count,
                            transport.play_state,
                            position_seconds,
                            tempo_bpm,
                            transport.playrate
                        );
                    }
                }
                Ok(Ok(None)) => {
                    warn!("[REAPER Connection] ❌ Transport stream ended (receiver returned None)");
                    break;
                }
                Ok(Err(e)) => {
                    error!("[REAPER Connection] ❌ Error receiving transport update: {}", e);
                    break;
                }
                Err(_) => {
                    // Timeout - connection appears dead
                    let elapsed = last_received.elapsed();
                    warn!("[REAPER Connection] ⏱️ Connection timeout - no data received for {:?} (last received {:?} ago)", connection_timeout, elapsed);
                    break;
                }
            }
        }
        
        warn!("[REAPER Connection] 🛑 Receiver task ended after {} messages. Will reconnect...", message_count);
    });
    
    Ok(())
}
