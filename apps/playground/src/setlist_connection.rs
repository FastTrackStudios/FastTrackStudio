//! Setlist Connection Module
//!
//! Handles connection to REAPER extension and streaming setlist updates.
//! Uses the setlist crate's abstraction, so it doesn't depend on reaper_extension directly.
//! Includes automatic reconnection logic similar to the transport connection.

use anyhow::Result;
use dioxus::prelude::*;
use iroh::Endpoint;
use peer_2_peer::iroh_connection::read_endpoint_id;
use tracing::{error, info, warn};
use setlist::SetlistStreamApi;

use crate::setlist_display::SETLIST;

/// Connect to REAPER extension and start streaming setlist updates with automatic reconnection
#[cfg(not(target_arch = "wasm32"))]
pub async fn connect_to_reaper_setlist() -> Result<()> {
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
    let mut backoff_seconds = 1u64;
    const MAX_BACKOFF_SECONDS: u64 = 30;
    
    loop {
        info!("[Setlist Connection] Attempting to connect to setlist stream (attempt {})...", retry_count + 1);
        
        match try_connect().await {
            Ok(()) => {
                info!("[Setlist Connection] Successfully connected to setlist stream");
                retry_count = 0;
                backoff_seconds = 1;
                
                // Wait a bit before checking if we need to reconnect
                tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            }
            Err(e) => {
                retry_count += 1;
                warn!("[Setlist Connection] Connection attempt {} failed: {}", retry_count, e);
                
                // Exponential backoff with max limit
                info!("[Setlist Connection] Retrying in {} seconds...", backoff_seconds);
                tokio::time::sleep(tokio::time::Duration::from_secs(backoff_seconds)).await;
                
                backoff_seconds = (backoff_seconds * 2).min(MAX_BACKOFF_SECONDS);
            }
        }
    }
}

/// Attempt a single connection to the setlist stream
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

    info!("[Setlist Connection] Connecting to setlist stream (endpoint ID: {})", endpoint_id);

    // Create client endpoint and connect to the setlist stream service
    let client_endpoint = Endpoint::builder().bind().await
        .map_err(|e| anyhow::anyhow!("Failed to create client endpoint: {}", e))?;
    
    // Create EndpointAddr from EndpointId
    let endpoint_addr = iroh::EndpointAddr::from(endpoint_id);
    
    // Connect to the setlist stream service
    let setlist_api = SetlistStreamApi::connect(client_endpoint, endpoint_addr)
        .map_err(|e| anyhow::anyhow!("Failed to connect to setlist stream service: {}", e))?;
    
    info!("[Setlist Connection] Connected! Subscribing to setlist stream...");
    
    // Subscribe to get a receiver with timeout
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
    
    info!("[Setlist Connection] ✅ Subscribed! Got receiver, starting to receive setlist updates...");
    
    // Spawn task to receive updates and update the global signal
    spawn(async move {
        info!("[Setlist Connection] 📡 Receiver task started, waiting for messages...");
        let mut message_count = 0u64;
        let mut last_received = tokio::time::Instant::now();
        let connection_timeout = tokio::time::Duration::from_secs(5); // 5 second timeout for health check (setlist updates less frequently)
        
        loop {
            // Use timeout to detect if connection is dead
            match tokio::time::timeout(connection_timeout, receiver.recv()).await {
                Ok(Ok(Some(msg))) => {
                    last_received = tokio::time::Instant::now();
                    message_count += 1;
                    
                    // Log every message (setlist updates are less frequent)
                    info!("[Setlist Connection] ✅ Received setlist update #{}!", message_count);
                    
                    // Update the global SETLIST signal
                    *SETLIST.write() = Some(msg.setlist.clone());
                    
                    info!(
                        "[Setlist Connection] 📨 Update #{}: {} ({} songs)",
                        message_count,
                        msg.setlist.name,
                        msg.setlist.songs.len()
                    );
                }
                Ok(Ok(None)) => {
                    warn!("[Setlist Connection] ❌ Setlist stream ended (receiver returned None)");
                    break;
                }
                Ok(Err(e)) => {
                    error!("[Setlist Connection] ❌ Error receiving setlist update: {}", e);
                    break;
                }
                Err(_) => {
                    // Timeout - connection appears dead
                    let elapsed = last_received.elapsed();
                    warn!("[Setlist Connection] ⏱️ Connection timeout - no data received for {:?} (last received {:?} ago)", connection_timeout, elapsed);
                    break;
                }
            }
        }
        
        warn!("[Setlist Connection] 🛑 Receiver task ended after {} messages. Will reconnect...", message_count);
    });
    
    Ok(())
}

