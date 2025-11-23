//! Setlist Connection Module
//!
//! Handles connection to REAPER extension and streaming setlist updates.
//! Updates the SETLIST global signal from the setlist crate.

use anyhow::Result;
use dioxus::prelude::*;
use iroh::Endpoint;
use peer_2_peer::iroh_connection::read_endpoint_id;
use tracing::{error, warn};
use setlist::{SetlistStreamApi, SETLIST};

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
        match try_connect().await {
            Ok(()) => {
                retry_count = 0;
                backoff_seconds = 1;
                tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
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

    let client_endpoint = Endpoint::builder().bind().await
        .map_err(|e| anyhow::anyhow!("Failed to create client endpoint: {}", e))?;
    
    let endpoint_addr = iroh::EndpointAddr::from(endpoint_id);
    
    let setlist_api = SetlistStreamApi::connect(client_endpoint, endpoint_addr)
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
    
    // Spawn task to receive updates and update the global signal
    spawn(async move {
        let connection_timeout = tokio::time::Duration::from_secs(5);
        
        loop {
            // Use timeout to detect if connection is dead
            match tokio::time::timeout(connection_timeout, receiver.recv()).await {
                Ok(Ok(Some(msg))) => {
                    // Update the global SETLIST signal only if data actually changed
                    // This prevents unnecessary rerenders and stuttering
                    let mut setlist_write = SETLIST.write();
                    let should_update = match setlist_write.as_ref() {
                        Some(existing) => {
                            // Compare active song/section indices and transport positions
                            let existing_active_song = existing.active_song_index();
                            let existing_active_section = existing.active_section_index();
                            let new_active_song = msg.setlist_api.active_song_index();
                            let new_active_section = msg.setlist_api.active_section_index();
                            
                            // Check if active song/section changed
                            if existing_active_song != new_active_song || existing_active_section != new_active_section {
                                true
                            } else {
                                // Check if transport positions changed for any song
                                let existing_songs = existing.get_setlist().songs.iter();
                                let new_songs = msg.setlist_api.get_setlist().songs.iter();
                                
                                existing_songs.zip(new_songs).any(|(existing_song, new_song)| {
                                    let existing_pos = existing_song.transport_info.as_ref()
                                        .map(|t| t.playhead_position.time.to_seconds());
                                    let new_pos = new_song.transport_info.as_ref()
                                        .map(|t| t.playhead_position.time.to_seconds());
                                    
                                    // Only update if position changed by more than 0.01s to avoid micro-updates
                                    match (existing_pos, new_pos) {
                                        (Some(existing), Some(new)) => (existing - new).abs() > 0.01,
                                        (None, Some(_)) | (Some(_), None) => true,
                                        (None, None) => false,
                                    }
                                })
                            }
                        }
                        None => true, // First update, always apply
                    };
                    
                    if should_update {
                        *setlist_write = Some(msg.setlist_api.clone());
                    }
                }
                Ok(Ok(None)) => {
                    warn!("[Setlist Connection] Stream ended");
                    break;
                }
                Ok(Err(e)) => {
                    error!("[Setlist Connection] Error receiving update: {}", e);
                    break;
                }
                Err(_) => {
                    // Timeout - connection appears dead
                    warn!("[Setlist Connection] Connection timeout");
                    break;
                }
            }
        }
    });
    
    Ok(())
}

