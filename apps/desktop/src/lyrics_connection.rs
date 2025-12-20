//! Lyrics Connection Module
//!
//! Handles connection to REAPER extension and streaming lyrics updates.
//! Updates global signals for lyrics state.

use anyhow::Result;
use dioxus::prelude::*;
use std::sync::OnceLock;
use tracing::{debug, error, info, warn};
use fts::lyrics::reactive::irpc::{LyricsApi, LyricsUpdateMessage};
use fts::lyrics::core::Lyrics;
use fts::lyrics::source::LyricsAnnotations;
use crate::iroh_connection_manager::{init_shared_endpoint, get_shared_endpoint, get_reaper_endpoint_addr};
// Cached lyrics API (for making RPC calls)
static LYRICS_API: OnceLock<std::sync::Mutex<Option<LyricsApi>>> = OnceLock::new();
// Connection status channel for UI updates
static CONNECTION_STATUS: OnceLock<tokio::sync::watch::Sender<bool>> = OnceLock::new();

// Global signals for lyrics state
static LYRICS_STATE: OnceLock<GlobalSignal<std::collections::HashMap<String, Lyrics>>> = OnceLock::new();
static ACTIVE_SLIDE_INDEX: OnceLock<GlobalSignal<std::collections::HashMap<String, Option<usize>>>> = OnceLock::new();
static LYRICS_ANNOTATIONS: OnceLock<GlobalSignal<std::collections::HashMap<String, LyricsAnnotations>>> = OnceLock::new();

/// Initialize the lyrics API storage and signals
#[cfg(not(target_arch = "wasm32"))]
fn init_lyrics_storage() {
    LYRICS_API.get_or_init(|| std::sync::Mutex::new(None));
    LYRICS_STATE.get_or_init(|| Signal::global(|| std::collections::HashMap::new()));
    ACTIVE_SLIDE_INDEX.get_or_init(|| Signal::global(|| std::collections::HashMap::new()));
    LYRICS_ANNOTATIONS.get_or_init(|| Signal::global(|| std::collections::HashMap::new()));
}

/// Get a receiver for connection status updates
#[cfg(not(target_arch = "wasm32"))]
pub fn get_connection_status_receiver() -> Option<tokio::sync::watch::Receiver<bool>> {
    CONNECTION_STATUS.get().map(|tx| tx.subscribe())
}

/// Get the current lyrics state signal
pub fn get_lyrics_state() -> Option<&'static GlobalSignal<std::collections::HashMap<String, Lyrics>>> {
    LYRICS_STATE.get()
}

/// Get the current active slide index signal
pub fn get_active_slide_index() -> Option<&'static GlobalSignal<std::collections::HashMap<String, Option<usize>>>> {
    ACTIVE_SLIDE_INDEX.get()
}

/// Get the current lyrics annotations signal
pub fn get_lyrics_annotations() -> Option<&'static GlobalSignal<std::collections::HashMap<String, LyricsAnnotations>>> {
    LYRICS_ANNOTATIONS.get()
}

/// Get the current lyrics API instance
/// Returns None if not connected
#[cfg(not(target_arch = "wasm32"))]
pub fn get_lyrics_api() -> Option<LyricsApi> {
    init_lyrics_storage();
    LYRICS_API.get()?.lock().ok()?.as_ref().map(|api| {
        // LyricsApi doesn't implement Clone, so we need to create a new connection
        // For now, return None - callers should handle this
        // TODO: Implement Clone for LyricsApi or use Arc
        None
    }).flatten()
}

/// Connect to REAPER extension and start streaming lyrics updates with automatic reconnection
/// 
/// Note: This shares the endpoint with other connections. The endpoint should be initialized
/// by the first connection module (usually setlist_connection).
#[cfg(not(target_arch = "wasm32"))]
pub async fn connect_to_reaper_lyrics() -> Result<()> {
    // Initialize the API storage
    init_lyrics_storage();
    
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
    
    info!("[Lyrics Connection] Starting reconnection loop - will continuously retry every {} seconds until connected", RETRY_INTERVAL_SECONDS);
    
    loop {
        retry_count += 1;
        info!("[Lyrics Connection] Attempting to connect (attempt {})...", retry_count);
        
        match try_connect().await {
            Ok(disconnect_rx) => {
                info!("[Lyrics Connection] ✅ Successfully connected on attempt {}", retry_count);
                retry_count = 0;
                
                // Update connection status to connected
                if let Some(status_tx) = CONNECTION_STATUS.get() {
                    let _ = status_tx.send(true);
                }
                
                // Wait for the disconnect signal
                info!("[Lyrics Connection] Waiting for connection to be established and receiving updates...");
                let _ = disconnect_rx.await;
                warn!("[Lyrics Connection] ❌ Connection lost, will retry immediately...");
                
                // Clear the lyrics state when disconnected
                if let Some(state) = LYRICS_STATE.get() {
                    *state.write() = std::collections::HashMap::new();
                }
                if let Some(state) = ACTIVE_SLIDE_INDEX.get() {
                    *state.write() = std::collections::HashMap::new();
                }
                if let Some(state) = LYRICS_ANNOTATIONS.get() {
                    *state.write() = std::collections::HashMap::new();
                }
                
                // Clear the API instance
                if let Some(api_storage) = LYRICS_API.get() {
                    if let Ok(mut guard) = api_storage.lock() {
                        *guard = None;
                    }
                }
                
                // Update connection status to disconnected
                if let Some(status_tx) = CONNECTION_STATUS.get() {
                    let _ = status_tx.send(false);
                }
                
                info!("[Lyrics Connection] Retrying immediately after connection loss...");
            }
            Err(e) => {
                if retry_count == 1 || retry_count.is_multiple_of(10) {
                    warn!("[Lyrics Connection] Connection attempt {} failed: {}", retry_count, e);
                }
                
                // Clear the lyrics state when disconnected
                if let Some(state) = LYRICS_STATE.get() {
                    *state.write() = std::collections::HashMap::new();
                }
                if let Some(state) = ACTIVE_SLIDE_INDEX.get() {
                    *state.write() = std::collections::HashMap::new();
                }
                if let Some(state) = LYRICS_ANNOTATIONS.get() {
                    *state.write() = std::collections::HashMap::new();
                }
                
                // Clear the API instance
                if let Some(api_storage) = LYRICS_API.get() {
                    if let Ok(mut guard) = api_storage.lock() {
                        *guard = None;
                    }
                }
                
                // Update connection status to disconnected
                if let Some(status_tx) = CONNECTION_STATUS.get() {
                    let _ = status_tx.send(false);
                }
                
                info!("[Lyrics Connection] Retrying in {} seconds...", RETRY_INTERVAL_SECONDS);
                tokio::time::sleep(tokio::time::Duration::from_secs(RETRY_INTERVAL_SECONDS)).await;
            }
        }
    }
}

/// Attempt a single connection to the lyrics stream
#[cfg(not(target_arch = "wasm32"))]
async fn try_connect() -> Result<tokio::sync::oneshot::Receiver<()>> {
    // Get the shared endpoint
    // Get the shared endpoint
    let endpoint = get_shared_endpoint()
        .ok_or_else(|| anyhow::anyhow!("Shared endpoint not initialized. Call connect_to_reaper_lyrics() first."))?;
    
    // Get the REAPER endpoint address
    let endpoint_addr = get_reaper_endpoint_addr()?
        .ok_or_else(|| anyhow::anyhow!("No endpoint ID found, REAPER extension may not be running"))?;
    
    // Create a NEW LyricsApi instance on each connection attempt
    let lyrics_api = LyricsApi::connect(endpoint.clone(), endpoint_addr)
        .map_err(|e| {
            warn!("[Lyrics Connection] Failed to create connection wrapper: {}", e);
            anyhow::anyhow!("Failed to create connection wrapper: {}", e)
        })?;
    
    // Subscribe to lyrics updates
    info!("[Lyrics Connection] Attempting to connect and subscribe to lyrics stream...");
    let connection_timeout = tokio::time::Duration::from_secs(10);
    
    let mut lyrics_rx = match tokio::time::timeout(
        connection_timeout,
        lyrics_api.subscribe()
    ).await {
        Ok(Ok(rx)) => {
            info!("[Lyrics Connection] ✅ Successfully connected and subscribed to lyrics stream");
            rx
        }
        Ok(Err(e)) => {
            warn!("[Lyrics Connection] ❌ Failed to connect/subscribe to stream: {}", e);
            return Err(anyhow::anyhow!("Failed to connect/subscribe to stream: {}", e));
        }
        Err(_) => {
            warn!("[Lyrics Connection] ❌ Connection timeout after {:?} - REAPER extension may not be running or unreachable", connection_timeout);
            return Err(anyhow::anyhow!("Connection timeout after {:?}", connection_timeout));
        }
    };
    
    // Store the API instance AFTER subscribe() succeeds
    // Note: LyricsApi doesn't implement Clone, so we store it directly
    if let Some(api_storage) = LYRICS_API.get() {
        if let Ok(mut guard) = api_storage.lock() {
            *guard = Some(lyrics_api);
            info!("[Lyrics Connection] Stored lyrics API instance");
        }
    }
    
    // Create a channel to signal when the connection is lost
    let (tx, rx) = tokio::sync::oneshot::channel();
    
    // Spawn task to receive updates and update the global signal
    spawn(async move {
        use std::sync::atomic::{AtomicU64, Ordering};
        
        static RECV_COUNT: AtomicU64 = AtomicU64::new(0);
        
        info!("[Lyrics Connection] ✅ Started receiving lyrics updates - signal will now update");
        let mut message_count = 0u64;
        
        loop {
            match lyrics_rx.recv().await {
                Ok(Some(msg)) => {
                    message_count += 1;
                    RECV_COUNT.fetch_add(1, Ordering::Relaxed);
                    handle_message(msg, &mut message_count, &RECV_COUNT).await;
                }
                Ok(None) => {
                    warn!("[Lyrics Connection] Lyrics stream ended (received {} messages) - will reconnect", message_count);
                    break;
                }
                Err(e) => {
                    error!(
                        error = %e,
                        error_debug = ?e,
                        message_count = message_count,
                        "[Lyrics Connection] Error receiving lyrics update (received {} messages)",
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
        
        info!("[Lyrics Connection] Update receiver task ended - reconnection loop will create a new one");
    });
    
    Ok(rx)
}

/// Handle a single lyrics update message
async fn handle_message(msg: LyricsUpdateMessage, message_count: &mut u64, recv_count: &std::sync::atomic::AtomicU64) {
    use std::sync::atomic::{AtomicU64, Ordering};
    use std::time::Instant;
    
    static UPDATE_COUNT: AtomicU64 = AtomicU64::new(0);
    static LAST_LOG: std::sync::OnceLock<std::sync::Mutex<Instant>> = std::sync::OnceLock::new();
    
    // Handle lyrics updates
    match msg {
        LyricsUpdateMessage::LyricsChanged(msg) => {
            UPDATE_COUNT.fetch_add(1, Ordering::Relaxed);
            if let Some(state) = LYRICS_STATE.get() {
                let mut lyrics_map = state.write();
                lyrics_map.insert(msg.song_name.clone(), msg.lyrics.clone());
            }
            if *message_count == 1 {
                info!("[Lyrics Connection] ✅ Received first lyrics update for song: {}", msg.song_name);
            } else {
                debug!("[Lyrics Connection] Updated lyrics for song: {}", msg.song_name);
            }
        }
        LyricsUpdateMessage::ActiveSlideIndexChanged(msg) => {
            UPDATE_COUNT.fetch_add(1, Ordering::Relaxed);
            if let Some(state) = ACTIVE_SLIDE_INDEX.get() {
                let mut slide_map = state.write();
                slide_map.insert(msg.song_name.clone(), msg.slide_index);
            }
            debug!("[Lyrics Connection] Updated active slide index for song: {} -> {:?}", msg.song_name, msg.slide_index);
        }
        LyricsUpdateMessage::LyricsAnnotationsChanged(msg) => {
            UPDATE_COUNT.fetch_add(1, Ordering::Relaxed);
            if let Some(state) = LYRICS_ANNOTATIONS.get() {
                let mut annotations_map = state.write();
                annotations_map.insert(msg.song_name.clone(), msg.annotations.clone());
            }
            debug!("[Lyrics Connection] Updated lyrics annotations for song: {}", msg.song_name);
        }
    }
    
    // Log performance metrics every 10 seconds
    let last_log = LAST_LOG.get_or_init(|| std::sync::Mutex::new(Instant::now()));
    let mut last_log_guard = last_log.lock().unwrap();
    if last_log_guard.elapsed().as_secs() >= 10 {
        let recv_rate = recv_count.swap(0, Ordering::Relaxed) / 10;
        let update_rate = UPDATE_COUNT.swap(0, Ordering::Relaxed) / 10;
        info!("[Lyrics Performance] IRPC receive: {} msg/s, UI updates: {} updates/s", recv_rate, update_rate);
        *last_log_guard = Instant::now();
    }
}

/// Cleanup signals when connection is lost
fn cleanup_on_disconnect() {
    // Clear the lyrics state when disconnected
    if let Some(state) = LYRICS_STATE.get() {
        *state.write() = std::collections::HashMap::new();
    }
    if let Some(state) = ACTIVE_SLIDE_INDEX.get() {
        *state.write() = std::collections::HashMap::new();
    }
    if let Some(state) = LYRICS_ANNOTATIONS.get() {
        *state.write() = std::collections::HashMap::new();
    }
    // Clear the API instance
    if let Some(api_storage) = LYRICS_API.get() {
        if let Ok(mut guard) = api_storage.lock() {
            *guard = None;
        }
    }
}

