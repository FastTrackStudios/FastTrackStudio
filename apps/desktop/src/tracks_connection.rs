//! Tracks Connection Module
//!
//! Handles connection to REAPER extension and streaming track updates.
//! Updates global signals for track state.

use crate::iroh_connection_manager::{
    get_reaper_endpoint_addr, get_shared_endpoint, init_shared_endpoint,
};
use anyhow::Result;
use daw::tracks::reactive::irpc::{TrackApi, TrackUpdateMessage};
use daw::tracks::Track;
use dioxus::prelude::*;
use fts::setlist::{SETLIST_STRUCTURE, SONG_TRACKS};
use std::sync::OnceLock;
use tracing::{debug, error, info, warn};
// Cached tracks API (for making RPC calls)
pub static TRACKS_API: OnceLock<tokio::sync::Mutex<Option<TrackApi>>> = OnceLock::new();
// Connection status channel for UI updates
static CONNECTION_STATUS: OnceLock<tokio::sync::watch::Sender<bool>> = OnceLock::new();

// Global signals for tracks state (project_name -> tracks)
static TRACKS_STATE: OnceLock<GlobalSignal<std::collections::HashMap<String, Vec<Track>>>> =
    OnceLock::new();

/// Initialize the tracks API storage and signals
#[cfg(not(target_arch = "wasm32"))]
fn init_tracks_storage() {
    TRACKS_API.get_or_init(|| tokio::sync::Mutex::new(None));
    #[allow(clippy::redundant_closure)]
    TRACKS_STATE.get_or_init(|| Signal::global(|| std::collections::HashMap::new()));
}

/// Get a receiver for connection status updates
#[cfg(not(target_arch = "wasm32"))]
pub fn get_connection_status_receiver() -> Option<tokio::sync::watch::Receiver<bool>> {
    CONNECTION_STATUS.get().map(|tx| tx.subscribe())
}

/// Get the current tracks state signal
pub fn get_tracks_state(
) -> Option<&'static GlobalSignal<std::collections::HashMap<String, Vec<Track>>>> {
    TRACKS_STATE.get()
}

/// Helper function to find song_index by project name
/// Looks up the project name in the setlist structure
fn find_song_index_by_project_name(project_name: &str) -> Option<usize> {
    let setlist_structure = SETLIST_STRUCTURE.read();
    let setlist = setlist_structure.as_ref()?;

    for (idx, song) in setlist.songs.iter().enumerate() {
        if song.project_name_from_metadata() == project_name {
            return Some(idx);
        }
    }
    None
}

/// Get the current tracks API instance
/// Returns None if not connected
/// Note: TrackApi doesn't implement Clone, so this function is deprecated.
#[cfg(not(target_arch = "wasm32"))]
#[deprecated(note = "TrackApi no longer implements Clone. Use track_commands functions instead.")]
pub fn get_tracks_api() -> Option<TrackApi> {
    // TrackApi doesn't implement Clone, so we can't return it
    None
}

/// Connect to REAPER extension and start streaming track updates with automatic reconnection
///
/// Note: This shares the endpoint with other connections. The endpoint should be initialized
/// by the first connection module (usually setlist_connection).
#[cfg(not(target_arch = "wasm32"))]
pub async fn connect_to_reaper_tracks() -> Result<()> {
    // Initialize the API storage
    init_tracks_storage();

    // Create connection status channel
    let (tx, _rx) = tokio::sync::watch::channel(false);
    CONNECTION_STATUS
        .set(tx)
        .map_err(|_| anyhow::anyhow!("Connection status already initialized"))?;

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

    info!("[Tracks Connection] Starting reconnection loop - will continuously retry every {} seconds until connected", RETRY_INTERVAL_SECONDS);

    loop {
        retry_count += 1;
        info!(
            "[Tracks Connection] Attempting to connect (attempt {})...",
            retry_count
        );

        match try_connect().await {
            Ok(disconnect_rx) => {
                info!(
                    "[Tracks Connection] ✅ Successfully connected on attempt {}",
                    retry_count
                );
                retry_count = 0;

                // Update connection status to connected
                if let Some(status_tx) = CONNECTION_STATUS.get() {
                    let _ = status_tx.send(true);
                }

                // Wait for the disconnect signal
                info!("[Tracks Connection] Waiting for connection to be established and receiving updates...");
                let _ = disconnect_rx.await;
                warn!("[Tracks Connection] ❌ Connection lost, will retry immediately...");

                // Clear the tracks state when disconnected
                if let Some(state) = TRACKS_STATE.get() {
                    *state.write() = std::collections::HashMap::new();
                }

                // Clear the API instance
                if let Some(api_storage) = TRACKS_API.get() {
                    let mut guard = api_storage.lock().await;
                    *guard = None;
                }

                // Update connection status to disconnected
                if let Some(status_tx) = CONNECTION_STATUS.get() {
                    let _ = status_tx.send(false);
                }

                info!("[Tracks Connection] Retrying immediately after connection loss...");
            }
            Err(e) => {
                if retry_count == 1 || retry_count.is_multiple_of(10) {
                    warn!(
                        "[Tracks Connection] Connection attempt {} failed: {}",
                        retry_count, e
                    );
                }

                // Clear the tracks state when disconnected
                if let Some(state) = TRACKS_STATE.get() {
                    *state.write() = std::collections::HashMap::new();
                }

                // Clear the API instance
                if let Some(api_storage) = TRACKS_API.get() {
                    let mut guard = api_storage.lock().await;
                    *guard = None;
                }

                // Update connection status to disconnected
                if let Some(status_tx) = CONNECTION_STATUS.get() {
                    let _ = status_tx.send(false);
                }

                info!(
                    "[Tracks Connection] Retrying in {} seconds...",
                    RETRY_INTERVAL_SECONDS
                );
                tokio::time::sleep(tokio::time::Duration::from_secs(RETRY_INTERVAL_SECONDS)).await;
            }
        }
    }
}

/// Attempt a single connection to the tracks stream
#[cfg(not(target_arch = "wasm32"))]
async fn try_connect() -> Result<tokio::sync::oneshot::Receiver<()>> {
    // Get the shared endpoint
    let endpoint = get_shared_endpoint().ok_or_else(|| {
        anyhow::anyhow!("Shared endpoint not initialized. Call connect_to_reaper_tracks() first.")
    })?;

    // Get the REAPER endpoint address
    let endpoint_addr = get_reaper_endpoint_addr()?.ok_or_else(|| {
        anyhow::anyhow!("No endpoint ID found, REAPER extension may not be running")
    })?;

    // Create a NEW TrackApi instance on each connection attempt
    let tracks_api = TrackApi::connect(endpoint.clone(), endpoint_addr).map_err(|e| {
        warn!(
            "[Tracks Connection] Failed to create connection wrapper: {}",
            e
        );
        anyhow::anyhow!("Failed to create connection wrapper: {}", e)
    })?;

    // Subscribe to track updates
    info!("[Tracks Connection] Attempting to connect and subscribe to tracks stream...");
    let connection_timeout = tokio::time::Duration::from_secs(10);

    let mut tracks_rx = match tokio::time::timeout(connection_timeout, tracks_api.subscribe()).await
    {
        Ok(Ok(rx)) => {
            info!("[Tracks Connection] ✅ Successfully connected and subscribed to tracks stream");
            rx
        }
        Ok(Err(e)) => {
            warn!(
                "[Tracks Connection] ❌ Failed to connect/subscribe to stream: {}",
                e
            );
            return Err(anyhow::anyhow!(
                "Failed to connect/subscribe to stream: {}",
                e
            ));
        }
        Err(_) => {
            warn!("[Tracks Connection] ❌ Connection timeout after {:?} - REAPER extension may not be running or unreachable", connection_timeout);
            return Err(anyhow::anyhow!(
                "Connection timeout after {:?}",
                connection_timeout
            ));
        }
    };

    // Store the API instance AFTER subscribe() succeeds
    // Note: TrackApi doesn't implement Clone, so we store it directly
    if let Some(api_storage) = TRACKS_API.get() {
        let mut guard = api_storage.lock().await;
        *guard = Some(tracks_api);
        info!("[Tracks Connection] Stored tracks API instance");
    }

    // Create a channel to signal when the connection is lost
    let (tx, rx) = tokio::sync::oneshot::channel();

    // Spawn task to receive updates and update the global signal
    spawn(async move {
        use std::sync::atomic::{AtomicU64, Ordering};

        static RECV_COUNT: AtomicU64 = AtomicU64::new(0);

        info!("[Tracks Connection] ✅ Started receiving track updates - signal will now update");
        let mut message_count = 0u64;

        loop {
            match tracks_rx.recv().await {
                Ok(Some(msg)) => {
                    message_count += 1;
                    RECV_COUNT.fetch_add(1, Ordering::Relaxed);
                    handle_message(msg, &mut message_count, &RECV_COUNT).await;
                }
                Ok(None) => {
                    warn!("[Tracks Connection] Tracks stream ended (received {} messages) - will reconnect", message_count);
                    break;
                }
                Err(e) => {
                    error!(
                        error = %e,
                        error_debug = ?e,
                        message_count = message_count,
                        "[Tracks Connection] Error receiving track update (received {} messages)",
                        message_count
                    );
                    break;
                }
            }
        }

        // Cleanup and signal disconnect
        cleanup_on_disconnect().await;

        // Signal disconnect - this will trigger reconnection
        let _ = tx.send(());

        info!("[Tracks Connection] Update receiver task ended - reconnection loop will create a new one");
    });

    Ok(rx)
}

/// Handle a single track update message
async fn handle_message(
    msg: TrackUpdateMessage,
    message_count: &mut u64,
    recv_count: &std::sync::atomic::AtomicU64,
) {
    use std::sync::atomic::{AtomicU64, Ordering};
    use std::time::Instant;

    static UPDATE_COUNT: AtomicU64 = AtomicU64::new(0);
    static LAST_LOG: std::sync::OnceLock<std::sync::Mutex<Instant>> = std::sync::OnceLock::new();

    // Handle track updates
    match msg {
        TrackUpdateMessage::TracksChanged(msg) => {
            UPDATE_COUNT.fetch_add(1, Ordering::Relaxed);

            // Update TRACKS_STATE (project_name -> tracks)
            if let Some(state) = TRACKS_STATE.get() {
                let mut tracks_map = state.write();
                tracks_map.insert(msg.project_id.clone(), msg.tracks.clone());
            }

            // Also update SONG_TRACKS (song_index -> tracks) for UI reactivity
            // Map project_id to song_index by looking up in setlist structure
            if let Some(song_index) = find_song_index_by_project_name(&msg.project_id) {
                // Clone the entire HashMap and replace it to ensure Dioxus detects the change
                let mut song_tracks = SONG_TRACKS.write();
                let mut new_map = song_tracks.clone();
                new_map.insert(song_index, msg.tracks.clone());
                *song_tracks = new_map;
                drop(song_tracks); // Explicitly drop to trigger signal update
                debug!(
                    "[Tracks Connection] Updated SONG_TRACKS for song {} (project: {})",
                    song_index, msg.project_id
                );
            } else {
                debug!("[Tracks Connection] Could not find song_index for project: {} - SONG_TRACKS not updated", msg.project_id);
            }

            if *message_count == 1 {
                info!("[Tracks Connection] ✅ Received first tracks update for project: {} ({} tracks)", msg.project_id, msg.tracks.len());
            } else {
                debug!(
                    "[Tracks Connection] Updated tracks for project: {} ({} tracks)",
                    msg.project_id,
                    msg.tracks.len()
                );
            }
        }
        TrackUpdateMessage::TrackChanged(msg) => {
            UPDATE_COUNT.fetch_add(1, Ordering::Relaxed);

            use tracing::info;
            info!(
                project_id = %msg.project_id,
                track_index = msg.track_index,
                track_name = %msg.track.name,
                track_color = ?msg.track.color,
                "[Tracks Connection] 🎵 Track changed - updating state"
            );

            // Update TRACKS_STATE (project_name -> tracks)
            if let Some(state) = TRACKS_STATE.get() {
                let mut tracks_map = state.write();
                if let Some(tracks) = tracks_map.get_mut(&msg.project_id) {
                    if msg.track_index < tracks.len() {
                        // Replace entire track to ensure all fields are updated
                        tracks[msg.track_index] = msg.track.clone();
                    } else {
                        // Track index out of bounds - extend vector
                        let current_len = tracks.len();
                        tracks.resize_with(msg.track_index + 1, || {
                            let idx = current_len;
                            Track::new(format!("Track {}", idx))
                        });
                        tracks[msg.track_index] = msg.track.clone();
                    }
                } else {
                    // Project doesn't exist yet - create it
                    let mut new_tracks = Vec::with_capacity(msg.track_index + 1);
                    for i in 0..=msg.track_index {
                        new_tracks.push(Track::new(format!("Track {}", i)));
                    }
                    new_tracks[msg.track_index] = msg.track.clone();
                    tracks_map.insert(msg.project_id.clone(), new_tracks);
                }
            }

            // Also update SONG_TRACKS (song_index -> tracks) for UI reactivity
            if let Some(song_index) = find_song_index_by_project_name(&msg.project_id) {
                // Create a completely new HashMap to ensure Dioxus detects the change
                // Dioxus signals use reference equality, so modifying the existing HashMap
                // might not trigger reactivity. We need to create a new one.
                let mut song_tracks = SONG_TRACKS.write();
                let mut new_tracks_map = song_tracks.clone(); // Clone the entire HashMap

                let mut tracks = new_tracks_map.get(&song_index).cloned().unwrap_or_else(|| {
                    // Song doesn't have tracks yet - create empty vector
                    Vec::new()
                });

                // Update the specific track in the vector
                // Always replace the entire track to ensure ALL fields are updated (name, color, etc.)
                if msg.track_index < tracks.len() {
                    // Replace entire track - this ensures color, name, and all other fields are updated
                    tracks[msg.track_index] = msg.track.clone();
                } else {
                    // Track index out of bounds - extend vector
                    let current_len = tracks.len();
                    tracks.resize_with(msg.track_index + 1, || {
                        let idx = current_len;
                        Track::new(format!("Track {}", idx))
                    });
                    tracks[msg.track_index] = msg.track.clone();
                }

                // Insert the updated vector into the new HashMap
                new_tracks_map.insert(song_index, tracks);

                // Replace the entire HashMap to ensure Dioxus detects the change
                *song_tracks = new_tracks_map;
                drop(song_tracks); // Explicitly drop to trigger signal update
                info!(
                    track_index = msg.track_index,
                    song_index,
                    project_id = %msg.project_id,
                    track_name = %msg.track.name,
                    "[Tracks Connection] ✅ Updated SONG_TRACKS - UI should react"
                );
            } else {
                warn!(
                    project_id = %msg.project_id,
                    "[Tracks Connection] Could not find song_index for project - SONG_TRACKS not updated"
                );
            }

            debug!(
                "[Tracks Connection] Updated track {} for project: {} -> {}",
                msg.track_index, msg.project_id, msg.track.name
            );
        }
        TrackUpdateMessage::TrackAdded(msg) => {
            UPDATE_COUNT.fetch_add(1, Ordering::Relaxed);
            if let Some(state) = TRACKS_STATE.get() {
                let mut tracks_map = state.write();
                if let Some(tracks) = tracks_map.get_mut(&msg.project_id) {
                    if msg.track_index <= tracks.len() {
                        tracks.insert(msg.track_index, msg.track.clone());
                    } else {
                        // Track index out of bounds - extend vector
                        let current_len = tracks.len();
                        tracks.resize_with(msg.track_index + 1, || {
                            let idx = current_len;
                            Track::new(format!("Track {}", idx))
                        });
                        tracks[msg.track_index] = msg.track.clone();
                    }
                } else {
                    // Project doesn't exist yet - create it
                    let mut new_tracks = Vec::with_capacity(msg.track_index + 1);
                    for i in 0..=msg.track_index {
                        new_tracks.push(Track::new(format!("Track {}", i)));
                    }
                    new_tracks[msg.track_index] = msg.track.clone();
                    tracks_map.insert(msg.project_id.clone(), new_tracks);
                }
            }
            info!(
                "[Tracks Connection] Added track {} for project: {} -> {}",
                msg.track_index, msg.project_id, msg.track.name
            );
        }
        TrackUpdateMessage::TrackRemoved(msg) => {
            UPDATE_COUNT.fetch_add(1, Ordering::Relaxed);
            if let Some(state) = TRACKS_STATE.get() {
                let mut tracks_map = state.write();
                if let Some(tracks) = tracks_map.get_mut(&msg.project_id) {
                    if msg.track_index < tracks.len() {
                        tracks.remove(msg.track_index);
                    }
                }
            }
            info!(
                "[Tracks Connection] Removed track {} for project: {}",
                msg.track_index, msg.project_id
            );
        }
    }

    // Log performance metrics every 10 seconds
    let last_log = LAST_LOG.get_or_init(|| std::sync::Mutex::new(Instant::now()));
    let mut last_log_guard = last_log.lock().unwrap();
    if last_log_guard.elapsed().as_secs() >= 10 {
        let recv_rate = recv_count.swap(0, Ordering::Relaxed) / 10;
        let update_rate = UPDATE_COUNT.swap(0, Ordering::Relaxed) / 10;
        info!(
            "[Tracks Performance] IRPC receive: {} msg/s, UI updates: {} updates/s",
            recv_rate, update_rate
        );
        *last_log_guard = Instant::now();
    }
}

/// Cleanup signals when connection is lost
async fn cleanup_on_disconnect() {
    // Clear the tracks state when disconnected
    if let Some(state) = TRACKS_STATE.get() {
        *state.write() = std::collections::HashMap::new();
    }
    // Clear the API instance
    if let Some(api_storage) = TRACKS_API.get() {
        let mut guard = api_storage.lock().await;
        *guard = None;
    }
}
