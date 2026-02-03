//! Connection Management for FTS Control Web
//!
//! Handles WebSocket connection to the gateway-ws server with automatic
//! reconnection and streaming setlist/transport data to update global signals.
//!
//! ## Architecture
//!
//! The connection uses a simple retry loop pattern:
//! 1. UI calls `start_connection()` which starts the reconnection loop
//! 2. Once connected, fetches setlist and starts transport sync
//! 3. If connection drops, automatically reconnects and re-initializes
//! 4. If server not ready on startup, keeps retrying until successful

use std::cell::Cell;
use std::rc::Rc;
use std::time::Duration;

use dioxus::prelude::*;
use roam_session::{HandshakeConfig, NoDispatcher};
use roam_websocket::WsTransport;
use session_proto::{SetlistServiceClient, Song};
use session_ui::{
    ConnectionState, Session, TransportState, ACTIVE_INDICES, LATENCY_TRACKER, PLAYBACK_STATE,
    SETLIST_STRUCTURE, SONG_TRANSPORT,
};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

/// Get WebSocket URL from query params or derive from current page hostname
pub fn get_ws_url() -> String {
    web_sys::window()
        .and_then(|w| {
            let location = w.location();

            // First check for explicit ?ws= override in query params
            if let Ok(search) = location.search() {
                let params: Vec<_> = search.trim_start_matches('?').split('&').collect();
                for param in params {
                    if let Some(url) = param.strip_prefix("ws=") {
                        return Some(url.to_string());
                    }
                }
            }

            // No override - derive from current page's hostname
            // Use the same host but port 3030 for the gateway WebSocket
            if let Ok(hostname) = location.hostname() {
                return Some(format!("ws://{}:3030/ws", hostname));
            }

            None
        })
        .unwrap_or_else(|| "ws://localhost:3030/ws".to_string())
}

/// Initialize connection with automatic reconnection.
///
/// This starts a background task that:
/// 1. Attempts to connect to the gateway
/// 2. On success: initializes Session, fetches setlist, starts transport sync
/// 3. On failure: waits with exponential backoff and retries
/// 4. On disconnect: clears state and reconnects
pub fn start_connection(mut state_signal: Signal<ConnectionState>) {
    let ws_url = get_ws_url();

    wasm_bindgen_futures::spawn_local(async move {
        // Retry policy: infinite retries with exponential backoff
        // Start at 500ms, max 10s between attempts
        let mut attempt = 0u32;
        let initial_backoff = Duration::from_millis(500);
        let max_backoff = Duration::from_secs(10);
        let backoff_multiplier = 1.5f64;

        loop {
            state_signal.set(ConnectionState::Connecting);
            log(&format!(
                "[fts-control] Connecting to {} (attempt {})...",
                ws_url,
                attempt + 1
            ));

            match try_connect_and_run(&ws_url, &mut state_signal).await {
                Ok(()) => {
                    // Connection closed normally
                    log("[fts-control] Connection closed, reconnecting...");
                    attempt = 0; // Reset backoff on successful connection that later closed
                }
                Err(e) => {
                    log(&format!("[fts-control] Connection failed: {}", e));
                }
            }

            // Mark as disconnected and wait before retry
            state_signal.set(ConnectionState::Disconnected);

            // Calculate backoff with exponential increase
            let backoff = initial_backoff.mul_f64(backoff_multiplier.powi(attempt as i32));
            let backoff = backoff.min(max_backoff);

            log(&format!("[fts-control] Reconnecting in {:?}...", backoff));

            gloo_timers::future::TimeoutFuture::new(backoff.as_millis() as u32).await;
            attempt = attempt.saturating_add(1);
        }
    });
}

/// Attempt to connect and run the connection loop.
/// Returns when connection is lost or fails.
async fn try_connect_and_run(
    ws_url: &str,
    state_signal: &mut Signal<ConnectionState>,
) -> Result<(), String> {
    // Create WebSocket transport
    let transport = WsTransport::connect(ws_url)
        .await
        .map_err(|e| format!("WebSocket connect failed: {e}"))?;

    log("[fts-control] WebSocket connected, initiating roam handshake...");

    // Use roam's initiate_framed for the handshake
    let (handle, _incoming, driver) =
        roam_session::initiate_framed(transport, HandshakeConfig::default(), NoDispatcher)
            .await
            .map_err(|e| format!("Handshake failed: {e}"))?;

    log("[fts-control] Connection established!");
    state_signal.set(ConnectionState::Connected);

    // IMPORTANT: Spawn the driver FIRST, before making any RPC calls!
    // The driver processes incoming responses - without it running, RPC calls will hang.
    let driver_done = Rc::new(Cell::new(false));
    let driver_done_clone = driver_done.clone();

    wasm_bindgen_futures::spawn_local(async move {
        if let Err(e) = driver.run().await {
            log(&format!("[fts-control] Driver ended: {e}"));
        } else {
            log("[fts-control] Driver ended normally");
        }
        driver_done_clone.set(true);
    });

    log("[fts-control] Driver spawned");

    // Create SetlistServiceClient and initialize Session singleton
    let setlist_client = SetlistServiceClient::new(handle.clone());

    // Re-initialize Session (it may have been initialized before a disconnect)
    reinit_session(setlist_client);

    log("[fts-control] Session initialized with SetlistServiceClient");

    // Fetch initial setlist (driver is now running, so RPC calls will work)
    if let Err(e) = fetch_setlist().await {
        log(&format!("[fts-control] Failed to fetch setlist: {e}"));
        // Continue anyway - we'll get updates from the stream
    }

    // Create a shared flag to detect when connection is lost
    let connection_lost = Rc::new(Cell::new(false));
    let connection_lost_clone = connection_lost.clone();

    // Start transport sync - it will set the flag when connection is lost
    start_transport_sync_task(connection_lost_clone);

    // Poll until either driver or sync task signals connection loss
    loop {
        gloo_timers::future::TimeoutFuture::new(100).await;

        if driver_done.get() || connection_lost.get() {
            log("[fts-control] Connection lost detected");
            break;
        }
    }

    Ok(())
}

/// Re-initialize Session singleton, replacing if it already exists
fn reinit_session(setlist_client: SetlistServiceClient) {
    // Try to init - if it fails because already initialized, that's expected
    // The Session will still work with the old client until we implement proper reset
    let _ = Session::init(setlist_client);
}

/// Fetch setlist from the server and update global signals
pub async fn fetch_setlist() -> Result<(), String> {
    log("[fts-control] Fetching setlist from server...");

    let session = Session::get();
    let setlist_client = session.setlist();

    // First, trigger a build from open projects
    log("[fts-control] Calling build_from_open_projects...");
    setlist_client
        .build_from_open_projects()
        .await
        .map_err(|e| format!("Failed to build setlist: {e}"))?;

    log("[fts-control] Setlist built, fetching structure...");

    // Get the full setlist (now returns complete Setlist with all songs and sections)
    let setlist = setlist_client
        .get_setlist()
        .await
        .map_err(|e| format!("Failed to get setlist: {e}"))?;

    match setlist {
        Some(setlist) => {
            log(&format!(
                "[fts-control] Got setlist '{}' with {} songs from server",
                setlist.name,
                setlist.songs.len()
            ));

            // Store the songs for later index lookup
            let songs: Vec<Song> = setlist.songs.clone();

            // Update SETLIST_STRUCTURE directly with the full setlist
            *SETLIST_STRUCTURE.write() = setlist;

            // Get active song to set initial indices
            match setlist_client.get_active_song().await {
                Ok(Some(active_song)) => {
                    // Find the song index by matching project_guid
                    let song_idx = songs
                        .iter()
                        .position(|s| s.project_guid == active_song.project_guid);

                    if let Some(idx) = song_idx {
                        let mut indices = ACTIVE_INDICES.write();
                        indices.song_index = Some(idx);
                        indices.section_index = Some(0); // Start at first section
                        log(&format!(
                            "[fts-control] Active song set to index {idx}: {}",
                            active_song.name
                        ));
                    }
                }
                Ok(None) => {
                    // No active song, set to first song if available
                    if !songs.is_empty() {
                        let mut indices = ACTIVE_INDICES.write();
                        indices.song_index = Some(0);
                        indices.section_index = Some(0);
                        log("[fts-control] No active song, defaulting to first song");
                    }
                }
                Err(e) => {
                    log(&format!("[fts-control] Failed to get active song: {e}"));
                }
            }

            log("[fts-control] Setlist loaded successfully");
        }
        None => {
            log("[fts-control] No setlist available from server");
        }
    }

    Ok(())
}

/// Start the transport sync task
fn start_transport_sync_task(connection_lost: Rc<Cell<bool>>) {
    wasm_bindgen_futures::spawn_local(async move {
        log("[fts-control] Starting session-based transport sync...");

        // Get the setlist client from Session
        let session = Session::get();
        let setlist_client = session.setlist();

        // Create a channel for receiving setlist events
        let (tx, mut rx) = roam::channel::<session_proto::SetlistEvent>();

        // Subscribe to setlist events from the Session service
        // This gives us transport state for ALL songs, plus active song/section changes
        match setlist_client.subscribe(tx).await {
            Ok(()) => {
                log("[fts-control] Subscribed to SetlistService events");

                while let Ok(Some(event)) = rx.recv().await {
                    match event {
                        session_proto::SetlistEvent::SetlistChanged(setlist) => {
                            log(&format!(
                                "[fts-control] Setlist changed: {} songs",
                                setlist.songs.len()
                            ));
                            *SETLIST_STRUCTURE.write() = setlist;
                        }

                        session_proto::SetlistEvent::ActiveIndicesChanged(indices) => {
                            // Update ACTIVE_INDICES
                            *ACTIVE_INDICES.write() = indices.clone();

                            // Update PLAYBACK_STATE based on active song
                            if indices.is_playing {
                                *PLAYBACK_STATE.write() = daw_proto::PlayState::Playing;
                            } else {
                                *PLAYBACK_STATE.write() = daw_proto::PlayState::Stopped;
                            }
                        }

                        session_proto::SetlistEvent::TransportUpdate(transports) => {
                            // Get current active song index
                            let active_song_index = ACTIVE_INDICES.read().song_index;

                            // Update SONG_TRANSPORT for ALL songs
                            let mut song_transport = SONG_TRANSPORT.write();
                            for transport in transports {
                                song_transport.insert(
                                    transport.song_index,
                                    TransportState {
                                        position: transport.position,
                                        bpm: transport.bpm,
                                        time_sig_num: transport.time_sig_num as i32,
                                        time_sig_denom: transport.time_sig_denom as i32,
                                        is_playing: transport.is_playing,
                                        is_looping: transport.is_looping,
                                        loop_region: None,
                                    },
                                );

                                // Update global PLAYBACK_STATE based on ACTIVE song's state
                                if Some(transport.song_index) == active_song_index {
                                    let old_playing = *PLAYBACK_STATE.read();
                                    let new_playing = if transport.is_playing {
                                        daw_proto::PlayState::Playing
                                    } else {
                                        daw_proto::PlayState::Stopped
                                    };

                                    // Check if play state actually changed
                                    if old_playing != new_playing {
                                        *PLAYBACK_STATE.write() = new_playing;

                                        // Complete latency measurement if we were tracking
                                        if let Some(latency) =
                                            LATENCY_TRACKER.write().complete_play_toggle()
                                        {
                                            log(&format!(
                                                "[fts-control] Play toggle latency: {:.1}ms",
                                                latency
                                            ));
                                        }
                                    }

                                    // Also update ACTIVE_INDICES progress from transport
                                    let mut indices = ACTIVE_INDICES.write();
                                    indices.song_progress = Some(transport.progress);
                                    indices.section_progress = transport.section_progress;
                                    indices.section_index = transport.section_index;
                                    indices.is_playing = transport.is_playing;
                                    indices.looping = transport.is_looping;
                                }
                            }
                        }

                        session_proto::SetlistEvent::SongEntered { index, song } => {
                            log(&format!(
                                "[fts-control] Entered song {}: {}",
                                index, song.name
                            ));
                        }

                        session_proto::SetlistEvent::SongExited { index } => {
                            log(&format!("[fts-control] Exited song {}", index));
                        }

                        session_proto::SetlistEvent::SectionEntered {
                            song_index,
                            section_index,
                            section,
                        } => {
                            log(&format!(
                                "[fts-control] Entered section {}.{}: {}",
                                song_index, section_index, section.name
                            ));
                        }

                        session_proto::SetlistEvent::SectionExited {
                            song_index,
                            section_index,
                        } => {
                            log(&format!(
                                "[fts-control] Exited section {}.{}",
                                song_index, section_index
                            ));
                        }

                        session_proto::SetlistEvent::PositionChanged { .. } => {
                            // Legacy event, ignored - we use TransportUpdate now
                        }
                    }
                }

                log("[fts-control] Setlist event stream ended (connection lost)");
            }
            Err(e) => {
                log(&format!(
                    "[fts-control] Failed to subscribe to setlist events: {e}"
                ));
            }
        }

        // Signal that connection is lost
        connection_lost.set(true);
    });
}

/// Hook for managing connection state in a component
pub fn use_connection() -> (Signal<ConnectionState>, Callback<()>) {
    let connection_state = use_signal(|| ConnectionState::Disconnected);

    // Callback to initiate connection
    let connect_callback = use_callback(move |_: ()| {
        start_connection(connection_state);
    });

    (connection_state, connect_callback)
}
