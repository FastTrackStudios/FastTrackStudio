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
use roam_websocket::WsLink;
use session::{SetlistServiceClient, Song, WebClientServiceDispatcher};

use crate::web_client_handler::WebClientHandler;
use session_ui::{
    ConnectionState, LatencyInfo, Session, TransportState, ACTIVE_INDICES,
    ACTIVE_PLAYBACK_IS_PLAYING, ACTIVE_PLAYBACK_MUSICAL, AUDIO_LATENCY_SECONDS, LATENCY_INFO,
    LATENCY_TRACKER, PLAYBACK_STATE, SETLIST_STRUCTURE, SONG_CHARTS, SONG_TRANSPORT,
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
    let link = WsLink::connect(ws_url)
        .await
        .map_err(|e| format!("WebSocket connect failed: {e}"))?;

    log("[fts-control] WebSocket connected, initiating roam handshake...");

    // Use the new roam initiator builder API for the handshake.
    // Register WebClientServiceDispatcher so the desktop can push events to us.
    let handler = WebClientServiceDispatcher::new(WebClientHandler);
    let (caller, _session_handle) = roam::initiator(link)
        .spawn_fn(|fut| {
            wasm_bindgen_futures::spawn_local(async move {
                fut.await;
            });
        })
        .max_concurrent_requests(64)
        .establish::<roam::DriverCaller>(handler)
        .await
        .map_err(|e| format!("Handshake failed: {e:?}"))?;

    log("[fts-control] Connection established!");
    state_signal.set(ConnectionState::Connected);

    log("[fts-control] Session task spawned");

    // Create SetlistServiceClient and initialize Session singleton
    let handle = roam::ErasedCaller::new(caller);
    let setlist_client = SetlistServiceClient::new(handle);

    // Re-initialize Session (it may have been initialized before a disconnect)
    reinit_session(setlist_client);

    log("[fts-control] Session initialized with SetlistServiceClient");

    // Fetch initial setlist (session is now running, so RPC calls will work)
    if let Err(e) = fetch_setlist().await {
        log(&format!("[fts-control] Failed to fetch setlist: {e}"));
        // Continue anyway - we'll get updates from the stream
    }

    // Create a shared flag to detect when connection is lost
    let connection_lost = Rc::new(Cell::new(false));
    let connection_lost_clone = connection_lost.clone();

    // Start transport sync - it will set the flag when connection is lost
    start_transport_sync_task(connection_lost_clone);

    // Poll until sync task signals connection loss
    loop {
        gloo_timers::future::TimeoutFuture::new(100).await;

        if connection_lost.get() {
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
        .map_err(|e| format!("Failed to build setlist: {e:?}"))?;

    log("[fts-control] Setlist built, fetching structure...");

    // Get the full setlist (now returns Setlist directly, not Option<Setlist>)
    let setlist = setlist_client
        .get_setlist()
        .await
        .map_err(|e| format!("Failed to get setlist: {e:?}"))?;

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
        Ok(active_song) => {
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
        Err(e) => {
            // No active song or error, set to first song if available
            if !songs.is_empty() {
                let mut indices = ACTIVE_INDICES.write();
                indices.song_index = Some(0);
                indices.section_index = Some(0);
                log(&format!(
                    "[fts-control] No active song ({e:?}), defaulting to first song"
                ));
            }
        }
    }

    log("[fts-control] Setlist loaded successfully");

    Ok(())
}

/// Start the transport sync task
fn start_transport_sync_task(connection_lost: Rc<Cell<bool>>) {
    wasm_bindgen_futures::spawn_local(async move {
        log("[fts-control] Starting session-based transport sync...");

        // Get the setlist client from Session
        let session = Session::get();
        let setlist_client = session.setlist();

        // Fetch initial audio latency info
        match setlist_client.get_audio_latency_info().await {
            Ok(info) => {
                let output_ms = info.output_seconds * 1000.0;
                let input_ms = if info.sample_rate > 0 {
                    (info.input_samples as f64 / info.sample_rate as f64) * 1000.0
                } else {
                    0.0
                };

                *AUDIO_LATENCY_SECONDS.write() = info.output_seconds;
                *LATENCY_INFO.write() = LatencyInfo {
                    input_ms,
                    output_ms,
                    network_rtt_ms: 0.0, // Will be measured separately
                    sample_rate: info.sample_rate,
                    is_running: info.is_running,
                };

                log(&format!(
                    "[fts-control] Audio latency: input={:.1}ms, output={:.1}ms",
                    input_ms, output_ms
                ));
            }
            Err(e) => {
                log(&format!(
                    "[fts-control] Failed to get audio latency: {e:?}"
                ));
            }
        }

        // Start a background task to periodically refresh audio latency and measure network RTT
        let setlist_client_clone = setlist_client.clone();
        wasm_bindgen_futures::spawn_local(async move {
            // Rolling average of network RTT measurements
            let mut rtt_samples: Vec<f64> = Vec::with_capacity(10);

            loop {
                // Wait 5 seconds between latency updates
                gloo_timers::future::TimeoutFuture::new(5000).await;

                // Measure network RTT by timing the get_audio_latency_info call
                let start = web_sys::window()
                    .and_then(|w| w.performance())
                    .map(|p| p.now())
                    .unwrap_or(0.0);

                if let Ok(info) = setlist_client_clone.get_audio_latency_info().await {
                    let end = web_sys::window()
                        .and_then(|w| w.performance())
                        .map(|p| p.now())
                        .unwrap_or(0.0);

                    let rtt = end - start;

                    // Add to rolling average (keep last 10 samples)
                    rtt_samples.push(rtt);
                    if rtt_samples.len() > 10 {
                        rtt_samples.remove(0);
                    }

                    // Calculate average RTT, filtering out spikes (>3x average)
                    let avg_rtt = if rtt_samples.len() >= 3 {
                        let sum: f64 = rtt_samples.iter().sum();
                        let avg = sum / rtt_samples.len() as f64;
                        // Filter out spikes for final calculation
                        let filtered: Vec<_> =
                            rtt_samples.iter().filter(|&&r| r < avg * 3.0).collect();
                        if filtered.is_empty() {
                            avg
                        } else {
                            filtered.iter().copied().sum::<f64>() / filtered.len() as f64
                        }
                    } else {
                        rtt
                    };

                    let output_ms = info.output_seconds * 1000.0;
                    let input_ms = if info.sample_rate > 0 {
                        (info.input_samples as f64 / info.sample_rate as f64) * 1000.0
                    } else {
                        0.0
                    };

                    // Update AUDIO_LATENCY_SECONDS for transport compensation
                    let current = *AUDIO_LATENCY_SECONDS.read();
                    if (info.output_seconds - current).abs() > 0.0001 {
                        *AUDIO_LATENCY_SECONDS.write() = info.output_seconds;
                    }

                    // Update full latency info including network RTT
                    let new_info = LatencyInfo {
                        input_ms,
                        output_ms,
                        network_rtt_ms: avg_rtt,
                        sample_rate: info.sample_rate,
                        is_running: info.is_running,
                    };

                    let current_info = LATENCY_INFO.read().clone();
                    if new_info != current_info {
                        *LATENCY_INFO.write() = new_info;
                        log(&format!(
                            "[fts-control] Latency updated: input={:.1}ms, output={:.1}ms, network={:.1}ms",
                            input_ms, output_ms, avg_rtt
                        ));
                    }
                }
            }
        });

        // Events are now pushed from the desktop via WebClientService.push_event().
        // The WebClientHandler (registered during handshake) writes directly to GlobalSignals.
        // We just need to keep this task alive to detect connection loss.
        log("[fts-control] Waiting for push events from desktop...");

        loop {
            // Periodically check connection health via a lightweight RPC call
            gloo_timers::future::TimeoutFuture::new(5000).await;

            // Use the latency ping as a connection health check
            if setlist_client.get_audio_latency_info().await.is_err() {
                log("[fts-control] Connection health check failed");
                break;
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
