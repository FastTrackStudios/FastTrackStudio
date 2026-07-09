//! WebSocket connection to the session desktop gateway.
//!
//! Connects to the desktop app's gateway-ws server via vox over WebSocket.
//! The desktop pushes setlist/transport events; the browser makes RPC calls
//! for navigation and control.
//!
//! ## Connection flow
//!
//! 1. Derive WebSocket URL from the page hostname (same host, port 3030)
//! 2. Connect via `vox-websocket::WsLink`
//! 3. Establish vox session with `WebClientServiceDispatcher` handler
//! 4. Initialize `Session` singleton with `SetlistServiceClient`
//! 5. Fetch initial setlist, then wait for push events

use std::cell::Cell;
use std::rc::Rc;
use std::time::Duration;

use dioxus::prelude::*;
use session_proto::SetlistServiceClient;
use vox_core;
use vox;
use vox_websocket::WsLink;

use session_ui::{
    ACTIVE_INDICES, ConnectionState, PLAYBACK_STATE, SETLIST_STRUCTURE, SONG_CHARTS, Session,
};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

/// Derive WebSocket URL from the current page hostname.
/// Uses port 3030 (gateway default) unless overridden via `?ws=` query param.
fn get_ws_url() -> String {
    web_sys::window()
        .and_then(|w| {
            let location = w.location();

            // Check for explicit ?ws= override
            if let Ok(search) = location.search() {
                for param in search.trim_start_matches('?').split('&') {
                    if let Some(url) = param.strip_prefix("ws=") {
                        return Some(url.to_string());
                    }
                }
            }

            // Default: same hostname, port 3030
            if let Ok(hostname) = location.hostname() {
                return Some(format!("ws://{}:3030/ws", hostname));
            }

            None
        })
        .unwrap_or_else(|| "ws://localhost:3030/ws".to_string())
}

/// Start the connection loop with automatic reconnection.
fn start_connection(mut state_signal: Signal<ConnectionState>) {
    let ws_url = get_ws_url();

    wasm_bindgen_futures::spawn_local(async move {
        let mut attempt = 0u32;
        let initial_backoff = Duration::from_millis(500);
        let max_backoff = Duration::from_secs(10);

        loop {
            state_signal.set(ConnectionState::Connecting);
            log(&format!(
                "[session-web] Connecting to {} (attempt {})...",
                ws_url,
                attempt + 1
            ));

            match try_connect_and_run(&ws_url, &mut state_signal).await {
                Ok(()) => {
                    log("[session-web] Connection closed, reconnecting...");
                    attempt = 0;
                }
                Err(e) => {
                    log(&format!("[session-web] Connection failed: {}", e));
                }
            }

            state_signal.set(ConnectionState::Disconnected);

            let backoff = initial_backoff.mul_f64(1.5f64.powi(attempt as i32));
            let backoff = backoff.min(max_backoff);
            log(&format!("[session-web] Reconnecting in {:?}...", backoff));

            gloo_timers::future::TimeoutFuture::new(backoff.as_millis() as u32).await;
            attempt = attempt.saturating_add(1);
        }
    });
}

/// Attempt to connect and run until the connection drops.
async fn try_connect_and_run(
    ws_url: &str,
    state_signal: &mut Signal<ConnectionState>,
) -> Result<(), String> {
    // Typed clients, one lane each (a vox caller is service-bound once
    // constructed): the RPC surface + the `#[subscribe]` stream sibling.
    async fn connect_link(url: &str) -> Result<vox_websocket::WsLink, String> {
        WsLink::connect(url)
            .await
            .map_err(|e| format!("WebSocket connect failed: {e:?}"))
    }
    let rpc: SetlistServiceClient = vox_core::initiator_on(connect_link(ws_url).await?)
        .establish()
        .await
        .map_err(|e| format!("handshake (rpc): {e:?}"))?;
    let stream: session_proto::services::setlist_service::SetlistServiceStreamClient =
        vox_core::initiator_on(connect_link(ws_url).await?)
            .establish()
            .await
            .map_err(|e| format!("handshake (stream): {e:?}"))?;

    log("[session-web] Connection established!");
    state_signal.set(ConnectionState::Connected);
    let _ = Session::init(rpc);

    // Seed, then fold the event stream into the UI globals. The stream
    // call stays in flight for the life of the subscription; when it ends
    // the connection is gone and the outer loop reconnects.
    if let Err(e) = fetch_setlist().await {
        log(&format!("[session-web] Failed to fetch setlist: {e}"));
    }

    let (tx, mut rx) = vox::channel::<session_proto::SetlistEvent>();
    let events_call = stream.events(tx);
    let recv_loop = async move {
        while let Ok(Some(ev)) = rx.recv().await {
            session_ui::apply_setlist_event(ev.get());
        }
    };
    futures_util::future::join(
        async {
            let _ = events_call.await;
        },
        recv_loop,
    )
    .await;

    Ok(())
}

/// Fetch the current setlist from the server and populate UI signals.
async fn fetch_setlist() -> Result<(), String> {
    log("[session-web] Fetching setlist...");

    let session = Session::get();
    let client = session.setlist();

    client
        .build_from_open_projects()
        .await
        .map_err(|e| format!("build_from_open_projects: {e:?}"))?;

    let setlist = client
        .setlist()
        .await
        .map_err(|e| format!("setlist: {e:?}"))?;

    log(&format!(
        "[session-web] Setlist '{}' with {} songs",
        setlist.name,
        setlist.songs.len()
    ));

    let songs = setlist.songs.clone();
    *SETLIST_STRUCTURE.write() = setlist;

    // Set initial active song
    match client.active_song().await {
        Ok(active) => {
            if let Some(idx) = songs
                .iter()
                .position(|s| s.project_guid == active.project_guid)
            {
                let mut indices = ACTIVE_INDICES.write();
                indices.song_index = Some(idx);
                indices.section_index = Some(0);
            }
        }
        Err(_) => {
            if !songs.is_empty() {
                let mut indices = ACTIVE_INDICES.write();
                indices.song_index = Some(0);
                indices.section_index = Some(0);
            }
        }
    }

    Ok(())
}

/// Dioxus hook: returns (connection_state, connect_callback).
pub fn use_connection() -> (Signal<ConnectionState>, Callback<()>) {
    let connection_state = use_signal(|| ConnectionState::Disconnected);

    let connect = use_callback(move |_: ()| {
        start_connection(connection_state);
    });

    (connection_state, connect)
}
