//! WebSocket connection to the desktop gateway.
//!
//! Connects via vox over WebSocket, initializes Session singleton,
//! fetches initial setlist, and auto-reconnects on failure.

use std::cell::Cell;
use std::rc::Rc;
use std::time::Duration;

use dioxus::prelude::*;
use session::{SetlistServiceClient, WebClientServiceDispatcher};
use vox_websocket::WsLink;

use crate::web_client_handler::WebClientHandler;
use session_ui::{ConnectionState, Session, ACTIVE_INDICES, SETLIST_STRUCTURE};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

fn get_ws_url() -> String {
    web_sys::window()
        .and_then(|w| {
            let location = w.location();
            if let Ok(search) = location.search() {
                for param in search.trim_start_matches('?').split('&') {
                    if let Some(url) = param.strip_prefix("ws=") {
                        return Some(url.to_string());
                    }
                }
            }
            if let Ok(hostname) = location.hostname() {
                return Some(format!("ws://{}:3030/ws", hostname));
            }
            None
        })
        .unwrap_or_else(|| "ws://localhost:3030/ws".to_string())
}

fn start_connection(mut state_signal: Signal<ConnectionState>) {
    let ws_url = get_ws_url();

    wasm_bindgen_futures::spawn_local(async move {
        let mut attempt = 0u32;
        let initial_backoff = Duration::from_millis(500);
        let max_backoff = Duration::from_secs(10);

        loop {
            state_signal.set(ConnectionState::Connecting);
            log(&format!(
                "[fts-web] Connecting to {} (attempt {})...",
                ws_url,
                attempt + 1
            ));

            match try_connect_and_run(&ws_url, &mut state_signal).await {
                Ok(()) => {
                    log("[fts-web] Connection closed, reconnecting...");
                    attempt = 0;
                }
                Err(e) => {
                    log(&format!("[fts-web] Connection failed: {}", e));
                }
            }

            state_signal.set(ConnectionState::Disconnected);

            let backoff = initial_backoff.mul_f64(1.5f64.powi(attempt as i32));
            let backoff = backoff.min(max_backoff);
            log(&format!("[fts-web] Reconnecting in {:?}...", backoff));

            gloo_timers::future::TimeoutFuture::new(backoff.as_millis() as u32).await;
            attempt = attempt.saturating_add(1);
        }
    });
}

async fn try_connect_and_run(
    ws_url: &str,
    state_signal: &mut Signal<ConnectionState>,
) -> Result<(), String> {
    let link = WsLink::connect(ws_url)
        .await
        .map_err(|e| format!("WebSocket connect failed: {e}"))?;

    log("[fts-web] WebSocket connected, initiating vox handshake...");

    let handler = WebClientServiceDispatcher::new(WebClientHandler);
    let handshake_result = vox::HandshakeResult {
        role: vox::SessionRole::Initiator,
        our_settings: vox::ConnectionSettings {
            parity: vox::Parity::Odd,
            max_concurrent_requests: 64,
        },
        peer_settings: vox::ConnectionSettings {
            parity: vox::Parity::Even,
            max_concurrent_requests: 64,
        },
        peer_supports_retry: true,
        session_resume_key: None,
        peer_resume_key: None,
        our_schema: vec![],
        peer_schema: vec![],
    };
    let (caller, _session_handle) =
        vox::initiator_conduit(vox::BareConduit::new(link), handshake_result)
            .establish::<vox::DriverCaller>(handler)
            .await
            .map_err(|e| format!("Handshake failed: {e:?}"))?;

    log("[fts-web] Connection established!");
    state_signal.set(ConnectionState::Connected);

    let handle = vox::ErasedCaller::new(caller);
    let setlist_client = SetlistServiceClient::new(handle);
    let _ = Session::init(setlist_client);

    log("[fts-web] Session initialized");

    if let Err(e) = fetch_setlist().await {
        log(&format!("[fts-web] Failed to fetch setlist: {e}"));
    }

    // Keep alive — poll connection health
    let connection_lost = Rc::new(Cell::new(false));
    let lost_clone = connection_lost.clone();

    let session = Session::get();
    let client = session.setlist().clone();

    wasm_bindgen_futures::spawn_local(async move {
        loop {
            gloo_timers::future::TimeoutFuture::new(5000).await;
            if client.get_audio_latency_info().await.is_err() {
                log("[fts-web] Connection health check failed");
                lost_clone.set(true);
                break;
            }
        }
    });

    loop {
        gloo_timers::future::TimeoutFuture::new(100).await;
        if connection_lost.get() {
            break;
        }
    }

    Ok(())
}

async fn fetch_setlist() -> Result<(), String> {
    log("[fts-web] Fetching setlist...");

    let session = Session::get();
    let client = session.setlist();

    client
        .build_from_open_projects()
        .await
        .map_err(|e| format!("build_from_open_projects: {e:?}"))?;

    let setlist = client
        .get_setlist()
        .await
        .map_err(|e| format!("get_setlist: {e:?}"))?;

    log(&format!(
        "[fts-web] Setlist '{}' with {} songs",
        setlist.name,
        setlist.songs.len()
    ));

    let songs = setlist.songs.clone();
    *SETLIST_STRUCTURE.write() = setlist;

    match client.get_active_song().await {
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

pub fn use_connection() -> (Signal<ConnectionState>, Callback<()>) {
    let connection_state = use_signal(|| ConnectionState::Disconnected);

    let connect = use_callback(move |_: ()| {
        start_connection(connection_state);
    });

    (connection_state, connect)
}
