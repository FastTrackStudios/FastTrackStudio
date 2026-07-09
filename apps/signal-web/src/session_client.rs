//! Session-engine client — connects the rig remote to a standalone
//! `session` gateway (default port 3030) so the Session view renders the
//! performance layout live: songs, sections, charts + chords.
//!
//! Same shape as the rig's own connection: typed clients over
//! `vox_core::initiator_on(WsLink)`, seed via RPC, then fold the
//! `#[subscribe]` event stream into session-ui's global signals.

use std::time::Duration;

use dioxus::prelude::*;
use session_proto::SetlistServiceClient;
use session_proto::services::setlist_service::SetlistServiceStreamClient;
use session_ui::{ConnectionState, Session};
use vox_websocket::WsLink;

/// Gateway URL: same host as the page, port 3030 (session gateway default).
fn session_ws_url() -> String {
    let host = web_sys::window()
        .and_then(|w| w.location().hostname().ok())
        .filter(|h| !h.is_empty())
        .unwrap_or_else(|| "127.0.0.1".to_string());
    format!("ws://{host}:3030/ws")
}

/// Kick off the session connection loop (retries forever; harmless when no
/// session engine is running — the Session view just shows disconnected).
pub fn start(mut state: Signal<ConnectionState>) {
    wasm_bindgen_futures::spawn_local(async move {
        let url = session_ws_url();
        let mut attempt = 0u32;
        loop {
            state.set(ConnectionState::Connecting);
            match connect_once(&url, &mut state).await {
                Ok(()) => attempt = 0,
                Err(e) => tracing::debug!("session gateway: {e}"),
            }
            state.set(ConnectionState::Disconnected);
            let backoff = Duration::from_millis(500)
                .mul_f64(1.5f64.powi(attempt.min(8) as i32))
                .min(Duration::from_secs(10));
            gloo_timers::future::TimeoutFuture::new(backoff.as_millis() as u32).await;
            attempt = attempt.saturating_add(1);
        }
    });
}

async fn connect_link(url: &str) -> Result<WsLink, String> {
    WsLink::connect(url)
        .await
        .map_err(|e| format!("connect: {e:?}"))
}

async fn connect_once(url: &str, state: &mut Signal<ConnectionState>) -> Result<(), String> {
    // One lane per typed client: the RPC surface + its stream sibling.
    let rpc: SetlistServiceClient = vox_core::initiator_on(connect_link(url).await?)
        .establish()
        .await
        .map_err(|e| format!("handshake (rpc): {e:?}"))?;
    let stream: SetlistServiceStreamClient = vox_core::initiator_on(connect_link(url).await?)
        .establish()
        .await
        .map_err(|e| format!("handshake (stream): {e:?}"))?;

    state.set(ConnectionState::Connected);
    let _ = Session::init(rpc);

    // Seed: build from whatever projects the engine has open, snapshot the
    // setlist, then stream every change.
    let session = Session::get();
    let client = session.setlist().clone();
    if client.build_from_open_projects().await.is_ok() {
        if let Ok(setlist) = client.setlist().await {
            *session_ui::SETLIST_STRUCTURE.write() = setlist;
        }
    }

    let (tx, mut rx) = vox::channel::<session_proto::SetlistEvent>();
    let events_call = stream.events(tx);
    let recv_loop = async move {
        while let Ok(Some(ev)) = rx.recv().await {
            session_ui::apply_setlist_event(ev.get());
        }
    };
    // The subscribe call stays in flight for the life of the stream; when
    // either side ends, the connection is gone → outer loop reconnects.
    futures_util::future::join(
        async {
            let _ = events_call.await;
        },
        recv_loop,
    )
    .await;

    Ok(())
}
