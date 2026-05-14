//! Minimal wasm-side vox session helper.
//!
//! Opens a WebSocket to `/vox` on the same origin as the page, runs
//! the vox handshake, and leaks the session handle so the connection
//! stays open for the lifetime of the page. Service-specific clients
//! (`ChatServiceClient`, `AgentServiceClient`, …) are not yet
//! constructed — that lands when each trait grows the RPC method it
//! needs (e.g. `ChatService::send_message`).
//!
//! The handshake establishes the rails so the next slice of work
//! only needs to add the method-level wiring, not the transport.

#[cfg(target_arch = "wasm32")]
use wasm_bindgen_futures::spawn_local;

pub struct VoxSession {
    pub server_url: String,
}

impl VoxSession {
    /// Open a session against the same origin as the page, at the
    /// fixed `/vox` route. Returns `None` on any failure — caller
    /// falls back to local-only behaviour. Leaks the session handle
    /// so the link's IO loop survives this fn's stack frame.
    #[cfg(target_arch = "wasm32")]
    pub async fn connect() -> Option<Self> {
        let server_url = vox_url();
        match vox_websocket::WsLink::connect(&server_url).await {
            Ok(link) => {
                let session = match vox_core::acceptor_on(link)
                    .on_connection(())
                    .establish::<vox_core::NoopClient>()
                    .await
                {
                    Ok(s) => s,
                    Err(e) => {
                        tracing::warn!(?e, %server_url, "vox establish failed");
                        return None;
                    }
                };
                std::mem::forget(session);
                Some(Self { server_url })
            }
            Err(e) => {
                tracing::warn!(?e, %server_url, "vox WS connect failed");
                None
            }
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub async fn connect() -> Option<Self> {
        None
    }
}

/// Build the `/vox` WS URL from `window.location`. Same scheme/host
/// as the page; the `ws:` / `wss:` chosen by mirroring http/https.
#[cfg(target_arch = "wasm32")]
fn vox_url() -> String {
    let win = web_sys::window().expect("no window");
    let loc = win.location();
    let proto = loc.protocol().unwrap_or_else(|_| "http:".into());
    let host = loc.host().unwrap_or_else(|_| "localhost".into());
    let ws_proto = if proto == "https:" { "wss" } else { "ws" };
    format!("{ws_proto}://{host}/vox")
}

#[cfg(not(target_arch = "wasm32"))]
fn vox_url() -> String {
    "ws://localhost:8080/vox".to_string()
}

/// Establish the vox session once at app start and stash it in a
/// Dioxus context so route components can grab it. For now this is a
/// best-effort connect with no per-service client construction.
#[cfg(target_arch = "wasm32")]
pub fn spawn_session_bootstrap() {
    spawn_local(async move {
        let Some(session) = VoxSession::connect().await else {
            tracing::info!("vox session not established; chat falls back to local sim");
            return;
        };
        tracing::info!(url = %session.server_url, "vox session established (no service clients wired yet)");
    });
}

#[cfg(not(target_arch = "wasm32"))]
pub fn spawn_session_bootstrap() {}
