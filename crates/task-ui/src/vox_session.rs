//! Wasm-side vox session helper + status signal.
//!
//! Opens a WebSocket to `/vox` on the same origin as the page, runs
//! the vox handshake, and publishes the result via a `VoxStatus`
//! Dioxus context. The `/vox-test` route reads that context to show
//! whether the session is live. Service-specific clients
//! (`ChatServiceClient`, `AgentServiceClient`, …) are not yet
//! constructed — that lands when each trait grows the RPC method it
//! needs (e.g. `ChatService::send_message`).

use dioxus::prelude::*;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen_futures::spawn_local;

/// Lifecycle of the vox session, surfaced via Dioxus context.
#[derive(Clone, Debug, PartialEq)]
pub enum VoxStatus {
    /// Bootstrap hasn't run yet (e.g. native target, or before
    /// `spawn_session_bootstrap` fires).
    Idle,
    /// The WS connection is being established. Carries the URL we're
    /// dialing.
    Connecting { url: String },
    /// Handshake succeeded — the link is open + no service clients
    /// constructed yet.
    Connected { url: String },
    /// Establish failed at some stage. `stage` is `"connect"` or
    /// `"establish"`; `error` is the wrapped reason.
    Failed { stage: String, error: String },
}

impl VoxStatus {
    pub fn label(&self) -> &'static str {
        match self {
            Self::Idle => "idle",
            Self::Connecting { .. } => "connecting",
            Self::Connected { .. } => "connected",
            Self::Failed { .. } => "failed",
        }
    }

    pub fn is_connected(&self) -> bool {
        matches!(self, Self::Connected { .. })
    }
}

/// Context-friendly newtype so the test page can `use_context` it.
#[derive(Clone, Copy)]
pub struct VoxStatusCtx(pub Signal<VoxStatus>);

impl VoxStatusCtx {
    pub fn new() -> Self {
        Self(Signal::new(VoxStatus::Idle))
    }
}

impl Default for VoxStatusCtx {
    fn default() -> Self {
        Self::new()
    }
}

pub struct VoxSession {
    pub server_url: String,
}

impl VoxSession {
    /// Open a session against the same origin as the page at the
    /// fixed `/vox` route. Leaks the session handle so the link's IO
    /// loop survives this fn's stack frame.
    #[cfg(target_arch = "wasm32")]
    pub async fn connect_and_publish(mut status: Signal<VoxStatus>) -> Option<Self> {
        let server_url = vox_url();
        status.set(VoxStatus::Connecting {
            url: server_url.clone(),
        });
        // `WsLink::connect` calls `web_sys::WebSocket::new` which
        // throws (InvalidStateError / SecurityError / etc.) on bad
        // URLs and certain navigation states. wasm-bindgen surfaces
        // that as a non-catchable trap, which becomes the
        // "imported JS function … threw an error" console message
        // on every retry. Guard with `catch_unwind` so a throw fails
        // gracefully into VoxStatus::Failed instead of trapping the
        // whole wasm instance.
        let connect_fut = std::panic::AssertUnwindSafe(async {
            vox_websocket::WsLink::connect(&server_url).await
        });
        let connect_result = futures_util::FutureExt::catch_unwind(connect_fut).await;
        let link_result = match connect_result {
            Ok(r) => r,
            Err(_) => {
                tracing::warn!(%server_url, "vox WS connect panicked (likely WebSocket::new threw)");
                status.set(VoxStatus::Failed {
                    stage: "connect".into(),
                    error: "WebSocket::new threw (page not ready or invalid URL)".into(),
                });
                return None;
            }
        };
        match link_result {
            Ok(link) => {
                let establish_fut = std::panic::AssertUnwindSafe(async {
                    vox_core::acceptor_on(link)
                        .on_connection(())
                        .establish::<vox_core::NoopClient>()
                        .await
                });
                let establish_result = futures_util::FutureExt::catch_unwind(establish_fut).await;
                let final_result = match establish_result {
                    Ok(r) => r,
                    Err(_) => {
                        tracing::warn!(%server_url, "vox handshake panicked");
                        status.set(VoxStatus::Failed {
                            stage: "establish".into(),
                            error: "vox handshake threw (server may not be running an acceptor on /vox)".into(),
                        });
                        return None;
                    }
                };
                match final_result {
                    Ok(session) => {
                        std::mem::forget(session);
                        status.set(VoxStatus::Connected {
                            url: server_url.clone(),
                        });
                        Some(Self { server_url })
                    }
                    Err(e) => {
                        let err = format!("{e:?}");
                        tracing::warn!(?e, %server_url, "vox establish failed");
                        status.set(VoxStatus::Failed {
                            stage: "establish".into(),
                            error: err,
                        });
                        None
                    }
                }
            }
            Err(e) => {
                let err = format!("{e:?}");
                tracing::warn!(?e, %server_url, "vox WS connect failed");
                status.set(VoxStatus::Failed {
                    stage: "connect".into(),
                    error: err,
                });
                None
            }
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub async fn connect_and_publish(mut status: Signal<VoxStatus>) -> Option<Self> {
        let _ = status;
        None
    }
}

/// Build the `/vox` WS URL. Hardcoded to `127.0.0.1:9090` to match
/// `sync::sync_url` — `dx serve` runs on `:8765` but the task-server
/// (with the actual `/vox` route) listens on `:9090`. The dev shell
/// doesn't proxy WS routes. Production deployments override via the
/// reverse proxy and same-origin routing; that swap lives in this fn
/// when we have a real prod target to point at.
#[cfg(target_arch = "wasm32")]
fn vox_url() -> String {
    "ws://127.0.0.1:9090/vox".to_string()
}

#[cfg(not(target_arch = "wasm32"))]
#[allow(dead_code)]
fn vox_url() -> String {
    "ws://localhost:9090/vox".to_string()
}

/// Spawn the session bootstrap. Publishes lifecycle to the
/// `VoxStatusCtx` Signal in the surrounding Dioxus context. Safe to
/// call once at app start; subsequent invocations re-attempt the
/// connection (the previous attempt's leaked session stays open
/// until the WS itself closes).
#[cfg(target_arch = "wasm32")]
pub fn spawn_session_bootstrap(status: Signal<VoxStatus>) {
    spawn_local(async move {
        let _ = VoxSession::connect_and_publish(status).await;
    });
}

#[cfg(not(target_arch = "wasm32"))]
pub fn spawn_session_bootstrap(_status: Signal<VoxStatus>) {}
