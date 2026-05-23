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
    #[must_use]
    pub fn label(&self) -> &'static str {
        match self {
            Self::Idle => "idle",
            Self::Connecting { .. } => "connecting",
            Self::Connected { .. } => "connected",
            Self::Failed { .. } => "failed",
        }
    }

    #[must_use]
    pub fn is_connected(&self) -> bool {
        matches!(self, Self::Connected { .. })
    }
}

/// Context-friendly newtype so the test page can `use_context` it.
#[derive(Clone, Copy)]
pub struct VoxStatusCtx(pub Signal<VoxStatus>);

impl VoxStatusCtx {
    #[must_use]
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
        let Ok(link_result) = connect_result else {
            tracing::warn!(%server_url, "vox WS connect panicked (likely WebSocket::new threw)");
            status.set(VoxStatus::Failed {
                stage: "connect".into(),
                error: "WebSocket::new threw (page not ready or invalid URL)".into(),
            });
            return None;
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
                let Ok(final_result) = establish_result else {
                    tracing::warn!(%server_url, "vox handshake panicked");
                    status.set(VoxStatus::Failed {
                        stage: "establish".into(),
                        error:
                            "vox handshake threw (server may not be running an acceptor on /vox)"
                                .into(),
                    });
                    return None;
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
    pub async fn connect_and_publish(status: Signal<VoxStatus>) -> Option<Self> {
        let _ = status;
        None
    }
}

/// Build the `/vox` WS URL.
///
/// Resolution order (build time, since wasm can't read process env):
/// 1. `TASK_VOX_URL_WEB` env var if set at compile time — baked into
///    the wasm bundle. Set this in CI / Nix / wherever you build for
///    production:
///
///        TASK_VOX_URL_WEB=wss://task.example.com/vox dx build --web --release
///
/// 2. Falls back to `ws://127.0.0.1:9090/vox` for local dev (matches
///    `just dev`'s defaults — `dx serve` on :8765, task-server on
///    :9090, no reverse-proxy in the dev shell).
pub const DEFAULT_VOX_URL: &str = "ws://127.0.0.1:9090/vox";

/// Resolved at call time so each surface can choose its default.
///
/// - **wasm** (browser bundle): the URL is baked in at compile
///   time via `TASK_VOX_URL_WEB`; falls back to the local-dev
///   default. Browser builds always try to sync (no server =
///   visible failure chip).
/// - **native** (desktop binary): reads `TASK_VOX_URL` from the
///   process environment at runtime; returns an EMPTY string
///   when unset so the desktop app starts in offline-only mode
///   without spamming connection-refused errors. Set
///   `TASK_VOX_URL=ws://host:9090/vox` to opt into syncing.
#[must_use]
pub fn vox_url() -> String {
    #[cfg(target_arch = "wasm32")]
    {
        option_env!("TASK_VOX_URL_WEB")
            .unwrap_or(DEFAULT_VOX_URL)
            .to_string()
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        std::env::var("TASK_VOX_URL").unwrap_or_default()
    }
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
