//! Vox client lifecycle for the app shell.
//!
//! Screens program against the generated `ExampleRepoClient` /
//! `ExampleServiceClient` and never care where the data lives — that's
//! the [`Transport`] choice, made once at the app root:
//!
//! - [`Transport::Remote`] — talk to `app-server` over a vox WebSocket.
//! - [`Transport::Local`] — serve a backend **in-process** over a vox
//!   in-memory link (no server, no socket). Native only (the browser
//!   stays remote — vox's in-memory link isn't compiled for wasm).
//!
//! The web app picks `Remote`; the desktop app picks `Local` and runs the
//! whole thing offline. Same screens, same clients — only the transport
//! differs. Established once at the root, stashed in a
//! `Signal<ConnState>`, pulled by screens via [`use_conn`].

use dioxus::prelude::*;
use example::{ExampleRepoClient, ExampleServiceClient};
use vox_core::{TransportMode, initiator_on};
use vox_websocket::WsLink;

/// Default vox endpoint for the remote transport.
pub const DEFAULT_SERVER_URL: &str = "ws://127.0.0.1:4040/vox";

/// Where the services live. Chosen at the app root and provided via
/// context; `App` reads it to establish the clients.
#[derive(Clone)]
pub enum Transport {
    /// Remote `app-server` over a vox WebSocket.
    Remote(String),
    /// A backend served in-process — no server. Native only.
    #[cfg(not(target_arch = "wasm32"))]
    Local(architect::LocalServer),
}

/// The two typed clients the UI drives. Cheap to clone — each is a vox
/// caller handle, not a connection.
#[derive(Clone)]
pub struct ExampleClients {
    pub repo: ExampleRepoClient,
    pub service: ExampleServiceClient,
}

impl ExampleClients {
    /// Establish both clients over the chosen transport.
    pub async fn connect(transport: &Transport) -> Result<Self, String> {
        match transport {
            Transport::Remote(url) => Ok(Self {
                repo: establish_ws(url).await?,
                service: establish_ws(url).await?,
            }),
            #[cfg(not(target_arch = "wasm32"))]
            Transport::Local(server) => Ok(Self {
                repo: server.establish().await.map_err(|e| format!("{e:?}"))?,
                service: server.establish().await.map_err(|e| format!("{e:?}"))?,
            }),
        }
    }
}

/// Establish one typed client over a fresh WebSocket, **resiliently**.
///
/// A just-booting or briefly-flaky server shouldn't fail the first frame,
/// so the connect+handshake is wrapped in [`architect::schedule::retry`]
/// under an exponential-backoff-with-jitter policy: ~200ms, 400ms, 800ms,
/// … (±20% jitter, capped at 5s), up to 5 retries. The policy's clock is
/// platform-portable — `tokio::time` on desktop, browser timers on web —
/// so the same resilient connect works on both targets.
async fn establish_ws<C>(url: &str) -> Result<C, String>
where
    C: vox_core::FromVoxSession,
{
    architect::schedule::retry(
        || establish_ws_once::<C>(url),
        architect::Schedule::exponential(std::time::Duration::from_millis(200))
            .max_delay(std::time::Duration::from_secs(5))
            .jittered()
            .take(5),
    )
    .await
}

/// One connect attempt. vox's `establish` consumes the link and yields
/// exactly one client; the server's `/vox` acceptor routes by service
/// name, so each client gets its own socket.
async fn establish_ws_once<C>(url: &str) -> Result<C, String>
where
    C: vox_core::FromVoxSession,
{
    let link = WsLink::connect(url)
        .await
        .map_err(|e| format!("connect {url}: {e:?}"))?;
    initiator_on(link, TransportMode::Bare)
        .establish::<C>()
        .await
        .map_err(|e| format!("vox handshake: {e:?}"))
}

/// Connection state, provided to the tree as `Signal<ConnState>`.
#[derive(Clone)]
pub enum ConnState {
    Connecting,
    Ready(ExampleClients),
    Failed(String),
}

/// Pull the connection signal any screen was handed by [`App`].
pub fn use_conn() -> Signal<ConnState> {
    use_context::<Signal<ConnState>>()
}
