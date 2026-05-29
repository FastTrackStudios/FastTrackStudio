//! Vox client lifecycle for the app shell.
//!
//! The whole app talks to `app-server` over vox — *never* through
//! Dioxus server functions. We establish the typed clients once at the
//! root (`App`), stash them in a context-provided [`Signal`], and let
//! every screen pull them with [`use_clients`]. Data reads then go
//! through `use_resource` against these handles.
//!
//! Two links, one per service: vox's `establish` consumes the link and
//! yields exactly one typed client, and the server's `/vox` acceptor
//! routes each connection by service name. So `ExampleRepo` and
//! `ExampleService` get a socket each — mirrors the working pattern in
//! `examples/app/cli`.

use dioxus::prelude::*;
use example::{ExampleRepoClient, ExampleServiceClient};
use vox_core::{TransportMode, initiator_on};
use vox_websocket::WsLink;

/// Default vox endpoint. `app-server` binds `:4040`; `dx serve` runs the
/// web app on a different port and connects back here.
pub const DEFAULT_SERVER_URL: &str = "ws://127.0.0.1:4040/vox";

/// The two typed clients the UI drives. Cheap to clone — each is a vox
/// caller handle, not a connection.
#[derive(Clone)]
pub struct VoxClients {
    pub repo: ExampleRepoClient,
    pub service: ExampleServiceClient,
}

impl VoxClients {
    /// Open one socket per service and establish the typed client on each.
    pub async fn connect(url: &str) -> Result<Self, String> {
        let repo = {
            let link = WsLink::connect(url)
                .await
                .map_err(|e| format!("connect {url}: {e:?}"))?;
            initiator_on(link, TransportMode::Bare)
                .establish::<ExampleRepoClient>()
                .await
                .map_err(|e| format!("ExampleRepo handshake: {e:?}"))?
        };
        let service = {
            let link = WsLink::connect(url)
                .await
                .map_err(|e| format!("connect {url}: {e:?}"))?;
            initiator_on(link, TransportMode::Bare)
                .establish::<ExampleServiceClient>()
                .await
                .map_err(|e| format!("ExampleService handshake: {e:?}"))?
        };
        Ok(Self { repo, service })
    }
}

/// Connection state, provided to the tree as `Signal<ConnState>`.
#[derive(Clone)]
pub enum ConnState {
    Connecting,
    Ready(VoxClients),
    Failed(String),
}

/// Pull the connection signal any screen was handed by [`App`].
pub fn use_conn() -> Signal<ConnState> {
    use_context::<Signal<ConnState>>()
}
