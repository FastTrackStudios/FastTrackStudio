//! Unix-socket Vox publisher.
//!
//! REAPER extensions that want external apps (CLI, desktop, mobile,
//! audio-sync peers, …) to call their service surface over Vox RPC use
//! [`publish_extension_socket`]. The host owns a fully-populated
//! [`architect::LayerRouter`] (built via `Reaper::layers()` plus any
//! extension-specific layers) and hands it to the publisher; the
//! publisher opens a Unix socket on `socket_path()`, accepts virtual
//! connections, and routes every request to the supplied router.
//!
//! Extracted from `daw-bridge` so any extension can stand up its own
//! external surface without duplicating socket plumbing.

use std::path::PathBuf;
use std::sync::Arc;

use architect::LayerRouter;
use tokio::net::UnixListener;
use tracing::{debug, info, warn};
use vox::{ConnectionAcceptor, ConnectionRequest, MetadataValue, PendingConnection};

/// Vox `ConnectionAcceptor` that hands every inbound virtual connection
/// to a single shared `LayerRouter`. The router contains the mounted
/// dispatchers for every service exposed by the extension.
#[derive(Clone)]
pub struct ExtensionConnectionAcceptor {
    handler: Arc<LayerRouter>,
}

impl ExtensionConnectionAcceptor {
    pub fn new(handler: LayerRouter) -> Self {
        Self {
            handler: Arc::new(handler),
        }
    }
}

impl ConnectionAcceptor for ExtensionConnectionAcceptor {
    fn accept(
        &self,
        request: &ConnectionRequest,
        connection: PendingConnection,
    ) -> Result<(), vox::Metadata<'static>> {
        let role = request
            .metadata()
            .iter()
            .find(|e| e.key == "role")
            .and_then(|e| match &e.value {
                MetadataValue::String(s) => Some(s.as_ref()),
                _ => None,
            })
            .unwrap_or("unknown");
        info!(role = role, "Accepting virtual connection");
        connection.handle_with(self.handler.as_ref().clone());
        Ok(())
    }
}

/// Resolve the Unix socket path for this REAPER process.
///
/// - `$FTS_SOCKET` overrides if set (used by tests / forced paths).
/// - Default: `/tmp/fts-daw-{pid}.sock` so multiple REAPER instances on
///   the same machine never collide. External discovery enumerates
///   `/tmp/fts-daw-*.sock`.
pub fn socket_path() -> PathBuf {
    std::env::var("FTS_SOCKET")
        .map(PathBuf::from)
        .unwrap_or_else(|_| {
            let pid = std::process::id();
            PathBuf::from(format!("/tmp/fts-daw-{pid}.sock"))
        })
}

/// Spawn the Unix-socket listener and start accepting Vox connections
/// against `router`. Returns nothing — the listener task is detached
/// onto the moire runtime and lives for the rest of the process. Any
/// failure to bind logs a warning but does not panic; the in-process
/// service surface remains usable.
///
/// Idempotent: stale socket files from a previous run are removed
/// before binding. Safe to call once per `plugin_main`.
pub fn publish_extension_socket(router: LayerRouter) {
    let path = socket_path();
    let _ = std::fs::remove_file(&path);

    let listener = match UnixListener::bind(&path) {
        Ok(l) => l,
        Err(e) => {
            warn!(
                path = %path.display(),
                error = %e,
                "Failed to bind extension Unix socket"
            );
            return;
        }
    };

    info!(path = %path.display(), "Extension Unix socket listening");

    let acceptor = ExtensionConnectionAcceptor::new(router);
    moire::task::spawn(async move {
        loop {
            match listener.accept().await {
                Ok((stream, _addr)) => {
                    debug!("Client connected via Unix socket");
                    let acceptor = acceptor.clone();
                    moire::task::spawn(async move {
                        let link = vox_stream::StreamLink::unix(stream);
                        let handshake = vox::HandshakeResult {
                            role: vox::SessionRole::Acceptor,
                            our_settings: vox::ConnectionSettings {
                                parity: vox::Parity::Even,
                                max_concurrent_requests: 64,
                                initial_channel_credit: 16,
                            },
                            peer_settings: vox::ConnectionSettings {
                                parity: vox::Parity::Odd,
                                max_concurrent_requests: 64,
                                initial_channel_credit: 16,
                            },
                            peer_supports_retry: true,
                            session_resume_key: None,
                            peer_resume_key: None,
                            our_schema: vec![],
                            peer_schema: vec![],
                            peer_metadata: vec![],
                        };
                        match vox::acceptor_conduit(vox::BareConduit::new(link), handshake)
                            .on_connection(acceptor)
                            .establish::<vox::NoopClient>()
                            .await
                        {
                            Ok(_root) => {
                                debug!("Unix socket session established");
                                std::future::pending::<()>().await;
                            }
                            Err(e) => {
                                warn!(error = ?e, "Unix socket handshake failed");
                            }
                        }
                    });
                }
                Err(e) => {
                    warn!(error = %e, "Unix socket accept error");
                }
            }
        }
    });
}
