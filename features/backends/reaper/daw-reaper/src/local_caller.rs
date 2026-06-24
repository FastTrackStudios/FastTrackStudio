//! In-process vox caller using memory channels.
//!
//! Wraps `vox::memory_link_pair` + acceptor/initiator into a reusable struct
//! that any in-process consumer (plugins, extensions, desktop apps) can use
//! to get a `Caller` without duplicating the boilerplate.
//!
//! Uses vox's virtual connection pattern: the root session is established
//! with an `on_connection` acceptor, then the client opens a virtual connection
//! to get a service-specific `Driver` and `Caller`.

use moire::task::JoinHandle;
use std::sync::Arc;
use tracing::{debug, warn};
use vox::{Caller, DriverReplySink, Handler};

/// Keeps the server-side acceptor task alive.
struct KeepAlive {
    _handle: JoinHandle<()>,
}

/// Minimal `FromVoxLane` client that captures only the lane's `Caller` —
/// the vox 0.10 replacement for the removed `NoopClient`. Opening a lane
/// with this client yields a ready-to-use `Caller` whose requests are
/// dispatched (by method id) against the server-side router.
#[derive(Clone)]
struct LocalLaneClient {
    caller: Caller,
}

impl vox::FromVoxLane for LocalLaneClient {
    const SERVICE_NAME: &'static str = "daw-local";

    fn from_vox_lane(caller: Caller, _connection: Option<vox::ConnectionHandle>) -> Self {
        Self { caller }
    }
}

/// In-process vox caller backed by memory channels.
///
/// Creates a `memory_link_pair`, spawns an acceptor task for the server side
/// with a `ConnectionAcceptor`, and establishes an initiator on the client side.
/// The client then opens a virtual connection to get a `Caller` for RPC.
///
/// # Example
///
/// ```ignore
/// let handler = RoutedHandler::new()
///     .with(fx_service_descriptor(), EffectsDispatcher::new(fx_impl));
/// let local = LocalCaller::new(handler).await?;
/// let daw = Daw::new(local.caller());
/// ```
#[derive(Clone)]
pub struct LocalCaller {
    caller: Caller,
    _keep_alive: Arc<KeepAlive>,
}

impl LocalCaller {
    /// Create a new in-process caller from any handler.
    ///
    /// Spawns a background task that accepts virtual connections and dispatches
    /// requests via an in-memory link pair. The task lives as long as any
    /// `LocalCaller` clone exists.
    pub async fn new<H>(handler: H) -> eyre::Result<Self>
    where
        H: Handler<DriverReplySink> + Clone + 'static,
    {
        let (client_link, server_link) = vox::memory_link_pair(256);

        // Server side: accept any lane and hand it to the supplied handler
        // (which dispatches by method id). vox 0.10 lane model.
        let handle = moire::task::spawn(async move {
            let acceptor = vox::lane_acceptor_fn(move |_req, connection| {
                connection.handle_with(handler.clone());
                Ok(())
            });
            match vox::acceptor_on(server_link)
                .on_lane(acceptor)
                .establish_connection()
                .await
            {
                Ok(_connection) => {
                    debug!("LocalCaller server session established");
                    std::future::pending::<()>().await;
                }
                Err(e) => {
                    warn!("LocalCaller server accept failed: {:?}", e);
                }
            }
        });

        // Client side: establish the connection and open the DAW service
        // lane, yielding a ready-to-use `Caller`.
        let client = vox::initiator_on(client_link)
            .establish::<LocalLaneClient>()
            .await
            .map_err(|e| eyre::eyre!("LocalCaller initiation failed: {:?}", e))?;
        let caller = client.caller;

        debug!("LocalCaller established (in-process memory channels, vox 0.10 lane)");

        Ok(Self {
            caller,
            _keep_alive: Arc::new(KeepAlive { _handle: handle }),
        })
    }

    /// Get the `Caller` for use with `Daw::new()` or `Daw::init()`.
    pub fn caller(&self) -> Caller {
        self.caller.clone()
    }
}
