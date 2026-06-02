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
use std::borrow::Cow;
use std::sync::Arc;
use tracing::{debug, warn};
use vox::{
    BareConduit, Caller, ConnectionSettings, Driver, DriverReplySink, Handler, MetadataEntry,
    MetadataFlags, MetadataValue, Parity,
};

/// Keeps the server-side acceptor task alive.
struct KeepAlive {
    _handle: JoinHandle<()>,
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

        // Server side: accept with virtual connection support
        let handle = moire::task::spawn(async move {
            let handshake = vox::HandshakeResult {
                role: vox::SessionRole::Acceptor,
                our_settings: ConnectionSettings {
                    parity: Parity::Even,
                    max_concurrent_requests: 64,

                    initial_channel_credit: 16,
                },
                peer_settings: ConnectionSettings {
                    parity: Parity::Odd,
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
            match vox::acceptor_conduit(BareConduit::new(server_link), handshake)
                .on_connection(handler)
                .establish::<vox::NoopClient>()
                .await
            {
                Ok(_root) => {
                    debug!("LocalCaller server session established");
                    std::future::pending::<()>().await;
                }
                Err(e) => {
                    warn!("LocalCaller server accept failed: {:?}", e);
                }
            }
        });

        // Client side: establish root session
        let handshake = vox::HandshakeResult {
            role: vox::SessionRole::Initiator,
            our_settings: ConnectionSettings {
                parity: Parity::Odd,
                max_concurrent_requests: 64,

                initial_channel_credit: 16,
            },
            peer_settings: ConnectionSettings {
                parity: Parity::Even,
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
        let root = vox::initiator_conduit(BareConduit::new(client_link), handshake)
            .establish::<vox::NoopClient>()
            .await
            .map_err(|e| eyre::eyre!("LocalCaller initiation failed: {:?}", e))?;
        let session = root
            .session
            .clone()
            .ok_or_else(|| eyre::eyre!("LocalCaller root session missing handle"))?;

        // Open a virtual connection for DAW services
        let conn = session
            .open_connection(
                ConnectionSettings {
                    parity: Parity::Odd,
                    max_concurrent_requests: 64,

                    initial_channel_credit: 16,
                },
                vec![
                    MetadataEntry {
                        key: Cow::Borrowed("vox-service"),
                        value: MetadataValue::String(Cow::Borrowed("daw-local")),
                        flags: MetadataFlags::NONE,
                    },
                    MetadataEntry {
                        key: Cow::Borrowed("role"),
                        value: MetadataValue::String(Cow::Borrowed("local")),
                        flags: MetadataFlags::NONE,
                    },
                ],
            )
            .await
            .map_err(|e| eyre::eyre!("LocalCaller open_connection failed: {:?}", e))?;

        let mut driver = Driver::new(conn, ());
        let caller = Caller::new(driver.caller());
        moire::task::spawn(async move { driver.run().await });

        debug!("LocalCaller established (in-process memory channels, virtual connection)");

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
