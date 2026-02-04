//! Host connection management.

use std::sync::Arc;

use cell_host_proto::HostServiceClient;
use daw_proto::{ProjectServiceClient, TransportServiceClient};
use gateway_proto::GatewayCoordinatorClient;
use host_manager_proto::{HostIdentity, HOST_IDENTITY_KEY};
use roam::session::{ConnectionHandle, HandshakeConfig, NoDispatcher};
use roam_session::IncomingConnections;
use roam_wire::MetadataValue;
use tracing::{info, warn};

use crate::error::HostClientError;

/// An active connection to a FastTrackStudio host.
///
/// Provides access to the host's services:
/// - `transport()` - Transport control (play, stop, etc.)
/// - `project()` - Project management
/// - `gateway()` - Gateway coordination (for desktop apps managing gateways)
///
/// The connection also provides access to the host's identity metadata.
pub struct HostConnection {
    /// The underlying roam connection handle.
    handle: ConnectionHandle,

    /// The host's identity, received during connection.
    identity: Option<HostIdentity>,

    /// Cached transport client.
    transport_client: Arc<TransportServiceClient>,

    /// Cached project client.
    project_client: Arc<ProjectServiceClient>,

    /// Cached gateway coordinator client.
    gateway_client: Arc<GatewayCoordinatorClient>,

    /// Cached host service client (for admin operations like reload_cell).
    host_service_client: Arc<HostServiceClient>,
}

impl HostConnection {
    /// Connect to a host via Unix socket (native only).
    #[cfg(not(target_arch = "wasm32"))]
    pub async fn connect_unix(path: &std::path::Path) -> Result<Self, HostClientError> {
        use roam_local::LocalStream;
        use roam_session::initiate_framed;
        use roam_stream::CobsFramed;

        info!("Connecting to host via Unix socket: {}", path.display());

        // Connect to the Unix socket
        let stream = LocalStream::connect(path).await.map_err(|e| {
            HostClientError::ConnectionFailed(format!("Unix socket connection failed: {}", e))
        })?;

        // Wrap in COBS framing
        let framed = CobsFramed::new(stream);

        // Configure handshake with higher credit for 60Hz streaming
        // The negotiated credit is the minimum of both peers, so both sides need high limits
        let config = HandshakeConfig {
            max_payload_size: 1024 * 1024,            // 1 MiB
            initial_channel_credit: 16 * 1024 * 1024, // 16 MiB for high-frequency streaming
        };

        // Initiate the roam connection (we're the client/initiator)
        let (handle, mut incoming, driver) = initiate_framed(framed, config, NoDispatcher)
            .await
            .map_err(|e| HostClientError::HandshakeFailed(e.to_string()))?;

        // Spawn the driver to process messages
        tokio::spawn(async move {
            if let Err(e) = driver.run().await {
                warn!("Connection driver error: {}", e);
            }
        });

        // Try to receive host identity from virtual connection
        let identity = Self::receive_identity(&mut incoming).await;

        // Create service clients
        let transport_client = Arc::new(TransportServiceClient::new(handle.clone()));
        let project_client = Arc::new(ProjectServiceClient::new(handle.clone()));
        let gateway_client = Arc::new(GatewayCoordinatorClient::new(handle.clone()));
        let host_service_client = Arc::new(HostServiceClient::new(handle.clone()));

        info!(
            "Connected to host: {}",
            identity
                .as_ref()
                .map(|i| i.name.as_str())
                .unwrap_or("unknown")
        );

        Ok(Self {
            handle,
            identity,
            transport_client,
            project_client,
            gateway_client,
            host_service_client,
        })
    }

    /// Connect to a host via WebSocket.
    pub async fn connect_websocket(url: &str) -> Result<Self, HostClientError> {
        info!("Connecting to host via WebSocket: {}", url);

        // Configure handshake with higher credit for 60Hz streaming
        let config = HandshakeConfig {
            max_payload_size: 1024 * 1024,            // 1 MiB
            initial_channel_credit: 16 * 1024 * 1024, // 16 MiB for high-frequency streaming
        };

        #[cfg(target_arch = "wasm32")]
        {
            use roam_session::initiate_framed;
            use roam_websocket::WsTransport;

            // Connect via WebSocket (WASM)
            let transport = WsTransport::connect(url).await.map_err(|e| {
                HostClientError::ConnectionFailed(format!("WebSocket connection failed: {}", e))
            })?;

            // Initiate the roam connection
            let (handle, mut incoming, driver) = initiate_framed(transport, config, NoDispatcher)
                .await
                .map_err(|e| HostClientError::HandshakeFailed(e.to_string()))?;

            // Spawn the driver
            wasm_bindgen_futures::spawn_local(async move {
                if let Err(e) = driver.run().await {
                    warn!("Connection driver error: {}", e);
                }
            });

            // Try to receive host identity
            let identity = Self::receive_identity_wasm(&mut incoming).await;

            // Create service clients
            let transport_client = Arc::new(TransportServiceClient::new(handle.clone()));
            let project_client = Arc::new(ProjectServiceClient::new(handle.clone()));
            let gateway_client = Arc::new(GatewayCoordinatorClient::new(handle.clone()));
            let host_service_client = Arc::new(HostServiceClient::new(handle.clone()));

            info!(
                "Connected to host: {}",
                identity
                    .as_ref()
                    .map(|i| i.name.as_str())
                    .unwrap_or("unknown")
            );

            Ok(Self {
                handle,
                identity,
                transport_client,
                project_client,
                gateway_client,
                host_service_client,
            })
        }

        #[cfg(not(target_arch = "wasm32"))]
        {
            use roam_session::initiate_framed;
            use roam_websocket::WsTransport;
            use tokio_tungstenite::connect_async;

            // Connect via WebSocket (native) using tokio-tungstenite
            let (ws_stream, _response) = connect_async(url).await.map_err(|e| {
                HostClientError::ConnectionFailed(format!("WebSocket connection failed: {}", e))
            })?;

            // Wrap in roam WsTransport
            let transport = WsTransport::new(ws_stream);

            // Initiate the roam connection
            let (handle, mut incoming, driver) = initiate_framed(transport, config, NoDispatcher)
                .await
                .map_err(|e| HostClientError::HandshakeFailed(e.to_string()))?;

            // Spawn the driver
            tokio::spawn(async move {
                if let Err(e) = driver.run().await {
                    warn!("Connection driver error: {}", e);
                }
            });

            // Try to receive host identity
            let identity = Self::receive_identity(&mut incoming).await;

            // Create service clients
            let transport_client = Arc::new(TransportServiceClient::new(handle.clone()));
            let project_client = Arc::new(ProjectServiceClient::new(handle.clone()));
            let gateway_client = Arc::new(GatewayCoordinatorClient::new(handle.clone()));
            let host_service_client = Arc::new(HostServiceClient::new(handle.clone()));

            info!(
                "Connected to host: {}",
                identity
                    .as_ref()
                    .map(|i| i.name.as_str())
                    .unwrap_or("unknown")
            );

            Ok(Self {
                handle,
                identity,
                transport_client,
                project_client,
                gateway_client,
                host_service_client,
            })
        }
    }

    /// Attempt to receive host identity from incoming virtual connection metadata (native).
    #[cfg(not(target_arch = "wasm32"))]
    async fn receive_identity(incoming: &mut IncomingConnections) -> Option<HostIdentity> {
        use tokio::time::{timeout, Duration};

        // Wait briefly for the host to send us a virtual connection with identity
        match timeout(Duration::from_secs(2), incoming.recv()).await {
            Ok(Some(virtual_conn)) => Self::parse_identity_from_metadata(&virtual_conn.metadata),
            Ok(None) => {
                warn!("No virtual connection received from host");
                None
            }
            Err(_) => {
                warn!("Timeout waiting for host identity");
                None
            }
        }
    }

    /// Attempt to receive host identity from incoming virtual connection metadata (WASM).
    #[cfg(target_arch = "wasm32")]
    async fn receive_identity_wasm(incoming: &mut IncomingConnections) -> Option<HostIdentity> {
        // In WASM we can't easily do timeouts, so just try to get the next connection
        // This may block if the host doesn't send identity
        match incoming.try_recv().ok() {
            Some(virtual_conn) => Self::parse_identity_from_metadata(&virtual_conn.metadata),
            None => {
                warn!("No virtual connection received from host");
                None
            }
        }
    }

    /// Parse host identity from connection metadata.
    fn parse_identity_from_metadata(
        metadata: &[(String, MetadataValue, u64)],
    ) -> Option<HostIdentity> {
        for (key, value, _) in metadata {
            if key == HOST_IDENTITY_KEY {
                if let MetadataValue::Bytes(bytes) = value {
                    match facet_postcard::from_slice::<HostIdentity>(bytes) {
                        Ok(identity) => {
                            info!(
                                "Received host identity: {} ({})",
                                identity.name, identity.purpose
                            );
                            return Some(identity);
                        }
                        Err(e) => {
                            warn!("Failed to parse host identity: {}", e);
                        }
                    }
                }
            }
        }
        None
    }

    /// Get the host's identity, if available.
    pub fn identity(&self) -> Option<&HostIdentity> {
        self.identity.as_ref()
    }

    /// Get a reference to the transport service client.
    pub fn transport(&self) -> &TransportServiceClient {
        &self.transport_client
    }

    /// Get a reference to the project service client.
    pub fn project(&self) -> &ProjectServiceClient {
        &self.project_client
    }

    /// Get a reference to the gateway coordinator client.
    pub fn gateway(&self) -> &GatewayCoordinatorClient {
        &self.gateway_client
    }

    /// Get a reference to the host service client.
    ///
    /// This provides admin operations like `reload_cell` for hot-reloading cells.
    pub fn host_service(&self) -> &HostServiceClient {
        &self.host_service_client
    }

    /// Get the underlying connection handle for advanced use cases.
    pub fn handle(&self) -> &ConnectionHandle {
        &self.handle
    }

    /// Check if the connection is still active.
    pub fn is_connected(&self) -> bool {
        // TODO: Add proper connection state tracking
        true
    }
}
