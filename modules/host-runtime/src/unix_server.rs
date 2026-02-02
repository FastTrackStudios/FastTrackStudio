//! Unix socket server for desktop app connections.
//!
//! This module provides a Unix socket listener that accepts connections from
//! the fts-control desktop app. Desktop apps can then:
//! - Call TransportService/ProjectService to control the DAW (via Host's DAW dispatcher)
//! - Call GatewayCoordinator to take over/release gateway cells
//! - Receive virtual connections with host identity metadata

use std::path::Path;

use crate::forwarder::ArcDispatcher;
use crate::Host;
use gateway_proto::{
    GatewayCoordinator, GatewayCoordinatorDispatcher, GatewayInfo, GatewayState, GatewayType,
    TakeOverRequest, TakeOverResponse,
};
use host_manager_proto::{HostIdentity, HOST_IDENTITY_KEY};
use roam::session::{HandshakeConfig, RoutedDispatcher};
use roam::Context;
use roam_local::LocalListener;
use roam_stream::CobsFramed;
use roam_wire::MetadataValue;
use tracing::{info, warn};

/// Start the Unix socket server for desktop app connections.
pub async fn start_server(socket_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    // Clean up old socket file if it exists
    if socket_path.exists() {
        std::fs::remove_file(socket_path)?;
    }

    let listener = LocalListener::bind(socket_path)?;
    info!("Unix socket listening at: {}", socket_path.display());

    // Get host identity from environment or use defaults
    let host_id = std::env::var("FTS_HOST_ID").unwrap_or_else(|_| uuid::Uuid::new_v4().to_string());
    let host_name = std::env::var("FTS_HOST_NAME").unwrap_or_else(|_| "DAW Host".to_string());
    let host_purpose = std::env::var("FTS_HOST_PURPOSE").unwrap_or_else(|_| "default".to_string());
    let host_instance = std::env::var("FTS_HOST_INSTANCE").ok();

    loop {
        match listener.accept().await {
            Ok(stream) => {
                info!("Desktop app connected via Unix socket");
                let identity = HostIdentity {
                    id: host_id.clone(),
                    name: host_name.clone(),
                    purpose: host_purpose.clone(),
                    instance: host_instance.clone(),
                    tags: vec![],
                };

                tokio::spawn(async move {
                    if let Err(e) = handle_connection(stream, identity).await {
                        warn!("Desktop connection error: {}", e);
                    }
                    info!("Desktop app disconnected");
                });
            }
            Err(e) => {
                warn!("Unix socket accept error: {}", e);
            }
        }
    }
}

/// Handle a single desktop app connection.
async fn handle_connection(
    stream: roam_local::LocalStream,
    identity: HostIdentity,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    // Wrap in COBS framing for message delimiting
    let framed = CobsFramed::new(stream);

    // Get the DAW dispatcher from Host (TransportService + ProjectService)
    // This is registered by the extension during initialization
    let daw_dispatcher = Host::get()
        .daw_dispatcher()
        .ok_or("DAW dispatcher not registered")?
        .clone();

    // Create gateway coordinator dispatcher
    let gateway_dispatcher = GatewayCoordinatorDispatcher::new(GatewayCoordinatorImpl);

    // Combine dispatchers: DAW services + Gateway coordinator
    // The DAW dispatcher handles TransportService and ProjectService calls
    // by routing them to the in-process REAPER API handlers
    let dispatcher = RoutedDispatcher::new(
        ArcDispatcher::new(daw_dispatcher.clone()),
        gateway_dispatcher,
    );

    // Accept the connection
    let (handle, _incoming, driver) =
        roam::session::accept_framed(framed, HandshakeConfig::default(), dispatcher).await?;

    // Spawn driver first so it processes messages
    let driver_handle = tokio::spawn(async move {
        if let Err(e) = driver.run().await {
            warn!("Driver error: {}", e);
        }
    });

    // Open virtual connection back to desktop with our identity
    let identity_bytes = facet_postcard::to_vec(&identity)?;
    let metadata = vec![(
        HOST_IDENTITY_KEY.to_string(),
        MetadataValue::Bytes(identity_bytes),
        0u64,
    )];

    // Create dispatcher for the virtual connection using the same DAW dispatcher
    let virtual_dispatcher = ArcDispatcher::new(daw_dispatcher);

    match handle
        .connect(metadata, Some(Box::new(virtual_dispatcher)))
        .await
    {
        Ok(_virtual_handle) => {
            info!(
                "Opened virtual connection with purpose '{}'",
                identity.purpose
            );
        }
        Err(e) => {
            warn!("Failed to open virtual connection: {}", e);
        }
    }

    // Wait for driver to complete
    driver_handle.await?;

    Ok(())
}

/// Implementation of GatewayCoordinator for managing gateway cells.
#[derive(Clone)]
struct GatewayCoordinatorImpl;

impl GatewayCoordinator for GatewayCoordinatorImpl {
    async fn take_over(&self, _cx: &Context, request: TakeOverRequest) -> TakeOverResponse {
        info!(
            "GatewayCoordinator: take_over({:?}) called",
            request.gateway_type
        );

        // TODO: Actually suspend the gateway cell
        // let gateway = Host::get().client_async::<GatewayControlClient>().await;
        // gateway.suspend(request.redirect_info).await;

        TakeOverResponse {
            success: true,
            active_connections: 0,
        }
    }

    async fn release(&self, _cx: &Context, gateway_type: GatewayType) {
        info!("GatewayCoordinator: release({:?}) called", gateway_type);

        // TODO: Actually resume the gateway cell
        // let gateway = Host::get().client_async::<GatewayControlClient>().await;
        // gateway.resume().await;
    }

    async fn get_states(&self, _cx: &Context) -> Vec<GatewayInfo> {
        // TODO: Query actual gateway cells
        vec![GatewayInfo {
            gateway_type: GatewayType::WebSocket,
            state: GatewayState::Active,
            connection_count: 0,
        }]
    }
}
