//! Unix socket server for desktop app connections.
//!
//! This module provides a Unix socket listener that accepts connections from
//! the fts-control desktop app. Desktop apps can then:
//! - Call TransportService/ProjectService to control the DAW (via Host's DAW dispatcher)
//! - Call GatewayCoordinator to take over/release gateway cells
//! - Receive virtual connections with host identity metadata

use std::path::Path;

use crate::cells::{cell_ready_registry, HostServiceImpl};
use crate::forwarder::ArcDispatcher;
use crate::Host;
use cell_host_proto::HostServiceDispatcher;
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

    // Create gateway coordinator dispatcher
    let gateway_dispatcher = GatewayCoordinatorDispatcher::new(GatewayCoordinatorImpl);

    // Create host service dispatcher for admin operations (reload_cell, poll_ready)
    let host_service = HostServiceImpl::new(cell_ready_registry().clone());
    let host_service_dispatcher = HostServiceDispatcher::new(host_service);

    // Configure handshake with higher credit for 60Hz streaming
    // Default is 64KB which gets exhausted quickly with real-time updates
    let config = HandshakeConfig {
        max_payload_size: 1024 * 1024,            // 1 MiB
        initial_channel_credit: 16 * 1024 * 1024, // 16 MiB for high-frequency streaming
    };

    // Handle based on whether we have an in-process DAW dispatcher
    // - With DAW dispatcher (REAPER): DAW services are handled in-process
    // - Without DAW dispatcher (test-extension): Only gateway and host services
    if let Some(daw_dispatcher) = Host::get().daw_dispatcher() {
        // REAPER mode: DAW calls handled in-process
        let with_gateway = RoutedDispatcher::new(
            ArcDispatcher::new(daw_dispatcher.clone()),
            gateway_dispatcher,
        );
        let dispatcher = RoutedDispatcher::new(with_gateway, host_service_dispatcher);

        let (handle, _incoming, driver) =
            roam::session::accept_framed(framed, config.clone(), dispatcher).await?;

        let driver_handle = tokio::spawn(async move {
            if let Err(e) = driver.run().await {
                warn!("Driver error: {}", e);
            }
        });

        // Open virtual connection with DAW dispatcher
        send_identity_virtual_connection(&handle, &identity, Some(daw_dispatcher.clone())).await;

        driver_handle.await?;
    } else {
        // Test mode: No in-process DAW, only gateway coordinator + host service
        let dispatcher = RoutedDispatcher::new(gateway_dispatcher, host_service_dispatcher);

        let (handle, _incoming, driver) =
            roam::session::accept_framed(framed, config, dispatcher).await?;

        let driver_handle = tokio::spawn(async move {
            if let Err(e) = driver.run().await {
                warn!("Driver error: {}", e);
            }
        });

        // Open virtual connection without DAW dispatcher
        send_identity_virtual_connection(&handle, &identity, None).await;

        driver_handle.await?;
    }

    Ok(())
}

/// Send identity to connected client via virtual connection.
async fn send_identity_virtual_connection(
    handle: &roam::session::ConnectionHandle,
    identity: &HostIdentity,
    daw_dispatcher: Option<crate::host::BoxedDispatcher>,
) {
    let identity_bytes = match facet_postcard::to_vec(identity) {
        Ok(bytes) => bytes,
        Err(e) => {
            warn!("Failed to serialize identity: {}", e);
            return;
        }
    };

    let metadata = vec![(
        HOST_IDENTITY_KEY.to_string(),
        MetadataValue::Bytes(identity_bytes),
        0u64,
    )];

    // Create dispatcher for the virtual connection if DAW dispatcher is available
    let virtual_dispatcher: Option<Box<dyn roam::session::ServiceDispatcher>> = daw_dispatcher
        .map(|d| Box::new(ArcDispatcher::new(d)) as Box<dyn roam::session::ServiceDispatcher>);

    match handle.connect(metadata, virtual_dispatcher).await {
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
