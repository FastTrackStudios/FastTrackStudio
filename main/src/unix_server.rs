//! Unix socket server for desktop app connections.
//!
//! This module provides a Unix socket listener that accepts connections from
//! the fts-control desktop app. Desktop apps can then:
//! - Call TransportService/ProjectService to control the DAW
//! - Call GatewayCoordinator to take over/release gateway cells
//! - Receive virtual connections with host identity metadata

use std::path::Path;

use daw_proto::project::ProjectServiceDispatcher;
use daw_proto::transport::transport::TransportServiceDispatcher;
use daw_proto::{ProjectInfo, ProjectService, TransportService};
use gateway_proto::{
    GatewayCoordinator, GatewayCoordinatorDispatcher, GatewayInfo, GatewayState, GatewayType,
    TakeOverRequest, TakeOverResponse,
};
use host_manager_proto::{HOST_IDENTITY_KEY, HostIdentity};
use roam::Context;
use roam::session::{HandshakeConfig, RoutedDispatcher};
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

    // Create dispatchers for the services we expose
    let transport_dispatcher = TransportServiceDispatcher::new(HostTransportService);
    let project_dispatcher = ProjectServiceDispatcher::new(HostProjectService);
    let gateway_dispatcher = GatewayCoordinatorDispatcher::new(GatewayCoordinatorImpl);

    // Combine into a routed dispatcher: (project + transport) + gateway
    let daw_dispatcher = RoutedDispatcher::new(project_dispatcher, transport_dispatcher);
    let dispatcher = RoutedDispatcher::new(daw_dispatcher, gateway_dispatcher);

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

    // Create dispatcher for the virtual connection
    let virtual_transport = TransportServiceDispatcher::new(HostTransportService);
    let virtual_project = ProjectServiceDispatcher::new(HostProjectService);
    let virtual_dispatcher = RoutedDispatcher::new(virtual_project, virtual_transport);

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

/// Implementation of TransportService for desktop app connections.
///
/// TODO: Forward to actual DAW cell via Host singleton
#[derive(Clone)]
struct HostTransportService;

impl TransportService for HostTransportService {
    async fn play(&self, _cx: &Context, _project_id: Option<String>) {
        // TODO: Forward to DAW cell
        // let daw = Host::get().client_async::<TransportServiceClient>().await;
        // daw.play(project_id).await;
        info!("TransportService: play() called");
    }

    async fn stop(&self, _cx: &Context, _project_id: Option<String>) {
        // TODO: Forward to DAW cell
        info!("TransportService: stop() called");
    }
}

/// Implementation of ProjectService for desktop app connections.
///
/// TODO: Forward to actual DAW cell via Host singleton
#[derive(Clone)]
struct HostProjectService;

impl ProjectService for HostProjectService {
    async fn get_current(&self, _cx: &Context) -> Option<ProjectInfo> {
        // TODO: Forward to DAW cell
        info!("ProjectService: get_current() called");
        None
    }

    async fn get(&self, _cx: &Context, project_id: String) -> Option<ProjectInfo> {
        // TODO: Forward to DAW cell
        info!("ProjectService: get({}) called", project_id);
        None
    }

    async fn list(&self, _cx: &Context) -> Vec<ProjectInfo> {
        // TODO: Forward to DAW cell
        info!("ProjectService: list() called");
        vec![]
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
