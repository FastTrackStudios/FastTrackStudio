//! IROH connection helpers for REAPER extension communication
//!
//! Provides utilities for setting up IROH endpoints with MDNS discovery
//! for connecting the desktop app to the REAPER extension.

use crate::reaper_api::ReaperProtocol;
use anyhow::Result;
use iroh::{Endpoint, EndpointId, protocol::Router};
use irpc::LocalSender;
use irpc_iroh::IrohProtocol;
use tracing::{info, warn};

/// ALPN string for REAPER extension protocol
pub const REAPER_ALPN: &[u8] = b"fasttrackstudio/reaper/1";

/// Create an IROH endpoint and router for the REAPER extension server
///
/// Returns the router (which keeps the server running) and the endpoint ID
/// that clients can use to connect.
pub async fn create_reaper_server(
    local_sender: impl Into<LocalSender<ReaperProtocol>>,
) -> Result<(Router, EndpointId)> {
    let endpoint = Endpoint::bind().await?;
    let endpoint_id = endpoint.id();

    let protocol = IrohProtocol::with_sender(local_sender);
    let router = Router::builder(endpoint)
        .accept(REAPER_ALPN, protocol)
        .spawn();

    info!("[IROH] REAPER server endpoint ID: {}", endpoint_id);
    info!(
        "[IROH] REAPER server listening on ALPN: {:?}",
        String::from_utf8_lossy(REAPER_ALPN)
    );

    Ok((router, endpoint_id))
}

/// Create an IROH client endpoint for connecting to the REAPER extension
///
/// Uses MDNS discovery to find the REAPER extension by its service name.
/// For now, we'll use a fixed EndpointId or discovery mechanism.
pub async fn create_reaper_client() -> Result<Endpoint> {
    let endpoint = Endpoint::bind().await?;
    info!("[IROH] Desktop client endpoint ID: {}", endpoint.id());
    Ok(endpoint)
}

/// Discover REAPER extension endpoint ID via MDNS
///
/// This will search for a service named "fasttrackstudio-reaper" on the local network.
/// Returns the EndpointId if found.
pub async fn discover_reaper_endpoint() -> Result<Option<EndpointId>> {
    // TODO: Implement MDNS discovery
    // For now, we can use a known endpoint ID or read from a file
    // In production, this would use IROH's discovery mechanisms

    // Check if endpoint ID is stored in a file (written by REAPER)
    let endpoint_id_path = std::path::Path::new("/tmp/reaper_endpoint_id.txt");
    if let Ok(content) = std::fs::read_to_string(endpoint_id_path) {
        if let Ok(endpoint_id) = content.trim().parse::<EndpointId>() {
            info!("[IROH] Found REAPER endpoint ID from file: {}", endpoint_id);
            return Ok(Some(endpoint_id));
        }
    }

    warn!("[IROH] Could not discover REAPER endpoint ID");
    Ok(None)
}

/// Store endpoint ID to a file for client discovery
pub fn store_endpoint_id(endpoint_id: EndpointId) -> Result<()> {
    let endpoint_id_path = std::path::Path::new("/tmp/reaper_endpoint_id.txt");
    std::fs::write(endpoint_id_path, endpoint_id.to_string())
        .map_err(|e| anyhow::anyhow!("Failed to write endpoint ID: {}", e))?;
    info!(
        "[IROH] Stored endpoint ID to {}",
        endpoint_id_path.display()
    );
    Ok(())
}

/// Store both socket address and endpoint ID for client discovery
pub fn store_socket_addr_and_endpoint_id(
    socket_addr: std::net::SocketAddr,
    endpoint_id: EndpointId,
) -> Result<()> {
    let socket_addr_path = std::path::Path::new("/tmp/reaper_socket_addr.txt");
    let endpoint_id_path = std::path::Path::new("/tmp/reaper_endpoint_id.txt");

    // Store socket address
    std::fs::write(socket_addr_path, socket_addr.to_string())
        .map_err(|e| anyhow::anyhow!("Failed to write socket address: {}", e))?;

    // Store endpoint ID (needed for EndpointAddr)
    std::fs::write(endpoint_id_path, endpoint_id.to_string())
        .map_err(|e| anyhow::anyhow!("Failed to write endpoint ID: {}", e))?;

    info!(
        "[IROH] Stored socket address to {}: {}",
        socket_addr_path.display(),
        socket_addr
    );
    info!(
        "[IROH] Stored endpoint ID to {}: {}",
        endpoint_id_path.display(),
        endpoint_id
    );
    Ok(())
}

/// Read socket address from file for client connection
pub fn read_socket_addr() -> Result<Option<std::net::SocketAddr>> {
    let socket_addr_path = std::path::Path::new("/tmp/reaper_socket_addr.txt");
    if let Ok(content) = std::fs::read_to_string(socket_addr_path) {
        if let Ok(socket_addr) = content.trim().parse::<std::net::SocketAddr>() {
            info!(
                "[IROH] Found REAPER socket address from file: {}",
                socket_addr
            );
            return Ok(Some(socket_addr));
        }
    }
    Ok(None)
}

/// Read endpoint ID from file (needed to create EndpointAddr)
pub fn read_endpoint_id() -> Result<Option<EndpointId>> {
    let endpoint_id_path = std::path::Path::new("/tmp/reaper_endpoint_id.txt");
    if let Ok(content) = std::fs::read_to_string(endpoint_id_path) {
        if let Ok(endpoint_id) = content.trim().parse::<EndpointId>() {
            info!("[IROH] Found REAPER endpoint ID from file: {}", endpoint_id);
            return Ok(Some(endpoint_id));
        }
    }
    Ok(None)
}
