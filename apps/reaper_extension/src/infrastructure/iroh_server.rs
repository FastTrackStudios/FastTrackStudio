//! IROH Server Infrastructure
//!
//! Manages the IROH server setup for peer-to-peer communication.

use std::sync::Arc;
use setlist::SetlistStreamApi;
use crate::services::StreamService;
use tracing::{info, error, warn};

/// Start the IROH server with setlist stream service
pub fn start_iroh_server(stream_service: Arc<StreamService>) {
    std::thread::spawn(move || {
        let rt = tokio::runtime::Runtime::new().expect("Failed to create tokio runtime");
        
        rt.block_on(async {
            info!("Starting IROH router with setlist stream in tokio runtime...");
            
            // Create setlist stream service
            let setlist_api = match stream_service.create_stream_api() {
                Ok(api) => api,
                Err(e) => {
                    warn!("Failed to create setlist stream service: {}", e);
                    return;
                }
            };
            
            // Expose setlist stream handler
            let setlist_handler = match setlist_api.expose() {
                Ok(handler) => handler,
                Err(e) => {
                    warn!("Failed to expose setlist stream service: {}", e);
                    return;
                }
            };
            
            // Create IROH endpoint and router with ALL protocols
            let endpoint = match iroh::Endpoint::bind().await {
                Ok(ep) => ep,
                Err(e) => {
                    error!("Failed to create IROH endpoint: {}", e);
                    return;
                }
            };
            let endpoint_id = endpoint.id();
                    
            // Build router with REAPER_ALPN - all protocols route through this single ALPN
            let _router = iroh::protocol::Router::builder(endpoint.clone())
                .accept(peer_2_peer::iroh_connection::REAPER_ALPN.to_vec(), setlist_handler)
                .spawn();
            
            // Store endpoint ID for client discovery
            if let Err(e) = peer_2_peer::iroh_connection::store_endpoint_id(endpoint_id) {
                warn!("Failed to store endpoint ID: {}", e);
            }
            
            info!("IROH router started successfully");
            info!("Router endpoint ID: {}", endpoint_id);
            info!("REAPER ALPN: {:?}", String::from_utf8_lossy(peer_2_peer::iroh_connection::REAPER_ALPN));
                    
            // Keep router alive - it handles all connections
            // The router will run until shutdown is called
            loop {
                tokio::time::sleep(tokio::time::Duration::from_secs(60)).await;
            }
        });
    });
}

