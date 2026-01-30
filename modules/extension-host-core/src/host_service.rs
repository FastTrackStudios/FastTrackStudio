//! Host Services - Services that extensions can call back to the host
//!
//! This implements the HostService trait from hello-world-proto,
//! allowing extensions to log messages back to the REAPER extension.

use hello_world_proto::{ForwardCallResult, HostService, HostServiceDispatcher};
use roam_session::{ConnectionHandle, Context};
use std::collections::HashMap;
use std::sync::Arc;
use parking_lot::RwLock;
use tracing::{debug, error, info, warn};

/// Host service implementation
///
/// Extensions can call these services via RPC to interact with the REAPER extension.
#[derive(Clone)]
pub struct HostServiceImpl {
    /// Connection handles per peer ID
    connections: Arc<RwLock<HashMap<u8, ConnectionHandle>>>,
    /// Extension name registry (name → peer_id)
    extension_names: Arc<RwLock<HashMap<String, u8>>>,
}

impl HostServiceImpl {
    /// Create a new host service with access to the extension registry
    pub fn new(
        connections: Arc<RwLock<HashMap<u8, ConnectionHandle>>>,
        extension_names: Arc<RwLock<HashMap<String, u8>>>,
    ) -> Self {
        Self {
            connections,
            extension_names,
        }
    }
}

impl HostService for HostServiceImpl {
    /// Log a message from an extension to the REAPER extension's log
    async fn log_message(&self, _cx: &Context, level: String, message: String) {
        match level.as_str() {
            "trace" => debug!("[Extension] {}", message),
            "debug" => debug!("[Extension] {}", message),
            "info" => info!("[Extension] {}", message),
            "warn" => warn!("[Extension] {}", message),
            "error" => error!("[Extension] {}", message),
            _ => info!("[Extension] {}", message),
        }
    }

    /// Forward an RPC call to another registered service
    ///
    /// This allows extensions (like hello-world) to call other services (like daw)
    /// through the host as a mediator, since SHM guests cannot communicate directly.
    async fn forward_call(
        &self,
        _cx: &Context,
        service_name: String,
        method_id: u64,
        payload: Vec<u8>,
    ) -> ForwardCallResult {
        debug!(
            "Forwarding call to service: {}, method_id: {}",
            service_name, method_id
        );

        // Look up the service by name
        let peer_id = match self.extension_names.read().get(&service_name).copied() {
            Some(id) => id,
            None => {
                warn!("Service not found: {}", service_name);
                return ForwardCallResult::ServiceNotFound { service_name };
            }
        };

        // Get the connection handle for this peer
        let handle = match self.connections.read().get(&peer_id).cloned() {
            Some(h) => h,
            None => {
                error!("Connection handle not found for peer {}", peer_id);
                return ForwardCallResult::CallFailed {
                    error: format!("Connection handle not found for peer {}", peer_id),
                };
            }
        };

        // Make the RPC call
        match handle.call_raw(method_id, payload).await {
            Ok(response_bytes) => {
                debug!("Forward call succeeded, response size: {} bytes", response_bytes.len());
                ForwardCallResult::Success { response_bytes }
            }
            Err(e) => {
                error!("Forward call failed: {:?}", e);
                ForwardCallResult::CallFailed {
                    error: format!("{:?}", e),
                }
            }
        }
    }
}

/// Create a host service dispatcher
///
/// This dispatcher handles incoming RPC calls from extensions.
pub fn create_host_dispatcher(
    connections: Arc<RwLock<HashMap<u8, ConnectionHandle>>>,
    extension_names: Arc<RwLock<HashMap<String, u8>>>,
) -> HostServiceDispatcher<HostServiceImpl> {
    HostServiceDispatcher::new(HostServiceImpl::new(connections, extension_names))
}
