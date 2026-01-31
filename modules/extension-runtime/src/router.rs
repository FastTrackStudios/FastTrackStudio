//! Service Router for the Service Mesh
//!
//! Routes service calls to the appropriate peer using ROAM's ReconnectingClient
//! for automatic retry and failover support.

use crate::registry::ServiceRegistry;
use host_proto::ServiceInfo;
use roam::session::ConnectionHandle;
use std::collections::HashMap;
use std::sync::Arc;
use parking_lot::RwLock;
use tracing::{debug, error, trace, warn};

/// Routes calls to the appropriate service provider
#[derive(Clone)]
pub struct ServiceRouter {
    registry: ServiceRegistry,
    /// service_name → method_id_offset
    /// This maps method IDs to service names for routing
    service_offsets: Arc<RwLock<HashMap<String, (u64, u64)>>>, // service → (start_id, end_id)
}

impl ServiceRouter {
    pub fn new(registry: ServiceRegistry) -> Self {
        Self {
            registry,
            service_offsets: Arc::new(RwLock::new(HashMap::new())),
        }
    }
    
    /// Register a service with its method ID range
    /// Called when setting up the router
    pub fn register_service_range(&self, service_name: &str, start_id: u64, end_id: u64) {
        let mut offsets = self.service_offsets.write();
        offsets.insert(service_name.to_string(), (start_id, end_id));
        trace!("Registered service '{}' with method range {}-{}", service_name, start_id, end_id);
    }
    
    /// Determine which service a method ID belongs to
    pub fn resolve_service(&self, method_id: u64) -> Option<String> {
        let offsets = self.service_offsets.read();
        offsets.iter()
            .find(|(_, (start, end))| method_id >= *start && method_id < *end)
            .map(|(name, _)| name.clone())
    }
    
    /// Route a call to the appropriate provider
    pub async fn route(
        &self,
        method_id: u64,
        _payload: &[u8],
        // TODO: Need access to peer handles here
    ) -> Result<Vec<u8>, RouterError> {
        // 1. Determine service from method_id
        let service_name = self.resolve_service(method_id)
            .ok_or_else(|| RouterError::UnknownMethod(method_id))?;
        
        // 2. Lookup active provider
        let provider_info = self.registry.lookup(&service_name)
            .ok_or_else(|| RouterError::ServiceNotFound(service_name.clone()))?;
        
        debug!("Routing {}::method_{} to provider '{}' (peer {})", 
            service_name, method_id, provider_info.provider_name, provider_info.provider_peer_id);
        
        // 3. Forward to provider
        // TODO: Implement actual forwarding using peer connection handles
        // For now, this is a placeholder showing the flow
        Err(RouterError::NotImplemented(
            format!("Forwarding to peer {} not yet implemented", provider_info.provider_peer_id)
        ))
    }
    
    /// Get registry for configuration
    pub fn registry(&self) -> &ServiceRegistry {
        &self.registry
    }
}

#[derive(Debug, Clone)]
pub enum RouterError {
    UnknownMethod(u64),
    ServiceNotFound(String),
    ProviderNotFound(String),
    ConnectionFailed(String),
    NotImplemented(String),
}

impl std::fmt::Display for RouterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RouterError::UnknownMethod(id) => write!(f, "Unknown method ID: {}", id),
            RouterError::ServiceNotFound(name) => write!(f, "Service '{}' not found", name),
            RouterError::ProviderNotFound(name) => write!(f, "Provider '{}' not found", name),
            RouterError::ConnectionFailed(msg) => write!(f, "Connection failed: {}", msg),
            RouterError::NotImplemented(msg) => write!(f, "Not implemented: {}", msg),
        }
    }
}

impl std::error::Error for RouterError {}
