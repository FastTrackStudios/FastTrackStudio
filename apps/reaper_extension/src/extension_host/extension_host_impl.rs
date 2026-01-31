//! Extension Host Implementation - Service Mesh Support
//!
//! Implements ExtensionHost with full service mesh support:
//! - Extension lifecycle management (ready handshake)
//! - Service registration with metadata
//! - Service discovery
//! - Hot-swap support

use extension_runtime::{ServiceRegistry, ProviderInfo, ProviderStatus};
use host_proto::{ExtensionHost, ReadyMsg, ReadyAck, ServiceInfo, ServiceMetadata, ExtensionRegistration, RegistrationResult};
use roam_session::{ConnectionHandle, Context};
use std::collections::HashMap;
use std::sync::Arc;
use parking_lot::RwLock;
use tracing::{info, warn};

/// Extension host implementation with service mesh support
#[derive(Clone)]
pub struct ExtensionHostImpl {
    /// Connection handles per peer ID
    connections: Arc<RwLock<HashMap<u8, ConnectionHandle>>>,
    /// Extension name registry (name → peer_id)
    extension_names: Arc<RwLock<HashMap<String, u8>>>,
    /// Service registry for the mesh
    service_registry: ServiceRegistry,
}

impl ExtensionHostImpl {
    pub fn new(
        connections: Arc<RwLock<HashMap<u8, ConnectionHandle>>>,
        extension_names: Arc<RwLock<HashMap<String, u8>>>,
        service_registry: ServiceRegistry,
    ) -> Self {
        Self {
            connections,
            extension_names,
            service_registry,
        }
    }
    
    /// Get the service registry for external access
    pub fn service_registry(&self) -> &ServiceRegistry {
        &self.service_registry
    }
}

impl ExtensionHost for ExtensionHostImpl {
    async fn ready(&self, _cx: &Context, msg: ReadyMsg) -> ReadyAck {
        info!("✅ Extension '{}' (peer_id={}) is ready", msg.extension_name, msg.peer_id);
        self.extension_names.write().insert(msg.extension_name.clone(), msg.peer_id as u8);
        ReadyAck { ok: true }
    }
    
    async fn register(&self, _cx: &Context, reg: ExtensionRegistration) -> RegistrationResult {
        info!("📦 Extension '{}' registering {} service(s)", reg.extension_name, reg.provides_services.len());
        
        // Register extension name
        self.extension_names.write().insert(reg.extension_name.clone(), reg.peer_id);
        
        let mut accepted = Vec::new();
        let mut conflicts = Vec::new();
        
        for metadata in reg.provides_services {
            let service_name = metadata.name.clone();
            
            // Check for existing providers
            let existing = self.service_registry.get_all_providers(&service_name);
            if !existing.is_empty() {
                warn!(
                    "Conflict: Service '{}' already has provider(s): {:?}",
                    service_name,
                    existing.iter().map(|p| &p.name).collect::<Vec<_>>()
                );
                conflicts.push((service_name.clone(), existing[0].name.clone()));
            }
            
            // Register the provider
            match self.service_registry.register_provider(metadata) {
                Ok(_) => {
                    info!("✅ Registered '{}' from '{}'", service_name, reg.extension_name);
                    accepted.push(service_name);
                }
                Err(e) => {
                    warn!("❌ Failed to register '{}': {}", service_name, e);
                }
            }
        }
        
        if !conflicts.is_empty() {
            // Return first conflict
            RegistrationResult::Conflict {
                service: conflicts[0].0.clone(),
                existing_provider: conflicts[0].1.clone(),
                message: format!("Service already has active provider: {}", conflicts[0].1),
            }
        } else if accepted.is_empty() {
            RegistrationResult::Error { message: "No services were accepted".to_string() }
        } else {
            RegistrationResult::Success { accepted_services: accepted }
        }
    }
    
    async fn discover_service(&self, _cx: &Context, service_name: String) -> Option<ServiceInfo> {
        self.service_registry.lookup(&service_name)
    }
    
    async fn list_services(&self, _cx: &Context) -> Vec<ServiceInfo> {
        self.service_registry.list_services()
    }
    
    async fn set_active_provider(&self, _cx: &Context, service: String, provider: String) -> Result<(), String> {
        info!("🔄 Hot-swapping '{}' to provider '{}'", service, provider);
        self.service_registry.set_active_provider(&service, &provider)
    }
}

/// Create the extension host dispatcher with service mesh support
pub fn create_extension_host_dispatcher(
    connections: Arc<RwLock<HashMap<u8, ConnectionHandle>>>,
    extension_names: Arc<RwLock<HashMap<String, u8>>>,
    service_registry: ServiceRegistry,
) -> host_proto::ExtensionHostDispatcher<ExtensionHostImpl> {
    host_proto::ExtensionHostDispatcher::new(ExtensionHostImpl::new(connections, extension_names, service_registry))
}
