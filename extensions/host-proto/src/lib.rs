//! Host Protocol - Transport and Extension Lifecycle
//!
//! Re-exports Transport from daw-proto.
//! Provides extension lifecycle protocol for the extension-runtime.
//! Includes Service Mesh support for routing and hot-swapping.

pub use daw_proto::{
    transport_service_detail, PlaybackState, Transport, TransportClient, TransportResult,
};

// Re-export roam types commonly used
pub use roam::Tx;

// ============================================================================
// Extension Lifecycle Protocol
// ============================================================================

use facet::Facet;
use std::collections::HashMap;

/// Service capability metadata
#[derive(Debug, Clone, Facet)]
pub struct ServiceCapability {
    pub name: String,
    pub description: String,
}

/// Full service metadata for registration
#[derive(Debug, Clone, Facet)]
pub struct ServiceMetadata {
    pub name: String,
    pub version: String,              // semver string e.g., "1.0.0"
    pub provider_name: String,        // e.g., "daw-reaper", "daw-standalone"
    pub capabilities: Vec<String>,    // Method names: ["play", "stop", "record"]
}

/// Message sent by extensions to signal they're ready
#[derive(Debug, Clone, Facet)]
pub struct ReadyMsg {
    pub extension_name: String,
    pub peer_id: u8,
}

/// Acknowledgment from host that extension is registered
#[derive(Debug, Clone, Facet)]
pub struct ReadyAck {
    pub ok: bool,
}

/// Extended registration with service metadata
#[derive(Debug, Clone, Facet)]
pub struct ExtensionRegistration {
    pub extension_name: String,
    pub peer_id: u8,
    pub provides_services: Vec<ServiceMetadata>,
}

/// Registration result
#[derive(Debug, Clone, Facet)]
#[repr(u8)]
pub enum RegistrationResult {
    Success { accepted_services: Vec<String> },
    Conflict {
        service: String,
        existing_provider: String,
        message: String,
    },
    Error { message: String },
}

/// Service info for discovery
#[derive(Debug, Clone, Facet)]
pub struct ServiceInfo {
    pub name: String,
    pub provider_name: String,
    pub provider_peer_id: u8,
    pub version: String,
}

/// Host service for extension lifecycle and service mesh management
#[roam::service]
pub trait ExtensionHost {
    /// Extension calls this after starting to signal it's ready
    async fn ready(&self, msg: ReadyMsg) -> ReadyAck;
    
    /// Register extension with full service metadata (Service Mesh)
    async fn register(&self, reg: ExtensionRegistration) -> RegistrationResult;
    
    /// Discover which peer provides a given service
    async fn discover_service(&self, service_name: String) -> Option<ServiceInfo>;
    
    /// Get all available services
    async fn list_services(&self) -> Vec<ServiceInfo>;
    
    /// Set active provider for a service (hot-swap support)
    /// Only allowed for host/admin extensions
    async fn set_active_provider(&self, service: String, provider: String) -> Result<(), String>;
}

// ============================================================================
// Service Mesh Configuration
// ============================================================================

/// Configuration for service mesh active providers
#[derive(Debug, Clone, Facet)]
pub struct ServiceMeshConfig {
    pub active_providers: HashMap<String, String>, // service_name -> provider_name
}

impl Default for ServiceMeshConfig {
    fn default() -> Self {
        let mut providers = HashMap::new();
        providers.insert("Transport".to_string(), "daw-reaper-extension".to_string());
        Self { active_providers: providers }
    }
}
