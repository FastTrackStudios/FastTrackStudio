//! Service Registry for the Service Mesh
//!
//! Tracks all available services, their providers, and which is currently active.
//! Supports conflict detection and hot-swapping between providers.

use host_proto::{ServiceInfo, ServiceMetadata};
use parking_lot::RwLock;
use std::collections::HashMap;
use std::sync::Arc;
use tracing::{info, warn};

/// Information about a service provider
#[derive(Debug, Clone)]
pub struct ProviderInfo {
    pub name: String,
    pub peer_id: u8,
    pub version: String,
    pub capabilities: Vec<String>,
    pub status: ProviderStatus,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ProviderStatus {
    Active,
    Standby,
    Failed { error: String },
}

/// Service entry with all providers
#[derive(Debug, Clone)]
pub struct ServiceEntry {
    pub name: String,
    pub providers: Vec<ProviderInfo>,
    pub active_provider: String,
}

/// Service registry for the mesh
#[derive(Clone)]
pub struct ServiceRegistry {
    /// service_name → ServiceEntry
    services: Arc<RwLock<HashMap<String, ServiceEntry>>>,
}

impl ServiceRegistry {
    pub fn new() -> Self {
        Self {
            services: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Register a service provider
    pub fn register_provider(&self, metadata: ServiceMetadata) -> Result<Vec<String>, String> {
        let mut services = self.services.write();
        let service_name = metadata.name.clone();

        let provider = ProviderInfo {
            name: metadata.provider_name.clone(),
            peer_id: 0, // Will be set during registration
            version: metadata.version,
            capabilities: metadata.capabilities,
            status: ProviderStatus::Standby,
        };

        if let Some(entry) = services.get_mut(&service_name) {
            // Check for existing provider
            if entry
                .providers
                .iter()
                .any(|p| p.name == metadata.provider_name)
            {
                warn!(
                    "Provider '{}' already registered for '{}'",
                    metadata.provider_name, service_name
                );
                return Err(format!(
                    "Provider '{}' already registered",
                    metadata.provider_name
                ));
            }

            // Add as standby (only one active at a time)
            entry.providers.push(provider);
            info!(
                "Added standby provider '{}' for '{}'",
                metadata.provider_name, service_name
            );

            // Detect conflicts (multiple providers)
            if entry.providers.len() > 1 {
                warn!(
                    "Conflict detected: {} providers for '{}' - active: '{}', standby: {:?}",
                    entry.providers.len(),
                    service_name,
                    entry.active_provider,
                    entry.providers.iter().map(|p| &p.name).collect::<Vec<_>>()
                );
            }

            Ok(vec![service_name])
        } else {
            // First provider - make it active
            let provider_name = metadata.provider_name.clone();
            let entry = ServiceEntry {
                name: service_name.clone(),
                providers: vec![provider],
                active_provider: provider_name.clone(),
            };
            services.insert(service_name.clone(), entry);
            info!(
                "Registered '{}' with active provider '{}'",
                service_name, provider_name
            );
            Ok(vec![service_name])
        }
    }

    /// Set the active provider for a service (hot-swap)
    pub fn set_active_provider(&self, service: &str, provider: &str) -> Result<(), String> {
        let mut services = self.services.write();

        let entry = services
            .get_mut(service)
            .ok_or_else(|| format!("Service '{}' not found", service))?;

        // Verify provider exists
        if !entry.providers.iter().any(|p| p.name == provider) {
            return Err(format!(
                "Provider '{}' not found for service '{}'",
                provider, service
            ));
        }

        let old = entry.active_provider.clone();
        entry.active_provider = provider.to_string();

        // Update statuses
        for p in &mut entry.providers {
            p.status = if p.name == provider {
                ProviderStatus::Active
            } else {
                ProviderStatus::Standby
            };
        }

        info!("Hot-swapped '{}' from '{}' to '{}'", service, old, provider);
        Ok(())
    }

    /// Get active provider info for a service
    pub fn get_active_provider(&self, service: &str) -> Option<(String, u8)> {
        let services = self.services.read();
        let entry = services.get(service)?;

        entry
            .providers
            .iter()
            .find(|p| p.name == entry.active_provider)
            .map(|p| (p.name.clone(), p.peer_id))
    }

    /// Lookup service by name (returns active provider info)
    pub fn lookup(&self, service_name: &str) -> Option<ServiceInfo> {
        let services = self.services.read();
        let entry = services.get(service_name)?;

        entry
            .providers
            .iter()
            .find(|p| p.name == entry.active_provider)
            .map(|p| ServiceInfo {
                name: entry.name.clone(),
                provider_name: p.name.clone(),
                provider_peer_id: p.peer_id,
                version: p.version.clone(),
            })
    }

    /// List all registered services
    pub fn list_services(&self) -> Vec<ServiceInfo> {
        let services = self.services.read();
        services
            .values()
            .filter_map(|entry| {
                entry
                    .providers
                    .iter()
                    .find(|p| p.name == entry.active_provider)
                    .map(|p| ServiceInfo {
                        name: entry.name.clone(),
                        provider_name: p.name.clone(),
                        provider_peer_id: p.peer_id,
                        version: p.version.clone(),
                    })
            })
            .collect()
    }

    /// Unregister all services from a provider (when extension disconnects)
    pub fn unregister_provider(&self, provider_name: &str) {
        let mut services = self.services.write();

        for (service_name, entry) in services.iter_mut() {
            entry.providers.retain(|p| p.name != provider_name);

            // If we removed the active provider, failover to another
            if entry.active_provider == provider_name && !entry.providers.is_empty() {
                let new_active = entry.providers[0].name.clone();
                warn!(
                    "Failover: '{}' lost active provider '{}', switching to '{}'",
                    service_name, provider_name, new_active
                );
                entry.active_provider = new_active;
                entry.providers[0].status = ProviderStatus::Active;
            }
        }

        // Remove services with no providers
        services.retain(|_, entry| !entry.providers.is_empty());
    }

    /// Get all providers for conflict detection
    pub fn get_all_providers(&self, service: &str) -> Vec<ProviderInfo> {
        let services = self.services.read();
        services
            .get(service)
            .map(|e| e.providers.clone())
            .unwrap_or_default()
    }
}

impl Default for ServiceRegistry {
    fn default() -> Self {
        Self::new()
    }
}
