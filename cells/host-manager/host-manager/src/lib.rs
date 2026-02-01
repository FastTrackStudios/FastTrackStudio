//! Host Manager
//!
//! Manages connections to multiple DAW hosts, supporting:
//! - Multiple hosts with different purposes (tracks, guitar, keyboard, etc.)
//! - Multiple instances of the same purpose (Guitar 1, Guitar 2)
//! - Redundant hosts within an instance (primary, backup)
//! - Live host designation with automatic failover
//!
//! # Architecture
//!
//! ```text
//! HostManager
//! └── Instance Groups (by instance_key)
//!     ├── "tracks"
//!     │   └── [host-tracks-primary]
//!     ├── "guitar:Guitar 1"
//!     │   ├── [guitar1-primary] (live)
//!     │   └── [guitar1-backup]
//!     └── "guitar:Guitar 2"
//!         ├── [guitar2-primary] (live)
//!         └── [guitar2-backup]
//! ```
//!
//! # Usage
//!
//! ```ignore
//! use host_manager::HostManager;
//! use daw_proto::TransportServiceClient;
//!
//! let mut manager = HostManager::new();
//!
//! // Connect to hosts
//! manager.connect(socket_path, 0).await?;  // priority 0 = primary
//!
//! // Get a host connection
//! let host = manager.get_host("guitar:Guitar 1", "guitar1-primary").await?;
//!
//! // Use any service client directly
//! let transport = TransportServiceClient::new(host.handle().clone());
//! transport.play(None).await?;
//! ```

use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;

use eyre::Result;
use host_manager_proto::{HostIdentity, HOST_IDENTITY_KEY};
use roam::session::{ConnectionHandle, HandshakeConfig, IncomingConnections};
use roam_stream::CobsFramed;
use roam_wire::MetadataValue;
use tokio::sync::{Mutex, RwLock};
use tracing::{info, warn};

/// A connection to a specific host.
///
/// Use `handle()` to get the connection handle, then create any
/// service client you need from it.
#[derive(Clone)]
pub struct HostConnection {
    identity: HostIdentity,
    handle: ConnectionHandle,
    priority: u32,
    healthy: Arc<RwLock<bool>>,
}

impl HostConnection {
    /// Get the connection handle for creating service clients.
    ///
    /// # Example
    /// ```ignore
    /// let host = manager.get_host("guitar", "guitar-primary").await?;
    ///
    /// // Create any service client
    /// let transport = TransportServiceClient::new(host.handle().clone());
    /// transport.play(None).await?;
    ///
    /// let project = ProjectServiceClient::new(host.handle().clone());
    /// let info = project.get_current().await?;
    /// ```
    pub fn handle(&self) -> &ConnectionHandle {
        &self.handle
    }

    /// Get the host's identity.
    pub fn identity(&self) -> &HostIdentity {
        &self.identity
    }

    /// Get the host's priority (lower = higher priority, 0 = primary).
    pub fn priority(&self) -> u32 {
        self.priority
    }

    /// Check if this host is healthy.
    pub async fn is_healthy(&self) -> bool {
        *self.healthy.read().await
    }

    /// Mark this host as unhealthy.
    pub async fn mark_unhealthy(&self) {
        *self.healthy.write().await = false;
    }

    /// Mark this host as healthy.
    pub async fn mark_healthy(&self) {
        *self.healthy.write().await = true;
    }
}

/// Manages connections to multiple hosts.
///
/// Hosts are grouped by their "instance key" (purpose:instance or just purpose).
/// Each instance can have multiple redundant hosts. You access hosts directly
/// and use whatever service clients you need.
pub struct HostManager {
    /// Map of instance_key -> list of host connections (ordered by priority)
    hosts_by_instance: Arc<Mutex<HashMap<String, Vec<HostConnection>>>>,
    /// Currently live host name per instance_key
    live_hosts: Arc<Mutex<HashMap<String, String>>>,
    /// Background task handles
    tasks: Vec<tokio::task::JoinHandle<()>>,
}

impl Default for HostManager {
    fn default() -> Self {
        Self::new()
    }
}

impl HostManager {
    /// Create a new host manager.
    pub fn new() -> Self {
        Self {
            hosts_by_instance: Arc::new(Mutex::new(HashMap::new())),
            live_hosts: Arc::new(Mutex::new(HashMap::new())),
            tasks: Vec::new(),
        }
    }

    /// Connect to a host via Unix socket with a given priority.
    ///
    /// Priority determines failover order: 0 = primary, 1 = secondary, etc.
    /// The host will open a virtual connection back with its identity.
    pub async fn connect<P: AsRef<Path>>(&mut self, socket_path: P, priority: u32) -> Result<()> {
        let path = socket_path.as_ref();
        info!("Connecting to host at {:?} with priority {}", path, priority);

        let stream = roam_local::connect(path).await?;
        let framed = CobsFramed::new(stream);

        let (handle, incoming, driver) = roam::session::initiate_framed(
            framed,
            HandshakeConfig::default(),
            roam::session::NoDispatcher,
        )
        .await?;

        info!("Connection established, spawning handlers");

        // Spawn incoming connection handler
        let hosts_by_instance = self.hosts_by_instance.clone();
        let live_hosts = self.live_hosts.clone();
        let incoming_task = tokio::spawn(async move {
            Self::handle_incoming_connections(incoming, hosts_by_instance, live_hosts, handle, priority).await;
        });
        self.tasks.push(incoming_task);

        // Spawn driver
        let driver_task = tokio::spawn(async move {
            let _ = driver.run().await;
        });
        self.tasks.push(driver_task);

        tokio::task::yield_now().await;
        Ok(())
    }

    async fn handle_incoming_connections(
        mut incoming: IncomingConnections,
        hosts_by_instance: Arc<Mutex<HashMap<String, Vec<HostConnection>>>>,
        live_hosts: Arc<Mutex<HashMap<String, String>>>,
        _root_handle: ConnectionHandle,
        priority: u32,
    ) {
        while let Some(conn) = incoming.recv().await {
            if let Some((_, MetadataValue::Bytes(bytes), _)) = conn
                .metadata
                .iter()
                .find(|(k, _, _)| k == HOST_IDENTITY_KEY)
            {
                match facet_postcard::from_slice::<HostIdentity>(bytes) {
                    Ok(identity) => {
                        let instance_key = identity.instance_key();
                        info!(
                            "Received host connection: '{}' instance_key='{}' priority={}",
                            identity.name, instance_key, priority
                        );

                        match conn.accept(vec![], None).await {
                            Ok(handle) => {
                                let host_name = identity.name.clone();
                                let key = instance_key.clone();

                                let mut hosts = hosts_by_instance.lock().await;
                                let list = hosts.entry(key.clone()).or_default();
                                let is_first = list.is_empty();

                                list.push(HostConnection {
                                    identity,
                                    handle,
                                    priority,
                                    healthy: Arc::new(RwLock::new(true)),
                                });

                                // Sort by priority (lower = higher priority)
                                list.sort_by_key(|h| h.priority);

                                // First host for this instance becomes live
                                if is_first {
                                    drop(hosts);
                                    let mut live = live_hosts.lock().await;
                                    live.insert(key.clone(), host_name.clone());
                                    info!("Host '{}' is now LIVE for instance '{}'", host_name, key);
                                }
                            }
                            Err(e) => {
                                warn!("Failed to accept virtual connection: {}", e);
                            }
                        }
                    }
                    Err(e) => {
                        warn!("Failed to decode host identity: {}", e);
                        conn.reject("invalid identity".to_string(), vec![]);
                    }
                }
            } else {
                conn.reject("missing identity".to_string(), vec![]);
            }
        }
    }

    /// Get all instance keys that have at least one host.
    pub async fn instance_keys(&self) -> Vec<String> {
        let hosts = self.hosts_by_instance.lock().await;
        hosts.keys().cloned().collect()
    }

    /// Get the number of hosts for an instance.
    pub async fn host_count(&self, instance_key: &str) -> usize {
        let hosts = self.hosts_by_instance.lock().await;
        hosts.get(instance_key).map(|v| v.len()).unwrap_or(0)
    }

    /// Get a specific host by instance key and host name.
    pub async fn get_host(&self, instance_key: &str, host_name: &str) -> Option<HostConnection> {
        let hosts = self.hosts_by_instance.lock().await;
        hosts
            .get(instance_key)
            .and_then(|list| list.iter().find(|h| h.identity.name == host_name).cloned())
    }

    /// Get all hosts for an instance.
    pub async fn get_all_hosts(&self, instance_key: &str) -> Vec<HostConnection> {
        let hosts = self.hosts_by_instance.lock().await;
        hosts.get(instance_key).cloned().unwrap_or_default()
    }

    /// Get all healthy hosts for an instance.
    pub async fn get_healthy_hosts(&self, instance_key: &str) -> Vec<HostConnection> {
        let hosts = self.hosts_by_instance.lock().await;
        let Some(list) = hosts.get(instance_key) else {
            return vec![];
        };

        let mut result = Vec::new();
        for host in list {
            if host.is_healthy().await {
                result.push(host.clone());
            }
        }
        result
    }

    /// Get the current live host for an instance.
    pub async fn get_live_host(&self, instance_key: &str) -> Option<HostConnection> {
        let live = self.live_hosts.lock().await;
        let live_name = live.get(instance_key)?;

        let hosts = self.hosts_by_instance.lock().await;
        hosts
            .get(instance_key)
            .and_then(|list| list.iter().find(|h| &h.identity.name == live_name).cloned())
    }

    /// Set which host is live for an instance.
    ///
    /// Returns an error if the host doesn't exist or is unhealthy.
    pub async fn set_live_host(&self, instance_key: &str, host_name: &str) -> Result<()> {
        let host = self.get_host(instance_key, host_name).await.ok_or_else(|| {
            eyre::eyre!("Host '{}' not found in instance '{}'", host_name, instance_key)
        })?;

        if !host.is_healthy().await {
            return Err(eyre::eyre!("Host '{}' is not healthy", host_name));
        }

        let mut live = self.live_hosts.lock().await;
        live.insert(instance_key.to_string(), host_name.to_string());
        info!("Set '{}' as LIVE for instance '{}'", host_name, instance_key);
        Ok(())
    }

    /// Check and perform failover if the current live host is unhealthy.
    ///
    /// Promotes the next healthy host (by priority order) to live status.
    pub async fn check_failover(&self, instance_key: &str) {
        let hosts = self.hosts_by_instance.lock().await;
        let Some(host_list) = hosts.get(instance_key) else {
            return;
        };

        let mut live = self.live_hosts.lock().await;

        // Check if current live host is still healthy
        if let Some(live_name) = live.get(instance_key) {
            if let Some(host) = host_list.iter().find(|h| &h.identity.name == live_name) {
                if *host.healthy.read().await {
                    return; // Live host is fine
                }
            }
        }

        // Find next healthy host by priority order
        for host in host_list {
            if *host.healthy.read().await {
                info!(
                    "Failover: '{}' is now LIVE for instance '{}'",
                    host.identity.name, instance_key
                );
                live.insert(instance_key.to_string(), host.identity.name.clone());
                return;
            }
        }

        warn!("No healthy hosts remaining for instance '{}'!", instance_key);
        live.remove(instance_key);
    }
}

impl Drop for HostManager {
    fn drop(&mut self) {
        for task in self.tasks.drain(..) {
            task.abort();
        }
    }
}
