//! Peer Registry - Tracks peer connection handles for cross-extension calls
//!
//! Allows extensions to discover and call services on other extensions.

use parking_lot::RwLock;
use roam_session::ConnectionHandle;
use std::collections::HashMap;
use std::sync::Arc;

/// Registry of peer connection handles for service discovery
#[derive(Clone)]
pub struct PeerRegistry {
    /// Map of extension name -> ConnectionHandle
    peers: Arc<RwLock<HashMap<String, ConnectionHandle>>>,
}

impl PeerRegistry {
    /// Create a new peer registry
    pub fn new() -> Self {
        Self {
            peers: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Register a peer's connection handle
    pub fn register_peer(&self, name: String, handle: ConnectionHandle) {
        tracing::info!("Registering peer handle: {} for cross-extension calls", name);
        self.peers.write().insert(name, handle);
    }

    /// Get a peer's connection handle by name
    pub fn get_peer(&self, name: &str) -> Option<ConnectionHandle> {
        self.peers.read().get(name).cloned()
    }

    /// List all registered peer names
    pub fn list_peers(&self) -> Vec<String> {
        self.peers.read().keys().cloned().collect()
    }

    /// Remove a peer from the registry
    pub fn unregister_peer(&self, name: &str) {
        if self.peers.write().remove(name).is_some() {
            tracing::info!("Unregistered peer handle: {}", name);
        }
    }
}

impl Default for PeerRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// Global peer registry accessible to all extensions
static GLOBAL_PEER_REGISTRY: once_cell::sync::Lazy<PeerRegistry> =
    once_cell::sync::Lazy::new(PeerRegistry::new);

/// Get the global peer registry
pub fn global_peer_registry() -> &'static PeerRegistry {
    &GLOBAL_PEER_REGISTRY
}
