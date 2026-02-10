//! DAW Connection Manager
//!
//! Manages connections to one or more DAW instances (REAPER) via Unix socket.
//! Each DAW exposes roam services via a Unix socket, and this manager handles
//! connecting, tracking, and reconnecting to those DAWs.
//!
//! # Multi-DAW Support
//!
//! The desktop app can control multiple DAW instances simultaneously. Each
//! DAW connection provides access to that DAW's services (transport, project, etc.).

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use host_client::{HostConnection, HostConnector, HostIdentity};
use tokio::sync::RwLock;
use tracing::{debug, info, warn};

/// Unique identifier for a DAW connection
pub type DawId = String;

/// Manages connections to multiple DAW instances.
///
/// # Example
///
/// ```rust,ignore
/// let manager = DawConnectionManager::new();
///
/// // Connect to the default socket
/// let conn = manager.connect_default().await?;
///
/// // Or connect to a specific socket
/// let conn = manager.connect("/tmp/reaper-2.sock").await?;
///
/// // Get all active connections
/// let connections = manager.connections().await;
/// ```
#[allow(dead_code)]
pub struct DawConnectionManager {
    /// Active connections indexed by DAW ID
    connections: RwLock<HashMap<DawId, Arc<HostConnection>>>,
}

#[allow(dead_code)]
impl DawConnectionManager {
    /// Create a new connection manager.
    pub fn new() -> Self {
        Self {
            connections: RwLock::new(HashMap::new()),
        }
    }

    /// Connect to a DAW via Unix socket at the specified path.
    ///
    /// Returns the connection if successful, or an error if the connection fails.
    /// The connection is stored and can be retrieved later via `get()` or `connections()`.
    pub async fn connect(
        &self,
        socket_path: impl Into<PathBuf>,
    ) -> eyre::Result<Arc<HostConnection>> {
        let path = socket_path.into();
        let connector = HostConnector::unix(&path);

        debug!("Connecting to DAW at: {}", path.display());

        let connection = connector
            .connect()
            .await
            .map_err(|e| eyre::eyre!("Failed to connect to DAW at {}: {}", path.display(), e))?;

        let conn = Arc::new(connection);

        // Generate an ID for this connection based on identity or path
        let id = conn
            .identity()
            .map(|i| i.name.clone())
            .unwrap_or_else(|| path.display().to_string());

        debug!("Connected to DAW: {}", id);

        self.connections
            .write()
            .await
            .insert(id.clone(), conn.clone());
        Ok(conn)
    }

    /// Connect to the default DAW socket path.
    ///
    /// Uses the `FTS_SOCKET` environment variable if set, otherwise defaults to
    /// `/tmp/fts-control.sock`.
    pub async fn connect_default(&self) -> eyre::Result<Arc<HostConnection>> {
        let path = std::env::var("FTS_SOCKET")
            .map(PathBuf::from)
            .unwrap_or_else(|_| PathBuf::from("/tmp/fts-control.sock"));

        self.connect(path).await
    }

    /// Get a connection by its ID.
    pub async fn get(&self, id: &str) -> Option<Arc<HostConnection>> {
        self.connections.read().await.get(id).cloned()
    }

    /// Get all active connections.
    pub async fn connections(&self) -> Vec<Arc<HostConnection>> {
        self.connections.read().await.values().cloned().collect()
    }

    /// Get all connection IDs and their identities.
    pub async fn list(&self) -> Vec<(DawId, Option<HostIdentity>)> {
        self.connections
            .read()
            .await
            .iter()
            .map(|(id, conn)| (id.clone(), conn.identity().cloned()))
            .collect()
    }

    /// Disconnect from a specific DAW.
    pub async fn disconnect(&self, id: &str) -> bool {
        if let Some(_conn) = self.connections.write().await.remove(id) {
            info!("Disconnected from DAW: {}", id);
            true
        } else {
            warn!("No connection found for DAW: {}", id);
            false
        }
    }

    /// Disconnect from all DAWs.
    pub async fn disconnect_all(&self) {
        let mut connections = self.connections.write().await;
        let count = connections.len();
        connections.clear();
        info!("Disconnected from {} DAW(s)", count);
    }

    /// Get the first (primary) connection, if any.
    ///
    /// Useful when only one DAW is typically connected.
    pub async fn primary(&self) -> Option<Arc<HostConnection>> {
        self.connections.read().await.values().next().cloned()
    }
}

impl Default for DawConnectionManager {
    fn default() -> Self {
        Self::new()
    }
}
