//! Host connection configuration and factory.

use std::path::PathBuf;

use crate::connection::HostConnection;
use crate::error::HostClientError;

/// Configuration for connecting to a host.
///
/// Supports multiple transport types:
/// - Unix socket (native only)
/// - WebSocket (native + WASM)
#[derive(Debug, Clone)]
pub enum HostConnector {
    /// Connect via Unix socket (local connections, native only).
    #[cfg(not(target_arch = "wasm32"))]
    Unix {
        /// Path to the Unix socket.
        path: PathBuf,
    },

    /// Connect via WebSocket (remote connections, works in WASM).
    WebSocket {
        /// WebSocket URL (e.g., "ws://localhost:3030/ws").
        url: String,
    },
}

impl HostConnector {
    /// Create a connector for a Unix socket path.
    #[cfg(not(target_arch = "wasm32"))]
    pub fn unix(path: impl Into<PathBuf>) -> Self {
        HostConnector::Unix { path: path.into() }
    }

    /// Create a connector for a WebSocket URL.
    pub fn websocket(url: impl Into<String>) -> Self {
        HostConnector::WebSocket { url: url.into() }
    }

    /// Connect to the host using the configured transport.
    pub async fn connect(&self) -> Result<HostConnection, HostClientError> {
        match self {
            #[cfg(not(target_arch = "wasm32"))]
            HostConnector::Unix { path } => HostConnection::connect_unix(path).await,

            HostConnector::WebSocket { url } => HostConnection::connect_websocket(url).await,
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
impl Default for HostConnector {
    fn default() -> Self {
        // Default to the standard Unix socket path
        let path = std::env::var("FTS_SOCKET")
            .map(PathBuf::from)
            .unwrap_or_else(|_| PathBuf::from("/tmp/fts-control.sock"));
        HostConnector::Unix { path }
    }
}

#[cfg(target_arch = "wasm32")]
impl Default for HostConnector {
    fn default() -> Self {
        // Default to localhost WebSocket in WASM
        HostConnector::WebSocket {
            url: "ws://localhost:3030/ws".to_string(),
        }
    }
}
