//! Host Client Library
//!
//! This module provides shared connection logic for connecting to FastTrackStudio hosts.
//! It abstracts over different transport mechanisms:
//! - Unix socket (native only) - for local host connections
//! - WebSocket (native + WASM) - for remote host connections
//!
//! # Usage
//!
//! ```rust,ignore
//! use host_client::{HostConnection, HostConnector};
//!
//! // Connect to a local host via Unix socket
//! let connector = HostConnector::unix("/tmp/fts-control.sock");
//! let connection = connector.connect().await?;
//!
//! // Or connect via WebSocket
//! let connector = HostConnector::websocket("ws://localhost:3030/ws");
//! let connection = connector.connect().await?;
//!
//! // Use the connection to control the DAW
//! connection.transport().play(None).await?;
//! ```

mod connection;
mod connector;
mod error;

pub use connection::HostConnection;
pub use connector::HostConnector;
pub use error::HostClientError;

// Re-export commonly used types
pub use host_manager_proto::HostIdentity;
