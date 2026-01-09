//! Transport Stream Infrastructure
//!
//! Provides abstractions for streaming transport state updates.
//! This allows different implementations (REAPER, other DAWs, etc.) to provide
//! transport streams without the consumer needing to know about the specific implementation.

use crate::transport::core::transport::Transport;
use async_trait::async_trait;
use std::fmt::Debug;

/// A transport update sent over a stream
#[derive(Debug, Clone)]
pub struct TransportUpdate {
    pub transport: Transport,
}

/// Trait for types that can provide a stream of transport updates
#[async_trait]
pub trait TransportStream: Send + Sync + Debug {
    /// Subscribe to transport state updates
    /// Returns a receiver that will receive TransportUpdate messages
    async fn subscribe(&self) -> Result<Box<dyn TransportStreamReceiver>, TransportStreamError>;
}

/// Trait for receiving transport updates from a stream
#[async_trait]
pub trait TransportStreamReceiver: Send + Sync {
    /// Receive the next transport update
    /// Returns None if the stream has ended
    async fn recv(&mut self) -> Result<Option<TransportUpdate>, TransportStreamError>;
}

/// Errors that can occur when working with transport streams
#[derive(Debug, thiserror::Error)]
pub enum TransportStreamError {
    #[error("Connection error: {0}")]
    Connection(String),

    #[error("Stream ended")]
    StreamEnded,

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Other error: {0}")]
    Other(String),
}

/// Trait for transport stream backends
///
/// Different backends (REAPER, other DAWs, etc.) implement this trait
/// to provide transport stream connections. The transport crate uses
/// dynamic dispatch to call the registered backend without knowing
/// which specific implementation is being used.
#[async_trait]
pub trait TransportStreamBackend: Send + Sync {
    /// Create a transport stream by connecting to the given socket address
    async fn create_stream(
        &self,
        socket_addr: String,
    ) -> Result<Box<dyn TransportStream>, TransportStreamError>;

    /// Get the name of this backend (for logging/debugging)
    fn name(&self) -> &'static str;
}

/// Create a transport stream by connecting to a socket address using a backend
///
/// This is a generic function that connects to any transport stream service
/// listening on the given socket address. It uses the provided backend
/// to create the connection.
///
/// The backend implementation (REAPER, other DAWs, etc.) is passed in,
/// allowing the caller to choose which backend to use.
#[cfg(not(target_arch = "wasm32"))]
pub async fn create_transport_stream_from_socket(
    backend: &dyn TransportStreamBackend,
    socket_addr: String,
) -> Result<Box<dyn TransportStream>, TransportStreamError> {
    backend.create_stream(socket_addr).await
}
