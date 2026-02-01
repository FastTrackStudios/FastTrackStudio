//! Error types for host client operations.

use thiserror::Error;

/// Errors that can occur when connecting to or communicating with a host.
#[derive(Debug, Error)]
pub enum HostClientError {
    /// Failed to connect to the host.
    #[error("connection failed: {0}")]
    ConnectionFailed(String),

    /// The connection was closed unexpectedly.
    #[error("connection closed")]
    ConnectionClosed,

    /// Failed to parse host identity from metadata.
    #[error("invalid host identity: {0}")]
    InvalidIdentity(String),

    /// RPC call failed.
    #[error("rpc error: {0}")]
    RpcError(String),

    /// The requested service is not available.
    #[error("service unavailable: {0}")]
    ServiceUnavailable(String),

    /// IO error during connection.
    #[error("io error: {0}")]
    Io(#[from] std::io::Error),

    /// Handshake failed.
    #[error("handshake failed: {0}")]
    HandshakeFailed(String),
}

impl From<eyre::Report> for HostClientError {
    fn from(e: eyre::Report) -> Self {
        HostClientError::RpcError(e.to_string())
    }
}
