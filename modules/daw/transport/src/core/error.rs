//! Transport Error Types
//!
//! This module defines all error types that can occur during transport operations.

/// Transport service error
#[derive(Debug, thiserror::Error)]
pub enum TransportError {
    #[error("Invalid tempo: {0}")]
    InvalidTempo(String),

    #[error("Invalid time signature: {0}")]
    InvalidTimeSignature(String),

    #[error("Transport is not ready: {0}")]
    NotReady(String),

    #[error("Recording error: {0}")]
    RecordingError(String),

    #[error("Invalid position: {0}")]
    InvalidPosition(String),

    #[error("Invalid playrate: {0}")]
    InvalidPlayrate(String),

    #[error("State lock error: {0}")]
    LockError(String),

    #[error("Internal error: {0}")]
    Internal(String),
}

impl From<std::io::Error> for TransportError {
    fn from(err: std::io::Error) -> Self {
        TransportError::Internal(err.to_string())
    }
}

// Serde support for protocol serialization
impl serde::Serialize for TransportError {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}
