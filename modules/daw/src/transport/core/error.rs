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

    #[error("Internal error: {0}")]
    Internal(String),
}
