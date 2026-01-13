//! Error types for visibility manager

use thiserror::Error;

/// Visibility manager error types
#[derive(Error, Debug)]
pub enum Error {
    /// Group not found
    #[error("Group not found: {0}")]
    GroupNotFound(String),

    /// Snapshot not found
    #[error("Snapshot not found: {0}")]
    SnapshotNotFound(String),

    /// Classification error
    #[error("Classification error: {0}")]
    Classification(String),

    /// Invalid operation for current view mode
    #[error("Invalid operation for view mode: {0}")]
    InvalidOperation(String),

    /// Serialization error
    #[error("Serialization error: {0}")]
    Serialization(#[from] serde_json::Error),
}

/// Result type alias
pub type Result<T> = std::result::Result<T, Error>;
