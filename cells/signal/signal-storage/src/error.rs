//! Storage error types.

use uuid::Uuid;

/// Result type alias for storage operations.
pub type StorageResult<T> = Result<T, StorageError>;

/// Errors that can occur during storage operations.
#[derive(Debug, thiserror::Error)]
pub enum StorageError {
    /// Database query or connection error.
    #[error("database error: {0}")]
    Database(#[from] sqlx::Error),

    /// Entity not found.
    #[error("{entity} not found: {id}")]
    NotFound { entity: &'static str, id: Uuid },

    /// Version conflict during optimistic locking update.
    #[error("version conflict on {entity} {id}: expected version {expected}")]
    VersionConflict {
        entity: &'static str,
        id: Uuid,
        expected: i64,
    },

    /// JSON serialization/deserialization error.
    #[error("serialization error: {0}")]
    Serialization(#[from] serde_json::Error),

    /// Configuration error.
    #[error("configuration error: {0}")]
    Config(String),

    /// Connection pool exhausted or unavailable.
    #[error("connection unavailable: {0}")]
    ConnectionUnavailable(String),
}
