//! Action error types

use thiserror::Error;

// region:    --- Types

pub type Result<T> = core::result::Result<T, Error>;

/// Error type for action operations
#[derive(Debug, Error)]
pub enum Error {
    /// Custom error for flexible error handling
    #[error("{0}")]
    Custom(String),

    // -- Action Errors
    /// Action not found in registry
    #[error("Action not found: {0}")]
    NotFound(String),

    /// Action execution failed
    #[error("Action execution failed: {0}")]
    ExecutionFailed(String),

    /// Action is not available in current state
    #[error("Action not available: {0}")]
    NotAvailable(String),

    /// Action requires a different context (e.g., needs project open)
    #[error("Invalid context: {0}")]
    InvalidContext(String),
}

// endregion: --- Types

// region:    --- Custom

impl Error {
    pub fn custom(val: impl Into<String>) -> Self {
        Self::Custom(val.into())
    }

    pub fn not_found(id: impl Into<String>) -> Self {
        Self::NotFound(id.into())
    }

    pub fn execution_failed(msg: impl Into<String>) -> Self {
        Self::ExecutionFailed(msg.into())
    }

    pub fn not_available(msg: impl Into<String>) -> Self {
        Self::NotAvailable(msg.into())
    }

    pub fn invalid_context(msg: impl Into<String>) -> Self {
        Self::InvalidContext(msg.into())
    }
}

// endregion: --- Custom
