//! MicroService error types

use thiserror::Error;

// region:    --- Types

pub type Result<T> = core::result::Result<T, Error>;

/// Error type for microservice operations
#[derive(Debug, Error)]
pub enum Error {
    /// Custom error for flexible error handling
    #[error("{0}")]
    Custom(String),

    // -- Registration Errors
    /// Service already registered
    #[error("Service already registered: {0}")]
    AlreadyRegistered(String),

    /// Service not found
    #[error("Service not found: {0}")]
    NotFound(String),

    // -- Runtime Errors
    /// Service initialization failed
    #[error("Service initialization failed: {0}")]
    InitFailed(String),

    /// Server start failed
    #[error("Server start failed: {0}")]
    ServerFailed(String),

    // -- External Errors
    /// Action error
    #[error("Action error: {0}")]
    Action(#[from] actions::Error),
}

// endregion: --- Types

// region:    --- Custom

impl Error {
    pub fn custom(val: impl Into<String>) -> Self {
        Self::Custom(val.into())
    }

    pub fn already_registered(name: impl Into<String>) -> Self {
        Self::AlreadyRegistered(name.into())
    }

    pub fn not_found(name: impl Into<String>) -> Self {
        Self::NotFound(name.into())
    }

    pub fn init_failed(msg: impl Into<String>) -> Self {
        Self::InitFailed(msg.into())
    }

    pub fn server_failed(msg: impl Into<String>) -> Self {
        Self::ServerFailed(msg.into())
    }
}

// endregion: --- Custom
