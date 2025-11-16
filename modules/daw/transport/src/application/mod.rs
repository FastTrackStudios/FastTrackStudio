
pub mod app;

// Re-export for convenience
pub use crate::core::TransportActions;

/// Trait alias for transport implementations that can be used across the system
pub trait ApplicationTransport: TransportActions + Send + Sync + 'static {}
