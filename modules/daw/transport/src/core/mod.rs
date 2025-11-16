//! Transport Core Domain
//!
//! This module contains the pure domain logic for transport operations.
//! It defines the core types, traits, and business rules without any
//! infrastructure dependencies.

pub mod error;
pub mod transport;

// Re-export core types for easy access
pub use error::TransportError;
pub use transport::{PlayState, RecordMode, Tempo, Transport, TransportActions};
//
