//! Transport Core Domain
//!
//! This module contains the pure domain logic for transport operations.
//! It defines the core types, traits, and business rules without any
//! infrastructure dependencies.

pub mod transport;
pub mod error;

// Re-export core types for easy access
pub use transport::{
    Transport, TransportActions,
    Tempo, PlayState, RecordMode
};
pub use error::TransportError;
