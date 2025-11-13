//! Core Transport Types and Logic
//!
//! This module contains the core domain types and business logic for the transport system.
//! It represents Layer 3 (Domain Layer) and Layer 4 (Application Layer) abstractions
//! from the Universal Network Architecture.
//!
//! The core module is protocol-agnostic and contains:
//! - Domain request/response types (DomainRequest, DomainResponse)
//! - Transport actions trait (TransportActions)
//! - Core transport types (Transport, Tempo, PlayState)
//! - Domain routing logic (TransportDomainHandler)
//! - Error types (TransportError)

pub mod actions;
pub mod transport;
pub mod error;

// Re-export core types for easy access
pub use actions::{TransportActions, TransportAction};
pub use transport::{Transport, Tempo, PlayState, RecordMode};
pub use error::TransportError;
