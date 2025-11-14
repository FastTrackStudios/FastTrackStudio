//! Marker Region Core Domain
//!
//! This module contains the pure domain logic for marker and region operations.
//! It defines the core types, traits, and business rules without any
//! infrastructure dependencies.

pub mod error;
pub mod marker;
pub mod region;
pub mod source;

// Re-export core types for easy access
pub use error::MarkerRegionError;
pub use marker::{Marker, MarkerSource};
pub use region::{Region, RegionSource};
pub use source::{MarkerRegionSource, NavigationPoint, SourceSummary};
