//! Master / output blocks.
//!
//! Re-exports from other block modules where appropriate. The master
//! module uses EQ, multiband compressor, and output volume blocks.

// Master blocks are defined in eq.rs (mastering_eq), dynamics.rs
// (multiband_compressor, limiter), and amp.rs (output_volume).
// This module exists for discoverability.

pub use super::amp::output_volume;
pub use super::dynamics::{limiter, multiband_compressor};
pub use super::eq::mastering_eq;
