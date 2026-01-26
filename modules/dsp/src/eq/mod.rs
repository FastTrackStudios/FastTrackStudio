//! Parametric EQ module with Pro-Q 4 compatible parameters.
//!
//! This module provides a complete parametric EQ implementation with:
//! - Up to 24 bands with full parameter control
//! - Dynamic EQ with threshold, attack, release
//! - Spectral dynamics processing
//! - Multiple filter shapes and slopes
//! - Side-chain support
//! - Spectrum analyzer
//!
//! The parameter structure is designed to be 1:1 compatible with FabFilter Pro-Q 4
//! for preset conversion and mapping.

pub mod params;
pub mod filter;
pub mod analyzer;

#[cfg(feature = "render")]
pub mod render;

pub use params::*;
