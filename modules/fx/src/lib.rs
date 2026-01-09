//! # fts-fx
//!
//! DSP effect blocks for FastTrackStudio.
//!
//! This crate provides a modular system for building audio effects with:
//! - **Primitives**: Low-level DSP building blocks (filters, delays, resampling, modulation)
//! - **Core**: Parameter system with ranges, smoothing, and modulation routing
//! - **Effect Blocks**: High-level effects (EQ, dynamics, modulation, distortion)
//! - **Chain**: Routing and connecting blocks together
//!
//! ## Architecture
//!
//! ```text
//! User-facing chain (pedalboard UI)
//! [Parametric EQ] -> [Chorus] -> [Compressor] -> ...
//!         |
//!         v
//! High-level blocks (with presets/variations)
//! Chorus Block = { "Vintage CE-1", "Digital Tri", ... }
//!         |
//!         v
//! Core framework (this crate)
//! - Parameter: smoothed, ranged, modulatable values
//! - ModSource: LFOs, ADSRs connect to parameters
//!         |
//!         v
//! Internal DSP composition
//! [Upsample] -> [LFO] -> [Delay] -> [Mix] -> [Downsample]
//!         |
//!         v
//! Primitives (dasp_* or custom)
//! Filters, delays, interpolation, modulation sources
//! ```

mod chain;
mod traits;

pub mod core;
pub mod distortion;
pub mod dynamics;
pub mod eq;
pub mod modulation;
pub mod primitives;

// Re-export core types
pub use chain::Chain;
pub use core::{ModAmount, ModSlot, ModSource, ModSourceBox, Parameter, Range, Skew};
pub use traits::{Block, Params};
