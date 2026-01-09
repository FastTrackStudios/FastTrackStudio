//! Core framework abstractions.
//!
//! This module provides the foundational types that blocks build upon:
//!
//! - [`param`] - Parameter system with ranges, smoothing, and modulation
//! - [`modulation`] - Modulation source trait and routing
//!
//! ## Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────┐
//! │  Blocks (EQ, Chorus, etc.)                              │
//! │  - Use Parameters for controllable values               │
//! │  - Process audio using primitives                       │
//! └─────────────────────────────────────────────────────────┘
//!                          │
//!                          ▼
//! ┌─────────────────────────────────────────────────────────┐
//! │  Core (this module)                                     │
//! │  - Parameter: smoothed, ranged, modulatable values      │
//! │  - ModSource: trait for LFO, ADSR, etc.                 │
//! │  - ModSlot: connects sources to parameters              │
//! └─────────────────────────────────────────────────────────┘
//!                          │
//!                          ▼
//! ┌─────────────────────────────────────────────────────────┐
//! │  Primitives                                             │
//! │  - Raw DSP: filters, delays, oscillators                │
//! │  - Modulation sources implement ModSource               │
//! └─────────────────────────────────────────────────────────┘
//! ```

pub mod modulation;
pub mod param;

pub use modulation::{ModAmount, ModSlot, ModSource, ModSourceBox};
pub use param::{Parameter, Range, Skew};
