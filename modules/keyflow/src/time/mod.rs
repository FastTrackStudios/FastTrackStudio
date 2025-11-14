//! Time System - Musical durations and positions
//!
//! This module provides trait-based time primitives for musical timing.

pub mod duration;
pub mod position;
pub mod tempo;

pub use duration::{Duration, MusicalDuration, TimeSignature};
pub use position::{Position, AbsolutePosition};
pub use tempo::Tempo;
