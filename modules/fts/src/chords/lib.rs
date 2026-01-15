//! Chord detection and display for FastTrackStudio
//!
//! This module provides functionality for detecting, analyzing, and displaying chords
//! with support for multiple notation formats (standard names, Nashville Number System, Roman Numerals).

pub mod types;

#[cfg(feature = "iroh")]
pub mod reactive;

pub use types::*;

#[cfg(feature = "iroh")]
pub use reactive::{
    ChordsStreams, ChordsReactiveState, ChordsReactiveService,
    DefaultChordsReactiveService, EventStreamSubject,
};

#[cfg(feature = "iroh")]
pub use reactive::irpc::{ChartApi, ChartUpdateMessage};

