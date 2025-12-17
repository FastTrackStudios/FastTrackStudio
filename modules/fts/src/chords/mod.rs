//! Chord detection and display for FastTrackStudio
//!
//! This module provides functionality for detecting, analyzing, and displaying chords
//! with support for multiple notation formats (standard names, Nashville Number System, Roman Numerals).

pub mod types;

#[cfg(feature = "dioxus")]
pub mod infra;

#[cfg(not(target_arch = "wasm32"))]
pub mod reactive;

pub use types::*;

#[cfg(not(target_arch = "wasm32"))]
pub use reactive::{
    ChordsStreams, ChordsReactiveState, ChordsReactiveService,
    DefaultChordsReactiveService, EventStreamSubject,
};

#[cfg(not(target_arch = "wasm32"))]
pub use reactive::irpc::{ChartApi, ChartUpdateMessage};

#[cfg(feature = "dioxus")]
pub use infra::CHART_STATE as CHART_STATE_SIGNAL;

