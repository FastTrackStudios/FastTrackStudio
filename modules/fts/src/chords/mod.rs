//! Chord detection and display for FastTrackStudio
//!
//! This module provides functionality for detecting, analyzing, and displaying chords
//! with support for multiple notation formats (standard names, Nashville Number System, Roman Numerals).

pub mod types;

// Stream types are always available (used by both native and WASM)
#[cfg(feature = "iroh")]
pub mod stream_types;

#[cfg(feature = "dioxus")]
pub mod infra;

// Reactive module only available on native (uses rxrust, tokio)
#[cfg(all(feature = "iroh", not(target_arch = "wasm32")))]
pub mod reactive;

pub use types::*;

// Reactive services only on native
#[cfg(all(feature = "iroh", not(target_arch = "wasm32")))]
pub use reactive::{
    ChordsReactiveService, ChordsReactiveState, ChordsStreams, DefaultChordsReactiveService,
    EventStreamSubject,
};

// Native-only IRPC types (require full iroh)
#[cfg(all(feature = "iroh", not(target_arch = "wasm32")))]
pub use reactive::irpc::{ChartApi, ChartStreamApi, ChartUpdateMessage};

// Stream types available on both native and WASM
#[cfg(feature = "iroh")]
pub use stream_types::{
    ChartStreamMessage, ChartWithPlayback, PlaybackPositionMessage, SerializableBeatPosition,
    SerializablePageLayout,
};

#[cfg(feature = "dioxus")]
pub use infra::CHART_STATE as CHART_STATE_SIGNAL;
