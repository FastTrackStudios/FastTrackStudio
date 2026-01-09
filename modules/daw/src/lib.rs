//! Unified DAW crate - contains all DAW-related functionality
//!
//! This crate consolidates all DAW modules into a single crate:
//! - `primitives`: Core primitives (Position, TimeSignature, etc.)
//! - `transport`: Transport control and state management
//! - `marker_region`: Markers and regions functionality
//! - `tracks`: Track and item management
//! - `project`: Project structure and management
//! - `mix_fx`: Mixing and FX functionality

pub mod primitives;

pub use primitives::{
    Duration, MusicalDuration, MusicalPosition, Position, TimeDuration, TimePosition, TimeRange,
    TimeSignature,
};
pub mod marker_region;
pub mod mix_fx;
pub mod project;
pub mod state;
pub mod streams;
pub mod tracks;
pub mod transport;

// Re-export commonly used types for convenience
pub use marker_region::{Marker, MarkerRegionSource, Region};
pub use primitives::*;
pub use project::Project;
pub use state::DawReactiveState;
pub use streams::{DawStreams, EventStreamSubject, ProjectStreams};
pub use tracks::reactive::TrackStreams;
pub use tracks::{Envelope, FxChain, Item, Track};
pub use transport::reactive::TransportStreams;
pub use transport::{PlayState, RecordMode, Tempo, Transport, TransportActions, TransportError};
