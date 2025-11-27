//! Unified DAW crate - contains all DAW-related functionality
//!
//! This crate consolidates all DAW modules into a single crate:
//! - `primitives`: Core primitives (Position, TimeSignature, etc.)
//! - `transport`: Transport control and state management
//! - `marker_region`: Markers and regions functionality
//! - `tracks`: Track and item management
//! - `project`: Project structure and management
//! - `mix_fx`: Mixing and FX functionality
//! - `instruments`: Instrument definitions

pub mod primitives;
pub mod transport;
pub mod marker_region;
pub mod tracks;
pub mod project;
pub mod mix_fx;
pub mod instruments;
pub mod streams;
pub mod state;

// Re-export commonly used types for convenience
pub use primitives::*;
pub use transport::{Transport, TransportActions, TransportError, PlayState, RecordMode, Tempo};
pub use marker_region::{Marker, Region, MarkerRegionSource};
pub use tracks::{Track, Item, Envelope, FxChain};
pub use project::Project;
pub use streams::{DawStreams, ProjectStreams, EventStreamSubject};
pub use state::DawReactiveState;
pub use transport::reactive::TransportStreams;
pub use tracks::reactive::TrackStreams;

