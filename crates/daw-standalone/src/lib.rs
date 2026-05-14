//! Standalone DAW Implementation
//!
//! This is a minimal DAW implementation that runs standalone without any external DAW.
//! It serves as both the reference implementation and the mock for testing.
//!
//! The implementations in this module (`StandaloneTransport`, `StandaloneProject`) can be
//! used directly in tests without spawning a separate cell process.
//!
//! ## Mock Data
//!
//! The standalone implementation includes mock data for testing:
//! - **3 songs** with markers (SONGSTART/SONGEND)
//! - **Sections** as regions (Intro, Verse, Chorus, Bridge, Outro, Solo)
//! - **Tempo/time signature changes** throughout the timeline
//!
//! This allows testing the full fts-control-web experience without a real DAW.

#![deny(unsafe_code)]

#[cfg(feature = "audio")]
pub mod audio_engine;
mod automation;
mod ext_state;
mod fx;
mod fx_chains;
mod fx_params;
mod item;
mod live_midi;
mod marker;
mod midi;
pub(crate) mod platform;
mod position_conversion;
mod project;
mod region;
mod resource;
mod routing;
mod routing_sync;
mod shared_state;
pub mod sync;
mod take;
mod tempo_map;
mod track;
mod transport;
mod ui;

pub use automation::StandaloneAutomation;
// `ExtState` impl'd directly on `Standalone` — see `crate::ext_state`.
pub use fx::StandaloneFx;
// `Items` impl'd directly on `Standalone` — see `crate::item`.
// `StandaloneItem` / `StandaloneTake` retired with their async services.
pub use live_midi::StandaloneLiveMidi;
// `Markers` is now impl'd directly on `Standalone` — see `crate::marker`.
// The old async `StandaloneMarker` service struct was retired with the
// architect::rpc port; mount via `daw_proto::marker::serve(Standalone::new())`.
pub use midi::StandaloneMidi;
pub use position_conversion::StandalonePositionConversion;
pub use project::{StandaloneProject, project_guids};
// `Regions` impl'd directly on `Standalone` — see `crate::region`.
// The old async `StandaloneRegion` service struct retired with the
// architect::rpc port.
pub use resource::StandaloneResource;
pub use routing::StandaloneRouting;
pub use sync::Standalone;
// `TempoMap` impl'd directly on `Standalone` — see `crate::tempo_map`.
// `Tracks` impl'd directly on `Standalone` — see `crate::track`.
// The old async `StandaloneTrack` service struct retired with the port.
// `Transport` impl'd directly on `Standalone` — see `crate::transport`.
// The old async `StandaloneTransport` service struct retired with the port.
pub use shared_state::SharedProjectState;
pub use ui::StandaloneUi;
