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

mod action_registry;
mod audio_accessor;
#[cfg(feature = "audio")]
pub mod audio_engine;
mod audio_engine_svc;
mod automation;
mod dawfile_service;
mod ext_state;
mod fx;
mod fx_chains;
mod fx_params;
mod health;
mod input;
mod item;
mod live_midi;
mod marker;
mod midi;
mod peak;
pub(crate) mod platform;
mod plugin_loader;
mod position_conversion;
mod project;
mod region;
mod resource;
mod routing;
mod routing_sync;
mod screenset;
mod shared_state;
pub mod sync;
mod take;
mod tempo_map;
mod toolbar;
mod track;
mod transport;
mod ui;

// All per-service impls are on `Standalone` directly post-port.
// Old `Standalone*` per-service structs retired.
pub use project::project_guids;
pub use shared_state::SharedProjectState;
pub use sync::Standalone;
