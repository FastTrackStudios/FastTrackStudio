//! Sync trait implementation for the standalone backend.
//!
//! This module implements `daw_proto::sync::*` traits with an in-memory state
//! store. It is intended for tests and non-REAPER hosts. The state lives behind
//! a single `std::sync::Mutex` (this is non-realtime control state — std mutex
//! is fine, this is not the audio path).
//!
//! The sync state here is independent from the older async services in this
//! crate (`StandaloneTransport`, `StandaloneRegion`, etc.). Sharing the storage
//! across both API styles would be valuable but is left for a follow-up; the
//! async services have their own per-service locks and entry points and would
//! need broader surgery to converge.

mod daw;
mod ext_state;
mod fx_chains;
mod fx_params;
mod items;
mod markers;
mod project;
mod regions;
mod routing;
mod takes;
mod tempo_map;
mod tracks;
mod transport;

pub use daw::{
    FxChainKey, FxEntry, ItemEntry, ProjectState, Standalone, StandaloneState, TakeList,
};
pub use ext_state::StandaloneExtState;
pub use fx_chains::StandaloneFxChains;
pub use fx_params::StandaloneFxParams;
pub use items::StandaloneItems;
pub use markers::StandaloneMarkers;
pub use project::StandaloneProject;
pub use regions::StandaloneRegions;
pub use routing::StandaloneRouting;
pub use takes::StandaloneTakes;
pub use tempo_map::StandaloneTempoMap;
pub use tracks::StandaloneTracks;
pub use transport::StandaloneTransport;
