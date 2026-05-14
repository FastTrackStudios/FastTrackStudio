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
// markers ported to architect::rpc — `impl Markers for Standalone`
// lives at `crate::marker`. The borrowed `StandaloneMarkers<'a>` view
// retired with the port.
mod project;
// regions ported to architect::rpc — see `crate::region`.
mod routing;
// takes ported to architect::rpc — see `crate::take`.
// tempo_map ported to architect::rpc — see `crate::tempo_map`.
// tracks ported to architect::rpc — `impl Tracks for Standalone`
// lives at `crate::track`.
// `Transport` ported to architect::rpc — `impl Transport for
// Standalone` lives at `crate::transport`.

pub use daw::{
    FxChainKey, FxEntry, ItemEntry, ProjectState, Standalone, StandaloneState, TakeList,
};
pub use ext_state::StandaloneExtState;
pub use fx_chains::StandaloneFxChains;
pub use fx_params::StandaloneFxParams;
pub use items::StandaloneItems;
pub use project::StandaloneProject;
pub use routing::StandaloneRouting;
