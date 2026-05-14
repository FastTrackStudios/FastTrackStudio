//! Sync domain traits — the canonical, source-of-truth API for DAW operations.
//!
//! These traits replace the parallel async/sync API duplication. Three impls
//! are expected (see `DAW_API_ARCHITECTURE.md`):
//!
//! - `ReaperMainThread` in `daw-reaper`: direct reaper-rs calls, `!Send`,
//!   valid only on REAPER's main thread. Zero-overhead hot path.
//! - `ReaperRemote` in `daw-reaper`: `Send + Sync`, wraps each call in
//!   `main_thread::query().block_on()`. Replaces `daw-control-sync`.
//! - `Standalone` in `daw-standalone`: in-memory backend for tests and
//!   non-REAPER hosts.
//!
//! The Vox async services become thin adapters that call sync trait methods
//! inside one `main_thread::query` per RPC, batching whole workflows into
//! a single main-thread transaction.
//!
//! # Conventions
//!
//! - Reads return `Vec<T>` / `Option<T>` directly. Failure to find ≠ error.
//! - Mutations return `DawResult<T>` so callers can react to invalid handles.
//! - Sub-handle accessors return owned lightweight wrappers via GATs
//!   (`type Foo<'a>: Foo + 'a`). Impls are free to use `Self`-borrowed structs.
//! - `Project::guid` returns the project identifier; subsequent ops scope
//!   to that project. `Daw::current_project()` is the typical entry point.

mod action_registry;
mod audio_engine;
mod daw;
// `ExtState` ported to architect::rpc — see `crate::ext_state`.
// `FxChains` ported to architect::rpc — see `crate::fx_chains`.
// `FxParams` ported to architect::rpc — see `crate::fx_params`.
// `Items` ported to architect::rpc — see `crate::item::service`.
// `Markers` lives in `crate::marker::service` now — one canonical
// declaration alongside `Marker` itself. See `marker/service.rs`.
mod plugin_loader;
mod project;
// `Regions` ported to architect::rpc — see `crate::region`.
// `Routing` ported to architect::rpc — see `crate::routing::sync_service`.
// `Takes` ported to architect::rpc — see `crate::take::service`.
// `TempoMap` ported to architect::rpc — see `crate::tempo_map`.
mod toolbar;
// `Tracks` ported to architect::rpc — see `crate::track::service`.
// Re-exported below alongside the rest of the sync trait family.
// `Transport` ported to architect::rpc — see `crate::transport::service`.
mod window_geometry;

pub use crate::ext_state::{ExtState, ExtStateRpc};
#[cfg(feature = "vox")]
pub use crate::ext_state::{ExtStateClient, serve as serve_ext_state};
pub use crate::fx_chains::{FxChains, FxChainsRpc};
#[cfg(feature = "vox")]
pub use crate::fx_chains::{FxChainsClient, serve as serve_fx_chains};
pub use crate::fx_params::{FxParams, FxParamsRpc};
#[cfg(feature = "vox")]
pub use crate::fx_params::{FxParamsClient, serve as serve_fx_params};
pub use crate::item::{Items, ItemsRpc};
#[cfg(feature = "vox")]
pub use crate::item::{ItemsClient, serve as serve_items};
pub use action_registry::ActionRegistry;
pub use audio_engine::AudioEngine;
pub use daw::Daw;
// `Markers` and its architect-emitted RPC face (`MarkersRpc`,
// `MarkersClient`, `serve`) are re-exported from `crate::marker`,
// which is the canonical home for everything marker-related.
pub use crate::marker::{Markers, MarkersRpc};
#[cfg(feature = "vox")]
pub use crate::marker::{MarkersClient, serve as serve_markers};
pub use crate::region::{Regions, RegionsRpc};
#[cfg(feature = "vox")]
pub use crate::region::{RegionsClient, serve as serve_regions};
pub use crate::routing::{Routing, RoutingRpc};
#[cfg(feature = "vox")]
pub use crate::routing::{RoutingClient, serve as serve_routing};
pub use crate::take::{Takes, TakesRpc};
#[cfg(feature = "vox")]
pub use crate::take::{TakesClient, serve as serve_takes};
pub use crate::tempo_map::{TempoMap, TempoMapRpc};
#[cfg(feature = "vox")]
pub use crate::tempo_map::{TempoMapClient, serve as serve_tempo_map};
pub use crate::track::{Tracks, TracksRpc};
#[cfg(feature = "vox")]
pub use crate::track::{TracksClient, serve as serve_tracks};
pub use crate::transport::TransportRpc;
pub use crate::transport::service::Transport;
#[cfg(feature = "vox")]
pub use crate::transport::{TransportClient, serve as serve_transport};
pub use plugin_loader::PluginLoader;
pub use project::Project;
pub use toolbar::Toolbar;
pub use window_geometry::WindowGeometry;
