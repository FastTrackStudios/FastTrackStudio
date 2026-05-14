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
mod ext_state;
mod fx_chains;
mod fx_params;
mod items;
// `Markers` lives in `crate::marker::service` now — one canonical
// declaration alongside `Marker` itself. See `marker/service.rs`.
mod plugin_loader;
mod project;
mod regions;
mod routing;
mod takes;
mod tempo_map;
mod toolbar;
mod tracks;
mod transport;
mod window_geometry;

pub use action_registry::ActionRegistry;
pub use audio_engine::AudioEngine;
pub use daw::Daw;
pub use ext_state::ExtState;
pub use fx_chains::FxChains;
pub use fx_params::FxParams;
pub use items::Items;
// `Markers` and its architect-emitted RPC face (`MarkersRpc`,
// `MarkersClient`, `serve`) are re-exported from `crate::marker`,
// which is the canonical home for everything marker-related.
pub use crate::marker::{Markers, MarkersRpc};
#[cfg(feature = "vox")]
pub use crate::marker::{MarkersClient, serve as serve_markers};
pub use plugin_loader::PluginLoader;
pub use project::Project;
pub use regions::Regions;
pub use routing::Routing;
pub use takes::Takes;
pub use tempo_map::TempoMap;
pub use toolbar::Toolbar;
pub use tracks::Tracks;
pub use transport::Transport;
pub use window_geometry::WindowGeometry;
