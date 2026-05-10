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

pub use daw::Daw;
pub use ext_state::ExtState;
pub use fx_chains::FxChains;
pub use fx_params::FxParams;
pub use items::Items;
pub use markers::Markers;
pub use project::Project;
pub use regions::Regions;
pub use routing::Routing;
pub use takes::Takes;
pub use tempo_map::TempoMap;
pub use tracks::Tracks;
pub use transport::Transport;
