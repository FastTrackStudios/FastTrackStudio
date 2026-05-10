//! `Send + Sync` REAPER implementation of the `daw_proto::sync` traits.
//!
//! [`ReaperRemote`] wraps every trait call in `main_thread::query` and blocks
//! the calling thread on a tokio runtime until the closure has executed on
//! REAPER's main thread. This is the replacement for the old
//! `daw-control-sync` crate (a blocking wrapper over the now-retired async
//! `Daw` API).
//!
//! Each trait method delegates into a fresh [`crate::sync::ReaperMainThread`]
//! constructed inside the dispatched closure — the `!Send` main-thread handle
//! never crosses the closure boundary, so [`ReaperRemote`] itself is `Send +
//! Sync` and safe to share between worker threads / audio callbacks.
//!
//! Sub-handles ([`RemoteProject`], [`RemoteTransport`], etc.) borrow
//! `&'a ReaperRemote` and inherit its `Send + Sync` discipline via the borrow.

pub mod daw;
pub mod ext_state;
pub mod fx_chains;
pub mod fx_params;
pub mod items;
pub mod markers;
pub mod project;
pub mod regions;
pub mod routing;
pub mod takes;
pub mod tempo_map;
pub mod tracks;
pub mod transport;

pub use daw::ReaperRemote;
pub use ext_state::RemoteExtState;
pub use fx_chains::RemoteFxChains;
pub use fx_params::RemoteFxParams;
pub use items::RemoteItems;
pub use markers::RemoteMarkers;
pub use project::RemoteProject;
pub use regions::RemoteRegions;
pub use routing::RemoteRouting;
pub use takes::RemoteTakes;
pub use tempo_map::RemoteTempoMap;
pub use tracks::RemoteTracks;
pub use transport::RemoteTransport;

use daw_proto::{DawError, DawResult};

/// Dispatch a fallible mutation onto REAPER's main thread and block until it
/// returns. The outer `None` from `main_thread::query` (TaskSupport not yet
/// initialized) is flattened into [`DawError::MainThreadUnavailable`].
pub(crate) fn dispatch<F, R>(remote: &ReaperRemote, f: F) -> DawResult<R>
where
    F: FnOnce() -> DawResult<R> + Send + 'static,
    R: Send + 'static,
{
    remote.runtime.block_on(async move {
        crate::main_thread::query(f)
            .await
            .ok_or(DawError::MainThreadUnavailable)?
    })
}

/// Dispatch an infallible read onto REAPER's main thread, falling back to
/// `R::default()` if TaskSupport is unavailable. Matches the
/// "reads silently degrade" convention documented on the sync traits.
pub(crate) fn dispatch_read<F, R>(remote: &ReaperRemote, f: F) -> R
where
    F: FnOnce() -> R + Send + 'static,
    R: Send + 'static + Default,
{
    remote
        .runtime
        .block_on(async move { crate::main_thread::query(f).await.unwrap_or_default() })
}

/// Construct a `ReaperMainThread` inside the dispatched closure or surface
/// `DawError::MainThreadUnavailable` if the high-level REAPER API has not been
/// initialized yet.
pub(crate) fn main_thread() -> DawResult<crate::sync::ReaperMainThread> {
    crate::sync::ReaperMainThread::try_new().ok_or(DawError::MainThreadUnavailable)
}
